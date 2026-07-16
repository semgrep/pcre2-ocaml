(* Fast-engine IR compiler (M11 chunk C1). Walks the shared compiler's
   bytecode ([re.code], LINK_SIZE = 2 via Pcre2_engine.Compile.get) and
   produces an [Ir.t] whose operands are pre-decoded and whose jump targets
   are absolute IR indices. The opcode walk MIRRORS pcre2_printint.c /
   src/engine/debug_printer.ml; the choice-point lowering is native design
   (fast-design.md §3). This is compile-time code (not the runner hot loop),
   so plain recursion bounded by bracket nesting is fine (port-conventions
   §6 — the C recurses the same way, capped by error 119 ~250).

   Coverage is the chunk C1 subset (11-fast-engine.md, fast-design.md §6):
   OP_END; OP_CHAR (fused into CHAR_RUN) / OP_CHARI; non-capturing,
   non-repeated OP_BRA/OP_ALT/OP_KET; simple anchors \A \G \z \Z ^ $.
   Everything else yields [Error "fast: <construct> (chunk X)"] naming the
   FIRST unsupported construct and the chunk where support lands. *)

module C = Pcre2_engine.Compile
module Op = Pcre2_engine.Opcodes
module Opt = Pcre2_engine.Options
module Limits = Pcre2_engine.Limits
module Chartables = Pcre2_engine.Chartables

(* Signals the first unsupported construct; caught at the top of [compile]
   and turned into [Error]. Compile-phase only — never crosses the runner
   loop (port-conventions §2/§6). *)
exception Unsupported of string

(* fast-design.md §6 — map an unsupported opcode to its taxonomy reason. The
   chunk letters follow 11-fast-engine.md's checklist (D captures/repeats,
   E classes/types, F backrefs, G lookaround/atomic, H verbs, I UTF/UCP,
   J conditionals, K recursion/script-run/callout, C2+ = runner semantics
   that chunk C1 does not lower). Named per-opcode so the FIRST rejection is
   precise and stable for skip-report grouping. *)
let reason_of_op (op : int) : string =
  let named name chunk = "fast: " ^ name ^ " (chunk " ^ chunk ^ ")" in
  (* Simple-anchor variants and boundaries whose runtime semantics arrive
     with the runner (chunk C2+). *)
  if Int.equal op Op.op_set_som then named "\\K" "C2+"
    (* UTF / UCP property machinery (chunk I). The UCP word-boundary opcodes
       (OP_*_UCP_WORD_BOUNDARY) use Unicode properties even without UTF; the
       non-UCP \b / \B (OP_WORD_BOUNDARY / OP_NOT_WORD_BOUNDARY) are handled in
       [compile_branch]. *)
  else if Int.equal op Op.op_not_ucp_word_boundary then named "\\B (ucp)" "I"
  else if Int.equal op Op.op_ucp_word_boundary then named "\\b (ucp)" "I"
  else if Int.equal op Op.op_prop then named "\\p" "I"
  else if Int.equal op Op.op_notprop then named "\\P" "I"
  else if Int.equal op Op.op_extuni then named "\\X" "I"
    (* Class / ref repeat quantifiers OP_CRSTAR..OP_CRPOSRANGE: a class repeat
       is consumed inline after its OP_CLASS/OP_NCLASS in [compile_branch]; a
       standalone CR* here can only follow an (out-of-subset) OP_REF (chunk
       F). *)
  else if op >= Op.op_crstar && op <= Op.op_crposrange then
    named "ref repeat" "F"
    (* OP_XCLASS: in non-UTF/non-UCP the compiler only emits OP_XCLASS for a
       class containing a Unicode property (\p/\P) — see compile.ml:2199-2281,
       where xclass=true implies xclass_has_prop=true (wide chars > 255 cannot
       occur non-UTF). Matching it needs the property machinery, so it is
       declined to chunk I rather than ported. *)
  else if Int.equal op Op.op_xclass then named "OP_XCLASS (\\p in class)" "I"
    (* Back references (chunk F). *)
  else if Int.equal op Op.op_ref then named "OP_REF" "F"
  else if Int.equal op Op.op_refi then named "OP_REFI" "F"
  else if Int.equal op Op.op_dnref then named "OP_DNREF" "F"
  else if Int.equal op Op.op_dnrefi then named "OP_DNREFI" "F"
    (* Recursion / script-run / callouts (chunk K). *)
  else if Int.equal op Op.op_recurse then named "OP_RECURSE" "K"
  else if Int.equal op Op.op_script_run then named "OP_SCRIPT_RUN" "K"
  else if Int.equal op Op.op_callout then named "OP_CALLOUT" "K"
  else if Int.equal op Op.op_callout_str then named "OP_CALLOUT_STR" "K"
    (* Repeated-group kets KETRMAX/KETRMIN are lowered in [compile_group]
       (chunk D2). The possessive repeating ket KETRPOS stays declined
       (possessive groups → chunk G; see the [compile_group] guard). *)
  else if Int.equal op Op.op_ketrpos then named "possessive group (KETRPOS)" "G"
    (* Lookaround / lookbehind reverse / atomic (chunk G). *)
  else if Int.equal op Op.op_reverse then named "OP_REVERSE" "G"
  else if Int.equal op Op.op_vreverse then named "OP_VREVERSE" "G"
  else if op >= Op.op_assert && op <= Op.op_assertback_na then
    named "lookaround" "G"
  else if Int.equal op Op.op_once then named "OP_ONCE" "G"
    (* Groups: non-capturing OP_BRA, capturing OP_CBRA/OP_SCBRA and the
       empty-checking non-capturing OP_SBRA are handled in [compile_group]
       (chunk D2). The possessive brackets (OP_*POS) resist the minimal-save
       design (their KETRPOS frame-juggling protocol) and are declined to
       chunk G, as is OP_BRAPOSZERO. OP_CLOSE (before ACCEPT) → chunk H. *)
  else if Int.equal op Op.op_brapos then named "possessive group (BRAPOS)" "G"
  else if Int.equal op Op.op_cbrapos then named "possessive group (CBRAPOS)" "G"
  else if Int.equal op Op.op_sbrapos then named "possessive group (SBRAPOS)" "G"
  else if Int.equal op Op.op_scbrapos then named "possessive group (SCBRAPOS)" "G"
  else if Int.equal op Op.op_braposzero then
    named "possessive group (BRAPOSZERO)" "G"
  else if Int.equal op Op.op_close then named "OP_CLOSE" "H"
    (* Conditionals (chunk J). *)
  else if Int.equal op Op.op_cond then named "OP_COND" "J"
  else if Int.equal op Op.op_scond then named "OP_SCOND" "J"
  else if op >= Op.op_cref && op <= Op.op_true then named "conditional ref" "J"
  else if Int.equal op Op.op_define then named "OP_DEFINE" "J"
    (* Backtracking control verbs and forced success/failure (chunk H). *)
  else if op >= Op.op_mark && op <= Op.op_assert_accept then named "verb" "H"
  else "fast: opcode " ^ string_of_int op ^ " (unclassified)"

(* Anchor opcode -> IR tag; [None] otherwise. Chunk D adds the multiline
   anchors OP_CIRCM/OP_DOLLM (their runner arms mirror interpreter.ml
   :3108-3161 via Pcre2_engine.Newline was_newline/is_newline). *)
let anchor_tag (op : int) : int option =
  if Int.equal op Op.op_sod then Some Ir.t_sod
  else if Int.equal op Op.op_som then Some Ir.t_som
  else if Int.equal op Op.op_eod then Some Ir.t_eod
  else if Int.equal op Op.op_eodn then Some Ir.t_eodn
  else if Int.equal op Op.op_circ then Some Ir.t_circ
  else if Int.equal op Op.op_doll then Some Ir.t_doll
  else if Int.equal op Op.op_circm then Some Ir.t_circm
  else if Int.equal op Op.op_dollm then Some Ir.t_dollm
  else None

(* ---------- Single-char repeat decode (fast-design.md §2) ----------

   Decode a repeat opcode OP_STAR..OP_NOTPOSUPTOI (33-84) at bytecode
   offset [p] into (ir_tag, reptype, lmin, lmax, char_off). Mirrors the
   interpreter dispatch pcre2_match.c:1188-1257 (positive) and 1542-1611
   (NOT); interpreter.ml:1718-1806. [fidx] is the position within a
   13-opcode family (STAR MINSTAR PLUS MINPLUS QUERY MINQUERY UPTO MINUPTO
   EXACT POSSTAR POSPLUS POSQUERY POSUPTO), identical across the four
   families. UPTO/MINUPTO/EXACT/POSUPTO (fidx 6,7,8,12) carry an IMM2 count
   before the char; the rest have the char immediately after the opcode. *)

let is_char_repeat (op : int) : bool =
  op >= Op.op_star && op <= Op.op_notposuptoi

(* Returns (ir_tag, caseless, fidx) for a char-repeat opcode. *)
let rep_kind (op : int) : int * bool * int =
  if op >= Op.op_star && op <= Op.op_posupto then
    (* 33..45 caseful positive *)
    (Ir.t_rep, false, op - Op.op_star)
  else if op >= Op.op_stari && op <= Op.op_posuptoi then
    (* 46..58 caseless positive *)
    (Ir.t_repi, true, op - Op.op_stari)
  else if op >= Op.op_notstar && op <= Op.op_notposupto then
    (* 59..71 caseful NOT *)
    (Ir.t_notrep, false, op - Op.op_notstar)
  else
    (* 72..84 caseless NOT *)
    (Ir.t_notrepi, true, op - Op.op_notstari)

(* (reptype, lmin, lmax) for a family index and (already-read) count. *)
let rep_bounds (fidx : int) (count : int) : int * int * int =
  match fidx with
  | 0 -> (Ir.reptype_max, 0, Ir.rep_inf) (* STAR *)
  | 1 -> (Ir.reptype_min, 0, Ir.rep_inf) (* MINSTAR *)
  | 2 -> (Ir.reptype_max, 1, Ir.rep_inf) (* PLUS *)
  | 3 -> (Ir.reptype_min, 1, Ir.rep_inf) (* MINPLUS *)
  | 4 -> (Ir.reptype_max, 0, 1) (* QUERY *)
  | 5 -> (Ir.reptype_min, 0, 1) (* MINQUERY *)
  | 6 -> (Ir.reptype_max, 0, count) (* UPTO *)
  | 7 -> (Ir.reptype_min, 0, count) (* MINUPTO *)
  | 8 -> (Ir.reptype_min, count, count) (* EXACT — reptype unread (lmin=lmax) *)
  | 9 -> (Ir.reptype_pos, 0, Ir.rep_inf) (* POSSTAR *)
  | 10 -> (Ir.reptype_pos, 1, Ir.rep_inf) (* POSPLUS *)
  | 11 -> (Ir.reptype_pos, 0, 1) (* POSQUERY *)
  | _ -> (Ir.reptype_pos, 0, count) (* POSUPTO (fidx 12) *)

(* fidx that carry an IMM2 count before the char (UPTO/MINUPTO/EXACT/POSUPTO). *)
let rep_has_count (fidx : int) : bool =
  Int.equal fidx 6 || Int.equal fidx 7 || Int.equal fidx 8 || Int.equal fidx 12

(* ---------- Class-repeat decode (fast-design.md §2/§4) ----------

   Decode a class/ref repeat quantifier OP_CRSTAR..OP_CRPOSRANGE (98-109) at
   bytecode offset [q] into (reptype, lmin, lmax, byte_len). Mirrors the
   OP_CLASS repeat dispatch (pcre2_match.c:1939-1971 / interpreter.ml
   :1830-1872) via the rep_min/rep_max/rep_typ tables, indexed by
   [q_op - OP_CRSTAR] (STAR MINSTAR PLUS MINPLUS QUERY MINQUERY RANGE MINRANGE
   POSSTAR POSPLUS POSQUERY POSRANGE). The three RANGE forms read GET2 bounds
   (a zero max means infinity). *)
let cr_bounds (src : Bytes.t) (q : int) : int * int * int * int =
  let crop = Char.code (Bytes.get src q) in
  let idx = crop - Op.op_crstar in
  match idx with
  | 0 -> (Ir.reptype_max, 0, Ir.rep_inf, 1) (* CRSTAR *)
  | 1 -> (Ir.reptype_min, 0, Ir.rep_inf, 1) (* CRMINSTAR *)
  | 2 -> (Ir.reptype_max, 1, Ir.rep_inf, 1) (* CRPLUS *)
  | 3 -> (Ir.reptype_min, 1, Ir.rep_inf, 1) (* CRMINPLUS *)
  | 4 -> (Ir.reptype_max, 0, 1, 1) (* CRQUERY *)
  | 5 -> (Ir.reptype_min, 0, 1, 1) (* CRMINQUERY *)
  | 8 -> (Ir.reptype_pos, 0, Ir.rep_inf, 1) (* CRPOSSTAR *)
  | 9 -> (Ir.reptype_pos, 1, Ir.rep_inf, 1) (* CRPOSPLUS *)
  | 10 -> (Ir.reptype_pos, 0, 1, 1) (* CRPOSQUERY *)
  | 6 | 7 | 11 ->
      (* CRRANGE (6) / CRMINRANGE (7) / CRPOSRANGE (11): GET2 min, GET2 max
         (0 => infinity); item length 1 + 2*IMM2. *)
      let reptype =
        if Int.equal idx 7 then Ir.reptype_min
        else if Int.equal idx 11 then Ir.reptype_pos
        else Ir.reptype_max
      in
      let lmin = C.get2 src (q + 1) in
      let lmaxv = C.get2 src (q + 1 + Limits.imm2_size) in
      let lmax = if Int.equal lmaxv 0 then Ir.rep_inf else lmaxv in
      (reptype, lmin, lmax, 1 + (2 * Limits.imm2_size))
  | _ -> (Ir.reptype_max, 1, 1, 1) (* not a CR opcode (defensive) *)

let is_cr_op (op : int) : bool = op >= Op.op_crstar && op <= Op.op_crposrange

(* A character-type opcode the fast engine matches in the non-UTF subset:
   \D \d \S \s \W \w (ctypes), . (OP_ANY), OP_ALLANY/OP_ANYBYTE, \R (OP_ANYNL),
   \h \H \v \V. OP_PROP/OP_NOTPROP/OP_EXTUNI need the property machinery (chunk
   I) and are excluded here. *)
let is_supported_type (op : int) : bool =
  (op >= Op.op_not_digit && op <= Op.op_anybyte)
  || (op >= Op.op_anynl && op <= Op.op_vspace)

(* ---------- optimized_cbracket analysis (pcre2_jit_compile.c:404) ----------

   Port of the JIT's [optimized_cbracket] flag (allocated at :14266, memset
   to 1 at :14272, cleared at :1145/1159/1173/1184). A capturing bracket is
   "optimized" (bit stays 1) unless it is reached by a back reference
   (OP_REF/OP_REFI), a possessive capture (OP_CBRAPOS/OP_SCBRAPOS), a
   conditional numbered reference (OP_CREF), or a duplicate-name reference
   (OP_DNREF/OP_DNREFI/OP_DNCREF). An optimized capture never has its
   ovector slots read mid-match, so [compile_group] may use the slot itself
   as the in-progress start scratch and save/restore just the 2 slots on
   backtrack (fast-design.md §3 minimal-save). In THIS chunk's subset every
   referencing opcode is itself out of scope (chunks F/J/K/G), so all
   captures come out optimized — but the analysis is written in full so
   those chunks only need to widen the referencing lowering, not this gate.

   [top_bracket = 0] short-circuits (no captures to analyse). *)
let optimized_cbracket (re : C.re) : bool array =
  let src = re.C.code in
  let top = re.C.top_bracket in
  let opt = Array.make (top + 1) true in
  let limit = Bytes.length src in
  let byte p = Char.code (Bytes.get src p) in
  let mark n = if n >= 0 && n <= top then opt.(n) <- false in
  (* Byte length of the opcode item at [p] (op_lengths + the variable-length
     exceptions), to advance this linear analysis walk. Mirrors the extra
     byte-count logic of pcre2_printint.c / debug_printer.ml. *)
  let code_len p =
    let op = byte p in
    if Int.equal op Op.op_xclass then C.get src (p + 1)
    else if Int.equal op Op.op_callout_str then
      C.get src (p + 1 + (2 * Limits.link_size))
    else if
      Int.equal op Op.op_mark || Int.equal op Op.op_prune_arg
      || Int.equal op Op.op_skip_arg || Int.equal op Op.op_then_arg
      || Int.equal op Op.op_commit_arg
    then Op.op_lengths.(op) + byte (p + 1)
    else if op >= Op.op_typestar && op <= Op.op_typeposupto then (
      (* A type repeat whose type is \p/\P carries 2 extra property bytes
         (debug_printer.ml:373-385). The type byte follows the IMM2 count for
         TYPEUPTO/TYPEMINUPTO/TYPEEXACT/TYPEPOSUPTO (family indices 6,7,8,12),
         else the opcode. *)
      let fidx = op - Op.op_typestar in
      let type_off =
        if rep_has_count fidx then p + 1 + Limits.imm2_size else p + 1
      in
      let t = byte type_off in
      Op.op_lengths.(op)
      + (if Int.equal t Op.op_prop || Int.equal t Op.op_notprop then 2 else 0))
    else Op.op_lengths.(op)
  in
  let rec go p =
    (* Defensive: a length miscalculation must never index out of bounds. A
       conservative early stop only leaves captures optimized (they decline at
       their referencing opcode during the lowering walk). *)
    if p < 0 || p + 1 > limit then ()
    else
      let op = byte p in
      if Int.equal op Op.op_end then ()
      else (
        (if Int.equal op Op.op_ref || Int.equal op Op.op_refi
            || Int.equal op Op.op_cref then mark (C.get2 src (p + 1))
         else if Int.equal op Op.op_cbrapos || Int.equal op Op.op_scbrapos then
           mark (C.get2 src (p + 1 + Limits.link_size))
         else if
           Int.equal op Op.op_dnref || Int.equal op Op.op_dnrefi
           || Int.equal op Op.op_dncref
         then
           let count = C.get2 src (p + 1 + Limits.imm2_size) in
           let slot0 = C.get2 src (p + 1) * re.C.name_entry_size in
           for k = 0 to count - 1 do
             mark (C.get2 re.C.name_table (slot0 + (k * re.C.name_entry_size)))
           done);
        let w = code_len p in
        if w < 1 then () (* defensive: never advance backwards *)
        else (go [@tailcall]) (p + w))
  in
  if top > 0 then go 0;
  opt

let compile (re : C.re) : (Ir.t, string) result =
  (* Compile-level gates BEFORE the walk (fast-design.md §6, task spec). *)
  if not (Int.equal (re.C.overall_options land Opt.utf) 0) then
    Error "fast: UTF mode (chunk I)"
  else if not (Int.equal (re.C.overall_options land Opt.ucp) 0) then
    Error "fast: UCP mode (chunk I)"
  else if not (Int.equal (re.C.overall_options land Opt.firstline) 0) then
    (* PCRE2_FIRSTLINE constrains an unanchored match to the first line. The
       C enforces this partly through the start-of-match scans' shortened
       end_subject (pcre2_match.c:7164-7186), which the naive chunk-C2 driver
       does not replicate; a bump_bottom-only stop (pcre2_match.c:7584-7588)
       diverges at the newline position for patterns with a first code unit.
       Declined until the JIT start-opts land (chunk L, fast-design.md §5). *)
    Error "fast: PCRE2_FIRSTLINE (chunk L)"
  else
    let src = re.C.code in
    let byte (p : int) : int = Char.code (Bytes.get src p) in
    (* GET(code, p+1): a LINK_SIZE = 2 big-endian offset relative to the
       opcode at [p] (pcre2_intmodedep.h:108-109; debug_printer.ml uses the
       same read for Bra/Alt/Ket links). *)
    let link (p : int) : int = C.get src (p + 1) in

    (* Growable int buffer for the flat IR stream (fast-design.md §2). *)
    let buf = ref (Array.make 128 0) in
    let n = ref 0 in
    let ensure (k : int) : unit =
      if k > Array.length !buf then (
        let cap = ref (Array.length !buf) in
        while k > !cap do
          cap := !cap * 2
        done;
        let nb = Array.make !cap 0 in
        Array.blit !buf 0 nb 0 !n;
        buf := nb)
    in
    let push (v : int) : unit =
      ensure (!n + 1);
      !buf.(!n) <- v;
      incr n
    in
    let here () : int = !n in
    let set (i : int) (v : int) : unit = !buf.(i) <- v in
    let lit = Buffer.create 32 in
    let opt_cbracket = optimized_cbracket re in
    (* Chunk D2: fresh id per empty-check-tracked repeated group (grouploop
       bracket + KETRMAX/KETRMIN); indexes [mb.group_start] at run time
       (fast-design.md §3). OP_BRA repeats (bra_loop) get [Ir.no_group]. *)
    let n_groups = ref 0 in
    let fresh_group () : int =
      let g = !n_groups in
      incr n_groups;
      g
    in

    (* [compile_group bra_off]: lower a group (OP_BRA / OP_CBRA / OP_SCBRA
       ... OP_KET) starting at [bra_off]; returns the bytecode offset just
       past its KET. Native choice-point lowering per fast-design.md §3.

       Non-capturing OP_BRA lowers bra_loop-style (the LAST branch has no
       choice point; it runs in the enclosing frame — interpreter.ml
       bra_loop): the runner's rdepth-0 special case still ticks the
       whole-pattern wrapper's last branch (grouploop at the top level).
       OP_CBRA/OP_SCBRA lower grouploop-style: EVERY branch (including the
       last/only one) gets an ALT choice point, and the last ALT's handler
       is a t_fail so backtracking out of the exhausted group propagates
       NOMATCH — this reproduces grouploop's per-branch RMATCH tick
       (interpreter.ml:2434-2444 / grouploop 6635-6657), which a capturing
       group performs even for its final branch. *)
    let rec compile_group (bra_off : int) : int =
      let op = byte bra_off in
      let is_capture = Int.equal op Op.op_cbra || Int.equal op Op.op_scbra in
      let is_sbra = Int.equal op Op.op_sbra in
      let is_bra = Int.equal op Op.op_bra in
      (* Possessive brackets (OP_*POS) use the KETRPOS frame-juggling protocol
         (pcre2_match.c:5283-5328), which resists the minimal-save design —
         declined to chunk G (fast-design.md §3). *)
      if not (is_bra || is_capture || is_sbra) then
        raise (Unsupported (reason_of_op op));
      (* grouploop lowering (ALT for every branch + t_fail) applies to the
         brackets the C dispatches through GROUPLOOP — OP_CBRA/OP_SCBRA
         (capturing) and OP_SBRA (empty-checking non-capturing); OP_BRA keeps
         bra_loop lowering (interpreter.ml:2416-2452). *)
      let grouploop = is_capture || is_sbra in
      let ovbase =
        if is_capture then (
          let num = C.get2 src (bra_off + 1 + Limits.link_size) in
          (* optimized_cbracket gate: a referenced capture would need its
             ovector slot to survive as a completed value mid-match, so the
             direct-slot scratch used by CAP_START/CAP_END is invalid —
             decline (the referencing opcode is itself out of subset). *)
          if not opt_cbracket.(num) then
            raise (Unsupported "fast: referenced capture (chunk F/J/K)");
          2 * num)
        else -1
      in
      (* Collect branch-body start offsets by following the BRA/ALT link
         chain, and the trailing KET offset (mirrors the interpreter's
         GET-based branch walk, pcre2_match.c:5349-5372/5894-5897). *)
      let rec collect (p : int) (acc : int list) : int list * int =
        let start = p + Op.op_lengths.(byte p) in
        let nxt = p + link p in
        if Int.equal (byte nxt) Op.op_alt then
          (collect [@tailcall]) nxt (start :: acc)
        else (List.rev (start :: acc), nxt)
      in
      let branch_offs, ket_off = collect bra_off [] in
      let ket_op = byte ket_off in
      (* Chunk D2: KETRMAX/KETRMIN are lowered as repeating kets; the
         possessive KETRPOS is declined to chunk G (its bracket was already
         declined above, so this only fires defensively). Any other ket is
         out of subset. *)
      let is_max = Int.equal ket_op Op.op_ketrmax in
      let is_min = Int.equal ket_op Op.op_ketrmin in
      let repeating = is_max || is_min in
      if not (Int.equal ket_op Op.op_ket || repeating) then
        raise (Unsupported (reason_of_op ket_op));
      (* The empty-string loop check runs at the ket for grouploop brackets
         only (C's P != NULL — pcre2_match.c:5922/6107); an OP_BRA repeat has
         P == NULL, so it never force-breaks (and can never match empty). A
         tracked group records its per-iteration start in [mb.group_start.(g)]
         via a t_group_start at the group entry. *)
      let group_id =
        if repeating && grouploop then fresh_group () else Ir.no_group
      in
      (* [entry_pc] is the IR index a repeating ket loops back to: the
         t_group_start (grouploop) or the BRA marker (bra_loop). *)
      let entry_pc = here () in
      if repeating && grouploop then (
        push Ir.t_group_start;
        push group_id);
      if is_capture then (
        push Ir.t_cap_start;
        push ovbase);
      push Ir.t_bra;
      let nbr = List.length branch_offs in
      (* End-of-branch jumps to fix up to the KET once its index is known. *)
      let jmp_fixups = ref [] in
      (* Operand slot of the previous branch's choice point, to fix up to
         THIS branch's entry (fast-design.md §3: an ALT's handler = the next
         alternative's entry). *)
      let prev_cp = ref (-1) in
      List.iteri
        (fun i b_off ->
          let entry = here () in
          if !prev_cp >= 0 then set !prev_cp entry;
          if i < nbr - 1 || grouploop then (
            (* A branch that records a choice point (every non-last branch,
               plus a grouploop group's last branch): emit its ALT, body,
               then a jump to the group KET. *)
            push Ir.t_alt;
            let cp_operand = here () in
            push 0 (* handler placeholder, resolved at next branch entry *);
            prev_cp := cp_operand;
            ignore (compile_branch b_off : int);
            push Ir.t_jmp;
            let j = here () in
            push 0 (* target placeholder, resolved after the KET *);
            jmp_fixups := j :: !jmp_fixups)
          else (
            (* bra_loop last branch: no choice point, no jump — flows into
               the KET at the enclosing frame's depth. *)
            prev_cp := -1;
            ignore (compile_branch b_off : int)))
        branch_offs;
      if grouploop then (
        (* The last ALT's handler resolves to a t_fail: backtracking out of
           the exhausted grouploop group propagates NOMATCH (no tick). *)
        let fail_pc = here () in
        push Ir.t_fail;
        if !prev_cp >= 0 then set !prev_cp fail_pc;
        prev_cp := -1);
      (* The ket. For a capture the CAP_END writes the group's end slot
         (pcre2_match.c:6077-6084) — and, for a repeat, precedes the repeat
         logic exactly as the C writes captures before the empty-string test.
         A repeating ket then loops back to [entry_pc] or continues past
         (t_ket_rmax/t_ket_rmin); a non-repeating non-capturing group keeps
         its structural t_ket marker (chunk D). Branch JMPs target [ket_ir]
         (the CAP_END for captures, else the first ket instruction). *)
      let ket_ir = here () in
      if is_capture then (
        push Ir.t_cap_end;
        push ovbase);
      if repeating then (
        push (if is_max then Ir.t_ket_rmax else Ir.t_ket_rmin);
        push entry_pc;
        push group_id)
      else if not is_capture then push Ir.t_ket;
      List.iter (fun j -> set j ket_ir) !jmp_fixups;
      ket_off + Op.op_lengths.(ket_op)
    (* [compile_branch p0]: lower one branch body; stops at (without
       consuming) the branch terminator (OP_ALT or an OP_KET family opcode)
       that closes the enclosing group; returns that terminator's offset. *)
    and compile_branch (p0 : int) : int =
      let p = ref p0 in
      let running = ref true in
      while !running do
        let op = byte !p in
        if Int.equal op Op.op_alt || (op >= Op.op_ket && op <= Op.op_ketrpos)
        then running := false
        else if is_char_repeat op then (
          (* Single-char repeat superinstruction (fast-design.md §2; the
             OP_STAR..OP_NOTPOSUPTOI arms interpreter.ml:1718-1806). Operands
             pre-decoded: reptype/lmin/lmax and the char (caseless carries
             the fcc fold-pair, exactly as repeatchar_tail's Loc =
             mb->fcc[Lc], pcre2_match.c:1393). *)
          let ir_tag, caseless, fidx = rep_kind op in
          let count =
            if rep_has_count fidx then C.get2 src (!p + 1) else 0
          in
          let reptype, lmin, lmax = rep_bounds fidx count in
          let char_off =
            if rep_has_count fidx then !p + 1 + Limits.imm2_size else !p + 1
          in
          let c1 = byte char_off in
          push ir_tag;
          push reptype;
          push lmin;
          push lmax;
          push c1;
          if caseless then push (Chartables.fcc c1);
          p := !p + Op.op_lengths.(op))
        else if Int.equal op Op.op_char then (
          (* pcre2_jit_compile.c:7479 (byte_sequence_compare) — fuse
             consecutive caseful OP_CHARs into one CHAR_RUN over the literal
             pool. Non-UTF: each OP_CHAR item is [opcode; one code unit]
             (OP_lengths[OP_CHAR] = 2), so each contributes one byte to
             [lit] and CHAR_RUN's byte length = bytes added. *)
          let off = Buffer.length lit in
          let q = ref !p in
          while Int.equal (byte !q) Op.op_char do
            Buffer.add_char lit (Bytes.get src (!q + 1));
            q := !q + Op.op_lengths.(Op.op_char)
          done;
          let len = Buffer.length lit - off in
          push Ir.t_char_run;
          push off;
          push len;
          p := !q)
        else if Int.equal op Op.op_chari then (
          (* OP_CHARI: one CHARI instruction per code unit — a DELIBERATE
             conservative simplification, not JIT parity: the vendored JIT
             DOES fuse caseless runs when a code unit's othercase differs by
             a single bit (or it has none) — compile_charn_matchingpath
             (pcre2_jit_compile.c:9334-9396, reached for both OP_CHAR and
             OP_CHARI at :12542-12547) concatenates them into one
             byte_sequence_compare (:9392) with caseless=true. Declining the
             fusion is behavior-identical (CHARI creates no choice point, so
             no limit-tick divergence); caseless fusion is left as a possible
             chunk-M optimization citing those lines. The runner folds case
             with Chartables exactly as the interpreter's non-UTF CHARI arm
             (pcre2_match.c:1097-1103). *)
          push Ir.t_chari;
          push (byte (!p + 1));
          p := !p + Op.op_lengths.(Op.op_chari))
        else if
          Int.equal op Op.op_bra || Int.equal op Op.op_cbra
          || Int.equal op Op.op_scbra || Int.equal op Op.op_sbra
        then p := compile_group !p
        else if Int.equal op Op.op_brazero || Int.equal op Op.op_braminzero
        then (
          (* OP_BRAZERO / OP_BRAMINZERO (pcre2_match.c:5224-5238) — the greedy
             / lazy zero-repeat wrapper preceding a group. Both lower to a
             choice point (KIND_CONT) plus the group body; BRAZERO falls
             through into the group and skips on backtrack, BRAMINZERO jumps
             past the group and enters it on backtrack (fast-design.md §3).
             The skip target (past the group) is patched after the group is
             lowered. OP_lengths[OP_BRAZERO] = 1, so the group starts at
             !p + 1. *)
          push (if Int.equal op Op.op_brazero then Ir.t_brazero else Ir.t_braminzero);
          let skip_operand = here () in
          push 0 (* skip placeholder, resolved past the group *);
          let after = compile_group (!p + Op.op_lengths.(op)) in
          set skip_operand (here ());
          p := after)
        else if Int.equal op Op.op_skipzero then (
          (* OP_SKIPZERO (pcre2_match.c:5242-5246) — a {0}-quantified group:
             never entered, so emit no IR; just advance the bytecode pointer
             past the whole (dead) group, exactly as the C does
             (Fecode++; skip alts; += 1 + LINK_SIZE). Skipping the walk means
             an unsupported construct INSIDE a {0} group cannot decline the
             pattern — strictly safe, since the group is dead code. *)
          let bra_off = !p + Op.op_lengths.(op) in
          let rec find_ket (q : int) : int =
            let nxt = q + link q in
            if Int.equal (byte nxt) Op.op_alt then (find_ket [@tailcall]) nxt
            else nxt
          in
          let ket_off = find_ket bra_off in
          p := ket_off + Op.op_lengths.(byte ket_off))
        else if Int.equal op Op.op_not || Int.equal op Op.op_noti then (
          (* Single negated char (pcre2_match.c:1107-1174 non-UTF;
             interpreter.ml:1701-1717): "not this char", caseless via fcc for
             OP_NOTI. Lowered as a NOT-char repeat with lmin=lmax=1 — min==max
             creates no choice point and no tick, identical to the single arm
             (repeatnotchar's Lmin==Lmax continue, pcre2_match.c:1483-1485). *)
          let caseless = Int.equal op Op.op_noti in
          let c1 = byte (!p + 1) in
          push (if caseless then Ir.t_notrepi else Ir.t_notrep);
          push Ir.reptype_min (* unread when lmin=lmax *);
          push 1;
          push 1;
          push c1;
          if caseless then push (Chartables.fcc c1);
          p := !p + Op.op_lengths.(op))
        else if Int.equal op Op.op_class || Int.equal op Op.op_nclass then (
          (* OP_CLASS / OP_NCLASS (pcre2_match.c:1933-1972). In non-UTF every
             code unit is 0..255, so both opcodes reduce to the 32-byte bitmap
             test (class_bit, interpreter.ml:721); the bitmap stays in re.code
             and the IR records its byte offset (like the JIT / XCLASS, no
             copy). A following OP_CR* quantifier folds into a t_class_rep
             (pcre2_match.c:1939-1966); otherwise a single t_class. *)
          let map_off = !p + 1 in
          let after = !p + Op.op_lengths.(op) (* past the 32-byte bitmap *) in
          let nx = byte after in
          if is_cr_op nx then (
            let reptype, lmin, lmax, crlen = cr_bounds src after in
            push Ir.t_class_rep;
            push reptype;
            push lmin;
            push lmax;
            push map_off;
            p := after + crlen)
          else (
            push Ir.t_class;
            push map_off;
            p := after))
        else if op >= Op.op_typestar && op <= Op.op_typeposupto then (
          (* Character-type repeat OP_TYPESTAR..OP_TYPEPOSUPTO
             (pcre2_match.c:2651-2701). The type opcode follows the optional
             IMM2 count (UPTO/MINUPTO/EXACT/POSUPTO); a \p/\P or \X type needs
             the property machinery, declined to chunk I. *)
          let fidx = op - Op.op_typestar in
          let count = if rep_has_count fidx then C.get2 src (!p + 1) else 0 in
          let reptype, lmin, lmax = rep_bounds fidx count in
          let type_off =
            if rep_has_count fidx then !p + 1 + Limits.imm2_size else !p + 1
          in
          let tp = byte type_off in
          if not (is_supported_type tp) then
            raise (Unsupported (reason_of_op tp));
          push Ir.t_type_rep;
          push reptype;
          push lmin;
          push lmax;
          push tp;
          p := !p + Op.op_lengths.(op))
        else if is_supported_type op then (
          (* Single character type (pcre2_match.c:2305-2470 non-UTF; the
             \D \d \S \s \W \w / . (OP_ANY) / \C (OP_ALLANY) / \R (OP_ANYNL) /
             \h \H \v \V single arms). One code unit consumed; no choice
             point, no tick. *)
          push Ir.t_type;
          push op;
          p := !p + Op.op_lengths.(op))
        else if
          Int.equal op Op.op_word_boundary
          || Int.equal op Op.op_not_word_boundary
        then (
          (* \b / \B, non-UCP (pcre2_match.c:6258-6333 /
             interpreter.ml:3181-3332). want=1 for OP_WORD_BOUNDARY (\b),
             want=0 for OP_NOT_WORD_BOUNDARY (\B). *)
          push Ir.t_wordbound;
          push (if Int.equal op Op.op_word_boundary then 1 else 0);
          p := !p + Op.op_lengths.(op))
        else
          match anchor_tag op with
          | Some tag ->
              push tag;
              p := !p + Op.op_lengths.(op)
          | None -> raise (Unsupported (reason_of_op op))
      done;
      !p
    in
    match
      let after = compile_group 0 in
      let end_op = byte after in
      if not (Int.equal end_op Op.op_end) then
        raise (Unsupported (reason_of_op end_op));
      push Ir.t_end;
      {
        Ir.code = Array.sub !buf 0 !n;
        lit = Buffer.contents lit;
        re;
        n_groups = !n_groups;
      }
    with
    | ir -> Ok ir
    | exception Unsupported reason -> Error reason
