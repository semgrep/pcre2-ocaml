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
  else if Int.equal op Op.op_not_word_boundary then named "\\B" "C2+"
  else if Int.equal op Op.op_word_boundary then named "\\b" "C2+"
  else if Int.equal op Op.op_not_ucp_word_boundary then named "\\B (ucp)" "C2+"
  else if Int.equal op Op.op_ucp_word_boundary then named "\\b (ucp)" "C2+"
  else if Int.equal op Op.op_circm then named "multiline ^" "C2+"
  else if Int.equal op Op.op_dollm then named "multiline $" "C2+"
    (* UTF / UCP property machinery (chunk I). *)
  else if Int.equal op Op.op_prop then named "\\p" "I"
  else if Int.equal op Op.op_notprop then named "\\P" "I"
  else if Int.equal op Op.op_extuni then named "\\X" "I"
    (* Character types and \R / \h \H \v \V and any/allany/anybyte (chunk E). *)
  else if op >= Op.op_not_digit && op <= Op.op_anybyte then
    named "character type" "E"
  else if op >= Op.op_anynl && op <= Op.op_vspace then named "character type" "E"
    (* Negated single chars (chunk E). *)
  else if Int.equal op Op.op_not || Int.equal op Op.op_noti then
    named "negated char" "E"
    (* Single-char and type repeats OP_STAR..OP_TYPEPOSUPTO (chunk D). *)
  else if op >= Op.op_star && op <= Op.op_typeposupto then named "char repeat" "D"
    (* Class / ref repeat quantifiers OP_CRSTAR..OP_CRPOSRANGE (chunk D). *)
  else if op >= Op.op_crstar && op <= Op.op_crposrange then
    named "class/ref repeat" "D"
    (* Classes (chunk E). *)
  else if Int.equal op Op.op_class then named "OP_CLASS" "E"
  else if Int.equal op Op.op_nclass then named "OP_NCLASS" "E"
  else if Int.equal op Op.op_xclass then named "OP_XCLASS" "E"
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
    (* Repeated groups: a KET that repeats (chunk D). *)
  else if Int.equal op Op.op_ketrmax then named "repeated group (KETRMAX)" "D"
  else if Int.equal op Op.op_ketrmin then named "repeated group (KETRMIN)" "D"
  else if Int.equal op Op.op_ketrpos then named "repeated group (KETRPOS)" "D"
    (* Lookaround / lookbehind reverse / atomic (chunk G). *)
  else if Int.equal op Op.op_reverse then named "OP_REVERSE" "G"
  else if Int.equal op Op.op_vreverse then named "OP_VREVERSE" "G"
  else if op >= Op.op_assert && op <= Op.op_assertback_na then
    named "lookaround" "G"
  else if Int.equal op Op.op_once then named "OP_ONCE" "G"
    (* Groups chunk C1 does not lower: possessive / capturing / empty-checking
       brackets (chunk D). *)
  else if Int.equal op Op.op_brapos then named "OP_BRAPOS" "D"
  else if Int.equal op Op.op_cbra then named "OP_CBRA" "D"
  else if Int.equal op Op.op_cbrapos then named "OP_CBRAPOS" "D"
  else if Int.equal op Op.op_sbra then named "OP_SBRA" "D"
  else if Int.equal op Op.op_sbrapos then named "OP_SBRAPOS" "D"
  else if Int.equal op Op.op_scbra then named "OP_SCBRA" "D"
  else if Int.equal op Op.op_scbrapos then named "OP_SCBRAPOS" "D"
  else if Int.equal op Op.op_close then named "OP_CLOSE" "D"
  else if Int.equal op Op.op_skipzero then named "OP_SKIPZERO" "D"
  else if
    op >= Op.op_brazero && op <= Op.op_braposzero
  then named "OP_BRAZERO" "D"
    (* Conditionals (chunk J). *)
  else if Int.equal op Op.op_cond then named "OP_COND" "J"
  else if Int.equal op Op.op_scond then named "OP_SCOND" "J"
  else if op >= Op.op_cref && op <= Op.op_true then named "conditional ref" "J"
  else if Int.equal op Op.op_define then named "OP_DEFINE" "J"
    (* Backtracking control verbs and forced success/failure (chunk H). *)
  else if op >= Op.op_mark && op <= Op.op_assert_accept then named "verb" "H"
  else "fast: opcode " ^ string_of_int op ^ " (unclassified)"

(* Anchor opcode -> IR tag for the chunk C1 subset; [None] otherwise. *)
let anchor_tag (op : int) : int option =
  if Int.equal op Op.op_sod then Some Ir.t_sod
  else if Int.equal op Op.op_som then Some Ir.t_som
  else if Int.equal op Op.op_eod then Some Ir.t_eod
  else if Int.equal op Op.op_eodn then Some Ir.t_eodn
  else if Int.equal op Op.op_circ then Some Ir.t_circ
  else if Int.equal op Op.op_doll then Some Ir.t_doll
  else None

let compile (re : C.re) : (Ir.t, string) result =
  (* Compile-level gates BEFORE the walk (fast-design.md §6, task spec). *)
  if not (Int.equal (re.C.overall_options land Opt.utf) 0) then
    Error "fast: UTF mode (chunk I)"
  else if not (Int.equal (re.C.overall_options land Opt.ucp) 0) then
    Error "fast: UCP mode (chunk I)"
  else if re.C.top_bracket > 0 then Error "fast: capturing groups (chunk D)"
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

    (* [compile_group bra_off]: lower the OP_BRA...OP_KET group starting at
       [bra_off]; returns the bytecode offset just past its KET. Native
       choice-point lowering per fast-design.md §3. *)
    let rec compile_group (bra_off : int) : int =
      let op = byte bra_off in
      if not (Int.equal op Op.op_bra) then raise (Unsupported (reason_of_op op));
      push Ir.t_bra;
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
      (* A KETRMAX/KETRMIN/KETRPOS bracket is a repeated group (chunk D). *)
      if not (Int.equal ket_op Op.op_ket) then
        raise (Unsupported (reason_of_op ket_op));
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
          if i < nbr - 1 then (
            (* Non-last branch: emit its choice point, body, then a jump to
               the group KET. *)
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
            (* Last branch: no choice point, no jump — flows into the KET. *)
            prev_cp := -1;
            ignore (compile_branch b_off : int)))
        branch_offs;
      let ket_ir = here () in
      push Ir.t_ket;
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
        else if Int.equal op Op.op_bra then p := compile_group !p
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
      { Ir.code = Array.sub !buf 0 !n; lit = Buffer.contents lit; re }
    with
    | ir -> Ok ir
    | exception Unsupported reason -> Error reason
