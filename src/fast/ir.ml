(* Fast-engine IR (M11 chunk C1). This is engine-native code: it has no
   direct PCRE2 counterpart, so per port-conventions.md §9 the design blocks
   cite the design doc (fast-design.md §2/§3) rather than a C source range.

   The IR is a flat [int array] ([code]) of pre-decoded instructions plus a
   [lit] literal pool for fused character runs. [pc] indexes instruction
   heads: each head is [tag; operand...] where [tag] is a Fast-private dense
   enum (0..max_tag, jump-table friendly — fast-design.md §2). Jump targets
   are stored as ABSOLUTE IR indices (resolved at IR-compile time, no
   run-time link reads). Variable-length payloads (none in chunk C1) will be
   left in [re.code] and referenced by offset, so [t] pins [Compile.re]. *)

module C = Pcre2_engine.Compile

(* fast-design.md §2 — the dense instruction tags. Values are contiguous
   from 0 and never change meaning across chunks (coverage only widens); the
   runner (chunk C2) dispatches on them with a literal-int match. *)

let t_end = 0 (* [t_end]                — end of program (accept) *)
let t_char_run = 1 (* [t_char_run; lit_off; len] — fused caseful literal run *)
let t_chari = 2 (* [t_chari; ch]         — one caseless char (never fused) *)
let t_bra = 3 (* [t_bra]                — group entry marker (structural nop) *)
let t_ket = 4 (* [t_ket]                — group exit marker (structural nop) *)
let t_alt = 5 (* [t_alt; next]          — choice point: [next] = IR index of *)
(*                          the next alternative's entry (handler) *)
let t_jmp = 6 (* [t_jmp; target]        — end-of-branch jump to the group KET *)
let t_sod = 7 (* [t_sod]                — \A (start of subject) *)
let t_som = 8 (* [t_som]                — \G (start of match) *)
let t_eod = 9 (* [t_eod]                — \z (end of subject) *)
let t_eodn = 10 (* [t_eodn]               — \Z (end, or newline at end) *)
let t_circ = 11 (* [t_circ]               — ^ (non-multiline) *)
let t_doll = 12 (* [t_doll]               — $ (non-multiline) *)

(* Chunk D additions (fast-design.md §2). *)
let t_circm = 13 (* [t_circm]              — ^ multiline (OP_CIRCM) *)
let t_dollm = 14 (* [t_dollm]              — $ multiline (OP_DOLLM) *)
let t_fail = 15 (* [t_fail]               — grouploop exhausted: backtrack *)
let t_cap_start = 16 (* [t_cap_start; ovbase]  — open capture N (ovbase = 2N) *)
let t_cap_end = 17 (* [t_cap_end; ovbase]    — close capture N (the group KET) *)

(* Single-char repeat superinstructions. reptype ∈ {0=min,1=max,2=pos};
   lmin/lmax pre-decoded (lmax = [rep_inf] for STAR/PLUS); the char pool is
   inline in the operands (caseless carries the fold-pair c1/c2). *)
let t_rep = 18 (* [t_rep;    reptype; lmin; lmax; c]      — caseful char rep *)
let t_repi = 19 (* [t_repi;   reptype; lmin; lmax; c1; c2] — caseless char rep *)
let t_notrep = 20 (* [t_notrep; reptype; lmin; lmax; c]      — caseful NOT rep *)
let t_notrepi = 21 (* [t_notrepi;reptype; lmin; lmax; c1; c2] — caseless NOT rep *)

(* Chunk D2 additions (fast-design.md §2/§3) — quantified and optional groups.
   A repeated group's entry records its per-iteration start position (for the
   empty-string loop check) in [mb.group_start.(g)]; its ket loops back to the
   group entry ([t_group_start]/the bracket) or continues past. Optional-group
   wrappers OP_BRAZERO/OP_BRAMINZERO become a choice point (KIND_CONT). *)
let t_group_start = 22 (* [t_group_start; g] — record group g's iter start *)
let t_brazero = 23 (* [t_brazero; skip]  — greedy optional: try group, skip on bt *)
let t_braminzero = 24 (* [t_braminzero; skip] — lazy optional: skip, enter on bt *)
let t_ket_rmax = 25 (* [t_ket_rmax; entry; g] — greedy repeating ket (OP_KETRMAX) *)
let t_ket_rmin = 26 (* [t_ket_rmin; entry; g] — lazy repeating ket (OP_KETRMIN) *)

(* Sentinel [g] for a repeated group whose bracket is OP_BRA (bra_loop, C's
   P == NULL): NO empty-string check (the C short-circuits it, and OP_BRA can
   never match empty), so no [t_group_start] and no [mb.group_start] slot. *)
let no_group = -1

(* Repeat type constants (mirror interpreter.ml:106-108 reptype min/max/pos). *)
let reptype_min = 0
let reptype_max = 1
let reptype_pos = 2

(* [lmax] sentinel for an unbounded repeat (STAR/PLUS/POSSTAR/POSPLUS):
   0xFFFFFFFF, exactly the interpreter's uint32_max (interpreter.ml:115). *)
let rep_inf = 0xFFFFFFFF

(* fast-design.md §2 — highest valid tag; used by the verifier and dump. *)
let max_tag = 26

(* fast-design.md §2 — instruction WIDTH in ints (tag + operands), indexed
   by tag. The verifier walks [code] by these widths; the runner advances by
   them. Keep in step with the tag list above. *)
let arity =
  [|
    1 (* t_end *);
    3 (* t_char_run: lit_off, len *);
    2 (* t_chari: ch *);
    1 (* t_bra *);
    1 (* t_ket *);
    2 (* t_alt: next *);
    2 (* t_jmp: target *);
    1 (* t_sod *);
    1 (* t_som *);
    1 (* t_eod *);
    1 (* t_eodn *);
    1 (* t_circ *);
    1 (* t_doll *);
    1 (* t_circm *);
    1 (* t_dollm *);
    1 (* t_fail *);
    2 (* t_cap_start: ovbase *);
    2 (* t_cap_end: ovbase *);
    5 (* t_rep: reptype, lmin, lmax, c *);
    6 (* t_repi: reptype, lmin, lmax, c1, c2 *);
    5 (* t_notrep: reptype, lmin, lmax, c *);
    6 (* t_notrepi: reptype, lmin, lmax, c1, c2 *);
    2 (* t_group_start: g *);
    2 (* t_brazero: skip *);
    2 (* t_braminzero: skip *);
    3 (* t_ket_rmax: entry, g *);
    3 (* t_ket_rmin: entry, g *);
  |]

(* fast-design.md §2 — textual tag names for [dump] (golden tests) and the
   verifier's diagnostics. Indexed by tag. *)
let tag_name =
  [|
    "END";
    "CHAR_RUN";
    "CHARI";
    "BRA";
    "KET";
    "ALT";
    "JMP";
    "SOD";
    "SOM";
    "EOD";
    "EODN";
    "CIRC";
    "DOLL";
    "CIRCM";
    "DOLLM";
    "FAIL";
    "CAP_START";
    "CAP_END";
    "REP";
    "REPI";
    "NOTREP";
    "NOTREPI";
    "GROUP_START";
    "BRAZERO";
    "BRAMINZERO";
    "KET_RMAX";
    "KET_RMIN";
  |]

(* fast-design.md §2 — the compiled fast program. [code] is the flat
   instruction stream; [lit] backs [t_char_run] items; [re] is pinned as the
   source of any (future) variable-length payloads and of the options /
   top_bracket the runner (chunk C2) needs. [n_groups] (chunk D2) is the
   number of empty-check-tracked repeated groups: the runner allocates a
   [group_start] array of that size, indexed by the group id in
   [t_group_start]/[t_ket_rmax]/[t_ket_rmin]. *)
type t = { code : int array; lit : string; re : C.re; n_groups : int }

(* ---------- Decode accessors (fast-design.md §2) ----------
   Positional reads used by the runner (chunk C2) and the verifier. They
   assume [pc] is an instruction head of the stated tag (the verifier proves
   both once per compile). *)

let tag (ir : t) (pc : int) : int = ir.code.(pc)
let width (t : int) : int = arity.(t)

(* [t_char_run] operands. *)
let char_run_off (ir : t) (pc : int) : int = ir.code.(pc + 1)
let char_run_len (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* [t_chari] operand — the pattern code unit (non-UTF; the runner folds case
   with Chartables exactly as the interpreter's non-UTF CHARI arm does). *)
let chari_char (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_alt] operand — absolute IR index of the next alternative's entry (the
   save-record handler slot, fast-design.md §3). *)
let alt_next (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_jmp] operand — absolute IR index of the enclosing group's KET. *)
let jmp_target (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_cap_start] / [t_cap_end] operand — the ovector base index 2N of the
   captured group (its slots are [ovbase], [ovbase+1]). *)
let cap_ovbase (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* Repeat operands. reptype/lmin/lmax are common to all four repeat tags;
   [rep_c1]/[rep_c2] are the char (caseful) or fold-pair (caseless). *)
let rep_reptype (ir : t) (pc : int) : int = ir.code.(pc + 1)
let rep_lmin (ir : t) (pc : int) : int = ir.code.(pc + 2)
let rep_lmax (ir : t) (pc : int) : int = ir.code.(pc + 3)
let rep_c1 (ir : t) (pc : int) : int = ir.code.(pc + 4)

(* Only [t_repi]/[t_notrepi] carry a second (other-case) char at offset 5. *)
let rep_c2 (ir : t) (pc : int) : int = ir.code.(pc + 5)

(* Chunk D2 operands (fast-design.md §2/§3). *)

(* [t_group_start] operand — the repeated group's id (index into
   [mb.group_start]); the arm records the current position there. *)
let group_start_id (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_brazero]/[t_braminzero] operand — the IR index PAST the group (the
   skip target: BRAZERO resumes here on backtrack; BRAMINZERO jumps here on
   entry and resumes at the group on backtrack). *)
let braz_skip (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_ket_rmax]/[t_ket_rmin] operands — [entry] is the IR index of the group
   entry to loop back to; [g] is the empty-check group id ([no_group] = no
   check). The continuation past the ket is [pc + 3]. *)
let ket_entry (ir : t) (pc : int) : int = ir.code.(pc + 1)
let ket_group (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* ---------- Text dump (fast-design.md §2) ----------
   Stable, debug_printer.ml-style listing for golden tests: one line per
   instruction, [%3d TAG operands]. Printf/Format here is a debug path
   (sanctioned by port-conventions §7). Robust against a corrupted tag so it
   can also aid debugging of hand-patched IR. *)

let printable (c : int) : bool =
  c >= 32 && c < 127 && (not (Int.equal c (Char.code '"')))
  && not (Int.equal c (Char.code '\\'))

let add_escaped (buf : Buffer.t) (c : int) : unit =
  if printable c then Buffer.add_char buf (Char.chr c)
  else Buffer.add_string buf (Printf.sprintf "\\x%02x" c)

let escaped_sub (s : string) (off : int) (len : int) : string =
  let buf = Buffer.create (len + 2) in
  for i = off to off + len - 1 do
    add_escaped buf (Char.code s.[i])
  done;
  Buffer.contents buf

let render (ir : t) (pc : int) (t : int) : string =
  if Int.equal t t_char_run then
    Printf.sprintf "CHAR_RUN \"%s\""
      (escaped_sub ir.lit (char_run_off ir pc) (char_run_len ir pc))
  else if Int.equal t t_chari then (
    let buf = Buffer.create 8 in
    Buffer.add_string buf "CHARI \"";
    add_escaped buf (chari_char ir pc);
    Buffer.add_char buf '"';
    Buffer.contents buf)
  else if Int.equal t t_alt then Printf.sprintf "ALT next=%d" (alt_next ir pc)
  else if Int.equal t t_jmp then Printf.sprintf "JMP %d" (jmp_target ir pc)
  else if Int.equal t t_cap_start then
    Printf.sprintf "CAP_START ovbase=%d" (cap_ovbase ir pc)
  else if Int.equal t t_cap_end then
    Printf.sprintf "CAP_END ovbase=%d" (cap_ovbase ir pc)
  else if Int.equal t t_group_start then
    Printf.sprintf "GROUP_START g=%d" (group_start_id ir pc)
  else if Int.equal t t_brazero then
    Printf.sprintf "BRAZERO skip=%d" (braz_skip ir pc)
  else if Int.equal t t_braminzero then
    Printf.sprintf "BRAMINZERO skip=%d" (braz_skip ir pc)
  else if Int.equal t t_ket_rmax then
    Printf.sprintf "KET_RMAX entry=%d g=%d" (ket_entry ir pc) (ket_group ir pc)
  else if Int.equal t t_ket_rmin then
    Printf.sprintf "KET_RMIN entry=%d g=%d" (ket_entry ir pc) (ket_group ir pc)
  else if
    Int.equal t t_rep || Int.equal t t_repi || Int.equal t t_notrep
    || Int.equal t t_notrepi
  then (
    let ty = rep_reptype ir pc in
    let tystr =
      if Int.equal ty reptype_min then "min"
      else if Int.equal ty reptype_max then "max"
      else "pos"
    in
    let lmax = rep_lmax ir pc in
    let lmaxstr = if Int.equal lmax rep_inf then "inf" else string_of_int lmax in
    let buf = Buffer.create 24 in
    Buffer.add_string buf tag_name.(t);
    Buffer.add_char buf ' ';
    Buffer.add_string buf tystr;
    Buffer.add_string buf (Printf.sprintf " {%d,%s} \"" (rep_lmin ir pc) lmaxstr);
    add_escaped buf (rep_c1 ir pc);
    if Int.equal t t_repi || Int.equal t t_notrepi then (
      Buffer.add_char buf '/';
      add_escaped buf (rep_c2 ir pc));
    Buffer.add_char buf '"';
    Buffer.contents buf)
  else tag_name.(t)

let dump (ppf : Format.formatter) (ir : t) : unit =
  let code = ir.code in
  let len = Array.length code in
  let rec go (pc : int) : unit =
    if pc >= len then ()
    else
      let t = code.(pc) in
      if t < 0 || t > max_tag then Format.fprintf ppf "%3d <bad tag %d>\n" pc t
      else (
        Format.fprintf ppf "%3d %s\n" pc (render ir pc t);
        (go [@tailcall]) (pc + arity.(t)))
  in
  go 0
