(* Horizontal / vertical space byte predicates, shared between the mainline
   interpreter (interpreter.ml) and the M11 fast engine (src/fast/runner.ml).

   These are a pure extraction of the two module-level helpers that lived in
   interpreter.ml (identical bodies, same citations); the interpreter now
   aliases them so the two engines cannot diverge on the \h / \H / \v / \V byte
   tests. No behavior change — proved by the full engine conformance
   (--driver=engine) staying byte-identical. *)

(* pcre2_internal.h:424-427 — HSPACE_BYTE_CASES: HT, SPACE, NBSP. The 8-bit
   code-unit switches in pcre2_match.c use only these (the
   HSPACE_MULTIBYTE_CASES arms are compiled out at PCRE2_CODE_UNIT_WIDTH ==
   8). *)
let hspace_byte (c : int) : bool =
  Int.equal c 0x09 || Int.equal c 0x20 || Int.equal c 0xa0

(* pcre2_internal.h:440-445 — VSPACE_BYTE_CASES: LF, VT, FF, CR, NEL. *)
let vspace_byte (c : int) : bool =
  Int.equal c Newline.char_lf
  || Int.equal c Newline.char_vt
  || Int.equal c Newline.char_ff
  || Int.equal c Newline.char_cr
  || Int.equal c Newline.char_nel

(* pcre2_internal.h:416-431 — HSPACE_CASES: the byte cases plus
   HSPACE_MULTIBYTE_CASES, for the UTF single/repeat arms that switch on a
   DECODED code point (\h / \H in UTF mode). Extracted here so the interpreter
   and the M11 fast engine share one definition (no behavior change — the body
   is identical to the interpreter's former hspace_char). *)
let hspace_char (c : int) : bool =
  match c with
  | 0x09 | 0x20 | 0xa0 | 0x1680 | 0x180e | 0x2000 | 0x2001 | 0x2002 | 0x2003
  | 0x2004 | 0x2005 | 0x2006 | 0x2007 | 0x2008 | 0x2009 | 0x200a | 0x202f
  | 0x205f | 0x3000 ->
      true
  | _ -> false

(* pcre2_internal.h:433-449 — VSPACE_CASES: the byte cases plus
   VSPACE_MULTIBYTE_CASES (U+2028 LS, U+2029 PS). *)
let vspace_char (c : int) : bool =
  match c with
  | 0x0a | 0x0b | 0x0c | 0x0d | 0x85 | 0x2028 | 0x2029 -> true
  | _ -> false

(* ---------- Unicode property predicates (M11 chunk I2 extraction) ----------

   [prop_clist_member], [caseless_set_member] and [prop_test] are a pure
   extraction of the module-level helpers that lived in interpreter.ml
   (identical bodies, same citations; the interpreter now aliases them), shared
   with the fast runner so the two engines cannot diverge on OP_PROP/OP_NOTPROP
   / caseless-set semantics. No behavior change — proved by the full engine
   conformance (--driver=engine) staying byte-identical. *)

(* pcre2_match.c:2576-2583 (= 2903-2916, 3723-3736, 4311-4319) — the
   PT_CLIST scan: cp = PRIV(ucd_caseless_sets) + <property value>;
   for (;;) { if (fc < *cp) <no match>; if (fc == *cp++) <match>; }.
   Terminates inside the table: the sets are ascending and end with
   NOTACHAR = 0xffffffff (pcre2_ucd.c:114-143), which exceeds every
   decoded character (getutf8 yields at most 0x7fffffff), so the fc < *cp
   exit always fires by the set's end. *)
let rec prop_clist_member (fc : int) (cp : int) : bool =
  let v = Ucd_tables.ucd_caseless_sets.(cp) in
  if fc < v then false
  else if Int.equal fc v then true
  else (prop_clist_member [@tailcall]) fc (cp + 1)

(* pcre2_match.c:424-429 — match_ref()'s caseless-set probe: walk the
   NOTACHAR-terminated ascending list at [pp] in
   Ucd_tables.ucd_caseless_sets; `if (c < *pp) return -1` is the
   no-match exit (entries ascend and end with NOTACHAR = 0xffffffff,
   larger than any code point). *)
let rec caseless_set_member (c : int) (pp : int) : bool =
  let v = Ucd_tables.ucd_caseless_sets.(pp) in
  if c < v then false
  else if Int.equal c v then true
  else (caseless_set_member [@tailcall]) c (pp + 1)

(* pcre2_match.c:2493-2610 — the Unicode property test shared by the
   OP_PROP/OP_NOTPROP single-character arm and the property repeat loops
   (min 2726-2974, minimize 3515-3801, maximize 4115-4381): does
   character [c] have property ([ptype], [pdata])?

   DEVIATION(structure): the C repeats this switch inline at each of
   those four sites with the notmatch / (Lctype == OP_NOTPROP)
   comparison folded into every case; factored here once — each call
   site keeps its own comparison, control flow and evaluation order.
   Cases in the C's order, with the C's exact category groupings; the
   PT_CLIST arms' fc < */== *cp exits reduce to set membership compared
   against notmatch at the call site, exactly as the other cases.
   Callers exclude property types above PT_BOOL (the C switches'
   defaults: PCRE2_ERROR_INTERNAL). The C fetches the ucd_record once
   per character (GET_UCD); cases probing several record fields do the
   same through [Ucd.record_index]. *)
let prop_test (c : int) (ptype : int) (pdata : int) : bool =
  if Int.equal ptype Opcodes.pt_any then true (* 2495-2497 *)
  else if Int.equal ptype Opcodes.pt_lamp then
    (* pcre2_match.c:2499-2505 *)
    let chartype = Ucd.chartype c in
    Int.equal chartype Ucp.ucp_lu
    || Int.equal chartype Ucp.ucp_ll
    || Int.equal chartype Ucp.ucp_lt
  else if Int.equal ptype Opcodes.pt_gc then
    (* pcre2_match.c:2507-2510 — Fecode[2] ==
       PRIV(ucp_gentype)[prop->chartype]. *)
    Int.equal pdata Tables.ucp_gentype.(Ucd.chartype c)
  else if Int.equal ptype Opcodes.pt_pc then
    (* pcre2_match.c:2512-2515 *)
    Int.equal pdata (Ucd.chartype c)
  else if Int.equal ptype Opcodes.pt_sc then
    (* pcre2_match.c:2517-2520 *)
    Int.equal pdata (Ucd.script c)
  else if Int.equal ptype Opcodes.pt_scx then
    (* pcre2_match.c:2522-2528 — script match, or the Script Extensions
       set bit (MAPBIT bound proof: Ucd.script_set_contains). *)
    let ri = Ucd.record_index c in
    Int.equal pdata (Ucd_tables.script ri) || Ucd.script_set_contains ri pdata
  else if Int.equal ptype Opcodes.pt_alnum then
    (* pcre2_match.c:2532-2537 — these are specials. *)
    let gentype = Tables.ucp_gentype.(Ucd.chartype c) in
    Int.equal gentype Ucp.ucp_l || Int.equal gentype Ucp.ucp_n
  else if Int.equal ptype Opcodes.pt_space || Int.equal ptype Opcodes.pt_pxspace
  then
    (* pcre2_match.c:2539-2557 — Perl space and POSIX space are identical
       since Perl 5.18 / PCRE 8.34: the HSPACE/VSPACE cases, else general
       category Z. *)
    hspace_char c || vspace_char c
    || Int.equal Tables.ucp_gentype.(Ucd.chartype c) Ucp.ucp_z
  else if Int.equal ptype Opcodes.pt_word then
    (* pcre2_match.c:2559-2566 *)
    let chartype = Ucd.chartype c in
    let gentype = Tables.ucp_gentype.(chartype) in
    Int.equal gentype Ucp.ucp_l
    || Int.equal gentype Ucp.ucp_n
    || Int.equal chartype Ucp.ucp_mn
    || Int.equal chartype Ucp.ucp_pc
  else if Int.equal ptype Opcodes.pt_clist then
    (* pcre2_match.c:2568-2584 (the width-32 MAX_UTF guard is compiled
       out in the 8-bit library) *)
    prop_clist_member c pdata
  else if Int.equal ptype Opcodes.pt_ucnc then
    (* pcre2_match.c:2586-2591 — CHAR_DOLLAR_SIGN 0x24,
       CHAR_COMMERCIAL_AT 0x40, CHAR_GRAVE_ACCENT 0x60. *)
    Int.equal c 0x24 || Int.equal c 0x40 || Int.equal c 0x60
    || (c >= 0xa0 && c <= 0xd7ff)
    || c >= 0xe000
  else if Int.equal ptype Opcodes.pt_bidicl then
    (* pcre2_match.c:2593-2596 — UCD_BIDICLASS_PROP(prop) == Fecode[2]. *)
    Int.equal (Ucd.bidiclass c) pdata
  else
    (* PT_BOOL (pcre2_match.c:2598-2604) — MAPBIT over the
       Boolean-property set (bound proof: Ucd.boolprop_set_contains);
       the callers' ptype <= PT_BOOL gate makes this the last case. *)
    Ucd.boolprop_set_contains (Ucd.record_index c) pdata

(* pcre2_match.c:6283-6289 (= 6316-6322) — the UCP word-character probe
   shared by OP_UCP_WORD_BOUNDARY / OP_NOT_UCP_WORD_BOUNDARY for the
   previous and next character: category L or N, or chartype Mn or Pc.
   Extracted with [prop_test] (chunk I2) so the interpreter's word-boundary
   arm and the fast runner's share one definition. *)
let ucp_wordchar (c : int) : bool =
  let chartype = Ucd.chartype c in
  let category = Tables.ucp_gentype.(chartype) in
  Int.equal category Ucp.ucp_l
  || Int.equal category Ucp.ucp_n
  || Int.equal chartype Ucp.ucp_mn
  || Int.equal chartype Ucp.ucp_pc
