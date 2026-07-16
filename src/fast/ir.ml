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
module Op = Pcre2_engine.Opcodes

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

(* Chunk E additions (fast-design.md §2/§4) — character types and classes.
   [t_type] is a single character-type test whose operand is the C type
   opcode (OP_NOT_DIGIT..OP_VSPACE / OP_ANY / OP_ALLANY / OP_ANYBYTE /
   OP_ANYNL); OP_PROP/OP_NOTPROP/OP_EXTUNI have their own tags 59-62 (chunk
   I2). [t_class]
   is a single 32-byte-bitmap class test (OP_CLASS/OP_NCLASS, identical in
   non-UTF where every code unit is 0..255); its operand is the byte offset of
   the bitmap in [re.code] (like the JIT / XCLASS, no copy). [t_wordbound] is
   \b / \B; its operand [want] encodes bit 0 = boundary wanted (1 for
   OP_WORD_BOUNDARY / OP_UCP_WORD_BOUNDARY, 0 for the NOT variants) and, since
   chunk I2, bit 1 = the UCP variant (OP_UCP_WORD_BOUNDARY /
   OP_NOT_UCP_WORD_BOUNDARY use Unicode properties for the word test even
   without UTF, pcre2_match.c:6250-6256). The type/class repeats reuse the REP
   superinstr
   machinery: [t_type_rep] / [t_class_rep] carry [reptype; lmin; lmax] then the
   type opcode / bitmap offset. *)
let t_type = 27 (* [t_type; type_op]  — one char-type test *)
let t_class = 28 (* [t_class; map_off] — one bitmap-class test *)
let t_wordbound = 29 (* [t_wordbound; want] — \b (want=1) / \B (want=0) *)
let t_type_rep = 30 (* [t_type_rep; reptype; lmin; lmax; type_op] *)
let t_class_rep = 31 (* [t_class_rep; reptype; lmin; lmax; map_off] *)

(* Chunk F additions (fast-design.md §2/§3) — backreferences.

   A numbered backref [t_ref] / repeated numbered backref [t_ref_rep]; a
   duplicate-named backref [t_dnref] / repeated dup-named [t_dnref_rep]
   (scan the name-table group list for the first SET group,
   pcre2_match.c:5002-5007). [ovbase] = 2N (fast public ovector convention,
   group N at [2N,2N+1]); [caseless] = 1 for OP_REFI/OP_DNREFI. A DNREF
   stores the name-table byte offset [slot_base] of its first list entry and
   the [count] of entries; the runner resolves the offset at run time
   ([mb.name_table], mirroring dnref_scan). The repeat forms carry the
   pre-decoded [reptype]/[lmin]/[lmax] (only the OP_CRSTAR..OP_CRMINQUERY /
   OP_CRRANGE / OP_CRMINRANGE forms — a possessive ref repeat compiles to an
   atomic group, out of subset, pcre2_match.c:5024-5043).

   Referenced captures use the JIT's non-optimized-cbracket protocol
   (pcre2_jit_compile.c:1145, 11055-11061, 10692-10702): the in-progress
   start is kept in [mb.cap_start] (NOT the ovector slot, which must stay
   UNSET until the group CLOSES so a mid-match backref sees only closed
   values), so [t_cap_start_ref] records the group entry (private-scratch
   save, KIND_CAPSTART) and [t_cap_end_ref] writes BOTH ovector slots at
   close (ovector save, KIND_CAP). *)
let t_ref = 32 (* [t_ref; ovbase; caseless]                          *)
let t_ref_rep = 33 (* [t_ref_rep; reptype; lmin; lmax; ovbase; caseless] *)
let t_dnref = 34 (* [t_dnref; slot_base; count; caseless]              *)

let t_dnref_rep = 35
(* [t_dnref_rep; reptype; lmin; lmax; slot_base; count; caseless] *)

let t_cap_start_ref = 36 (* [t_cap_start_ref; ovbase] — referenced capture entry *)
let t_cap_end_ref = 37 (* [t_cap_end_ref; ovbase]   — referenced capture close *)

(* Chunk G additions (fast-design.md §2/§3) — lookaround and atomic groups.

   [t_reverse]/[t_vreverse] are the lookbehind step-back opcodes (OP_REVERSE /
   OP_VREVERSE, pcre2_match.c:5793-5883): fixed and variable back-step at the
   start of a lookbehind branch. [t_once] is the atomic-group / atomic-assertion
   ENTRY (it pushes the KIND_ONCE boundary that snapshots the group ovector +
   the enclosing [mb.once_base] and enables the atomic COMMIT); [t_once_end] is
   the atomic-GROUP ket (OP_ONCE, pcre2_match.c:6023-6031: commit — discard the
   body's internal choice points). [t_assert_end] is the POSITIVE-assertion ket
   (OP_ASSERT/ASSERTBACK/ASSERT_NA/ASSERTBACK_NA, pcre2_match.c:5999-6016):
   restore eptr to the assertion's entry position ([mb.group_start.(g)], written
   by a preceding [t_group_start]) and, for the atomic kinds ([atomic]=1),
   commit. [t_nassert]/[t_nassert_match] are the NEGATIVE-assertion boundary and
   its "a branch matched" fail action (OP_ASSERT_NOT/ASSERTBACK_NOT,
   pcre2_match.c:5547-5584); [t_assertback_check] is the variable-lookbehind
   end-point check (branch_start[1+LINK_SIZE] == OP_VREVERSE && Feptr != P->eptr,
   pcre2_match.c:5995/6009/6038). *)
let t_reverse = 38 (* [t_reverse; number]   — OP_REVERSE (fixed lookbehind step) *)
let t_vreverse = 39 (* [t_vreverse; lmin; lmax] — OP_VREVERSE (variable step) *)

(* [t_once; cont] — atomic group / atomic|NA positive assertion entry. [cont]
   (chunk K2) is the assertion's continuation pc (just past its ASSERT_END),
   stored in the KIND_ONCE boundary so a ( *ACCEPT) reaching THIS assertion's
   boundary (possibly an INNER one still live between the ACCEPT and its lexical
   enclosing lookaround) commits it and continues at ITS own continuation — the C
   RRETURNs MATCH_ACCEPT to the innermost active assertion frame, not the lexical
   one (pcre2_match.c:5520-5535). For an atomic GROUP (subtype once_group) [cont]
   is unused (-1): a ( *ACCEPT) walk passes through it. *)
let t_once = 40
let t_once_end = 41 (* [t_once_end]          — atomic group ket (commit) *)
let t_assert_end = 42 (* [t_assert_end; atomic; g] — positive assertion ket *)
let t_nassert = 43 (* [t_nassert; g; cont]  — negative assertion entry *)
let t_nassert_match = 44 (* [t_nassert_match]     — negative branch matched (fail) *)
let t_assertback_check = 45 (* [t_assertback_check; g] — variable-lookbehind check *)

(* Chunk G possessive brackets (fast-design.md §2/§3) — OP_BRAPOS / OP_CBRAPOS /
   OP_SBRAPOS / OP_SCBRAPOS + OP_KETRPOS + OP_BRAPOSZERO ((?:X)++, (X)*+, ...).
   A possessive quantified group is a greedy-atomic repeat: match X as many times
   as possible, each iteration COMMITTED (its internal backtracking discarded),
   the whole group giving nothing back (pcre2_match.c:5283-5328). [t_possess] is
   the entry (push a KIND_POS boundary — the KIND_ONCE snapshot + the per-loop
   matched_once / iter_start / zero_allowed / entry_rdepth state); [t_ketrpos] is
   the per-iteration commit + loop-back (OP_KETRPOS, pcre2_match.c:6092-6098);
   [t_possess_done] ends the loop (the RM8 loop `break` + the Lmatched_once ||
   Lzero_allowed success test, pcre2_match.c:5320-5328). [cap_ovbase] = 2N for a
   capturing possessive bracket (0 = non-capturing). *)
let t_possess = 46 (* [t_possess; cap_ovbase; zero_allowed] — possessive entry *)

(* [t_ketrpos; body_entry; cap_ovbase; number] — iteration commit. [number]
   (chunk K2) = the possessive group's recursion number (2N/2 for a capturing
   OP_CBRAPOS/OP_SCBRAPOS, else -1): when [mb.current_recurse == number] the ket
   is a recursion RETURN (pcre2_match.c:6056-6074), NOT the possessive commit —
   a recursion into a possessive capture runs the branches grouploop-style
   (RM11) and returns at the ket like a normal recursion, bypassing the
   possessive loop (:6092 is after the :6065 `continue`). *)
let t_ketrpos = 47

(* [t_possess_done; number] — possessive loop end. [number] (chunk K2) as
   t_ketrpos: during a recursion into this possessive capture the branch
   exhaustion is a plain NOMATCH (the C's :5495 RRETURN(MATCH_NOMATCH)), not the
   possessive success/fail test — checked via [mb.current_recurse == number]. *)
let t_possess_done = 48

(* Chunk H additions (fast-design.md §2/§3) — backtracking control verbs and
   forced accept/close.

   The verbs convert backtracking into scoped propagation (pcre2_match.c:6336-
   6442, interpreter.ml:3322-3397). Each verb whose continuation may fail pushes
   a save record that, on the continuation's exhaustion (NOMATCH), FIRES its verb
   code; that code then propagates up the save stack (runner [backtrack_code]),
   discarding choice points until a scope boundary handles it (the interpreter's
   RRETURN of MATCH_COMMIT..MATCH_THEN through the frame stack). MARK payloads
   stay in [re.code] and are stored here as byte OFFSETS of the name (like the
   engine's mark_of_offset, decoded only at the seam). [t_commit]/[t_prune]/
   [t_then] carry [mark_off] = -1 for the plain verb or the name offset for the
   _ARG form (which additionally sets mb.mark/nomatch_mark, pcre2_match.c:6373/
   6386/6437); [t_skip] has no arg (SKIP passes back the current position);
   [t_skip_arg] carries the name offset (the rerun protocol,
   pcre2_match.c:6407-6424). *)
let t_mark = 49 (* [t_mark; name_off]  — OP_MARK (set mark, catch SKIP_ARG) *)
let t_commit = 50 (* [t_commit; mark_off] — OP_COMMIT / OP_COMMIT_ARG *)
let t_prune = 51 (* [t_prune; mark_off]  — OP_PRUNE / OP_PRUNE_ARG *)
let t_skip = 52 (* [t_skip]            — OP_SKIP *)
let t_skip_arg = 53 (* [t_skip_arg; name_off] — OP_SKIP_ARG (rerun protocol) *)
let t_then = 54 (* [t_then; mark_off]  — OP_THEN / OP_THEN_ARG *)
let t_accept = 55 (* [t_accept]          — OP_ACCEPT (end the whole match) *)

(* OP_CLOSE (pcre2_match.c:809-829) — close an open capture before OP_ACCEPT.
   [ovbase] = 2N; [referenced] = 1 for a non-optimized (referenced) capture whose
   in-progress start lives in mb.cap_start (else 0: an optimized capture whose
   start is already in ovector[ovbase]). *)
let t_close = 56 (* [t_close; ovbase; referenced] *)

(* Chunk I (fast-design.md §2, task spec) — OP_XCLASS: an extended class that
   may contain code points above 255, ranges, and/or Unicode properties
   (pcre2_match.c:2175-2224). Matched by the shared, self-contained
   Pcre2_engine.Xclass.xclass helper against the class DATA offset [data_off]
   (the flag code unit inside re.code = OP_XCLASS position + 1 + LINK_SIZE).
   [t_xclass] is a lone class (lmin=lmax=1, no choice point); [t_xclass_rep]
   carries an OP_CR* quantifier (reuses the REP machinery via rk_xclass). *)
let t_xclass = 57 (* [t_xclass; data_off]                          *)
let t_xclass_rep = 58 (* [t_xclass_rep; reptype; lmin; lmax; data_off] *)

(* Chunk I2 (fast-design.md §2) — Unicode properties and grapheme clusters.

   [t_prop] is a single OP_PROP/OP_NOTPROP character-property test
   (pcre2_match.c:2479-2614): [notprop] = 1 for OP_NOTPROP; [ptype]/[pdata]
   are the two property code units following the opcode (Fecode[1]/Fecode[2]),
   tested via the shared Pcre2_engine.Char_predicates.prop_test. [t_prop_rep]
   is a property TYPE repeat (OP_TYPESTAR..OP_TYPEPOSUPTO whose type opcode is
   OP_PROP/OP_NOTPROP, pcre2_match.c:2708-2714) — it reuses the REP machinery
   as rk_prop. [t_extuni] is a single \X extended grapheme cluster
   (OP_EXTUNI, pcre2_match.c:2617-2635, via Pcre2_engine.Extuni.extuni);
   [t_extuni_rep] its type-repeat form (pcre2_match.c:2976-2996 min /
   3804-3827 minimize / 4401-4466 maximize), rk_extuni. *)
let t_prop = 59 (* [t_prop; notprop; ptype; pdata]                    *)
let t_prop_rep = 60 (* [t_prop_rep; reptype; lmin; lmax; notprop; ptype; pdata] *)
let t_extuni = 61 (* [t_extuni]                                         *)
let t_extuni_rep = 62 (* [t_extuni_rep; reptype; lmin; lmax]                *)

(* Chunk J additions (fast-design.md §2/§3/§4) — conditional groups
   OP_COND/OP_SCOND and their condition opcodes (pcre2_match.c:5603-5778 /
   interpreter.ml:2427-2548). A conditional has one or two branches whose
   selection is DETERMINED by the condition (not a backtrack-driven ALT retry):
   the yes-branch when the condition is TRUE, else the no-branch (or an empty
   match past the group when there is no no-branch). Only backtracking INTO the
   chosen branch happens normally.

   The NON-assertion condition tests are inline (no choice point, no tick): TRUE
   falls through to the yes-branch, FALSE jumps to [no_target] (the no-branch's
   entry, or the group KET for an empty no-branch).
   - [t_cond_cref] (OP_CREF, pcre2_match.c:5668-5671): TRUE iff group N is SET,
     i.e. [ovector.(ovbase) <> UNSET] (ovbase = 2N; CREF groups are marked
     non-optimized by the F-era analysis, so the slot is written only at CLOSE —
     the test matches the interpreter's `Fovector[offset] != PCRE2_UNSET`).
   - [t_cond_dncref] (OP_DNCREF, pcre2_match.c:5673-5685): TRUE iff ANY group in
     the duplicate-name list [slot_base, slot_base+count*entry) is set (the
     runner scans it like dnref_scan, testing each group's ovector slot).
   - [t_cond_false] (OP_FALSE/OP_FAIL for (?(DEFINE)…) and (?(VERSION<x)…), and
     OP_RREF/OP_DNRREF: the recursion tests are ALWAYS false with recursion out
     of subset — Fcurrent_recurse == RECURSE_UNSET, pcre2_match.c:5645-5666 —
     chunk K revisits them): always jump to [no_target]. OP_TRUE emits NO test
     (the yes-branch runs directly, since the condition never fails).

   Assertion conditions ((?(?=…)…) etc.) reuse chunk G's grouploop-style branch
   lowering + the KIND_NASSERT boundary (fast-design.md §3): [t_cond_assert]
   pushes the boundary (its [cont] = [nomatch_target], the branch taken when the
   assertion body does NOT match, i.e. condition = !Lpositive); a matching
   assertion branch reaches [t_cond_assert_match], which COMMITS (converts the
   KIND_NASSERT to a KIND_ONCE — the atomic behaviour + capture persistence of
   pcre2_match.c:5934-5948) and jumps to [match_target] (condition = Lpositive).
   [match_target]/[nomatch_target] are the yes/no branch heads chosen at
   IR-compile time by the assertion's positivity.

   [t_scond_descend] is the OP_SCOND descend (RM35, pcre2_match.c:5772-5776): a
   repeated conditional that might match empty descends one virtual frame per
   iteration so the empty-string loop check (mb.group_start.(g)) has the
   iteration start — one tick, no choice point (like a single-branch grouploop's
   entry tick). *)
let t_cond_cref = 63 (* [t_cond_cref; ovbase; no_target] *)
let t_cond_dncref = 64 (* [t_cond_dncref; slot_base; count; no_target] *)
let t_cond_false = 65 (* [t_cond_false; no_target] *)
let t_cond_assert = 66 (* [t_cond_assert; nomatch_target] *)
let t_cond_assert_match = 67 (* [t_cond_assert_match; match_target] *)
let t_scond_descend = 68 (* [t_scond_descend] *)

(* Chunk K additions (fast-design.md §2/§3) — script runs and recursion.

   [t_script_run_end] is the OP_SCRIPT_RUN ket (pcre2_match.c:6045-6051 /
   interpreter.ml:2882-2897). A script run (the sr / script_run verb) is a
   non-capturing, NON-atomic group (GF_NOCAPTURE, grouploop) whose ket applies
   the script-checking rules to the group's matched span [group_start.(g), eptr)
   via Pcre2_engine.Script_run.script_run; on failure it backtracks, else it
   continues at the current (advanced) eptr. The entry records the span start in
   mb.group_start.(g) via a preceding [t_group_start]. The atomic_script_run
   verb compiles as OP_SCRIPT_RUN wrapping OP_ONCE, so the atomic wrapper is the
   chunk-G machinery and the script run needs no extra atomicity here. *)
let t_script_run_end = 69 (* [t_script_run_end; g] — OP_SCRIPT_RUN ket *)

(* Chunk K1b additions (fast-design.md §2/§3) — pattern recursion OP_RECURSE
   (pcre2_match.c:5427-5497) and the recursion-condition tests OP_RREF/OP_DNRREF
   (pcre2_match.c:5645-5666, un-FALSEd for recursion-containing patterns).

   [t_recurse] is a subroutine call: [number] = the recursed group's number
   (0 = whole-pattern (?R)/(?0)), [entry_pc] = the IR index of that group's
   first-branch entry (its t_bra, resolved at IR-compile time even for forward
   references). The recursion pushes a FAT KIND_RECURSE save record (the full
   ovector + group_start + cap_start + mark + current_recurse + once_base
   snapshot — the wholesale-frame answer, fast-design.md §3), sets
   mb.current_recurse = number, and jumps to [entry_pc]; the recursed group's
   ket (t_cap_end / t_cap_end_ref for CBRA/SCBRA, or the whole-pattern t_ket for
   group 0) detects mb.current_recurse == number and RETURNS (restore the
   snapshot's captures — captures do NOT escape a recursion — and continue past
   the call at [pc + 3]). The continuation runs at the recursion body's rdepth
   (the C's `continue` in the same frame, pcre2_match.c:6073). *)
let t_recurse = 70 (* [t_recurse; number; entry_pc] *)

(* [t_cond_rref] (OP_RREF, pcre2_match.c:5645-5651): a group-recursion test for a
   conditional. TRUE iff we are inside a recursion (mb.current_recurse >= 0) AND
   ([number] = RREF_ANY, i.e. (?(R)), or number == mb.current_recurse, (?(Rn))).
   FALSE jumps to [no_target]. Un-FALSEs the chunk-J COND_FALSE for recursion. *)
let t_cond_rref = 71 (* [t_cond_rref; number; no_target] *)

(* [t_cond_dnrref] (OP_DNRREF, pcre2_match.c:5653-5666): a duplicate-named
   recursion test ((?(R&name))). TRUE iff inside a recursion AND any group in the
   name-table list [slot_base, slot_base+count*entry) equals mb.current_recurse.
   FALSE jumps to [no_target]. *)
let t_cond_dnrref = 72 (* [t_cond_dnrref; slot_base; count; no_target] *)

(* [t_set_som] (chunk K2) — \K (OP_SET_SOM, pcre2_match.c:6252-6255): reset the
   start of the reported match to the current position (Fstart_match = Feptr).
   The runner pushes a KIND_SET_SOM record saving the old start_match (restored
   on backtrack-past, mirroring the C's per-frame Fstart_match) and sets
   mb.start_match = eptr. No operands. *)
let t_set_som = 74

(* [t_assert_accept; conv_kind; target] (chunk K2) — ( *ACCEPT) inside a
   lookaround assertion (OP_ASSERT_ACCEPT, pcre2_match.c:832-840): the C RRETURNs
   MATCH_ACCEPT, which propagates to the innermost enclosing assertion boundary
   (RM3 positive / RM4 negative / RM5 condition) and commits it there. The runner
   walks the save stack to that boundary ([backtrack_accept]) and applies the
   assertion's success/fail action; [conv_kind] (0 = positive, 1 = negative,
   2 = condition) and [target] (the positive continuation pc, or the condition's
   match_target; unused for negative) name that action, chosen at IR-compile time
   from the lexically-enclosing lookaround. *)
let t_assert_accept = 75

let accept_positive = 0
let accept_negative = 1
let accept_cond = 2

(* [t_fail_nassert] (chunk K1b) — branch exhaustion of a NEGATIVE assertion
   (OP_ASSERT_NOT/OP_ASSERTBACK_NOT) or an assertion CONDITION: identical to
   [t_fail] (propagate a NOMATCH backtrack) EXCEPT it does NOT record
   last_used_ptr. The C's exhaustion there is a SAME-FRAME transfer with no
   RRETURN — ASSERT_NOT_FAILED (pcre2_match.c:5561-5564 -> :5582) and the
   condition-assertion `condition = !Lpositive; break` (:5729-5734) — so the
   assertion frame's entry Feptr is never recorded; every OTHER FAIL head
   (grouploop group / positive-assertion / atomic-group / script-run exhaustion,
   and the ( *FAIL) verb) corresponds to a C RRETURN(MATCH_NOMATCH) from the
   frame at the FAIL's restored eptr (:5410/:5531/:6360) and keeps recording. *)
let t_fail_nassert = 73 (* [t_fail_nassert] *)

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

(* RREF_ANY for [t_cond_rref]'s [number] ((?(R)) — "in ANY recursion"); the same
   0xffff the shared bytecode stores (pcre2_internal.h:1816 / Opcodes.rref_any),
   which cannot collide with a real group number. *)
let rref_any = 0xffff

(* fast-design.md §2 — highest valid tag; used by the verifier and dump. *)
let max_tag = 75

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
    2 (* t_type: type_op *);
    2 (* t_class: map_off *);
    2 (* t_wordbound: want *);
    5 (* t_type_rep: reptype, lmin, lmax, type_op *);
    5 (* t_class_rep: reptype, lmin, lmax, map_off *);
    3 (* t_ref: ovbase, caseless *);
    6 (* t_ref_rep: reptype, lmin, lmax, ovbase, caseless *);
    4 (* t_dnref: slot_base, count, caseless *);
    7 (* t_dnref_rep: reptype, lmin, lmax, slot_base, count, caseless *);
    2 (* t_cap_start_ref: ovbase *);
    2 (* t_cap_end_ref: ovbase *);
    2 (* t_reverse: number *);
    3 (* t_vreverse: lmin, lmax *);
    2 (* t_once: cont *);
    1 (* t_once_end *);
    3 (* t_assert_end: atomic, g *);
    3 (* t_nassert: g, cont *);
    1 (* t_nassert_match *);
    2 (* t_assertback_check: g *);
    3 (* t_possess: cap_ovbase, zero_allowed *);
    4 (* t_ketrpos: body_entry, cap_ovbase, number *);
    2 (* t_possess_done: number *);
    2 (* t_mark: name_off *);
    2 (* t_commit: mark_off *);
    2 (* t_prune: mark_off *);
    1 (* t_skip *);
    2 (* t_skip_arg: name_off *);
    2 (* t_then: mark_off *);
    1 (* t_accept *);
    3 (* t_close: ovbase, referenced *);
    2 (* t_xclass: data_off *);
    5 (* t_xclass_rep: reptype, lmin, lmax, data_off *);
    4 (* t_prop: notprop, ptype, pdata *);
    7 (* t_prop_rep: reptype, lmin, lmax, notprop, ptype, pdata *);
    1 (* t_extuni *);
    4 (* t_extuni_rep: reptype, lmin, lmax *);
    3 (* t_cond_cref: ovbase, no_target *);
    4 (* t_cond_dncref: slot_base, count, no_target *);
    2 (* t_cond_false: no_target *);
    2 (* t_cond_assert: nomatch_target *);
    2 (* t_cond_assert_match: match_target *);
    1 (* t_scond_descend *);
    2 (* t_script_run_end: g *);
    3 (* t_recurse: number, entry_pc *);
    3 (* t_cond_rref: number, no_target *);
    4 (* t_cond_dnrref: slot_base, count, no_target *);
    1 (* t_fail_nassert *);
    1 (* t_set_som *);
    3 (* t_assert_accept: conv_kind, target *);
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
    "TYPE";
    "CLASS";
    "WORDBOUND";
    "TYPE_REP";
    "CLASS_REP";
    "REF";
    "REF_REP";
    "DNREF";
    "DNREF_REP";
    "CAP_START_REF";
    "CAP_END_REF";
    "REVERSE";
    "VREVERSE";
    "ONCE";
    "ONCE_END";
    "ASSERT_END";
    "NASSERT";
    "NASSERT_MATCH";
    "ASSERTBACK_CHECK";
    "POSSESS";
    "KETRPOS";
    "POSSESS_DONE";
    "MARK";
    "COMMIT";
    "PRUNE";
    "SKIP";
    "SKIP_ARG";
    "THEN";
    "ACCEPT";
    "CLOSE";
    "XCLASS";
    "XCLASS_REP";
    "PROP";
    "PROP_REP";
    "EXTUNI";
    "EXTUNI_REP";
    "COND_CREF";
    "COND_DNCREF";
    "COND_FALSE";
    "COND_ASSERT";
    "COND_ASSERT_MATCH";
    "SCOND_DESCEND";
    "SCRIPT_RUN_END";
    "RECURSE";
    "COND_RREF";
    "COND_DNRREF";
    "FAIL_NASSERT";
    "SET_SOM";
    "ASSERT_ACCEPT";
  |]

(* fast-design.md §2 — the compiled fast program. [code] is the flat
   instruction stream; [lit] backs [t_char_run] items; [re] is pinned as the
   source of any (future) variable-length payloads and of the options /
   top_bracket the runner (chunk C2) needs. [n_groups] (chunk D2) is the
   number of empty-check-tracked repeated groups: the runner allocates a
   [group_start] array of that size, indexed by the group id in
   [t_group_start]/[t_ket_rmax]/[t_ket_rmin]. *)
type t = {
  code : int array;
  lit : string;
  re : C.re;
  n_groups : int;
  (* Chunk H (fast-design.md §3) — THEN scope boundary per ALT choice point,
     indexed by the ALT's [pc]. [alt_then_end.(alt_pc)] = the handler (next-
     alternative entry) when the ALT belongs to a genuine >= 2-branch
     alternation, else -1. A ( *THEN) firing converts to NOMATCH at a KIND_ALT
     record (resuming that alternative) iff [then_pc < then_end] — reproducing
     the C's `verb_ecode_ptr < next_ecode && ( *ecode == OP_ALT || *next_ecode ==
     OP_ALT)` grouploop check (pcre2_match.c:5401-5407): the multi-branch bit is
     baked into the -1 sentinel (single-branch groups never convert THEN, so it
     escapes). Kept OUT of [code] so the IR dump / pc numbering are unchanged;
     the runner copies [alt_then_end.(pc)] into the KIND_ALT save record at push
     time. Non-ALT indices are unused (-1). *)
  alt_then_end : int array;
  (* Chunk H (fast-design.md §3) — KIND_ONCE boundary subtype per t_once [pc]:
     0 = atomic group (OP_ONCE), 1 = atomic positive assertion (OP_ASSERT/
     ASSERTBACK). Both push a KIND_ONCE, but a ( *THEN) backtracking to the
     boundary is CONTAINED (converted to NOMATCH) for a positive assertion
     (pcre2_match.c:5504-5507 / RM3 treats THEN as NOMATCH) yet ESCAPES an atomic
     group (RM2's position scope check fails for a THEN after the group). Kept out
     of [code] (dump unchanged); the runner copies it into the KIND_ONCE record
     at push time. Non-t_once indices are unused (0). *)
  once_subtype : int array;
  (* Chunk K1b — the pattern contains OP_RECURSE. When true the runner tracks
     mb.last_used_ptr (the RECURSELOOP-check input, fast-design.md §3/§4); when
     false last_used_ptr is never consulted (no COND_RREF/RECURSE reads it), so
     the tracking is skipped for zero hot-loop cost on non-recursive patterns. *)
  has_recurse : bool;
}

(* Chunk K2 — the [alt_then_end] value for a NON-ATOMIC positive assertion's
   branch ALT: THEN is ALWAYS converted to the next branch there (the C's RM3
   tries the next branch for any MATCH_THEN reaching the assertion, not the
   grouploop pc scope check). Since [verb_then_pc] is always a valid IR pc,
   [max_int] makes `verb_then_pc < then_end` unconditionally true. The verifier
   accepts this sentinel alongside -1 / a valid head. *)
let then_always = max_int

(* Chunk H — KIND_ONCE subtype values. Chunk K2 adds [once_na_assert] for a
   NON-ATOMIC positive assertion (OP_ASSERT_NA/ASSERTBACK_NA): it pushes a
   KIND_ONCE boundary (so a ( *THEN)/( *ACCEPT) reaching it is contained / handled
   like any positive assertion) but the boundary does NOT set mb.once_base (the
   assertion is non-atomic — its body is re-enterable, so once_base cannot track a
   single boundary across re-entry; the atomic COMMIT never targets it, and the
   THEN/ACCEPT walks find it by save-stack order). *)
let once_group = 0
let once_pos_assert = 1
let once_na_assert = 2

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

(* Chunk E operands. [t_type] / [t_wordbound] carry one operand at pc+1;
   [t_class] carries the bitmap byte offset at pc+1; the type / class repeats
   carry the type opcode / bitmap offset at pc+4 (after reptype/lmin/lmax,
   read by rep_reptype/rep_lmin/rep_lmax above). *)
let type_op (ir : t) (pc : int) : int = ir.code.(pc + 1)
let class_map_off (ir : t) (pc : int) : int = ir.code.(pc + 1)
let wordbound_want (ir : t) (pc : int) : int = ir.code.(pc + 1)
let rep_type_op (ir : t) (pc : int) : int = ir.code.(pc + 4)
let rep_map_off (ir : t) (pc : int) : int = ir.code.(pc + 4)

(* Chunk I (fast-design.md §2) — XCLASS data offset: for a lone [t_xclass] it
   is operand 1; for [t_xclass_rep] it follows the reptype/lmin/lmax at
   operand 4. *)
let xclass_data (ir : t) (pc : int) : int = ir.code.(pc + 1)
let rep_xclass_data (ir : t) (pc : int) : int = ir.code.(pc + 4)

(* Chunk I2 (fast-design.md §2) — property operands: a lone [t_prop] carries
   [notprop; ptype; pdata] at pc+1..pc+3; [t_prop_rep] carries them at
   pc+4..pc+6 after the reptype/lmin/lmax triple. *)
let prop_not (ir : t) (pc : int) : int = ir.code.(pc + 1)
let prop_ptype (ir : t) (pc : int) : int = ir.code.(pc + 2)
let prop_pdata (ir : t) (pc : int) : int = ir.code.(pc + 3)
let rep_prop_not (ir : t) (pc : int) : int = ir.code.(pc + 4)
let rep_prop_ptype (ir : t) (pc : int) : int = ir.code.(pc + 5)
let rep_prop_pdata (ir : t) (pc : int) : int = ir.code.(pc + 6)

(* Chunk F operands (fast-design.md §2/§3). [t_ref] carries [ovbase; caseless]
   at pc+1/pc+2; [t_ref_rep] the reptype/lmin/lmax triple (rep_reptype/rep_lmin/
   rep_lmax above) then [ovbase; caseless] at pc+4/pc+5; [t_dnref] the
   name-table [slot_base; count; caseless] at pc+1..pc+3; [t_dnref_rep] the
   reptype triple then [slot_base; count; caseless] at pc+4..pc+6. *)
let ref_ovbase (ir : t) (pc : int) : int = ir.code.(pc + 1)
let ref_caseless (ir : t) (pc : int) : int = ir.code.(pc + 2)
let ref_rep_ovbase (ir : t) (pc : int) : int = ir.code.(pc + 4)
let ref_rep_caseless (ir : t) (pc : int) : int = ir.code.(pc + 5)
let dnref_slot_base (ir : t) (pc : int) : int = ir.code.(pc + 1)
let dnref_count (ir : t) (pc : int) : int = ir.code.(pc + 2)
let dnref_caseless (ir : t) (pc : int) : int = ir.code.(pc + 3)
let dnref_rep_slot_base (ir : t) (pc : int) : int = ir.code.(pc + 4)
let dnref_rep_count (ir : t) (pc : int) : int = ir.code.(pc + 5)
let dnref_rep_caseless (ir : t) (pc : int) : int = ir.code.(pc + 6)

(* Chunk G operands (fast-design.md §2/§3). *)

(* [t_once] operand (chunk K2) — the assertion's continuation pc (or -1 for an
   atomic group). *)
let once_cont (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_reverse] operand — the fixed lookbehind back-step (code units). *)
let reverse_number (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_vreverse] operands — the min/max variable lookbehind back-step. *)
let vreverse_lmin (ir : t) (pc : int) : int = ir.code.(pc + 1)
let vreverse_lmax (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* [t_assert_end] operands — [atomic] (1 = OP_ASSERT/ASSERTBACK, 0 = NA) and
   the assertion's group id [g] (its entry eptr lives in mb.group_start.(g)). *)
let assert_atomic (ir : t) (pc : int) : int = ir.code.(pc + 1)
let assert_group (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* [t_nassert] operands — the group id [g] (used only by a variable-lookbehind
   [t_assertback_check]; [no_group] otherwise) and the success continuation
   [cont] (past the whole negative assertion). *)
let nassert_group (ir : t) (pc : int) : int = ir.code.(pc + 1)
let nassert_cont (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* [t_assertback_check] operand — the assertion group id whose entry eptr the
   variable lookbehind branch must have reached. *)
let assertback_check_group (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_possess] operands — [cap_ovbase] (2N for a capturing possessive bracket,
   0 = non-capturing) and [zero_allowed] (1 for a BRAPOSZERO-prefixed *+ / {0,n}+
   group). *)
let possess_ovbase (ir : t) (pc : int) : int = ir.code.(pc + 1)
let possess_zero_allowed (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* [t_ketrpos] operands — [body_entry] (IR index the iteration loops back to),
   [cap_ovbase] (the capture pair to write each iteration, 0 = none) and
   [number] (chunk K2 — the recursion number, -1 if not a capture). *)
let ketrpos_entry (ir : t) (pc : int) : int = ir.code.(pc + 1)
let ketrpos_ovbase (ir : t) (pc : int) : int = ir.code.(pc + 2)
let ketrpos_number (ir : t) (pc : int) : int = ir.code.(pc + 3)

(* [t_possess_done] operand — [number] (chunk K2, as t_ketrpos). *)
let possess_done_number (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_assert_accept] operands (chunk K2) — [conv_kind] (accept_positive /
   accept_negative / accept_cond) and [target] (the positive continuation pc or
   the condition match_target). *)
let assert_accept_kind (ir : t) (pc : int) : int = ir.code.(pc + 1)
let assert_accept_target (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* Chunk H operands (fast-design.md §2/§3). *)

(* [t_mark] / [t_skip_arg] operand — the byte OFFSET of the verb name in
   [re.code] (mark_of_offset decodes it at the seam; the length byte is at
   name_off - 1). *)
let verb_name_off (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_commit] / [t_prune] / [t_then] operand — the mark offset (-1 for the plain
   verb, else the _ARG form's name offset which is also set as mb.mark). *)
let verb_mark_off (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_close] operands — the capture pair base 2N and whether it is a referenced
   (non-optimized) capture (start in mb.cap_start) vs optimized (start already in
   ovector[ovbase]). *)
let close_ovbase (ir : t) (pc : int) : int = ir.code.(pc + 1)
let close_referenced (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* [t_alt] THEN scope boundary (fast-design.md §3) — see the [alt_then_end]
   field. Read by the runner at ALT push time. *)
let alt_then_end (ir : t) (alt_pc : int) : int = ir.alt_then_end.(alt_pc)

(* Chunk J operands (fast-design.md §2/§3). *)

(* [t_cond_cref] — [ovbase] (= 2N of the tested group) and [no_target] (the IR
   index to jump to when the condition is FALSE). *)
let cond_cref_ovbase (ir : t) (pc : int) : int = ir.code.(pc + 1)
let cond_cref_no (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* [t_cond_dncref] — the name-table [slot_base]/[count] of the duplicate-name
   group list, and [no_target]. *)
let cond_dncref_slot_base (ir : t) (pc : int) : int = ir.code.(pc + 1)
let cond_dncref_count (ir : t) (pc : int) : int = ir.code.(pc + 2)
let cond_dncref_no (ir : t) (pc : int) : int = ir.code.(pc + 3)

(* [t_cond_false] — [no_target] (always taken). *)
let cond_false_no (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_cond_assert] — [nomatch_target] (the branch taken when the assertion body
   does NOT match; stored as the KIND_NASSERT record's [cont]). *)
let cond_assert_nomatch (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_cond_assert_match] — [match_target] (the branch taken when the assertion
   body matches). *)
let cond_assert_match_target (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* [t_script_run_end] — the script run's group id [g] (its span start lives in
   mb.group_start.(g), written by the preceding t_group_start). *)
let script_run_group (ir : t) (pc : int) : int = ir.code.(pc + 1)

(* Chunk K1b operands (fast-design.md §2/§3). *)

(* [t_recurse] — the recursed group's [number] and the IR [entry_pc] of its
   first-branch body. *)
let recurse_number (ir : t) (pc : int) : int = ir.code.(pc + 1)
let recurse_entry (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* [t_cond_rref] — the tested recursion [number] (or [rref_any]) and the FALSE
   jump [no_target]. *)
let cond_rref_number (ir : t) (pc : int) : int = ir.code.(pc + 1)
let cond_rref_no (ir : t) (pc : int) : int = ir.code.(pc + 2)

(* [t_cond_dnrref] — the name-table [slot_base]/[count] and the FALSE jump. *)
let cond_dnrref_slot_base (ir : t) (pc : int) : int = ir.code.(pc + 1)
let cond_dnrref_count (ir : t) (pc : int) : int = ir.code.(pc + 2)
let cond_dnrref_no (ir : t) (pc : int) : int = ir.code.(pc + 3)

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
  else if Int.equal t t_cap_start_ref then
    Printf.sprintf "CAP_START_REF ovbase=%d" (cap_ovbase ir pc)
  else if Int.equal t t_cap_end_ref then
    Printf.sprintf "CAP_END_REF ovbase=%d" (cap_ovbase ir pc)
  else if Int.equal t t_ref then
    Printf.sprintf "REF ovbase=%d ci=%d" (ref_ovbase ir pc) (ref_caseless ir pc)
  else if Int.equal t t_dnref then
    Printf.sprintf "DNREF slot=%d count=%d ci=%d" (dnref_slot_base ir pc)
      (dnref_count ir pc) (dnref_caseless ir pc)
  else if Int.equal t t_ref_rep then (
    let ty = rep_reptype ir pc in
    let tystr =
      if Int.equal ty reptype_min then "min"
      else if Int.equal ty reptype_max then "max"
      else "pos"
    in
    let lmax = rep_lmax ir pc in
    let lmaxstr = if Int.equal lmax rep_inf then "inf" else string_of_int lmax in
    Printf.sprintf "REF_REP %s {%d,%s} ovbase=%d ci=%d" tystr (rep_lmin ir pc)
      lmaxstr (ref_rep_ovbase ir pc) (ref_rep_caseless ir pc))
  else if Int.equal t t_dnref_rep then (
    let ty = rep_reptype ir pc in
    let tystr =
      if Int.equal ty reptype_min then "min"
      else if Int.equal ty reptype_max then "max"
      else "pos"
    in
    let lmax = rep_lmax ir pc in
    let lmaxstr = if Int.equal lmax rep_inf then "inf" else string_of_int lmax in
    Printf.sprintf "DNREF_REP %s {%d,%s} slot=%d count=%d ci=%d" tystr
      (rep_lmin ir pc) lmaxstr (dnref_rep_slot_base ir pc)
      (dnref_rep_count ir pc) (dnref_rep_caseless ir pc))
  else if Int.equal t t_reverse then
    Printf.sprintf "REVERSE %d" (reverse_number ir pc)
  else if Int.equal t t_vreverse then
    Printf.sprintf "VREVERSE {%d,%d}" (vreverse_lmin ir pc) (vreverse_lmax ir pc)
  else if Int.equal t t_assert_end then
    Printf.sprintf "ASSERT_END %s g=%d"
      (if Int.equal (assert_atomic ir pc) 1 then "atomic" else "na")
      (assert_group ir pc)
  else if Int.equal t t_nassert then
    Printf.sprintf "NASSERT g=%d cont=%d" (nassert_group ir pc)
      (nassert_cont ir pc)
  else if Int.equal t t_assertback_check then
    Printf.sprintf "ASSERTBACK_CHECK g=%d" (assertback_check_group ir pc)
  else if Int.equal t t_possess then
    Printf.sprintf "POSSESS ovbase=%d zero=%d" (possess_ovbase ir pc)
      (possess_zero_allowed ir pc)
  else if Int.equal t t_ketrpos then
    Printf.sprintf "KETRPOS entry=%d ovbase=%d n=%d" (ketrpos_entry ir pc)
      (ketrpos_ovbase ir pc) (ketrpos_number ir pc)
  else if Int.equal t t_possess_done then
    Printf.sprintf "POSSESS_DONE n=%d" (possess_done_number ir pc)
  else if Int.equal t t_assert_accept then
    Printf.sprintf "ASSERT_ACCEPT kind=%d target=%d" (assert_accept_kind ir pc)
      (assert_accept_target ir pc)
  else if Int.equal t t_mark then
    Printf.sprintf "MARK name_off=%d" (verb_name_off ir pc)
  else if Int.equal t t_skip_arg then
    Printf.sprintf "SKIP_ARG name_off=%d" (verb_name_off ir pc)
  else if Int.equal t t_commit then
    Printf.sprintf "COMMIT mark_off=%d" (verb_mark_off ir pc)
  else if Int.equal t t_prune then
    Printf.sprintf "PRUNE mark_off=%d" (verb_mark_off ir pc)
  else if Int.equal t t_then then
    Printf.sprintf "THEN mark_off=%d" (verb_mark_off ir pc)
  else if Int.equal t t_close then
    Printf.sprintf "CLOSE ovbase=%d ref=%d" (close_ovbase ir pc)
      (close_referenced ir pc)
  else if Int.equal t t_cond_cref then
    Printf.sprintf "COND_CREF ovbase=%d no=%d" (cond_cref_ovbase ir pc)
      (cond_cref_no ir pc)
  else if Int.equal t t_cond_dncref then
    Printf.sprintf "COND_DNCREF slot=%d count=%d no=%d"
      (cond_dncref_slot_base ir pc) (cond_dncref_count ir pc)
      (cond_dncref_no ir pc)
  else if Int.equal t t_cond_false then
    Printf.sprintf "COND_FALSE no=%d" (cond_false_no ir pc)
  else if Int.equal t t_cond_assert then
    Printf.sprintf "COND_ASSERT nomatch=%d" (cond_assert_nomatch ir pc)
  else if Int.equal t t_cond_assert_match then
    Printf.sprintf "COND_ASSERT_MATCH match=%d" (cond_assert_match_target ir pc)
  else if Int.equal t t_script_run_end then
    Printf.sprintf "SCRIPT_RUN_END g=%d" (script_run_group ir pc)
  else if Int.equal t t_recurse then
    Printf.sprintf "RECURSE number=%d entry=%d" (recurse_number ir pc)
      (recurse_entry ir pc)
  else if Int.equal t t_cond_rref then
    Printf.sprintf "COND_RREF number=%d no=%d" (cond_rref_number ir pc)
      (cond_rref_no ir pc)
  else if Int.equal t t_cond_dnrref then
    Printf.sprintf "COND_DNRREF slot=%d count=%d no=%d"
      (cond_dnrref_slot_base ir pc) (cond_dnrref_count ir pc)
      (cond_dnrref_no ir pc)
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
  else if Int.equal t t_type then
    Printf.sprintf "TYPE %s" Op.op_names.(type_op ir pc)
  else if Int.equal t t_class then
    Printf.sprintf "CLASS map=%d" (class_map_off ir pc)
  else if Int.equal t t_xclass then
    Printf.sprintf "XCLASS data=%d" (xclass_data ir pc)
  else if Int.equal t t_wordbound then
    (* Chunk I2 — [want] bit 0 = boundary wanted (\b vs \B), bit 1 = the UCP
       variant (OP_UCP_WORD_BOUNDARY / OP_NOT_UCP_WORD_BOUNDARY). Non-UCP
       dumps are unchanged. *)
    let w = wordbound_want ir pc in
    Printf.sprintf "WORDBOUND %s%s"
      (if Int.equal (w land 1) 1 then "\\b" else "\\B")
      (if Int.equal (w land 2) 2 then " ucp" else "")
  else if Int.equal t t_prop then
    Printf.sprintf "%s ptype=%d pdata=%d"
      (if Int.equal (prop_not ir pc) 1 then "NOTPROP" else "PROP")
      (prop_ptype ir pc) (prop_pdata ir pc)
  else if Int.equal t t_prop_rep then (
    let ty = rep_reptype ir pc in
    let tystr =
      if Int.equal ty reptype_min then "min"
      else if Int.equal ty reptype_max then "max"
      else "pos"
    in
    let lmax = rep_lmax ir pc in
    let lmaxstr = if Int.equal lmax rep_inf then "inf" else string_of_int lmax in
    Printf.sprintf "PROP_REP %s {%d,%s} %s ptype=%d pdata=%d" tystr
      (rep_lmin ir pc) lmaxstr
      (if Int.equal (rep_prop_not ir pc) 1 then "not" else "is")
      (rep_prop_ptype ir pc) (rep_prop_pdata ir pc))
  else if Int.equal t t_extuni_rep then (
    let ty = rep_reptype ir pc in
    let tystr =
      if Int.equal ty reptype_min then "min"
      else if Int.equal ty reptype_max then "max"
      else "pos"
    in
    let lmax = rep_lmax ir pc in
    let lmaxstr = if Int.equal lmax rep_inf then "inf" else string_of_int lmax in
    Printf.sprintf "EXTUNI_REP %s {%d,%s}" tystr (rep_lmin ir pc) lmaxstr)
  else if Int.equal t t_type_rep then (
    let ty = rep_reptype ir pc in
    let tystr =
      if Int.equal ty reptype_min then "min"
      else if Int.equal ty reptype_max then "max"
      else "pos"
    in
    let lmax = rep_lmax ir pc in
    let lmaxstr = if Int.equal lmax rep_inf then "inf" else string_of_int lmax in
    Printf.sprintf "TYPE_REP %s {%d,%s} %s" tystr (rep_lmin ir pc) lmaxstr
      Op.op_names.(rep_type_op ir pc))
  else if Int.equal t t_class_rep then (
    let ty = rep_reptype ir pc in
    let tystr =
      if Int.equal ty reptype_min then "min"
      else if Int.equal ty reptype_max then "max"
      else "pos"
    in
    let lmax = rep_lmax ir pc in
    let lmaxstr = if Int.equal lmax rep_inf then "inf" else string_of_int lmax in
    Printf.sprintf "CLASS_REP %s {%d,%s} map=%d" tystr (rep_lmin ir pc) lmaxstr
      (rep_map_off ir pc))
  else if Int.equal t t_xclass_rep then (
    let ty = rep_reptype ir pc in
    let tystr =
      if Int.equal ty reptype_min then "min"
      else if Int.equal ty reptype_max then "max"
      else "pos"
    in
    let lmax = rep_lmax ir pc in
    let lmaxstr = if Int.equal lmax rep_inf then "inf" else string_of_int lmax in
    Printf.sprintf "XCLASS_REP %s {%d,%s} data=%d" tystr (rep_lmin ir pc) lmaxstr
      (rep_xclass_data ir pc))
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
