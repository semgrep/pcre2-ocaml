# Fast-engine design (M11) — v0

The citable spec for engine-native code in `src/fast/` (port-conventions.md §9): blocks with
no `pcre2_jit_compile.c` counterpart cite a section of THIS document, and the fidelity
reviewer reviews them against it. Milestone/chunks: `11-fast-engine.md`. Approved plan:
`~/.claude/plans/shimmying-wibbling-brooks.md`.

## §1 Seam and the no-fallback contract

- Public seam: `src/fast/pcre2_fast.mli` — mirrors `engine.mli`, plus
  `compile_error = Compile_error of {errcode; erroroffset} | Unsupported of string`.
- **No fallback.** `compile` runs the SHARED compiler (`Pcre2_engine.Engine.compile_ctx`) —
  real PCRE2 compile errors carry exact number/offset parity — then the IR compiler; any
  construct the IR compiler does not handle yields `Unsupported`. A fast `t` is never
  executed by the interpreter, and no code path routes a match through `Engine.exec`.
- On accepted patterns, observable behavior is IDENTICAL to `Engine.exec/exec_full/
  exec_captures`: rc/ovector/mark/startchar, and limit trip points (§4). The interpreter is
  the differential ORACLE (conformance `--driver=fast`, fuzz fast-vs-interp).
- The Matcher surface is assembled from `pcre2.matcher` (`MakeMatcher` + `MakeConvenience`)
  exactly like `Pcre2.Interp`/`Jit`; iterator empty-match semantics are FROZEN.
- Harness encoding: at the pcre2test driver seam (`test/pcre2test/fast_driver.ml`),
  `Unsupported` is the sentinel errcode 998 + a reason cell read by `unsupported_of_error`;
  the harness records `unsupported:<reason>` as a skip, never a `Failed:` line.

## §2 IR encoding (landed in chunk C1 — `src/fast/ir.ml`)

Principles (fixed by the plan) and the CONCRETE layout as implemented:

- One flat `code : int array`; `pc` indexes instruction heads: `[tag; operand...]`.
- Tags are a Fast-private dense enum (`Ir` module), NOT PCRE2 opcode numbers — fused and
  specialized instructions get their own tags; dispatch is a literal-int `match` (jump
  table), like interpreter.ml:1337. `Ir.arity.(tag)` gives each instruction's WIDTH in ints
  (tag + operands); `Ir.tag_name.(tag)` its dump name; `Ir.max_tag` the highest valid tag.
- Operands are PRE-DECODED: no `get2` link reads at run time; LINK offsets resolved to
  absolute IR indices at IR-compile time.
- `CHAR_RUN`: consecutive caseful `OP_CHAR`s fuse into one instruction comparing against a
  literal pool (`lit : string`) word-at-a-time (`String.get_int64_ne` + masked tail —
  chunk M) — mirrors `byte_sequence_compare` (pcre2_jit_compile.c:7479). `OP_CHARI` runs do
  NOT fuse: one `CHARI` per code unit. That is a DELIBERATE conservative simplification,
  not JIT parity — the JIT fuses caseless runs too when a code unit's othercase differs by
  a single bit (or it has none): `compile_charn_matchingpath`
  (pcre2_jit_compile.c:9334-9396, reached for both OP_CHAR and OP_CHARI at :12542-12547)
  concatenates them into one `byte_sequence_compare` (:9392) with caseless=true. Declining
  it is behavior-identical (CHARI creates no choice point, so no limit-tick divergence);
  caseless fusion is a possible chunk-M optimization citing those lines.
- Variable-length payloads (XCLASS data, MARK names) stay in the original bytecode;
  the IR stores offsets into `re.code` (JIT does the same); `Ir.t` therefore pins
  `Compile.re`. `Ir.t = { code : int array; lit : string; re : Compile.re;
  n_groups : int; alt_then_end : int array; once_subtype : int array }`
  (chunk D2 added `n_groups`; chunk H added `alt_then_end` and `once_subtype`, two
  compile-time side arrays indexed by IR `pc`, kept OUT of `code` so the dump / pc
  numbering are unchanged — see §3).

**Instruction table (chunk C1 subset).** `pc` = instruction head; operands are the ints
following the tag. Widths are `Ir.arity`:

| tag (int) | name | operands | width | meaning |
|---|---|---|---|---|
| 0 | `END` | — | 1 | end of program (accept) |
| 1 | `CHAR_RUN` | `lit_off; len` | 3 | match `len` caseful code units `lit[lit_off .. lit_off+len)` |
| 2 | `CHARI` | `ch` | 2 | match one caseless code unit `ch` (runner folds case via `Chartables`, non-UTF CHARI arm pcre2_match.c:1097-1103) |
| 3 | `BRA` | — | 1 | group entry marker (structural no-op) |
| 4 | `KET` | — | 1 | group exit marker (structural no-op) |
| 5 | `ALT` | `next` | 2 | choice point: `next` = absolute IR index of the next alternative's entry (the save-record HANDLER, §3) |
| 6 | `JMP` | `target` | 2 | end-of-branch jump: `target` = absolute IR index of the enclosing group's `KET` |
| 7 | `SOD` | — | 1 | `\A` (start of subject) |
| 8 | `SOM` | — | 1 | `\G` (start of match) |
| 9 | `EOD` | — | 1 | `\z` (end of subject) |
| 10 | `EODN` | — | 1 | `\Z` (end, or newline at end) |
| 11 | `CIRC` | — | 1 | `^` (non-multiline) |
| 12 | `DOLL` | — | 1 | `$` (non-multiline) |
| 13 | `CIRCM` | — | 1 | `^` multiline (OP_CIRCM); runner mirrors interpreter.ml:3108-3124 via `Newline.was_newline` |
| 14 | `DOLLM` | — | 1 | `$` multiline (OP_DOLLM); interpreter.ml:3126-3161 via `Newline.is_newline` |
| 15 | `FAIL` | — | 1 | grouploop exhausted: propagate backtrack (no tick). A last-branch ALT's handler; forward flow JMPs over it |
| 16 | `CAP_START` | `ovbase` | 2 | open capture N (`ovbase = 2N`): push a CAP cleanup record, set `ovector[ovbase]` to the current position |
| 17 | `CAP_END` | `ovbase` | 2 | close capture N (the group's ket): set `ovector[ovbase+1]` to the current position |
| 18 | `REP` | `reptype; lmin; lmax; c` | 5 | caseful single-char repeat |
| 19 | `REPI` | `reptype; lmin; lmax; c1; c2` | 6 | caseless single-char repeat (`c2 = fcc(c1)`) |
| 20 | `NOTREP` | `reptype; lmin; lmax; c` | 5 | caseful negated-char repeat (match `≠ c`) |
| 21 | `NOTREPI` | `reptype; lmin; lmax; c1; c2` | 6 | caseless negated-char repeat (match `∉ {c1,c2}`) |
| 22 | `GROUP_START` | `g` | 2 | repeated-group iteration entry: record the start position in `mb.group_start.(g)` (empty-check source), pushing a `KIND_GSTART` to restore it on backtrack |
| 23 | `BRAZERO` | `skip` | 2 | greedy zero-repeat (`OP_BRAZERO`): try the group (tick, child frame), skip to `skip` (past the group) on backtrack |
| 24 | `BRAMINZERO` | `skip` | 2 | lazy zero-repeat (`OP_BRAMINZERO`): jump to `skip` (the continuation) first (tick, child frame), enter the group (`pc+2`) on backtrack |
| 25 | `KET_RMAX` | `entry; g` | 3 | greedy repeating ket (`OP_KETRMAX`): empty-check `g` (`no_group` = none) then loop back to `entry` (tick), give back the continuation (`pc+3`) on backtrack |
| 26 | `KET_RMIN` | `entry; g` | 3 | lazy repeating ket (`OP_KETRMIN`): empty-check `g` then try the continuation (`pc+3`, tick), reiterate at `entry` on backtrack |
| 27 | `TYPE` | `type_op` | 2 | one character-type test; `type_op` is the C opcode (`OP_NOT_DIGIT`..`OP_VSPACE` / `OP_ANY` / `OP_ALLANY` / `OP_ANYBYTE` / `OP_ANYNL`). `OP_ANY` (newline-sensitive + CRLF-partial) and `OP_ANYNL` (\R, variable length) get special arms; the rest are one code unit via `simple_type_match` |
| 28 | `CLASS` | `map_off` | 2 | one 32-byte-bitmap class test (`OP_CLASS`/`OP_NCLASS` — identical in non-UTF where every code unit is 0..255). `map_off` = byte offset of the bitmap in `re.code` (like the JIT / XCLASS, no copy) |
| 29 | `WORDBOUND` | `want` | 2 | `\b` (`want=1`, `OP_WORD_BOUNDARY`) / `\B` (`want=0`, `OP_NOT_WORD_BOUNDARY`), non-UCP. The prev-char read lowers `mb.start_used_ptr` (the SCHECK_PARTIAL floor, pcre2_match.c:6280) |
| 30 | `TYPE_REP` | `reptype; lmin; lmax; type_op` | 5 | character-type repeat (`OP_TYPESTAR`..`OP_TYPEPOSUPTO`) |
| 31 | `CLASS_REP` | `reptype; lmin; lmax; map_off` | 5 | class repeat (`OP_CLASS`/`OP_NCLASS` + an `OP_CR*` quantifier) |
| 32 | `REF` | `ovbase; caseless` | 3 | numbered backref (`OP_REF`/`OP_REFI`), no repeat: `match_ref` once (`ovbase = 2N`, fast public ovector), then continue |
| 33 | `REF_REP` | `reptype; lmin; lmax; ovbase; caseless` | 6 | numbered backref + `OP_CR*` repeat |
| 34 | `DNREF` | `slot_base; count; caseless` | 4 | duplicate-named backref (`OP_DNREF`/`OP_DNREFI`), no repeat: scan the name-table list (`slot_base` = byte offset, `count` entries) for the first SET group (`dnref_scan`), then `match_ref` |
| 35 | `DNREF_REP` | `reptype; lmin; lmax; slot_base; count; caseless` | 7 | dup-named backref + `OP_CR*` repeat |
| 36 | `CAP_START_REF` | `ovbase` | 2 | open a REFERENCED capture N (`ovbase = 2N`): push a `KIND_CAPSTART` (private-scratch save), set `mb.cap_start[ovbase]` to the current position — the ovector slot stays UNSET until CLOSE |
| 37 | `CAP_END_REF` | `ovbase` | 2 | close a REFERENCED capture: push a `KIND_CAP` (ovector save), write `ovector[ovbase] = cap_start[ovbase]` and `ovector[ovbase+1]` = current position |
| 38 | `REVERSE` | `number` | 2 | `OP_REVERSE` fixed lookbehind step (`eptr -= number`, lower `start_used_ptr`); NOMATCH if too close to the start |
| 39 | `VREVERSE` | `lmin; lmax` | 3 | `OP_VREVERSE` variable lookbehind step: back up by the clamped max, try forward one unit at a time (KIND_VREVERSE / RM37) |
| 40 | `ONCE` | — | 1 | atomic-group / atomic-positive-assertion ENTRY: push a KIND_ONCE boundary (group-ovector snapshot + enclosing `mb.once_base`), set `mb.once_base` |
| 41 | `ONCE_END` | — | 1 | atomic-group ket (`OP_ONCE`): COMMIT — truncate the body's records to the boundary, restore `mb.once_base`, continue |
| 42 | `ASSERT_END` | `atomic; g` | 3 | positive-assertion ket: `eptr := mb.group_start.(g)` (entry eptr), then commit if `atomic=1` |
| 43 | `NASSERT` | `g; cont` | 3 | negative-assertion ENTRY: push a KIND_NASSERT boundary (snapshot + entry eptr/rdepth + `cont`), set `mb.once_base` |
| 44 | `NASSERT_MATCH` | — | 1 | a negative-assertion branch matched → the assertion FAILS: roll the snapshot back, restore `mb.once_base`, propagate NOMATCH past the boundary |
| 45 | `ASSERTBACK_CHECK` | `g` | 2 | variable-lookbehind end-point check: `eptr == mb.group_start.(g)` else backtrack |
| 46 | `POSSESS` | `cap_ovbase; zero_allowed` | 3 | possessive-bracket ENTRY: push a KIND_POS boundary (snapshot + per-loop state), set `mb.once_base` |
| 47 | `KETRPOS` | `body_entry; cap_ovbase` | 3 | possessive iteration commit (`OP_KETRPOS`): write the capture (if `cap_ovbase>0`), truncate to the boundary, loop back to `body_entry` (or break on empty) |
| 48 | `POSSESS_DONE` | — | 1 | possessive loop end: success iff `matched_once \|\| zero_allowed`, else the group fails |
| 49 | `MARK` | `name_off` | 2 | `OP_MARK`: set `mb.mark`/`mb.nomatch_mark` = the name's byte offset in `re.code`, push a KIND_VERB (`vt_mark`) that reverts the mark on backtrack and catches a name-matching `MATCH_SKIP_ARG` (RM12), continue |
| 50 | `COMMIT` | `mark_off` | 2 | `OP_COMMIT`/`OP_COMMIT_ARG`: `mark_off` = -1 (plain) or the name offset (`_ARG`, also sets mark). Push a KIND_VERB (`vt_commit`); on the continuation's exhaustion it fires `MATCH_COMMIT` (disable bumpalong) |
| 51 | `PRUNE` | `mark_off` | 2 | `OP_PRUNE`/`OP_PRUNE_ARG`: as COMMIT but fires `MATCH_PRUNE` (fail to bumpalong) |
| 52 | `SKIP` | — | 1 | `OP_SKIP`: push a KIND_VERB (`vt_skip`); fires `MATCH_SKIP`, passing back the current position (`verb_skip_ptr = eptr`) |
| 53 | `SKIP_ARG` | `name_off` | 2 | `OP_SKIP_ARG`: the rerun protocol. Count it; while `count <= ignore_skip_arg` it is a no-op; else push a KIND_VERB (`vt_skip_arg`) that fires `MATCH_SKIP_ARG` (`verb_skip_ptr` = the name offset) |
| 54 | `THEN` | `mark_off` | 2 | `OP_THEN`/`OP_THEN_ARG`: push a KIND_VERB (`vt_then`); fires `MATCH_THEN`, passing back this opcode's IR pc (`verb_then_pc`) for the enclosing alternation's KIND_ALT scope check |
| 55 | `ACCEPT` | — | 1 | `OP_ACCEPT`: end the whole match (shares the END recording; ENDANCHORED-and-not-at-end is a DIRECT NOMATCH return, pcre2_match.c:916) |
| 56 | `CLOSE` | `ovbase; referenced` | 3 | `OP_CLOSE` before an ACCEPT: close capture N, pushing a KIND_CAP so it rolls back if ACCEPT then backtracks. `referenced=1` reads the start from `mb.cap_start[ovbase]`, else ovector[ovbase] already holds it |

**Chunk H additions — backtracking control verbs, FAIL, ACCEPT, CLOSE.** Eight
new tags (49-56) and one save kind (KIND_VERB 13). Verbs turn backtracking into
scoped propagation: a verb whose continuation may fail pushes a KIND_VERB record;
on the continuation's exhaustion (NOMATCH backtrack) the record FIRES its verb
code, which then propagates up the save stack via a cold `backtrack_code` (§3),
discarding choice points until a scope boundary handles it — mirroring the C's
RRETURN of `MATCH_COMMIT..MATCH_THEN` through the frame stack. `OP_FAIL` reuses
the existing `FAIL` marker (tag 15) — its runner arm is exactly a NOMATCH
backtrack. THEN scoping is baked into the ALT records via a compile-time parallel
array (§3). `OP_ASSERT_ACCEPT` (( *ACCEPT) inside an assertion) and a NON-ATOMIC
positive assertion combined with ( *THEN) are DECLINED (`Unsupported`, chunk H+ —
they need MATCH_ACCEPT propagation to the assertion boundary / a boundary a NA
assertion does not have). The `hasthen` switch forces all `OP_BRA` groups to the
grouploop lowering (ALT choice point for every branch + FAIL) when the pattern
contains ( *THEN), so every alternation boundary exists for the scope check
(interpreter.ml:2419-2425 / pcre2_match.c:5350). MARK/nomatch_mark are byte
offsets into `re.code` decoded only at the seam (`mark_of_offset`).

**Chunk D additions.** `CIRCM`/`DOLLM` are the multiline anchors (their arms
carry no operands; runtime semantics only). `CAP_START`/`CAP_END` bracket a
capturing group; the group's *ket* IS the `CAP_END` (the branch `JMP`s target
it), so a capturing group emits no separate `KET`. `FAIL` marks a
grouploop group's exhaustion.

**Repeat superinstructions** (`REP`/`REPI`/`NOTREP`/`NOTREPI`). The 13 C
opcode families (STAR MINSTAR PLUS MINPLUS QUERY MINQUERY UPTO MINUPTO EXACT
POSSTAR POSPLUS POSQUERY POSUPTO) × {caseful, caseless} × {plain, NOT}
(OP_STAR..OP_NOTPOSUPTOI, 33-84) collapse to FOUR tags, distinguished only by
case-ness and NOT-ness; the family collapses into the pre-decoded operands
`reptype` (0 min / 1 max / 2 pos), `lmin`, `lmax` (= `Ir.rep_inf` =
`0xFFFFFFFF` for STAR/PLUS, matching interpreter.ml `uint32_max`), and the
char pool: `c` (caseful) or the `c1`/`c2` fold-pair (caseless, `c2 = fcc(c1)`;
NOT variants test `∉`). `Ir.dump` shows `REP <ty> {min,max} "c"` (or
`"c1/c2"`). EXACT is `lmin = lmax` with `reptype` unread.
`Ir_compile.rep_kind`/`rep_bounds` decode the opcode; the char follows the
opcode (offset +1) or the IMM2 count (offset +1+IMM2 for UPTO/MINUPTO/EXACT/
POSUPTO). Type repeats OP_TYPESTAR..OP_TYPEPOSUPTO (85-97) and class repeats
OP_CRSTAR.. stay declined (chunk E).

`BRA`/`KET` are emitted as no-op markers so the IR mirrors the bytecode structure (readable
dumps, jump targets land on real heads); the runner (chunk C2) advances past them. Simple
anchors carry no operands — their runtime semantics land in chunk C2. Multiline `^`/`$`
(`OP_CIRCM`/`OP_DOLLM`), `\b`/`\B`, `\K` are NOT this subset → `Unsupported "... (chunk
C2+)"`.

**Chunk D2 additions — quantified and optional GROUPS.** A repeated group
(bytecode bracket + `OP_KETRMAX`/`OP_KETRMIN`) lowers to the chunk-D group
body (ALT choice points + FAIL / bra_loop last branch) with two changes: (1)
if the bracket is a grouploop bracket (`OP_CBRA`/`OP_SCBRA`/`OP_SBRA` — the C's
`P != NULL`), a `GROUP_START g` precedes the group and the ket carries the
matching `g`, so the empty-string loop check compares the ket position to the
iteration start; an `OP_BRA` repeat (bra_loop, `P == NULL`) carries `g =
Ir.no_group` = −1 and never force-breaks (it can't match empty, mirroring the
C's short-circuit). (2) The ket is `KET_RMAX`/`KET_RMIN` whose `entry` loops
back to the group entry (the `GROUP_START`, or the `BRA` marker for bra_loop).
`OP_BRAZERO`/`OP_BRAMINZERO` (the greedy/lazy zero-repeat wrappers before a
group, for `*`/`*?` and the `{n,m}` optional tail) become one instruction plus
the group body; the skip target (past the group) is patched after the group is
lowered. `OP_SKIPZERO` (a `{0}` group) is elided entirely — no IR, the walk
just steps past the dead bytecode (so an unsupported construct inside a `{0}`
group cannot decline the pattern — strictly safe, the group is never entered).

**Possessive group repeats — LANDED in chunk G** (was declined here in D2).
`OP_KETRPOS` + `OP_BRAPOS`/`OP_CBRAPOS`/`OP_SCBRAPOS`/`OP_SBRAPOS` +
`OP_BRAPOSZERO` (`(?:…)++`, `(…)*+`, …). The commit-but-restore interplay the
D2 decline flagged is solved by the chunk-G atomic-commit machinery (the
`mb.once_base` stack + the KIND_POS boundary's group-ovector snapshot, §3):
each iteration commits by truncating the body's records back to the boundary,
and the boundary's snapshot restores the whole group's captures + eptr if it is
backtracked past — exactly the C's abandon-and-restore-wholesale, without a
frame-copy-back mechanism. See "Chunk G additions" below.

**detect_repeat (`pcre2_jit_compile.c:1699-1835`) — DECLINED PERMANENTLY (not
a coverage gap).** Now that group repeats exist, detect_repeat's recognised
bytecode shapes (a run of identical `(?:X)(?:X)…` collapsed to `OP_EXACT n`, or
`(?:AB){4,6}` → a `{3}` prefix + a bounded `OP_UPTO`/`OP_MINUPTO` tail) DO
occur. But the shared compiler already UNROLLS `{n,m}` into physical bracket
copies (each with its own `OP_KET`), and the interpreter — the fast engine's
differential oracle — runs those copies as-is, ticking (`RMATCH`) once per
copy. detect_repeat is a JIT-ONLY normalization that re-collapses the copies;
the JIT never had a tick-parity obligation. Porting it would make the fast
engine tick FEWER times than the interpreter for an unrolled repeat, so a
`(*LIMIT_MATCH=N)` cap (`-47`) would trip at a different N — a §4 tick-parity
violation and a fuzz `--mode fast-vs-interp` divergence. It is therefore NOT
ported, by design; the unrolled copies are lowered individually (correct,
tick-identical) instead.

**Chunk E additions — character types, classes, and their repeats.** Five new
tags (27-31, above). Design notes:

- **CLASS / NCLASS (non-UTF are the same).** In non-UTF every code unit is
  0..255, and both `OP_CLASS` and `OP_NCLASS` reduce to the same 32-byte bitmap
  probe (`class_bit`, interpreter.ml:721 / `scan_class_min`): the compiler has
  already baked the negation into the bitmap for the 0..255 range. So a class
  is one tag, `t_class` (or `t_class_rep` with an `OP_CR*` quantifier); the
  bitmap is left in place in `re.code` and referenced by byte offset (`Ir.t`
  already pins `Compile.re`; `mb.bytecode` holds `re.code`). A lone class is
  `class_min` with `lmin=lmax=1` in the C — one code unit, no choice point, no
  tick — so `t_class` neither ticks nor records.
- **OP_XCLASS is declined to chunk I, not ported.** In non-UTF/non-UCP the
  shared compiler only emits `OP_XCLASS` for a class that contains a Unicode
  property (`\p`/`\P`) — `compile.ml:2199-2281`, where `xclass = true` implies
  `xclass_has_prop = true` (wide chars > 255 cannot occur non-UTF, and
  `\h`/`\v` etc. stay in the bitmap). Matching it needs the property machinery
  (chunk I), so it is declined (`"fast: OP_XCLASS (\p in class) (chunk I)"`)
  rather than routed through `Xclass.xclass`.
- **Single negated char `OP_NOT`/`OP_NOTI` (`[^a]`).** Lowered as a NOT-char
  repeat with `lmin=lmax=1` (the C's `repeatnotchar` with `Lmin==Lmax` is a
  bare one-char test — no choice point, no tick), reusing `NOTREP`/`NOTREPI`.
- **Type / class repeats reuse the four `REP*` records + `KIND_REP_MIN/MAX`.**
  `setup_rep` decodes any REP superinstruction (char / type / class) into
  `mb.rep_*` including a `rep_kind` discriminator; the forward min/greedy loops
  and the REP_MIN/REP_MAX backtracks dispatch on it. Char / ctype (`\d`..`\W`) /
  class / `\h`\`\v` are "one code unit, predicate" (shared `rep_min_loop` /
  `rep_greedy` via `rep_unit_matches`); `OP_ALLANY`/`OP_ANYBYTE` use a bulk min
  loop + bulk greedy (one bound check — the C's `SCHECK_PARTIAL` at the
  ORIGINAL eptr, not a per-char eptr); `OP_ANY` and `OP_ANYNL` (\R) get
  dedicated loops (newline stop + CRLF-partial for ANY; variable-length CR/LF
  absorption for \R). No new record kinds — a nested repeat clobbers `mb.rep_*`
  freely, so the REP_MIN/REP_MAX backtracks re-run `setup_rep` from the IR at
  `rep_pc` before extending / giving back.
- **CLASS-repeat maxbt ticks the floor; char/type do not.** The C's char/type
  greedy backtrack (`pcre2_match.c:1451/4960`) tries the floor position
  (`Lstart_eptr`) IN PLACE (`if Feptr == Lstart_eptr break`, no RMATCH); the
  CLASS one (`:2143`) tries it via a ticked `RMATCH` (`while Feptr >= Lstart`)
  before failing. `rep_greedy_done_dispatch` therefore routes a class repeat to
  `rep_greedy_done_class` (always push + tick, give back down to and INCLUDING
  floor) and every other kind to `rep_greedy_done` (floor in place, no tick) —
  a §4 tick-parity requirement, not cosmetic.
- **\R greedy give-back skips mid-CRLF (`giveback_pos`).** The C's RM34
  (`:4966-4967`) decrements `Feptr` by one, then — for `OP_ANYNL` only — by one
  MORE if it landed between a CR and its LF (backing into the middle of a CRLF
  is not a valid \R boundary). `giveback_pos` reproduces this on the stored
  give-back position for a `t_type_rep` whose type is `OP_ANYNL`.

**Chunk F additions — backreferences.** Six new tags (32-37, above). Design
notes:

- **`match_ref` (non-UTF/non-UCP)** ports `pcre2_match.c:360-481`
  (interpreter.ml:899-957): unset-group check, the caseless (`lcc`-fold), the
  caseful-partial (unit-by-unit) and the caseful-non-partial (`memcmp`) compare
  loops. The consumed length goes through `mb.ref_length` (the C's
  `*lengthptr`); non-UTF it always equals the reference length. A group is
  "set" iff `ovector[ovbase] != UNSET` — which for a REFERENCED group (written
  only at CLOSE, restored on backtrack) captures exactly the C's
  `offset >= Foffset_top || Fovector[offset] == PCRE2_UNSET`.
- **`OP_REF`/`OP_REFI` single** (no `OP_CR*`) is the C default case
  (`:5045-5056`): `match_ref` once, advance, continue — **no choice point, no
  tick**. Unset-ref semantics (`-1` NOMATCH, or an empty match under
  `PCRE2_MATCH_UNSET_BACKREF`) and zero-length-set-group (empty match) fall out
  of `match_ref` directly.
- **`OP_DNREF`/`OP_DNREFI`** (duplicate-named, needs `PCRE2_DUPNAMES`) first
  scans the name-table list for the first SET group, or the LAST entry when none
  is set (`dnref_scan`, `:5002-5007`); then it is a `REF`. The scan reads
  `mb.name_table` via `Compile.get2` (safe `Bytes.get`) — the verifier proves
  the whole list span lies inside the table so no read raises across the loop.
- **Ref repeats** (`REF_REP`/`DNREF_REP`, the `OP_CR*` forms `:5017-5162`)
  mirror `ref_repeat_head` → `ref_min` → minimize (RM20) / maximize samelengths
  (RM21). In non-UTF every iteration matches exactly `Flength` units, so
  `samelengths` is ALWAYS true and the rare RM22 rescan (`:5164-5184`) never
  occurs. A possessive ref repeat compiles to an atomic group (`OP_ONCE`, out of
  subset → chunk G), so only `OP_CRSTAR`..`OP_CRMINQUERY` / `OP_CRRANGE` /
  `OP_CRMINRANGE` follow a ref. The invariants are decoded by `setup_ref_rep`
  into `mb.ref_*` on the forward path and re-derived on the RM20 backtrack (a
  nested ref may clobber them).
- **Referenced-capture protocol (`optimized_cbracket` = 0).** A capture reached
  by `OP_REF*`/`OP_DNREF*` (or `OP_CREF`/`OP_DNCREF`, or a possessive capture)
  clears its `optimized_cbracket` bit; chunk D ported the analysis and DECLINED
  such captures. Chunk F LOWERS them with the JIT's non-optimized cbracket
  protocol (`pcre2_jit_compile.c:1145`, `11055-11061`, `10692-10702`): the
  in-progress start lives in `mb.cap_start` (NOT the ovector slot — a mid-match
  backref must see only CLOSED values), and both ovector slots are written only
  at CLOSE. `CAP_START_REF` pushes `KIND_CAPSTART` (saving the enclosing
  `cap_start`), `CAP_END_REF` pushes `KIND_CAP` (saving the OLD ovector pair).
  The D design's single entry-save `KIND_CAP` is INSUFFICIENT for referenced
  groups (it writes the ovector start at ENTRY, so an in-progress group would
  read as "set" to a self-reference like `(\1a|b)`, and the ovector would not be
  rolled back when backtracking INTO the group body past a completed CLOSE); the
  split entry-private + close-ovector protocol fixes both. It is
  behaviour-correct for ANY referencing construct, so the gate is generic: an
  out-of-subset referencer (`OP_COND`/`OP_CBRAPOS`) declines ELSEWHERE and the
  lowered IR is never executed. Verified against the interpreter on `(a\1)`,
  `(\1a|b)+`, `((a)\2)+`, `((?:a|ab))+\1` (a re-entered earlier iteration whose
  re-run `CAP_END_REF` must read the restored `cap_start`, which `KIND_CAPSTART`
  provides) and 260k+ fuzz cases.

**Chunk G additions — lookaround, atomic groups, possessive brackets.** Eleven
new tags (38-48) and four new save kinds (KIND_ONCE 9, KIND_NASSERT 10,
KIND_VREVERSE 11, KIND_POS 12). The hard problem — the C frame arena's
abandon-and-restore-wholesale of an atomic construct's captures + eptr on
backtrack-past, which the fast engine's single shared ovector cannot reproduce
once a per-iteration COMMIT truncates the body's per-capture cleanup records — is
solved with two pieces:

- **Group-ovector snapshot in a boundary record.** `t_once` (atomic groups +
  atomic positive assertions), `t_nassert` (negative assertions) and `t_possess`
  (possessive brackets) each push a boundary record (KIND_ONCE / KIND_NASSERT /
  KIND_POS) that snapshots the group ovector slots `[2, 2*oveccount)`. On
  backtrack PAST the construct the snapshot restores those slots — exactly the
  C's P->ovector restore (`pcre2_match.c:6023-6031`). The record is
  VARIABLE-width (`2*oveccount` + a per-kind tail); the runner derives the width
  from `mb.oveccount`. (A range-limited snapshot — only the capture numbers
  textually inside the construct — is a possible optimization; the full group
  snapshot is correct and simplest, and per-attempt allocation stays zero since
  the snapshot lives in the save-stack int array.)
- **`mb.once_base` stack.** A single `mb` int holds the save-stack base of the
  innermost open atomic construct, so the atomic COMMIT (`t_once_end` /
  `t_assert_end` atomic / `t_nassert_match` / `t_ketrpos`) finds the boundary
  without a per-loop `run`/`backtrack` parameter (the hot mainline signature is
  unchanged). Each boundary record stores the ENCLOSING `once_base` and restores
  it on commit or backtrack-past, so nested constructs form a balanced stack.

**Snapshot completeness proof.** The boundary snapshot deliberately covers ONLY
the group ovector slots — NOT `mb.group_start[]` and NOT `mb.cap_start[]` — even
though the atomic COMMIT truncates the body's `KIND_GSTART`/`KIND_CAPSTART`
restore records along with everything else. This is sound because both arrays
are read only at fixed IR positions strictly INSIDE the construct that wrote
them: `mb.group_start.(g)` is read by `t_ket_rmax`/`t_ket_rmin` (the empty-check
of the repeated group whose `t_group_start` wrote `g`) and by
`t_assert_end`/`t_assertback_check` (the assertion whose entry `t_group_start`
wrote `g`); `mb.cap_start.(ovb)` is read only by the matching `t_cap_end_ref`.
After backtracking PAST the construct, those inside positions are unreachable —
there are no recursion/subroutine calls in scope yet, so the only way back in is
through the construct's entry — and on every path that re-enters, the read is
dominated by a fresh write (the entry `t_group_start` / `t_cap_start_ref`
re-executes before any read). Stale values left by the truncation are therefore
never observed; only the ovector (readable ANYWHERE via backrefs and at END)
needs the snapshot. **Chunk K (recursion) MUST revisit this proof**: a
subroutine call `(?N)`/`(?R)` can jump to an IR position inside a construct
without executing its entry, making inside-positions reachable with stale
`group_start`/`cap_start` state — chunk K either widens the snapshot or gives
recursion frames their own save/restore of these arrays.

Design notes:

- **Positive assertions** (`t_assert_end atomic g`). `OP_ASSERT`/`OP_ASSERTBACK`
  are atomic (`t_once` boundary + commit); `OP_ASSERT_NA`/`OP_ASSERTBACK_NA` are
  non-atomic (no boundary, the body's choice points stay live for re-entry).
  BOTH record the entry eptr in `mb.group_start.(g)` via a preceding
  `t_group_start` (reused from D2) and restore it at the ket (a zero-width
  lookahead restores the advanced eptr; a lookbehind's is a no-op). The body
  branches lower grouploop-style (ALT per branch + FAIL), so each branch entry
  ticks at rdepth+1 exactly as the C's RM3 (§4); the continuation runs at the
  matching branch's rdepth (the C's ket `break` in that frame). Captures set
  inside PERSIST forward and are rolled back by the boundary snapshot (atomic) or
  the branch CAP records (NA) on backtrack-past.
- **Negative assertions** (`t_nassert g cont` + `t_nassert_match`). The body
  branches lower grouploop-style; a branch that MATCHES reaches `t_nassert_match`
  (the assertion fails: roll the snapshot back, restore `once_base`, propagate
  NOMATCH past the boundary). Exhausting ALL branches backtracks to the
  KIND_NASSERT record = SUCCESS (`ASSERT_NOT_FAILED`): continue at `cont` with the
  entry eptr/rdepth stored in the record (the C's RM4 resume in the entry frame).
  Negative assertions are atomic by nature (one way to succeed), so the record is
  consumed on success.
- **Lookbehind** (`t_reverse`, `t_vreverse`, `t_assertback_check`). `OP_REVERSE`
  is a fixed back-step (`eptr -= number`, lowering `start_used_ptr`), no choice
  point. `OP_VREVERSE` is the variable back-step: move back by the clamped max,
  then try forward one code unit at a time (KIND_VREVERSE, the RM37 loop —
  `pcre2_match.c:5874-5883`), each attempt a ticked child. A variable-lookbehind
  branch ends with `t_assertback_check g` (Feptr == entry eptr, else backtrack —
  `pcre2_match.c:5995/6009/6038`).
- **Atomic groups** (`t_once` + `t_once_end`). `OP_ONCE`: the body matches, then
  the ket commits (truncate to the boundary, discard internal choice points),
  continuing with the advanced eptr. A possessive ref repeat `\1++` compiles to
  `(?>\1+)` = OP_ONCE and rides this path. A REPEATED atomic group `(?>a)+`
  (Once … KetRmax) combines the atomic commit with a repeating ket and is
  declined (`"fast: repeated atomic group / assertion (chunk G+)"`).
- **Possessive brackets** (`t_possess` + `t_ketrpos` + `t_possess_done`). A
  possessive group is a greedy-atomic repeat. The KIND_POS boundary carries the
  KIND_ONCE snapshot plus the per-loop state (`iter_start` for the empty-match
  check, `matched_once`, `zero_allowed` from `OP_BRAPOSZERO`, `entry_rdepth`).
  The body branches lower grouploop-style — one ALT per branch attempt = one
  RM8 tick (`pcre2_match.c:5291`, §4). A matching branch reaches `t_ketrpos`,
  which writes the capture (capturing brackets, at CLOSE only so a mid-body
  backref sees only closed values — the non-optimized-cbracket property by
  construction), commits the iteration (truncate to the boundary), and loops
  back to the body entry at `entry_rdepth` (an empty match breaks to
  `t_possess_done`). Exhausting the branches (the last ALT's handler) drops into
  `t_possess_done`, which succeeds iff `matched_once || zero_allowed` (else the
  whole group fails). `t_possess_done` is excluded from the KIND_ALT top-level
  tick heuristic (it is a loop `break`, never a ticked branch — the possessive
  body is never at rdepth 0 in practice, but the exclusion makes that safe).
  Verified against the interpreter on `(?:ab)++`, `(a)*+`, `(a)++\1`,
  `((a)b)++`, `(?:a?)*+`, `(?:a+)++` and 285k+ fuzz cases.

**charpos (`pcre2_jit_compile.c:11831-12130`) — DECLINED PERMANENTLY (not a
coverage gap), same class of decision as detect_repeat.** charpos optimises a
GREEDY single-char/type repeat immediately followed by a fixed literal char
(`X*Y` / `X{0,n}Y`, `type != OP_CHAR/CHARI`, `*end == OP_CHAR/CHARI` — e.g.
`.*;`, `[a-z]*=`, `\w*:`): instead of over-eating `X` and trying `Y` at EVERY
give-back position, it scans (memchr-like) for the first/last occurrence of
`Y`'s char and only attempts the continuation where `Y` can match. Those
bytecode shapes DO occur in this chunk's subset (the type/class greedy repeats
are now lowered). But the interpreter — the fast engine's differential oracle —
does the greedy give-back one code unit at a time, and RMATCHes the
continuation (a `match_call_count` tick) at EVERY position from the greedy end
down to the floor, INCLUDING positions where the trailing char does not match
(the `OP_CHAR` fails there, but the RMATCH still ticked). charpos skips exactly
those non-matching positions, so porting it would make the fast engine tick
FEWER times than the interpreter for `.*x`-shaped patterns — a `(*LIMIT_MATCH=N)`
cap (`-47`) would trip at a different N (a §4 tick-parity violation) and a fuzz
`--mode fast-vs-interp` divergence. It is therefore NOT ported; the per-position
give-back (`rep_greedy_done` / `backtrack_rep_max`) is tick-identical to the
interpreter. (The JIT never had a tick-parity obligation; this is the same
differential-contract decision recorded for detect_repeat.)

`Ir.dump : Format.formatter -> Ir.t -> unit` prints one `%3d TAG operands` line per head
(debug_printer.ml style); `CHAR_RUN`/`CHARI` show the escaped literal, `ALT` shows
`next=<pc>`, `JMP` shows the target pc. Used for golden tests (`test/fast/fast_tests.ml`).

- `Ir_verify.check` (static checker, `src/fast/ir_verify.ml`): every instruction head has a
  known tag (`0..max_tag`) whose operands fit in `code`; the program ends with exactly one
  trailing `END`; every jump target (`ALT` handler, `JMP` target) is in range AND on an
  instruction head; every `CHAR_RUN` lit reference is in-bounds (`0 <= off`,
  `off+len <= |lit|`, `len >= 1`); ALT save-record metadata is consistent (handler is a
  valid head; record width is the fixed `arity.(ALT)`). Virtual-frame accounting balance is
  added with the runner (chunk C2). Runs after every compile in tests; a cheap subset will
  be always-on in `Fast.compile` once the runner exists. There is no runtime fallback, so
  the verifier is the guarantee that accepted IR cannot reach an unhandled instruction
  mid-match.

`Ir_compile.compile : Compile.re -> (Ir.t, string) result` walks `re.code` (mirroring
debug_printer.ml's opcode walk; LINK_SIZE = 2 via `Compile.get`). Compile-level gates run
BEFORE the walk: UTF/UCP option bits → `"fast: UTF mode (chunk I)"` / `"... UCP mode ..."`;
`PCRE2_FIRSTLINE` → `"... (chunk L)"`. (Chunk D removed the `top_bracket > 0`
gate — capturing groups are now lowered.) The first out-of-subset opcode
yields `Error "fast: <construct> (chunk X)"` (taxonomy §6).

## §3 Runner and save records (choice-point IR landed in C1; runner in C2)

- ONE module-level tail-recursive loop; hot state (pc, eptr, save-stack pointer, rdepth,
  ...) as tail-call int parameters (register discipline). No function boundary per
  backtrack tick — the fusion declined for the mainline (performance-report.md §4) is THE
  design here. **(chunk C2)**
- Backtracking is direct-threaded: a save record's slot 0 is the handler's IR index;
  restore = positional loads, then tail-loop with the new pc. Save SETS are computed at IR-
  compile time per choice point (minimal state, mirrors JIT's backtrack_common chain +
  private data, pcre2_jit_compile.c:244-256) — never the interpreter's full
  28+2×top_bracket frame copy.

**Choice-point lowering of alternation (landed in chunk C1, `ir_compile.ml`).** A
non-capturing, non-repeated group `(?:B1|B2|…|Bn)` (bytecode `BRA b1 ALT b2 ALT … ALT bn
KET`, where each `OP_ALT` link chains forward to the next `OP_ALT`/`OP_KET`) lowers to a
branch-entry choice-point form:

```
    BRA
    ALT next=e2        ; choice point for B1: on fail resume at e2 (B2's entry)
    <B1>
    JMP KET            ; B1 matched through -> skip the rest of the group
e2: ALT next=e3        ; choice point for B2: on fail resume at e3 (B3's entry)
    <B2>
    JMP KET
    …
    <Bn>               ; LAST branch: no choice point, no JMP (flows into KET)
    KET
```

so there are `n-1` `ALT` choice points and `n-1` `JMP`s. `ALT next=` is the save-record
HANDLER: at run time (chunk C2) an `ALT` pushes a save record and
falls through into its branch body; on backtrack the runner pops it, restores `eptr`, and
tail-loops at `handler`. The C's dual-role `OP_ALT` (branch separator that, in forward
flow, jumps to the KET) is split into a branch-entry `ALT` (choice point) plus a
branch-tail `JMP` (skip to KET). `ALT` handlers chain: `ALT_k.next` = the entry of branch
`k+1`, which is that branch's own `ALT` for `k+1 < n`, or `Bn`'s body for `k+1 = n`. A
single-branch group emits just `BRA … KET` (no choice point).
- §6 stack-safety and §8 hot-loop rules of port-conventions.md bind unchanged: iterative
  only, no exceptions across the loop, zero allocation, `[@tailcall]`, `unsafe_*` only
  under bounds-proof comments. Cold instructions go out-of-line (icache; global `-inline`
  regressed in M10).

**Save-record layout (chunk C2 + chunk D, `save_stack.ml`).** Records are now
VARIABLE width. The discriminator `kind` is the record's TOP slot (highest
index) so `backtrack` reads it at `data.(sp-1)` and derives the record base
without knowing the type in advance; each kind has a fixed width. Low index
first:

| kind | width | layout | pushed at | on backtrack |
|---|---|---|---|---|
| `KIND_ALT` | 5 | `handler; eptr; rdepth; then_end; KIND_ALT` | each `ALT` | resume the next alternative at `handler` (restore `eptr`/`rdepth`); `then_end` (chunk H) is the THEN scope boundary, read only by `backtrack_code` |
| `KIND_CAP` | 4 | `ovbase; old_start; old_end; KIND_CAP` | each `CAP_START` / `CLOSE` | restore `ovector[ovbase]`/`[ovbase+1]`, then keep popping |
| `KIND_REP_MAX` | 5 | `rep_pc; try_pos; floor; rdepth; KIND_REP_MAX` | greedy char / type / class repeat with extra units | retry the continuation at `try_pos` (decrement to `floor`; \R skips mid-CRLF; class also tries floor) |
| `KIND_REP_MIN` | 5 | `rep_pc; count; eptr; rdepth; KIND_REP_MIN` | minimizing char / type / class repeat with `lmin<lmax` | match one more unit at `eptr`, retry the continuation |
| `KIND_CONT` | 4 | `target; eptr; rdepth; KIND_CONT` | BRAZERO / BRAMINZERO / greedy KETRMAX / lazy KETRMIN | resume at IR index `target` (restore `eptr`/`rdepth`), NO tick |
| `KIND_GSTART` | 3 | `g; old_start; KIND_GSTART` | each `GROUP_START` (tracked repeated group) | restore `mb.group_start.(g)`, then keep popping |
| `KIND_CAPSTART` | 3 | `ovbase; old_cap_start; KIND_CAPSTART` | each `CAP_START_REF` (referenced capture entry) | restore `mb.cap_start.(ovbase)` to the enclosing value, then keep popping (the ovector was already rolled back by the CLOSE's `KIND_CAP`) |
| `KIND_REF_MIN` | 5 | `rep_pc; count; eptr; rdepth; KIND_REF_MIN` | minimizing ref repeat with `lmin<lmax` (RM20) | match one more copy at `eptr` (`count` up to Lmax), retry the continuation; `rep_pc` re-derives ovbase/caseless/Lmax/cont |
| `KIND_REF_MAX` | 6 | `cont; try_pos; flength; lstart; rdepth; KIND_REF_MAX` | maximizing ref repeat, samelengths (RM21) | give back one copy (`try_pos` −= `flength`, down to and INCLUDING `lstart`), retry the continuation; below `lstart` → NOMATCH |
| `KIND_ONCE` | `2*oveccount+2` | `ov_snapshot[2,2n); prev_once_base; saved_mark; subtype; KIND_ONCE` | each `ONCE` (atomic group / atomic positive assertion) | restore the snapshot + `mb.once_base` + `mb.mark`, then keep popping. `subtype` (chunk H) = 0 atomic group / 1 pos-assert, read only by `backtrack_code` (THEN escape vs contain) |
| `KIND_NASSERT` | `2*oveccount+4` | `ov_snapshot; prev_once_base; cont; eptr_enter; rdepth_enter; saved_mark; KIND_NASSERT` | each `NASSERT` (negative assertion) | ALL branches failed = SUCCESS: restore snapshot + `mb.once_base` + entry `mb.mark`, continue at `cont` with the entry eptr/rdepth |
| `KIND_VREVERSE` | 6 | `body_pc; cur_lmax; lmin; cur_eptr; rdepth; KIND_VREVERSE` | each `VREVERSE` (RM37) | `if cur_lmax<=lmin` NOMATCH, else give up one back-step (`cur_lmax--`, `cur_eptr++`) and retry the branch body |
| `KIND_POS` | `2*oveccount+5` | `ov_snapshot; prev_once_base; iter_start; matched_once; zero_allowed; entry_rdepth; saved_mark; KIND_POS` | each `POSSESS` (possessive bracket) | backtrack PAST the group: restore snapshot + `mb.once_base` + `mb.mark`, keep popping (as `KIND_ONCE`); the extra slots are read only on the forward loop |
| `KIND_VERB` | 5 | `vtype; aux; eptr; old_mark; KIND_VERB` | each `MARK`/`COMMIT`/`PRUNE`/`SKIP`/`SKIP_ARG`/`THEN` (chunk H) | revert `mb.mark` = `old_mark`; MARK keeps backtracking (and catches a name-matching `MATCH_SKIP_ARG` → `MATCH_SKIP`); every other `vtype` FIRES its verb code into `backtrack_code` |

`KIND_ONCE` / `KIND_NASSERT` / `KIND_POS` are VARIABLE-width (the group-ovector
snapshot size depends on `oveccount`); `backtrack` derives the width from
`mb.oveccount`. The single `mb.once_base` int is the save-stack base of the
innermost open atomic construct (or -1), maintained as a balanced stack by those
boundary records so the atomic COMMIT can find the boundary without a
`run`/`backtrack` parameter (the hot mainline signature is unchanged).

`match_call_count`, `hitend`, `start_used_ptr` are NOT saved (monotonic /
constant per attempt). `sp` is a runner tail-call parameter; push/pop are
explicit index arithmetic. The stack resets to `sp = 0` per attempt.
Backtracking with `sp = 0` returns `MATCH_NOMATCH`.

**Backtracking control verbs (chunk H — verb-code propagation).** The C's verb
machinery propagates a special negative return `MATCH_COMMIT..MATCH_THEN` UP
through the frame stack (each RM arm does `if rrc != MATCH_NOMATCH RRETURN(rrc)` —
pass it up — with the assertion / atomic / THEN-scope exceptions). The fast engine
reproduces this with a SECOND, cold backtrack function
`backtrack_code (mb, sp, mcc, vcode)`, entered ONLY when a verb fires (a KIND_VERB
record's `vt_*` on a NOMATCH backtrack), so the hot NOMATCH `backtrack` is
unchanged. It walks the save stack:

- **choice-point records** (`KIND_ALT`, `KIND_CONT`, `KIND_REP_MIN/MAX`,
  `KIND_REF_MIN/MAX`, `KIND_VREVERSE`) are DISCARDED (the verb skips them — a
  NOMATCH would retry them). EXCEPTION: at a `KIND_ALT` a `MATCH_THEN` whose
  `verb_then_pc < then_end` (the ALT is a genuine ≥2-branch alternation and the
  THEN is within this branch) is CONVERTED to NOMATCH by resuming the next
  alternative (`resume_alt`, shared with the hot `backtrack`).
- **restore-only records** (`KIND_CAP`, `KIND_GSTART`, `KIND_CAPSTART`,
  `KIND_VERB`) run their restore (the C's frame unwind) then keep propagating.
  A `KIND_VERB` for a MARK also reverts `mb.mark` and catches a name-matching
  `MATCH_SKIP_ARG`, converting it to `MATCH_SKIP` (RM12).
- **boundaries**: `KIND_ONCE`/`KIND_POS` — COMMIT/SKIP/PRUNE ESCAPE (restore
  snapshot + `once_base` + `mark`, keep propagating); THEN ESCAPES an atomic
  GROUP (subtype 0) but is CONTAINED at a positive ASSERTION (subtype 1) →
  converted to NOMATCH (the assertion fails), reproducing RM3 vs RM2.
  `KIND_NASSERT` — COMMIT/SKIP/PRUNE/THEN → the negative assertion SUCCEEDS
  (continue at `cont`), EXCEPT `MATCH_SKIP_ARG`, which ESCAPES (RM4
  `default: RRETURN`, pcre2_match.c:5567-5574).
- reaching `sp = 0` returns `vcode` to the driver (`run_attempt`), whose rc-switch
  mirrors interpreter.ml:9329-9374: SKIP_ARG → set `ignore_skip_arg` and re-run
  at the same start (the rerun protocol); SKIP → advance to `verb_skip_ptr`;
  PRUNE/THEN/NOMATCH → bump one; COMMIT → disable bumpalong.

**THEN scoping — reconstructing the branch boundaries.** The C's grouploop THEN
check `verb_ecode_ptr < next_ecode && ( *ecode == OP_ALT || *next_ecode ==
OP_ALT)` (pcre2_match.c:5401-5407) is baked, at IR-compile time, into `then_end`
per ALT: the handler value for a ≥2-branch alternation branch, else -1
(single-branch → THEN never converts, it escapes). Stored in `Ir.alt_then_end`
(a side array indexed by ALT pc, kept out of `code`) and copied into the KIND_ALT
record at push time. The `PCRE2_HASTHEN` switch (`Ir_compile`: OP_BRA lowers
grouploop-style when the pattern has ( *THEN), matching pcre2_match.c:5350)
guarantees every alternation has ALT records for the scope check.

**MARK state and the boundary snapshot (the chunk-K revisit, extended).** `mb.mark`
is the C's per-frame `Fmark` (reset per attempt, set forward by MARK/`_ARG`
verbs, reverted on backtrack-past by the KIND_VERB record); `mb.nomatch_mark` is
the sticky failure mark (reset per exec, never reverted). An atomic COMMIT
(`once_commit` / `t_ketrpos`) TRUNCATES the body's KIND_VERB (MARK) records, so —
exactly as the chunk-G "snapshot completeness proof" required the ovector to be
snapshotted — the KIND_ONCE / KIND_NASSERT / KIND_POS boundary records ALSO
snapshot the entry `mb.mark` and restore it on backtrack-past (or, for a matched
negative-assertion branch, at `t_nassert_match`). `mb.mark` is observable at END
and via nothing else; `mb.group_start`/`mb.cap_start` stay OUT of the snapshot
per the original proof, but a possessive CAPTURE now maintains `mb.cap_start`
across its iterations (written at `POSSESS`/`KETRPOS`) so an `OP_CLOSE` at an
inner ( *ACCEPT) reads the CURRENT iteration's start rather than the last
committed one. **Chunk K (recursion) must still revisit the snapshot** for
subroutine re-entry, and should note that the mark/`cap_start` additions here are
subject to the same reasoning.

**Capture save sets (chunk D — the minimal-save design).** A single global
`mb.ovector` (size `2*(top_bracket+1)`, reused across execs, group slots reset
to UNSET per attempt) holds the whole match at `[0,1]` and group N at
`[2N,2N+1]`. Rather than a full-ovector copy at every choice point, each
capturing bracket cleans up ITSELF locally (mirroring the JIT's per-cbracket
entry-save / exhaustion-restore, `pcre2_jit_compile.c:11045-11054` /
`13443-13452`): `CAP_START N` pushes a `KIND_CAP` record saving group N's two
slots and sets `ovector[2N]` to the current position; `CAP_END N` sets
`ovector[2N+1]`; on backtracking PAST the entry the `KIND_CAP` record restores
both slots and keeps popping. So a branch that can write group k's ovector
saves/restores exactly those two slots — computed at IR-compile time (one
`CAP_START`/`CAP_END` pair per capturing bracket), never a wide per-choice-
point copy. `ALT`/repeat choice points therefore save NO ovector slots; the
enclosing group's cleanup already rolls back everything a failed branch wrote
(verified: `(?:(a)x|ay)` on "ay" gives group 1 unset, matching the interp).
`rc` (the pcre2 pair count) is recomputed at END as `highest set group + 1`
(= the interpreter's `end_offset_top/2 + 1`), not tracked incrementally.

**Group-repeat protocol (chunk D2 — the minimal-record design).** The C runs a
repeated group frame-per-iteration: entering the group RMATCHes a branch
(grouploop RM2, or bra_loop's RM1/same-frame), and the repeating ket RMATCHes
back to the bracket (KETRMAX RM7) or forward to the continuation (KETRMIN RM6),
each iteration in a fresh frame. The fast engine drives the SAME loop with the
existing group lowering plus four instructions and one shared record kind:

- `KET_RMAX` (greedy): if the iteration matched empty (`eptr = group_start.(g)`,
  the C's `Feptr == P->eptr`), break the loop and continue past the group (no
  record, no tick — matching the C's `Fecode += 1+LINK_SIZE; break`). Else push
  a `KIND_CONT` resuming at the continuation (the give-back point), tick, and
  loop to `entry`. The `entry` re-runs the group body, whose first `ALT`
  (grouploop) ticks again — so a grouploop reiteration is 2 ticks (RM7 +
  grouploop) and a bra_loop reiteration is 1 (RM7 only, no entry ALT), exactly
  the C's frame counts. On backtrack, `KIND_CONT` runs the continuation with NO
  tick (the C's same-frame `break` after RM7 NOMATCH).
- `KET_RMIN` (lazy): empty case as above; else push a `KIND_CONT` resuming at
  `entry` (the reiteration point), tick, and run the continuation. On backtrack,
  `KIND_CONT` reiterates at `entry` with NO tick (the C's `Fecode -= GET; break`
  dispatching the bracket in the same frame — the entry's grouploop ALT then
  ticks).
- `BRAZERO`/`BRAMINZERO` share `KIND_CONT`: BRAZERO ticks, pushes a give-back to
  the skip target, and falls into the group; BRAMINZERO ticks, pushes an
  enter-the-group point, and jumps to the skip (continuation). Both restore
  with NO tick on backtrack (RM9/RM10's same-frame `break`).
- The empty-string loop check needs each iteration's start position. A single
  `mb.group_start : int array` (indexed by the compile-time group id `g`, sized
  `ir.n_groups`, reused/grown across execs) holds it: `GROUP_START g` writes the
  current position and pushes a `KIND_GSTART` saving the OLD value; on backtrack
  `KIND_GSTART` restores it. This mirrors the C exactly — each C iteration keeps
  its start in its own frame (`P->eptr`), preserved across backtracking into an
  earlier iteration's branch; the `KIND_GSTART` restore reproduces that
  preservation for the shared slot (without it, a later iteration's
  `GROUP_START` clobbers an earlier one and the empty check loops — the bug the
  `^(?:a?b?)*$` conformance unit caught). An `OP_BRA` (bra_loop) repeat carries
  `g = Ir.no_group` and skips the check (the C's `P == NULL`; OP_BRA can never
  match empty, so the check would never fire anyway — but this stays faithful).
- Captures across iterations: each iteration's `CAP_START`/`CAP_END` write group
  `N`'s slots and push a `KIND_CAP` cleanup, so the LAST completed iteration's
  capture survives at END and a given-back iteration is restored by its
  `KIND_CAP` (verified: `(a)+` on `"aa"` → group 1 = `[1,2]`; `(a*)*` on `""` →
  group 1 = `[0,0]`; both match the interpreter).

**`optimized_cbracket` (`pcre2_jit_compile.c:404`, cleared `:1145-1184`).**
`Ir_compile.optimized_cbracket` ports the JIT flag: a capture is "optimized"
(its ovector slot may double as the in-progress start scratch, as `CAP_START`
does) UNLESS reached by `OP_REF`/`OP_REFI` (`:1145`), `OP_CREF` (`:1173`),
`OP_DNREF`/`OP_DNREFI`/`OP_DNCREF` (`:1180-1186`), or `OP_CBRAPOS`/
`OP_SCBRAPOS` (`:1159`). A linear analysis walk over `re.code` (with a
byte-length helper covering the variable-length opcodes — XCLASS, CALLOUT_STR,
arg-verbs, and type-repeat-of-`\p`) clears the bit for referenced groups. A
non-optimized capture uses the private-scratch path (`CAP_START_REF`/
`CAP_END_REF`, chunk F): the in-progress start lives in `mb.cap_start` and both
ovector slots are written only at CLOSE, so a mid-match backref sees only CLOSED
values. Chunk D declined all non-optimized captures (its subset had no
referencing opcode in scope); chunk F flips the gate to the referenced lowering
(behaviour-correct for any referencer) — an out-of-subset referencer
(`OP_COND`, chunk J) still declines at ITS opcode, so the lowered IR is never
executed for it. (A possessive capture `OP_CBRAPOS`/`OP_SCBRAPOS` clears the bit
too, but chunk G does NOT route it through `CAP_START_REF`/`CAP_END_REF`:
`t_ketrpos` writes the ovector directly at the iteration CLOSE, so the "mid-body
backref sees only CLOSED values" property holds by construction without the
`cap_start` scratch.) The walk is written to never index out of bounds (a
conservative early stop only leaves captures optimized).

**Scratch policy (chunk C2).** `save_stack.ml` retains ONE module-level `int array` across
execs, guarded by its OWN `Atomic.t busy` flag (NOT Frames' scratch slot); a concurrent exec
(flag held) falls back to a fresh, never-retained array. `runner.ml` caches ONE match block
(`mb`, the arena-slot-equivalent record holding this state) whose `ss` is the save-stack
scratch, reused only when the exec owns the flag — the single acquire gates both, mirroring
the interpreter's scratch trio (frames.ml scratch slot + `fresh_trio`). Growth is geometric;
on release an array over `scratch_max_retained_ints` (131072 ints, ~1 MiB) is dropped
(frames.ml precedent). Steady state (single-threaded): zero per-exec record/stack
allocation — only the `outcome` record + the result wrapper (measured < 60 minor words/exec,
0 per bump-along attempt).

## §4 Limits — shadow C-frame accounting (landed in chunk C2)

Trip points are byte-identical to the interpreter. The `mb` record (runner.ml) carries the
shadow state; there is no physical frame arena.

- `match_call_count` (`mcc`, a runner tail-call parameter): ticked once per
  C-`new_frame`-equivalent event, compare-old-then-bump exactly as
  interpreter.ml:1260-1267/1301-1306; reset to 0 per attempt (the `mcc = 1` start after the
  frame-0 tick), interpreter.ml:9324 site.
- Depth: virtual `rdepth` (runner parameter); a child branch runs at `rdepth + 1`, restored
  from the save record on backtrack; the check is `new_rdepth >= match_limit_depth`.
- Heap: virtual `heapframes_size` (simulated C bytes) initialized by the `Frames.create`
  arithmetic and grown by the `Frames.grow` arithmetic for frame index `n = new_rdepth`
  (`frame_size_bytes = Frames.frame_size_bytes_for ~top_bracket:0 = 136`); `HEAPLIMIT` /
  `NOMEMORY` fire at C-identical byte points. Persists across attempts within one exec
  (only grows), re-initialized per exec.
- Order at a child tick (transcribes rmatch, interpreter.ml:1188-1267): (1) heap push/grow
  for frame `new_rdepth`, then (2) `mcc` vs `match_limit`, then (3) `new_rdepth` vs
  `match_limit_depth`.
- Limits resolved as `min(re.limit_*, Limits.*)` per interpreter.ml:9825-9837.

**Tick-site table (IR instruction / event → ticks).** `d` = current `rdepth`. Derived from
the OP_BRA/OP_ALT/OP_KET arms: only branch entries that the C rmatches (`grouploop` for the
top-level group — rdepth 0; `bra_loop` non-last branches for nested groups) tick and enter a
child frame. `grouploop`'s single/last branch ticks (the top level "can't optimize" case);
`bra_loop`'s last branch does not (runs in the same frame).

| event | ticks | rdepth after | heap push |
|---|---|---|---|
| attempt entry (frame 0) | 1 (rdepth 0) | 0 | none |
| `BRA`, next head is `ALT` (multi-branch) | 0 | `d` | none |
| `BRA`, single-branch, `d = 0` (top-level group) | 1 (rdepth 1) | 1 | frame 1 |
| `BRA`, single-branch, `d >= 1` (nested group) | 0 | `d` | none |
| `ALT` (any non-last branch entry) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_ALT` → handler non-`ALT`/`FAIL`, saved `d = 0` (top-level last branch) | 1 (rdepth 1) | 1 | frame 1 |
| backtrack pop `KIND_ALT` → handler is `ALT`/`FAIL`, or saved `d >= 1` | 0 | saved `d` | none |
| `JMP`, `KET`, `CHAR_RUN`, `CHARI`, `SOD`/`SOM`/`EOD`/`EODN`/`CIRC`/`DOLL`/`CIRCM`/`DOLLM`, `END` | 0 | unchanged | none |
| `CAP_START`, `CAP_END`, `FAIL`, `GROUP_START` | 0 | unchanged | none |
| `BRAZERO` / `BRAMINZERO` (enter the group / continuation) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| `KET_RMAX`/`KET_RMIN`, empty iteration (`eptr = group_start`) | 0 | `d` | none |
| `KET_RMAX` non-empty: greedy loop-back (RM7, push `KIND_CONT`) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| `KET_RMIN` non-empty: continuation try (RM6, push `KIND_CONT`) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_CONT` (BRAZERO skip / BRAMINZERO enter / KETRMAX give-back / KETRMIN reiterate) | 0 | saved `d` | none |
| backtrack pop `KIND_GSTART` (restore `group_start`, keep popping) | 0 | — | none |
| repeat min-loop / greedy scan (per char consumed) | 0 | unchanged | none |
| `REP`/`REPI`/`NOTREP`/`NOTREPI` EXACT (`lmin=lmax`) or possessive | 0 | unchanged | none |
| minimize repeat, `lmin<lmax`: first continuation try (push `KIND_REP_MIN`) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_REP_MIN`: extend one char + retry (`count<lmax`, matches) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_REP_MIN`: `count>=lmax` / end / mismatch | 0 | — | none (pop, propagate) |
| maximize repeat, greedy end `> floor`: first continuation try (push `KIND_REP_MAX`) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| maximize repeat, greedy end `= floor`: continuation in place | 0 | `d` | none |
| backtrack pop `KIND_REP_MAX`: `try_pos > floor` (give back one) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_REP_MAX`: `try_pos = floor` (min position, in place) | 0 | `d` | none (pop) |
| `REF`/`DNREF` single, `CAP_START_REF`, `CAP_END_REF` | 0 | unchanged | none |
| ref repeat min-loop / greedy scan (per copy `match_ref`) | 0 | unchanged | none |
| ref repeat, `lmin=lmax` (EXACT) or continue (zero-length / unset+MUB) | 0 | unchanged | none |
| minimize ref repeat, `lmin<lmax`: first continuation try (push `KIND_REF_MIN`, RM20) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_REF_MIN`: match one more copy + retry (`count<lmax`) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_REF_MIN`: `count>=lmax` / mismatch | 0 | — | none (pop, propagate) |
| maximize ref repeat: first give-back try at greedy end (push `KIND_REF_MAX`, RM21) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_REF_MAX`: `try_pos−flength >= lstart` (give back one copy) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_REF_MAX`: `try_pos−flength < lstart` (below floor) | 0 | — | none (pop) |

The whole-pattern outer `BRA` (dispatched at rdepth 0) is `grouploop` at the
top level; nested `OP_BRA` is `bra_loop` (rdepth `>= 1`, no THEN in chunk D).
Its last branch is reached either by falling through the `BRA` (single-branch)
or via the preceding `ALT`'s handler landing on a non-`ALT`/`FAIL` head with
saved `d = 0`; both tick.

**Group repeats (chunk D2) compose the ket tick with the entry tick.** A
`KET_RMAX`/`KET_RMIN` row above ticks ONCE (the C's RM7/RM6 RMATCH into a child
frame); the reiteration then re-runs the group entry, whose first `ALT` ticks
AGAIN for a grouploop group (`OP_CBRA`/`OP_SCBRA`/`OP_SBRA`) but not for a
bra_loop single-branch group (`OP_BRA`, no entry `ALT`). So a grouploop KETRMAX
reiteration is 2 ticks (RM7 + grouploop RM2) and a bra_loop one is 1 (RM7
only), matching the interpreter's frame counts exactly (interpreter.ml
6117-6120 RM7 → grouploop/bra_loop). The `BRAZERO`/`BRAMINZERO` tick precedes
the group's own entry tick the same way (the C's RM9/RM10 frame wraps the
bracket dispatch). Verified continuously by the fuzz `--mode fast-vs-interp`
`LIMIT_MATCH=2000` differential (0 divergences over 200k+ cases including
group repeats) and the group-repeat `LIMIT_MATCH` N-sweep test.

**Type / class repeats (chunk E) reuse the char-repeat tick rows** — a
`t_type_rep` / `t_class_rep` ticks exactly like `REP`/`REPI` (min-loop and
greedy scan tick-free; the minimize continuation, the greedy-end continuation,
and every give-back / extend a child-frame tick) because they share
`rep_min_loop` / `rep_after_min` / `rep_greedy` / `backtrack_rep_min` /
`backtrack_rep_max`, mirroring the C's RM25/RM26/RM27/RM28 (char) ≡
RM33/RM34 (type) frame counts. The ONE difference is the CLASS maxbt: the C
tries the floor position via a ticked `RMATCH` (`:2143`, `while Feptr >=
Lstart`) whereas char/type try it in place (`:1451/4960`, `if == break`), so a
class greedy repeat ticks ONE more time (at the floor) than a char/type one —
reproduced by `rep_greedy_done_class`. The single `t_type` / `t_class` /
`t_wordbound` arms never tick (one code unit, no choice point — the C's
`class_min` with `lmin=lmax=1` and the single-type arms dispatch in place).
Verified by the fuzz `--mode fast-vs-interp` `LIMIT_MATCH=2000` differential (0
divergences over 310k+ cases with classes/types/\R/\b) and the type/class-repeat
`LIMIT_MATCH` N-sweep test.

**Backreferences (chunk F) tick like the C's REF_REPEAT.** A single `REF`/
`DNREF` and the `CAP_START_REF`/`CAP_END_REF` bracket never tick (the C's
`continue` / ket at the same level). A ref repeat's min-loop and greedy scan run
`match_ref` tick-free; the minimize continuation (RM20) and every extend-one-
copy tick a child frame; the maximize give-back (RM21) ticks a child frame per
copy given back — and, unlike char/type repeats but LIKE the class maxbt, it
tries the FLOOR position (`lstart`, the Lmin-copies position) via a ticked
`RMATCH` too (the C's `while (Feptr >= Lstart) RMATCH(RM21)`,
pcre2_match.c:5156), so a greedy ref repeat ticks once at the floor before
NOMATCH. In non-UTF every copy is exactly `Flength` units (`samelengths` always
true), so the rare RM22 rescan never fires and there is no tick divergence from
it. Verified by the fuzz `--mode fast-vs-interp` `LIMIT_MATCH=2000` differential
(0 divergences over 260k+ cases with backrefs) and the ref-repeat `LIMIT_MATCH`
N-sweep test.

**Lookaround / atomic / possessive (chunk G) tick at grouploop / RM8 sites.**
The boundary entries (`ONCE`, `NASSERT`, `POSSESS`) and the group-start / ket
markers (`GROUP_START`, `ONCE_END`, `ASSERT_END`, `NASSERT_MATCH`,
`ASSERTBACK_CHECK`, `POSSESS_DONE`, `REVERSE`) NEVER tick — they dispatch in the
current frame, mirroring the C where the OP_ONCE/assertion opcode itself and its
ket `break` add no frame. The WORK ticks at the branch level: assertion / atomic
/ possessive bodies lower grouploop-style, so each branch entry `ALT` ticks a
child frame at rdepth+1 (the C's per-branch RM3 for positive assertions, RM4 for
negative, RM2 for OP_ONCE, RM8 for the possessive loop — all one RMATCH per
branch attempt). A positive assertion's continuation runs at the matching
branch's rdepth (the C's ket `break` in that frame); a negative assertion's
SUCCESS continuation runs at the stored entry rdepth (the RM4 resume in the entry
frame). A `VREVERSE` back-length attempt ticks a child frame at rdepth+1 (RM37),
each RM37 backtrack too. A possessive `KETRPOS` loop-back does NOT tick — the
next iteration's first `ALT` does (the next RM8), and the loop-back resets rdepth
to the boundary's `entry_rdepth`; `POSSESS_DONE` is excluded from the KIND_ALT
top-level tick heuristic (a loop `break`, not a ticked branch). Verified by the
fuzz `--mode fast-vs-interp` `LIMIT_MATCH=2000` differential (0 divergences over
285k+ cases with lookaround/atomic/possessive across 6 seeds) and the
lookaround/atomic/possessive `LIMIT_MATCH` N-sweep test.

**Capturing groups (chunk D) are `grouploop`, so their LAST branch ticks too.**
`OP_CBRA`/`OP_SCBRA` (interpreter.ml:2434-2444 → `grouploop`) record a
backtracking point for EVERY branch, including the final one, unlike a nested
`bra_loop` `OP_BRA` whose last branch runs in the enclosing frame. The IR
mirrors this by emitting an `ALT` choice point for every branch of a capturing
group and pointing the LAST `ALT`'s handler at a `FAIL` (so exhaustion
propagates NOMATCH with no tick). Thus every capturing-group branch ticks at
`d+1` via its `ALT` (the `ALT` rows), and the `FAIL` head is only reached with
saved `d >= 1` (captures nest inside the wrapper), never triggering the
top-level `d = 0` tick. `CAP_START`/`CAP_END` themselves never tick.

**Verbs (chunk H) tick ONCE at their `RMATCH`.** MARK/COMMIT/PRUNE/SKIP/THEN and a
non-no-op SKIP_ARG each reach their continuation via `RMATCH(continuation, RMk)`
(pcre2_match.c:6342/6367/6374/6380/6387/6393/6414/6430/6438), so each ticks a
child frame at `rdepth+1` and the continuation runs at `rdepth+1` — the fast arms
`tick_child (rdepth+1)` then run the continuation at `rdepth+1`. A no-op SKIP_ARG
(`count <= ignore_skip_arg`) is the C's `break` (no RMATCH, no tick, same frame).
OP_FAIL (a bare `RRETURN(NOMATCH)`), OP_ACCEPT (falls to op_end's direct return)
and OP_CLOSE (`break`) do NOT tick. The verb-code propagation (`backtrack_code`)
is pure unwinding: it ticks NOTHING except when it CONVERTS a THEN to NOMATCH at a
`KIND_ALT`, where it calls `resume_alt` — the SAME tick path as a normal NOMATCH
backtrack of that ALT (the §4 top-level heuristic). The `PCRE2_HASTHEN` grouploop
switch changes tick counts for OP_BRA groups, but IDENTICALLY to the interpreter
(which makes the same switch, pcre2_match.c:5350), so tick parity holds. The
SKIP_ARG rerun re-runs the whole match at the same start with
`skip_arg_count`/`ignore_skip_arg` reset exactly as the driver
(interpreter.ml:7513/9339), so the Θ(n·m) tick flow matches. Verified by the
fuzz `--mode fast-vs-interp` `LIMIT_MATCH=2000` differential (0 divergences over
500k+ cases with verbs across 5 seeds) and the verb `LIMIT_MATCH` boundary/sweep
tests.

**Start-of-match scan is required for tick parity (chunk C2, not deferred).** A skipped
attempt does ZERO ticks, so a naive bump-along that runs attempts the interpreter's
first_cu/start_bits/startline/minlength/req_cu scans skip would trip `(*LIMIT_MATCH=N)`
(`-47`) where the interpreter does not, AND — for the anchored first_cu/start_bits gate,
which breaks unconditionally with no partial exception (pcre2_match.c:7188-7215, unlike
first_cu_tail:7300-7315) — would surface a PARTIAL the interpreter suppresses when
`allowemptypartial` holds (e.g. `\A0` on `""` under `PARTIAL_SOFT`: `\A` gives
`max_lookbehind = 1`). runner.ml therefore ports the scalar (uncached, result-identical)
bump_top + first_cu_tail + tail_opts scans; only the memchr result-caching and the
JIT range-skip table are left to chunk L. `PCRE2_FIRSTLINE` is declined at compile
(`Ir_compile`) — its enforcement is entangled with those scans' shortened `end_subject`.

- Continuous check: fuzz `--mode fast-vs-interp` with the imposed `LIMIT_MATCH=2000`
  differential — any `-47`/`-2`/ovector divergence fast-vs-interp is a bug (0 over 150k+
  cases in chunk C2).

## §5 Start-of-match (JIT-mirrored ONLY; lands in chunks L/M)

No invented accelerators (binding user constraint): no BMH-on-full-literal, no
Aho–Corasick, no mid-pattern prefix entry. Exactly the JIT's menu, scalar:
`fast_forward_first_n_chars`/`scan_prefix` + the range skip table
(pcre2_jit_compile.c:5592,6159-6330), single/pair char scan, startline, start_bits, req_cu
(`search_requested_char:6655`), minlength, and `detect_early_fail` watermarks
(`:1292`, types `:232`). Until chunk L the driver uses a naive bump-along (correctness
first; the conformance diff does not depend on scan strategy).

## §6 Unsupported taxonomy

`Unsupported of string` — the string names the construct (stable prefix for skip-report
grouping), e.g. `"fast: IR compiler not yet implemented (chunk C, 11-fast-engine.md)"`,
later `"fast: OP_RECURSE (chunk K)"`. Coverage only widens (baseline ratchet
`fast_baseline_counts.sexp`); chunk K removes the last in-scope reasons.

## §7 Testing hooks

Conformance `--driver=fast` (unsupported→skip, byte-diff otherwise); IR goldens
(`Ir.dump`); fuzz `--mode fast-vs-interp` (from chunk C) + repro-corpus replay through
fast; alloc pins (per-attempt / per-exec major / per-exec minor at the fast seam);
`ulimit -s 512` stack-safety; bench Fast column + G11.2 gates.
