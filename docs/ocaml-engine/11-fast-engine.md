# M11 — fast engine (`Pcre2_fast`)

**Goal**: a second pure-OCaml execution engine over the existing compiler's bytecode — a
pre-decoded-IR **fused** runner (dispatch+backtrack in one tail-loop, minimal per-choice-point
saves) whose optimizations are **strictly ports of PCRE2's own JIT logic**
(`pcre2_jit_compile.c`, 10.44, vendored read-only) retargeted to an OCaml IR. The mainline
interpreter is its differential oracle. Approved plan:
`~/.claude/plans/shimmying-wibbling-brooks.md` (2026-07-14).

**Binding constraints (user decisions)**:
- NO fallback to the interpreter — `Fast.compile` returns explicit `Unsupported` until
  coverage reaches **full parity** (end state: never rejects an in-scope pattern).
- Not called "jit" (`Pcre2_fast` / `pcre2.fast` / commit area `fast`); frozen `Pcre2.Jit`
  aliases untouched.
- NO invented accelerators — nothing PCRE2 itself doesn't do (no BMH-on-full-literal, no
  Aho–Corasick, no mid-pattern prefix entry). Every optimization cites
  `pcre2_jit_compile.c`; native design (IR encoding, runner loop, save records) cites
  `fast-design.md`.
- Semantics contract: on supported patterns, observable behavior identical to
  `Engine.exec/exec_full/exec_captures` (rc/ovector/mark/startchar); match/depth limits tick
  at C-equivalent sites; heap limit accounted in the SAME simulated C frame bytes.

**Gate G11**:
1. Full parity — conformance `--driver=fast` byte-identical to the oracle on all in-scope
   units (no `Unsupported` remaining); fuzz fast-vs-interp long run (100k+) clean.
2. Perf — strictly faster than the existing engine on EVERY supported benchmark AND
   geomean ≤ 1.5× of the C-interpreter oracle (release profile, bench/ suite).
3. `dune build -p pcre2` stays dependency-free; `git diff src/pcre2.mli` stays empty.

## Chunks

- [x] **A — Matcher factoring** — `pcre2.matcher` (src/matcher/): verbatim moves of
  Intf/Match/Error/Options/MakeConvenience out of src/intf.ml + src/pcre2.ml; new
  `MakeMatcher` functor (find/captures/split over a `match_raw`/`capture_raw` seam, frozen
  empty-match semantics); pcre2.ml aliases, Interp/Jit bodies byte-identical;
  `src/pcre2.mli` zero diff.
- [x] **B — Scaffolding + process** — src/fast/ skeleton (compile → `Unsupported` for all;
  real compile errors already byte-parity → 307 units pass under `--driver=fast`);
  vendor `pcre2_jit_compile.c`; `fast_driver.ml` + `runner --driver=fast` +
  `fast_baseline_counts.sexp`; `fast-design.md` v0; rules/agents/skills amendments; fix
  verify-skill `--smoke`→`--quick` drift. (Deviation: the fuzz `--mode fast-vs-interp`
  plumbing moved to chunk C, where the fast engine first executes matches.)
- [x] **C1 — IR + IR compiler + static verifier** — 13-tag IR (fast-design.md §2 table):
  CHAR_RUN fusion (`pcre2_jit_compile.c:7479`; CHARI deliberately unfused — conservative
  simplification, the JIT does fuse single-bit-othercase runs, candidate for chunk M),
  BRA/KET markers, ALT choice points + JMP (n-1 lowering, §3), simple anchors, END;
  compile-level UTF/UCP/top_bracket gates; `Ir.dump` goldens (17) + unsupported-reason +
  corrupt-IR verifier tests + sweep (46 cases, test/fast/). Seam unchanged (runner is C2).
- [x] **C2 — save stack + fused runner + seam wiring** — save_stack (scratch reuse, §3
  records: 3-int `[handler; eptr; rdepth]`), the fused tail-loop runner over the C1 subset,
  shadow limit accounting (§4 tick table), `t` becomes the IR record, always-on verifier in
  `Fast.compile`; fuzz `--mode fast-vs-interp`; runner units + fast alloc pins;
  `--driver=fast` ratchet rose 307 → 493. Deviation: the scalar start-of-match scan
  (first_cu/start_bits/startline/minlength/req_cu, uncached) had to land here (not chunk L)
  for LIMIT tick parity + the anchored-gate partial correctness (§4); `PCRE2_FIRSTLINE`
  declined at compile ("chunk L").
- [x] **D — Captures + char repeats** — CBRA/SCBRA grouploop lowering (ALT per
  branch + FAIL) + CAP_START/CAP_END ovector writes + per-cbracket cleanup
  records (KIND_CAP) + optimized_cbracket analysis (`:404,1145-1184`, all
  captures optimized in-subset); four repeat superinstructions REP/REPI/NOTREP/
  NOTREPI (OP_STAR..OP_NOTPOSUPTOI, min/max/pos loops, KIND_REP_MIN/MAX records)
  with §4 tick parity; multiline anchors CIRCM/DOLLM. Ratchet 493 → 701;
  fuzz fast-vs-interp clean over 300k cases. **Declined** (precise reasons):
  OP_CBRAPOS/OP_SCBRAPOS/OP_BRAPOS/OP_SBRAPOS (possessive brackets → chunk G),
  OP_SBRA (non-capturing empty-check bracket; only ever appears with a repeated
  ket → chunk D repeated-group decline), OP_CLOSE (before ACCEPT → chunk H),
  repeated groups KETRMAX/KETRMIN/KETRPOS + OP_BRAZERO/OP_SKIPZERO (optional /
  quantified groups → chunk D-adjacent, need the empty-string loop check), a
  back-referenced/conditional/recursed capture (its referencing opcode is out
  of subset → chunk F/J/K). **detect_repeat (`:1699-1835`) NOT ported** — it
  normalises repeated identical GROUPS (walks brackets: `OP_BRA`+`OP_KETRMAX`
  etc.), which are declined here (group repeats are out of subset), so its
  recognised bytecode shapes never occur for single-char repeats. It belongs to
  the chunk that admits group repeats.
- [x] **D2 — Quantified + optional GROUPS** — the constructs chunk D declined.
  Repeated groups KETRMAX/KETRMIN + their brackets, incl. OP_SBRA/OP_SCBRA (the
  empty-string loop check via `mb.group_start` + `KIND_GSTART` restore, mirroring
  the C's per-frame `P->eptr`, pcre2_match.c:6107); optional groups
  OP_BRAZERO/OP_BRAMINZERO (`KIND_CONT`) and OP_SKIPZERO (elided). New IR tags
  GROUP_START/BRAZERO/BRAMINZERO/KET_RMAX/KET_RMIN; new save kinds KIND_CONT +
  KIND_GSTART (fast-design.md §2/§3/§4). Frame-per-iteration tick parity (RM6/
  RM7/RM9/RM10 + grouploop/bra_loop entry) verified by the fuzz fast-vs-interp
  LIMIT_MATCH differential (0 divergences / 200k+) and a group-repeat LIMIT_MATCH
  N-sweep. Ratchet 701 → 770. **Declined to chunk G:** possessive group repeats
  OP_KETRPOS + OP_BRAPOS/CBRAPOS/SCBRAPOS/SBRAPOS/BRAPOSZERO — the KETRPOS
  frame-copy-back protocol (pcre2_match.c:5283-5328) resists the minimal-save
  design (commit-per-iteration yet restore-captures-on-backtrack-past).
  **detect_repeat (`pcre2_jit_compile.c:1699-1835`) DECLINED PERMANENTLY** — it
  re-collapses the shared compiler's unrolled `{n,m}` bracket copies, which the
  interpreter (the oracle) ticks one per copy; porting it would tick fewer times
  and break the §4 LIMIT_MATCH tick parity. Not a coverage gap — a differential-
  contract decision (fast-design.md §2).
- [x] **E — Classes/types** — CLASS/NCLASS bitmap (byte offset into re.code;
  in non-UTF both reduce to the 32-byte bitmap probe) + class repeats
  (OP_CR*, incl. the CLASS-maxbt floor tick, pcre2_match.c:2143); single
  character types TYPE (\D\d\S\s\W\w / . / \C / \R / \h\H\v\V, with the OP_ANY
  newline+CRLF-partial and OP_ANYNL variable-length arms) + type repeats
  (OP_TYPESTAR..OP_TYPEPOSUPTO); single negated char OP_NOT/OP_NOTI (as a
  NOT-rep{1,1}); \b/\B non-UCP (WORDBOUND, lowering start_used_ptr). New IR tags
  TYPE/CLASS/WORDBOUND/TYPE_REP/CLASS_REP (27-31); the type/class repeats reuse
  the four REP superinstructions' KIND_REP_MIN/MAX records + machinery
  (`setup_rep` decode, `rep_kind` dispatch), \R give-back skips mid-CRLF
  (`giveback_pos`). A tiny cited extraction `Char_predicates.hspace_byte/
  vspace_byte` is now shared by the interpreter and the fast runner (no
  behavior change — engine conformance byte-identical). Ratchet 770 → 1185;
  fuzz fast-vs-interp clean over 310k+ cases. **Declined to chunk I:** OP_XCLASS
  (in non-UTF/non-UCP the compiler only emits it for a \p/\P in a class, which
  needs the property machinery — compile.ml:2199-2281, xclass ⟹ xclass_has_prop),
  OP_PROP/OP_NOTPROP/OP_EXTUNI, the UCP word boundaries.
  **charpos (`pcre2_jit_compile.c:11831-12130`) NOT ported — permanent
  differential-contract decision (fast-design.md §2), same class as
  detect_repeat.** It optimises a greedy type/class repeat followed by a fixed
  char (`.*x`) by scanning for that char instead of trying the continuation at
  every give-back position; those shapes now occur, but skipping the
  non-matching positions ticks FEWER times than the interpreter (which RMATCHes
  every give-back position, incl. failures), breaking §4 LIMIT_MATCH tick
  parity. The per-position give-back is tick-identical without it.
- [x] **F — Backreferences** — REF/REFI (numbered) + DNREF/DNREFI
  (duplicate-named, first-set-wins name-table scan) singles and their OP_CR*
  repeats (minimize RM20 / maximize-samelengths RM21; non-UTF is always
  samelengths so the RM22 rescan never fires; possessive ref repeats compile to
  atomic groups → chunk G). New IR tags REF/REF_REP/DNREF/DNREF_REP +
  CAP_START_REF/CAP_END_REF (32-37); `match_ref`/`dnref_scan`/`setup_ref_rep` +
  ref_min/ref_max helpers. Referenced captures (optimized_cbracket = 0) now
  LOWER with the JIT non-optimized-cbracket protocol: the in-progress start
  lives in mb.cap_start (new KIND_CAPSTART save), both ovector slots written
  only at CLOSE (KIND_CAP at CAP_END_REF), so a mid-match backref sees only
  CLOSED values — the chunk-D decline is replaced. MATCH_UNSET_BACKREF (unset
  ref matches empty) exact both ways. New save kinds KIND_CAPSTART/KIND_REF_MIN/
  KIND_REF_MAX (fast-design.md §2/§3/§4). Ratchet 656/470 → 727/503
  (testinput1/2); fuzz fast-vs-interp clean over 260k+ cases.
  **Declined (unchanged reason):** possessive ref repeat `\1++` (OP_ONCE →
  chunk G); a capture referenced by a conditional (OP_COND → chunk J).
- [x] **G — Lookaround/atomic** — ASSERT* families (positive atomic
  OP_ASSERT/OP_ASSERTBACK, non-atomic OP_ASSERT_NA/OP_ASSERTBACK_NA, negative
  OP_ASSERT_NOT/OP_ASSERTBACK_NOT), lookbehind REVERSE/VREVERSE (fixed + variable
  step-back, with the variable end-point check), atomic groups OP_ONCE, AND the
  possessive brackets re-pointed here from D2/F: OP_BRAPOS/CBRAPOS/SBRAPOS/
  SCBRAPOS + OP_KETRPOS + OP_BRAPOSZERO (`(?:X)++`, `(X)*+`, …), plus the
  possessive ref repeat `\1++` (compiles to OP_ONCE). The hard problem — the C
  frame arena's abandon-and-restore-wholesale of an atomic group's captures/eptr
  on backtrack-past, which the fast engine's single shared ovector cannot roll
  back after a per-iteration COMMIT truncates the body's cleanup records — is
  solved by a group-ovector SNAPSHOT in the boundary record (KIND_ONCE /
  KIND_NASSERT / KIND_POS) + an `mb.once_base` stack maintained by those records
  (fast-design.md §3). New IR tags REVERSE/VREVERSE/ONCE/ONCE_END/ASSERT_END/
  NASSERT/NASSERT_MATCH/ASSERTBACK_CHECK/POSSESS/KETRPOS/POSSESS_DONE (38-48);
  new save kinds KIND_ONCE/KIND_NASSERT/KIND_VREVERSE/KIND_POS (9-12). Positive
  assertions reuse t_group_start for the entry-eptr restore; assertion/atomic
  bodies reuse the grouploop branch lowering (ALT-per-branch tick = RM3/RM4/RM8
  parity, §4). Ratchet 727/503 → 893/534 (testinput1/2); fuzz fast-vs-interp
  clean over 285k+ cases (5 seeds), oracle fuzz clean. **Declined to chunk G+:**
  a REPEATED atomic group / assertion (`(?>a)+` = Once … KetRmax — the
  per-iteration atomic commit combined with a repeating ket); SKIP_ARG / verbs
  stay chunk H; conditional assertions `(?(?=…)…)` stay chunk J (OP_COND).
- [x] **H — Verbs** — MARK/PRUNE/SKIP/THEN/COMMIT (+_ARG), FAIL, ACCEPT, OP_CLOSE;
  SKIP_ARG rerun protocol (interpreter.ml:9329-9374 driver rc-switch). New IR tags
  MARK/COMMIT/PRUNE/SKIP/SKIP_ARG/THEN/ACCEPT/CLOSE (49-56) + one save kind
  KIND_VERB (13). Verbs propagate a MATCH_COMMIT..MATCH_THEN code up the save
  stack via a cold `backtrack_code` (mirroring the C's RRETURN of a verb code
  through the frame stack); choice points are discarded, restore-only records
  (CAP/GSTART/CAPSTART) run their restore, and boundaries handle the code per
  construct. THEN scoping is reconstructed from the enclosing alternation's ALT
  records (a per-ALT `then_end` boundary in a compile-time parallel array +
  the KIND_ALT record; the hasthen switch forces OP_BRA groups grouploop-style
  so every alternation boundary is present — pcre2_match.c:5350). At an atomic
  positive assertion's KIND_ONCE (tagged by a pos-assert subtype) THEN is
  CONTAINED (→ NOMATCH, the assertion fails, pcre2_match.c:5504-5507); at an
  atomic group / possessive boundary THEN escapes; at a negative assertion
  KIND_NASSERT every verb code succeeds EXCEPT SKIP_ARG, which escapes (RM4
  `default: RRETURN`, pcre2_match.c:5567-5574). MARK/nomatch_mark plumbed as
  code offsets (mark_of_offset at the seam); boundary records snapshot the entry
  mark (revert on backtrack-past, the atomic commit truncating inner MARK
  records). Ratchet 893/534 → 1064/612 (testinput1/2); fuzz fast-vs-interp clean
  over 500k+ cases (5 seeds). **Declined to chunk H+:** OP_ASSERT_ACCEPT
  (( *ACCEPT) inside an assertion — needs MATCH_ACCEPT propagation to the
  assertion boundary with capture fishing through nested boundaries); a
  NON-ATOMIC positive assertion (OP_ASSERT_NA/ASSERTBACK_NA) in a pattern that
  also contains ( *THEN) (a NA assertion has no KIND_ONCE boundary, so a THEN
  reaching it cannot be contained).
- [x] **I — UTF-8 mode** (I1 landed) — the UTF compile gate is removed; per-exec UTF
  validation (BADUTFOFFSET, the -3..-23 UTF error passthrough, max_lookbehind
  back-up to `mb.check_subject`, non-invalid path); UTF-aware IR walk (multi-byte
  OP_CHAR/CHARI/NOT/repeats). Runner UTF arms: CHAR_RUN (byte-exact, walk only),
  CHARI (`Ucd.othercase` fold > 127), single NOT/NOTI, single character types
  (`\d\w\s` ASCII code-point-guarded, `\h\v` via `Char_predicates.hspace_char/
  vspace_char`, `.` / OP_ALLANY / OP_ANYBYTE), CLASS/NCLASS (a code point > 255
  matches only OP_NCLASS, read at `map_off-1`), **OP_XCLASS** (single + repeat, via
  the self-contained `Pcre2_engine.Xclass.xclass` — wide chars, ranges AND `\p`
  properties, in UTF and non-UTF alike — this also unlocked non-UTF `\p`-in-class),
  word boundary (BACKCHAR'd previous char, `check_subject` floor), anchors (UTF
  newlines threaded through `Newline.is_newline/was_newline`), char/ctype/class/
  xclass/`\h`/`\v` repeats (char-step forward, one-CHARACTER-at-a-time BACKCHAR
  give-back), dot repeats; bump-along / STARTLINE ACROSSCHAR char-stepping;
  first_cu/req_cu/start_bits/minlength unchanged (already scalar, UTF-correct).
  Conformance `--driver=fast` testinput4 9→253, testinput5 32→183, testinput10
  11→24, zero failures; fuzz fast-vs-interp clean over 500k+ cases (multiple
  seeds). **Declined to chunk I2:** UCP mode (OP_PROP/OP_NOTPROP/OP_EXTUNI/UCP
  word boundary — property predicates, `\X` grapheme clusters, multi-case
  `caseless_sets`); PCRE2_MATCH_INVALID_UTF (fragment carry-on); caseless
  backreferences in UTF (`Ucd.othercase`/caseset fold with variable byte length);
  UTF lookbehind (char-wise OP_REVERSE/OP_VREVERSE); `\R` (OP_ANYNL) in UTF —
  BOTH single and repeated (multi-byte NEL/LS/PS members + variable give-back);
  OP_ALLANY / OP_ANYBYTE repeats in UTF (char-step / no-SCHECK partial
  subtleties).
- [x] **I2 — UCP + UTF remainder** (chunk I closed) — the full I1 decline
  list: OP_PROP/OP_NOTPROP singles + repeats (all property types PT_ANY..
  PT_BOOL, via a pure extraction of the interpreter's prop predicates into
  `Char_predicates` — prop_test/prop_clist_member/caseless_set_member/
  ucp_wordchar, aliased back by the interpreter so the engines share one
  definition), OP_EXTUNI singles + repeats (`Extuni.extuni`, cluster-wise
  give-back via the RM220 pair-table re-walk), UCP word boundaries (WORDBOUND
  want bit 1), UCP caseless folds ((utf||ucp) `Ucd.othercase` in the IR
  compiler's CHARI/NOT/char-repeat folds, the runner's UCP-no-UTF CHARI arm,
  and exec's first_cu2/req_cu2 othercase-land-0xff branch), multi-case
  caseless sets (PT_CLIST + caseless_set_member in the uni-mode match_ref),
  caseless backrefs in UTF/UCP (uni-mode fold; the RM22 differing-lengths
  maximize via the new KIND_REF_MAX2 record — samelengths tracked per the C),
  UTF lookbehind (char-wise REVERSE floored at check_subject; VREVERSE with
  the interpreter's continuation-byte crossing pin + FORWARDCHARTEST RM37
  step), `\R` in UTF (code-point decode incl. NEL/LS/PS in the single arm and
  every repeat loop; BACKCHAR + mid-CRLF give-back), OP_ALLANY repeats in UTF
  (char-step via the generic loops), OP_ANYBYTE repeats (new rk_anybyte:
  no-SCHECK byte-bulk min pcre2_match.c:3041-3044, byte-bulk greedy, char-wise
  RM219/RM202 extend/give-back — the maxbt in-place floor now runs at try_pos,
  which \C can put below a mid-character floor), and PCRE2_MATCH_INVALID_UTF
  (the fragment carry-on driver: entry bad-start skip / end truncation /
  re-validate, ENDLOOP → next_fragment → FRAGMENT_RESTART with per-fragment
  NOTBOL/NOTEOL, true_end_subject for \z + the bumpalong limit, per-fragment
  hitend/match_partial resets, startchar for error returns). New IR tags
  PROP/PROP_REP/EXTUNI/EXTUNI_REP (59-62) + save kind KIND_REF_MAX2 (14). The
  UCP and MATCH_INVALID_UTF compile gates are removed. Ratchet 277/193/24 →
  594/406/43 (testinput4/5/10), zero failures; fuzz fast-vs-interp clean; the
  new LIMIT_MATCH N-sweep entries were mutation-tested (extuni floor tick,
  prop floor dispatch, RM22 pop tick, \C no-SCHECK min — each deliberate
  break fails the suite). §3 snapshot-proof revisit check: I2 adds no
  recursion/subroutine re-entry and its arms read neither group_start nor
  cap_start — the enumeration holds; the real revisit stays chunk K.
  **Remaining declines after I2:** OP_COND/SCOND + conditional refs (chunk J);
  OP_RECURSE / OP_SCRIPT_RUN / callouts (chunk K); `\K` (C2+); repeated
  atomic group / assertion (G+); ( *ACCEPT) inside an assertion and ( *THEN)
  with a non-atomic assertion (H+); PCRE2_FIRSTLINE (chunk L).
- [x] **J — Conditionals** — OP_COND/OP_SCOND + all in-subset condition kinds:
  OP_CREF (numbered group-set), OP_DNCREF (dup-named group-set list scan),
  OP_FALSE/OP_FAIL ((?(DEFINE)…), (?(VERSION<x)…)), OP_TRUE, OP_RREF/OP_DNRREF
  (recursion tests, ALWAYS false with recursion out of subset — chunk K
  revisits), and assertion conditions ((?(?=…)…) etc, positive and negative,
  lookahead and lookbehind incl. variable). Branch selection is DETERMINED by
  the condition — no ALT choice point for the yes/no pick — so the non-assertion
  tests are inline (COND_CREF/COND_DNCREF/COND_FALSE: TRUE falls through, FALSE
  jumps to no_target; OP_TRUE emits no test). Assertion conditions reuse chunk
  G's KIND_NASSERT boundary + grouploop branch lowering: COND_ASSERT pushes the
  boundary (cont = the "did-not-match" branch), a matching assertion branch
  reaches COND_ASSERT_MATCH which COMMITS (converts the KIND_NASSERT to a
  KIND_ONCE — capture persistence + atomicity of pcre2_match.c:5934-5948) and
  takes the "matched" branch; the positive/negative sense picks which of yes/no
  is match vs nomatch (RM5, :5710-5745). OP_SCOND (a repeated conditional that
  might match empty) adds SCOND_DESCEND (the RM35 descend, one tick/iteration)
  and the empty-string loop check via mb.group_start.(g) (a GROUP_START +
  KET_RMAX/KET_RMIN). CREF/DNCREF-referenced groups were already non-optimized
  by the F-era analysis, so their ovector slot (written only at CLOSE) makes the
  "set" test exact vs the interpreter. New IR tags COND_CREF/COND_DNCREF/
  COND_FALSE/COND_ASSERT/COND_ASSERT_MATCH/SCOND_DESCEND (63-68); NO new save
  kind (reuse KIND_NASSERT + KIND_ONCE). THEN/verb scoping at conditional
  branch boundaries: a THEN in a non-assertion conditional branch ESCAPES (the
  branches are deterministic, no KIND_ALT to contain it); a verb (incl. THEN)
  reaching the assertion-condition boundary follows RM5 = RM4 (COMMIT/SKIP/
  PRUNE/THEN → the nomatch branch, SKIP_ARG escapes). Ratchet 1064/612/594/406/43
  → 1142/640/596/407/43 (testinput1/2/4/5/10), zero failures; fuzz
  fast-vs-interp clean (163k+ conditional-inclusive comparisons). **Declined
  (unchanged reasons):** OP_RECURSE / OP_SCRIPT_RUN / callouts (chunk K).
- [x] **K1a — script runs + callouts + repeated atomics** — OP_SCRIPT_RUN
  ((*sr:)/(*asr:), single+repeated), OP_CALLOUT/CALLOUT_STR no-op skips (incl. the
  condition-position callout, C:5623-5638 — the J-era latent shape now lowers),
  repeated atomic groups (?>X)+/* and BRAZERO-wrapped assertions (per-iteration
  KIND_ONCE re-push); +56 units. Deferred with rationale: recursion -> K1b (RECURSELOOP
  needs faithful last_used_ptr tracking; wrong -52 = wrong answer), \K -> K2 (scope gap:
  never chunk-assigned), (*ACCEPT)-in-assertion + (*THEN)+NA -> K2.
- [ ] **K1b — recursion** — OP_RECURSE all forms + last_used_ptr infrastructure +
  RECURSELOOP (-52) parity + fat recursion save record (full ovector/group_start/
  cap_start/mark/current_recurse/once_base — plan in fast-design §3) + RREF un-FALSE
  for recursion-containing patterns + snapshot-proof re-establishment (~161 units).
- [ ] **K2 — \K + H+ cleanups** — OP_SET_SOM (\K, ~36 units), (*ACCEPT) inside all
  4 assertion kinds (~13), (*THEN) with non-atomic assertions. After K2 the decline
  list reads exactly {PCRE2_FIRSTLINE (chunk L)}.
- [ ] **L — JIT start-opts** — scan_prefix + range skip table (`:5592,6159-6330`),
  first_cu/req_cu scalar ports, startline/start_bits/minlength.
- [ ] **M — Early-fail + tuning** — detect_early_fail watermarks (`:1292`, types `:232`);
  CHAR_RUN word-compare (`String.get_int64_ne`) verification.
- [ ] **N — Perf close** — bench Fast column + compare gates; meet G11.2.
- [ ] **O — Gate close** — G11.1 + fuzz long run + plan-update marks the gate.

## Testing (added incrementally; see the approved plan's testing matrix)

IR static verifier after every compile in tests; IR golden dumps; conformance
`--driver=fast` with unsupported-as-skip accounting + `fast_baseline_counts.sexp` ratchet;
fuzz `--mode fast-vs-interp` (pure OCaml) + repro-corpus replay through fast; fast alloc
pins (per-attempt / per-exec major / per-exec minor); bench Fast column + gates; `ulimit -s
512` stack-safety; white-box suites in test/engine; Fast Matcher suite in test/fast
(pins the frozen empty-match find_iter quirk).

## Rough LOC estimate

src/matcher/: ~1,050 moved + ~130 new (chunk A). src/fast/: ~4,500–6,000 OCaml across
B–M (IR compiler ~1,200, runner ~2,500, start_opt ~600, save_stack ~250, verifier ~300,
seam ~300), plus ~1,500–2,500 test LOC.
