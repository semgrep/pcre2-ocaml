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
  of subset → chunk F/J/K). **detect_repeat (`:1699-1850`) NOT ported** — it
  normalises repeated identical GROUPS (walks brackets: `OP_BRA`+`OP_KETRMAX`
  etc.), which are declined here (group repeats are out of subset), so its
  recognised bytecode shapes never occur for single-char repeats. It belongs to
  the chunk that admits group repeats.
- [ ] **E — Classes/types** — CLASS/NCLASS bitmap, XCLASS (via `Xclass`), type
  singles+repeats; charpos loops (`:11831-12022`).
- [ ] **F — Backreferences** — REF/REFI/DNREF/DNREFI + ref repeats.
- [ ] **G — Lookaround/atomic** — ASSERT* families, REVERSE/VREVERSE, ONCE.
- [ ] **H — Verbs** — MARK/PRUNE/SKIP/THEN/COMMIT (+_ARG), ACCEPT/FAIL; SKIP_ARG rerun
  protocol (interpreter.ml:9336-9377 semantics).
- [ ] **I — UTF/UCP** — runner UTF decode, per-exec validation, PROP/NOTPROP, caseless via
  `Ucd.othercase`, `\X`, MATCH_INVALID_UTF fragments.
- [ ] **J — Conditionals** — COND/SCOND + CREF/DNCREF/RREF/DNRREF/FALSE/TRUE.
- [ ] **K — Recursion + script-run + callout no-ops** — full parity reached; no
  `Unsupported` remains for in-scope patterns.
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
