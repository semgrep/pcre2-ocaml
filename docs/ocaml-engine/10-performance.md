# M9 + M10 — Optimization parity & performance hardening

One doc, two gated phases: both are behavior-neutral by definition, so they share the
"zero conformance delta" discipline and this file.

## Phase M9 — Optimization parity

**Goal**: the C library's match-time optimizations, ported for speed with ZERO observable
behavior change: full `auto_possess.ml`, full `study.ml` (start bitmap, req code unit with
`String.index_from`-style skipping).

**Gate G9** (verbatim from plan): conformance unchanged; fuzz clean; preliminary bench
report.

**GATE MET 2026-07-09** — conformance held byte-identical through every M9/M10 chunk
(the standing battery gated each commit); fuzz clean throughout (incl. the G8 1M
campaign and the 50k-per-chunk runs of perf chunk 2); preliminary bench report
committed at `35c6fb6`.

### Chunks

- [x] auto_possess.ml: compare_opcodes + possessification pass over compiled code, opcode
  compatibility tables (pcre2_auto_possess.c:1-1371); PCRE2_NO_AUTO_POSSESS respected
- [x] study.ml full: start-code-unit bitmap (set_start_bits), req code unit + memchr-style
  skipping, minlength completion (pcre2_study.c:1-1915 remainder beyond the M5 subset)
- [x] driver integration: bitmap/req-cu fast-forward in the start-of-match loop gated on
  NO_START_OPTIMIZE, exactly as C (landed with dfa52ee; scan shapes localized in
  chunk-2 P4b, c756bbc)
- [x] verification: conformance byte-identical before/after each chunk (the standing
  battery); fuzz per chunk; bench.exe first full run committed (35c6fb6)

## Phase M10 — Perf hardening

**Goal**: meet the ≤2x gate against the C interpreter through the same OCaml API.

**Gate G10** (verbatim from plan): engine/oracle ratio ≤ 2.0 (geomean AND per-benchmark;
any exception requires explicit user sign-off); `dune build -p pcre2` dependency-free.

**GATE CLOSED 2026-07-09 BY EXPLICIT USER SIGN-OFF** (the user chose "Run P6, then sign
off" at the post-P5 decision point, accepting the residuals that remain after P6). The
numeric criterion is NOT met: final re-gate (release profile, full 5-rep medians,
uncontended, commit `b311b5a`) = **geomean 2.159** vs ≤2.0, with 8 benchmarks over.
Final table (engine/oracle):

| under gate | ratio | signed-off residuals | ratio |
|---|---|---|---|
| http_lines | 1.55 | repeat_negclass | 2.15 |
| findall_utf | 1.76 | keywords | 2.17 |
| utf_letters | 1.80 | ipv4 | 2.30 |
| repeat_bounded | 1.85 | pathological | 2.42 |
| | | uri | 2.43 |
| | | findall_email | 2.54 |
| | | email | 2.56 |
| | | backref | 2.74 |

Campaign record: 46.9 (35c6fb6) → 10.30 (chunk 1, the closure-nest hoist) → 8.57
(release-profile gating) → 2.159 (chunk 2: arena reuse, barrier kill, scan localization,
driver hoist + scratch trio, targeted unsafe sweep, floor specials) — **21.7x total**.
The residual is function-boundary/frame-churn cost vs the C's single-function-with-
registers design; the measured next step (fusing dispatch/backtrack into the merged
rmatch) is a large structural rewrite explicitly declined at sign-off. The `dune build
-p pcre2` half of the gate holds (dependency-free, verified every commit). Stack-safety
proof: under `ulimit -s 512`, (a+)+b on a 10MB subject returns -47 MATCHLIMIT and a
200-deep nested pattern matches at the tail of a 10MB subject (0.7s).

**Measurement profile (decided 2026-07-09, chunk-2 P0)**: the gate is measured under
`--profile release`. Dune's dev profile passes `-opaque`, which strips cmx approximations
and forces generic `caml_apply` calls at every cross-module call in the hot loop (241
sites in interpreter.ml's cmm) — a build configuration the published package
(`dune build -p pcre2`) never uses. Release differs from dev only by dropping `-opaque`
(`-g` stays, asserts stay ON — verified via `dune printenv`). Conformance/fuzz batteries
stay on dev. Measured effect at 9d62182: geomean 10.30 (dev) → 8.57 (release), identical
match counts.

### Chunks

- [x] bench corpus: mariomka regex-benchmark trio (email/URI/IPv4 over large text), keyword
  alternations, bounded repeats, backref-heavy, `\p{L}+` UTF scan, pathological `(a+)+b`
  MATCHLIMIT case, semgrep-style many-small-subjects; compare.exe enforcing the ratio,
  interleaved 5-rep medians, raw-C honesty number recorded (bench/bench.ml, bench/compare.ml,
  bench/raw_c_stubs.c; nightly workflow .github/workflows/bench.yml)
- [x] zero-alloc frame loop: three permanent OUnit pins — alloc_per_attempt (205 minor
  words per exec across ~100k attempts), alloc_per_exec_major (0 major words / 1,000
  execs), alloc_per_exec_minor (28 minor words/exec at the engine seam)
- [x] unsafe access: ~190 sites upgraded under four re-derived §8 proof classes
  (chunk-2 P2 + P5); no polymorphic compare/closures in hot paths (the chunk-1/P4
  hoists eliminated all per-attempt/per-exec closures)
- [x] opcode specialization: CLOSED-UNNEEDED by measurement (chunk-2 diagnosis,
  do-not-bother list): the dispatch `match` already compiles to a jump table, arms
  don't box, and profile heat was in per-exec setup + frame machinery, not opcode
  dispatch shape; the scan-loop localization (P3/P4b) captured the per-family win
  the specialization idea was after
- [x] final proof: full bench recorded (geomean 2.159 — gate closed by user sign-off,
  see above); `dune build -p pcre2` dependency-free (every commit + CI build-pure);
  `ulimit -s 512` stress: 10MB (a+)+b → -47 MATCHLIMIT, 200-deep nesting over 10MB →
  correct match, 0.7s total

## Rough LOC estimate

M9: ~3,300 C LOC → ~2,700 OCaml. M10: ~800 OCaml (bench + tuning churn).

## Notes

- Behavior-neutrality is enforced mechanically: /verify's regression check makes any
  observable optimization effect a hard failure.
- If a benchmark can't reach 2.0, stop and get explicit user sign-off — do not quietly
  drop it from the corpus.
