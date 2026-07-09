# M9 + M10 — Optimization parity & performance hardening

One doc, two gated phases: both are behavior-neutral by definition, so they share the
"zero conformance delta" discipline and this file.

## Phase M9 — Optimization parity

**Goal**: the C library's match-time optimizations, ported for speed with ZERO observable
behavior change: full `auto_possess.ml`, full `study.ml` (start bitmap, req code unit with
`String.index_from`-style skipping).

**Gate G9** (verbatim from plan): conformance unchanged; fuzz clean; preliminary bench
report.

### Chunks

- [x] auto_possess.ml: compare_opcodes + possessification pass over compiled code, opcode
  compatibility tables (pcre2_auto_possess.c:1-1371); PCRE2_NO_AUTO_POSSESS respected
- [x] study.ml full: start-code-unit bitmap (set_start_bits), req code unit + memchr-style
  skipping, minlength completion (pcre2_study.c:1-1915 remainder beyond the M5 subset)
- [ ] driver integration: bitmap/req-cu fast-forward in the start-of-match loop gated on
  NO_START_OPTIMIZE, exactly as C (pcre2_match.c:6530-7777 optimization block)
- [ ] verification: conformance byte-identical before/after each chunk (this is the gate);
  fuzz 100k per chunk; bench.exe first full run → preliminary report committed

## Phase M10 — Perf hardening

**Goal**: meet the ≤2x gate against the C interpreter through the same OCaml API.

**Gate G10** (verbatim from plan): engine/oracle ratio ≤ 2.0 (geomean AND per-benchmark;
any exception requires explicit user sign-off); `dune build -p pcre2` dependency-free.

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
- [ ] zero-alloc frame loop: verify with allocation counters (e.g. Gc.minor_words deltas
  per match in a bench mode); eliminate any hot-loop allocation found
- [ ] unsafe access: `Bytes.unsafe_get`/`Array.unsafe_get` under bounds-proof comments at
  the proven hot sites; no polymorphic compare/closures in hot paths (audit + fix)
- [ ] opcode specialization: split/specialize top opcodes by bench profile (e.g. dedicated
  ASCII OP_CHAR fast path) — every specialization stays citation-mapped
- [ ] final proof: full bench suite ≤ 2.0; `nix develop -c dune build -p pcre2` on a
  machine/sandbox WITHOUT libpcre2; `ulimit -s 512` stress run (10MB subjects, deep
  nesting, `(a+)+b`) terminates with correct MATCHLIMIT errors

## Rough LOC estimate

M9: ~3,300 C LOC → ~2,700 OCaml. M10: ~800 OCaml (bench + tuning churn).

## Notes

- Behavior-neutrality is enforced mechanically: /verify's regression check makes any
  observable optimization effect a hard failure.
- If a benchmark can't reach 2.0, stop and get explicit user sign-off — do not quietly
  drop it from the corpus.
