---
name: verify
description: Use when validating the working tree after a chunk (or before commit) — build, OUnit, full conformance vs baselines, skiplist staleness, fuzz smoke, bench smoke.
---

# Verify

Run the stages in order. **Stop at the first FAIL** and report which stage failed and why.
All commands run from the repo root via the Nix devshell (no global OCaml).

## 1. Build
```
nix develop -c dune build @all
```
FAIL on any error. Warnings-as-errors count as errors.

## 2. Unit suites
```
nix develop -c dune runtest
```
Runs the pure OUnit suite over BOTH `Interp` and `Jit` instantiations. When the oracle is
built (post-M0 restructure, machine has libpcre2), this also covers `test/oracle` — the same
functor over the C bindings; if `dune runtest` is partitioned, additionally run:
```
nix develop -c dune runtest test/oracle
```
FAIL on any test failure in either engine tag.

## 3. Full conformance vs baseline
```
nix develop -c dune exec test/conformance/runner.exe
```
Compare per-file pass counts against `test/conformance/baseline_counts.sexp`:
- Any test that was passing in the baseline and now fails → **REGRESSION → FAIL, stop.**
  Report the exact `file:ordinal` list.
- New passes are expected after a PORT chunk; note the delta (the /commit skill records it
  and updates the baseline).
- Zero delta after a PORT chunk is suspicious — flag it (chunk may not be covered by
  in-scope tests yet; acceptable only if the milestone doc says the coverage lands later).

## 4. Skiplist staleness
The runner enforces this itself, but check its summary explicitly:
- A skipped test that now PASSES → FAIL (remove the stale entry — see /commit).
- A `deferred` entry whose revisit-milestone is ≤ the current milestone → FAIL.

## 5. Fuzz smoke (only when the oracle is built)
```
nix develop -c dune exec fuzz/fuzz_diff.exe -- --cases 10000
```
FAIL on any engine-vs-oracle mismatch. The fuzzer writes minimized repros to
`fuzz/corpus/regressions/` — mention new repro files in the report; they become permanent
conformance inputs.

## 6. Bench smoke (post-M9 only)
Skip entirely before gate G9 is marked met in `docs/ocaml-engine/10-performance.md`. After:
```
nix develop -c dune exec bench/bench.exe -- --smoke
nix develop -c dune exec bench/compare.exe
```
FAIL if `compare.exe` reports ratio > 2.0 (geomean or any per-benchmark, per gate G10 rules).

## Report format
```
verify: PASS|FAIL at stage <n>
conformance: <passed>/<in-scope> (+<new> / -<regressed>)
regressions: <none | file:ordinal list>
notes: <stale skips, new fuzz repros, suspicious zero-delta, ...>
```
