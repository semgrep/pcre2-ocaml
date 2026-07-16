---
name: verify
description: Use when validating the working tree after a chunk (or before commit) — build, OUnit, full conformance vs baselines, skiplist staleness, fuzz smoke, bench smoke.
---

# Verify

**Preferred: the parallel battery.** `scripts/battery.sh [seeds...]` (or `make battery`)
runs everything below concurrently as independent processes on a pre-built tree —
one dune invocation for build+@runtest, then the three conformance drivers, the
regressions replay, fuzz seeds, and the pure-package build in parallel (~20s vs ~3min
sequential). Exit 0 = all stages pass; per-stage logs on failure. Safety argument is in
the script header (process-level only; within-executable Alcotest stays sequential
because the alloc pins measure Gc deltas and test/engine pins module-init order).
Do NOT run it while another dune (e.g. a port-executor) is building — it retries on
the lock but results on a mid-edit tree reflect the WIP, not HEAD.

The stages below remain the reference definition (and the fallback when you need to
isolate a single failing stage). Run in order, **stop at the first FAIL**, report which
stage failed and why. All commands run from the repo root via the Nix devshell.

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

## 3b. Fast-engine conformance vs baseline (M11+, once src/fast exists)
```
nix develop -c dune exec test/conformance/runner.exe -- --driver=fast
```
Compare against `test/conformance/fast_baseline_counts.sexp` — a RATCHET (counts only
rise as chunk coverage grows). Any previously-passing fast unit now failing → **FAIL**.
`unsupported:*` skips are expected until chunk K (full parity).

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
nix develop -c dune exec bench/bench.exe -- --quick
nix develop -c dune exec bench/compare.exe
```
(The bench CLI flag is `--quick` — one rep, reduced corpora; an earlier revision of this
skill said `--smoke`, which the CLI never implemented.)
FAIL if `compare.exe` reports ratio > 2.0 (geomean or any per-benchmark, per gate G10 rules).

## Report format
```
verify: PASS|FAIL at stage <n>
conformance: <passed>/<in-scope> (+<new> / -<regressed>)
regressions: <none | file:ordinal list>
notes: <stale skips, new fuzz repros, suspicious zero-delta, ...>
```
