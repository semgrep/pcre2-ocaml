#!/usr/bin/env bash
# Parallel commit-gate battery (M11+). All stages are independent PROCESSES on
# a pre-built tree, so they run concurrently with no shared mutable state:
# the engines are pure OCaml, corpus inputs are read-only, and fuzz repro
# writes (divergence-only) use distinct hash-based names.
#
# Deliberately NOT parallelized (measurement correctness, not engine
# correctness): within-executable Alcotest cases — the alloc pins measure
# Gc deltas and test/engine pins module-init order (frames_test first).
# `dune runtest` below still runs the separate test executables in parallel.
#
# Usage: scripts/battery.sh [fuzz-seeds...]   (default seeds: 2 random)
# Exit 0 iff every stage passed. Runs from the repo root.
set -u
cd "$(dirname "$0")/.."

SEEDS=("$@")
if [ ${#SEEDS[@]} -eq 0 ]; then SEEDS=($RANDOM$RANDOM $RANDOM$RANDOM); fi

# One dune invocation covers build AND the unit suites (@runtest as build
# rules) — a single lock acquisition, fully parallel internally. Retry a few
# times in case another dune (e.g. an executor agent) briefly holds the lock;
# dune errors on a held lock rather than waiting.
echo "battery: building (@all @runtest, parallel)..."
BUILD_RC=1
for attempt in 1 2 3 4 5; do
  nix develop -c dune build @all @runtest >"$HOME/.cache/battery-build.log" 2>&1
  BUILD_RC=$?
  [ "$BUILD_RC" -eq 0 ] && break
  if grep -q "locked the build directory" "$HOME/.cache/battery-build.log"; then
    echo "battery: dune lock held (attempt $attempt), retrying in 15s..."
    sleep 15
  else
    break
  fi
done
if [ "$BUILD_RC" -ne 0 ]; then
  echo "battery: BUILD/RUNTEST FAILED"
  grep -v '^gen_tables' "$HOME/.cache/battery-build.log" | head -20
  exit 1
fi

RUNNER=_build/default/test/conformance/runner.exe
FUZZ=_build/default/fuzz/fuzz_diff.exe
OUT=$(mktemp -d)
declare -A PIDS

run_bg() { # name cmd...
  local name=$1; shift
  ( "$@" ) >"$OUT/$name.log" 2>&1 &
  PIDS[$name]=$!
}

echo "battery: launching parallel stages (seeds: ${SEEDS[*]})..."
run_bg oracle       $RUNNER
run_bg engine       $RUNNER --driver=engine
run_bg fast         $RUNNER --driver=fast
run_bg regressions  $RUNNER --regressions
run_bg fuzz-oracle  $FUZZ --cases 10000
i=0
for s in "${SEEDS[@]}"; do
  run_bg "fuzz-fast-$i" $FUZZ --mode fast-vs-interp --cases 50000 --seed "$s"
  i=$((i+1))
done
run_bg pure-build   bash -c 'eval $(opam env) && dune build -p pcre2'

FAIL=0
for name in "${!PIDS[@]}"; do
  if ! wait "${PIDS[$name]}"; then
    FAIL=1
    echo "battery: FAIL [$name] — tail of log:"
    tail -8 "$OUT/$name.log" | sed 's/^/    /'
  fi
done

# one-line summaries for the interesting stages
grep -h "compared=" "$OUT"/fuzz-fast-*.log 2>/dev/null | sed 's/^/battery: /'
if [ "$FAIL" -eq 0 ]; then
  echo "battery: ALL STAGES PASS (logs: $OUT)"
else
  echo "battery: FAILURES — logs in $OUT"
fi
exit $FAIL
