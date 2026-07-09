# M0 — Infrastructure & harness-against-oracle

**Goal**: everything needed to run the frontier loop before any engine logic exists —
package restructure, vendored reference, generated tables, constants, pcre2test-compatible
harness, conformance runner, C oracle, engine skeleton behind the rewritten `bindings.ml`
seam, CI. This is the keystone milestone: it forever isolates "harness bug" from "engine bug".

**Gate G0** (verbatim from plan): harness + C oracle reproduce testoutput1,2,4,5,8,9,10
byte-identically outside the skip-list; oracle OUnit green.

## Chunks

- [x] **vendor**: pin PCRE2 10.44 sources + testdata under `vendor/pcre2/` with
  provenance/license README — commit 9b1ab09. DONE.
- [x] **build restructure**: (done: 4f8f03f + oracle test driver 38ecb9e; used include_subdirs unqualified) `dune-project` — package `pcre2` drops `dune-configurator` +
  `conf-libpcre2-8`, bump `ocaml >= 4.14`; add unpublished package `pcre2-dev`; move
  `pcre2_stubs.c` + current `bindings.ml` + a copy of the `pcre2.ml` front-end to `oracle/`
  as `Pcre2_c` (satisfies `Pcre2.Matcher`); `config/discover.ml` moves there; delete
  `old_stubs.c`; `src/dune` drops foreign_stubs/c_flags, adds `(include_subdirs qualified)`.
  Proof: `nix develop -c dune build -p pcre2` needs no libpcre2. (~150 LOC dune/opam)
- [x] **constants**: (this commit; ucp.ml rides with generators chunk) `src/engine/options.ml` (pcre2.h.in option bits + PUBLIC masks + error
  ints), `limits.ml` (pcre2_internal.h), `opcodes.ml` (OP_* enum + OP_lengths,
  pcre2_internal.h), `errors.ml` (verbatim message strings, pcre2_error.c:1-345),
  `ucp.ml` (pcre2_ucp.h:1-396). (~900 LOC)
- [ ] **generators + tables**: `gen/gen_tables.exe` (pcre2-dev) parses
  `pcre2_chartables.c.dist` (196 l), `pcre2_ucd.c` (5460 l), `pcre2_ucptables.c` (1533 l)
  → emits committed `chartables.ml`, `ucd_tables.ml`, `ucptables.ml`; dune rule + CI check
  assert in-sync. Hand-port `tables.ml` (pcre2_tables.c:1-234). (~800 LOC gen + ~6.5k generated)
- [ ] **harness**: `test/pcre2test/pcre2test_ml.exe` — pcre2test-compatible: `#` command
  lines, arbitrary delimiters + modifier lists (full list in plan §Testing), subject escape
  processing, `\=` subject modifiers, byte-identical output (` 0: text`, `<unset>`,
  `No match`, `Partial match:`, `MK:`, `Failed: error NNN at offset N: <message>`); `/g`
  implements pcre2test's own empty-match advance (NOTEMPTY_ATSTART retry then CRLF/UTF-aware
  char bump) — independent of the library's frozen `find_iter`. Runs against any
  `Pcre2.Matcher` (oracle now, engine later). (~1,500 LOC)
- [ ] **runner**: `test/conformance/runner.exe` — executes the harness over the curated
  order, diffing against vendored testoutput files; `--frontier` (first failing in-scope
  test in curated order + tallies), `--only file:ordinal`, `--write-baseline`; pin
  `test/conformance/testdata-order` (machine file; see `testdata-order.md`); initial
  `skiplist.sexp` (see `skip-list.md`) and `baseline_counts.sexp`. Enforces skip staleness
  + deferred expiry; replays `fuzz/corpus/regressions/`. (~800 LOC)
- [x] **oracle suite**: (done in 4f8f03f) `test/oracle/` (pcre2-dev) instantiates the existing OUnit functor
  over `Pcre2_c`; guards against suite drift. Existing pure suite unchanged. (~100 LOC)
- [ ] **engine skeleton + swap**: `src/engine/engine.ml/.mli` per the signature in
  `00-architecture.md`, all paths returning not-implemented errors; `frames.ml`,
  `parse.ml`, `compile.ml`, `interpreter.ml` stubs; rewrite `src/bindings.ml` as the pure
  shim (same names/types, BADOFFSET checks, NOMATCH/PARTIAL folding, jit aliases,
  version=(10,44)). Pure OUnit will be red until M1 — conformance baseline starts at ~0
  passes for the engine, but the ORACLE path must satisfy G0. (~400 LOC)
- [ ] **CI**: GitHub Actions — `build-pure` (4.14 + 5.x matrix, no libpcre2, pure runtest),
  `conformance` (runner + baseline delta + staleness + frontier artifact), `differential`
  (apt libpcre2-dev, oracle OUnit, harness-vs-oracle spot check, fuzz smoke once fuzzer
  exists), `bench` (nightly + label; activates post-M9); `ulimit -s 512` stress job.
  Scaffold `fuzz/fuzz_diff.exe` + `bench/` dirs as buildable stubs so CI wiring is real.
  (~400 LOC yaml + stubs)

## Rough LOC estimate

~5,000 hand-written (harness + runner + constants + skeleton + gen) + ~6,500 generated.

## Notes

- G0 is measured with the HARNESS driving the C ORACLE — engine passes are not required.
- The test infrastructure chunks (harness/runner) may land concurrently with this doc;
  reconcile checkboxes via /plan-update before starting M1.
