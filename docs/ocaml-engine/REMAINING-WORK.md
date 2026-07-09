# Remaining work & handoff context — pure-OCaml PCRE2 engine

Snapshot date: 2026-07-10 (after commit `35c6fb6`). This file is the single
place to resume from. Read it with the tail of `ORCHESTRATOR_LOG.md`; the
approved master plan is at
`/home/ubuntu/.claude/plans/let-s-try-researching-and-shiny-spindle.md`.

## 1. Where the project stands

**Functionally complete.** The pure-OCaml engine (`src/engine/`, library
`pcre2.engine`, behind the frozen `src/bindings.ml` shim) has **full
conformance parity with the C PCRE2 10.44 oracle**: all 3,112 in-scope units
across testinput1/2/4/5/8/9/10 pass byte-identically (matches, ovectors,
MARKs, partials, error codes/offsets/messages); skip-sets are diff-identical
with the oracle (1,531 rows); `--driver=engine --frontier` reports `none`.
`pcre2.ml`/`pcre2.mli`/`intf.ml` have **zero diffs** (interface parity by
construction); `dune build -p pcre2` is pure OCaml, stdlib-only. Pure OUnit
suite 41/41 for both `Interp` and `Jit` on default `runtest`.

Gates **G0–G7 met** and marked in the milestone docs (grep `GATE MET`).
M9 (auto-possess + study) landed behavior-neutral; `Engine.print_code` dumps
are byte-identical to `pcre2test -d`. ~60 commits, every engine chunk
fidelity-reviewed (findings log in ORCHESTRATOR_LOG.md); oracle baseline
never regressed.

## 2. What is left, in order

### 2.1 IN FLIGHT: fuzz-crash FIX (blocks G8)
The 1M-case campaign (seed 42) found one crash: exec of
`(*LIMIT_MATCH=80000)()()((()()))`-shaped input raises
`Invalid_argument("index out of bounds")` through `Engine.exec_full`.
Minimized repro: `fuzz/corpus/regressions/204387-b7915f8f.txt`.
A FIX executor is dispatched (root-cause vs the C, no defensive clamping;
must match oracle behavior exactly; zero conformance delta; repro replays
green; re-run 200k cases on seed 42 + 2 more seeds). After it lands +
review + commit: re-run a full clean 1M campaign, then **mark G8** in
`09-long-tail.md` (conformance half already met).

### 2.2 THE PERF GAP (M10 — the main remaining engineering)
**Gate: engine/oracle ≤ 2.0x, geomean AND per-benchmark** (user's hard
requirement). First honest numbers (commit `35c6fb6`,
`bench/results.json`, clean machine, median-of-5, oracle ≈ raw-C so the
denominator is fair): **geomean 46.9x FAIL**, range 7.6x–95.4x.

Diagnosis (measured, in the log row for `35c6fb6`):
- The interpreter frame loop itself is zero-alloc as designed
  (244 minor words per 10M backtrack ticks) and its floor is **7.6x**
  (`pathological` benchmark: one attempt, 10M ticks, both engines hit the
  same `-47 MATCHLIMIT`).
- The dominant cost is **~66 minor words allocated per match ATTEMPT**
  (per start position): the interpreter's giant mutually-recursive
  function nest lives INSIDE `match_` (`interpreter.ml:604-7460`) and the
  closure graph is rebuilt at every attempt via `attempt`
  (`interpreter.ml:8269-8291`). Email benchmark: 2.6 GB minor heap/rep →
  93.6x. First-char-selective patterns (ipv4) drop to 18x.

Plan, two sequential chunks (both touch `interpreter.ml` — do NOT
parallelize with the crash fix):
1. **Hoist the closure nest**: move the `let rec … and …` graph to module
   level over an explicit state record (mirror the C's shape: `mb` +
   frame arena + the F-slot locals as one mutable state value, allocated
   once per `exec` — or fully static). Expected effect: collapse the
   per-attempt overhead; geomean should fall toward the ~7.6x floor.
   Gate for the chunk: zero conformance delta (per-unit set diff), fuzz
   smoke clean, bench re-run showing the attempt-scaling benchmarks
   (email/uri/findall/repeat_*) collapsing toward pathological's ratio.
   §6/§8 still binding ([@tailcall], no exceptions across the loop).
2. **Grind the dispatch floor 7.6x → ≤2.0x**: profile-guided (perf is
   unavailable on this box — `perf_event_paranoid=4`, lowering denied;
   use OCaml Gc counters + manual instrumentation or `ltrace`-style
   sampling via repeated bench `--only` runs). Candidate levers, in
   likely-impact order: upgrade hot-path `Bytes.get`/`Array.get` to
   `unsafe_*` under proven bounds (§8 requires a proof comment per site —
   many sites still use safe variants); frame-slot access patterns (cache
   `fr` array + base offset in locals per dispatch iteration); opcode
   dispatch shape (the big `match` on int should compile to a jump table —
   verify with `-dlambda`/`-dcmm` that it does and that arms aren't
   boxing); `land 0xff`/`Char.code` chains in char arms; the
   ovector-write paths in CBRA/KET; possibly flambda (`-O3` exists in the
   release profile — the dev shell compiler is NON-flambda; test whether
   an flambda switch changes the floor materially and document, but the
   gate should hold on the standard compiler if possible).
3. **Re-gate**: `dune exec bench/bench.exe` (full, uncontended) +
   `compare.exe` exit 0. Then mark **G9/G10** in `10-performance.md`.
   Per-benchmark >2.0x requires explicit user sign-off if any residual.

### 2.3 Housekeeping (small, after or interleaved)
- **Skip-list freeze** (09-long-tail.md item): final review of
  `test/conformance/skiplist.sexp` (7 out-of-scope callout-trace units +
  1 env RunTest artifact) + the harness modifier-skip census; write the
  freeze note in the doc.
- **CHANGES.md** entry + README.md rewrite (the library is no longer
  "bindings"; describe the pure engine, the 10.44 pin, the dev-only
  oracle) + `dune-project` synopsis/description update. Engine.version
  stays (10,44).
- **build-pure.yml**: uncomment the `@pure-tests` step (parked since G1;
  the suite is green and on runtest — the CI comment is stale).
- **Final plan-update pass**: reconcile all milestone docs' checkboxes,
  mark G8/G9/G10 when earned, close ORCHESTRATOR_LOG with a summary row.
- Kill the stale monitor shell if still looping: `pgrep -f 'until !
  pgrep' ` (PID was 320787; it self-matches fuzz_diff in its cmdline).

### 2.4 Optional / deferred (user-visible decisions, not blockers)
- Harness `bincode`/`fullbincode` wiring to `Debug_printer` would convert
  ~117 testinput10 harness-skips into real coverage (dumps already match).
- `max_varlookbehind` and `offset_limit` knobs: exist in the C contexts;
  our `Engine.compile_ctx` doesn't expose them (t2:1402, 1809-1811 stay
  harness-skipped). Adding = small driver + harness work.
- The eventual **merge to develop**: plan says full parity + perf gate
  first. When both hold: PR from `austin/pcre-ocaml-rewrite` → `develop`
  (repo convention: never commit to develop directly).

## 3. Operational context (how to work on this repo)

- **Toolchain**: no global OCaml. Everything via `nix develop -c dune …`.
  The flake pins OCaml 4.14.2 and builds the C pcre2 **10.44** for the
  oracle (`pcre2c1044` + `pkg-config` must stay in `buildInputs` —
  package `pcre2` has no C deps anymore so nothing pulls them
  transitively; the nixpkgs pkg-config wrapper IGNORES ambient
  PKG_CONFIG_PATH; GCC ignores `-I` dirs duplicated in `-isystem`).
- **Verification battery** (run before every commit; capture exit codes
  via command substitution `x=$(cmd); echo $?` — NEVER `$?` after a
  pipeline):
  - `nix develop -c dune build @all`
  - `nix develop -c dune runtest --force` (OUnit ×2 + engine module-init
    asserts + harness tests)
  - `nix develop -c dune exec test/conformance/runner.exe` (oracle mode —
    must stay at `baseline_counts.sexp`, incl. fuzz-regression replay)
  - `… -- --driver=engine` (must stay at `engine_baseline_counts.sexp`;
    `--frontier` = `none`; `--update-baseline` only when counts
    legitimately move, in the same commit)
  - per-unit regression proof: diff the `--failures` id sets before/after
  - `nix develop -c dune exec fuzz/fuzz_diff.exe -- --cases 10000` smoke
  - `nix develop -c dune build -p pcre2` (pure-package proof)
- **Workflow per chunk**: port-executor agent → fidelity-reviewer agent
  (REVISE loops until PASS; findings history shows this catches real
  crashes/perf violations) → verify → commit
  `[ocaml-engine] <area>: <summary>` with C refs / tests newly passing /
  frontier delta + ORCHESTRATOR_LOG row in the SAME commit (commit column
  `pending`, backfilled with the real SHA in the NEXT commit — never
  amend; amending invalidates recorded SHAs).
- **Binding rules**: `.claude/rules/port-conventions.md` (naming map,
  goto translations, unsigned arithmetic, citation comments, the
  behavior-parity contract §5, stack safety §6, forbidden list §7, hot
  loop §8) and `.claude/rules/commit-conventions.md`.
- **Key seams** (frozen): `src/bindings.ml` names/types;
  `Engine.exec_result` — `Match{ovector(length encodes rc); mark;
  start_char}`, `No_match{mark}`, `Partial{start; mark}`,
  `Error{code; start_char}`; NOMATCH/PARTIAL → `Ok None` at bindings;
  BADOFFSET −33 uniform; mark names are length-prefixed at `mark[-1]`;
  `find_iter` empty-match behavior is frozen-buggy on purpose;
  Jit = alias of Interp (re-tag, always Ok).
- **Chosen defined behaviors where C is UB** (never "fix" toward
  crashing): NO_UTF_CHECK malformed tails read clamped zeros (mirrors
  C's NUL-terminated reads — oracle-verified identical where reachable);
  `Ucd.record_index` clamps > 0x10FFFF (≡ 32-bit dummy record except
  bidiclass/bprops, documented); fuzzer excludes NO_UTF_CHECK draws for
  this reason.
- **Where things live**: engine `src/engine/*.ml` (module ↔ C file map in
  `00-architecture.md`); vendored 10.44 reference `vendor/pcre2/`
  (read-only); harness `test/pcre2test/`; runner + skiplist + baselines
  `test/conformance/`; fuzzer + regression corpus `fuzz/`; benchmarks
  `bench/` (results.json gitignored); oracle C library `oracle/`
  (`oracle_`-prefixed symbols); table generators `gen/`
  (`dune build @gen-check` asserts sync); CI `.github/workflows/`
  (build-pure, conformance incl. fuzz smoke + regression replay,
  bench.yml nightly/dispatch).

## 4. Known quirks worth remembering

- 10.44 quirk: `META_OPTIONS` emits 2 words; `meta_extra_lengths` says 1
  (C consumers hardcode +2). `{,n}` IS a quantifier ({0,n}).
  `pcre2_study.c:878` reads tables without `cbit_type` (copied
  faithfully). `pcre2_xclass.c:79-81` comment is stale vs its own code.
- The oracle's pcre2test stubs read MARK via the length prefix
  (`oracle/pcre2test_stubs.c:117`) — same protocol as the engine.
- Auto-possess/study are behavior-neutral passes: any conformance flip
  after touching them = a port bug, full stop.
- testinput8 has 0 in-scope units (all debug/bincode modifier skips) —
  that's expected, not a hole.
