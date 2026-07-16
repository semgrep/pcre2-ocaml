# Remaining work & handoff context — pure-OCaml PCRE2 engine

Snapshot date: 2026-07-08 (after the G8 campaign series; supersedes the
2026-07-10-dated snapshot at `35c6fb6` — that machine clock ran ahead). This
file is the single place to resume from. Read it with the tail of
`ORCHESTRATOR_LOG.md`; the approved master plan is at
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

## 0. G8 CAMPAIGN SERIES — RESOLVED (2026-07-08)

The §0 tangle recorded at `98044cf` was discarded exactly as recommended
(tree reset to `81269db`; the tangle patch remains durably at
`/home/ubuntu/.claude/plans/pcre2-worktree-handoff-20260710.patch`, never
salvaged) and the crash fix was redone cleanly. The campaigns then surfaced
a chain of FOUR further defects, each fixed, reviewed, and committed
separately (full detail in the ORCHESTRATOR_LOG rows for these SHAs):

- `e8eeaa8` — the original crash: was_newline/BACKCHAR OOB read BEFORE the
  subject under MATCH_INVALID_UTF (upstream C bug #1). Engine pins stop@-1
  read-0.
- `13454e2` — seed-101 SIGSEGV in the C oracle: 8-bit GET_UCD has no bounds
  guard (upstream C bug #2); dev-only oracle patch clamps to
  MAX_UTF_CODE_POINT ≡ engine ucd.ml pin; oracle stubs now pad
  pattern/subject with 8 zero slack bytes each side (bounded-overrun
  families read deterministic 0x00). Also: fuzz_diff `--dump-case N`.
- `5c790c2` — seed-20260708 GPF in OUR dev-only stubs: match_data->mark is
  uninitialized on early-error rcs; oracle_test_exec now reads mark only
  for rc >= 0/-1/-2 (pcre2test's consumed set).
- `afa9b41` — engine↔oracle divergence: OP_VREVERSE stop test uses
  start_subject where OP_REVERSE uses check_subject (upstream C bug #3,
  confirmed live in 10.46 — corrupted ovector). Pin: C-literal stop test
  kept; only the BACKCHAR walk bounded; a check_subject floor was REJECTED
  (it would fork DEFINED C behavior — nested lookbehinds walk below
  check_subject because 10.44's max_lookbehind undercounts nesting,
  pcre2_compile.c:9604-9612).
- `3417078` — word-boundary prev-char probe below the subject (family
  member #4): Utf.peek admitted negative indices (String.unsafe_get s (-1),
  §8 violation) AND the old 0-clamp flipped prev_is_word on UCP-letter
  continuation bytes (real divergence, fixed + assert-pinned).

Standing consequences:
- The **oracle build carries two dev-only patches** (`oracle/patches/`,
  wired in flake.nix) pinning C UB to the engine's documented behavior.
  vendor/ stays pristine; the published package is unaffected. **After any
  flake/patch change run `dune clean`** — discover.exe caches the oracle
  path in `oracle/c_library_flags.sexp` and a stale link is silent.
- Regression corpus = **7 repros**, all replaying green via
  `runner --regressions` and in CI.
- **Three upstream-reportable PCRE2 C bugs** (was_newline BACKCHAR OOB;
  8-bit GET_UCD unguarded; VREVERSE start_subject/check_subject asymmetry)
  — all documented in the oracle patch headers / DEVIATION comments; not
  yet reported upstream (user decision).
- Residual documented-open: extuni/repeat-maximize backtrack walks are
  memory-safe but unpinned for \C-manufactured below-subject crossings
  (divergence-only, inside C's UB zone; 0 hits in 600k+ cases) — log row
  for 3417078.

## 2. What is left, in order

### 2.1 fuzz-crash FIX + G8 — DONE
See §0. G8's fuzz half: 200k×{42,101,20260708} + 1M seed-42 all clean at
`3417078`; gate marked in `09-long-tail.md`.

### 2.2 THE PERF GAP — CLOSED 2026-07-09 (user sign-off)
Final: **geomean 2.159** (release profile, full reps; 46.9 → 2.159 =
21.7x total), 4 benchmarks under the ≤2.0 gate, 8 signed-off residuals
(2.15–2.74; worst backref). Full table, campaign record, and the
sign-off note in `10-performance.md` (G9 MET; G10 CLOSED by explicit
user sign-off — the numeric criterion is NOT met, stated plainly
there). Chunk history: chunk 1 = the closure-nest hoist (P0–P6,
6d2be8e→e233089); chunk 2 = release gating, arena reuse, barrier kill,
scan localization, driver hoist + scratch trio, targeted unsafe sweep,
floor specials (6106082→b311b5a). Three permanent alloc pins guard the
zero-alloc properties. The declined next step (recorded): fusing
dispatch/backtrack into the merged rmatch — a large structural rewrite.

The original section below is retained for the measurement history:

**Gate: engine/oracle ≤ 2.0x, geomean AND per-benchmark** (user's hard
requirement).

**Chunk 1 (closure-nest hoist) — DONE** (commits `6d2be8e`→`e233089`,
P0–P6 per the plan at
`/home/ubuntu/.claude/plans/pcre2-perf-chunk1-hoist-plan.md`; every step
battery-gated + fidelity-reviewed, P2/P3-P5/P6 by whole-file
token-equality proofs). The 131-function nest is module-level over the
per-exec `match_state`; `match_` is a 12-line per-attempt entry; the new
`alloc_per_attempt` OUnit test (both Interp and Jit) pins ONE exec
spanning ~100k attempts at < 1000 minor words (measures 205; pre-hoist
≈ 6.3M).

**Bench denominator fix** (`9aba2e1`): 13454e2's stub slack padding
(kept, correct for fuzz/conformance) had turned the oracle bench column
into per-call subject memcpy; bench now uses `oracle_test_exec_nopad`
(timing-only, containment documented) so oracle ≈ raw-C again.

**Chunk 2 diagnosis + plan (2026-07-09)**: measurement-grounded (perf(1)
works via `sudo sysctl kernel.perf_event_paranoid=1`, revert after),
prototype-validated plan at
`/home/ubuntu/.claude/plans/pcre2-perf-chunk2-plan.md`. Headline: the gap
was never the matching loop — it is (a) per-exec frames-arena allocation
(Frames.create's 32KB major-heap array per exec = 81% of http_lines
engine time; C reuses its buffer, pcre2_match.c:7062-7077) and (b) dev
profile's `-opaque` (generic caml_apply at hot cross-module calls; the
published package never has it). Measured prototype ladder: release
8.57 → +arena-reuse 3.42 → +barrier-kill 2.60 → +scan-localization 2.39;
-unsafe ceiling 2.17. Read the plan's do-not-bother list before
proposing levers (jump table already fine; flambda no help; UTF decode a
red herring). **P0 (release-profile gating) DONE** — see the
"Measurement profile" note in `10-performance.md`.

**Honest baseline after chunk 1** (full bench, reps=5, uncontended,
2026-07-09, dev profile — pre-P0): **geomean 10.30x** (was 46.9x);
release-profile equivalent 8.57x. Per-benchmark
engine/oracle: repeat_negclass 5.45, uri 5.96, findall_email 6.08,
email 6.13, pathological 6.85 (the floor; was 7.6), ipv4 7.57, backref
7.80, keywords 10.56, repeat_bounded 14.94, findall_utf 23.92,
utf_letters 25.23, http_lines 30.44. `bench/results.json` (gitignored)
holds the full run. Note: on high-call-count benchmarks the oracle
column includes per-call FFI overhead (match-data create/free,
copy-out) by design — documented in bench.ml.

**Chunk 2 — grind 10.3x → ≤2.0x, profile-guided** (perf(1) unavailable:
`perf_event_paranoid=4`; use Gc counters, bench `--only` sampling,
`-dlambda`/`-dcmm` inspection). Fresh target order from the new numbers:
1. **http_lines 30.4x** — line-anchored scan shape; suspect the
   bump-along/startline path (C uses a fast newline scan; check the
   engine's per-position work vs C's memchr-like advance).
2. **utf_letters 25.2x / findall_utf 23.9x** — the UTF decode path
   (GETCHARINC chains, Utf.getchar call overhead, per-char prop lookups).
3. **repeat_bounded 14.9x, keywords 10.6x** — repeat machinery + multi-
   keyword scan.
4. **The ~6x floor** (pathological 6.85, negclass 5.45): candidate
   levers in likely-impact order: hot-path `Bytes.get`/`Array.get` →
   `unsafe_*` under §8 proof comments (many sites still safe variants);
   frame-slot access caching (fr array + base offset in locals per
   dispatch iteration); opcode dispatch shape (verify the big int
   `match` compiles to a jump table and arms don't box); `land
   0xff`/`Char.code` chains in char arms; ovector-write paths in
   CBRA/KET; possibly flambda (dev shell is NON-flambda; test whether it
   moves the floor materially and document — but the gate should hold on
   the standard compiler if possible).

Then **re-gate**: `dune exec bench/bench.exe` (full, uncontended) +
`compare.exe` exit 0 → mark **G9/G10** in `10-performance.md`.
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
- Per-call match-context limits — **DONE (2026-07-16)**: `?match_limit`,
  `?depth_limit`, `?heap_limit` are exposed on both engine seams
  (`Engine.exec_full`/`exec_captures`, `Pcre2_fast.exec_full`/`exec_captures`)
  and the public `Matcher` surface (`find`/`find_iter`/`captures`/
  `captures_iter`); omitted = build default, bit-for-bit as before. The
  pcre2test `match_limit`/`depth_limit`/`heap_limit`/`recursion_limit`
  MOD_CTM modifiers are now honored (passed to the driver's `exec` as
  per-call args; testinput2:1840 `/(?0)/ match_limit=100 -> error -47`
  passes on all three drivers). `find_limits`/`find_limits_noheap`
  (search-loop semantics) and `offset_limit` stay harness-skipped.
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
