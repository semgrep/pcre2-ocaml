# Performance report — pure-OCaml PCRE2 engine (M10)

How the engine went from **46.9× to 2.159× of the C interpreter** (geomean,
21.7× faster overall), what was actually slow, and why. Companion to the
tables-only `test-report.md`; the blow-by-blow is in the `ORCHESTRATOR_LOG.md`
rows and the plans under `~/.claude/plans/pcre2-perf-*`.

Gate (from the approved plan): engine/oracle ≤ 2.0 (geomean **and**
per-benchmark); `dune build -p pcre2` dependency-free. Denominator = the C
10.44 interpreter through the same OCaml API, `oracle ≈ raw-C` (verified).

## 0. TL;DR of the causes

The slowness was **never in the matching loop** — the frame-dispatch loop was
zero-allocation as designed (244 minor words per 10 million backtrack ticks).
It was **allocation that scaled with work instead of with matching**, in two
independent places, both of which the C avoids by construction:

| # | what allocated | how often | heap | fixed by |
|---|---|---|---|---|
| 1 | the interpreter's mutually-recursive **closure nest** | **per match attempt** (per start position) | minor | chunk 1 (hoist) |
| 2 | the backtracking-**frame arena** | **per exec** (per `pcre2_match` call) | major | chunk 2 P1 (reuse) |

Everything else (bounds checks, write barriers, per-character scan overhead,
`-opaque`) was second-order and mopped up in chunk 2.

---

## 1. The big one: mutually-recursive closures rebuilt per attempt

### What the code looked like

The interpreter is **131 mutually-recursive functions** — `rmatch`,
`dispatch`, `backtrack`, `new_frame`, and ~127 opcode/repeat/assertion helpers
— written as one `let rec … and … and …` group. Originally that whole group
lived **inside** the `match_` function, because the functions need `match_`'s
locals: the match block `mb`, the frame arena `a`, the `utf`/`ucp` flags, and a
handful of scratch cells.

```ocaml
let match_ mb a … =
  let branch_end = ref (-1) in          (* per-call locals *)
  …
  let rec rmatch … = …                  (* 131 functions,   *)
  and dispatch … = …                    (* each closing over *)
  and backtrack … = …                   (* mb / a / utf / …  *)
  … in
  new_frame 0 start_ecode 0             (* enter the nest    *)
```

### Why that is expensive (OCaml, non-flambda)

A function nested inside another function and referencing the outer function's
variables is a **closure**: at run time, *entering* `match_` allocates a heap
block that captures the current values of `mb`, `a`, `utf`, the `ref`s, etc. For
a *mutually-recursive* group OCaml allocates **one shared closure block** with
an infix entry point per function, all sharing the captured environment.

Crucially, those captures differ on every call (`a` is a fresh arena, the
`ref`s are fresh), so the compiler **cannot** hoist the block — it genuinely
depends on the per-call environment, and non-flambda 4.14 does no escape/
loop-invariance analysis that would lift the invariant parts. So the entire
131-function closure graph is **rebuilt every time `match_` is entered.**

And `match_` is entered **once per match *attempt*** — i.e. once per candidate
start position in the bump-along scan. For a pattern run over a large subject
that is *one attempt per byte*.

### Why "high memory" — the symptom

The rebuild was ~66 minor words per attempt. That sounds tiny until you
multiply by attempts:

- `findall_email` / `email` over their corpora: **~2.6 GB of minor-heap churn
  per benchmark repetition** — gigabytes allocated and immediately collected,
  doing nothing but reconstructing the same function graph at every start
  position. Ratio: **93–95×**.
- The GC never promotes it (it dies young), so it is invisible to major-heap
  tools and to "is the loop zero-alloc?" checks — but it dominates wall-clock
  through sheer minor-collection frequency.

The tell was the **shape** of the slowdown, and it lines up exactly with how
much *matching-limit / scan work* a benchmark does:

| benchmark | attempts | old ratio | note |
|---|---|---|---|
| `pathological` `(a+)+b` | **1** attempt, 10M backtrack ticks | **7.6×** | closure built once, then the loop runs — the pure dispatch floor |
| `ipv4` | few (first-char selective) | 18× | |
| `email` / `findall_email` | one per byte of a large subject | 93–95× | closure rebuilt per byte |

This is the connection to "the limit": a case that spends its budget **inside
one attempt** (deep backtracking under a high match limit, like `pathological`)
paid the closure cost **once** and sat near the 7.6× floor; a case that spends
its budget **across many attempts** (scanning a big subject) paid it **per
attempt** and blew up to ~95×. The closure rebuild cost scaled with attempt
count, not with match difficulty.

### The fix — hoist the nest (chunk 1, commits `6d2be8e`…`e233089`)

Move the 131-function group to **module level** (a plain top-level `let rec`),
and replace the captured locals with an explicit **`match_state` record**
allocated **once per exec** and threaded as the functions' first argument:

```ocaml
type match_state = { mb : match_block; arena : Frames.t; utf : bool; …;
                     mutable branch_end : int; … }
let rec rmatch st … = …    (* module level: compiled once, *)
and     dispatch st … = …  (* statically allocated, NO per-call closure *)
…
let match_ st ~start_eptr ~start_ecode =   (* 12-line per-attempt entry *)
  st.branch_end <- -1; …;
  new_frame st 0 start_ecode 0
```

Top-level functions are compiled once and carry no environment, so an attempt
now allocates **nothing** for dispatch. This was done as six machine-checkable
steps (P0–P6): introduce the record, hoist helpers, thread `st` through the
core seven then the remaining ~124 functions, then physically move the group
out of `match_`. The P6 move only compiles if the nest captures nothing — so
**the compiler proved the hoist complete**. Reviews used whole-file token-
equality diffs; the P6 move was proven *physically pure* (37,904 nest tokens
identical, the only residue a dropped `in`).

**Result:** a permanent regression test (`alloc_per_attempt`) pins one exec
spanning ~100,000 attempts at **< 1,000 minor words** — it measures **205**,
versus **~6.3 million** before. Geomean **46.9× → 10.30×**; the attempt-scaling
benchmarks collapsed toward the 7.6× floor (email 93.6 → ~6, uri 92 → ~6).

---

## 2. The second one: the frame arena on the major heap (chunk 2 P1)

With the closures gone, profiling (`perf`) said **81% of `http_lines`'s engine
time** was `caml_make_vect` + major-GC marking/sweeping. `Frames.create`
allocated a fresh **~4,000-word (32 KB) zeroed `int array` per exec**. 4,000
words is far past `Max_young_wosize` (256), so it went **straight to the major
heap** and became **major garbage on every `pcre2_match` call** — exec count
predicted the ratio perfectly (http_lines: 200k execs → 30×).

The C never pays this: it **reuses** its `heapframes` buffer across calls
(keep-if-big-enough, `pcre2_match.c:7062-7077`). The original OCaml port had
dropped that reuse (a documented DEVIATION). Chunk 2 P1 restored it: a
module-level scratch arena, acquired per exec under an atomic busy flag, reused
without re-zeroing (C-faithful; the frame protocol is write-before-read).

**The heap-limit angle here is real.** The arena grows geometrically **up to
the match context's heap limit** (`PCRE2_ERROR_HEAPLIMIT`), so a pathological
pattern under a *high heap limit* can grow it large. Two consequences we had to
handle in the reuse design:

- **Retention cap.** The reused arena is only cached back if it is ≤ ~1 MiB
  (`scratch_max_retained_ints = 131072`); a larger grown arena is dropped, so a
  single high-limit pathological match cannot pin megabytes for the life of the
  process. (The C's buffer lifetime is caller-controlled via `match_data`; ours
  is a global scratch, hence the cap — a documented DEVIATION.)
- **Heap-limit accounting is independent of the physical array.** All limit
  checks run on the *simulated* `heapframes_size`, computed fresh per exec, so
  reusing an oversized array can never shift where `HEAPLIMIT` fires. The engine
  matches the oracle's observable (fresh-`match_data`-per-call) behavior, not
  C's history-dependent pre-grow headroom.

**Result:** `alloc_per_exec_major` pins 1,000 execs at **0 major words** (was
~4.02 M). Geomean **8.57× → 3.42×** (http_lines 27 → 2.1, utf_letters 23 → 2.2).

---

## 3. Everything else (chunk 2 P2–P6)

Second-order levers, each `perf`/`cmm`-guided and prototype-measured before
landing:

| step | lever | geomean |
|---|---|---|
| P0 | **gate on `--profile release`** — dev's `-opaque` forces a generic `caml_apply` at every hot cross-module call (241 sites in cmm); the published package never has it | 10.30 → 8.57 |
| P1 | frame-arena reuse (§2) | 8.57 → 3.42 |
| P2 | kill the `Frames.push` write barrier (`Array.blit` → unsafe int loop; int stores need no `caml_modify`) + unsafe hot-path headers under §8 proofs | 3.42 → 2.60 |
| P3 | scan-loop localization — the hot per-character loops (class/char/ctype × min/max) → module-level tail loops over immediates, restoring C's register discipline | 2.60 → 2.39 |
| P4 | driver hoist (same closure fix as chunk 1, applied to the bump-along driver group) + one reusable `mb`/`match_state`/`driver_state` scratch trio; per-exec minor words **123 → 28** | — |
| P5 | ~190 targeted `unsafe_get/set` upgrades in profile-hot arms + `[@inline always]` on the 2-byte `Compile.get`/`get2` (57 call relocations → 0; text −1 KB) | — |
| P6 | merge `rmatch`+`new_frame` on the fast path, straight-line the small frame copy, convert the last 5 arena `Array.blit`s | 2.33 → 2.159 |

Two **measurement-honesty** findings on the way, both worth calling out:

- **The oracle denominator was silently broken** by a (correct, kept) fuzzing
  hardening that copied the subject per call — many-call benchmarks were timing
  `memcpy`, not the C matcher (`repeat_bounded` oracle read 105 s vs raw-C's
  96 ms). Fixed with a timing-only unpadded oracle path (bench-only). Without
  this, the gate numerator/denominator were meaningless.
- **A "hang" that was faithful behavior.** A fuzz case looked like an engine
  hang; measurement showed real C `pcre2test` takes 39 minutes on it too —
  a `(*SKIP:name)` re-run loop resets the match count per re-run, making
  worst-case work Θ(limit³). No engine bug; the fuzzer's imposed limit was
  lowered (80000 → 2000). This is the closest thing to a genuine "high limit"
  pathology we hit — and it was the C's, mirrored faithfully at 4.8×.

Settled by measurement, **not** pursued (recorded so they're not re-litigated):
the opcode dispatch already compiles to a jump table; flambda makes no material
difference post-`-opaque`; global `-inline` regressed (icache); the UTF decode
path was a red herring.

---

## 4. Final result

Full bench, release profile, 5-rep medians, uncontended (commit `b311b5a`):

| benchmark | engine/oracle | | benchmark | engine/oracle |
|---|---|---|---|---|
| http_lines | **1.55** ✓ | | uri | 2.43 |
| findall_utf | **1.76** ✓ | | pathological | 2.42 |
| utf_letters | **1.80** ✓ | | ipv4 | 2.30 |
| repeat_bounded | **1.85** ✓ | | keywords | 2.17 |
| repeat_negclass | 2.15 | | email | 2.56 |
| findall_email | 2.54 | | backref | 2.74 |

**geomean 2.159** — 4/12 under the ≤2.0 gate, 8 residuals (2.15–2.74).

Campaign: **46.9 → 2.159 = 21.7×.** The `dune build -p pcre2` half of the gate
holds (dependency-free, every commit). Stack safety verified under
`ulimit -s 512`: `(a+)+b` on 10 MB → correct `-47 MATCHLIMIT`; 200-deep nesting
over 10 MB matches (0.7 s).

**Gate outcome:** the ≤2.0 numeric criterion is **not** met; the residuals were
**closed by explicit user sign-off** after the floor-special step. The
residual is function-boundary / frame-churn cost per backtrack tick (we cross
`rmatch → dispatch → … → backtrack` re-deriving state, vs C's single function
with everything in registers). The measured next step — fusing dispatch and
backtrack into the merged `rmatch` — is a large structural rewrite that departs
from the C-mirroring design and was deliberately declined.

## 5. How zero-alloc is kept honest

Three permanent OUnit/Alcotest pins fail the build if allocation regresses:

| pin | asserts | measured |
|---|---|---|
| `alloc_per_attempt` | < 1,000 minor words for a ~100k-attempt exec | 205 |
| `alloc_per_exec_major` | < 2,000 major words / 1,000 execs | 0 |
| `alloc_per_exec_minor` | < 40 minor words / exec (engine seam) | 28 |
