# Test & performance report — pure-OCaml PCRE2 engine

Target: PCRE2 10.44. Engine == C oracle, both drivers (`Interp`, `Jit`).

## 1. PCRE2 conformance testdata (pass rate)

Byte-identical to the C oracle on every in-scope unit; failures = 0 on both
the oracle and engine drivers; skip sets diff-identical (1,531 rows).

| file | passed | failed | skipped | total | skip reason |
|---|---|---|---|---|---|
| testinput1 | 1290 | 0 | 0 | 1290 | — |
| testinput2 | 733 | 0 | 1110 | 1843 | 7 out-of-scope, 1 env, 1102 harness-modifier |
| testinput4 | 617 | 0 | 0 | 617 | — |
| testinput5 | 418 | 0 | 206 | 624 | 206 harness-modifier |
| testinput8 | 0 | 0 | 80 | 80 | 80 harness-modifier (all debug/bincode) |
| testinput9 | 10 | 0 | 18 | 28 | 18 harness-modifier |
| testinput10 | 44 | 0 | 117 | 161 | 117 harness-modifier |
| **total** | **3112** | **0** | **1531** | **4643** | frontier: none |

## 2. Extra tests added

| suite | count | notes |
|---|---|---|
| pure unit suite (OUnit→Alcotest) | 46 cases × 2 drivers | Interp + Jit |
| oracle-drift copy | same 46 vs C | detects engine/C contract drift |
| engine module-init asserts | 84 blocks / 17 modules | oracle-pinned; migrating to test files |
| allocation pins | 3 | see §5 |
| differential fuzzer | see §3 | grammar over full feature space |
| regression corpus | 8 repros | replayed every CI run |
| stack-safety stress | 2 cases | `ulimit -s 512`, 10 MB subjects |

## 3. Differential fuzz campaigns (engine vs C oracle)

| seed(s) | cases each | divergences | note |
|---|---|---|---|
| 42 | 1,000,000 | 0 | G8 gate |
| 42, 101, 20260708 | 200,000 | 0 | 3-seed |
| 7, 3117, 4243, 20260709 | 50,000 | 0 | per-chunk |
| per-commit smoke | 10,000 | 0 | — |

Rate ≈ 2,000–6,800 cases/sec.

## 4. Bugs found

| # | finding | kind | status |
|---|---|---|---|
| 1–4 | study-era divergences (nomatch-mark, recurse-loop, phantom partial, partial ovector) | engine | fixed (M9 study) |
| 5 | `was_newline` BACKCHAR reads before subject | **upstream C** (10.44+10.46) | pinned engine+oracle |
| 6 | 8-bit `GET_UCD` unbounded index | **upstream C** (10.44+10.46) | pinned engine+oracle |
| 7 | `OP_VREVERSE` BACKCHAR reads before subject | **upstream C** (10.44+10.46) | pinned engine+oracle |
| 8 | oracle-stub uninit `mark[-1]` (GPF) | dev harness | guarded |
| 9 | word-boundary prev-char `unsafe_get(-1)` | engine (§8) | pinned + divergence fixed |
| — | SKIP_ARG re-run Θ(limit³) stall | fuzzer assumption (not a bug; C does it too) | limit lowered |

Upstream-reportable C bugs: **3** (#5–7), report at `upstream-pcre2-bugs.md`.

## 5. Performance (engine / C-oracle interpreter, release profile, 5-rep medians)

### Campaign ladder (geomean)

| stage | geomean | note |
|---|---|---|
| start (`35c6fb6`) | 46.9 | dev profile |
| chunk 1 (closure-nest hoist) | 10.30 | dev profile |
| release-profile re-baseline | 8.57 | −opaque dropped |
| chunk 2 P0–P5 | 2.33 | arena reuse, barrier kill, scans, driver hoist, unsafe sweep |
| chunk 2 P6 (final, `b311b5a`) | **2.159** | floor specials |

Total: **46.9 → 2.159 = 21.7×**. Gate ≤2.0 (geomean + per-benchmark): geomean
missed by 0.159; closed by user sign-off on 8 residuals.

### Final per-benchmark (engine/oracle)

| benchmark | ratio | gate | benchmark | ratio | gate |
|---|---|---|---|---|---|
| http_lines | 1.55 | pass | uri | 2.43 | — |
| findall_utf | 1.76 | pass | pathological | 2.42 | — |
| utf_letters | 1.80 | pass | ipv4 | 2.30 | — |
| repeat_bounded | 1.85 | pass | keywords | 2.17 | — |
| repeat_negclass | 2.15 | — | email | 2.56 | — |
| findall_email | 2.54 | — | backref | 2.74 | — |

4/12 under gate; 8 signed-off residuals (2.15–2.74).

### Allocation (measured, pinned by OUnit tests)

| metric | before | after |
|---|---|---|
| minor words / exec (engine seam) | 123 | 28 |
| minor words / exec spanning ~100k attempts | ~6.3 M | 205 |
| major words / 1,000 execs | ~4.02 M | 0 |

### Stack safety (`ulimit -s 512`)

| case | subject | result |
|---|---|---|
| `(a+)+b` | 10 MB `a` | −47 MATCHLIMIT (correct) |
| 200-deep nesting | 10 MB tail match | correct match, 0.7 s |

## 6. Effort

79 commits ahead of `develop`; every engine chunk fidelity-reviewed; oracle
baseline never regressed.
