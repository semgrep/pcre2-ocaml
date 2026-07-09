# Skip-list policy

Human-readable mirror of `test/conformance/skiplist.sexp`. The machine file wins; keep this
doc in sync via /plan-update.

## Entry shape

```
(file ordinal-range category reason [revisit-milestone])
```

- `file` — testinput file name, e.g. `testinput2`.
- `ordinal-range` — single ordinal `184` or inclusive range `184-191`.
- `category` — one of:
  - `out-of-scope` — feature this project will never implement (DFA, substitute, callouts,
    compile contexts, serialization, locale). Permanent; requires a reason naming the
    feature.
  - `env` — depends on environment we don't control (system locale availability, memory
    limits that vary by machine). Permanent-ish; reason must name the dependency.
  - `deferred` — in-scope but not yet ported. MUST carry `revisit-milestone` (e.g. `m6`).
- `reason` — free-text string; mandatory for every entry.
- `revisit-milestone` — mandatory for `deferred`, forbidden otherwise.

Example entries:

```
(testinput2 500-512 out-of-scope "callouts are type-only in these bindings")
(testinput4 88-104 deferred "\\p{} properties" m7)
(testinput2 733 env "depends on heap limit varying with allocator")
```

## Rules (enforced by runner.exe — violations FAIL the conformance stage)

1. **Staleness**: a skipped test that PASSES is an error. Remove the entry in the same
   commit that made it pass (the /port and /commit skills handle this).
2. **Deferred expiry**: a `deferred` entry whose `revisit-milestone` is the current
   milestone or earlier is an error — port it or re-justify (category change needs a log
   note).
3. **Counts always reported**: every runner invocation prints skip counts by category next
   to pass counts. Skips are never silent.
4. **Freeze at M8**: after gate G8, no `deferred` entries may exist; the list is frozen and
   any change requires a `[ocaml-engine] skiplist:` commit with justification.

## Current counts

Maintained by /plan-update at each gate; initial list is authored during M0 (runner chunk)
from the file-level skips in `testdata-order.md` plus per-section deferrals for M1+
features.
