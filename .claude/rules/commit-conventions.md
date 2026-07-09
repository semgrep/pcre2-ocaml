# Commit conventions — ocaml-engine port

## Format

```
[ocaml-engine] <area>: <summary>
```

- `<area>` = engine module or work area: `parse`, `compile`, `interp`, `frames`, `utf`,
  `ucd`, `study`, `harness`, `runner`, `fuzz`, `bench`, `build`, `docs`, `vendor`, `skiplist`.
- `<summary>` = imperative, ≤ 60 chars. Example:
  `[ocaml-engine] interp: port backreference opcodes OP_REF..OP_DNREFI`

## Body (required for PORT/FIX commits)

Must list, in this order:

1. **C refs** — every vendored range ported/consulted, e.g. `pcre2_match.c:5011-5220`.
2. **Tests newly passing** — conformance ordinals, e.g. `testinput1:642-687, testinput2:14`.
3. **Frontier** — `frontier: testinput1:642 -> testinput1:731` (before → after).
4. Deviations from the C, if any (mirror the `(* DEVIATION *)` comments).

## Rules

- **One chunk per commit.** A commit maps to exactly one milestone-doc checklist item (PORT)
  or one diagnosed frontier failure (FIX). No drive-by refactors.
- **Baseline in the same commit.** If conformance pass counts changed,
  `test/conformance/baseline_counts.sexp` is updated in the SAME commit — never separately.
- **Log entry in the same commit.** Append the corresponding row to
  `docs/ocaml-engine/ORCHESTRATOR_LOG.md` in the SAME commit (commit column = `pending`,
  or amend with the short SHA after committing).
- **No regressions.** Committing when /verify reported any previously-passing test now
  failing is forbidden — fix first or revert the chunk.
- Milestone-doc checkbox flips (`- [ ]` → `- [x]`) ride in the same commit as the chunk.
- Never commit on `develop` directly; work stays on the port branch.
