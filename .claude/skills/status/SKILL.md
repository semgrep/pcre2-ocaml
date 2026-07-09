---
name: status
description: Use when the user asks where the port stands — reports current milestone, frontier, per-file pass counts, skip-list size by category, and open risks. Read-only.
---

# Status report

Read-only: no builds of new code, no dispatches, no edits.

## 1. Current milestone
Scan `docs/ocaml-engine/01-*.md` through `10-*.md` in order; the current milestone is the
first whose gate section is not marked met. Count its checklist: `- [x]` done vs `- [ ]`
remaining (grep for `^- \[`).

## 2. Frontier position
```
nix develop -c dune exec test/conformance/runner.exe -- --frontier
```
If the build is broken, say so and use the last frontier recorded in ORCHESTRATOR_LOG.md
instead (flag it as stale).

## 3. Per-file pass counts
```
nix develop -c dune exec test/conformance/runner.exe
```
Report `passed/in-scope` per testdata file (1, 2, 4, 5, 8, 9, 10) and the delta vs
`test/conformance/baseline_counts.sexp` (should be zero between commits).

## 4. Skip-list size by category
Count entries in `test/conformance/skiplist.sexp` grouped by category
(`out-of-scope` / `env` / `deferred`), and list `deferred` entries whose revisit-milestone
is the current or an earlier milestone (these are overdue).

## 5. Open risks
Read the last ~20 rows of `docs/ocaml-engine/ORCHESTRATOR_LOG.md`; surface notes mentioning
regressions, repeated FIX attempts on the same frontier, new fuzz repros, deferred cleanups,
or DEVIATION entries awaiting follow-up.

## Output format
```
milestone: M<k> <name> — <done>/<total> chunks
frontier:  <file:ordinal> (<feature tag>)  [or: gate candidate — no in-scope failures]
counts:    testinput1 a/b | testinput2 c/d | testinput4 ... (Δ vs baseline: 0)
skiplist:  N total (out-of-scope X, env Y, deferred Z; overdue: ...)
risks:     - <bullet per open risk from log tail>
```
