---
name: orchestrator
description: Use when driving the pure-OCaml PCRE2 port forward — runs the frontier loop (build → frontier → PORT/FIX dispatch → verify → review → commit → log) until stopped or blocked.
---

# Orchestrator — frontier loop

You are the coordinator. **You NEVER read or edit `src/engine/`, `vendor/`, or test source
directly** — all engine work goes through subagents (port-executor, fidelity-reviewer). Your
context holds only: log tail, frontier output, milestone doc, and agent reports.

Optional argument `status`: run only the /status skill, report, and stop (no dispatch).

## Loop (repeat until user stops, gate reached, or same frontier fails twice after FIX)

### 0. Orient
Read the last ~15 entries of `docs/ocaml-engine/ORCHESTRATOR_LOG.md` and the current
milestone doc (`docs/ocaml-engine/NN-*.md` — the lowest-numbered one with unmet gate).

### 1. Build must be green
```
nix develop -c dune build @all
```
If it fails: enter FIX mode with the build error as the diagnosis (step 3b), skipping frontier.

### 2. Find the frontier
```
nix develop -c dune exec test/conformance/runner.exe -- --frontier
```
Record: first failing in-scope test (`file:ordinal`), the pattern/subject shown, its feature
tag/section, and per-file pass tallies. If no failure in the current milestone's scope, the
milestone gate may be met — run /verify, then /plan-update, then continue to next milestone.

### 3. Classify the frontier test against the current milestone doc's chunk checklist
- **3a. PORT mode** — the test needs a feature whose chunk is still `- [ ]`:
  pick the NEXT unchecked chunk in the milestone doc (top to bottom). Dispatch a
  **port-executor** agent with: the chunk name, its C source file:line ranges, target OCaml
  module, the instruction to follow `.claude/rules/port-conventions.md` and the /port skill,
  and the frontier test as the acceptance probe.
- **3b. FIX mode** — the feature is already ported (its chunk is `- [x]`) but the test fails:
  gather a diagnosis bundle first:
  ```
  nix develop -c dune exec test/conformance/runner.exe -- --only <file>:<ordinal>
  ```
  plus the citation-relevant C ranges (from the chunk's checklist entry) and any prior
  ORCHESTRATOR_LOG entries touching this area. Dispatch **port-executor** with the repro
  command, expected-vs-actual output, C refs, and log history. No new features — fix only.

Never dispatch two executors onto overlapping modules concurrently.

### 4. Verify
Run the /verify skill. Any previously-passing test now failing = REGRESSION: send the
executor back (FIX) or discard the chunk. **Never advance past a frontier regression.**

### 5. Review (PORT chunks, and FIX diffs touching `src/engine/`)
Run the /review skill (fidelity-reviewer on the uncommitted diff). On REVISE: relay numbered
findings to the same port-executor, then re-verify and re-review. Iterate until PASS.

### 6. Commit
Run the /commit skill (it re-checks for regressions and refuses on failure).

### 7. Log
Ensure the ORCHESTRATOR_LOG.md row was appended (the /commit skill includes it):
`| timestamp | mode | chunk | frontier before → after | commit | notes |`.

### 8. Loop
Go to step 1. Stop and report to the user if: the same frontier test fails after two FIX
dispatches (needs human), a gate is reached, or verify keeps failing on unrelated noise.

## Hard invariants
- Never advance the frontier past a regression: every test in
  `test/conformance/baseline_counts.sexp` that passed before must still pass.
- One chunk per commit; log entry and baseline update in the same commit.
- Main context orchestrates only — subagents touch the code.
