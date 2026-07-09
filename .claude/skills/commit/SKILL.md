---
name: commit
description: Use when committing a verified+reviewed chunk — enforces the [ocaml-engine] convention, baseline update, and same-commit log entry; refuses on regression.
---

# Commit a chunk

Precondition: /verify reported PASS and /review reported PASS on the current working tree.

## 1. Refuse on regression
If the most recent /verify run reported ANY previously-passing test now failing, or was not
run on the current tree state: **REFUSE**, print why, and hand back to the orchestrator.
Re-run /verify if in doubt — never commit blind.

## 2. Update baselines (same commit)
If conformance pass counts changed, regenerate `test/conformance/baseline_counts.sexp`
(the runner has a flag for this; otherwise edit the counts to the verified values):
```
nix develop -c dune exec test/conformance/runner.exe -- --write-baseline
git diff test/conformance/baseline_counts.sexp   # sanity: counts only go UP
```
Any count going DOWN is a regression → go back to step 1's refusal.

## 3. Append the log entry (same commit)
Append one row to `docs/ocaml-engine/ORCHESTRATOR_LOG.md` (format documented in its header):
```
| <UTC ISO timestamp> | PORT|FIX | <chunk name> | <file:ord> → <file:ord> | pending | <notes> |
```

## 4. Flip the milestone checkbox (PORT mode, same commit)
Mark the chunk `- [x]` in the current `docs/ocaml-engine/NN-*.md`.

## 5. Commit
Format per `.claude/rules/commit-conventions.md`:
```
git add -A
git commit -m "[ocaml-engine] <area>: <summary>" -m "<body>"
```
Body must list, in order:
1. C refs: `pcre2_match.c:5011-5220, pcre2_internal.h:...`
2. Tests newly passing: `testinput1:642-687`
3. Frontier: `testinput1:642 -> testinput1:731`
4. Deviations (if any)

Include the trailer:
```
Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>
```

## 6. Backfill the SHA
Replace `pending` in the new log row with the short SHA and amend:
```
git log -1 --format=%h
# edit docs/ocaml-engine/ORCHESTRATOR_LOG.md: pending -> <sha>
git add docs/ocaml-engine/ORCHESTRATOR_LOG.md && git commit --amend --no-edit
```

## Rules
- One chunk per commit; no unrelated files (check `git status` before `add -A`).
- Never commit on `develop`; never push unless the user asked.
- baseline_counts.sexp, log row, and checkbox flip travel IN the chunk's commit — a commit
  that changes engine code without them is malformed.
