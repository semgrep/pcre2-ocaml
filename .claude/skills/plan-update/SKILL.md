---
name: plan-update
description: Use when a milestone gate is reached (or a milestone doc has drifted from reality) — reconciles the chunk checklist with git history and conformance counts, re-chunks remaining work, marks the gate.
---

# Plan update (milestone boundary)

Run at every milestone gate, and whenever the orchestrator notices the milestone doc no
longer matches reality (chunks done but unchecked, chunks that grew, etc.).

## 1. Gather ground truth
```
git log --oneline --grep='\[ocaml-engine\]' <last-gate-sha>..HEAD
nix develop -c dune exec test/conformance/runner.exe
```
Plus the ORCHESTRATOR_LOG.md rows since the last gate.

## 2. Reconcile the milestone doc (`docs/ocaml-engine/NN-*.md`)
- Every committed chunk → checkbox `- [x]`, appending the short SHA:
  `- [x] interp: backrefs (pcre2_match.c:5011-5220) — a1b2c3d`.
- Chunks done differently than planned (split, merged, different line ranges) → rewrite the
  entry to what actually happened; keep the plan honest, not aspirational.
- Work discovered but not planned → add as new checked/unchecked items.

## 3. Check the gate
Compare the doc's **Gate** section (copied verbatim from the approved plan) against the
runner output / OUnit / fuzz / bench evidence. If met, mark it:
```
**Gate GN: MET <YYYY-MM-DD> at <short-sha>** — <one-line evidence summary>
```
If NOT met, list exactly what's missing as unchecked items and do not mark it.

## 4. Re-chunk the next milestone
Open the next `NN-*.md`:
- Split any chunk now known to exceed ~600 C LOC; merge trivial ones below ~150.
- Correct C line ranges against `vendor/pcre2/src/` (line numbers in early docs are
  estimates — pin them now by reading the actual function boundaries).
- Update the milestone's rough LOC estimate.
- Reorder chunks so the frontier's curated order (docs/ocaml-engine/testdata-order.md) hits
  them in sequence — the next frontier failure should always map to the top unchecked chunk.

## 5. Skip-list pass
Reconcile `test/conformance/skiplist.sexp` with `docs/ocaml-engine/skip-list.md`:
- `deferred` entries for the milestone just completed must be gone (or explicitly moved,
  with a note in the log).
- Update category counts in skip-list.md if it carries them.

## 6. Commit
One commit per `.claude/rules/commit-conventions.md`:
```
[ocaml-engine] docs: reconcile M<k> plan at gate G<k>
```
with an ORCHESTRATOR_LOG.md row (`mode` = `PLAN`).
