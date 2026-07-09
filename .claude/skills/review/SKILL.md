---
name: review
description: Use when a ported chunk needs fidelity review before commit — launches the fidelity-reviewer agent on the uncommitted diff and iterates executor↔reviewer until PASS.
---

# Review a ported chunk

## 1. Scope the diff
```
git status --short
git diff --stat
```
Review targets: everything uncommitted under `src/engine/`, `src/bindings.ml`, and test
enables. Pure test/docs diffs with no engine changes may skip review (note that decision).

## 2. Launch the fidelity-reviewer agent
Dispatch `fidelity-reviewer` with:
- The list of changed files and the chunk's C citation ranges (from the executor's report).
- Instruction to read the vendored C and the OCaml side-by-side.

The reviewer's checklist (it applies `.claude/rules/port-conventions.md` in full):
- **Unsigned arithmetic**: every C unsigned op translated with explicit `land 0xff` /
  `land 0xFFFF_FFFF` / `lsr` where wraparound or unsigned compare matters.
- **goto/fallthrough**: control flow restructured faithfully — no dropped fallthrough
  tails, no reordered case guards, `(* fallthrough from OP_X *)` comments present.
- **Error parity**: error CODE and MESSAGE (and `erroroffset`) match `pcre2_error.c` /
  the C call sites exactly.
- **LINK_SIZE offsets**: every `get2`/`put2` and `+1`/`+LINK_SIZE` adjustment matches the
  C line it cites (off-by-one hunting).
- **[@tailcall] discipline**: dispatch/scan self-calls annotated; no non-tail recursion on
  subject-proportional data.
- **Hot loop allocation**: no closures/tuples/options/records allocated inside the frame
  loop; `unsafe_get` only under a bounds-proof comment.
- **Citations**: every ported block has an accurate `(* file.c:A-B *)` comment; DEVIATION
  comments match the executor's reported deviations.

## 3. Act on the verdict
- **PASS** → proceed to /commit.
- **REVISE** → forward the numbered findings verbatim to the SAME port-executor agent
  (SendMessage if still alive, else a new dispatch with findings + chunk context). After it
  revises: re-run /verify, then re-launch the reviewer on the new diff.
- Iterate until PASS. If 3 rounds don't converge, stop and escalate to the user with the
  outstanding findings — do not commit a REVISE state.

## Notes
- The reviewer NEVER edits files; if it did, discard its changes (`git checkout -- <file>`)
  and re-dispatch.
- Findings format expected: `N. [CATEGORY] file:line — issue; Expected: ...; Found: ...`
  with CATEGORY ∈ FIDELITY | CONVENTION | ERROR-HANDLING | PERF-HOTLOOP.
