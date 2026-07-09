# Orchestrator log

Append-only. One row per orchestrator action (PORT chunk, FIX, PLAN reconcile, gate).
Never edit or delete prior rows; corrections get a new row referencing the old one.
Each row is appended in the SAME commit as the work it records (commit column = short SHA;
`pending` only transiently before the amend step of /commit).

Columns:

- **timestamp** — UTC, ISO 8601.
- **mode** — `PORT` | `FIX` | `PLAN` | `GATE` | `NOTE`.
- **chunk** — milestone-doc checklist item name, or the fix's frontier test / diagnosis.
- **frontier before → after** — `file:ordinal → file:ordinal` (`—` when not applicable,
  e.g. before the runner exists).
- **commit** — short SHA of the commit carrying the work.
- **notes** — regressions hit, deviations, out-of-scope findings, review rounds, risks.

| timestamp | mode | chunk | frontier before → after | commit | notes |
|---|---|---|---|---|---|
| 2026-07-06T18:15:00Z | NOTE | M0 start: vendored PCRE2 10.44 sources + testdata; porting infrastructure (.claude/ skills, agents, rules; docs/ocaml-engine/ plan docs) authored | — → — | 9b1ab09 | vendor chunk of 01-infrastructure.md complete; harness/runner being built concurrently; next: build restructure chunk toward gate G0 |
