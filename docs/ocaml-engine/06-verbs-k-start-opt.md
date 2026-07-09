# M5 — Verbs, \K, observable start-optimizations

**Goal**: backtracking verbs `(*MARK:name)/(*PRUNE)/(*SKIP)/(*THEN)/(*COMMIT)/(*ACCEPT)/
(*FAIL)` with and without arguments; `\K` (OP_SET_SOM) incl. error 199
(BACKSLASH_K_IN_LOOKAROUND); PARTIAL_SOFT/PARTIAL_HARD; **basic study (first code unit,
minlength) + NO_START_OPTIMIZE must land here** — start optimizations are observable via
COMMIT/MARK/partial matching; MARK output through the harness (`MK:` lines).

**Gate G5** (verbatim from plan): testinput1 100% minus skip-list; ≥85% of in-scope
testinput2 (non-UTF).

## Chunks

- [ ] parse: verb recognition incl. verb args and name-quoting rules, `(*LIMIT_MATCH=)/
  (*LIMIT_DEPTH=)/(*LIMIT_HEAP=)` pattern limits (pcre2_compile.c: parse_regex verb table +
  arms, ~2900-3100 within the main loop range)
- [ ] compile: OP_MARK/OP_PRUNE(_ARG)/OP_SKIP(_ARG)/OP_THEN(_ARG)/OP_COMMIT(_ARG)/
  OP_ACCEPT/OP_FAIL emission; \K → OP_SET_SOM + error 199 check
  (pcre2_compile.c: verb arms in compile_branch 6812-8370)
- [ ] match: verb opcodes + backtrack-verb semantics threaded through the frame unwind
  (mark propagation, skip-to-mark, THEN group scoping, COMMIT/ACCEPT cut points)
  (pcre2_match.c:6340-6475 + verb handling in the return dispatch 6479-6527 and
  pcre2_match driver)
- [ ] match: partial matching — PARTIAL_SOFT/HARD rules, start_used_offset tracking,
  `Partial of {start; mark}` surfaced via exec_full → harness `Partial match:` lines
  (pcre2_match.c: partial logic in driver 6530-7777 + OP_END arm)
- [ ] study (basic): first code unit (+caseless), last req code unit scaffold OFF by
  default, minlength walk; PCRE2_NO_START_OPTIMIZE plumbed so verbs/partial see identical
  observable behavior to C (pcre2_study.c: first-cu/minlength portions of 1-1915;
  pcre2_match.c driver start-optimization block)
- [ ] harness/skiplist: enable MARK output assertions and verb sections; drop `deferred m5`
  rows; re-baseline

## Rough LOC estimate

~1,500 C LOC → ~1,300 OCaml. 5–6 commits.

## Notes

- Start-optimization OBSERVABILITY is the trap: with COMMIT/MARK/partial, skipping
  no-match starts changes answers. Port the C driver's exact NO_START_OPTIMIZE gates.
- Full study (start bitmap, req-cu skipping) is M9 and must be behavior-neutral there.
