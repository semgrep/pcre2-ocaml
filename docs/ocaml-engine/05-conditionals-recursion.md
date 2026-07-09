# M4 — Conditionals & recursion

**Goal**: conditional groups `(?(1))`, `(?(<name>))`, `(?('name'))`, `(?(R))`, `(?(Rn))`,
`(?(R&name))`, `(?(DEFINE))`, `(?(VERSION[>]=x))`, assertion conditions; recursion
`(?R)`, `(?1)`, `(?-1)`, `(?+1)`, `(?&name)`, `(?P>name)`, `\g<name>` call syntax;
recursion frames with capture save/restore; RECURSELOOP (−52) detection +
DISABLE_RECURSELOOP_CHECK.

**Gate G4** (verbatim from plan): testinput1 conditional/recursion ranges + testinput2
recursion sections.

## Chunks

- [ ] parse: `(?(...)` condition forms → META_COND_* (NUMBER/NAME/RNUMBER/DEFINE/VERSION/
  assertion), `(?n)/(?&name)/(?+n)` → META_RECURSE (pcre2_compile.c: parse_regex condition
  + recursion arms within 4400-5530)
- [ ] compile: OP_COND/OP_SCOND emission — group-number and name conditions (OP_CREF/
  OP_DNCREF), OP_RREF/OP_DNRREF, OP_FALSE/OP_TRUE for DEFINE/VERSION, assertion-condition
  wiring (pcre2_compile.c: compile_branch conditional arms within 6812-8370)
- [ ] compile: OP_RECURSE emission + forward-recursion fixups, must-be-closed checks,
  ERR15/ERR29/ERR40 paths (pcre2_compile.c: recursion arms 8050-8370 + driver fixup pass
  in 10126-11001)
- [ ] match: OP_COND/OP_SCOND — condition evaluation incl. assertion conditions
  (pcre2_match.c: OP_COND arms within 5511-5900)
- [ ] match: OP_RECURSE — recursion frame push/pop, ovector save/restore semantics, capture
  visibility rules, RECURSELOOP −52 + DISABLE_RECURSELOOP_CHECK option
  (pcre2_match.c:5427-5505 + recursion KET arms in 5906-6335)

## Rough LOC estimate

~1,300 C LOC → ~1,100 OCaml. 4–5 commits.

## Notes

- Recursion uses the same frame arena — no OCaml-native recursion (stack-safety rules §6).
- DEPTHLIMIT interplay with recursion frames must tick at C's sites (limits parity).
