# M3 — Lookaround, atomic groups, possessive quantifiers

**Goal**: all four lookarounds `(?=)(?!)(?<=)(?<!)` (+ `(*positive_lookahead:)` alpha
forms), atomic groups `(?>)` / `(*atomic:)`, possessive quantifiers (`++`, `*+`, `?+`,
`{n,m}+` → OP_POS* and OP_BRAPOS families), lookbehind length computation with exact
errors 125 (unbounded), 135 (too long branch), 187 (\K in lookaround assertion handled M5).

**Gate G3** (verbatim from plan): cumulative ~80% of testinput1.

## Chunks

- [ ] parse: lookaround/atomic group openers incl. alpha assertions `(*naming:)`,
  `(?<=`/`(?<!` detection, has_lookbehind flag (pcre2_compile.c: parse_regex group-opener
  arms within 4400-5530)
- [ ] compile: OP_ASSERT/ASSERT_NOT/ASSERTBACK/ASSERTBACK_NOT + OP_ONCE emission; assertion
  KET wiring (pcre2_compile.c: compile_branch/compile_regex assertion arms 6812-8370)
- [ ] compile: possessive quantifiers — OP_POSSTAR..OP_POSUPTO, OP_BRAPOS/SBRAPOS/CBRAPOS/
  SCBRAPOS + OP_KETRPOS emission (pcre2_compile.c: repeat arms within 6812-8050)
- [ ] compile: lookbehind lengths — get_branchlength / set_lookbehind_lengths with errors
  125/135, fixed-length metadata (pcre2_compile.c:9398-10120)
- [ ] match: OP_ASSERT*/OP_ASSERTBACK* frames — assertion frame type, eptr rewind for
  lookbehind, condition-assertion interplay stubs for M4 (pcre2_match.c:5511-5900)
- [ ] match: OP_ONCE + atomic backtrack cut; OP_BRAPOS/KETRPOS re-match loop; BRAPOSZERO
  (pcre2_match.c:5260-5425 + ket arms in 5906-6335); possessive char/type repeats
  (POS* arms inside 995-5005 that were stubbed in M1)

## Rough LOC estimate

~1,900 C LOC → ~1,600 OCaml. 5–6 commits.
