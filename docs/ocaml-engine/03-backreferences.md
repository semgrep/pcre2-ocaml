# M2 — Backreferences & names

**Goal**: `\1..\g{n}`, relative refs `\g{-1}`/`\g{+1}`, forward refs, `\k<name>`/
`\k'name'`/`(?P=name)`, DUPNAMES semantics, caseless backrefs, MATCH_UNSET_BACKREF.

**Gate G2** (verbatim from plan): testinput1 backref ranges.

**GATE MET 2026-07-08** — zero in-scope backref failures remain (full --failures scan:
every remaining unit with \1..\9/\g/\k/(?P= needs M4 lookaround/atomics, M5
conditionals/recursion, or M6 UTF). Engine: 1165 units (t1 735/1290 = 57%, t2 405,
t5 6). +117 backref units across the two M2 chunks.

## Chunks

- [x] parse: (shipped with M1 parse chunks; see 02 doc) backref escapes — backslash-digit vs octal disambiguation finalized,
  \g{n}/\g{-n}/\g<>/\k<name> forms → META_BACKREF/META_BACKREF_BYNAME
  (pcre2_compile.c: check_escape backref arms 1551-2160 + parse_regex \g/\k handling)
- [x] compile: OP_REF/OP_REFI/OP_DNREF/OP_DNREFI emission, forward-reference fixups,
  duplicate-name group lists, DUPNAMES option + error paths (ERR15 nonexistent ref etc.)
  (pcre2_compile.c:8052-8170 + name-table duplicate handling in the 10126-11001 driver)
- [x] match: match_ref helper — caseful/caseless compare incl. fcc folding
  (pcre2_match.c:345-485)
- [x] match: OP_REF/OP_REFI/OP_DNREF/OP_DNREFI + backref repeat loops, PCRE2_UNSET refs
  empty-match rule + MATCH_UNSET_BACKREF option (pcre2_match.c:5011-5220)

## Rough LOC estimate

~700 C LOC → ~600 OCaml. 3–4 commits.

## Notes

- Caseless backref UTF paths land in M6; here only the fcc (ASCII) fold is live.
- Frontier coverage: testinput1 backref sections; enable any `deferred m2` skiplist rows.
