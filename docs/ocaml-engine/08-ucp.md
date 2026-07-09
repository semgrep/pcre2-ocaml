# M7 — UCP, \p, \X, script runs

**Goal**: Unicode properties — `\p{...}`/`\P{...}` (+ single-letter forms), UCP-mode
`\d\w\s\b` semantics, caseless_sets (k/K/Å-style multi-char case classes), `\X` grapheme
clusters (extuni + grapheme-break properties), `(*script_run:)`/`(*sr:)` and
`(*atomic_script_run:)`.

**Gate G7** (verbatim from plan): testinput4+5 in-scope 100%; 8/9/10 in-scope sections.

## Chunks

- [ ] ucd.ml: GET_UCD stage-table lookup, script/chartype/gbprop/othercase/caseset
  accessors over generated ucd_tables.ml (pcre2_internal.h GET_UCD macros +
  pcre2_ucd.c:1-90 header layout)
- [x] parse: \p/\P — get_ucp name lookup incl. script extensions, Bidi_Class, bool props
  (pcre2_compile.c:2167-2340 + ucptables.ml binary search); compile OP_PROP/OP_NOTPROP +
  property XCLASS slots (compile_branch prop arms)
- [ ] match: OP_PROP/OP_NOTPROP + property repeat arms — PT_ANY/PT_LAMP/PT_GC/PT_PC/
  PT_SC/PT_SCX/PT_ALNUM/PT_SPACE/PT_PXSPACE/PT_WORD/PT_CLIST/PT_UCNC/PT_BIDICL/PT_BOOL
  (prop arms across pcre2_match.c:1934-5005)
- [ ] caseless_sets: tables.ml caseless_sets + ucd_caseless_sets wiring into charI/classI/
  backref-caseless paths (pcre2_tables.c caseless_sets + match arms)
- [ ] UCP-mode \d\w\s\b: OP_DIGIT/WORDCHAR/WHITESPACE et al. switch to property tests
  under PCRE2_UCP; \b word-boundary UCP arm (pcre2_compile.c handle_escdsw 2642-2770
  UCP branches + pcre2_match.c type arms)
- [ ] \X graphemes: extuni port (pcre2_extuni.c:1-150) — GB9/GB9a/GB11 rules, regional
  indicators; OP_EXTUNI + repeats (pcre2_match.c extuni arms)
- [ ] script_run.ml (pcre2_script_run.c:1-344) + OP_SCRIPT_RUN group wiring in
  compile/match

## Rough LOC estimate

~1,600 C LOC → ~1,400 OCaml (generated tables already landed in M0). 6–7 commits.
