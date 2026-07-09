# M7 — UCP, \p, \X, script runs

**Goal**: Unicode properties — `\p{...}`/`\P{...}` (+ single-letter forms), UCP-mode
`\d\w\s\b` semantics, caseless_sets (k/K/Å-style multi-char case classes), `\X` grapheme
clusters (extuni + grapheme-break properties), `(*script_run:)`/`(*sr:)` and
`(*atomic_script_run:)`.

**Gate G7** (verbatim from plan): testinput4+5 in-scope 100%; 8/9/10 in-scope sections.

**GATE MET 2026-07-10** — testinput4 617/617 (100%); testinput5 417/419 (the two
residuals are callout-owned units, not UCP: t5:419 expects a callout-string parse
error, t5:510 needs callout trace output — both ride the callout chunk);
testinput8/9/10 in-scope sections all pass. UCP feature surface complete.

## Chunks

- [x] ucd.ml: GET_UCD stage-table lookup, script/chartype/gbprop/othercase/caseset
  accessors over generated ucd_tables.ml (pcre2_internal.h GET_UCD macros +
  pcre2_ucd.c:1-90 header layout) — record_index/chartype/caseset/othercase landed
  with M1 classes; script/gbprop/bidiclass/scriptx/bprops + MAPBIT set membership
  landed with the property-match chunk
- [x] parse: \p/\P — get_ucp name lookup incl. script extensions, Bidi_Class, bool props
  (pcre2_compile.c:2167-2340 + ucptables.ml binary search); compile OP_PROP/OP_NOTPROP +
  property XCLASS slots (compile_branch prop arms)
- [x] match: OP_PROP/OP_NOTPROP + property repeat arms — PT_ANY/PT_LAMP/PT_GC/PT_PC/
  PT_SC/PT_SCX/PT_ALNUM/PT_SPACE/PT_PXSPACE/PT_WORD/PT_CLIST/PT_UCNC/PT_BIDICL/PT_BOOL
  (prop arms across pcre2_match.c:1934-5005) — incl. the XCL_PROP/XCL_NOTPROP switch
  (pcre2_xclass.c:133-299, + the PX* POSIX forms)
- [x] caseless_sets: tables.ml caseless_sets + ucd_caseless_sets wiring into charI/classI/
  backref-caseless paths (pcre2_tables.c caseless_sets + match arms) — the compile-side
  caseset expansion (CHARI→PT_CLIST, class additions) and the PT_CLIST/backref match
  arms landed with earlier M7 chunks; the ucp-without-UTF caseless singles
  (pcre2_match.c:1075-1092, 1139-1160, 1386-1389) landed with the \X/script-run chunk
- [x] UCP-mode \d\w\s\b: OP_DIGIT/WORDCHAR/WHITESPACE et al. switch to property tests
  under PCRE2_UCP; \b word-boundary UCP arm (pcre2_compile.c handle_escdsw 2642-2770
  UCP branches + pcre2_match.c type arms) — handle_escdsw UCP rewrite shipped with the
  M1 parse chunk (the rewritten escapes compile to OP_PROP, live above); the
  OP_(NOT_)UCP_WORD_BOUNDARY match arm landed with the property-match chunk
- [x] \X graphemes: extuni port (pcre2_extuni.c:1-150) — GB9/GB9a/GB11 rules, regional
  indicators; OP_EXTUNI + repeats (pcre2_match.c extuni arms)
- [x] script_run.ml (pcre2_script_run.c:1-344) + OP_SCRIPT_RUN group wiring in
  compile/match

## Rough LOC estimate

~1,600 C LOC → ~1,400 OCaml (generated tables already landed in M0). 6–7 commits.
