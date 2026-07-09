# M1 — Core compile + match loop

**Goal**: the engine compiles and matches the Perl-compatible core: literals, escapes,
`\Q..\E`, classes with ranges/POSIX names, anchors, `.`, greedy/lazy quantifiers,
alternation, capturing/non-capturing groups, name table; options i/m/s/x, UNGREEDY,
ANCHORED/ENDANCHORED, NOTBOL/NOTEOL/NOTEMPTY(_ATSTART), LITERAL, FIRSTLINE, DOLLAR_ENDONLY,
ALLOW_EMPTY_CLASS, NO_AUTO_CAPTURE; newline handling. ASCII paths only (UTF arms stubbed to
error until M6; UCP until M7).

**Gate G1** (verbatim from plan): OUnit green (Interp + Jit, pure); ~45–55% of testinput1.

## Chunks

Line ranges are 10.44 vendor estimates; pin exact boundaries when dispatching (§4 of
/plan-update).

Parse phase (`parse.ml` ← pcre2_compile.c front half):
- [x] parse helpers: (manage_callouts completed by parse_regex A) read_number/read_repeat_counts (pcre2_compile.c:1325-1520), read_name
  (2464-2560), check_posix_syntax/name (2377-2435), manage_callouts no-op shim (2595-2625)
- [x] check_escape (pcre2_compile.c:1551-2160) — ASCII escapes, \x/\o/\c, backslash-digit
  disambiguation (backref emission itself is M2)
- [x] parse_regex A — main loop skeleton, literals, \Q..\E, comments, inline option
  settings (?i)(?-i)(?^), newline conventions (*CR) etc. (pcre2_compile.c:2773-3800)
  (note: the (*CR)-style start-of-pattern items are owned by pcre2_compile()'s pso
  loop (pcre2_compile.c:10305-10381), i.e. the "pcre2_compile top-level" chunk below;
  parse_regex's share — IS_NEWLINE in # comment skipping — is ported with an
  NLTYPE_FIXED-only deferral to newline.ml)
- [x] parse_regex B — (48720f6) character classes incl. POSIX names, ranges, negation
  (pcre2_compile.c:3800-4400 + handle_escdsw 2642-2770)
- [x] parse_regex C — (this commit) quantifiers {n,m} +?*, groups (capturing/non/named), alternation,
  meta_extra_lengths bookkeeping (pcre2_compile.c:4400-5530; verb/lookaround/conditional
  arms parse-error or defer to M3–M5)

Compile phase (`compile.ml` ← pcre2_compile.c back half):
- [x] compile utilities: (this commit) PUT/GET/PUT2/GET2, code-size first pass plumbing, find_dupname
  scaffold (pcre2_compile.c:5538-5630 + macros pcre2_internal.h)
- [x] compile_branch A — (this commit) function head + two-pass lengthptr protocol,
  branch terminators, ^ $ ., META_OPTIONS, numerical callouts, simple escapes,
  literal chars OP_CHAR/CHARI (pcre2_compile.c:5604-5857, 6576-6587, 7101-7111,
  8014-8019, 8101-8206, 8209-8341; OP_NOT/NOTI come from the negated one-char
  class optimization at 5917-5947, i.e. chunk B, not this chunk; \P\p and the
  Unicode caseless-literal/ord2utf paths defer loudly to M6/M7)
- [x] compile_branch B — (this commit) class compilation → OP_CLASS/OP_NCLASS bitmaps
  (pcre2_compile.c:5860-6484 per A's corrected layout, NOT the 6500-6810 estimate;
  + add_to_class family 5206-5530, posix_class_maps 709-740, SETBIT 377-386;
  new ucd.ml for GET_UCD/UCD_CASESET/UCD_OTHERCASE, pcre2_internal.h:1864-1889.
  OP_XCLASS extra data, \p/\P in classes, and the UTF/UCP caseless closure
  defer loudly with err 299 to M6/M7, identically in both passes)
- [x] compile_branch C — (this commit) repeats: OP_STAR..OP_MINUPTO families, EXACT,
  repeated classes CRSTAR..CRMINRANGE, type repeats TYPESTAR..TYPEEXACT, the
  possessive pass (opcode_possessify / ONCE wrap)
  (pcre2_compile.c:7178-8011 — the 6812-8050 estimate was wrong, now pinned;
  + chartypeoffset 687-691, opcode_possessify 861-917. The repeated-bracket
  region 7424-7751 and repeated-recursion 7354-7422 defer loudly with err 299:
  no bracket/OP_RECURSE previous item exists until chunk D / M5)
- [ ] compile_branch D — group emission OP_BRA/OP_CBRA, name table entries
  (pcre2_compile.c:8050-8370, minus backref/recursion arms → M2/M4)
- [ ] compile_regex + branch linking, OP_ALT/OP_KET chains, first/req cu seed
  (pcre2_compile.c:8378-9390)
- [ ] pcre2_compile top-level: two-pass driver, workspace, error offset tracking,
  anchoring flags, name-table finalization (pcre2_compile.c:10126-11001)

Match phase (`interpreter.ml`, `frames.ml`, `newline.ml`):
- [ ] newline.ml (pcre2_newline.c:1-243)
- [ ] frames.ml — frame layout/arena alloc/grow/copy + heap accounting
  (pcre2_match.c:100-830)
- [ ] interpreter dispatch skeleton + OP_END/OP_ACCEPT/match-oveector copy + backtrack
  return dispatch (pcre2_match.c:837-990, 6479-6527)
- [ ] chars + char repeats: OP_CHAR/CHARI/NOT/NOTI + their STAR/PLUS/QUERY/UPTO arms
  (pcre2_match.c:995-1930)
- [ ] classes + typed repeats: OP_CLASS/OP_NCLASS, OP_ANY/ALLANY, \d\w\s type ops and
  repeat loops (ASCII arms; pcre2_match.c:1934-5005, UTF/UCP/extuni arms stubbed)
- [ ] anchors & simple assertions: OP_CIRC(M)/OP_DOLL(M)/OP_SOD/SOM/EOD/EODN/
  \b/\B/\A/\z/\Z (within pcre2_match.c:837-995 + scattered cases)
- [ ] brackets: OP_BRA/OP_CBRA/OP_SBRA/SCBRA, OP_ALT, OP_KET/KETRMIN/KETRMAX, BRAZERO/
  BRAMINZERO (pcre2_match.c:5224-5265, 5906-6335)
- [ ] pcre2_match driver: arg validation, BADOFFSET, option masking (−34), anchored/
  startline logic, start-of-match bump loop, NOTEMPTY/FIRSTLINE handling
  (pcre2_match.c:6530-7777, JIT/start-optimization branches reduced per M5/M9 notes)
- [ ] engine.ml: wire compile/exec through the real pipeline; debug_printer.ml first cut
  (pcre2_printint.c) for differential debugging

## Rough LOC estimate

~6,500 C LOC in scope → ~5,500 OCaml. Largest milestone; expect ~18–22 chunks after
re-chunking.
