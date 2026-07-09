# Testdata order — curated frontier ordering

Human-readable mirror of the machine file `test/conformance/testdata-order`, which is
pinned during M0 (gate G0) and consumed by `runner.exe --frontier`. If the two disagree,
the machine file wins; update this doc via /plan-update.

## Policy

The frontier is NOT raw file order. It is a curated sequence of **feature-tagged sections**
of the vendored testinput files, ordered so that the first failing test always points at
the next milestone's work (simple → complex, ASCII → UTF → UCP):

1. **testinput1** (Perl-compatible, main functionality) — sections tagged by feature:
   literals/classes/quantifiers/groups (M1) → backrefs (M2) → lookaround/atomic/possessive
   (M3) → conditionals/recursion (M4) → verbs/\K/partial (M5).
2. **testinput2** (PCRE2-specific API/options, non-UTF) — heavy skip-list; sections
   interleaved into the milestones that implement them (options, newline conventions,
   recursion oddities, verbs).
3. **testinput4 / testinput5** (UTF-8 + Unicode properties) — testinput4 non-\p sections
   at M6, \p/\X/script-run sections and testinput5 at M7.
4. **testinput8 / testinput9 / testinput10** (8-bit specific behaviors, DFA-excluded
   patterns, UTF-8 specials incl. the UTF8_ERR sections) — in-scope sections at M6–M8.

Within a section, tests keep the vendored file's ordinal order. Ordinals are 1-based test
indices within a testinput file, as counted by the harness.

## File-level skips (with reasons)

| file | reason |
|---|---|
| testinput3 | locale-specific (system locales; out of scope) |
| testinput6, testinput7 | DFA matching (`pcre2_dfa_match` — type-only, never implemented) |
| testinput11–13 | 16/32-bit code-unit libraries (we port the 8-bit library only) |
| testinput16, testinput17 | JIT-specific (pure engine aliases JIT to interp) |
| testinput18, testinput19 | POSIX wrapper API (not exposed by these bindings) |
| testinput20 | serialization (`pcre2_serialize_*` — not exposed) |
| testinput22 | substitute (`pcre2_substitute` — type-only, out of scope) |

Also unused: testinput14/15 (special modes not exposed), testinput21 (\C exclusions
covered inline), testinput23–26, EBC/heap variants — not in the curated order; add via
/plan-update only with a reason.

## Notes

- Section boundaries and their milestone tags live in the machine file as
  `(section <file> <start-ordinal> <end-ordinal> <feature-tag> <milestone>)` entries.
- `--frontier` walks this order, skipping skiplist entries, and prints the first failure
  plus per-file tallies.
- Fuzz-minimized regressions in `fuzz/corpus/regressions/` are appended to the very front
  of the order (a past mismatch must never re-break silently).
