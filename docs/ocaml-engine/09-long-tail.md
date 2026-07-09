# M8 — Long tail & skip-list freeze

**Goal**: everything remaining in scope — testinput2 oddities, the alternative-syntax
options, offset-limit errors — then freeze the skip-list and prove the whole in-scope
surface with a large fuzz run.

**Gate G8** (verbatim from plan): 100% of in-scope tests across 1,2,4,5,8,9,10; fuzzer
(full grammar) 1M cases, zero mismatches.

## Chunks

- [ ] options long tail: ALT_BSUX (+EXTRA_ALT_BSUX \u{}), ALT_CIRCUMFLEX, ALT_VERBNAMES,
  EXTENDED_MORE (xx) — parse/compile behavior switches
  (pcre2_compile.c: option-conditioned arms across check_escape/parse_regex)
- [ ] offset-limit: PCRE2_ERROR_OFFSET_LIMIT handling (−56) + USE_OFFSET_LIMIT flag
  semantics in the match driver (pcre2_match.c driver arms in 6530-7777)
- [ ] testinput2 oddities sweep A: frontier-driven FIX passes over remaining in-scope
  testinput2 sections (each fix cites its C range; expect newline/option/anchor corner
  cases) — split into concrete chunks at dispatch time via /plan-update
- [ ] testinput2 oddities sweep B: second sweep for 8/9/10 residuals after M6/M7
- [ ] skip-list freeze: every remaining skip is `out-of-scope` or `env` with a reason; no
  `deferred` rows survive; update docs/ocaml-engine/skip-list.md counts; runner enforces
- [ ] fuzz campaign: full-grammar mask, 1,000,000 cases vs oracle, zero mismatches; all
  minimized repros triaged into conformance regressions and fixed

## Rough LOC estimate

~600 C LOC of new port + fix churn. Chunk count unknowable up front — re-chunk as the
frontier reveals it.
