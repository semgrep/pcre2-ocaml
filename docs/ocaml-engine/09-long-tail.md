# M8 — Long tail & skip-list freeze

**Goal**: everything remaining in scope — testinput2 oddities, the alternative-syntax
options, offset-limit errors — then freeze the skip-list and prove the whole in-scope
surface with a large fuzz run.

**Gate G8** (verbatim from plan): 100% of in-scope tests across 1,2,4,5,8,9,10; fuzzer
(full grammar) 1M cases, zero mismatches.

**GATE MET 2026-07-08** — conformance half: engine == oracle on all 3,112 in-scope units
(frontier `none`, since fd6ab32). Fuzz half at `3417078`: 1,000,000 cases seed 42 —
`distinct-classes=0 dup-hits=0, no divergences found` (1937 cases/sec, 516.4s) — plus
200k×{42, 101, 20260708} all clean. Getting here surfaced and fixed 5 defects (3
upstream C UB bugs pinned engine+oracle, 1 oracle-stub bug, 1 engine divergence):
commits e8eeaa8, 13454e2, 5c790c2, afa9b41, 3417078 — see the ORCHESTRATOR_LOG rows.
Regression corpus: 7 repros, all replaying green. Remaining unchecked boxes below are
either subsumed by the full-parity chunks (options long tail, oddities sweeps —
reconcile in the final plan-update pass) or still-open housekeeping (skip-list freeze,
§2.3 of REMAINING-WORK.md); offset-limit context knobs stay deliberately unexposed
(§2.4 decision).

## Chunks

- [x] callouts (compile + no-op execution; the M8 chunk the G7 census pointed at): (?Cn)
  and (?C"text") parse arm (pcre2_compile.c:4449-4563, delimiter tables, ERR38/39/81/82
  sites), OP_CALLOUT_STR emission (7114-7175, doubled-delimiter copy + usedlength slack),
  do_callout shell + OP_CALLOUT/OP_CALLOUT_STR dispatch + OP_COND callout skip
  (pcre2_match.c:254-334, 5590-5600, 5623-5638) — always-continue because no callout
  function can be installed (mb->callout == NULL, C:283; the API stays type-only). The
  pcre2test default-callout trace stays out of scope: units demanding it are either the 7
  skiplisted units or harness modifier-skips (callout_capture/error/extra/no_where),
  identical to the oracle's skip set. t2 733/733, t5 418/418 — engine == oracle everywhere.
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
- [x] fuzz campaign: full-grammar mask, 1,000,000 cases vs oracle, zero mismatches; all
  minimized repros triaged into conformance regressions and fixed (7-repro corpus;
  campaign record in the GATE MET note above)

## Rough LOC estimate

~600 C LOC of new port + fix churn. Chunk count unknowable up front — re-chunk as the
frontier reveals it.
