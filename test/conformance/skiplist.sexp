; Conformance skiplist.
;
; Format: a single sexp list of entries
;   (<file> (<ord-start> <ord-end>) <category> "<reason>")
; where
;   <file>                 testinput file name, e.g. testinput1
;   (<ord-start> <ord-end>) inclusive 1-based pattern-unit ordinal range
;   <category>             out-of-scope | env | deferred
;   "<reason>"             free-text justification
;
; Units matching an entry are excluded from pass/fail counting. If a
; skiplisted unit PASSES, the runner reports STALE-SKIP and exits nonzero.
(
 ; Callout trace output: patterns contain explicit (?Cn)/(?C"str") callouts and
 ; the expected output is pcre2test's default-callout trace (--->, "Callout
 ; (N): ..."). Callouts have no surface in this library's API (plan: out of
 ; scope; only the type constructors exist). Content-triggered, so these are
 ; unit skips rather than harness modifier skips.
 (testinput2 (1106 1106) out-of-scope "explicit callouts: default-callout trace output")
 (testinput2 (1305 1306) out-of-scope "explicit callouts: default-callout trace output")
 (testinput2 (1313 1314) out-of-scope "explicit callouts: default-callout trace output")
 (testinput2 (1490 1490) out-of-scope "explicit callouts: default-callout trace output")
 (testinput2 (1783 1783) out-of-scope "explicit callouts: default-callout trace output")
 ; RunTest appends `pcre2test -error -70,...` output to testoutput2 (script
 ; artifact, not produced by any testinput2 line); the trailing expected lines
 ; attach to the final unit.
 (testinput2 (1843 1843) env "trailing testoutput2 lines come from RunTest's pcre2test -error step")
)
