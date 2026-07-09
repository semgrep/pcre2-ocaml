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
()
