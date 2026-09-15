# pcre2-ocaml changelog

## Unreleased

* Added `` `MATCH_LIMIT ``, `` `DEPTH_LIMIT ``, and `` `HEAP_LIMIT `` compile
  options, bounding the resources any match against the pattern may consume
  (guarding against catastrophic backtracking). The limits are bundled with the
  compiled pattern and apply to every subsequent match. Note that `` `DEPTH_LIMIT ``
  and `` `HEAP_LIMIT `` are ignored by JIT matching; only `` `MATCH_LIMIT ``
  applies to JIT.
* `config_match_limit` and `config_depth_limit` now report the linked library's
  actual defaults (previously the `-1` placeholder), and `config_heap_limit` was
  added.

## 7.5.3 (2024-04-18)

* Fixed bug in `raise_bad_pattern` regarding string creation for the exception,
  added a test.
* Changed some declarations to be C99-compatible (some functions were
  declared/defined with empty parameter lists but were intended to take no
  arguments).

## 7.5.2 (2023-09-06)

* fixed bug in `full_split`, added first unit-test for same

## 7.5.1 (2023-09-01)

* Created pcre2-ocaml bindings based on original pcre-ocaml project
