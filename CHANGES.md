# pcre2-ocaml changelog

## Unreleased

* Fixed a segfault under major GC compaction: the capture-group name table for
  a pattern with no named groups was built as a zero-length `caml_alloc_small`
  block rather than the shared atom.

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
