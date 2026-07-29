# pcre2-ocaml changelog

## Unreleased

* Match calls now reuse a per-thread `pcre2_match_data` instead of allocating
  and freeing one per call. Removes two C-allocator operations per match,
  which under multithreaded matching (e.g. OCaml 5 domains on musl's
  globally-locked allocator) previously serialized concurrent matching.
  Compiled regexps remain shareable across threads; per-call behavior is
  unchanged.

  A match that starts while another is already running on the same thread --
  a callout function that matches again -- falls back to a single-use block,
  since PCRE2 keeps the backtracking frames vector inside the block. So does
  `pcre2_dfa_match`, whose yield counts alternative match lengths rather than
  captures and therefore depends on how wide an ovector it is handed.

  The cached block grows to fit the widest pattern a thread has matched and is
  not shrunk, so a thread's resident memory reflects its widest pattern for as
  long as it lives. A thread frees the block it is holding when it exits on
  POSIX, with two exceptions that each retain one block per thread: the initial
  thread, since POSIX runs no thread-specific-data destructors when `main`
  returns or `exit` is called, and Windows, which registers no destructor at
  all.

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
