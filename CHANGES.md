# pcre2-ocaml changelog

## Unreleased

* Match calls now reuse a per-thread `pcre2_match_data` instead of allocating
  one per call, so matching in parallel across domains no longer serializes on
  the C allocator. Systhreads within a single domain still take turns, since
  matching holds the OCaml runtime lock. Compiled regexps remain shareable
  across threads.

  The cached block grows to fit the widest pattern a thread has matched and is
  not shrunk, so a thread's resident memory reflects its widest pattern for as
  long as it lives. It is freed when the thread exits on POSIX; the initial
  thread is the exception, since POSIX runs no thread-specific-data destructors
  when `main` returns or `exit` is called. Windows does not free it at all.
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
