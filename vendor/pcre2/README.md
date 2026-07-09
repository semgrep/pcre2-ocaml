# Vendored PCRE2 10.44

Pinned copy of selected files from the [PCRE2 project](https://github.com/PCRE2Project/pcre2),
tag `pcre2-10.44` (see `VERSION`). Licensed under the BSD 3-clause licence with the
PCRE2 exception — see `LICENCE`.

Purpose (read-only; never compiled into the published `pcre2` package):

- `src/` — reference sources for the pure-OCaml port. Port work cites these files by
  line range (e.g. `(* pcre2_match.c:5210-5288 *)`). `pcre2_chartables.c.dist`,
  `pcre2_ucd.c`, and `pcre2_ucptables.c` are also machine-parsed by `gen/gen_tables.exe`
  to produce the committed OCaml tables in `src/engine/`. `pcre2test.c` is the
  behavioural reference for the `test/pcre2test/` harness.
- `testdata/` — the upstream conformance corpus consumed by `test/conformance/runner.exe`.

Out-of-scope upstream files (JIT, DFA, substitute, serialization, POSIX shim, sljit)
are intentionally not vendored, except where useful as reference.

Do not edit anything under this directory. To move to a newer PCRE2, re-vendor the new
tag wholesale, update `VERSION`, and re-run the table generators.
