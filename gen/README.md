# gen — table generator (dev-only)

`gen_tables.exe` parses the machine-generated PCRE2 10.44 table sources in
`vendor/pcre2/src/` and emits the committed OCaml tables in `src/engine/`:

| output                    | parsed inputs |
|---------------------------|---------------|
| `src/engine/chartables.ml` | `pcre2_chartables.c.dist` |
| `src/engine/ucd_tables.ml` | `pcre2_ucd.c` |
| `src/engine/ucptables.ml`  | `pcre2_ucptables.c` (+ `pcre2_ucp.h` / `pcre2_internal.h` to resolve `ucp_*` / `PT_*` values) |

The parsers are strict: anything unexpected in the vendored sources is a fatal
error, never skipped.

## Regenerate

From the repo root:

    dune exec gen/gen_tables.exe -- --check --out src/engine vendor/pcre2/src

`--check` runs sanity assertions on the parsed tables before writing (known
Unicode facts: `fcc 'a' = 'A'`, two-stage UCD lookup of U+0041 gives
other_case U+0061, `utt` contains `"greek"` as `(PT_SCX, ucp_Greek)`, the
10.44 table sizes, etc.).

## Sync check

    dune build @gen-check

regenerates into `_build` and diffs against the committed `src/engine` files;
CI uses this to assert they never go stale. The plain build also exercises the
generator (the regeneration rule is a normal dune rule with `--check`).

The hand-ported companions `src/engine/ucp.ml` and `src/engine/tables.ml` are
NOT generated; they follow `.claude/rules/port-conventions.md` with citations.
