# Orchestrator log

Append-only. One row per orchestrator action (PORT chunk, FIX, PLAN reconcile, gate).
Never edit or delete prior rows; corrections get a new row referencing the old one.
Each row is appended in the SAME commit as the work it records (commit column = short SHA;
`pending` only transiently before the amend step of /commit).

Columns:

- **timestamp** — UTC, ISO 8601.
- **mode** — `PORT` | `FIX` | `PLAN` | `GATE` | `NOTE`.
- **chunk** — milestone-doc checklist item name, or the fix's frontier test / diagnosis.
- **frontier before → after** — `file:ordinal → file:ordinal` (`—` when not applicable,
  e.g. before the runner exists).
- **commit** — short SHA of the commit carrying the work.
- **notes** — regressions hit, deviations, out-of-scope findings, review rounds, risks.

| timestamp | mode | chunk | frontier before → after | commit | notes |
|---|---|---|---|---|---|
| 2026-07-06T18:15:00Z | NOTE | M0 start: vendored PCRE2 10.44 sources + testdata; porting infrastructure (.claude/ skills, agents, rules; docs/ocaml-engine/ plan docs) authored | — → — | 9b1ab09 | vendor chunk of 01-infrastructure.md complete; harness/runner being built concurrently; next: build restructure chunk toward gate G0 |
| 2026-07-06T19:05:00Z | PORT | M0 build restructure: pcre2-dev package, oracle/ C library (oracle_-prefixed symbols), flake pins C pcre2 at 10.44, ocaml floor 4.14, include_subdirs for engine/ | — → — | 4f8f03f | oracle OUnit suite green 41/41 vs 10.44 (version test was failing vs 10.42 depext); GCC -I/-isystem dedup pitfall documented in flake.nix |
| 2026-07-06T19:10:00Z | PORT | M0 infra commit for .claude/ + docs/ocaml-engine/ (authored at M0 start, committed separately) | — → — | fcb3264 | log rows for 4f8f03f/fcb3264 appended late (infra bootstrapping); from here on, same-commit rule applies |
| 2026-07-06T19:40:00Z | PORT | M0 oracle test driver: pcre2test_stubs.c + pcre2-dev.test-driver (compile ctx, erroroffset, full ovector, MARK, startchar, info, name table, error_message) | — → — | 38ecb9e | smoke-tested: err 114 off 4 on "(abc"; MARK + unset-group ovector correct; harness agent consumes this |
| 2026-07-06T20:30:00Z | PORT | M0 constants: engine/limits.ml, options.ml, opcodes.ml (OP enum 0..170 + op_lengths + op_names, length-asserted), errors.ml (verbatim error texts + message) | — → — | d8e54bc | anchors spot-checked vs pcre2_internal.h (OP_CHAR=29, OP_KET=121, OP_BRA=135, table len 171); masks vs pcre2_compile.c:774-792/pcre2_match.c:73-82; message() mirrors pcre2_get_error_message indexing |
| 2026-07-06T21:20:00Z | PORT | M0 generators + tables: gen/gen_tables.exe (strict parser, --check sanity pins, @gen-check diff alias) -> generated chartables.ml, ucd_tables.ml (nrecords 1423, stage2 39040, Unicode 15.0.0), ucptables.ml (utt 489); hand-ported ucp.ml (295 consts), tables.ml | — → — | 43775d5 | Kelvin sign other_case=0 (cased via caseless set 100) noted; utt loose-matched names ("greek", "l&"); dune build -p pcre2 unaffected |
| 2026-07-06T21:55:00Z | PORT | M0 engine skeleton + pure swap: pcre2.engine sub-library (stdlib-only), engine.ml/.mli boundary (skeleton fails with ERR23/-44), bindings.ml rewritten as pure shim (names/types frozen), C stubs + config/ + conf-libpcre2-8 + dune-configurator removed from package pcre2, pure OUnit moved to @pure-tests until G1 | — → — | c97324d | dune build -p pcre2 green with no C; runtest = oracle suite 41/41; pcre2.ml/.mli/intf.ml zero-diff as required |
| 2026-07-06T22:40:00Z | PORT | M0 harness + runner: pcre2test-compatible harness (functor over DRIVER; full 140-entry modlist; process_data escape decoder; global-loop with NOTEMPTY_ATSTART retry + CRLF/UTF bump; copy/get/getall; MK:/partial/aftertext/allcaptures) + conformance runner (unit splitter/aligner, skiplist w/ STALE-SKIP, baselines, --frontier/--only) | — → testinput2:1305 | 75e5352 | testinput1 1290/1290 BYTE-IDENTICAL vs oracle; sweep: t4 617/617, t5 418P/0F/206S, t2 729P/12F/1102S; 12 failures diagnosed: 7 callout units, 4 MARK-with-NUL (driver strlen bug), 1 RunTest -error artifact |
| 2026-07-06T23:10:00Z | GATE | G0 MET: all 7 testdata files byte-identical vs C oracle outside skiplist (3112 in-scope units: t1 1290, t2 733, t4 617, t5 418, t8 0, t9 10, t10 44); MARK driver bug fixed (length prefix at mark[-1], pcre2test.c:8131/3099); skiplist: 7 callout units out-of-scope + 1 env RunTest artifact | testinput2:1305 → none failing | pending | harness modifier-skip census: t2 1102, t5 206, t8 80, t9 18, t10 117 — revisit cheap `info` fields at M8; baselines pinned |
