# Architecture — pure-OCaml PCRE2 engine

Condensed from the approved plan. Milestone docs `01-*.md`..`10-*.md` carry the work items.

## The seam: `src/bindings.ml`

The entire C boundary of this library is `src/bindings.ml` (52 lines, 8 `external`s):
`pcre2_compile`, `pcre2_match`, `pcre2_capture`, `pcre2_jit_compile/match/capture`,
`get_capture_groups`, `get_version`, `pcre2_ocaml_init`. It is rewritten as a ~40-line pure
shim over `src/engine/`, **preserving every value name and type** — including
`type 'a regex constraint 'a = [< jit | interp ]`. Consequences:

- `src/pcre2.ml`, `src/pcre2.mli`, `src/intf.ml` get ZERO diffs; public API parity holds by
  construction and these files are off-limits to all agents.
- OUnit suite, conformance harness, differential fuzzer, and the C oracle all target this
  one narrow seam.
- `jit_*` entry points alias the interp paths (the C stub already used one `pcre2_code`);
  `get_version () = (10, 44)`.

The old C stubs move to `oracle/` (unpublished dev package `pcre2-dev`) as `Pcre2_c`,
satisfying the same `Pcre2.Matcher` signature — the oracle for differential fuzzing and the
perf baseline. `dune build -p pcre2` must build the published library with no C, no
configurator, no Unicode dependency.

## Why PCRE2 10.44 (pinned; vendor commit 9b1ab09)

The error-code surface matches `pcre2.ml`'s variants exactly: compile codes 101–199 ending
at `BACKSLASH_K_IN_LOOKAROUND`, match codes −2..−67. 10.45's class-parser rework would add
gratuitous porting risk. Tests require version ≥ (10,43). `vendor/pcre2/` holds the 10.44
sources + testdata, read-only, and is what every citation comment refers to.

## Fidelity policy

High-structural-fidelity port of PCRE2's own architecture (85–95% structural
correspondence, mirroring the React TS→Rust port philosophy) — NOT a clean-room design.
Same function decomposition, same opcode set and bytecode layout (8-bit library,
LINK_SIZE=2), same error paths. Why: error code/message parity comes nearly free;
differential debugging via a `pcre2test -d`-style bytecode dump diffed against real
pcre2test; chunks of work are "port C function X lines A–B" with mechanical review;
upstream tracking stays feasible.

## Two-phase compile

Exactly as `pcre2_compile.c`: **parse** (pattern → META 32-bit int stream) then **compile**
(META → bytecode in `Bytes.t` with PCRE2's `OP_*` opcodes, name table, lookbehind lengths),
plus auto-possessification and study as separate behavior-gated passes. Compile-time
recursion is the only true recursion, bounded by PCRE2's error 119 (nesting ~250).

## Frame-arena interpreter

`pcre2_match.c` (since 10.30) is already iterative: a loop over heap-allocated backtracking
frames. Ported as a flat `int array` frame arena grown geometrically with loop/label
dispatch — native stack is O(1) in subject length and backtracking depth by construction.
Frames are all-int (MARK names as code offsets). Match/depth/heap limits tick at the same
sites as C's RMATCH; heap limit counts simulated C frame-byte sizes so MATCHLIMIT /
DEPTHLIMIT / HEAPLIMIT fire at parity. No exceptions cross the frame loop.

## Engine module map (module ← C source)

| `src/engine/` module | ← C source (vendor/pcre2/src/) | contents |
|---|---|---|
| `limits.ml` | `pcre2_internal.h` | LINK_SIZE, MATCH_LIMIT=10_000_000, DEPTH/HEAP limits |
| `options.ml` | `pcre2.h.in` | PCRE2_* option bits, PUBLIC_*_OPTIONS masks, error ints |
| `opcodes.ml` | `pcre2_internal.h` | OP_* enum + OP_lengths tables |
| `errors.ml` | `pcre2_error.c` | compile-error numbers + exact message strings |
| `chartables.ml` | `pcre2_chartables.c.dist` | lcc/fcc/cbits/ctypes — GENERATED |
| `tables.ml` | `pcre2_tables.c` | utf8 tables, [hv]space lists, caseless_sets |
| `ucp.ml` | `pcre2_ucp.h` | property enum |
| `ucptables.ml` | `pcre2_ucptables.c` | property-name table — GENERATED |
| `ucd_tables.ml` | `pcre2_ucd.c` | stage tables + records — GENERATED |
| `ucd.ml` | (GET_UCD macros) | script/chartype/othercase lookups |
| `utf.ml` | `pcre2_ord2utf.c` + macros | UTF-8 decode/encode/step (GETCHARINC, ord2utf) |
| `valid_utf.ml` | `pcre2_valid_utf.c` | validity with exact UTF8_ERR1..21 |
| `newline.ml` | `pcre2_newline.c` | is_newline / was_newline |
| `parse.ml` | `pcre2_compile.c` (front half) | phase 1: pattern → META stream |
| `compile.ml` | `pcre2_compile.c` (rest) | phase 2: META → bytecode, name table, lookbehind lengths |
| `auto_possess.ml` | `pcre2_auto_possess.c` | auto-possessification |
| `study.ml` | `pcre2_study.c` | first/req code unit, start bitmap, minlength |
| `xclass.ml` | `pcre2_xclass.c` | extended-class runtime match |
| `script_run.ml` | `pcre2_script_run.c` | (*script_run:) checking |
| `frames.ml` | `pcre2_match.c:100-830` | frame arena alloc/grow/copy + heaplimit accounting |
| `interpreter.ml` | `pcre2_match.c` | the big iterative match loop |
| `debug_printer.ml` | `pcre2_printint.c` | pcre2test -d style dump (differential debugging) |
| `engine.ml/.mli` | — | boundary module |

Generated modules are emitted by `gen/gen_tables.exe` from the vendored sources and
committed; a dune rule + CI check assert they stay in sync.

## `engine.mli`

```ocaml
type t
type exec_result =
  | Match of { ovector : int array; mark : string option; start_char : int }
  | No_match
  | Partial of { start : int; mark : string option }
  | Error of int                                   (* negative PCRE2 code *)
val compile : string -> int32 -> (t, int) result   (* err = 101..199 *)
val exec_full : t -> string -> int -> int32 -> exec_result   (* harness uses this *)
val exec : t -> string -> int -> int32 -> ((int * int) option, int) result
val exec_captures : t -> string -> int -> int32 ->
  (((int * int) array * (string * int) array) option, int) result
val capture_groups : t -> (string * int) array
val version : int * int
val print_code : Format.formatter -> t -> unit
```

`exec`/`exec_captures` fold `No_match`/`Partial` into `Ok None` per the parity contract in
`.claude/rules/port-conventions.md` §5; the harness uses `exec_full` to print `Partial
match:` and `MK:` lines.

## Out of scope (type-only, forever in this project)

Substitute, DFA matching, callouts, compile contexts — declared-but-unimplemented in the
current bindings; they stay that way. Test files exercising them are file-level skips (see
`testdata-order.md`).
