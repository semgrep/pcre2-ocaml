# Port conventions — C (PCRE2 10.44) → OCaml (`src/engine/`)

Every line of `src/engine/` follows these rules. The fidelity-reviewer agent enforces them.
Reference sources live in `vendor/pcre2/src/` and are READ-ONLY. Never modify vendor files.

## 1. Naming map

| C | OCaml |
|---|---|
| `OP_CHAR`, `OP_KETRMAX`, ... | `Opcodes.op_char`, `Opcodes.op_ketrmax` (int constants, same numeric values) |
| `META_ALT`, `META_CAPTURE`, ... | `Parse.meta_alt`, ... (same 32-bit encodings) |
| Frame fields `Fecode`, `Feptr`, `Foffset_top`, `Fovector` | `ecode`, `eptr`, `offset_top`, `ovector` (drop the `F`; these are arena slots or loop locals) |
| `mb->end_subject`, `cb->...` | `mb.end_subject` on records `mb` (match block), `cb` (compile block) |
| PascalCase/struct types `heapframe`, `match_block`, `compile_block` | snake_case records: `frame` (arena slots), `match_block`, `compile_block` |
| Macros `GET2`/`PUT2`, `GETCHARINC`, `CHMAX_255` | functions: `Compile_util.get2`/`put2`, `Utf.getcharinc`, ... |
| Error macros `ERR5`, `PCRE2_ERROR_NOMATCH` | `Errors.err5`, `Options.error_nomatch` (same ints) |

Module ← C file mapping is in `docs/ocaml-engine/00-architecture.md`. New helpers go in the
module owning the C file they come from.

## 2. Control-flow translation

- **`goto` → loop/label**: translate backward gotos as `let rec loop ... ` or a `while` with
  mutable state; forward gotos (shared epilogue like `FRRETURN`, `LBL(...)`) become functions
  or a variant-typed "next action" value dispatched by `match`. The interpreter's central
  dispatch stays ONE iterative loop over the frame arena — never OCaml-native recursion per frame.
- **`switch` fallthrough**: either duplicate the tail code or factor it into a shared function.
  Always mark the seam: `(* fallthrough from OP_NOTI in C *)`. Do not silently reorder cases.
- **`for(;;)` + `break`/`continue`**: use exceptions ONLY outside the interpreter frame loop
  (see §5); inside it, use explicit loop functions with an accumulator/result variant.
- Preserve the C's evaluation ORDER of side conditions (limit ticks, UTF checks, ovector
  writes). Reordering is a fidelity bug even when the result looks equivalent.

## 3. Unsigned arithmetic

OCaml `int` is 63-bit signed; C code unit math is unsigned. Rules:

- uint8 (code units, table bytes): mask after arithmetic — `(c + d) land 0xff`. Reads from
  `Bytes` via `Char.code` are already 0..255.
- uint32 (chars, options, META words): represent as `int`, mask with `land 0xFFFF_FFFF` only
  where C relies on 32-bit wraparound; comparisons that C does unsigned must be written on
  the masked value. Option bits arrive as `int32` at the `Bindings` seam — convert once with
  `Int32.to_int ... land 0xFFFF_FFFF`, never deeper in.
- Right shifts of "unsigned" values: `lsr`, never `asr`.
- LINK_SIZE = 2, big-endian, exactly as the C 8-bit library:
  `put2 code i v` = `code.[i] <- v lsr 8; code.[i+1] <- v land 0xff`;
  `get2 code i` = `(code.[i] lsl 8) lor code.[i+1]`.
  Offsets stored by `PUT`/`GET` are relative to the opcode position — copy the C's `+1`/`+LINK_SIZE`
  adjustments literally and cite the C line. Link-offset off-by-ones are the #1 review category.

## 4. Citation comments

Every ported block (function, match arm group, table) carries a citation on its first line:

```ocaml
(* pcre2_match.c:5210-5288 *)
```

- File name + line range in the vendored 10.44 sources. Keep ranges accurate when editing.
- Deliberate deviations get a `(* DEVIATION: <why> *)` comment adjacent to the citation and
  must be listed in the executor's report and the commit body.

## 5. Behavior-parity contract (from the approved plan — binding)

- NOMATCH (−1) and PARTIAL (−2) → `Ok None` at the `Bindings` seam; every other negative code
  → `Error <raw code>`.
- Negative start offset, or offset > subject length → `Error (-33)` (BADOFFSET), checked
  uniformly in match/capture, interp and jit paths alike.
- Capture group `i` occupies `ovector.(2*i), ovector.(2*i+1)`; unset group = `(-1, -1)`;
  name table = `(name, group_number)` array in PCRE2 name-table order.
- `jit_compile` re-tags the same compiled value and always returns `Ok`; `jit_match`/`jit_capture`
  are aliases of the interp paths (pure version validates UTF where C JIT skipped it — that is
  the accepted, strictly-safer behavior). OUnit runs the same functor over `Interp` and `Jit`;
  both must stay green.
- Unknown option bits → error 117 (BAD_OPTIONS) at compile, −34 (BADOPTION) at match.
- `find_iter`/`captures_iter` semantics are FROZEN (empty-match-repeat bug included). Never
  "fix" them; `src/pcre2.ml` is untouched by this project.
- Compile errors: exact error NUMBER, exact `erroroffset`, and exact MESSAGE string from
  `pcre2_error.c` (the harness diffs `Failed: error NNN at offset N: <message>` lines).
- `Engine.version = (10, 44)`.

## 6. Stack safety

- NO exceptions may cross the interpreter frame loop; the loop body is exception-free
  (bounds errors are proven away, limits return error codes as values).
- The matcher is iterative ONLY: flat int-array frame arena, grown geometrically, heap-limit
  accounted in simulated C frame bytes. Depth = frame index; match_count ticks at the same
  sites as C's RMATCH.
- `let rec` self-calls in dispatch/scan loops are annotated `(f [@tailcall]) ...` so the
  compiler errors if a call stops being a tail call.
- Non-tail recursion on subject-length-proportional or backtrack-depth-proportional data is
  FORBIDDEN. Compile-time recursion is allowed only where C recurses, bounded by error 119
  (parenthesis nesting ~250).

## 7. Forbidden in `src/engine/`

- `Obj.magic` (or any `Obj.*`).
- Polymorphic `=`, `<>`, `compare`, `Hashtbl.hash` — use `Int.equal`, `String.equal`, etc.
- `Str`, `Re`, or any new dependency in package `pcre2` (stdlib only; `dune build -p pcre2`
  must stay dependency-free).
- `Printf`/`Format` in match/compile hot paths (fine in `debug_printer.ml` and error paths).
- Mutation of vendored files; editing `src/pcre2.ml{,i}` or `src/intf.ml`.

## 8. Performance rules (hot loop = interpreter dispatch + frame ops)

- Zero allocation inside the frame loop: no closures, no tuples/options/records per
  iteration, no partial application. Results flow through preallocated arrays and ints.
- `Bytes.unsafe_get`/`Array.unsafe_get` ONLY directly below a comment proving the bound,
  e.g. `(* safe: eptr < mb.end_subject checked at loop head *)`.
- No polymorphic comparison, no `List.*` in the hot loop.
- Prefer copying C's specialization structure (e.g. separate caseful/caseless arms) over
  "cleaner" unified code — fidelity and speed align here.
