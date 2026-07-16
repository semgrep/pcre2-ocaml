# Fast-engine design (M11) — v0

The citable spec for engine-native code in `src/fast/` (port-conventions.md §9): blocks with
no `pcre2_jit_compile.c` counterpart cite a section of THIS document, and the fidelity
reviewer reviews them against it. Milestone/chunks: `11-fast-engine.md`. Approved plan:
`~/.claude/plans/shimmying-wibbling-brooks.md`.

## §1 Seam and the no-fallback contract

- Public seam: `src/fast/pcre2_fast.mli` — mirrors `engine.mli`, plus
  `compile_error = Compile_error of {errcode; erroroffset} | Unsupported of string`.
- **No fallback.** `compile` runs the SHARED compiler (`Pcre2_engine.Engine.compile_ctx`) —
  real PCRE2 compile errors carry exact number/offset parity — then the IR compiler; any
  construct the IR compiler does not handle yields `Unsupported`. A fast `t` is never
  executed by the interpreter, and no code path routes a match through `Engine.exec`.
- On accepted patterns, observable behavior is IDENTICAL to `Engine.exec/exec_full/
  exec_captures`: rc/ovector/mark/startchar, and limit trip points (§4). The interpreter is
  the differential ORACLE (conformance `--driver=fast`, fuzz fast-vs-interp).
- The Matcher surface is assembled from `pcre2.matcher` (`MakeMatcher` + `MakeConvenience`)
  exactly like `Pcre2.Interp`/`Jit`; iterator empty-match semantics are FROZEN.
- Harness encoding: at the pcre2test driver seam (`test/pcre2test/fast_driver.ml`),
  `Unsupported` is the sentinel errcode 998 + a reason cell read by `unsupported_of_error`;
  the harness records `unsupported:<reason>` as a skip, never a `Failed:` line.

## §2 IR encoding (landed in chunk C1 — `src/fast/ir.ml`)

Principles (fixed by the plan) and the CONCRETE layout as implemented:

- One flat `code : int array`; `pc` indexes instruction heads: `[tag; operand...]`.
- Tags are a Fast-private dense enum (`Ir` module), NOT PCRE2 opcode numbers — fused and
  specialized instructions get their own tags; dispatch is a literal-int `match` (jump
  table), like interpreter.ml:1337. `Ir.arity.(tag)` gives each instruction's WIDTH in ints
  (tag + operands); `Ir.tag_name.(tag)` its dump name; `Ir.max_tag` the highest valid tag.
- Operands are PRE-DECODED: no `get2` link reads at run time; LINK offsets resolved to
  absolute IR indices at IR-compile time.
- `CHAR_RUN`: consecutive caseful `OP_CHAR`s fuse into one instruction comparing against a
  literal pool (`lit : string`) word-at-a-time (`String.get_int64_ne` + masked tail —
  chunk M) — mirrors `byte_sequence_compare` (pcre2_jit_compile.c:7479). `OP_CHARI` runs do
  NOT fuse: one `CHARI` per code unit. That is a DELIBERATE conservative simplification,
  not JIT parity — the JIT fuses caseless runs too when a code unit's othercase differs by
  a single bit (or it has none): `compile_charn_matchingpath`
  (pcre2_jit_compile.c:9334-9396, reached for both OP_CHAR and OP_CHARI at :12542-12547)
  concatenates them into one `byte_sequence_compare` (:9392) with caseless=true. Declining
  it is behavior-identical (CHARI creates no choice point, so no limit-tick divergence);
  caseless fusion is a possible chunk-M optimization citing those lines.
- Variable-length payloads (XCLASS data, MARK names) stay in the original bytecode;
  the IR stores offsets into `re.code` (JIT does the same); `Ir.t` therefore pins
  `Compile.re`. `Ir.t = { code : int array; lit : string; re : Compile.re }`.

**Instruction table (chunk C1 subset).** `pc` = instruction head; operands are the ints
following the tag. Widths are `Ir.arity`:

| tag (int) | name | operands | width | meaning |
|---|---|---|---|---|
| 0 | `END` | — | 1 | end of program (accept) |
| 1 | `CHAR_RUN` | `lit_off; len` | 3 | match `len` caseful code units `lit[lit_off .. lit_off+len)` |
| 2 | `CHARI` | `ch` | 2 | match one caseless code unit `ch` (runner folds case via `Chartables`, non-UTF CHARI arm pcre2_match.c:1097-1103) |
| 3 | `BRA` | — | 1 | group entry marker (structural no-op) |
| 4 | `KET` | — | 1 | group exit marker (structural no-op) |
| 5 | `ALT` | `next` | 2 | choice point: `next` = absolute IR index of the next alternative's entry (the save-record HANDLER, §3) |
| 6 | `JMP` | `target` | 2 | end-of-branch jump: `target` = absolute IR index of the enclosing group's `KET` |
| 7 | `SOD` | — | 1 | `\A` (start of subject) |
| 8 | `SOM` | — | 1 | `\G` (start of match) |
| 9 | `EOD` | — | 1 | `\z` (end of subject) |
| 10 | `EODN` | — | 1 | `\Z` (end, or newline at end) |
| 11 | `CIRC` | — | 1 | `^` (non-multiline) |
| 12 | `DOLL` | — | 1 | `$` (non-multiline) |

`BRA`/`KET` are emitted as no-op markers so the IR mirrors the bytecode structure (readable
dumps, jump targets land on real heads); the runner (chunk C2) advances past them. Simple
anchors carry no operands — their runtime semantics land in chunk C2. Multiline `^`/`$`
(`OP_CIRCM`/`OP_DOLLM`), `\b`/`\B`, `\K` are NOT this subset → `Unsupported "... (chunk
C2+)"`.

`Ir.dump : Format.formatter -> Ir.t -> unit` prints one `%3d TAG operands` line per head
(debug_printer.ml style); `CHAR_RUN`/`CHARI` show the escaped literal, `ALT` shows
`next=<pc>`, `JMP` shows the target pc. Used for golden tests (`test/fast/fast_tests.ml`).

- `Ir_verify.check` (static checker, `src/fast/ir_verify.ml`): every instruction head has a
  known tag (`0..max_tag`) whose operands fit in `code`; the program ends with exactly one
  trailing `END`; every jump target (`ALT` handler, `JMP` target) is in range AND on an
  instruction head; every `CHAR_RUN` lit reference is in-bounds (`0 <= off`,
  `off+len <= |lit|`, `len >= 1`); ALT save-record metadata is consistent (handler is a
  valid head; record width is the fixed `arity.(ALT)`). Virtual-frame accounting balance is
  added with the runner (chunk C2). Runs after every compile in tests; a cheap subset will
  be always-on in `Fast.compile` once the runner exists. There is no runtime fallback, so
  the verifier is the guarantee that accepted IR cannot reach an unhandled instruction
  mid-match.

`Ir_compile.compile : Compile.re -> (Ir.t, string) result` walks `re.code` (mirroring
debug_printer.ml's opcode walk; LINK_SIZE = 2 via `Compile.get`). Compile-level gates run
BEFORE the walk: UTF/UCP option bits → `"fast: UTF mode (chunk I)"` / `"... UCP mode ..."`;
`top_bracket > 0` → `"fast: capturing groups (chunk D)"`. The first out-of-subset opcode
yields `Error "fast: <construct> (chunk X)"` (taxonomy §6).

## §3 Runner and save records (choice-point IR landed in C1; runner in C2)

- ONE module-level tail-recursive loop; hot state (pc, eptr, save-stack pointer, rdepth,
  ...) as tail-call int parameters (register discipline). No function boundary per
  backtrack tick — the fusion declined for the mainline (performance-report.md §4) is THE
  design here. **(chunk C2)**
- Backtracking is direct-threaded: a save record's slot 0 is the handler's IR index;
  restore = positional loads, then tail-loop with the new pc. Save SETS are computed at IR-
  compile time per choice point (minimal state, mirrors JIT's backtrack_common chain +
  private data, pcre2_jit_compile.c:244-256) — never the interpreter's full
  28+2×top_bracket frame copy.

**Choice-point lowering of alternation (landed in chunk C1, `ir_compile.ml`).** A
non-capturing, non-repeated group `(?:B1|B2|…|Bn)` (bytecode `BRA b1 ALT b2 ALT … ALT bn
KET`, where each `OP_ALT` link chains forward to the next `OP_ALT`/`OP_KET`) lowers to a
branch-entry choice-point form:

```
    BRA
    ALT next=e2        ; choice point for B1: on fail resume at e2 (B2's entry)
    <B1>
    JMP KET            ; B1 matched through -> skip the rest of the group
e2: ALT next=e3        ; choice point for B2: on fail resume at e3 (B3's entry)
    <B2>
    JMP KET
    …
    <Bn>               ; LAST branch: no choice point, no JMP (flows into KET)
    KET
```

so there are `n-1` `ALT` choice points and `n-1` `JMP`s. `ALT next=` is the save-record
HANDLER: at run time (chunk C2) an `ALT` pushes a save record `[handler = next; eptr]` and
falls through into its branch body; on backtrack the runner pops it, restores `eptr`, and
tail-loops at `handler`. The C's dual-role `OP_ALT` (branch separator that, in forward
flow, jumps to the KET) is split into a branch-entry `ALT` (choice point) plus a
branch-tail `JMP` (skip to KET). `ALT` handlers chain: `ALT_k.next` = the entry of branch
`k+1`, which is that branch's own `ALT` for `k+1 < n`, or `Bn`'s body for `k+1 = n`. A
single-branch group emits just `BRA … KET` (no choice point). The C1 save-record width for
`ALT` is fixed (2 ints); wider records (ovector saves, repeat counters) arrive in chunk D.
- §6 stack-safety and §8 hot-loop rules of port-conventions.md bind unchanged: iterative
  only, no exceptions across the loop, zero allocation, `[@tailcall]`, `unsafe_*` only
  under bounds-proof comments. Cold instructions go out-of-line (icache; global `-inline`
  regressed in M10).

## §4 Limits — shadow C-frame accounting

Trip points must be byte-identical to the interpreter:

- `match_call_count`: ticked once per C-`new_frame`-equivalent event (group entry and every
  RMATCH choice point — `Ir_compile` tags these sites), compare-old-then-bump exactly as
  interpreter.ml:1260-1267/1301-1306; reset per attempt (interpreter.ml:9324 site).
- Depth: virtual `rdepth` (loop parameter), child = parent + 1, vs `match_limit_depth`.
- Heap: a virtual frame count × `Frames.frame_size_bytes_for ~top_bracket`, checked with
  `Frames.create`/`grow` arithmetic — `HEAPLIMIT`/`NOMEMORY` fire at C-identical byte
  points even though physical save records are smaller. The runner tracks virtual frame
  high-water wherever `Frames.push` would run.
- Continuous check: fuzz `(*LIMIT_MATCH=2000)` differential — any `-47` divergence
  fast-vs-interp is a bug.

## §5 Start-of-match (JIT-mirrored ONLY; lands in chunks L/M)

No invented accelerators (binding user constraint): no BMH-on-full-literal, no
Aho–Corasick, no mid-pattern prefix entry. Exactly the JIT's menu, scalar:
`fast_forward_first_n_chars`/`scan_prefix` + the range skip table
(pcre2_jit_compile.c:5592,6159-6330), single/pair char scan, startline, start_bits, req_cu
(`search_requested_char:6655`), minlength, and `detect_early_fail` watermarks
(`:1292`, types `:232`). Until chunk L the driver uses a naive bump-along (correctness
first; the conformance diff does not depend on scan strategy).

## §6 Unsupported taxonomy

`Unsupported of string` — the string names the construct (stable prefix for skip-report
grouping), e.g. `"fast: IR compiler not yet implemented (chunk C, 11-fast-engine.md)"`,
later `"fast: OP_RECURSE (chunk K)"`. Coverage only widens (baseline ratchet
`fast_baseline_counts.sexp`); chunk K removes the last in-scope reasons.

## §7 Testing hooks

Conformance `--driver=fast` (unsupported→skip, byte-diff otherwise); IR goldens
(`Ir.dump`); fuzz `--mode fast-vs-interp` (from chunk C) + repro-corpus replay through
fast; alloc pins (per-attempt / per-exec major / per-exec minor at the fast seam);
`ulimit -s 512` stack-safety; bench Fast column + G11.2 gates.
