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

## §2 IR encoding (lands in chunk C)

Principles fixed by the plan (concrete layout TBD in chunk C, kept current here):

- One flat `code : int array`; `pc` indexes instruction heads: `[tag; operand...]`.
- Tags are a Fast-private dense enum (`Ir` module), NOT PCRE2 opcode numbers — fused and
  specialized instructions get their own tags; dispatch is a literal-int `match` (jump
  table), like interpreter.ml:1337.
- Operands are PRE-DECODED: no `get2` link reads at run time; LINK offsets resolved to
  absolute IR indices at IR-compile time.
- `CHAR_RUN`: consecutive caseful `OP_CHAR`s fuse into one instruction comparing against a
  literal pool (`lit : string`) word-at-a-time (`String.get_int64_ne` + masked tail) —
  mirrors `byte_sequence_compare` (pcre2_jit_compile.c:7479).
- Variable-length payloads (XCLASS data, MARK names) stay in the original bytecode;
  the IR stores offsets into `re.code` (JIT does the same); `Ir.t` therefore pins
  `Compile.re`.
- `Ir_verify` (static checker): every jump target in-range and on an instruction head,
  every tag handled by the runner, save-record widths consistent, virtual-frame accounting
  balanced. Runs after every compile in tests; a cheap subset always-on in `Fast.compile`.
  There is no runtime fallback, so the verifier is the guarantee that accepted IR cannot
  reach an unhandled instruction mid-match.

## §3 Runner and save records (lands in chunk C)

- ONE module-level tail-recursive loop; hot state (pc, eptr, save-stack pointer, rdepth,
  ...) as tail-call int parameters (register discipline). No function boundary per
  backtrack tick — the fusion declined for the mainline (performance-report.md §4) is THE
  design here.
- Backtracking is direct-threaded: a save record's slot 0 is the handler's IR index;
  restore = positional loads, then tail-loop with the new pc. Save SETS are computed at IR-
  compile time per choice point (minimal state, mirrors JIT's backtrack_common chain +
  private data, pcre2_jit_compile.c:244-256) — never the interpreter's full
  28+2×top_bracket frame copy.
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
