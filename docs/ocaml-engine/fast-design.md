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
HANDLER: at run time (chunk C2) an `ALT` pushes a save record and
falls through into its branch body; on backtrack the runner pops it, restores `eptr`, and
tail-loops at `handler`. The C's dual-role `OP_ALT` (branch separator that, in forward
flow, jumps to the KET) is split into a branch-entry `ALT` (choice point) plus a
branch-tail `JMP` (skip to KET). `ALT` handlers chain: `ALT_k.next` = the entry of branch
`k+1`, which is that branch's own `ALT` for `k+1 < n`, or `Bn`'s body for `k+1 = n`. A
single-branch group emits just `BRA … KET` (no choice point).
- §6 stack-safety and §8 hot-loop rules of port-conventions.md bind unchanged: iterative
  only, no exceptions across the loop, zero allocation, `[@tailcall]`, `unsafe_*` only
  under bounds-proof comments. Cold instructions go out-of-line (icache; global `-inline`
  regressed in M10).

**Save-record layout (landed in chunk C2, `save_stack.ml`).** A record is a fixed
`record_width = 3` ints, pushed at each `ALT` (and popped on backtrack), low index first:

| slot | field | meaning |
|---|---|---|
| 0 | `handler` | IR index to resume at on backtrack (= the `ALT`'s `next`) |
| 1 | `eptr` | subject position to restore |
| 2 | `rdepth` | virtual frame depth to restore (§4 shadow accounting) |

`match_call_count`, `hitend`, `start_used_ptr` are NOT saved — the first is monotonic
(never restored), the second monotonic within an attempt, the third constant per attempt.
`sp` (the stack top, an int count) is a runner tail-call parameter; push/pop are explicit
`ss.data.(sp+k)` index arithmetic inlined at the `ALT`/backtrack sites. The stack resets to
`sp = 0` per attempt (write-before-read; no clearing). Backtracking with `sp = 0` returns
`MATCH_NOMATCH` (the C's RRETURN unwinding to frame 0).

**Scratch policy (chunk C2).** `save_stack.ml` retains ONE module-level `int array` across
execs, guarded by its OWN `Atomic.t busy` flag (NOT Frames' scratch slot); a concurrent exec
(flag held) falls back to a fresh, never-retained array. `runner.ml` caches ONE match block
(`mb`, the arena-slot-equivalent record holding this state) whose `ss` is the save-stack
scratch, reused only when the exec owns the flag — the single acquire gates both, mirroring
the interpreter's scratch trio (frames.ml scratch slot + `fresh_trio`). Growth is geometric;
on release an array over `scratch_max_retained_ints` (131072 ints, ~1 MiB) is dropped
(frames.ml precedent). Steady state (single-threaded): zero per-exec record/stack
allocation — only the `outcome` record + the result wrapper (measured < 60 minor words/exec,
0 per bump-along attempt).

## §4 Limits — shadow C-frame accounting (landed in chunk C2)

Trip points are byte-identical to the interpreter. The `mb` record (runner.ml) carries the
shadow state; there is no physical frame arena.

- `match_call_count` (`mcc`, a runner tail-call parameter): ticked once per
  C-`new_frame`-equivalent event, compare-old-then-bump exactly as
  interpreter.ml:1260-1267/1301-1306; reset to 0 per attempt (the `mcc = 1` start after the
  frame-0 tick), interpreter.ml:9324 site.
- Depth: virtual `rdepth` (runner parameter); a child branch runs at `rdepth + 1`, restored
  from the save record on backtrack; the check is `new_rdepth >= match_limit_depth`.
- Heap: virtual `heapframes_size` (simulated C bytes) initialized by the `Frames.create`
  arithmetic and grown by the `Frames.grow` arithmetic for frame index `n = new_rdepth`
  (`frame_size_bytes = Frames.frame_size_bytes_for ~top_bracket:0 = 136`); `HEAPLIMIT` /
  `NOMEMORY` fire at C-identical byte points. Persists across attempts within one exec
  (only grows), re-initialized per exec.
- Order at a child tick (transcribes rmatch, interpreter.ml:1188-1267): (1) heap push/grow
  for frame `new_rdepth`, then (2) `mcc` vs `match_limit`, then (3) `new_rdepth` vs
  `match_limit_depth`.
- Limits resolved as `min(re.limit_*, Limits.*)` per interpreter.ml:9825-9837.

**Tick-site table (IR instruction / event → ticks).** `d` = current `rdepth`. Derived from
the OP_BRA/OP_ALT/OP_KET arms: only branch entries that the C rmatches (`grouploop` for the
top-level group — rdepth 0; `bra_loop` non-last branches for nested groups) tick and enter a
child frame. `grouploop`'s single/last branch ticks (the top level "can't optimize" case);
`bra_loop`'s last branch does not (runs in the same frame).

| event | ticks | rdepth after | heap push |
|---|---|---|---|
| attempt entry (frame 0) | 1 (rdepth 0) | 0 | none |
| `BRA`, next head is `ALT` (multi-branch) | 0 | `d` | none |
| `BRA`, single-branch, `d = 0` (top-level group) | 1 (rdepth 1) | 1 | frame 1 |
| `BRA`, single-branch, `d >= 1` (nested group) | 0 | `d` | none |
| `ALT` (any non-last branch entry) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop → handler is non-`ALT`, saved `d = 0` (top-level last branch) | 1 (rdepth 1) | 1 | frame 1 |
| backtrack pop → handler is `ALT`, or saved `d >= 1` | 0 | saved `d` | none |
| `JMP`, `KET`, `CHAR_RUN`, `CHARI`, `SOD`/`SOM`/`EOD`/`EODN`/`CIRC`/`DOLL`, `END` | 0 | unchanged | none |

The only `grouploop` group in this subset is the whole-pattern outer `BRA` (dispatched at
rdepth 0 — nested groups are always entered from a child branch, rdepth `>= 1`). Its last
branch is reached either by falling through the `BRA` (single-branch) or via the preceding
`ALT`'s handler landing on a non-`ALT` head with saved `d = 0`; both tick (rows 3 and 6).

**Start-of-match scan is required for tick parity (chunk C2, not deferred).** A skipped
attempt does ZERO ticks, so a naive bump-along that runs attempts the interpreter's
first_cu/start_bits/startline/minlength/req_cu scans skip would trip `(*LIMIT_MATCH=N)`
(`-47`) where the interpreter does not, AND — for the anchored first_cu/start_bits gate,
which breaks unconditionally with no partial exception (pcre2_match.c:7188-7215, unlike
first_cu_tail:7300-7315) — would surface a PARTIAL the interpreter suppresses when
`allowemptypartial` holds (e.g. `\A0` on `""` under `PARTIAL_SOFT`: `\A` gives
`max_lookbehind = 1`). runner.ml therefore ports the scalar (uncached, result-identical)
bump_top + first_cu_tail + tail_opts scans; only the memchr result-caching and the
JIT range-skip table are left to chunk L. `PCRE2_FIRSTLINE` is declined at compile
(`Ir_compile`) — its enforcement is entangled with those scans' shortened `end_subject`.

- Continuous check: fuzz `--mode fast-vs-interp` with the imposed `LIMIT_MATCH=2000`
  differential — any `-47`/`-2`/ovector divergence fast-vs-interp is a bug (0 over 150k+
  cases in chunk C2).

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
