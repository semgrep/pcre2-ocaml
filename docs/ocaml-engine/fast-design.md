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
| 13 | `CIRCM` | — | 1 | `^` multiline (OP_CIRCM); runner mirrors interpreter.ml:3108-3124 via `Newline.was_newline` |
| 14 | `DOLLM` | — | 1 | `$` multiline (OP_DOLLM); interpreter.ml:3126-3161 via `Newline.is_newline` |
| 15 | `FAIL` | — | 1 | grouploop exhausted: propagate backtrack (no tick). A last-branch ALT's handler; forward flow JMPs over it |
| 16 | `CAP_START` | `ovbase` | 2 | open capture N (`ovbase = 2N`): push a CAP cleanup record, set `ovector[ovbase]` to the current position |
| 17 | `CAP_END` | `ovbase` | 2 | close capture N (the group's ket): set `ovector[ovbase+1]` to the current position |
| 18 | `REP` | `reptype; lmin; lmax; c` | 5 | caseful single-char repeat |
| 19 | `REPI` | `reptype; lmin; lmax; c1; c2` | 6 | caseless single-char repeat (`c2 = fcc(c1)`) |
| 20 | `NOTREP` | `reptype; lmin; lmax; c` | 5 | caseful negated-char repeat (match `≠ c`) |
| 21 | `NOTREPI` | `reptype; lmin; lmax; c1; c2` | 6 | caseless negated-char repeat (match `∉ {c1,c2}`) |
| 22 | `GROUP_START` | `g` | 2 | repeated-group iteration entry: record the start position in `mb.group_start.(g)` (empty-check source), pushing a `KIND_GSTART` to restore it on backtrack |
| 23 | `BRAZERO` | `skip` | 2 | greedy zero-repeat (`OP_BRAZERO`): try the group (tick, child frame), skip to `skip` (past the group) on backtrack |
| 24 | `BRAMINZERO` | `skip` | 2 | lazy zero-repeat (`OP_BRAMINZERO`): jump to `skip` (the continuation) first (tick, child frame), enter the group (`pc+2`) on backtrack |
| 25 | `KET_RMAX` | `entry; g` | 3 | greedy repeating ket (`OP_KETRMAX`): empty-check `g` (`no_group` = none) then loop back to `entry` (tick), give back the continuation (`pc+3`) on backtrack |
| 26 | `KET_RMIN` | `entry; g` | 3 | lazy repeating ket (`OP_KETRMIN`): empty-check `g` then try the continuation (`pc+3`, tick), reiterate at `entry` on backtrack |

**Chunk D additions.** `CIRCM`/`DOLLM` are the multiline anchors (their arms
carry no operands; runtime semantics only). `CAP_START`/`CAP_END` bracket a
capturing group; the group's *ket* IS the `CAP_END` (the branch `JMP`s target
it), so a capturing group emits no separate `KET`. `FAIL` marks a
grouploop group's exhaustion.

**Repeat superinstructions** (`REP`/`REPI`/`NOTREP`/`NOTREPI`). The 13 C
opcode families (STAR MINSTAR PLUS MINPLUS QUERY MINQUERY UPTO MINUPTO EXACT
POSSTAR POSPLUS POSQUERY POSUPTO) × {caseful, caseless} × {plain, NOT}
(OP_STAR..OP_NOTPOSUPTOI, 33-84) collapse to FOUR tags, distinguished only by
case-ness and NOT-ness; the family collapses into the pre-decoded operands
`reptype` (0 min / 1 max / 2 pos), `lmin`, `lmax` (= `Ir.rep_inf` =
`0xFFFFFFFF` for STAR/PLUS, matching interpreter.ml `uint32_max`), and the
char pool: `c` (caseful) or the `c1`/`c2` fold-pair (caseless, `c2 = fcc(c1)`;
NOT variants test `∉`). `Ir.dump` shows `REP <ty> {min,max} "c"` (or
`"c1/c2"`). EXACT is `lmin = lmax` with `reptype` unread.
`Ir_compile.rep_kind`/`rep_bounds` decode the opcode; the char follows the
opcode (offset +1) or the IMM2 count (offset +1+IMM2 for UPTO/MINUPTO/EXACT/
POSUPTO). Type repeats OP_TYPESTAR..OP_TYPEPOSUPTO (85-97) and class repeats
OP_CRSTAR.. stay declined (chunk E).

`BRA`/`KET` are emitted as no-op markers so the IR mirrors the bytecode structure (readable
dumps, jump targets land on real heads); the runner (chunk C2) advances past them. Simple
anchors carry no operands — their runtime semantics land in chunk C2. Multiline `^`/`$`
(`OP_CIRCM`/`OP_DOLLM`), `\b`/`\B`, `\K` are NOT this subset → `Unsupported "... (chunk
C2+)"`.

**Chunk D2 additions — quantified and optional GROUPS.** A repeated group
(bytecode bracket + `OP_KETRMAX`/`OP_KETRMIN`) lowers to the chunk-D group
body (ALT choice points + FAIL / bra_loop last branch) with two changes: (1)
if the bracket is a grouploop bracket (`OP_CBRA`/`OP_SCBRA`/`OP_SBRA` — the C's
`P != NULL`), a `GROUP_START g` precedes the group and the ket carries the
matching `g`, so the empty-string loop check compares the ket position to the
iteration start; an `OP_BRA` repeat (bra_loop, `P == NULL`) carries `g =
Ir.no_group` = −1 and never force-breaks (it can't match empty, mirroring the
C's short-circuit). (2) The ket is `KET_RMAX`/`KET_RMIN` whose `entry` loops
back to the group entry (the `GROUP_START`, or the `BRA` marker for bra_loop).
`OP_BRAZERO`/`OP_BRAMINZERO` (the greedy/lazy zero-repeat wrappers before a
group, for `*`/`*?` and the `{n,m}` optional tail) become one instruction plus
the group body; the skip target (past the group) is patched after the group is
lowered. `OP_SKIPZERO` (a `{0}` group) is elided entirely — no IR, the walk
just steps past the dead bytecode (so an unsupported construct inside a `{0}`
group cannot decline the pattern — strictly safe, the group is never entered).

**Declined to chunk G — possessive group repeats.** `OP_KETRPOS` +
`OP_BRAPOS`/`OP_CBRAPOS`/`OP_SCBRAPOS`/`OP_SBRAPOS` + `OP_BRAPOSZERO`
(`(?:…)++`, `(…)*+`, …). The C's KETRPOS protocol (`pcre2_match.c:5283-5328`)
juggles frames — `OP_KETRPOS` copies the whole frame-copy region back to the
predecessor `P` and returns `MATCH_KETRPOS`, and the `POSSESSIVE_GROUP` loop
(RM8) iterates one at a time from the outer level, committing each iteration
(discarding its internal backtracking) while still restoring the group's
captures if the whole group is backtracked past. That commit-but-restore
interplay does not map onto the minimal per-choice-point save records without a
frame-copy-back mechanism the fast engine deliberately lacks; it belongs to
chunk G (atomic/`OP_ONCE` groups, already declined there). The IR compiler
declines them precisely (`"fast: possessive group (…) (chunk G)"`).

**detect_repeat (`pcre2_jit_compile.c:1699-1835`) — DECLINED PERMANENTLY (not
a coverage gap).** Now that group repeats exist, detect_repeat's recognised
bytecode shapes (a run of identical `(?:X)(?:X)…` collapsed to `OP_EXACT n`, or
`(?:AB){4,6}` → a `{3}` prefix + a bounded `OP_UPTO`/`OP_MINUPTO` tail) DO
occur. But the shared compiler already UNROLLS `{n,m}` into physical bracket
copies (each with its own `OP_KET`), and the interpreter — the fast engine's
differential oracle — runs those copies as-is, ticking (`RMATCH`) once per
copy. detect_repeat is a JIT-ONLY normalization that re-collapses the copies;
the JIT never had a tick-parity obligation. Porting it would make the fast
engine tick FEWER times than the interpreter for an unrolled repeat, so a
`(*LIMIT_MATCH=N)` cap (`-47`) would trip at a different N — a §4 tick-parity
violation and a fuzz `--mode fast-vs-interp` divergence. It is therefore NOT
ported, by design; the unrolled copies are lowered individually (correct,
tick-identical) instead.

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
`PCRE2_FIRSTLINE` → `"... (chunk L)"`. (Chunk D removed the `top_bracket > 0`
gate — capturing groups are now lowered.) The first out-of-subset opcode
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

**Save-record layout (chunk C2 + chunk D, `save_stack.ml`).** Records are now
VARIABLE width. The discriminator `kind` is the record's TOP slot (highest
index) so `backtrack` reads it at `data.(sp-1)` and derives the record base
without knowing the type in advance; each kind has a fixed width. Low index
first:

| kind | width | layout | pushed at | on backtrack |
|---|---|---|---|---|
| `KIND_ALT` | 4 | `handler; eptr; rdepth; KIND_ALT` | each `ALT` | resume the next alternative at `handler` (restore `eptr`/`rdepth`) |
| `KIND_CAP` | 4 | `ovbase; old_start; old_end; KIND_CAP` | each `CAP_START` | restore `ovector[ovbase]`/`[ovbase+1]`, then keep popping |
| `KIND_REP_MAX` | 5 | `rep_pc; try_pos; floor; rdepth; KIND_REP_MAX` | greedy repeat with extra chars | retry the continuation at `try_pos` (decrement to `floor`) |
| `KIND_REP_MIN` | 5 | `rep_pc; count; eptr; rdepth; KIND_REP_MIN` | minimizing repeat with `lmin<lmax` | match one more char at `eptr`, retry the continuation |
| `KIND_CONT` | 4 | `target; eptr; rdepth; KIND_CONT` | BRAZERO / BRAMINZERO / greedy KETRMAX / lazy KETRMIN | resume at IR index `target` (restore `eptr`/`rdepth`), NO tick |
| `KIND_GSTART` | 3 | `g; old_start; KIND_GSTART` | each `GROUP_START` (tracked repeated group) | restore `mb.group_start.(g)`, then keep popping |

`match_call_count`, `hitend`, `start_used_ptr` are NOT saved (monotonic /
constant per attempt). `sp` is a runner tail-call parameter; push/pop are
explicit index arithmetic. The stack resets to `sp = 0` per attempt.
Backtracking with `sp = 0` returns `MATCH_NOMATCH`.

**Capture save sets (chunk D — the minimal-save design).** A single global
`mb.ovector` (size `2*(top_bracket+1)`, reused across execs, group slots reset
to UNSET per attempt) holds the whole match at `[0,1]` and group N at
`[2N,2N+1]`. Rather than a full-ovector copy at every choice point, each
capturing bracket cleans up ITSELF locally (mirroring the JIT's per-cbracket
entry-save / exhaustion-restore, `pcre2_jit_compile.c:11045-11054` /
`13443-13452`): `CAP_START N` pushes a `KIND_CAP` record saving group N's two
slots and sets `ovector[2N]` to the current position; `CAP_END N` sets
`ovector[2N+1]`; on backtracking PAST the entry the `KIND_CAP` record restores
both slots and keeps popping. So a branch that can write group k's ovector
saves/restores exactly those two slots — computed at IR-compile time (one
`CAP_START`/`CAP_END` pair per capturing bracket), never a wide per-choice-
point copy. `ALT`/repeat choice points therefore save NO ovector slots; the
enclosing group's cleanup already rolls back everything a failed branch wrote
(verified: `(?:(a)x|ay)` on "ay" gives group 1 unset, matching the interp).
`rc` (the pcre2 pair count) is recomputed at END as `highest set group + 1`
(= the interpreter's `end_offset_top/2 + 1`), not tracked incrementally.

**Group-repeat protocol (chunk D2 — the minimal-record design).** The C runs a
repeated group frame-per-iteration: entering the group RMATCHes a branch
(grouploop RM2, or bra_loop's RM1/same-frame), and the repeating ket RMATCHes
back to the bracket (KETRMAX RM7) or forward to the continuation (KETRMIN RM6),
each iteration in a fresh frame. The fast engine drives the SAME loop with the
existing group lowering plus four instructions and one shared record kind:

- `KET_RMAX` (greedy): if the iteration matched empty (`eptr = group_start.(g)`,
  the C's `Feptr == P->eptr`), break the loop and continue past the group (no
  record, no tick — matching the C's `Fecode += 1+LINK_SIZE; break`). Else push
  a `KIND_CONT` resuming at the continuation (the give-back point), tick, and
  loop to `entry`. The `entry` re-runs the group body, whose first `ALT`
  (grouploop) ticks again — so a grouploop reiteration is 2 ticks (RM7 +
  grouploop) and a bra_loop reiteration is 1 (RM7 only, no entry ALT), exactly
  the C's frame counts. On backtrack, `KIND_CONT` runs the continuation with NO
  tick (the C's same-frame `break` after RM7 NOMATCH).
- `KET_RMIN` (lazy): empty case as above; else push a `KIND_CONT` resuming at
  `entry` (the reiteration point), tick, and run the continuation. On backtrack,
  `KIND_CONT` reiterates at `entry` with NO tick (the C's `Fecode -= GET; break`
  dispatching the bracket in the same frame — the entry's grouploop ALT then
  ticks).
- `BRAZERO`/`BRAMINZERO` share `KIND_CONT`: BRAZERO ticks, pushes a give-back to
  the skip target, and falls into the group; BRAMINZERO ticks, pushes an
  enter-the-group point, and jumps to the skip (continuation). Both restore
  with NO tick on backtrack (RM9/RM10's same-frame `break`).
- The empty-string loop check needs each iteration's start position. A single
  `mb.group_start : int array` (indexed by the compile-time group id `g`, sized
  `ir.n_groups`, reused/grown across execs) holds it: `GROUP_START g` writes the
  current position and pushes a `KIND_GSTART` saving the OLD value; on backtrack
  `KIND_GSTART` restores it. This mirrors the C exactly — each C iteration keeps
  its start in its own frame (`P->eptr`), preserved across backtracking into an
  earlier iteration's branch; the `KIND_GSTART` restore reproduces that
  preservation for the shared slot (without it, a later iteration's
  `GROUP_START` clobbers an earlier one and the empty check loops — the bug the
  `^(?:a?b?)*$` conformance unit caught). An `OP_BRA` (bra_loop) repeat carries
  `g = Ir.no_group` and skips the check (the C's `P == NULL`; OP_BRA can never
  match empty, so the check would never fire anyway — but this stays faithful).
- Captures across iterations: each iteration's `CAP_START`/`CAP_END` write group
  `N`'s slots and push a `KIND_CAP` cleanup, so the LAST completed iteration's
  capture survives at END and a given-back iteration is restored by its
  `KIND_CAP` (verified: `(a)+` on `"aa"` → group 1 = `[1,2]`; `(a*)*` on `""` →
  group 1 = `[0,0]`; both match the interpreter).

**`optimized_cbracket` (`pcre2_jit_compile.c:404`, cleared `:1145-1184`).**
`Ir_compile.optimized_cbracket` ports the JIT flag: a capture is "optimized"
(its ovector slot may double as the in-progress start scratch, as `CAP_START`
does) UNLESS reached by `OP_REF`/`OP_REFI` (`:1145`), `OP_CREF` (`:1173`),
`OP_DNREF`/`OP_DNREFI`/`OP_DNCREF` (`:1180-1186`), or `OP_CBRAPOS`/
`OP_SCBRAPOS` (`:1159`). A linear analysis walk over `re.code` (with a
byte-length helper covering the variable-length opcodes — XCLASS, CALLOUT_STR,
arg-verbs, and type-repeat-of-`\p`) clears the bit for referenced groups; a
non-optimized capture is declined at `CAP_START` (`"fast: referenced capture
(chunk F/J/K)"`). In this chunk's subset every referencing opcode is itself
out of scope, so ALL captures come out optimized — chunks F/J/K widen the
referencing lowering and add the private-scratch path, flipping bits off
without changing this gate. The walk is written to never index out of bounds
(a conservative early stop only leaves captures optimized).

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
| backtrack pop `KIND_ALT` → handler non-`ALT`/`FAIL`, saved `d = 0` (top-level last branch) | 1 (rdepth 1) | 1 | frame 1 |
| backtrack pop `KIND_ALT` → handler is `ALT`/`FAIL`, or saved `d >= 1` | 0 | saved `d` | none |
| `JMP`, `KET`, `CHAR_RUN`, `CHARI`, `SOD`/`SOM`/`EOD`/`EODN`/`CIRC`/`DOLL`/`CIRCM`/`DOLLM`, `END` | 0 | unchanged | none |
| `CAP_START`, `CAP_END`, `FAIL`, `GROUP_START` | 0 | unchanged | none |
| `BRAZERO` / `BRAMINZERO` (enter the group / continuation) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| `KET_RMAX`/`KET_RMIN`, empty iteration (`eptr = group_start`) | 0 | `d` | none |
| `KET_RMAX` non-empty: greedy loop-back (RM7, push `KIND_CONT`) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| `KET_RMIN` non-empty: continuation try (RM6, push `KIND_CONT`) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_CONT` (BRAZERO skip / BRAMINZERO enter / KETRMAX give-back / KETRMIN reiterate) | 0 | saved `d` | none |
| backtrack pop `KIND_GSTART` (restore `group_start`, keep popping) | 0 | — | none |
| repeat min-loop / greedy scan (per char consumed) | 0 | unchanged | none |
| `REP`/`REPI`/`NOTREP`/`NOTREPI` EXACT (`lmin=lmax`) or possessive | 0 | unchanged | none |
| minimize repeat, `lmin<lmax`: first continuation try (push `KIND_REP_MIN`) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_REP_MIN`: extend one char + retry (`count<lmax`, matches) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_REP_MIN`: `count>=lmax` / end / mismatch | 0 | — | none (pop, propagate) |
| maximize repeat, greedy end `> floor`: first continuation try (push `KIND_REP_MAX`) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| maximize repeat, greedy end `= floor`: continuation in place | 0 | `d` | none |
| backtrack pop `KIND_REP_MAX`: `try_pos > floor` (give back one) | 1 (rdepth `d+1`) | `d+1` | frame `d+1` |
| backtrack pop `KIND_REP_MAX`: `try_pos = floor` (min position, in place) | 0 | `d` | none (pop) |

The whole-pattern outer `BRA` (dispatched at rdepth 0) is `grouploop` at the
top level; nested `OP_BRA` is `bra_loop` (rdepth `>= 1`, no THEN in chunk D).
Its last branch is reached either by falling through the `BRA` (single-branch)
or via the preceding `ALT`'s handler landing on a non-`ALT`/`FAIL` head with
saved `d = 0`; both tick.

**Group repeats (chunk D2) compose the ket tick with the entry tick.** A
`KET_RMAX`/`KET_RMIN` row above ticks ONCE (the C's RM7/RM6 RMATCH into a child
frame); the reiteration then re-runs the group entry, whose first `ALT` ticks
AGAIN for a grouploop group (`OP_CBRA`/`OP_SCBRA`/`OP_SBRA`) but not for a
bra_loop single-branch group (`OP_BRA`, no entry `ALT`). So a grouploop KETRMAX
reiteration is 2 ticks (RM7 + grouploop RM2) and a bra_loop one is 1 (RM7
only), matching the interpreter's frame counts exactly (interpreter.ml
6117-6120 RM7 → grouploop/bra_loop). The `BRAZERO`/`BRAMINZERO` tick precedes
the group's own entry tick the same way (the C's RM9/RM10 frame wraps the
bracket dispatch). Verified continuously by the fuzz `--mode fast-vs-interp`
`LIMIT_MATCH=2000` differential (0 divergences over 200k+ cases including
group repeats) and the group-repeat `LIMIT_MATCH` N-sweep test.

**Capturing groups (chunk D) are `grouploop`, so their LAST branch ticks too.**
`OP_CBRA`/`OP_SCBRA` (interpreter.ml:2434-2444 → `grouploop`) record a
backtracking point for EVERY branch, including the final one, unlike a nested
`bra_loop` `OP_BRA` whose last branch runs in the enclosing frame. The IR
mirrors this by emitting an `ALT` choice point for every branch of a capturing
group and pointing the LAST `ALT`'s handler at a `FAIL` (so exhaustion
propagates NOMATCH with no tick). Thus every capturing-group branch ticks at
`d+1` via its `ALT` (the `ALT` rows), and the `FAIL` head is only reached with
saved `d >= 1` (captures nest inside the wrapper), never triggering the
top-level `d = 0` tick. `CAP_START`/`CAP_END` themselves never tick.

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
