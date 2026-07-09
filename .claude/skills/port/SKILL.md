---
name: port
description: Use when porting exactly one planned C chunk (300–600 C LOC) from vendor/pcre2/src into src/engine/ with citation comments and covering tests.
---

# Port one chunk

Input (from the orchestrator dispatch): chunk name, C `file:line-range`(s), target OCaml
module in `src/engine/`, acceptance probe (frontier `file:ordinal`).

## Steps

1. **Read the rules first**: `.claude/rules/port-conventions.md` (naming map, unsigned
   arithmetic, goto translation, parity contract, forbidden list). Non-negotiable.

2. **Read the C source range** in `vendor/pcre2/src/` — the WHOLE range plus enough
   surrounding context (macros in `pcre2_internal.h`/`pcre2_intmodedep.h`, callers) to
   understand every branch. Note fallthroughs, gotos, unsigned ops, and LINK_SIZE math —
   these are where ports die.

3. **Check existing OCaml**: read the target module and its `.mli` (if any) in `src/engine/`,
   plus `src/engine/engine.mli` for the boundary types. Match the established style of
   already-ported code.

4. **Write the OCaml** with structural fidelity (85–95% correspondence): same function
   decomposition, same case order, same error paths. Every ported block starts with a
   citation comment `(* pcre2_match.c:5210-5288 *)`. Mark deliberate deviations with
   `(* DEVIATION: ... *)`. Stay inside the chunk — if you find an adjacent bug, report it,
   don't fix it.

5. **Build**:
   ```
   nix develop -c dune build @all
   ```
   Fix warnings-as-errors; no `[@warning]` suppressions without a comment justifying them.

6. **Enable/adjust covering tests**: if the chunk's conformance section is in
   `test/conformance/skiplist.sexp` as `deferred` for this milestone, remove those entries.
   Add/enable OUnit cases in `test/` where the plan doc calls for them.

7. **Run targeted conformance** on the acceptance probe and the chunk's feature section:
   ```
   nix develop -c dune exec test/conformance/runner.exe -- --only <file>:<ordinal>
   nix develop -c dune exec test/conformance/runner.exe -- --frontier
   ```
   Then the pure OUnit suite:
   ```
   nix develop -c dune runtest
   ```

8. **Report** (this is your return value — be exact):
   - Chunk name + C ranges actually ported (may differ slightly from dispatch — say so).
   - Conformance ordinals newly PASSING and any newly FAILING (list `file:ordinal`).
   - Frontier before → after.
   - Deviations from the C, with reasons.
   - Skiplist entries removed; tests enabled.
   - Anything discovered but out of scope (for the log's notes column).

## Don'ts
- Don't touch `src/pcre2.ml{,i}`, `src/intf.ml`, `vendor/`, or `baseline_counts.sexp`
  (verify/commit own the baseline).
- Don't port ahead of the chunk boundary "while you're there".
- Don't commit — the orchestrator's /commit skill does that after review.
