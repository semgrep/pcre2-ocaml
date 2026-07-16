(* Fast-engine backtracking save stack (M11 chunk C2). Engine-native code
   with no direct PCRE2 counterpart, so per port-conventions.md §9 the design
   cites the design doc (fast-design.md §3) rather than a C source range.

   A save record is a fixed [record_width]-int slot pushed at a choice point
   ([Runner]'s ALT). The stack is ONE flat [int array]; the runner drives it
   by explicit index arithmetic ([sp] = number of ints in use), inlining the
   loads/stores at its ALT and backtrack sites — this module only owns the
   array, its growth, and the cross-exec scratch reuse.

   Save-record layout (fast-design.md §3). Records are VARIABLE width; the
   discriminator [kind] is the TOP slot (highest index) so [backtrack] can
   read it at [data.(sp-1)] and derive the record base without knowing the
   type in advance. Each kind has a fixed width; low index first:

     KIND_ALT     (width 4): [handler; eptr; rdepth; KIND_ALT]
        handler — IR index to resume at (the ALT's [next]); eptr/rdepth the
        subject position and virtual frame depth to restore (§4).
     KIND_CAP     (width 4): [ovbase; old_start; old_end; KIND_CAP]
        a capture cleanup: on backtrack restore ovector[ovbase],[ovbase+1]
        to the values saved at CAP_START, then keep popping. Mirrors the
        JIT's optimized cbracket entry-save / exhaustion-restore
        (pcre2_jit_compile.c:11045-11054 / 13443-13452).
     KIND_REP_MAX (width 5): [rep_pc; try_pos; floor; rdepth; KIND_REP_MAX]
        a greedy char / type / class repeat (chunk E reuses this): on backtrack
        retry the continuation at [try_pos] (decrementing down to [floor]; a \R
        repeat skips mid-CRLF; a CLASS repeat also tries [floor] itself with a
        tick); rep_pc re-derives the kind + continuation pc via setup_rep.
        Mirrors repeatchar's RM26/RM28 (interpreter.ml:3800-3809 / 7192-7243),
        the class maxbt RM24 (:4436) and the type maxbt RM34 (:5515).
     KIND_REP_MIN (width 5): [rep_pc; count; eptr; rdepth; KIND_REP_MIN]
        a minimizing char / type / class repeat: on backtrack match one more
        unit at [eptr] (count from [count] up to lmax) and retry. Mirrors
        RM25/RM27 (char), RM23 (class, :7325) and RM33 (type, :7364).
     KIND_CONT     (width 4): [target; eptr; rdepth; KIND_CONT]
        a chunk-D2 group choice point that, on backtrack, resumes at IR index
        [target] with [eptr]/[rdepth] restored and NO tick (the C's same-frame
        `break` after RM9/RM10/RM7 NOMATCH or the KETRMIN reiteration). Used by
        OP_BRAZERO (skip), OP_BRAMINZERO (enter group), the greedy KETRMAX
        give-back and the lazy KETRMIN reiteration (fast-design.md §3).
     KIND_GSTART   (width 3): [g; old_start; KIND_GSTART]
        a chunk-D2 repeated-group iteration entry (t_group_start): on backtrack
        restore [mb.group_start.(g)] to the enclosing iteration's start, then
        keep popping. This mirrors the C's per-frame group-start: each
        iteration runs in its own frame whose predecessor P->eptr is preserved
        across backtracking (pcre2_match.c:6107), so re-entering an earlier
        iteration's branch sees ITS start, not a later iteration's.

   Scratch reuse mirrors Frames' scratch pattern (frames.ml:282-403) with its
   OWN [busy] flag — it does NOT share Frames' scratch slot. A single
   module-level array is retained across execs and reused when free; a second
   concurrent exec (busy flag held) falls back to a fresh allocation that is
   never written back. A retention cap (like frames.ml's
   [scratch_max_retained_ints]) drops a pathologically grown array so it does
   not stay pinned forever. *)

(* fast-design.md §3 — record kinds (the TOP slot of each record) and their
   fixed widths (tag + operands, low index first). *)
let kind_alt = 0
let kind_cap = 1
let kind_rep_max = 2
let kind_rep_min = 3
let kind_cont = 4
let kind_gstart = 5

(* Chunk F additions (fast-design.md §3) — backreferences.
     KIND_CAPSTART (width 3): [ovbase; old_cap_start; KIND_CAPSTART]
        a referenced capture's ENTRY private-scratch save (the JIT's
        non-optimized-cbracket 1-slot save, pcre2_jit_compile.c:11055-11061 /
        13454-13459): on backtrack restore [mb.cap_start.(ovbase)] to the
        enclosing value, then keep popping. The ovector slots are NOT touched
        at entry (they stay UNSET until CLOSE, so a mid-match backref sees only
        closed values); the enclosing group's exhaustion still rolls back the
        ovector via the KIND_CAP pushed at CLOSE.
     KIND_REF_MIN (width 5): [rep_pc; count; eptr; rdepth; KIND_REF_MIN]
        a minimizing ref repeat (RM20, pcre2_match.c:5097-5113): on backtrack
        match one more copy at [eptr] ([count] up to Lmax) and retry the
        continuation; rep_pc re-derives ovbase/caseless/Lmax/cont.
     KIND_REF_MAX (width 6): [cont; try_eptr; flength; lstart; rdepth;
        KIND_REF_MAX] a maximizing ref repeat, samelengths (RM21,
        pcre2_match.c:5154-5162; non-UTF lengths are always equal so the rare
        RM22 rescan never occurs): on backtrack give back one copy
        ([try_eptr] -= [flength], down to and INCLUDING [lstart]) and retry
        the continuation. *)
let kind_capstart = 6
let kind_ref_min = 7
let kind_ref_max = 8

(* Chunk G additions (fast-design.md §3) — lookaround and atomic groups.
     KIND_ONCE (VARIABLE width 2*oveccount+3): the atomic-group / atomic-assertion
        boundary. Layout [ov_snapshot(2*oveccount-2); prev_once_base; saved_mark;
        entry_eptr; subtype; KIND_ONCE]. Pushed by [t_once]; snapshots the group
        ovector slots [2, 2*oveccount), the enclosing [mb.once_base], the entry
        mark and (chunk K1b) the entry eptr. On backtrack (the atomic construct
        is backtracked PAST): restore the ovector snapshot + [mb.once_base] +
        mark, then keep popping (propagate NOMATCH). This reproduces the C frame
        arena's "abandon-and-restore-wholesale" of everything an atomic group did
        (pcre2_match.c:6023-6031 Fback_frame; the ovector/eptr of P are restored
        because P is a distinct frame). [entry_eptr] is recorded into
        last_used_ptr when a VERB code passes the boundary (the C's RRETURN(rrc)
        from the construct's frame, pcre2_match.c:5408/:5529 — chunk K1b, §4);
        [subtype] selects THEN containment (pos-assert) vs escape (group). The
        width is not fixed here — the runner computes 2*mb.oveccount + 3
        (fast-design.md §3).
     KIND_NASSERT (VARIABLE width 2*oveccount+4): the negative-assertion
        boundary. Layout [ov_snapshot(2*oveccount-2); prev_once_base; cont;
        eptr_enter; rdepth_enter; saved_mark; KIND_NASSERT]. Pushed by [t_nassert]. On
        backtrack (ALL branches failed = the negative assertion SUCCEEDS,
        pcre2_match.c:5578-5584 ASSERT_NOT_FAILED): restore the snapshot +
        [mb.once_base], continue at [cont] with eptr/rdepth = the entry values.
        A branch that matched instead reaches [t_nassert_match], which rolls the
        snapshot back and propagates NOMATCH past the record (the assertion
        fails, pcre2_match.c:5557-5559).
     KIND_VREVERSE (width 6): a variable-lookbehind back-step choice point (RM37,
        pcre2_match.c:5874-5883). Layout [body_pc; cur_lmax; lmin; cur_eptr;
        rdepth; KIND_VREVERSE]. On backtrack: give up one back-step (cur_lmax--,
        cur_eptr++) and retry the branch body, until cur_lmax <= lmin (NOMATCH).
   KIND_ONCE / KIND_NASSERT are variable-width; [max_record_width] below is only
   used for the fixed-width records — the runner grows the stack per push with
   the exact [need], so no static bound is required for the variable kinds. *)
let kind_once = 9
let kind_nassert = 10
let kind_vreverse = 11

(* KIND_POS (VARIABLE width 2*oveccount+5): the possessive-bracket boundary
   (fast-design.md §3). Layout [ov_snapshot(2*oveccount-2); prev_once_base;
   iter_start; matched_once; zero_allowed; entry_rdepth; saved_mark; KIND_POS]. Pushed by
   [t_possess]; carries the KIND_ONCE snapshot + the per-loop state the
   greedy-atomic repeat needs (the iteration start for the empty-match check,
   whether an iteration ever matched, whether zero iterations are allowed, and
   the loop-level rdepth to reset to on loop-back). On backtrack PAST the whole
   group it behaves exactly like KIND_ONCE (restore snapshot + once_base,
   propagate NOMATCH); the extra state slots are read only on the forward loop
   (t_ketrpos / t_possess_done). *)
let kind_pos = 12

(* Chunk H (fast-design.md §3) — the backtracking control verbs. A single
   KIND_VERB record backs MARK and the fire-a-code verbs (PRUNE/COMMIT/SKIP/
   SKIP_ARG/THEN, incl. the _ARG mark-setting forms). Layout [vtype; aux; eptr;
   old_mark; KIND_VERB], width 5:
     vtype    — which verb (runner vt_* discriminator);
     aux      — the verb name offset (MARK/SKIP_ARG) or the THEN opcode's IR pc
                (THEN); unused otherwise;
     eptr     — the subject position when the verb executed (MARK's SKIP_ARG
                catch position / SKIP's pass-back position);
     old_mark — the enclosing mb.mark, restored on backtrack-past so mb.mark
                tracks the C's per-frame Fmark (verbs that set no mark store the
                current mark, a harmless self-restore).
   On NORMAL backtrack (NOMATCH): MARK restores mb.mark and keeps popping; every
   other vtype FIRES its verb code into [backtrack_code] (pcre2_match.c:6357/
   6370/6383/6397/6424/6434). On a verb code already propagating, the record is
   passed through (mb.mark restored), except MARK catches a name-matching
   MATCH_SKIP_ARG and converts it to MATCH_SKIP (RM12, pcre2_match.c:6351-6356). *)
let kind_verb = 13

(* Chunk I2 (fast-design.md §3) — the varied-lengths maximizing ref repeat
   (RM22, pcre2_match.c:5164-5184). Only reachable for a CASELESS reference in
   UTF mode (case-equivalent characters can differ in UTF-8 length, e.g.
   U+023A/U+2C65, so the copies' consumed lengths can differ and the RM21
   fixed-step give-back cannot be used). Layout
   [ref_pc; lmax_cur; try_eptr; lstart; rdepth; KIND_REF_MAX2], width 6:
   on backtrack, if [try_eptr] = [lstart] NOMATCH (5174); else Lmax--
   (lmax_cur), re-scan lmax_cur - lmin copies forward from [lstart]
   (match_ref is known to succeed, its rc discarded like the C's (void) cast,
   5177-5182) and retry the continuation at the new end. [ref_pc] re-derives
   ovbase/caseless/lmin/cont via setup_ref_rep. *)
let kind_ref_max2 = 14

(* Chunk K1b (fast-design.md §3) — pattern recursion.
     KIND_RECURSE (FAT, VARIABLE width): the subroutine-call boundary, the
       wholesale-frame answer to the §3 snapshot proof (a recursion jumps INTO a
       group without executing its entry, so the group_start/cap_start arrays
       could be stale). Pushed by [t_recurse]. Layout (low first):
         [ov_snapshot(2*oveccount-2); group_start_snapshot(n_groups);
          cap_start_snapshot(2*oveccount); prev_once_base; prev_current_recurse;
          prev_recurse_base; saved_mark; number; call_eptr; recurse_last_used;
          cont_pc; KIND_RECURSE]
       width = 2*(2*oveccount) - 2 + n_groups + 9. On backtrack PAST (the whole
       recursion failed): restore ALL snapshots + mb.once_base/current_recurse/
       recurse_base/mark, then keep popping (the C frame arena discarding the
       recursion's frames wholesale, pcre2_match.c:6470 RRETURN past OP_RECURSE).
       [mb.recurse_base] points at each open KIND_RECURSE's base (a chain via
       [prev_recurse_base]) for the RECURSELOOP walk (pcre2_match.c:5438-5453).
     KIND_RECURSE_RET (VARIABLE width): pushed at a recursion RETURN (the ket
       detecting mb.current_recurse == number) to make backtracking INTO the
       completed recursion body restore its state. The RETURN restores the WHOLE
       arrays (ovector + group_start + cap_start) to the pre-call snapshot going
       FORWARD (captures do not escape, and the enclosing construct's
       group_start/cap_start must be reinstated — the recursion re-entered a group
       without executing the enclosing entry); this record saves the POST-body
       arrays so a later backtrack into the body re-establishes them. Layout:
       [ov_body(2*oveccount-2); group_start_body(n_groups);
        cap_start_body(2*oveccount); number; my_recurse_base; KIND_RECURSE_RET],
       width = 2*(2*oveccount) - 2 + n_groups + 3. On backtrack: restore the
       arrays + mb.current_recurse = number + mb.recurse_base = my_recurse_base,
       then keep popping (into the body). *)
let kind_recurse = 15
let kind_recurse_ret = 16

(* Chunk K2 (fast-design.md §3) — \K (OP_SET_SOM).
     KIND_SET_SOM (width 2): [old_start_match; KIND_SET_SOM]. Pushed by [t_set_som]
        before it moves mb.start_match to the current position; on backtrack-past
        restore mb.start_match to [old_start_match], then keep popping. This
        mirrors the C's per-frame Fstart_match: a choice point recorded BEFORE the
        \K resumes with the old start, one recorded AFTER with the new — the LIFO
        record achieves the same for the single shared mb.start_match. Committing
        constructs (atomic group / assertion / possessive / recursion) truncate an
        inner KIND_SET_SOM, so their boundary records ALSO snapshot start_match
        (saved_start_match), like saved_mark. *)
let kind_set_som = 17

let width_set_som = 2
let width_ref_max2 = 6
let width_verb = 5
let width_vreverse = 6

(* KIND_ALT grows by one slot in chunk H: [handler; eptr; rdepth; then_end;
   KIND_ALT] (width 5). [then_end] is the THEN scope boundary copied from
   Ir.alt_then_end at push time (fast-design.md §3); the hot NOMATCH backtrack
   still reads handler/eptr/rdepth at slots 0/1/2 unchanged. *)
let width_alt = 5
let width_cap = 4
let width_rep_max = 5
let width_rep_min = 5
let width_cont = 4
let width_gstart = 3
let width_capstart = 3
let width_ref_min = 5
let width_ref_max = 6

(* Widest record — the runner reserves this much headroom on a push. *)
let max_record_width = 6

(* Initial capacity in ints (~85 records); grows geometrically. *)
let initial_ints = 256

(* frames.ml:290 precedent — drop arenas larger than ~1 MiB (131072 8-byte
   words) on release so a pathological exec cannot pin them process-wide. *)
let scratch_max_retained_ints = 131072

type t = { mutable data : int array }

(* The ONE module-level scratch instance, retained across execs. Its [data]
   is re-pointed by [grow]; [Runner] reuses this same [t] (through its cached
   match block) whenever the [busy] flag is free. *)
let scratch : t = { data = Array.make initial_ints 0 }

(* This stack's OWN busy flag (port-conventions.md §9 / frames.ml:283); NOT
   Frames' scratch flag. *)
let busy = Atomic.make false

(* Test-only view of the busy flag (never true between execs). *)
let is_busy () : bool = Atomic.get busy

(* Acquire the scratch slot for one exec: the compare-and-set is the whole
   acquire (frames.ml:352). Returns true iff this exec owns the slot and may
   use/retain [scratch]; a busy slot makes the caller allocate a fresh [t]. *)
let try_acquire () : bool = Atomic.compare_and_set busy false true

(* A fresh, unshared stack for the busy fallback path (never written back). *)
let fresh () : t = { data = Array.make initial_ints 0 }

(* Release the scratch slot at exec exit (only the owner calls this): apply
   the retention cap to [scratch], then publish the busy clear (its release
   store synchronizes with the next acquire's compare-and-set). *)
let release () : unit =
  if Array.length scratch.data > scratch_max_retained_ints then
    scratch.data <- Array.make initial_ints 0;
  Atomic.set busy false

(* Cold path: grow [t.data] geometrically so it holds at least [need] ints,
   preserving the existing contents. Called from the runner's ALT arm only
   when a push would overflow. *)
let grow (t : t) (need : int) : unit =
  let cap = ref (Array.length t.data) in
  if !cap < initial_ints then cap := initial_ints;
  while need > !cap do
    cap := !cap * 2
  done;
  let nd = Array.make !cap 0 in
  Array.blit t.data 0 nd 0 (Array.length t.data);
  t.data <- nd
