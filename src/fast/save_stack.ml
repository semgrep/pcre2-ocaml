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
let width_alt = 4
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
