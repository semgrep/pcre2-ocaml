(* Fast-engine backtracking save stack (M11 chunk C2). Engine-native code
   with no direct PCRE2 counterpart, so per port-conventions.md §9 the design
   cites the design doc (fast-design.md §3) rather than a C source range.

   A save record is a fixed [record_width]-int slot pushed at a choice point
   ([Runner]'s ALT). The stack is ONE flat [int array]; the runner drives it
   by explicit index arithmetic ([sp] = number of ints in use), inlining the
   loads/stores at its ALT and backtrack sites — this module only owns the
   array, its growth, and the cross-exec scratch reuse.

   Save-record layout (fast-design.md §3), 3 ints, low index first:
     slot 0: handler   — IR index to resume at on backtrack (the ALT's
                         [next], i.e. the entry of the next alternative)
     slot 1: eptr      — subject position to restore
     slot 2: rdepth    — virtual frame depth to restore (§4 shadow accounting)

   Scratch reuse mirrors Frames' scratch pattern (frames.ml:282-403) with its
   OWN [busy] flag — it does NOT share Frames' scratch slot. A single
   module-level array is retained across execs and reused when free; a second
   concurrent exec (busy flag held) falls back to a fresh allocation that is
   never written back. A retention cap (like frames.ml's
   [scratch_max_retained_ints]) drops a pathologically grown array so it does
   not stay pinned forever. *)

(* fast-design.md §3 — one save record is [handler; eptr; rdepth]. *)
let record_width = 3

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
