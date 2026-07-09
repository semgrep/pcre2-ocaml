(* Backtracking-frame arena for the pure-OCaml PCRE2 10.44 port
   (8-bit library).

   Ported from vendor/pcre2/src/pcre2_match.c:100-830 (frame protocol:
   group-frame types, field short-names, MATCH_RECURSE vector growth and
   frame copying) together with the heapframe structure itself
   (pcre2_intmodedep.h:783-859) and the frame-size / initial-vector
   computation in pcre2_match() (pcre2_match.c:7019-7083).

   The C keeps backtracking frames in one heap vector of variable-sized
   heapframe structs (pcre2_match.c:485-510). Here the vector is ONE flat
   [int array]: a frame is [frame_size_ints] consecutive slots — a fixed
   header transcribing every heapframe field, then the ovector region of
   2 * top_bracket slots. C pointers/offsets held in frame fields become
   ints:

     - ecode                  -> offset into re.code (the compiled pattern)
     - eptr/start_match/...   -> offsets into the subject
     - mark                   -> offset into re.code of the mark name
                                 (frames are all-int); -1 = NULL
     - PCRE2_UNSET (~(PCRE2_SIZE)0, i.e. all-one bits) -> -1 (all-one bits
       of an OCaml int). DEVIATION: comparisons the C performs unsigned
       (where PCRE2_UNSET is the largest value) must test -1 explicitly in
       the interpreter.

   Heap-limit accounting is done in SIMULATED C BYTES so that
   PCRE2_ERROR_HEAPLIMIT fires at C-identical points: [frame_size_bytes]
   and [heapframes_size] reproduce the C's frame_size and
   match_data->heapframes_size for the 8-bit library on an LP64 platform
   (8-byte pointers and PCRE2_SIZE — the same platform as the conformance
   C oracle). Byte-accounting table for the heapframe struct
   (pcre2_intmodedep.h:790-842), giving each field's C type, simulated
   byte offset, and arena slot:

     C field             C type         bytes  C offset  arena slot
     ecode               PCRE2_SPTR       8       0         0
     temp_sptr[0]        PCRE2_SPTR       8       8         1
     temp_sptr[1]        PCRE2_SPTR       8      16         2
     length              PCRE2_SIZE       8      24         3
     back_frame          PCRE2_SIZE       8      32         4
     temp_size           PCRE2_SIZE       8      40         5
     rdepth              uint32_t         4      48         6
     group_frame_type    uint32_t         4      52         7
     temp_32[0..3]       uint32_t[4]     16      56         8..11
     return_id           uint8_t          1      72        12
     op                  uint8_t          1      73        13
     occu[0..5]          PCRE2_UCHAR[6]   6      74        14..19
     eptr                PCRE2_SPTR       8      80        20   MUST BE
                                                                FIRST of
                                                                copy region
     start_match         PCRE2_SPTR       8      88        21
     mark                PCRE2_SPTR       8      96        22
     recurse_last_used   PCRE2_SPTR       8     104        23
     current_recurse     uint32_t         4     112        24
     capture_last        uint32_t         4     116        25
     last_group_offset   PCRE2_SIZE       8     120        26
     offset_top          PCRE2_SIZE       8     128        27
     ovector[]           PCRE2_SIZE[]  8 each   136        28..

   There is no padding before eptr: return_id + op + occu[6] make the 8
   bytes 72..79 (the "odd multiple of 16 bits" note at
   pcre2_intmodedep.h:806-814 holds naturally for 8-bit code units), and
   sizeof(heapframe) is a multiple of sizeof(PCRE2_SIZE)
   (pcre2_intmodedep.h:844-848).

   DEVIATION (documented per field below): frame-relative quantities the C
   stores in bytes (back_frame; last_group_offset as a byte offset from
   match_data->heapframes) are stored in FRAME units here (frame count /
   frame index) — the int arena has no byte addressing. The two encodings
   are isomorphic by (value_in_bytes = value_in_frames * frame_size_bytes);
   every C use is a multiple of frame_size.

   No exceptions cross this module's operations: growth failure returns
   PCRE2_ERROR_HEAPLIMIT / PCRE2_ERROR_NOMEMORY as a value
   (port-conventions §6). Safe array ops only; the interpreter chunk adds
   Array.unsafe_* under proven bounds. *)

(* ---------- Frame protocol constants ---------- *)

(* pcre2_match.c:69 — RECURSE_UNSET, bigger than max group number; the
   current_recurse slot value when not in a pattern recursion. *)
let recurse_unset = 0xffffffff

(* pcre2.h.generic:482 — PCRE2_UNSET is ~(PCRE2_SIZE)0. In the all-int
   arena it is -1 (all-one bits): unset ovector entries, NULL mark, unset
   last_group_offset. See the DEVIATION note above on unsigned compares. *)
let unset = -1

(* pcre2_match.c:105-114 — group frame type values. Zero means the frame
   is not a group frame. The lower 16 bits are used for data (e.g. the
   capture number). Group frames are used for most groups so that
   information about the start is easily available at the end without
   having to scan back through intermediate frames (backtrack points). *)
let gf_capture = 0x00010000
let gf_nocapture = 0x00020000
let gf_condassert = 0x00030000
let gf_recurse = 0x00040000

(* pcre2_match.c:116-119 — masks for the identity and data parts of the
   group frame type. *)
let gf_idmask (a : int) : int = a land 0xffff0000
let gf_datamask (a : int) : int = a land 0x0000ffff

(* ---------- Frame slot layout ---------- *)

(* One named slot per heapframe field (pcre2_intmodedep.h:790-842; the
   Fxxx short-names are pcre2_match.c:171-194). Slots [slot_ecode] ..
   [slot_occu + 5] are the frame-local part that is NOT copied to new
   frames; slots [slot_eptr] .. frame end are copied by [push]
   (pcre2_intmodedep.h:792-793, 826-831). *)

(* PCRE2_SPTR ecode — the current position in the pattern (code offset). *)
let slot_ecode = 0

(* PCRE2_SPTR temp_sptr[2] — used for short-term PCRE2_SPTR values. *)
let slot_temp_sptr_0 = 1
let slot_temp_sptr_1 = 2

(* PCRE2_SIZE length — used for character, string, or code lengths. *)
let slot_length = 3

(* PCRE2_SIZE back_frame — amount to subtract on RRETURN
   (pcre2_match.c:6472 backtracks by byte arithmetic: F = F - Fback_frame).
   DEVIATION: stored in FRAME units, not bytes — the default "go back one
   frame" (pcre2_match.c:761: Fback_frame = frame_size) becomes 1, and the
   C's byte distance F - P assigned at pcre2_match.c:5946 and 6024 becomes
   the frame-index difference f - p. *)
let slot_back_frame = 4

(* PCRE2_SIZE temp_size — used for short-term PCRE2_SIZE values. *)
let slot_temp_size = 5

(* uint32_t rdepth — function "recursion" depth within pcre2_match(); the
   frame's index. Set by [push] (pcre2_match.c:753). *)
let slot_rdepth = 6

(* uint32_t group_frame_type — type information for group frames (the GF_*
   values above). *)
let slot_group_frame_type = 7

(* uint32_t temp_32[4] — used for short-term 32-bit or BOOL values. *)
let slot_temp_32_0 = 8
let slot_temp_32_1 = 9
let slot_temp_32_2 = 10
let slot_temp_32_3 = 11

(* uint8_t return_id — where to go on in internal "return" (the RM.. label
   number saved by RMATCH, dispatched by RETURN_SWITCH,
   pcre2_match.c:550-563, 6462-6480). *)
let slot_return_id = 12

(* uint8_t op — processing opcode (pcre2_match.c:797). *)
let slot_op = 13

(* PCRE2_UCHAR occu[6] — used for other case code units: the UTF-8
   encoding of a character's other case, filled by ord2utf and compared
   byte-by-byte (pcre2_match.c:1293-1345). Six consecutive slots, one per
   C byte: slot_occu + 0 .. slot_occu + 5. *)
let slot_occu = 14

(* PCRE2_SPTR eptr — subject position; MUST BE FIRST of the copied region
   (pcre2_intmodedep.h:833). *)
let slot_eptr = 20

(* PCRE2_SPTR start_match — can be adjusted by \K. *)
let slot_start_match = 21

(* PCRE2_SPTR mark — most recent mark on the success path (int code
   offset; -1 = NULL). *)
let slot_mark = 22

(* PCRE2_SPTR recurse_last_used — last character used at time of pattern
   recursion. *)
let slot_recurse_last_used = 23

(* uint32_t current_recurse — group number of current (deepest) pattern
   recursion ([recurse_unset] when not recursing). *)
let slot_current_recurse = 24

(* uint32_t capture_last — most recent capture. *)
let slot_capture_last = 25

(* PCRE2_SIZE last_group_offset — saved offset to most recent group frame.
   DEVIATION: stored as a FRAME INDEX into the arena, -1 = PCRE2_UNSET; the
   C stores the byte offset of F from match_data->heapframes
   (pcre2_match.c:769) and turns it back into a frame pointer at e.g.
   pcre2_match.c:817. Isomorphic by index * frame_size_bytes. *)
let slot_last_group_offset = 26

(* PCRE2_SIZE offset_top — offset after highest capture. *)
let slot_offset_top = 27

(* PCRE2_SIZE ovector[] — must be last in the frame: 2 * top_bracket slots
   at slot_ovector .. frame end. Like the C (pcre2_match.c:7032-7033 sizes
   it from re->top_bracket), group 0 is NOT stored here — Fovector[0..1]
   belong to group 1 (OP_CLOSE at pcre2_match.c:822-826 uses
   (number << 1) - 2); the overall match offsets live in the match data,
   outside the frame (pcre2_match.c:285-295). *)
let slot_ovector = 28

(* Number of header slots before the ovector region. *)
let frame_header_ints = slot_ovector

(* ---------- Simulated C sizes (heap-limit accounting) ---------- *)

(* sizeof(PCRE2_SIZE) = sizeof(size_t) = 8 on LP64. *)
let sizeof_pcre2_size = 8

(* offsetof(heapframe, eptr) = 80: the byte-accounting table above. Used
   by the C for frame_copy_size (pcre2_match.c:642). *)
let offsetof_eptr = 80

(* offsetof(heapframe, ovector) = 136: the byte-accounting table above. *)
let offsetof_ovector = 136

(* pcre2_intmodedep.h:850-859 — HEAPFRAME_ALIGNMENT: the minimum alignment
   required for a heapframe, in bytes; 8 on LP64 (the largest member
   alignment: pointers and PCRE2_SIZE). *)
let heapframe_alignment = 8

(* pcre2_match.c:7032-7034 — frame_size, the total simulated size of each
   frame in bytes: (offsetof(heapframe, ovector) +
   re->top_bracket * 2 * sizeof(PCRE2_SIZE) + HEAPFRAME_ALIGNMENT - 1) &
   ~(HEAPFRAME_ALIGNMENT - 1). The rounding is a no-op for these operand
   values (both terms are multiples of 8) but is transcribed to keep the
   accounting formula literal. *)
let frame_size_bytes_for ~(top_bracket : int) : int =
  (offsetof_ovector
  + (top_bracket * 2 * sizeof_pcre2_size)
  + heapframe_alignment - 1)
  land lnot (heapframe_alignment - 1)

(* ---------- The arena ---------- *)

(* The frames vector state the C splits between pcre2_match_data
   (heapframes / heapframes_size, pcre2_intmodedep.h:661-662) and
   pcre2_match() locals (frame_size, mb->heap_limit).
   DEVIATION: there is no match_data object at the OCaml seam, so the C's
   caller-owned vector cache (match_data->heapframes, kept across
   pcre2_match calls and reused when big enough, pcre2_match.c:7062-7077)
   becomes the single module-level scratch slot below ([scratch_arena] /
   [scratch_busy]): [create ~use_scratch:true] acquires it, [release]
   hands the (possibly grown) arena back. [heapframes_size] is
   nevertheless ALWAYS the freshly computed initial size — the C keeps
   the retained vector's larger heapframes_size (see the DEVIATION note
   in [create]) — so heap-limit trip points are history-independent. *)
type t = {
  mutable frames : int array;
      (* match_data->heapframes — the flat arena; frame f occupies slots
         f * frame_size_ints .. (f + 1) * frame_size_ints - 1 *)
  mutable heapframes_size : int;
      (* match_data->heapframes_size — SIMULATED allocated size in C
         bytes; all heap-limit checks run on this *)
  frame_size_ints : int; (* arena slots per frame *)
  frame_size_bytes : int; (* pcre2_match.c:7032 frame_size, simulated *)
  heap_limit : int;
      (* mb->heap_limit, in KiB — already resolved by the caller as
         min(mcontext->heap_limit, re->limit_heap), pcre2_match.c:7039 *)
  holds_scratch : bool;
      (* this arena holds the module scratch slot: [release] must write
         [frames] back and clear [scratch_busy] when the match returns *)
}

(* Base slot index of frame [f]. *)
let base (a : t) (f : int) : int = f * a.frame_size_ints

(* pcre2_match.c:7062-7077 — "If an existing frame vector in the
   match_data block is large enough, we can use it. Otherwise, free any
   pre-existing vector and get a new one." The C's cache is the
   caller-owned match_data, reused across pcre2_match calls; this port
   has no match_data object at the seam, so the cache is one module-level
   scratch slot: the arena of the previous match, written back by
   [release]. [scratch_busy] keeps the slot single-owner under
   reentrancy/systhreads: [create ~use_scratch:true] takes it only via an
   atomic test-and-set, and a busy slot (a match already running) falls
   back to a fresh allocation that is NOT written back. Reused memory is
   NOT re-zeroed — exactly the C, which reuses the malloc'd vector as is:
   the only initialization a match relies on is frame 0's ovector unset
   fill (pcre2_match.c:7079-7083, in [create]) plus the frame-0 field
   setup at match() entry (pcre2_match.c:649-656, in the interpreter);
   every other slot is write-before-read under the frame protocol. *)
let scratch_arena : int array ref = ref [||]
let scratch_busy : bool Atomic.t = Atomic.make false

(* DEVIATION: the C never caps the cached vector — its lifetime is
   caller-controlled (it lives until pcre2_match_data_free). Ours is a
   process-global scratch, so an arena grown by one pathological match
   must not stay pinned forever: [release] drops arenas larger than
   ~1 MiB (131072 8-byte words). *)
let scratch_max_retained_ints = 131072

(* pcre2_match.c:7048-7060 + 7079-7083 — set the initial frame vector size
   to ensure that there are at least 10 available frames, but enforce a
   minimum of START_FRAMES_SIZE. If this is greater than the heap limit,
   get as large a vector as possible; if even one frame does not fit,
   PCRE2_ERROR_HEAPLIMIT (as a value). The first frame's ovector region is
   marked all-unset (the C memsets it to 0xff bytes = PCRE2_UNSET; here
   each slot becomes -1) so that copying it to new frames never reads
   uninitialized captures. On the fresh path all other slots start 0; the
   interpreter initializes frame 0's live fields exactly as
   pcre2_match.c:649-656. [use_scratch] selects the scratch-slot reuse
   path (pcre2_match.c:7062-7077) — only the real match driver passes
   true; the module-initialization asserts here and the test-only
   entries stay on the fresh (zeroed) path. *)
let create ~(use_scratch : bool) ~(top_bracket : int) ~(heap_limit : int) :
    (t, int) result =
  let frame_size_bytes = frame_size_bytes_for ~top_bracket in
  let frame_size_ints = frame_header_ints + (2 * top_bracket) in
  (* pcre2_match.c:7053-7054 *)
  let heapframes_size = frame_size_bytes * 10 in
  let heapframes_size =
    if heapframes_size < Limits.start_frames_size then Limits.start_frames_size
    else heapframes_size
  in
  (* pcre2_match.c:7055-7060 *)
  let heapframes_size =
    if heapframes_size / 1024 > heap_limit then
      let max_size = 1024 * heap_limit in
      if max_size < frame_size_bytes then Errors.error_heaplimit else max_size
    else heapframes_size
  in
  if heapframes_size < 0 then Error heapframes_size (* error_heaplimit *)
  else
    (* Simulated capacity in whole frames: frame f is usable iff its end
       lies strictly below frames_top (see [push]'s >= test), so
       floor(heapframes_size / frame_size_bytes) frames always suffice. *)
    let capacity_frames = heapframes_size / frame_size_bytes in
    let needed = capacity_frames * frame_size_ints in
    (* pcre2_match.c:7062-7077 — reuse the scratch arena when it is large
       enough, without re-zeroing (see the scratch-slot comment above);
       otherwise get a new one ([release]'s write-back then replaces the
       slot's old, too-small contents — the C's free + malloc at
       7067-7070). The compare_and_set is the entire acquire: no separate
       test/set to interleave, so two concurrent matches can never both
       hold the slot; the loser allocates fresh. DEVIATION: on reuse the
       C keeps the retained vector's heapframes_size — the `<` test at
       7065 leaves the larger cached value in match_data, giving later
       matches extra pre-grow headroom that depends on call history. Here
       [heapframes_size] stays the freshly computed value, so grow points
       and heap-limit errors are identical to a first match on a fresh
       match_data (the behavior the conformance baselines record).
       DEVIATION (OOM while holding the flag): the undersized-replacement
       Array.make below — and any [grow] during the match — allocates
       while this match holds the flag; if the runtime raises
       Out_of_memory there, no [release] runs and the flag sticks true,
       permanently degrading every later create to the fresh-alloc path.
       Accepted: the degraded mode is behavior-neutral (identical to
       per-exec allocation before this cache existed, i.e. to a
       permanently busy slot), and OOM is already effectively fatal to
       the differential harness (the C oracle stubs abort on OOM). *)
    let holds_scratch =
      use_scratch && Atomic.compare_and_set scratch_busy false true
    in
    let frames =
      if holds_scratch && Array.length !scratch_arena >= needed then
        !scratch_arena
      else Array.make needed 0
    in
    (* pcre2_match.c:7079-7083 — mark every capture in frame 0 unset. *)
    for i = slot_ovector to frame_size_ints - 1 do
      frames.(i) <- unset
    done;
    Ok
      {
        frames;
        heapframes_size;
        frame_size_ints;
        frame_size_bytes;
        heap_limit;
        holds_scratch;
      }

(* Hand the arena back to the scratch slot at match exit — the C's
   counterpart is doing nothing: the vector simply stays in match_data
   for pcre2_match.c:7062-7077 to reuse on the next call. [a.frames] is
   written back rather than the array [create] handed out: [grow]
   re-points it, so growth is retained for the next match exactly like
   the C's grown vector staying in match_data->heapframes
   (pcre2_match.c:701-711). No-op for arenas that never held the slot
   (fresh-path creates and the busy fallback). This site is reached only
   on normal (value-encoded) match returns: an Out_of_memory raised while
   the flag is held — e.g. from [grow]'s Array.make mid-match — skips it
   and permanently degrades the process to fresh-alloc mode, which is
   accepted as behavior-neutral (see the DEVIATION note at the acquire in
   [create]). *)
let release (a : t) : unit =
  if a.holds_scratch then (
    if Array.length a.frames <= scratch_max_retained_ints then
      scratch_arena := a.frames
    else
      (* DEVIATION (retention cap): see [scratch_max_retained_ints]. *)
      scratch_arena := [||];
    (* The plain write above is published by this release store: the next
       acquire's compare_and_set synchronizes with it. *)
    Atomic.set scratch_busy false)

(* pcre2_match.c:668-712 — the frames vector is full: get a new one,
   doubling the size, but constrained by the heap limit (which is in KiB).
   [n] is the index of the frame about to be created, so the C's usedsize
   (N minus match_data->heapframes in bytes, pcre2_match.c:672) is
   n * frame_size_bytes. Returns 0 on success, else a negative PCRE2 error
   code as a value. The C's negative early returns thread through the
   [newsize] ints below (real sizes are positive, so negative = error). *)
let grow (a : t) ~(n : int) : int =
  let usedsize = n * a.frame_size_bytes in
  (* pcre2_match.c:674-681. DEVIATION: PCRE2_SIZE_MAX (2^64 - 1) exceeds
     OCaml's int; max_int stands in. Unreachable either way: heap_limit is
     a uint32 KiB count, bounding newsize far below both. *)
  let newsize =
    if a.heapframes_size >= max_int / 2 then
      if Int.equal a.heapframes_size (max_int - 1) then Errors.error_nomemory
      else max_int - 1
    else a.heapframes_size * 2
  in
  if newsize < 0 then newsize
  else
    (* pcre2_match.c:683-695 *)
    let newsize =
      if newsize / 1024 >= a.heap_limit then
        let old_size = a.heapframes_size / 1024 in
        if a.heap_limit <= old_size then Errors.error_heaplimit
        else
          let max_delta = 1024 * (a.heap_limit - old_size) in
          let over_bytes = a.heapframes_size mod 1024 in
          let max_delta =
            if not (Int.equal over_bytes 0) then max_delta - (1024 - over_bytes)
            else max_delta
          in
          a.heapframes_size + max_delta
      else newsize
    in
    if newsize < 0 then newsize
    else if newsize - usedsize < a.frame_size_bytes then
      (* pcre2_match.c:697-700 — with a heap limit set, the permitted
         additional size may not be enough for another frame. *)
      Errors.error_heaplimit
    else
      (* pcre2_match.c:701-711 — allocate the doubled vector, copy the
         used prefix, install it. DEVIATION: the C returns
         PCRE2_ERROR_NOMEMORY on malloc failure; Array.make has no NULL
         result (a genuine out-of-memory raises the runtime's
         Out_of_memory). The C's N/F pointer rebasing (705-706) is a no-op
         for frame indices. *)
      let new_frames =
        Array.make (newsize / a.frame_size_bytes * a.frame_size_ints) 0
      in
      (* pcre2_match.c:703 — memcpy(new, match_data->heapframes,
         usedsize): copy the used prefix. Manual int loop rather than
         Array.blit to skip the per-element caml_modify barrier (§8);
         bounds-checked accesses — this path is cold. *)
      for i = 0 to (n * a.frame_size_ints) - 1 do
        new_frames.(i) <- a.frames.(i)
      done;
      a.frames <- new_frames;
      a.heapframes_size <- newsize;
      0

(* pcre2_match.c:662-712 + 745-754 — MATCH_RECURSE: set up a new
   backtracking frame N just above the current frame [f], growing the
   vector first if N's end would reach the top of the vector (the C's
   `N + frame_size >= frames_top` byte test at 667-668, done here in
   simulated bytes). Copy boundary (pcre2_match.c:639-642, 749-751): slots
   [slot_eptr] .. frame end (eptr, start_match, mark, recurse_last_used,
   current_recurse, capture_last, last_group_offset, offset_top, ovector)
   are copied from F; the header slots ecode .. occu are NOT copied (they
   keep their previous arena contents, exactly as the C leaves that memory
   — the interpreter's NEW_FRAME code, pcre2_match.c:758-773, writes
   group_frame_type/ecode/back_frame before anything reads them, and the
   temp/occu/op/return_id slots are write-before-read scratch). The one
   reset field is rdepth: N->rdepth = F->rdepth + 1 (pcre2_match.c:753).

   Returns the new frame's index (the C's F = N), or a negative PCRE2
   error code as a value. Allocation-free except when growth occurs. *)
let push (a : t) (f : int) : int =
  let n = f + 1 in
  (* pcre2_match.c:667-668 *)
  let rc =
    if (n + 1) * a.frame_size_bytes >= a.heapframes_size then grow a ~n else 0
  in
  if rc < 0 then rc
  else
    (* pcre2_match.c:749-751 — memcpy of frame_copy_size bytes from F's
       eptr field to N's eptr field, where frame_copy_size = frame_size -
       offsetof(heapframe, eptr) (:642). A manual int-copy loop, NOT
       Array.blit: blit into a major-heap array runs the caml_modify
       write barrier per element even for immediates, and this copy is
       the matcher's hottest write site (§8; measured 20-35% of hot
       benches). safe: both spans lie inside frames f and n = f + 1, and
       (n + 1) * frame_size_ints <= Array.length a.frames — if the guard
       above was false, (n + 1) * frame_size_bytes < heapframes_size, so
       n + 1 <= heapframes_size / frame_size_bytes; if it was true, grow
       succeeded, and its `newsize - usedsize < frame_size_bytes` check
       guarantees (n + 1) * frame_size_bytes <= the new heapframes_size,
       so again n + 1 <= heapframes_size / frame_size_bytes. In every
       arena state Array.length a.frames >= (heapframes_size /
       frame_size_bytes) * frame_size_ints: create sizes fresh arenas
       exactly so and reuses only longer ones; grow allocates exactly so.
       f >= 0 by the caller contract (an existing frame index: 0 or a
       value previously returned by push). *)
    let fr = a.frames in
    let src = (f * a.frame_size_ints) + slot_eptr in
    let dst = (n * a.frame_size_ints) + slot_eptr in
    let len = a.frame_size_ints - slot_eptr in
    for i = 0 to len - 1 do
      Array.unsafe_set fr (dst + i) (Array.unsafe_get fr (src + i))
    done;
    (* pcre2_match.c:753 — N->rdepth = Frdepth + 1 *)
    a.frames.((n * a.frame_size_ints) + slot_rdepth) <-
      a.frames.((f * a.frame_size_ints) + slot_rdepth) + 1;
    n

(* ---------- Inline sanity checks (module-initialization asserts) ---------- *)

(* Slot offsets and simulated sizes are load-bearing for the interpreter
   chunk and for heap-limit parity: pin them. *)
let () =
  (* Header slot layout: one slot per field in struct order. *)
  assert (Int.equal slot_ecode 0);
  assert (Int.equal slot_temp_sptr_0 1);
  assert (Int.equal slot_temp_sptr_1 2);
  assert (Int.equal slot_length 3);
  assert (Int.equal slot_back_frame 4);
  assert (Int.equal slot_temp_size 5);
  assert (Int.equal slot_rdepth 6);
  assert (Int.equal slot_group_frame_type 7);
  assert (Int.equal slot_temp_32_0 8);
  assert (Int.equal slot_temp_32_3 11);
  assert (Int.equal slot_return_id 12);
  assert (Int.equal slot_op 13);
  assert (Int.equal slot_occu 14);
  (* occu occupies 6 slots: 14..19, then the copy region starts. *)
  assert (Int.equal slot_eptr (slot_occu + 6));
  assert (Int.equal slot_ovector 28);
  assert (Int.equal frame_header_ints 28);
  (* Simulated C offsets: eptr at 80, ovector at 136; the copied region
     eptr..offset_top is 7 * 8 = 56 bytes = ovector - eptr. *)
  assert (Int.equal offsetof_eptr 80);
  assert (Int.equal offsetof_ovector 136);
  assert (Int.equal (offsetof_ovector - offsetof_eptr) 56);
  (* frame_size (pcre2_match.c:7032-7034) = 136 + 16 * top_bracket. *)
  assert (Int.equal (frame_size_bytes_for ~top_bracket:0) 136);
  assert (Int.equal (frame_size_bytes_for ~top_bracket:1) 152);
  assert (Int.equal (frame_size_bytes_for ~top_bracket:2) 168);
  assert (Int.equal (frame_size_bytes_for ~top_bracket:65535) 1048696);
  (* GF constants (pcre2_match.c:105-119). *)
  assert (Int.equal (gf_idmask (gf_capture lor 3)) gf_capture);
  assert (Int.equal (gf_datamask (gf_capture lor 3)) 3);
  assert (Int.equal (gf_idmask (gf_recurse lor 65535)) gf_recurse);
  assert (Int.equal recurse_unset 0xffffffff)

(* create: default-limit sizing (pcre2_match.c:7053-7054), frame-0 ovector
   all-unset (7079-7083), header zeros. *)
let () =
  (match
     create ~use_scratch:false ~top_bracket:2 ~heap_limit:Limits.heap_limit
   with
  | Ok a ->
      assert (Int.equal a.frame_size_bytes 168);
      assert (Int.equal a.frame_size_ints 32);
      (* 168 * 10 = 1680 < START_FRAMES_SIZE = 20480. *)
      assert (Int.equal a.heapframes_size 20480);
      (* capacity = floor(20480 / 168) = 121 frames. *)
      assert (Int.equal (Array.length a.frames) (121 * 32));
      (* Frame 0: ovector slots -1, header slots 0. *)
      for i = 0 to slot_ovector - 1 do
        assert (Int.equal a.frames.(i) 0)
      done;
      for i = slot_ovector to a.frame_size_ints - 1 do
        assert (Int.equal a.frames.(i) unset)
      done
  | Error _ -> assert false);
  (* At least 10 frames when the frame is huge: top_bracket = 200 gives
     frame_size 3336, initial size 33360 > START_FRAMES_SIZE. *)
  (match
     create ~use_scratch:false ~top_bracket:200 ~heap_limit:Limits.heap_limit
   with
  | Ok a ->
      assert (Int.equal a.frame_size_bytes 3336);
      assert (Int.equal a.heapframes_size 33360);
      assert (Int.equal (Array.length a.frames) (10 * a.frame_size_ints))
  | Error _ -> assert false);
  (* create-time clamp (pcre2_match.c:7055-7060): 1 KiB limit clamps
     20480 -> 1024; a 0 KiB limit cannot hold one frame -> HEAPLIMIT. *)
  (match create ~use_scratch:false ~top_bracket:0 ~heap_limit:1 with
  | Ok a ->
      assert (Int.equal a.heapframes_size 1024);
      assert (Int.equal (Array.length a.frames) (7 * 28))
  | Error _ -> assert false);
  match create ~use_scratch:false ~top_bracket:0 ~heap_limit:0 with
  | Error e -> assert (Int.equal e Errors.error_heaplimit)
  | Ok _ -> assert false

(* push: copy boundary and rdepth (pcre2_match.c:745-754), frame
   isolation, and the RMATCH copy carrying frame-0 captures upward. *)
let () =
  match
    create ~use_scratch:false ~top_bracket:2 ~heap_limit:Limits.heap_limit
  with
  | Error _ -> assert false
  | Ok a ->
      (* Populate frame 0: header scratch + copied fields + captures. *)
      a.frames.(slot_ecode) <- 111;
      a.frames.(slot_temp_sptr_0) <- 222;
      a.frames.(slot_op) <- 33;
      a.frames.(slot_eptr) <- 5;
      a.frames.(slot_start_match) <- 3;
      a.frames.(slot_mark) <- unset;
      a.frames.(slot_recurse_last_used) <- 9;
      a.frames.(slot_current_recurse) <- recurse_unset;
      a.frames.(slot_capture_last) <- 1;
      a.frames.(slot_last_group_offset) <- unset;
      a.frames.(slot_offset_top) <- 2;
      a.frames.(slot_ovector) <- 10;
      a.frames.(slot_ovector + 1) <- 11;
      let n = push a 0 in
      assert (Int.equal n 1);
      let b1 = base a 1 in
      (* Copied region: eptr .. ovector end. *)
      assert (Int.equal a.frames.(b1 + slot_eptr) 5);
      assert (Int.equal a.frames.(b1 + slot_start_match) 3);
      assert (Int.equal a.frames.(b1 + slot_mark) unset);
      assert (Int.equal a.frames.(b1 + slot_recurse_last_used) 9);
      assert (Int.equal a.frames.(b1 + slot_current_recurse) recurse_unset);
      assert (Int.equal a.frames.(b1 + slot_capture_last) 1);
      assert (Int.equal a.frames.(b1 + slot_last_group_offset) unset);
      assert (Int.equal a.frames.(b1 + slot_offset_top) 2);
      assert (Int.equal a.frames.(b1 + slot_ovector) 10);
      assert (Int.equal a.frames.(b1 + slot_ovector + 1) 11);
      assert (Int.equal a.frames.(b1 + slot_ovector + 2) unset);
      assert (Int.equal a.frames.(b1 + slot_ovector + 3) unset);
      (* Header slots are NOT copied. *)
      assert (Int.equal a.frames.(b1 + slot_ecode) 0);
      assert (Int.equal a.frames.(b1 + slot_temp_sptr_0) 0);
      assert (Int.equal a.frames.(b1 + slot_op) 0);
      (* rdepth = previous + 1. *)
      assert (Int.equal a.frames.(b1 + slot_rdepth) 1);
      (* Frame isolation: writing N's ovector leaves F intact. *)
      a.frames.(b1 + slot_ovector) <- 99;
      assert (Int.equal a.frames.(slot_ovector) 10);
      (* Chained push: rdepth increments per frame. *)
      let n2 = push a 1 in
      assert (Int.equal n2 2);
      assert (Int.equal a.frames.(base a 2 + slot_rdepth) 2);
      assert (Int.equal a.frames.(base a 2 + slot_ovector) 99)

(* Heap-limit accounting parity. Worked against pcre2_match.c:662-712 by
   hand for frame_size 136 (top_bracket 0):

   - heap_limit 20 KiB: initial vector 20480 bytes = frames 0..149 usable
     (frame 150 would end at 20536 >= 20480 -> grow; doubling to 40960
     trips 40960/1024 = 40 >= 20 with heap_limit <= old_size 20 ->
     HEAPLIMIT). No growth ever succeeds.
   - heap_limit 21 KiB: same trigger at frame 150, but now the doubling is
     clamped to old_size 20 + 1 KiB -> newsize 21504; frames 0..157 usable
     (frame 158 would end at 21624 >= 21504 -> grow; 21504/1024 = 21 and
     heap_limit <= 21 -> HEAPLIMIT). *)
let () =
  (match create ~use_scratch:false ~top_bracket:0 ~heap_limit:20 with
  | Error _ -> assert false
  | Ok a ->
      let f = ref 0 in
      let rc = ref 0 in
      while !rc >= 0 && !f < 1000 do
        rc := push a !f;
        if !rc >= 0 then f := !rc
      done;
      assert (Int.equal !f 149);
      assert (Int.equal !rc Errors.error_heaplimit);
      assert (Int.equal a.heapframes_size 20480));
  (match create ~use_scratch:false ~top_bracket:0 ~heap_limit:21 with
  | Error _ -> assert false
  | Ok a ->
      a.frames.(slot_eptr) <- 77 (* survives growth + copies to every N *);
      let f = ref 0 in
      let rc = ref 0 in
      while !rc >= 0 && !f < 1000 do
        rc := push a !f;
        if !rc >= 0 then f := !rc
      done;
      assert (Int.equal !f 157);
      assert (Int.equal !rc Errors.error_heaplimit);
      (* The clamped growth landed exactly on 21 KiB. *)
      assert (Int.equal a.heapframes_size 21504);
      assert (Int.equal (Array.length a.frames) (21504 / 136 * 28));
      (* Data survived the reallocation, at both ends of the chain. *)
      assert (Int.equal a.frames.(slot_eptr) 77);
      assert (Int.equal a.frames.(base a 157 + slot_eptr) 77);
      assert (Int.equal a.frames.(base a 157 + slot_rdepth) 157));
  (* The over_bytes adjustment (pcre2_match.c:691-692): initial size 33360
     (top_bracket 200, frame 3336 bytes) is not a whole KiB (over 592);
     with heap_limit 40, growth at frame 9 clamps 66720 down to
     33360 + 1024*(40-32) - (1024-592) = 41120. *)
  match create ~use_scratch:false ~top_bracket:200 ~heap_limit:40 with
  | Error _ -> assert false
  | Ok a ->
      a.frames.(slot_ovector) <- 12345;
      let f = ref 0 in
      for _ = 1 to 9 do
        let n = push a !f in
        assert (n >= 0);
        f := n
      done;
      assert (Int.equal !f 9);
      assert (Int.equal a.heapframes_size 41120);
      (* Used prefix (including frame 0's captures) copied by grow. *)
      assert (Int.equal a.frames.(slot_ovector) 12345);
      assert (Int.equal a.frames.(base a 9 + slot_ovector) 12345)

(* Scratch-slot protocol (pcre2_match.c:7062-7077 reuse + the busy flag
   and retention-cap DEVIATIONs): acquire, busy fallback, write-back,
   dirty reuse with the frame-0 ovector re-fill, undersized replacement,
   cap drop. Runs last and leaves the slot exactly as module init found
   it (empty, not busy) — the fresh-path creates above never touch it. *)
let () =
  assert (Int.equal (Array.length !scratch_arena) 0);
  assert (not (Atomic.get scratch_busy));
  (match
     create ~use_scratch:true ~top_bracket:2 ~heap_limit:Limits.heap_limit
   with
  | Error _ -> assert false
  | Ok a ->
      (* Empty slot: fresh (zeroed) arena, but the slot is now held. *)
      assert a.holds_scratch;
      assert (Atomic.get scratch_busy);
      for i = 0 to slot_ovector - 1 do
        assert (Int.equal a.frames.(i) 0)
      done;
      (* Busy slot: a nested/concurrent match falls back to a fresh
         arena and its release is a no-op. *)
      (match
         create ~use_scratch:true ~top_bracket:2 ~heap_limit:Limits.heap_limit
       with
      | Error _ -> assert false
      | Ok b ->
          assert (not b.holds_scratch);
          assert (not (b.frames == a.frames));
          release b;
          assert (Atomic.get scratch_busy);
          assert (Int.equal (Array.length !scratch_arena) 0));
      a.frames.(slot_ecode) <- 424242 (* dirt: must survive reuse *);
      release a;
      assert (not (Atomic.get scratch_busy));
      assert (!scratch_arena == a.frames));
  (* Reuse: the same array comes back, NOT re-zeroed; only frame 0's
     ovector region is re-marked unset (pcre2_match.c:7079-7083). *)
  (match
     create ~use_scratch:true ~top_bracket:2 ~heap_limit:Limits.heap_limit
   with
  | Error _ -> assert false
  | Ok a ->
      assert a.holds_scratch;
      assert (a.frames == !scratch_arena);
      assert (Int.equal a.frames.(slot_ecode) 424242);
      for i = slot_ovector to a.frame_size_ints - 1 do
        assert (Int.equal a.frames.(i) unset)
      done;
      release a);
  (* Undersized slot: a bigger pattern gets a fresh arena, and the
     write-back replaces the slot's old contents (the C's free + malloc,
     pcre2_match.c:7064-7076). *)
  (match
     create ~use_scratch:true ~top_bracket:200 ~heap_limit:Limits.heap_limit
   with
  | Error _ -> assert false
  | Ok a ->
      assert a.holds_scratch;
      assert (not (a.frames == !scratch_arena));
      release a;
      assert (!scratch_arena == a.frames));
  (* Retention cap: an arena grown past [scratch_max_retained_ints]
     (simulated here by re-pointing [frames] like [grow] does) is dropped
     at release instead of staying pinned in the slot. *)
  (match
     create ~use_scratch:true ~top_bracket:2 ~heap_limit:Limits.heap_limit
   with
  | Error _ -> assert false
  | Ok a ->
      assert a.holds_scratch;
      a.frames <- Array.make (scratch_max_retained_ints + 1) 0;
      release a;
      assert (not (Atomic.get scratch_busy));
      assert (Int.equal (Array.length !scratch_arena) 0));
  (* The slot is back to its pristine state. *)
  assert (Int.equal (Array.length !scratch_arena) 0);
  assert (not (Atomic.get scratch_busy))
