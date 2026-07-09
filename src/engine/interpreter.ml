(* Interpreter dispatch skeleton for the pure-OCaml PCRE2 10.44 port
   (8-bit library).

   Ported from vendor/pcre2/src/pcre2_match.c: the match() function shell
   and first-frame setup (565-657), the RMATCH/RRETURN backtracking
   protocol as data (550-562; MATCH_RECURSE 662-754 via Frames.push;
   NEW_FRAME 758-783; RETURN_SWITCH 6462-6501), the main dispatch loop
   (790-798, 6445-6457), OP_END (876-940), the char family OP_CHAR/
   OP_CHARI/OP_NOT/OP_NOTI and the single-character repeat machinery
   REPEATCHAR/REPEATNOTCHAR with resume labels RM25-RM32 (992-1916,
   non-UTF/non-UCP arms; UTF -> M6, UCP -> M7), and the match_block
   structure (pcre2_intmodedep.h:864-906). Every other opcode that the C
   switch handles gets an exhaustive STUB arm returning the distinctive
   [error_unported] marker; the following M1-M8 chunks REPLACE arm bodies
   only.

   Control-flow translation (port-conventions §2, §6): the C's goto graph
   MATCH_RECURSE / NEW_FRAME / main-loop switch / RETURN_SWITCH becomes
   four mutually tail-recursive functions — [rmatch], [new_frame],
   [dispatch], [backtrack] — every cross-call annotated [@tailcall], so
   the matcher is ONE iterative loop over the Frames arena: depth = frame
   index, constant OCaml stack, no exception crosses the loop (limits and
   growth failures return negative PCRE2 codes as values).

   RMATCH(ra, rb) at a C opcode arm becomes
     (rmatch [@tailcall]) f ra rb group_frame_type
   as the arm's LAST expression; the C code that follows the macro (the
   L_RM##rb label) becomes the [rb] arm of [backtrack]'s return_id match.
   Both sides are stubs here; the chunk that owns each RMATCH site fills
   them in together. *)

(* pcre2_match.c:84-88 — non-error returns from and within the match()
   function. Error returns are the externally defined PCRE2_ERROR_xxx
   codes, which are all negative. *)
let match_match = 1
let match_nomatch = 0

(* pcre2_match.c:90-103 — special internal returns used in the match()
   function, sufficiently negative to avoid the external error codes. The
   five MATCH_COMMIT..MATCH_THEN must stay in sequence: range tests use
   MATCH_BACKTRACK_MIN/MAX (M5 verbs). *)
let match_accept = -999
let match_ketrpos = -998
let match_commit = -997
let match_prune = -996
let match_skip = -995
let match_skip_arg = -994
let match_then = -993
let match_backtrack_max = match_then
let match_backtrack_min = match_commit

(* pcre2_match.c:121-123 — repetition types. *)
let reptype_min = 0
let reptype_max = 1
let reptype_pos = 2

(* pcre2_match.c:125-126 — min and max values for the common repeats; a
   maximum of UINT32_MAX => infinity. OCaml ints are 63-bit, so the value
   is an ordinary (large) positive int: repeat counters stay bounded by
   the subject length, so the signed comparisons below agree with the C's
   unsigned ones. *)
let uint32_max = 0xFFFFFFFF

(* pcre2_match.c:128-133 *)
let rep_min = [| 0; 0; 1; 1; 0; 0; 0; 0; 0; 1; 0 |]

(* pcre2_match.c:135-140 *)
let rep_max =
  [|
    uint32_max;
    uint32_max;
    uint32_max;
    uint32_max;
    1;
    1;
    0;
    0;
    uint32_max;
    uint32_max;
    1;
  |]

(* pcre2_match.c:142-150 — repetition types - must include OP_CRPOSRANGE
   (not needed above). *)
let rep_typ =
  [|
    reptype_max;
    reptype_min;
    reptype_max;
    reptype_min;
    reptype_max;
    reptype_min;
    reptype_max;
    reptype_min;
    reptype_pos;
    reptype_pos;
    reptype_pos;
    reptype_pos;
  |]

(* pcre2_match.c:152-169 — numbers for RMATCH calls at backtracking
   points. When these change, [backtrack]'s return_id match must be
   updated in sync (the C's RETURN_SWITCH rule). RM100/RM101 exist in this
   port: SUPPORT_WIDE_CHARS is defined for the 8-bit library when Unicode
   is supported (pcre2_intmodedep.h:211-217). *)
let rm1 = 1

let rm2 = 2
and rm3 = 3
and rm4 = 4
and rm5 = 5
and rm6 = 6
and rm7 = 7
and rm8 = 8
and rm9 = 9
and rm10 = 10
and rm11 = 11
and rm12 = 12
and rm13 = 13
and rm14 = 14
and rm15 = 15
and rm16 = 16
and rm17 = 17
and rm18 = 18
and rm19 = 19
and rm20 = 20
and rm21 = 21
and rm22 = 22
and rm23 = 23
and rm24 = 24
and rm25 = 25
and rm26 = 26
and rm27 = 27
and rm28 = 28
and rm29 = 29
and rm30 = 30
and rm31 = 31
and rm32 = 32
and rm33 = 33
and rm34 = 34
and rm35 = 35
and rm36 = 36
and rm37 = 37

(* pcre2_match.c:160-162 — SUPPORT_WIDE_CHARS labels (OP_XCLASS repeats). *)
let rm100 = 100
and rm101 = 101

(* pcre2_match.c:164-169 — SUPPORT_UNICODE labels (UTF/UCP arms, M6/M7). *)
let rm200 = 200
and rm201 = 201
and rm202 = 202
and rm203 = 203
and rm204 = 204
and rm205 = 205
and rm206 = 206
and rm207 = 207
and rm208 = 208
and rm209 = 209
and rm210 = 210
and rm211 = 211
and rm212 = 212
and rm213 = 213
and rm214 = 214
and rm215 = 215
and rm216 = 216
and rm217 = 217
and rm218 = 218
and rm219 = 219
and rm220 = 220
and rm221 = 221
and rm222 = 222
and rm223 = 223
and rm224 = 224
and rm225 = 225

(* DEVIATION (M1 scaffolding only): distinctive marker returned by opcode
   arms and RM resume labels whose bodies are not yet ported. It is NOT a
   PCRE2 error code and cannot collide with one (public codes stay above
   -66, the MATCH_xxx internals below -993). Every stub is replaced by its
   owning chunk; until the pcre2_match driver chunk wires Engine.exec, the
   only caller is the test-only [match_internal], so the marker never
   crosses the engine boundary. *)
let error_unported = -979

(* ---------- The match data block (result surface) ---------- *)

(* pcre2_intmodedep.h:656-672 — pcre2_real_match_data, reduced to what the
   match() core reads and writes: oveccount (669) and ovector (671). The
   other C fields have these owners:
   - subject, mark, startchar, leftchar, rightchar, rc, subject_length,
     flags, matchedby: the pcre2_match driver chunk
     (pcre2_match.c:6530-7777) writes them after match() returns;
   - heapframes / heapframes_size: Frames.t (frames.ml DEVIATION: no
     cached vector across matches);
   - memctl: dropped (GC). *)
type match_data = {
  ovector : int array;
      (* PCRE2_SIZE ovector[]: oveccount pairs of subject offsets; unset
         pair = (-1, -1) (Frames.unset) *)
  oveccount : int; (* uint16_t oveccount: number of pairs *)
}

(* ---------- The match block ("static" data) ---------- *)

(* pcre2_intmodedep.h:864-906 — structure for passing "static" information
   around between the functions doing traditional NFA matching. Fields are
   transcribed in struct order; C pointers into the subject / compiled
   code become int offsets, so the base values themselves are carried as
   [subject] (DEVIATION: the C embeds the base in its pointers) and
   [start_code].

   Invariants relied on by the dispatch loop (established by the caller —
   the test-only [match_internal] here, the pcre2_match driver chunk
   later):
   - 0 <= start_subject <= end_subject <= true_end_subject
                                        <= String.length subject;
   - 0 <= start_eptr <= end_subject at [match_] entry — enforced by the
     caller's BADOFFSET check (pcre2_match.c:6610; [match_internal]
     validates [start] before calling [match_], the driver chunk performs
     the identical check). Subject positions in frame slots then never go
     negative (eptr advances from start_eptr in the live arms; the repeat
     maximize backtrack loops decrement it, but never below the saved
     Lstart_eptr, itself a former eptr >= start_eptr); the char arms'
     String.unsafe_get bound proofs rely on this;
   - start_code holds a complete compiled program terminated by OP_END
     (the OP_END arm returns without advancing, so ecode never runs off
     the end);
   - partial is 0, 1 (PCRE2_PARTIAL_SOFT) or 2 (PCRE2_PARTIAL_HARD).

   Later-milestone fields kept as comments at their struct positions:
   - pcre2_memctl memctl (865) — dropped: GC-managed arrays (frames.ml);
   - uint32_t heap_limit (866) — lives in Frames.t.heap_limit: frames.ml
     owns vector growth and its heap accounting;
   - const uint8_t *lcc / *fcc / *ctypes (873-875) — dropped: this port
     always reads the default tables through Chartables (same DEVIATION
     as Compile.compile_block);
   - PCRE2_SPTR check_subject (885) — M6 invalid-UTF fragment matching;
   - PCRE2_SPTR verb_ecode_ptr / verb_skip_ptr (893-894), uint32_t
     verb_current_recurse (895), uint32_t skip_arg_count /
     ignore_skip_arg (898-899) — M5 verbs chunk;
   - pcre2_callout_block *cb (903), void *callout_data (904),
     int ( *callout)(...) (905) — callout support chunk (M8); no callout
     block exists until then, see the RETURN_SWITCH note in [backtrack]. *)
type match_block = {
  match_limit : int; (* uint32_t match_limit (867) *)
  match_limit_depth : int; (* uint32_t match_limit_depth (868) *)
  mutable match_call_count : int;
      (* uint32_t match_call_count (869): number of times a new frame is
         created *)
  mutable hitend : bool;
      (* BOOL hitend (870): hit the end of the subject at some point *)
  hasthen : bool;
      (* BOOL hasthen (871): pattern contains ( *THEN) — consumed by the
         bracket/verb arms (M5); the driver sets it from re->flags *)
  allowemptypartial : bool;
      (* BOOL allowemptypartial (872): allow empty hard partial *)
  subject : string;
      (* DEVIATION: the subject string itself; C pointer fields below are
         int offsets into it *)
  start_offset : int; (* PCRE2_SIZE start_offset (876) *)
  mutable end_offset_top : int;
      (* PCRE2_SIZE end_offset_top (877): highwater mark at end of match *)
  partial : int; (* uint16_t partial (878): PARTIAL options as 0/1/2 *)
  bsr_convention : int; (* uint16_t bsr_convention (879): \R interpretation *)
  name_count : int; (* uint16_t name_count (880) *)
  name_entry_size : int; (* uint16_t name_entry_size (881) *)
  name_table : Bytes.t; (* PCRE2_SPTR name_table (882) *)
  start_code : Bytes.t;
      (* PCRE2_SPTR start_code (883): the compiled program; ecode values
         are offsets into it, offset 0 = the C's mb->start_code
         (pcre2_match.c:6979 = Compile.re.code offset 0) *)
  start_subject : int; (* PCRE2_SPTR start_subject (884) *)
  end_subject : int; (* PCRE2_SPTR end_subject (886): usable end *)
  true_end_subject : int; (* PCRE2_SPTR true_end_subject (887): actual end *)
  mutable end_match_ptr : int;
      (* PCRE2_SPTR end_match_ptr (888): subject position at end match *)
  mutable start_used_ptr : int;
      (* PCRE2_SPTR start_used_ptr (889): earliest consulted character *)
  mutable last_used_ptr : int;
      (* PCRE2_SPTR last_used_ptr (890): latest consulted character *)
  mutable mark : int;
      (* PCRE2_SPTR mark (891): mark to pass back on success — an offset
         into start_code (frames are all-int); -1 = NULL *)
  mutable nomatch_mark : int;
      (* PCRE2_SPTR nomatch_mark (892): mark to pass back on failure;
         -1 = NULL *)
  mutable moptions : int;
      (* uint32_t moptions (896): match options; mutable because the
         driver's bump-along loop resets it per attempt
         (pcre2_match.c:7502-7504) *)
  poptions : int; (* uint32_t poptions (897): pattern options *)
  nltype : int; (* uint32_t nltype (900): newline type *)
  nllen : int; (* uint32_t nllen (901): newline string length *)
  nl0 : int;
  nl1 : int;
      (* PCRE2_UCHAR nl[4] (902) — newline string when fixed; only
         elements 0..1 are ever used (nllen is 1 or 2), as in
         Compile.compile_block *)
}

(* pcre2_match.c:537-543 — SCHECK_PARTIAL(): called when we are already at
   or past the end of the subject. Sets the "hit end" flag if something
   has been matched (or an empty hard partial is allowed); for hard
   partial matching the C does `return PCRE2_ERROR_PARTIAL` from match()
   — returned here as Errors.error_partial, with 0 meaning fall through
   to the caller's RRETURN(MATCH_NOMATCH). *)
let scheck_partial (mb : match_block) (feptr : int) : int =
  if
    (not (Int.equal mb.partial 0))
    && (feptr > mb.start_used_ptr || mb.allowemptypartial)
  then (
    mb.hitend <- true;
    if mb.partial > 1 then Errors.error_partial else 0)
  else 0

(* ---------- Match from current position ---------- *)

(* pcre2_match.c:566-6502 — match(): run one match attempt at a single
   starting point in the subject.

   Arguments (pcre2_match.c:578-584; frame_size lives inside [a]):
     start_eptr   starting character in subject (offset)
     start_ecode  starting position in compiled code (offset)
     top_bracket  number of capturing parentheses in the pattern
     a            the backtracking-frame arena (match_data->heapframes)
     match_data   where to write the resulting ovector
     mb           the "static" variables block

   Returns (pcre2_match.c:586-591): MATCH_MATCH (1) if matched,
   MATCH_NOMATCH (0) if failed to match, a negative MATCH_xxx value for
   PRUNE, SKIP, etc, or a negative PCRE2_ERROR_xxx value if aborted by an
   error condition. *)
let match_ ~(start_eptr : int) ~(start_ecode : int) ~(top_bracket : int)
    (a : Frames.t) (match_data : match_data) (mb : match_block) : int =
  (* pcre2_match.c:630-637 — UTF and UCP flags. Arms testing them defer
     to M6 (utf) / M7 (ucp) with [error_unported]. *)
  let utf = not (Int.equal (mb.poptions land Options.utf) 0) in
  let ucp = not (Int.equal (mb.poptions land Options.ucp) 0) in

  (* pcre2_match.c:550-556 + 662-773 + 790-798 + 6462-6501 — the goto
     graph as four mutually tail-recursive functions over the frame index
     [f] (the C's F pointer). All state lives in the arena slots and in
     [mb]/[match_data]; the loop body allocates nothing. *)
  let rec rmatch (f : int) (ra : int) (rb : int) (group_frame_type : int) : int
      =
    (* pcre2_match.c:550-556 — RMATCH(ra, rb): start_ecode = ra;
       Freturn_id = rb; goto MATCH_RECURSE. The C's ambient
       group_frame_type local is an explicit parameter here: RMATCH sites
       that do not set it pass 0 (the C resets it to 0 in NEW_FRAME,
       772). *)
    a.Frames.frames.(Frames.base a f + Frames.slot_return_id) <- rb;
    (* pcre2_match.c:662-754 — MATCH_RECURSE: Frames.push grows the
       vector under the heap limit (667-712, returning
       error_heaplimit/error_nomemory as values), copies the eptr..ovector
       region (749-751) and sets N->rdepth = Frdepth + 1 (753). *)
    let n = Frames.push a f in
    if n < 0 then n else (new_frame [@tailcall]) n ra group_frame_type
  and new_frame (f : int) (ecode : int) (group_frame_type : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:758-761 — NEW_FRAME: type, starting code pointer,
       and the default backtrack of one frame (Fback_frame = frame_size in
       C bytes; frame units here, frames.ml DEVIATION). *)
    fr.(fb + Frames.slot_group_frame_type) <- group_frame_type;
    fr.(fb + Frames.slot_ecode) <- ecode;
    fr.(fb + Frames.slot_back_frame) <- 1;
    (* pcre2_match.c:763-773 — if this is a special type of group frame,
       remember its offset (frame index here, frames.ml DEVIATION) for
       quick access at the end of the group; if a recursion, set a new
       current recursion value. The C's group_frame_type = 0 reset is
       implicit: the local is per-call. *)
    if not (Int.equal group_frame_type 0) then (
      fr.(fb + Frames.slot_last_group_offset) <- f;
      if Int.equal (Frames.gf_idmask group_frame_type) Frames.gf_recurse then
        fr.(fb + Frames.slot_current_recurse) <-
          Frames.gf_datamask group_frame_type);
    (* pcre2_match.c:776-783 — first check that we haven't recorded too
       many backtracks (search tree too large), or exceeded the recursive
       depth limit (used too many backtracking frames). The C's
       post-increment compares the OLD count and bumps it regardless. *)
    let count = mb.match_call_count in
    mb.match_call_count <- count + 1;
    if count >= mb.match_limit then Errors.error_matchlimit
    else if fr.(fb + Frames.slot_rdepth) >= mb.match_limit_depth then
      Errors.error_depthlimit
    else (dispatch [@tailcall]) f
  and dispatch (f : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:790-798 — the main processing loop: Fop =
       (uint8_t)( *Fecode); switch (Fop). Arms that `break` in the C set
       slot_ecode and tail-call [dispatch]; RRETURN(x) is
       (backtrack [@tailcall]) f x. Reading the code unit is in bounds:
       the program is OP_END-terminated (mb invariant) and the OP_END arm
       returns without advancing. *)
    let ecode = fr.(fb + Frames.slot_ecode) in
    let op = Char.code (Bytes.get mb.start_code ecode) in
    fr.(fb + Frames.slot_op) <- op;
    (* Arms follow the C switch's source order (port-conventions §2); the
       int literals are pinned against Opcodes constants by the
       module-initialization asserts below. Every STUB is replaced by its
       owning chunk. *)
    match op with
    | 166 ->
        (* OP_CLOSE (pcre2_match.c:800-829) — STUB: M5
           conditionals/recursion chunk (rides with OP_ACCEPT). *)
        error_unported
    | 165 ->
        (* OP_ASSERT_ACCEPT (pcre2_match.c:832-840) — STUB: M4 lookaround
           chunk. *)
        error_unported
    | 164 ->
        (* OP_ACCEPT (pcre2_match.c:842-874) — STUB: M5 verbs chunk. In
           the C this arm falls through into the OP_END code (874); when
           it lands it must share the OP_END arm's body below from the
           empty-match check onwards. *)
        error_unported
    | 0 ->
        (* OP_END (pcre2_match.c:876-940). *)
        (* fallthrough target: OP_ACCEPT (not in a recursion) enters here
           in the C (874-877). *)
        (* pcre2_match.c:881-895 — fail for an empty string match if
           PCRE2_NOTEMPTY is set, or if PCRE2_NOTEMPTY_ATSTART is set and
           we have matched at the start of the subject; backtracking will
           then try other alternatives, if any. *)
        if
          Int.equal fr.(fb + Frames.slot_eptr) fr.(fb + Frames.slot_start_match)
          && ((not (Int.equal (mb.moptions land Options.notempty) 0))
             || (not (Int.equal (mb.moptions land Options.notempty_atstart) 0))
                && Int.equal
                     fr.(fb + Frames.slot_start_match)
                     (mb.start_subject + mb.start_offset))
        then (backtrack [@tailcall]) f match_nomatch
          (* pcre2_match.c:897-917 — fail if PCRE2_ENDANCHORED is set and
             the end of the match is not the end of the subject. After
             ( *ACCEPT) we fail the entire match at this position (direct
             return) but backtrack if we've reached the end of the
             pattern. *)
        else if
          fr.(fb + Frames.slot_eptr) < mb.end_subject
          && not
               (Int.equal
                  (mb.moptions lor mb.poptions land Options.endanchored)
                  0)
        then
          if Int.equal fr.(fb + Frames.slot_op) Opcodes.op_end then
            (backtrack [@tailcall]) f match_nomatch
          else match_nomatch (* ( *ACCEPT): return, NOT RRETURN (916) *)
        else (
          (* pcre2_match.c:919-940 — a successful match of the whole
             pattern: record the result and return directly. Pairs that
             follow the highest-numbered captured string but are less
             than the number of capturing groups are set to PCRE2_UNSET;
             "gaps" below offset_top were already set dynamically. *)
          mb.end_match_ptr <- fr.(fb + Frames.slot_eptr);
          mb.end_offset_top <- fr.(fb + Frames.slot_offset_top);
          mb.mark <- fr.(fb + Frames.slot_mark);
          if fr.(fb + Frames.slot_eptr) > mb.last_used_ptr then
            mb.last_used_ptr <- fr.(fb + Frames.slot_eptr);
          match_data.ovector.(0) <-
            fr.(fb + Frames.slot_start_match) - mb.start_subject;
          match_data.ovector.(1) <-
            fr.(fb + Frames.slot_eptr) - mb.start_subject;
          (* pcre2_match.c:934-939 — i = the smaller of the external and
             frame ovector sizes (in slots); copy the frame captures, then
             unset the tail down to Foffset_top + 2. *)
          let i =
            2
            *
            if top_bracket + 1 > match_data.oveccount then match_data.oveccount
            else top_bracket + 1
          in
          Array.blit fr (fb + Frames.slot_ovector) match_data.ovector 2 (i - 2);
          let i = ref i in
          decr i;
          while !i >= fr.(fb + Frames.slot_offset_top) + 2 do
            match_data.ovector.(!i) <- Frames.unset;
            decr i
          done;
          match_match (* return MATCH_MATCH — note: NOT RRETURN (940) *))
    | 12 ->
        (* OP_ANY (pcre2_match.c:943-958, falls through to OP_ALLANY) —
           STUB: classes + typed repeats chunk. *)
        error_unported
    | 13 ->
        (* OP_ALLANY (pcre2_match.c:960-973) — STUB: classes + typed
           repeats chunk. *)
        error_unported
    | 14 ->
        (* OP_ANYBYTE (pcre2_match.c:976-989) — STUB: classes + typed
           repeats chunk. *)
        error_unported
    | 29 ->
        (* OP_CHAR (pcre2_match.c:992-1025) — match a single character,
           casefully. *)
        if utf then
          (* pcre2_match.c:996-1011 — SUPPORT_UNICODE arm: M6
             (07-utf8.md). Unreachable until then: compiling with
             PCRE2_UTF is rejected upstream. *)
          error_unported
        else if
          (* pcre2_match.c:1015-1021 — not UTF mode. *)
          mb.end_subject - fr.(fb + Frames.slot_eptr) < 1
        then
          (* SCHECK_PARTIAL(), then no match. *)
          let rc = scheck_partial mb fr.(fb + Frames.slot_eptr) in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else
          (* pcre2_match.c:1022-1023 — if (Fecode[1] != *Feptr++)
             RRETURN(MATCH_NOMATCH); Fecode += 2. The post-increment
             advances Feptr even when the test fails: the character was
             consulted, and RETURN_SWITCH's last_used_ptr update (6470)
             must see it. *)
          let eptr = fr.(fb + Frames.slot_eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          if
            not
              (Int.equal
                 (Char.code (Bytes.get mb.start_code (ecode + 1)))
                 (* safe: 0 <= start_eptr <= eptr — the caller's BADOFFSET
                    check (pcre2_match.c:6610; mb invariant) establishes
                    0 <= start_eptr, and eptr only advances — and
                    eptr < mb.end_subject <= String.length mb.subject
                    (checked above; mb invariant) *)
                 (Char.code (String.unsafe_get mb.subject eptr)))
          then (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 2;
            (dispatch [@tailcall]) f)
    | 30 ->
        (* OP_CHARI (pcre2_match.c:1028-1104) — match a single character,
           caselessly. If we are at the end of the subject, give up
           immediately. We get here only when the pattern character has at
           most one other case. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          (* pcre2_match.c:1035-1039 — SCHECK_PARTIAL(), then no match. *)
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then
          (* pcre2_match.c:1041-1073 — SUPPORT_UNICODE utf arm: M6. *)
          error_unported
        else if ucp then
          (* pcre2_match.c:1075-1092 — UCP without UTF: M7. *)
          error_unported
        else
          (* pcre2_match.c:1097-1103 — not UTF or UCP mode; use the table
             for characters < 256: if (TABLE_GET(Fecode[1], mb->lcc,
             Fecode[1]) != TABLE_GET( *Feptr, mb->lcc, *Feptr))
             RRETURN(MATCH_NOMATCH); no Feptr post-increment here (unlike
             OP_CHAR). *)
          let pc = Char.code (Bytes.get mb.start_code (ecode + 1)) in
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let cc = Char.code (String.unsafe_get mb.subject eptr) in
          if not (Int.equal (Chartables.lcc pc) (Chartables.lcc cc)) then
            (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_eptr) <- eptr + 1;
            fr.(fb + Frames.slot_ecode) <- ecode + 2;
            (dispatch [@tailcall]) f)
    | 31 | 32 ->
        (* OP_NOT, OP_NOTI (pcre2_match.c:1107-1174) — match not a single
           character. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          (* pcre2_match.c:1112-1116 — SCHECK_PARTIAL(), then no match. *)
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then
          (* pcre2_match.c:1118-1137 — SUPPORT_UNICODE utf arm: M6. *)
          error_unported
        else if ucp then
          (* pcre2_match.c:1139-1160 — UCP without UTF: M7. *)
          error_unported
        else
          (* pcre2_match.c:1165-1173 — neither UTF nor UCP is set. *)
          let ch = Char.code (Bytes.get mb.start_code (ecode + 1)) in
          (* fc = UCHAR21INC(Feptr) — the post-increment advances Feptr
             even when the test fails: the character was consulted, and
             RETURN_SWITCH's last_used_ptr update (6470) must see it. *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          if
            Int.equal ch fc
            || (Int.equal op Opcodes.op_noti && Int.equal (Chartables.fcc ch) fc)
          then (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 2;
            (dispatch [@tailcall]) f)
    | 41 | 54 ->
        (* OP_EXACT, OP_EXACTI (pcre2_match.c:1188-1192). The C leaves
           reptype untouched here; it is never read when Lmin = Lmax (the
           min-loop epilogue continues before any strategy dispatch), so 0
           is passed. *)
        let n = Compile.get2 mb.start_code (ecode + 1) in
        (repeatchar [@tailcall]) f n n 0 (ecode + 1 + Limits.imm2_size)
    | 45 | 58 ->
        (* OP_POSUPTO, OP_POSUPTOI (pcre2_match.c:1194-1200). *)
        (repeatchar [@tailcall]) f 0
          (Compile.get2 mb.start_code (ecode + 1))
          reptype_pos
          (ecode + 1 + Limits.imm2_size)
    | 39 | 52 ->
        (* OP_UPTO, OP_UPTOI (pcre2_match.c:1202-1208). *)
        (repeatchar [@tailcall]) f 0
          (Compile.get2 mb.start_code (ecode + 1))
          reptype_max
          (ecode + 1 + Limits.imm2_size)
    | 40 | 53 ->
        (* OP_MINUPTO, OP_MINUPTOI (pcre2_match.c:1210-1216). *)
        (repeatchar [@tailcall]) f 0
          (Compile.get2 mb.start_code (ecode + 1))
          reptype_min
          (ecode + 1 + Limits.imm2_size)
    | 42 | 55 ->
        (* OP_POSSTAR, OP_POSSTARI (pcre2_match.c:1218-1224). *)
        (repeatchar [@tailcall]) f 0 uint32_max reptype_pos (ecode + 1)
    | 43 | 56 ->
        (* OP_POSPLUS, OP_POSPLUSI (pcre2_match.c:1226-1232). *)
        (repeatchar [@tailcall]) f 1 uint32_max reptype_pos (ecode + 1)
    | 44 | 57 ->
        (* OP_POSQUERY, OP_POSQUERYI (pcre2_match.c:1234-1240). *)
        (repeatchar [@tailcall]) f 0 1 reptype_pos (ecode + 1)
    | 33 | 46 | 34 | 47 | 35 | 48 | 36 | 49 | 37 | 50 | 38 | 51 ->
        (* OP_STAR(I), OP_MINSTAR(I), OP_PLUS(I), OP_MINPLUS(I),
           OP_QUERY(I), OP_MINQUERY(I) (pcre2_match.c:1242-1257): fc =
           *Fecode++ - ((Fop < OP_STARI)? OP_STAR : OP_STARI) indexes the
           rep_min/rep_max/rep_typ tables. *)
        let idx =
          op
          - if op < Opcodes.op_stari then Opcodes.op_star else Opcodes.op_stari
        in
        (repeatchar [@tailcall]) f rep_min.(idx) rep_max.(idx) rep_typ.(idx)
          (ecode + 1)
    | 67 | 80 ->
        (* OP_NOTEXACT, OP_NOTEXACTI (pcre2_match.c:1542-1546). reptype is
           unread when Lmin = Lmax, as at OP_EXACT: 0 is passed. *)
        let n = Compile.get2 mb.start_code (ecode + 1) in
        (repeatnotchar [@tailcall]) f n n 0 (ecode + 1 + Limits.imm2_size)
    | 65 | 78 ->
        (* OP_NOTUPTO, OP_NOTUPTOI (pcre2_match.c:1548-1554). *)
        (repeatnotchar [@tailcall]) f 0
          (Compile.get2 mb.start_code (ecode + 1))
          reptype_max
          (ecode + 1 + Limits.imm2_size)
    | 66 | 79 ->
        (* OP_NOTMINUPTO, OP_NOTMINUPTOI (pcre2_match.c:1556-1562). *)
        (repeatnotchar [@tailcall]) f 0
          (Compile.get2 mb.start_code (ecode + 1))
          reptype_min
          (ecode + 1 + Limits.imm2_size)
    | 68 | 81 ->
        (* OP_NOTPOSSTAR, OP_NOTPOSSTARI (pcre2_match.c:1564-1570). *)
        (repeatnotchar [@tailcall]) f 0 uint32_max reptype_pos (ecode + 1)
    | 69 | 82 ->
        (* OP_NOTPOSPLUS, OP_NOTPOSPLUSI (pcre2_match.c:1572-1578). *)
        (repeatnotchar [@tailcall]) f 1 uint32_max reptype_pos (ecode + 1)
    | 70 | 83 ->
        (* OP_NOTPOSQUERY, OP_NOTPOSQUERYI (pcre2_match.c:1580-1586). *)
        (repeatnotchar [@tailcall]) f 0 1 reptype_pos (ecode + 1)
    | 71 | 84 ->
        (* OP_NOTPOSUPTO, OP_NOTPOSUPTOI (pcre2_match.c:1588-1594). *)
        (repeatnotchar [@tailcall]) f 0
          (Compile.get2 mb.start_code (ecode + 1))
          reptype_pos
          (ecode + 1 + Limits.imm2_size)
    | 59 | 72 | 60 | 73 | 61 | 74 | 62 | 75 | 63 | 76 | 64 | 77 ->
        (* OP_NOTSTAR(I), OP_NOTMINSTAR(I), OP_NOTPLUS(I), OP_NOTMINPLUS(I),
           OP_NOTQUERY(I), OP_NOTMINQUERY(I) (pcre2_match.c:1596-1611):
           fc = *Fecode++ - ((Fop >= OP_NOTSTARI)? OP_NOTSTARI :
           OP_NOTSTAR) indexes the rep tables. *)
        let idx =
          op
          -
          if op >= Opcodes.op_notstari then Opcodes.op_notstari
          else Opcodes.op_notstar
        in
        (repeatnotchar [@tailcall]) f rep_min.(idx) rep_max.(idx) rep_typ.(idx)
          (ecode + 1)
    | 111 | 110 ->
        (* OP_NCLASS, OP_CLASS (pcre2_match.c:1932-2172) — STUB: classes +
           typed repeats chunk. *)
        error_unported
    | 112 ->
        (* OP_XCLASS (pcre2_match.c:2175-2302) — STUB: M6 (wide chars). *)
        error_unported
    | 6 | 7 | 8 | 9 | 10 | 11 ->
        (* OP_NOT_DIGIT, OP_DIGIT, OP_NOT_WHITESPACE, OP_WHITESPACE,
           OP_NOT_WORDCHAR, OP_WORDCHAR (pcre2_match.c:2305-2375) — STUB:
           classes + typed repeats chunk. *)
        error_unported
    | 17 ->
        (* OP_ANYNL (pcre2_match.c:2377-2409) — STUB: classes + typed
           repeats chunk. *)
        error_unported
    | 18 | 19 | 20 | 21 ->
        (* OP_NOT_HSPACE, OP_HSPACE, OP_NOT_VSPACE, OP_VSPACE
           (pcre2_match.c:2412-2476) — STUB: classes + typed repeats
           chunk. *)
        error_unported
    | 16 | 15 ->
        (* OP_PROP, OP_NOTPROP (pcre2_match.c:2479-2618) — STUB: M7
           (UCP). *)
        error_unported
    | 22 ->
        (* OP_EXTUNI (pcre2_match.c:2621-2648) — STUB: M7 (UCP). *)
        error_unported
    | 93 | 91 | 92 | 94 | 95 | 96 | 97 | 85 | 86 | 87 | 88 | 89 | 90 ->
        (* OP_TYPEEXACT, OP_TYPEUPTO, OP_TYPEMINUPTO, OP_TYPEPOSSTAR,
           OP_TYPEPOSPLUS, OP_TYPEPOSQUERY, OP_TYPEPOSUPTO, OP_TYPESTAR,
           OP_TYPEMINSTAR, OP_TYPEPLUS, OP_TYPEMINPLUS, OP_TYPEQUERY,
           OP_TYPEMINQUERY (pcre2_match.c:2651-4991) — STUB: classes +
           typed repeats chunk (ASCII arms; UTF/UCP arms M6/M7). *)
        error_unported
    | 115 | 116 ->
        (* OP_DNREF, OP_DNREFI (pcre2_match.c:4994-5008) — STUB: M2
           backreferences. *)
        error_unported
    | 113 | 114 ->
        (* OP_REF, OP_REFI (pcre2_match.c:5011-5220) — STUB: M2
           backreferences. *)
        error_unported
    | 151 ->
        (* OP_BRAZERO (pcre2_match.c:5223-5229) — STUB: brackets chunk. *)
        error_unported
    | 152 ->
        (* OP_BRAMINZERO (pcre2_match.c:5232-5239) — STUB: brackets
           chunk. *)
        error_unported
    | 167 ->
        (* OP_SKIPZERO (pcre2_match.c:5242-5245) — STUB: M5 verbs chunk. *)
        error_unported
    | 153 ->
        (* OP_BRAPOSZERO (pcre2_match.c:5257-5263) — STUB: M4 possessive
           chunk. *)
        error_unported
    | 136 | 141 | 138 | 143 ->
        (* OP_BRAPOS, OP_SBRAPOS, OP_CBRAPOS, OP_SCBRAPOS
           (pcre2_match.c:5266-5346) — STUB: M4 possessive chunk. *)
        error_unported
    | 135 ->
        (* OP_BRA (pcre2_match.c:5349-5378) — STUB: brackets chunk. *)
        error_unported
    | 137 | 142 ->
        (* OP_CBRA, OP_SCBRA (pcre2_match.c:5381-5389) — STUB: brackets
           chunk. *)
        error_unported
    | 133 | 134 | 140 ->
        (* OP_ONCE, OP_SCRIPT_RUN, OP_SBRA (pcre2_match.c:5391-5424) —
           OP_SBRA: brackets chunk; OP_ONCE: M4; OP_SCRIPT_RUN: M7.
           STUB. *)
        error_unported
    | 117 ->
        (* OP_RECURSE (pcre2_match.c:5427-5508) — STUB: M5 recursion
           chunk. *)
        error_unported
    | 127 | 129 | 131 | 132 ->
        (* OP_ASSERT, OP_ASSERTBACK, OP_ASSERT_NA, OP_ASSERTBACK_NA
           (pcre2_match.c:5511-5544) — STUB: M4 lookaround chunk. *)
        error_unported
    | 128 | 130 ->
        (* OP_ASSERT_NOT, OP_ASSERTBACK_NOT (pcre2_match.c:5547-5591) —
           STUB: M4 lookaround chunk. *)
        error_unported
    | 118 | 119 ->
        (* OP_CALLOUT, OP_CALLOUT_STR (pcre2_match.c:5594-5606) — STUB:
           callout support chunk (M8). *)
        error_unported
    | 139 | 144 ->
        (* OP_COND, OP_SCOND (pcre2_match.c:5609-5787; the condition
           opcodes OP_CREF/OP_DNCREF/OP_RREF/OP_DNRREF/OP_FALSE/OP_TRUE
           are read INSIDE this arm, 5641-5700) — STUB: M5 conditionals
           chunk. *)
        error_unported
    | 125 ->
        (* OP_REVERSE (pcre2_match.c:5790-5831) — STUB: M3 lookbehind
           chunk. *)
        error_unported
    | 126 ->
        (* OP_VREVERSE (pcre2_match.c:5834-5890) — STUB: M3 lookbehind
           chunk. *)
        error_unported
    | 120 ->
        (* OP_ALT (pcre2_match.c:5893-5903) — STUB: brackets chunk. *)
        error_unported
    | 121 | 123 | 122 | 124 ->
        (* OP_KET, OP_KETRMIN, OP_KETRMAX, OP_KETRPOS
           (pcre2_match.c:5906-6128) — KET/KETRMIN/KETRMAX: brackets
           chunk; KETRPOS: M4 possessive chunk. STUB. *)
        error_unported
    | 27 ->
        (* OP_CIRC (pcre2_match.c:6132-6137) — STUB: anchors chunk. *)
        error_unported
    | 1 ->
        (* OP_SOD (pcre2_match.c:6139-6142) — STUB: anchors chunk. *)
        error_unported
    | 25 ->
        (* OP_DOLL (pcre2_match.c:6147-6151) — STUB: anchors chunk. *)
        error_unported
    | 24 ->
        (* OP_EOD (pcre2_match.c:6154-6163) — STUB: anchors chunk. *)
        error_unported
    | 23 ->
        (* OP_EODN (pcre2_match.c:6166-6195) — STUB: anchors chunk. *)
        error_unported
    | 28 ->
        (* OP_CIRCM (pcre2_match.c:6198-6211) — STUB: anchors chunk. *)
        error_unported
    | 26 ->
        (* OP_DOLLM (pcre2_match.c:6214-6239) — STUB: anchors chunk. *)
        error_unported
    | 2 ->
        (* OP_SOM (pcre2_match.c:6242-6246) — STUB: anchors chunk. *)
        error_unported
    | 3 ->
        (* OP_SET_SOM (pcre2_match.c:6249-6255) — STUB: M5 (\K) chunk. *)
        error_unported
    | 4 | 5 | 169 | 170 ->
        (* OP_NOT_WORD_BOUNDARY, OP_WORD_BOUNDARY,
           OP_NOT_UCP_WORD_BOUNDARY, OP_UCP_WORD_BOUNDARY
           (pcre2_match.c:6258-6337) — STUB: anchors chunk (ASCII arms;
           UCP arms M7). *)
        error_unported
    | 154 ->
        (* OP_MARK (pcre2_match.c:6340-6356) — STUB: M5 verbs chunk. *)
        error_unported
    | 163 ->
        (* OP_FAIL (pcre2_match.c:6359-6363) — STUB: M5 verbs chunk. *)
        error_unported
    | 161 | 162 | 155 | 156 | 157 | 158 | 159 | 160 ->
        (* OP_COMMIT, OP_COMMIT_ARG, OP_PRUNE, OP_PRUNE_ARG, OP_SKIP,
           OP_SKIP_ARG, OP_THEN, OP_THEN_ARG (pcre2_match.c:6366-6442) —
           STUB: M5 verbs chunk. *)
        error_unported
    | 98 | 99 | 100 | 101 | 102 | 103 | 104 | 105 | 106 | 107 | 108 | 109 | 145
    | 146 | 147 | 148 | 149 | 150 | 168 | _ ->
        (* pcre2_match.c:6445-6450 — default: PCRE2_ERROR_INTERNAL. Like
           the C switch, these opcodes have no top-level case: the
           repeat modifiers OP_CRSTAR..OP_CRPOSRANGE (98-109) are consumed
           inside the CLASS/NCLASS/XCLASS/REF arms (e.g. 5022-5060); the
           condition opcodes OP_CREF..OP_TRUE (145-150) inside OP_COND
           (5641-5700); OP_DEFINE (168) is rewritten to OP_FALSE by the
           compiler. Values >= op_table_length are corrupt patterns. *)
        Errors.error_internal
  (* pcre2_match.c:1259-1400 — REPEATCHAR: common code for all repeated
     single-character matches (goto target of the OP_EXACT..OP_MINQUERY
     arms). We first check for the minimum number of characters. If the
     minimum equals the maximum, we are done. Otherwise, if minimizing,
     check the rest of the pattern for a match; if there isn't one,
     advance up to the maximum, one character at a time. If maximizing,
     advance up to the maximum number of matching characters, until Feptr
     is past the end of the maximum run. If possessive, we are then done
     (no backing up). Otherwise, match at this position; anything other
     than no match is immediately returned; for nomatch, back up one
     character at a time. The caseful/caseless cases are handled
     separately, for speed. The C's frame temporaries map to slots:
     Lstart_eptr = temp_sptr[0], Lmin/Lmax/Lc/Loc = temp_32[0..3]
     (pcre2_match.c:1180-1186). *)
  and repeatchar (f : int) (lmin : int) (lmax : int) (reptype : int)
      (ecode : int) : int =
    if utf then
      (* pcre2_match.c:1277-1374 — SUPPORT_UNICODE utf block (multi-code-
         unit chars, othercase runs, RM202/RM203): M6. *)
      error_unported
    else
      let fr = a.Frames.frames in
      let fb = Frames.base a f in
      (* pcre2_match.c:1378-1381 — when not in UTF mode, load a
         single-code-unit character: Lc = *Fecode++. The Lmin/Lmax stores
         transcribe the assignments in the opcode arms (1188-1257). *)
      let lc = Char.code (Bytes.get mb.start_code ecode) in
      let ecode = ecode + 1 in
      fr.(fb + Frames.slot_temp_32_0) <- lmin (* Lmin *);
      fr.(fb + Frames.slot_temp_32_1) <- lmax (* Lmax *);
      fr.(fb + Frames.slot_temp_32_2) <- lc (* Lc *);
      fr.(fb + Frames.slot_ecode) <- ecode;
      if fr.(fb + Frames.slot_op) >= Opcodes.op_stari then
        (* pcre2_match.c:1383-1400 — caseless comparison: Loc is the other
           case. *)
        if ucp && (not utf) && lc > 127 then
          (* pcre2_match.c:1388-1389 — Loc = UCD_OTHERCASE(Lc): M7. *)
          error_unported
        else (
          (* pcre2_match.c:1393 — Loc = mb->fcc[Lc] (Lc < 128 in UTF-8
             mode, and characters < 256 otherwise). *)
          fr.(fb + Frames.slot_temp_32_3) <- Chartables.fcc lc (* Loc *);
          (repeatchar_ci_min [@tailcall]) f 1 reptype)
      else (repeatchar_cs_min [@tailcall]) f 1 reptype
  and repeatchar_ci_min (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1402-1413 — caseless: for (i = 1; i <= Lmin; i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if
          (not (Int.equal fr.(fb + Frames.slot_temp_32_2) cc))
          && not (Int.equal fr.(fb + Frames.slot_temp_32_3) cc)
        then (backtrack [@tailcall]) f match_nomatch
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (repeatchar_ci_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:1414 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1416-1421 — minimize: the for(;;) starts with
         RMATCH(Fecode, RM25); the rest of the loop body is the RM25
         resume arm in [backtrack]. *)
      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm25 0
    else (
      (* pcre2_match.c:1436-1438 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (repeatchar_ci_maxscan [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        reptype)
  and repeatchar_ci_maxscan (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1439-1450 — caseless greedy scan:
       for (i = Lmin; i < Lmax; i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:1442-1446 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (repeatchar_ci_maxend [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if
          (not (Int.equal fr.(fb + Frames.slot_temp_32_2) cc))
          && not (Int.equal fr.(fb + Frames.slot_temp_32_3) cc)
        then (repeatchar_ci_maxend [@tailcall]) f reptype (* break, 1448 *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (repeatchar_ci_maxscan [@tailcall]) f (i + 1) reptype)
    else (repeatchar_ci_maxend [@tailcall]) f reptype
  and repeatchar_ci_maxend (f : int) (reptype : int) : int =
    (* pcre2_match.c:1451 — if possessive, no backing up: fall out of the
       arm (break, 1517) and continue the main loop at the advanced
       Fecode. *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) f
    else (repeatchar_ci_maxbt [@tailcall]) f
  and repeatchar_ci_maxbt (f : int) : int =
    (* pcre2_match.c:1451-1457 — the caseless maximize backtracking
       for(;;) head: the minimum position Lstart_eptr is tried in place
       (break -> main loop); every position above it via RMATCH. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if Int.equal fr.(fb + Frames.slot_eptr) fr.(fb + Frames.slot_temp_sptr_0)
    then (dispatch [@tailcall]) f
    else (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm26 0
  and repeatchar_cs_min (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1461-1473 — caseful comparisons (includes all
       multi-byte characters): for (i = 1; i <= Lmin; i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
      else
        (* pcre2_match.c:1472 — if (Lc != UCHAR21INCTEST(Feptr))
           RRETURN(MATCH_NOMATCH): the post-increment advances Feptr even
           when the test fails. *)
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        if not (Int.equal fr.(fb + Frames.slot_temp_32_2) cc) then
          (backtrack [@tailcall]) f match_nomatch
        else (repeatchar_cs_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:1475 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1477-1481 — minimize: RMATCH(Fecode, RM27); the
         rest of the loop body is the RM27 resume arm. *)
      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm27 0
    else (
      (* pcre2_match.c:1493-1495 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (repeatchar_cs_maxscan [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        reptype)
  and repeatchar_cs_maxscan (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1496-1506 — caseful greedy scan:
       for (i = Lmin; i < Lmax; i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:1498-1502 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (repeatchar_cs_maxend [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if not (Int.equal fr.(fb + Frames.slot_temp_32_2) cc) then
          (repeatchar_cs_maxend [@tailcall]) f reptype (* break, 1504 *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (repeatchar_cs_maxscan [@tailcall]) f (i + 1) reptype)
    else (repeatchar_cs_maxend [@tailcall]) f reptype
  and repeatchar_cs_maxend (f : int) (reptype : int) : int =
    (* pcre2_match.c:1508 — possessive: fall out (break, 1517). *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) f
    else (repeatchar_cs_maxbt [@tailcall]) f
  and repeatchar_cs_maxbt (f : int) : int =
    (* pcre2_match.c:1508-1514 — the caseful maximize backtracking for(;;)
       head. The C's `<=` guard (vs `==` at 1453) is transcribed as-is. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) <= fr.(fb + Frames.slot_temp_sptr_0) then
      (dispatch [@tailcall]) f
    else (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm28 0
  (* pcre2_match.c:1528-1634 — REPEATNOTCHAR: common code for all repeated
     single-character non-matches (goto target of the
     OP_NOTEXACT..OP_NOTMINQUERY arms). Almost a repeat of REPEATCHAR,
     kept separate exactly as the C keeps it (1529-1534). Frame
     temporaries: Lstart_eptr = temp_sptr[0], Lmin/Lmax/Lc/Loc =
     temp_32[0..3] (pcre2_match.c:1536-1540). *)
  and repeatnotchar (f : int) (lmin : int) (lmax : int) (reptype : int)
      (ecode : int) : int =
    if utf then
      (* pcre2_match.c:1616 GETCHARINCTEST's UTF decode and every
         `if (utf)` branch of this family (1636-1650, 1672-1689,
         1718-1747, 1778-1792, 1812-1829, 1856-1885, RM204-RM207): M6. *)
      error_unported
    else
      let fr = a.Frames.frames in
      let fb = Frames.base a f in
      (* pcre2_match.c:1615-1616 — GETCHARINCTEST(Lc, Fecode): one code
         unit when not UTF. Lmin/Lmax stores transcribe the opcode arms
         (1542-1611). *)
      let lc = Char.code (Bytes.get mb.start_code ecode) in
      let ecode = ecode + 1 in
      fr.(fb + Frames.slot_temp_32_0) <- lmin (* Lmin *);
      fr.(fb + Frames.slot_temp_32_1) <- lmax (* Lmax *);
      fr.(fb + Frames.slot_temp_32_2) <- lc (* Lc *);
      fr.(fb + Frames.slot_ecode) <- ecode;
      if fr.(fb + Frames.slot_op) >= Opcodes.op_notstari then
        (* pcre2_match.c:1626-1634 — caseless: Loc is the other case. *)
        if ucp && lc > 127 then
          (* pcre2_match.c:1629-1630 — (utf || ucp) && Lc > 127 ->
             UCD_OTHERCASE(Lc): M7 (utf is already excluded above). *)
          error_unported
        else (
          (* pcre2_match.c:1634 — Loc = TABLE_GET(Lc, mb->fcc, Lc). *)
          fr.(fb + Frames.slot_temp_32_3) <- Chartables.fcc lc (* Loc *);
          (repeatnotchar_ci_min [@tailcall]) f 1 reptype)
      else (repeatnotchar_cs_min [@tailcall]) f 1 reptype
  and repeatnotchar_ci_min (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1654-1666 — caseless, not UTF: ensure the minimum
       number of non-matches are present. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if
          Int.equal fr.(fb + Frames.slot_temp_32_2) cc
          || Int.equal fr.(fb + Frames.slot_temp_32_3) cc
        then (backtrack [@tailcall]) f match_nomatch
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (repeatnotchar_ci_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:1668 — if (Lmin == Lmax) continue: finished for
         exact count. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1693-1697 — minimize, not UTF: RMATCH(Fecode,
         RM29); the rest of the loop body is the RM29 resume arm. *)
      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm29 0
    else (
      (* pcre2_match.c:1714-1716 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (repeatnotchar_ci_maxscan [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        reptype)
  and repeatnotchar_ci_maxscan (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1751-1762 — caseless greedy scan, not UTF:
       for (i = Lmin; i < Lmax; i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:1755-1759 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (repeatnotchar_ci_maxend [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if
          Int.equal fr.(fb + Frames.slot_temp_32_2) cc
          || Int.equal fr.(fb + Frames.slot_temp_32_3) cc
        then (repeatnotchar_ci_maxend [@tailcall]) f reptype (* break, 1760 *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (repeatnotchar_ci_maxscan [@tailcall]) f (i + 1) reptype)
    else (repeatnotchar_ci_maxend [@tailcall]) f reptype
  and repeatnotchar_ci_maxend (f : int) (reptype : int) : int =
    (* pcre2_match.c:1763 — possessive: fall out (break, 1910). *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) f
    else (repeatnotchar_ci_maxbt [@tailcall]) f
  and repeatnotchar_ci_maxbt (f : int) : int =
    (* pcre2_match.c:1763-1769 — the caseless NOT maximize backtracking
       for(;;) head. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if Int.equal fr.(fb + Frames.slot_eptr) fr.(fb + Frames.slot_temp_sptr_0)
    then (dispatch [@tailcall]) f
    else (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm30 0
  and repeatnotchar_cs_min (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1795-1806 — caseful, not UTF: for (i = 1; i <= Lmin;
       i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
      else
        (* pcre2_match.c:1804 — if (Lc == *Feptr++)
           RRETURN(MATCH_NOMATCH): the post-increment advances Feptr even
           when the test fails. *)
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        if Int.equal fr.(fb + Frames.slot_temp_32_2) cc then
          (backtrack [@tailcall]) f match_nomatch
        else (repeatnotchar_cs_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:1808 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1832-1836 — minimize, not UTF: RMATCH(Fecode,
         RM31); the rest of the loop body is the RM31 resume arm. *)
      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm31 0
    else (
      (* pcre2_match.c:1852-1854 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (repeatnotchar_cs_maxscan [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        reptype)
  and repeatnotchar_cs_maxscan (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1887-1899 — caseful greedy scan, not UTF:
       for (i = Lmin; i < Lmax; i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:1892-1896 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (repeatnotchar_cs_maxend [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if Int.equal fr.(fb + Frames.slot_temp_32_2) cc then
          (repeatnotchar_cs_maxend [@tailcall]) f reptype (* break, 1897 *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (repeatnotchar_cs_maxscan [@tailcall]) f (i + 1) reptype)
    else (repeatnotchar_cs_maxend [@tailcall]) f reptype
  and repeatnotchar_cs_maxend (f : int) (reptype : int) : int =
    (* pcre2_match.c:1900 — possessive: fall out (break, 1910). *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) f
    else (repeatnotchar_cs_maxbt [@tailcall]) f
  and repeatnotchar_cs_maxbt (f : int) : int =
    (* pcre2_match.c:1900-1906 — the caseful NOT maximize backtracking
       for(;;) head. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if Int.equal fr.(fb + Frames.slot_eptr) fr.(fb + Frames.slot_temp_sptr_0)
    then (dispatch [@tailcall]) f
    else (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm32 0
  and backtrack (f : int) (rrc : int) : int =
    (* pcre2_match.c:6461-6501 — RETURN_SWITCH: the RRETURN() macro jumps
       here with the return value in rrc; Freturn_id says which L_RM##
       label to resume at, and Frdepth is the frame's index. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:6470 *)
    if fr.(fb + Frames.slot_eptr) > mb.last_used_ptr then
      mb.last_used_ptr <- fr.(fb + Frames.slot_eptr);
    (* pcre2_match.c:6471 — exit from the top level. *)
    if Int.equal fr.(fb + Frames.slot_rdepth) 0 then rrc
    else
      (* pcre2_match.c:6472 — backtrack: F = F - Fback_frame (frame units
         here, frames.ml DEVIATION). *)
      let f = f - fr.(fb + Frames.slot_back_frame) in
      (* pcre2_match.c:6473 — mb->cb->callout_flags |=
         PCRE2_CALLOUT_BACKTRACK: no callout block exists in this port
         until the callout chunk (M8); site kept for evaluation order. *)
      (* pcre2_match.c:6479-6500 — switch (Freturn_id), reading the NEW
         current frame. Each label resumes the C code after the RMATCH
         site listed below; the owning chunk replaces its stub together
         with that site's arm. Unknown ids are PCRE2_ERROR_INTERNAL like
         the C default.
           label : C RMATCH site (pcre2_match.c) — enclosing arm
           RM1   : 5364 OP_BRA                RM2  : 5400 OP_CBRA/group
           RM3   : 5519 OP_ASSERT family      RM4  : 5554 OP_ASSERT_NOT
           RM5   : 5708 OP_COND branch        RM6  : 6111 OP_KETRMIN
           RM7   : 6119 OP_KETRMAX            RM8  : 5291 OP_BRAPOS group
           RM9   : 5226 OP_BRAZERO            RM10 : 5235 OP_BRAMINZERO
           RM11  : 5468 OP_RECURSE branches   RM12 : 6342 OP_MARK
           RM13  : 6367 OP_COMMIT             RM14 : 6380 OP_PRUNE
           RM15  : 6387 OP_PRUNE_ARG          RM16 : 6393 OP_SKIP
           RM17  : 6414 OP_SKIP_ARG           RM18 : 6430 OP_THEN
           RM19  : 6438 OP_THEN_ARG           RM20 : 5102 OP_REF min
           RM21  : 5158 OP_REF max            RM22 : 5172 OP_REF max
           RM23  : 2055 OP_CLASS min          RM24 : 2145 OP_CLASS max
           RM25  : 1421 char repeat min       RM26 : 1454 char repeat max
           RM27  : 1481 char repeat min       RM28 : 1511 char repeat max
           RM29  : 1697 NOT repeat min        RM30 : 1766 NOT repeat max
           RM31  : 1836 NOT repeat min        RM32 : 1903 NOT repeat max
           RM33  : 3965 type repeat min       RM34 : 4963 type repeat max
           RM35  : 5775 OP_SCOND descend      RM36 : 6374 OP_COMMIT_ARG
           RM37  : 5876 OP_VREVERSE
           RM100 : 2236 OP_XCLASS min         RM101: 2280 OP_XCLASS max
           RM200 : 2032 class UTF min         RM201: 2112 class UTF max
           RM202 : 1317 CHARI UTF             RM203: 1361 CHARI UTF
           RM204 : 1678 NOTI UTF min          RM205: 1742 NOTI UTF max
           RM206 : 1818 NOTI UTF min          RM207: 1880 NOTI UTF max
           RM208-RM219, RM223-RM225 : 3522-3838, 3612-3781 type repeat
             min UTF/UCP arms
           RM220 : 4437, RM221 : 4710, RM222 : 4394 type repeat max
             UTF/UCP arms *)
      let fb = Frames.base a f in
      match fr.(fb + Frames.slot_return_id) with
      | 1 | 2 | 9 | 10 | 6 | 7 ->
          (* RM1 RM2 RM9 RM10 RM6 RM7 — STUB: brackets chunk. *)
          error_unported
      | 25 ->
          (* L_RM25 (pcre2_match.c:1421-1431) — caseless char repeat,
             minimize: the tail failed; take one more matching character
             and try again. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
              else
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let cc = Char.code (String.unsafe_get mb.subject eptr) in
                if
                  (not (Int.equal fr.(fb + Frames.slot_temp_32_2) cc))
                  && not (Int.equal fr.(fb + Frames.slot_temp_32_3) cc)
                then (backtrack [@tailcall]) f match_nomatch
                else (
                  fr.(fb + Frames.slot_eptr) <- eptr + 1;
                  (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm25 0)
      | 26 ->
          (* L_RM26 (pcre2_match.c:1454-1456) — caseless char repeat,
             maximize: Feptr-- BEFORE the rrc test (the C's order), then
             back to the for(;;) head. *)
          fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - 1;
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else (repeatchar_ci_maxbt [@tailcall]) f
      | 27 ->
          (* L_RM27 (pcre2_match.c:1481-1489) — caseful char repeat,
             minimize. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
              else
                (* pcre2_match.c:1489 — if (Lc != UCHAR21INCTEST(Feptr))
                   RRETURN(MATCH_NOMATCH): post-increment. *)
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let cc = Char.code (String.unsafe_get mb.subject eptr) in
                fr.(fb + Frames.slot_eptr) <- eptr + 1;
                if not (Int.equal fr.(fb + Frames.slot_temp_32_2) cc) then
                  (backtrack [@tailcall]) f match_nomatch
                else (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm27 0
      | 28 ->
          (* L_RM28 (pcre2_match.c:1511-1513) — caseful char repeat,
             maximize: Feptr-- before the rrc test, then the for(;;)
             head. *)
          fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - 1;
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else (repeatchar_cs_maxbt [@tailcall]) f
      | 29 ->
          (* L_RM29 (pcre2_match.c:1697-1706) — caseless NOT repeat,
             minimize. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
              else
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let cc = Char.code (String.unsafe_get mb.subject eptr) in
                if
                  Int.equal fr.(fb + Frames.slot_temp_32_2) cc
                  || Int.equal fr.(fb + Frames.slot_temp_32_3) cc
                then (backtrack [@tailcall]) f match_nomatch
                else (
                  fr.(fb + Frames.slot_eptr) <- eptr + 1;
                  (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm29 0)
      | 30 ->
          (* L_RM30 (pcre2_match.c:1766-1768) — caseless NOT repeat,
             maximize: rrc test BEFORE Feptr-- (opposite order to
             RM26/RM28), then the for(;;) head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else (
            fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - 1;
            (repeatnotchar_ci_maxbt [@tailcall]) f)
      | 31 ->
          (* L_RM31 (pcre2_match.c:1836-1844) — caseful NOT repeat,
             minimize. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
              else
                (* pcre2_match.c:1844 — if (Lc == *Feptr++)
                   RRETURN(MATCH_NOMATCH): post-increment. *)
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let cc = Char.code (String.unsafe_get mb.subject eptr) in
                fr.(fb + Frames.slot_eptr) <- eptr + 1;
                if Int.equal fr.(fb + Frames.slot_temp_32_2) cc then
                  (backtrack [@tailcall]) f match_nomatch
                else (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm31 0
      | 32 ->
          (* L_RM32 (pcre2_match.c:1903-1905) — caseful NOT repeat,
             maximize: rrc test before Feptr--, then the for(;;) head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else (
            fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - 1;
            (repeatnotchar_cs_maxbt [@tailcall]) f)
      | 23 | 24 | 33 | 34 ->
          (* RM23 RM24 RM33 RM34 — STUB: classes + typed repeats chunk. *)
          error_unported
      | 20 | 21 | 22 ->
          (* RM20 RM21 RM22 — STUB: M2 backreferences. *)
          error_unported
      | 37 ->
          (* RM37 — STUB: M3 lookbehind chunk. *)
          error_unported
      | 3 | 4 ->
          (* RM3 RM4 — STUB: M4 lookaround chunk. *)
          error_unported
      | 8 ->
          (* RM8 — STUB: M4 possessive chunk: the POSSESSIVE_GROUP RMATCH
             inside OP_BRAPOS/OP_SBRAPOS/OP_CBRAPOS/OP_SCBRAPOS
             (pcre2_match.c:5266-5346, site 5291). *)
          error_unported
      | 5 | 11 | 35 ->
          (* RM5 RM11 RM35 — STUB: M5 conditionals/recursion chunks. *)
          error_unported
      | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 36 ->
          (* RM12..RM19 RM36 — STUB: M5 verbs chunk. *)
          error_unported
      | 100 | 101 ->
          (* RM100 RM101 — STUB: M6 (OP_XCLASS repeats). *)
          error_unported
      | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 208 | 209 | 210 | 211
      | 212 | 213 | 214 | 215 | 216 | 217 | 218 | 219 | 220 | 221 | 222 | 223
      | 224 | 225 ->
          (* RM200..RM225 — STUB: M6/M7 UTF/UCP arms. *)
          error_unported
      | _ ->
          (* pcre2_match.c:6498-6499 — default: PCRE2_ERROR_INTERNAL. *)
          Errors.error_internal
  in
  (* pcre2_match.c:644-657 — set up the first frame and start processing
     with it (goto NEW_FRAME with F = frame 0, group_frame_type = 0). The
     frames-vector end (frames_top, 647) is tracked inside Frames. *)
  let fr = a.Frames.frames in
  fr.(Frames.slot_rdepth) <- 0 (* "Recursion" depth *);
  fr.(Frames.slot_capture_last) <- 0 (* Number of most recent capture *);
  fr.(Frames.slot_current_recurse) <- Frames.recurse_unset
  (* Not pattern recursing *);
  fr.(Frames.slot_start_match) <- start_eptr;
  fr.(Frames.slot_eptr) <- start_eptr (* Current data pointer and start match *);
  fr.(Frames.slot_mark) <- Frames.unset (* Most recent mark: NULL *);
  fr.(Frames.slot_offset_top) <- 0 (* End of captures within the frame *);
  fr.(Frames.slot_last_group_offset) <- Frames.unset
  (* Saved frame of most recent group *);
  (new_frame [@tailcall]) 0 start_ecode 0

(* ---------- Test-only single-attempt entry ---------- *)

(* TEST-ONLY (this chunk): run ONE match attempt of [code] against
   [subject] starting at [start], returning (rc, ovector). This is the
   scaffolding the pcre2_match driver chunk (pcre2_match.c:6530-7777)
   replaces: the ONLY argument validation is the BADOFFSET check on
   [start] (pre-staged from pcre2_match.c:6610 — it establishes the
   0 <= start_eptr <= end_subject invariant [match_] relies on); there is
   NO bump-along loop, NO anchored/startline/first-cu start optimization
   and NO NOTEMPTY retry here — callers position [start] themselves. It
   exists so the module-initialization asserts below (and the next opcode
   chunks) can drive [match_] end to end before Engine.exec is wired.

   The mb configuration transcribes the driver's field setup sites:
   partial from moptions (6658-6659), field block (6962-6979), fixed-LF
   newline default (6984-6995), limit defaults (7039-7046 with a default
   match context), per-attempt resets (7144, 7499-7508). [start_offset]
   models the driver's original start_offset when [start] simulates a
   bumped-along attempt position (default: [start]). *)
let match_internal ?(moptions = 0) ?(poptions = 0) ?start_offset
    ?(match_limit = Limits.match_limit)
    ?(match_limit_depth = Limits.match_limit_depth)
    ?(heap_limit = Limits.heap_limit) ?(oveccount = 1)
    ?(allowemptypartial = false) ~(code : Bytes.t) ~(top_bracket : int)
    (subject : string) (start : int) : int * int array =
  let start_offset = match start_offset with Some o -> o | None -> start in
  let match_data =
    (* Test-only init: all-unset so a nomatch is distinguishable. The
       driver chunk owns the real match_data lifecycle. *)
    { ovector = Array.make (2 * oveccount) Frames.unset; oveccount }
  in
  (* pcre2_match.c:6610 — if (start_offset > length) return
     PCRE2_ERROR_BADOFFSET. start_offset is PCRE2_SIZE (unsigned), so a
     negative OCaml int is a huge unsigned value, also > length
     (port-conventions §5: negative start offset or offset > subject
     length -> -33). This establishes 0 <= start_eptr <= end_subject for
     [match_] (see the match_block invariants). *)
  if start < 0 || start > String.length subject then
    (Errors.error_badoffset, match_data.ovector)
  else
    let mb =
      {
        match_limit;
        match_limit_depth;
        match_call_count = 0 (* pcre2_match.c:7506 *);
        hitend = false (* pcre2_match.c:7144 *);
        hasthen = false;
        allowemptypartial;
        subject;
        start_offset (* pcre2_match.c:6963 *);
        end_offset_top = 0 (* pcre2_match.c:7507 *);
        (* pcre2_match.c:6658-6659 — convert the partial matching flags
           into an integer. *)
        partial =
          (if not (Int.equal (moptions land Options.partial_hard) 0) then 2
           else if not (Int.equal (moptions land Options.partial_soft) 0) then 1
           else 0);
        bsr_convention = Options.bsr_unicode (* pcre2_match.c:6983 *);
        name_count = 0;
        name_entry_size = 0;
        name_table = Bytes.empty (* pcre2_match.c:6976-6978 *);
        start_code = code (* pcre2_match.c:6979 *);
        start_subject = 0 (* pcre2_match.c:6962 *);
        end_subject = String.length subject (* pcre2_match.c:6964 *);
        true_end_subject = String.length subject (* pcre2_match.c:6965 *);
        end_match_ptr = 0;
        start_used_ptr = start (* pcre2_match.c:7499 *);
        last_used_ptr = start (* pcre2_match.c:7500 *);
        mark = Frames.unset;
        nomatch_mark = Frames.unset (* pcre2_match.c:6971 *);
        moptions (* pcre2_match.c:7504 *);
        poptions (* pcre2_match.c:6969 *);
        (* pcre2_match.c:6984-6995 — fixed-LF newline (the build default);
           the driver chunk ports the full convention switch. *)
        nltype = Newline.nltype_fixed;
        nllen = 1;
        nl0 = Newline.char_lf;
        nl1 = 0;
      }
    in
    match Frames.create ~top_bracket ~heap_limit with
    | Error e -> (e, match_data.ovector)
    | Ok a ->
        let rc =
          match_ ~start_eptr:start ~start_ecode:0 ~top_bracket a match_data mb
        in
        (rc, match_data.ovector)

(* ---------- Inline sanity checks (module-initialization asserts) ---------- *)

(* Pin the dispatch-arm int literals that guard live behavior against the
   Opcodes constants (the stub groups are attribution-only: every stub
   returns the same marker). *)
let () =
  assert (Int.equal Opcodes.op_end 0);
  assert (Int.equal Opcodes.op_char 29);
  assert (Int.equal Opcodes.op_accept 164);
  assert (Int.equal Opcodes.op_close 166);
  (* Char-family opcodes now guarding live behavior (chars + char repeats
     chunk). *)
  assert (Int.equal Opcodes.op_chari 30);
  assert (Int.equal Opcodes.op_not 31);
  assert (Int.equal Opcodes.op_noti 32);
  assert (Int.equal Opcodes.op_star 33);
  assert (Int.equal Opcodes.op_minstar 34);
  assert (Int.equal Opcodes.op_plus 35);
  assert (Int.equal Opcodes.op_minplus 36);
  assert (Int.equal Opcodes.op_query 37);
  assert (Int.equal Opcodes.op_minquery 38);
  assert (Int.equal Opcodes.op_upto 39);
  assert (Int.equal Opcodes.op_minupto 40);
  assert (Int.equal Opcodes.op_exact 41);
  assert (Int.equal Opcodes.op_posstar 42);
  assert (Int.equal Opcodes.op_posplus 43);
  assert (Int.equal Opcodes.op_posquery 44);
  assert (Int.equal Opcodes.op_posupto 45);
  assert (Int.equal Opcodes.op_stari 46);
  assert (Int.equal Opcodes.op_minstari 47);
  assert (Int.equal Opcodes.op_plusi 48);
  assert (Int.equal Opcodes.op_minplusi 49);
  assert (Int.equal Opcodes.op_queryi 50);
  assert (Int.equal Opcodes.op_minqueryi 51);
  assert (Int.equal Opcodes.op_uptoi 52);
  assert (Int.equal Opcodes.op_minuptoi 53);
  assert (Int.equal Opcodes.op_exacti 54);
  assert (Int.equal Opcodes.op_posstari 55);
  assert (Int.equal Opcodes.op_posplusi 56);
  assert (Int.equal Opcodes.op_posqueryi 57);
  assert (Int.equal Opcodes.op_posuptoi 58);
  assert (Int.equal Opcodes.op_notstar 59);
  assert (Int.equal Opcodes.op_notminstar 60);
  assert (Int.equal Opcodes.op_notplus 61);
  assert (Int.equal Opcodes.op_notminplus 62);
  assert (Int.equal Opcodes.op_notquery 63);
  assert (Int.equal Opcodes.op_notminquery 64);
  assert (Int.equal Opcodes.op_notupto 65);
  assert (Int.equal Opcodes.op_notminupto 66);
  assert (Int.equal Opcodes.op_notexact 67);
  assert (Int.equal Opcodes.op_notposstar 68);
  assert (Int.equal Opcodes.op_notposplus 69);
  assert (Int.equal Opcodes.op_notposquery 70);
  assert (Int.equal Opcodes.op_notposupto 71);
  assert (Int.equal Opcodes.op_notstari 72);
  assert (Int.equal Opcodes.op_notminstari 73);
  assert (Int.equal Opcodes.op_notplusi 74);
  assert (Int.equal Opcodes.op_notminplusi 75);
  assert (Int.equal Opcodes.op_notqueryi 76);
  assert (Int.equal Opcodes.op_notminqueryi 77);
  assert (Int.equal Opcodes.op_notuptoi 78);
  assert (Int.equal Opcodes.op_notminuptoi 79);
  assert (Int.equal Opcodes.op_notexacti 80);
  assert (Int.equal Opcodes.op_notposstari 81);
  assert (Int.equal Opcodes.op_notposplusi 82);
  assert (Int.equal Opcodes.op_notposqueryi 83);
  assert (Int.equal Opcodes.op_notposuptoi 84);
  (* The rep tables (pcre2_match.c:128-150) and the op-offset indexing
     they are addressed with (pcre2_match.c:1254-1257, 1608-1611): each
     STAR..MINQUERY block is 6 consecutive opcodes. *)
  assert (Int.equal (Array.length rep_min) 11);
  assert (Int.equal (Array.length rep_max) 11);
  assert (Int.equal (Array.length rep_typ) 12);
  assert (Int.equal (Opcodes.op_minquery - Opcodes.op_star) 5);
  assert (Int.equal (Opcodes.op_minqueryi - Opcodes.op_stari) 5);
  assert (Int.equal (Opcodes.op_notminquery - Opcodes.op_notstar) 5);
  assert (Int.equal (Opcodes.op_notminqueryi - Opcodes.op_notstari) 5);
  assert (
    Int.equal reptype_min 0 && Int.equal reptype_max 1
    && Int.equal reptype_pos 2);
  (* The "no top-level case" default group boundaries. *)
  assert (Int.equal Opcodes.op_crstar 98);
  assert (Int.equal Opcodes.op_crposrange 109);
  assert (Int.equal Opcodes.op_cref 145);
  assert (Int.equal Opcodes.op_true 150);
  assert (Int.equal Opcodes.op_define 168);
  assert (Int.equal Opcodes.op_table_length 171);
  (* RM label constants match the C enum (pcre2_match.c:155-169). *)
  assert (Int.equal rm1 1);
  assert (Int.equal rm37 37);
  assert (Int.equal rm100 100);
  assert (Int.equal rm101 101);
  assert (Int.equal rm200 200);
  assert (Int.equal rm225 225);
  (* MATCH_xxx internals (pcre2_match.c:87-103). *)
  assert (Int.equal match_match 1);
  assert (Int.equal match_nomatch 0);
  assert (Int.equal match_accept (-999));
  assert (Int.equal match_ketrpos (-998));
  assert (Int.equal match_backtrack_min match_commit);
  assert (Int.equal match_backtrack_max match_then);
  assert (match_backtrack_min < match_prune && match_prune < match_skip);
  assert (match_skip < match_skip_arg && match_skip_arg < match_then);
  (* The unported marker collides with nothing it can meet. *)
  assert (error_unported < -66 && error_unported > match_then)

(* Hand-assembled programs: OP_BRA belongs to a later chunk, so these
   exercise OP_CHAR / OP_END directly (the compiler always wraps patterns
   in OP_BRA..OP_KET, pcre2_compile.c:10570-10604). *)
let mk_code (units : int list) : Bytes.t =
  let b = Bytes.create (List.length units) in
  List.iteri (fun i u -> Bytes.set b i (Char.chr u)) units;
  b

(* OP_CHAR chain + OP_END: the equivalent of /abc/ without the outer
   bracket. *)
let () =
  let abc =
    mk_code
      [
        Opcodes.op_char;
        Char.code 'a';
        Opcodes.op_char;
        Char.code 'b';
        Opcodes.op_char;
        Char.code 'c';
        Opcodes.op_end;
      ]
  in
  (* Anchored attempt at offset 1 of "xabc": MATCH_MATCH with ovector
     (1, 4). *)
  (match match_internal ~code:abc ~top_bracket:0 "xabc" 1 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 1);
      assert (Int.equal ov.(1) 4));
  (* "abd": the third OP_CHAR consults 'd', fails, backtracks to frame 0
     -> MATCH_NOMATCH; the ovector is untouched. *)
  (match match_internal ~code:abc ~top_bracket:0 "abd" 0 with
  | rc, ov ->
      assert (Int.equal rc match_nomatch);
      assert (Int.equal ov.(0) Frames.unset);
      assert (Int.equal ov.(1) Frames.unset));
  (* Attempt at an offset where the subject runs out: plain NOMATCH
     without partial flags... *)
  (match match_internal ~code:abc ~top_bracket:0 "xab" 1 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* ...soft partial: SCHECK_PARTIAL sets hitend and falls through to
     NOMATCH... *)
  (match
     match_internal ~moptions:Options.partial_soft ~code:abc ~top_bracket:0
       "xab" 1
   with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* ...hard partial: SCHECK_PARTIAL returns PCRE2_ERROR_PARTIAL from the
     match (pcre2_match.c:542). *)
  (match
     match_internal ~moptions:Options.partial_hard ~code:abc ~top_bracket:0
       "xab" 1
   with
  | rc, _ -> assert (Int.equal rc Errors.error_partial));
  (* ENDANCHORED (pattern option): /abc/ matches "abcx" up to eptr 3 <
     end_subject 4, so OP_END backtracks -> NOMATCH; exact-length subject
     still matches. *)
  (match
     match_internal ~poptions:Options.endanchored ~code:abc ~top_bracket:0
       "abcx" 0
   with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  match
    match_internal ~moptions:Options.endanchored ~code:abc ~top_bracket:0 "abc"
      0
  with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(1) 3)

(* Bare OP_END: the empty pattern, and the empty-match protocol
   (pcre2_match.c:881-895). *)
let () =
  let empty = mk_code [ Opcodes.op_end ] in
  (* Empty match at the start: ovector (0, 0). *)
  (match match_internal ~code:empty ~top_bracket:0 "abc" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 0));
  (* NOTEMPTY rejects any empty match. *)
  (match
     match_internal ~moptions:Options.notempty ~code:empty ~top_bracket:0 "abc"
       0
   with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* NOTEMPTY_ATSTART rejects the empty match only at
     start_subject + start_offset... *)
  (match
     match_internal ~moptions:Options.notempty_atstart ~code:empty
       ~top_bracket:0 "abc" 0
   with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* ...a bumped-along attempt (start 1, original start_offset 0) is an
     acceptable empty match, ovector (1, 1). *)
  match
    match_internal ~moptions:Options.notempty_atstart ~start_offset:0
      ~code:empty ~top_bracket:0 "abc" 1
  with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 1);
      assert (Int.equal ov.(1) 1)

(* Limit trips at the C's exact sites: match_call_count is bumped then
   compared >= match_limit, and Frdepth (= 0 for the first frame) is
   compared >= match_limit_depth, both at NEW_FRAME fallthrough
   (pcre2_match.c:782-783); the heap limit trips in Frames.create
   (pcre2_match.c:7055-7060). *)
let () =
  let abc = mk_code [ Opcodes.op_char; Char.code 'a'; Opcodes.op_end ] in
  (match match_internal ~match_limit:0 ~code:abc ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_matchlimit) (* -47 *));
  (match match_internal ~match_limit_depth:0 ~code:abc ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_depthlimit) (* -53 *));
  (match match_internal ~heap_limit:0 ~code:abc ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_heaplimit) (* -63 *));
  (* One frame per attempt while no opcode RMATCHes: limit 1 is enough. *)
  match match_internal ~match_limit:1 ~code:abc ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc match_match)

(* BADOFFSET at the seam (pcre2_match.c:6610; port-conventions §5): a
   negative start (unsigned PCRE2_SIZE in the C, hence > length) and
   start > length both give -33; start = length is a legal attempt
   position (the C check is strict >). *)
let () =
  let empty = mk_code [ Opcodes.op_end ] in
  (match match_internal ~code:empty ~top_bracket:0 "a" (-3) with
  | rc, _ -> assert (Int.equal rc Errors.error_badoffset));
  (match match_internal ~code:empty ~top_bracket:0 "a" 2 with
  | rc, _ -> assert (Int.equal rc Errors.error_badoffset));
  match match_internal ~code:empty ~top_bracket:0 "a" 1 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 1);
      assert (Int.equal ov.(1) 1)

(* Unset-capture fill on success (pcre2_match.c:934-939): with
   top_bracket = 2 and no OP_CBRA yet, offset_top stays 0 and both pairs
   read PCRE2_UNSET; an oveccount smaller than top_bracket + 1 clips the
   copy. *)
let () =
  let a_end = mk_code [ Opcodes.op_char; Char.code 'a'; Opcodes.op_end ] in
  (match match_internal ~oveccount:3 ~code:a_end ~top_bracket:2 "abc" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal (Array.length ov) 6);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 1);
      for i = 2 to 5 do
        assert (Int.equal ov.(i) Frames.unset)
      done);
  match match_internal ~oveccount:1 ~code:a_end ~top_bracket:2 "abc" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal (Array.length ov) 2);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 1)

(* Malformed / not-yet-ported programs: OP_DEFINE has no case in the C
   switch -> PCRE2_ERROR_INTERNAL; a stubbed opcode returns the
   scaffolding marker (this last check is removed with the stub). *)
let () =
  (match
     match_internal ~code:(mk_code [ Opcodes.op_define ]) ~top_bracket:0 "a" 0
   with
  | rc, _ -> assert (Int.equal rc Errors.error_internal));
  match
    match_internal
      ~code:(mk_code [ Opcodes.op_bra; 0; 3; Opcodes.op_end ])
      ~top_bracket:0 "a" 0
  with
  | rc, _ -> assert (Int.equal rc error_unported)

(* Direct match_block block: observability the (rc, ovector) surface
   hides — hitend, mark, last_used_ptr, counters — asserted against the
   C sites; also documents the direct [match_] call shape for the driver
   chunk. *)
let () =
  let abc =
    mk_code
      [
        Opcodes.op_char;
        Char.code 'a';
        Opcodes.op_char;
        Char.code 'b';
        Opcodes.op_char;
        Char.code 'c';
        Opcodes.op_end;
      ]
  in
  let mk_mb ?(moptions = 0) subject start =
    {
      match_limit = Limits.match_limit;
      match_limit_depth = Limits.match_limit_depth;
      match_call_count = 0;
      hitend = false;
      hasthen = false;
      allowemptypartial = false;
      subject;
      start_offset = start;
      end_offset_top = 0;
      partial =
        (if not (Int.equal (moptions land Options.partial_hard) 0) then 2
         else if not (Int.equal (moptions land Options.partial_soft) 0) then 1
         else 0);
      bsr_convention = Options.bsr_unicode;
      name_count = 0;
      name_entry_size = 0;
      name_table = Bytes.empty;
      start_code = abc;
      start_subject = 0;
      end_subject = String.length subject;
      true_end_subject = String.length subject;
      end_match_ptr = 0;
      start_used_ptr = start;
      last_used_ptr = start;
      mark = Frames.unset;
      nomatch_mark = Frames.unset;
      moptions;
      poptions = 0;
      nltype = Newline.nltype_fixed;
      nllen = 1;
      nl0 = Newline.char_lf;
      nl1 = 0;
    }
  in
  let run mb =
    match Frames.create ~top_bracket:0 ~heap_limit:Limits.heap_limit with
    | Error _ -> assert false
    | Ok a ->
        match_ ~start_eptr:mb.start_used_ptr ~start_ecode:0 ~top_bracket:0 a
          { ovector = Array.make 2 Frames.unset; oveccount = 1 }
          mb
  in
  (* Soft partial on "ab": rc NOMATCH but hitend set (SCHECK_PARTIAL,
     540-541: eptr 2 > start_used_ptr 0); last_used_ptr updated to the
     consulted end (RETURN_SWITCH, 6470); exactly one frame was created;
     mark never set. *)
  (let mb = mk_mb ~moptions:Options.partial_soft "ab" 0 in
   let rc = run mb in
   assert (Int.equal rc match_nomatch);
   assert mb.hitend;
   assert (Int.equal mb.last_used_ptr 2);
   assert (Int.equal mb.match_call_count 1);
   assert (Int.equal mb.mark Frames.unset));
  (* Success on "abc": end_match_ptr / end_offset_top / mark / last_used
     recorded at 926-929. *)
  let mb = mk_mb "abc" 0 in
  let rc = run mb in
  assert (Int.equal rc match_match);
  assert (Int.equal mb.end_match_ptr 3);
  assert (Int.equal mb.end_offset_top 0);
  assert (Int.equal mb.mark Frames.unset);
  assert (Int.equal mb.last_used_ptr 3);
  assert (not mb.hitend)

(* OP_CHARI: caseless single characters through the lcc table
   (pcre2_match.c:1097-1103); SCHECK_PARTIAL at the subject end
   (1035-1039). *)
let () =
  let ax =
    mk_code
      [
        Opcodes.op_chari;
        Char.code 'a';
        Opcodes.op_chari;
        Char.code 'X';
        Opcodes.op_end;
      ]
  in
  List.iter
    (fun s ->
      match match_internal ~code:ax ~top_bracket:0 s 0 with
      | rc, ov ->
          assert (Int.equal rc match_match);
          assert (Int.equal ov.(0) 0);
          assert (Int.equal ov.(1) 2))
    [ "ax"; "aX"; "Ax"; "AX" ];
  (match match_internal ~code:ax ~top_bracket:0 "ay" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* Subject runs out before the second CHARI: NOMATCH normally, -2 under
     hard partial. *)
  (match match_internal ~code:ax ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  match
    match_internal ~moptions:Options.partial_hard ~code:ax ~top_bracket:0 "a" 0
  with
  | rc, _ -> assert (Int.equal rc Errors.error_partial)

(* OP_NOT / OP_NOTI: negated single characters (pcre2_match.c:1165-1173);
   the caseless form also rejects the fcc other case. *)
let () =
  let not_a = mk_code [ Opcodes.op_not; Char.code 'a'; Opcodes.op_end ] in
  (match match_internal ~code:not_a ~top_bracket:0 "b" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 1));
  (match match_internal ~code:not_a ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* Caseful NOT: the other case is NOT excluded. *)
  (match match_internal ~code:not_a ~top_bracket:0 "A" 0 with
  | rc, _ -> assert (Int.equal rc match_match));
  let noti_a = mk_code [ Opcodes.op_noti; Char.code 'a'; Opcodes.op_end ] in
  (match match_internal ~code:noti_a ~top_bracket:0 "b" 0 with
  | rc, _ -> assert (Int.equal rc match_match));
  (match match_internal ~code:noti_a ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (match match_internal ~code:noti_a ~top_bracket:0 "A" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* Empty subject: eptr = start_used_ptr and no allowemptypartial, so
     SCHECK_PARTIAL does not fire even under hard partial (537-543). *)
  match
    match_internal ~moptions:Options.partial_hard ~code:noti_a ~top_bracket:0 ""
      0
  with
  | rc, _ -> assert (Int.equal rc match_nomatch)

(* TEST-ONLY: real compiled programs for the repeat arms. The compiler
   always wraps the pattern in OP_BRA .. OP_KET (pcre2_compile.c:
   10570-10604); the bracket opcodes belong to a later chunk, so strip the
   wrapper — asserting its exact shape — and terminate the body with
   OP_END. Returns (body code, top_bracket). *)
let compile_body (pattern : string) : Bytes.t * int =
  match Compile.pcre2_compile pattern ~options:0 with
  | Error _ -> assert false
  | Ok re ->
      let code = re.Compile.code in
      assert (Int.equal (Char.code (Bytes.get code 0)) Opcodes.op_bra);
      let ket = Compile.get code 1 in
      assert (Int.equal (Char.code (Bytes.get code ket)) Opcodes.op_ket);
      assert (
        Int.equal
          (Char.code (Bytes.get code (ket + 1 + Limits.link_size)))
          Opcodes.op_end);
      let body_len = ket - (1 + Limits.link_size) in
      let body = Bytes.create (body_len + 1) in
      Bytes.blit code (1 + Limits.link_size) body 0 body_len;
      Bytes.set body body_len (Char.chr Opcodes.op_end);
      (body, re.Compile.top_bracket)

(* Single-character repeats over real compiled bodies: all three match
   strategies (exact/min-only, minimize, maximize) in both cases, plus
   the possessive forms and SCHECK_PARTIAL placement inside the loops.
   Each attempt is anchored at offset 0 (match_internal has no bump-along
   loop), so a NOMATCH here is a statement about backtracking behavior at
   that fixed start. *)
let () =
  let run ?moptions pat subj =
    let code, top_bracket = compile_body pat in
    match_internal ?moptions ~code ~top_bracket subj 0
  in
  let expect_match pat subj e =
    match run pat subj with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal ov.(0) 0);
        assert (Int.equal ov.(1) e)
  in
  let expect_nomatch pat subj =
    match run pat subj with rc, _ -> assert (Int.equal rc match_nomatch)
  in
  let expect_hard_partial pat subj =
    match run ~moptions:Options.partial_hard pat subj with
    | rc, _ -> assert (Int.equal rc Errors.error_partial)
  in
  (* a{2,4} = OP_EXACT 2 + OP_UPTO 2 (greedy): consume the maximum, then
     back off in place (RM28; the `<=` break tries Lstart_eptr without a
     new frame). *)
  expect_match "a{2,4}b" "aaab" 4;
  expect_match "a{2,4}b" "aab" 3;
  expect_nomatch "a{2,4}b" "ab";
  expect_match "a{2,4}b" "aaaab" 5;
  (* The maximum run ends at 4; backing off to the minimum at 2 never
     finds 'b'. *)
  expect_nomatch "a{2,4}b" "aaaaab";
  (* RM28 resume then the break-at-Lstart in-place tail. *)
  expect_match "a{2,4}ab" "aaab" 4;
  (* OP_MINUPTO: RM27 iterations up to the bound... *)
  expect_match "a{2,4}?b" "aaab" 4;
  (* ...and the Lmin++ >= Lmax refusal at the bound. *)
  expect_nomatch "a{2,4}?b" "aaaaab";
  (* Lazy star (OP_MINSTAR, RM27). *)
  expect_match "a*?b" "aaab" 4;
  (* Plus: minimum of one. *)
  expect_nomatch "a+b" "b";
  expect_match "a+b" "ab" 2;
  (* Query: 0 or 1, greedy. *)
  expect_match "ab?c" "abc" 3;
  expect_match "ab?c" "ac" 2;
  (* Caseless repeats: OP_STARI maximize (RM26 resume at "ab*bc"),
     OP_MINSTARI minimize (RM25), and the full descend-to-Lstart NOMATCH. *)
  expect_match "(?i)ab*c" "aBBBc" 5;
  expect_match "(?i)ab*bc" "aBBc" 4;
  expect_match "(?i)ab*?c" "aBBc" 4;
  expect_nomatch "(?i)ab*d" "aBBc";
  (* Caseful analogues (RM28 resume / descend). *)
  expect_match "ab*bc" "abbc" 4;
  expect_nomatch "ab*d" "abbc";
  (* Possessive: no backing up once the run is consumed. *)
  expect_match "a*+b" "aaab" 4;
  expect_nomatch "a*+ab" "aaab";
  expect_match "a++b" "aaab" 4;
  expect_nomatch "a++ab" "aaab";
  expect_match "a{2,4}+b" "aaab" 4;
  expect_nomatch "a{2,4}+ab" "aaab";
  (* SCHECK_PARTIAL inside the loops: the EXACT min loop, the minimize
     resume (RM27), and the greedy scan all hit the subject end. *)
  expect_hard_partial "a{3}" "aa";
  expect_hard_partial "a*?b" "aaa";
  expect_hard_partial "a*b" "aaa"

(* Negated single-character repeats (REPEATNOTCHAR) over compiled
   bodies: [^b] compiles to OP_NOT via the one-char negated class
   optimization, and its quantifiers to the NOT repeat opcodes. *)
let () =
  let run ?moptions pat subj =
    let code, top_bracket = compile_body pat in
    match_internal ?moptions ~code ~top_bracket subj 0
  in
  let expect_match pat subj e =
    match run pat subj with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal ov.(0) 0);
        assert (Int.equal ov.(1) e)
  in
  let expect_nomatch pat subj =
    match run pat subj with rc, _ -> assert (Int.equal rc match_nomatch)
  in
  (* OP_NOTSTAR maximize (scan stops at 'b'); RM32 resume. *)
  expect_match "[^b]*b" "aaab" 4;
  expect_match "[^b]*ab" "aaab" 4;
  (* OP_NOTMINSTAR minimize (RM31). *)
  expect_match "[^b]*?b" "aab" 3;
  (* Caseless: OP_NOTSTARI maximize (RM30) and minimize (RM29); Loc
     rejection of the other case. *)
  expect_match "(?i)[^b]*ab" "aAab" 4;
  expect_match "(?i)[^b]*?c" "aAc" 3;
  expect_match "(?i)[^b]{2}c" "aac" 3;
  expect_nomatch "(?i)[^b]{2}c" "aBc";
  (* Possessive NOT. *)
  expect_match "[^b]*+b" "aab" 3;
  expect_nomatch "[^b]*+ab" "aab";
  (* SCHECK_PARTIAL in the NOTEXACT min loop. *)
  match
    let code, top_bracket = compile_body "[^b]{3}" in
    match_internal ~moptions:Options.partial_hard ~code ~top_bracket "aa" 0
  with
  | rc, _ -> assert (Int.equal rc Errors.error_partial)
