(* Interpreter dispatch skeleton for the pure-OCaml PCRE2 10.44 port
   (8-bit library).

   Ported from vendor/pcre2/src/pcre2_match.c: the match() function shell
   and first-frame setup (565-657), the RMATCH/RRETURN backtracking
   protocol as data (550-562; MATCH_RECURSE 662-754 via Frames.push;
   NEW_FRAME 758-783; RETURN_SWITCH 6462-6501), the main dispatch loop
   (790-798, 6445-6457), OP_END (876-940) and OP_CHAR (992-1025) as the
   only two live opcodes, and the match_block structure
   (pcre2_intmodedep.h:864-906). Every other opcode that the C switch
   handles gets an exhaustive STUB arm returning the distinctive
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
     negative (eptr only advances from start_eptr in the live arms); the
     OP_CHAR arm's String.unsafe_get bound proof relies on this;
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
  (* pcre2_match.c:630-637 — UTF flag (ucp joins with the M7 arms). *)
  let utf = not (Int.equal (mb.poptions land Options.utf) 0) in

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
        (* OP_CHARI (pcre2_match.c:1028-1105) — STUB: chars + char repeats
           chunk. *)
        error_unported
    | 31 | 32 ->
        (* OP_NOT, OP_NOTI (pcre2_match.c:1108-1179) — STUB: chars + char
           repeats chunk. *)
        error_unported
    | 41 | 54 | 45 | 58 | 39 | 52 | 40 | 53 | 42 | 55 | 43 | 56 | 44 | 57 | 33
    | 46 | 34 | 47 | 35 | 48 | 36 | 49 | 37 | 50 | 38 | 51 ->
        (* OP_EXACT(I), OP_POSUPTO(I), OP_UPTO(I), OP_MINUPTO(I),
           OP_POSSTAR(I), OP_POSPLUS(I), OP_POSQUERY(I), OP_STAR(I),
           OP_MINSTAR(I), OP_PLUS(I), OP_MINPLUS(I), OP_QUERY(I),
           OP_MINQUERY(I) (pcre2_match.c:1182-1539, REPEATCHAR) — STUB:
           chars + char repeats chunk. *)
        error_unported
    | 67 | 80 | 65 | 78 | 66 | 79 | 68 | 81 | 69 | 82 | 70 | 83 | 71 | 84 | 59
    | 72 | 60 | 73 | 61 | 74 | 62 | 75 | 63 | 76 | 64 | 77 ->
        (* OP_NOTEXACT(I), OP_NOTUPTO(I), OP_NOTMINUPTO(I),
           OP_NOTPOSSTAR(I), OP_NOTPOSPLUS(I), OP_NOTPOSQUERY(I),
           OP_NOTPOSUPTO(I), OP_NOTSTAR(I), OP_NOTMINSTAR(I),
           OP_NOTPLUS(I), OP_NOTMINPLUS(I), OP_NOTQUERY(I),
           OP_NOTMINQUERY(I) (pcre2_match.c:1542-1929, REPEATNOTCHAR) —
           STUB: chars + char repeats chunk. *)
        error_unported
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
      | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 ->
          (* RM25..RM32 — STUB: chars + char repeats chunk. *)
          error_unported
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
  (* DEVIATION (M1 scaffolding): no arm calls [rmatch] until the chunks
     with RMATCH sites land; this reference keeps the protocol compiled
     under warnings-as-errors. Remove with the first real RMATCH site. *)
  let _ = rmatch in

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
