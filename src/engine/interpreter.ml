(* Interpreter dispatch skeleton for the pure-OCaml PCRE2 10.44 port
   (8-bit library).

   Ported from vendor/pcre2/src/pcre2_match.c: the match() function shell
   and first-frame setup (565-657), the RMATCH/RRETURN backtracking
   protocol as data (550-562; MATCH_RECURSE 662-754 via Frames.push;
   NEW_FRAME 758-783; RETURN_SWITCH 6462-6501), the main dispatch loop
   (790-798, 6445-6457), OP_END (876-940), the char family OP_CHAR/
   OP_CHARI/OP_NOT/OP_NOTI and the single-character repeat machinery
   REPEATCHAR/REPEATNOTCHAR with resume labels RM25-RM32 (992-1916,
   non-UTF/non-UCP arms; UTF -> M6, UCP -> M7), the bit-mapped class
   family OP_CLASS/OP_NCLASS with resume labels RM23/RM24 (1919-2172),
   the character-type singles OP_ANY..OP_VSPACE (943-989, 2305-2476) and
   the TYPE repeat machinery REPEATTYPE with resume labels RM33/RM34
   (2651-5005) (all non-UTF/non-UCP arms), the backreference family
   match_ref and OP_REF/OP_REFI/OP_DNREF/OP_DNREFI with the repeat
   machinery and resume labels RM20-RM22 (338-481, 4980-5194; non-UTF/
   non-UCP arms), the anchors and simple
   assertions OP_CIRC(M)/OP_DOLL(M)/OP_SOD/OP_SOM/OP_SET_SOM/OP_EOD/
   OP_EODN/word boundaries (6132-6338, non-UTF/non-UCP arms), the
   bracket/alternation/ket family OP_BRAZERO/OP_BRAMINZERO/OP_SKIPZERO
   (5224-5246), OP_BRA/OP_CBRA/OP_SCBRA and the shared GROUPLOOP
   (5349-5411; the OP_ONCE/OP_SCRIPT_RUN/OP_SBRA head at 5391-5394 is
   live, their ket actions defer to M4/M7), OP_ALT and OP_KET/OP_KETRMIN/
   OP_KETRMAX with resume labels RM1/RM2/RM6/RM7/RM9/RM10 (5893-6127;
   OP_KETRPOS and the condassert/assertion/recursion ket actions defer to
   M4/M5), and the match_block structure (pcre2_intmodedep.h:864-906).
   Every other opcode
   that the C switch handles gets an exhaustive STUB arm returning the
   distinctive [error_unported] marker; the following M1-M8 chunks
   REPLACE arm bodies only.

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
   owning chunk. Now that the pcre2_match driver below wires Engine.exec,
   a pattern that compiles but reaches an unported arm surfaces this code
   at the seam ([Engine.Error -979]) — deliberately loud, and impossible
   to mistake for a real PCRE2 code. *)
let error_unported = -979

(* ---------- The match data block (result surface) ---------- *)

(* pcre2_intmodedep.h:656-672 — pcre2_real_match_data, reduced to what the
   match() core and the pcre2_match() driver read and write: rc (670),
   mark (660), startchar (666), leftchar (664), rightchar (665),
   oveccount (669) and ovector (671). The other C fields have these owners:
   - subject / subject_length (659, 663): the subject string travels
     alongside at the OCaml seam (no pointer to return);
   - flags (668) / matchedby (667): PCRE2_MD_COPIED_SUBJECT and the
     matched-by tag have no meaning without the C ABI — dropped;
   - code (658): dropped (the caller keeps its own [re]);
   - heapframes / heapframes_size (661-662): Frames.t (frames.ml
     DEVIATION: no cached vector across matches);
   - memctl (657): dropped (GC). *)
type match_data = {
  ovector : int array;
      (* PCRE2_SIZE ovector[]: oveccount pairs of subject offsets; unset
         pair = (-1, -1) (Frames.unset) *)
  oveccount : int; (* uint16_t oveccount: number of pairs *)
  mutable rc : int; (* int rc (670): the match result code *)
  mutable startchar : int;
      (* PCRE2_SIZE startchar (666): offset where the match attempt
         started (pcre2_get_startchar) *)
  mutable leftchar : int;
      (* PCRE2_SIZE leftchar (664): offset of the leftmost character
         consulted *)
  mutable rightchar : int;
      (* PCRE2_SIZE rightchar (665): offset of the rightmost character
         consulted *)
  mutable mark : int;
      (* PCRE2_SPTR mark (665): mark name to pass back — an offset into
         the compiled code (as in the frames; -1 = NULL) *)
}

(* ---------- The match block ("static" data) ---------- *)

(* pcre2_intmodedep.h:864-906 — structure for passing "static" information
   around between the functions doing traditional NFA matching. Fields are
   transcribed in struct order; C pointers into the subject / compiled
   code become int offsets, so the base values themselves are carried as
   [subject] (DEVIATION: the C embeds the base in its pointers) and
   [start_code].

   Invariants relied on by the dispatch loop (established by the caller —
   the [pcre2_match] driver below, or the test-only [match_internal]):
   - 0 <= start_subject <= end_subject <= true_end_subject
                                        <= String.length subject;
   - 0 <= start_eptr <= end_subject at [match_] entry — enforced by the
     caller's BADOFFSET check (pcre2_match.c:6610; both [pcre2_match] and
     [match_internal] validate the start offset before calling [match_],
     and the driver's bump-along loop never moves start_match past
     end_subject). Subject positions in frame slots then never go
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
  mutable nllen : int;
      (* uint32_t nllen (901): newline string length; mutable because
         IS_NEWLINE/WAS_NEWLINE pass &mb->nllen to PRIV(is_newline)/
         PRIV(was_newline), which store the length of the newline actually
         found for the ANY/ANYCRLF conventions
         (pcre2_internal.h:496-521) *)
  nl0 : int;
  nl1 : int;
      (* PCRE2_UCHAR nl[4] (902) — newline string when fixed; only
         elements 0..1 are ever used (nllen is 1 or 2), as in
         Compile.compile_block *)
}

(* pcre2_internal.h:424-427 — HSPACE_BYTE_CASES: HT, SPACE, NBSP. The
   8-bit code-unit switches in pcre2_match.c use only these (the
   HSPACE_MULTIBYTE_CASES arms are compiled out at
   PCRE2_CODE_UNIT_WIDTH == 8). *)
let hspace_byte (c : int) : bool =
  Int.equal c 0x09 || Int.equal c 0x20 || Int.equal c 0xa0

(* pcre2_internal.h:440-445 — VSPACE_BYTE_CASES: LF, VT, FF, CR, NEL. *)
let vspace_byte (c : int) : bool =
  Int.equal c Newline.char_lf
  || Int.equal c Newline.char_vt
  || Int.equal c Newline.char_ff
  || Int.equal c Newline.char_cr
  || Int.equal c Newline.char_nel

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

  (* pcre2_internal.h:496-507 — IS_NEWLINE(p): NLBLOCK is mb, PSEND is
     end_subject (pcre2_match.c:60-66). For the non-fixed conventions
     PRIV(is_newline) writes the length of the newline it found through
     &mb->nllen; [nl_scratch] is that out-parameter, preallocated once per
     match so the dispatch loop stays allocation-free, and copied to
     mb.nllen exactly when the C writes it (TRUE returns only —
     Newline.is_newline leaves the ref untouched on FALSE). *)
  let nl_scratch = ref 0 in
  let is_newline_at (p : int) : bool =
    if not (Int.equal mb.nltype Newline.nltype_fixed) then (
      p < mb.end_subject
      &&
      let hit =
        Newline.is_newline mb.subject mb.nltype p mb.end_subject nl_scratch utf
      in
      if hit then mb.nllen <- !nl_scratch;
      hit)
    else
      p <= mb.end_subject - mb.nllen
      && Int.equal
           (* safe: 0 <= start_subject <= p (callers pass eptr values; mb
              invariant) and p <= end_subject - nllen < end_subject <=
              String.length mb.subject (nllen is 1 or 2, mb invariant) *)
           (Char.code (String.unsafe_get mb.subject p))
           mb.nl0
      && (Int.equal mb.nllen 1
         || Int.equal (Char.code (String.unsafe_get mb.subject (p + 1))) mb.nl1
         )
  in
  (* pcre2_internal.h:510-521 — WAS_NEWLINE(p): PSSTART is start_subject
     (pcre2_match.c:60-66). *)
  let was_newline_at (p : int) : bool =
    if not (Int.equal mb.nltype Newline.nltype_fixed) then (
      p > mb.start_subject
      &&
      let hit =
        Newline.was_newline mb.subject mb.nltype p mb.start_subject nl_scratch
          utf
      in
      if hit then mb.nllen <- !nl_scratch;
      hit)
    else
      p >= mb.start_subject + mb.nllen
      && Int.equal
           (* safe: p - nllen >= start_subject >= 0 (checked above) and
              p - nllen < p <= end_subject <= String.length mb.subject
              (callers pass eptr values; mb invariant) *)
           (Char.code (String.unsafe_get mb.subject (p - mb.nllen)))
           mb.nl0
      && (Int.equal mb.nllen 1
         || Int.equal
              (Char.code (String.unsafe_get mb.subject (p - mb.nllen + 1)))
              mb.nl1)
  in
  (* pcre2_match.c:609 — PCRE2_SPTR branch_end = NULL: a match()-local;
     C:607 says such locals do not NEED to survive RMATCH, but this one in
     fact persists for the whole match() invocation — and must (the
     ALT->KET adjacency protocol reads it after resumes). OP_ALT records the end of
     the matched branch in it (5895); the OP_KET branch_start scan
     consumes and resets it (5913-5917). -1 = NULL (code offsets are
     >= 0). Preallocated once per match, like [nl_scratch], so the
     dispatch loop stays allocation-free. *)
  let branch_end = ref (-1) in
  (* pcre2_match.c:613 + 353 — PCRE2_SIZE length: the match()-local that
     the backreference arms pass to match_ref() as its lengthptr
     out-parameter (both the C's `length` at 5047 and the block-local
     `slength`s at 5080/5101/5128/5179 land here). Preallocated once per
     match, like [nl_scratch], so the dispatch loop stays allocation-free;
     a single cell suffices because match_ref never re-enters the
     dispatch loop, so at most one call's result is ever pending. *)
  let ref_length = ref 0 in
  (* pcre2_match.c:1930 + 2014 — Lbyte_map[fc/8] & (1u << (fc&7)): probe
     the 32-byte class bitmap saved at Lbyte_map_address (temp_sptr[1]).
     In bounds: the map is part of the OP_CLASS/OP_NCLASS item in the
     compiled program (mb invariant) and fc lsr 3 <= 31. *)
  let class_bit (f : int) (fc : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    Char.code
      (Bytes.get mb.start_code (fr.(fb + Frames.slot_temp_sptr_1) + (fc lsr 3)))
    land (1 lsl (fc land 7))
  in
  (* Bounds contract shared by the three match_ref compare loops below
     ([match_ref] establishes it): [p] ranges over the captured substring
     — p0 = start_subject + Fovector[Loffset] up to (exclusive)
     start_subject + Fovector[Loffset+1] — whose bounds are former eptr
     values recorded as eptr - start_subject by the capturing-ket writes
     (pcre2_match.c:6077-6084), so 0 <= start_subject <= p and, while
     length > 0, p < start_subject + Fovector[Loffset+1] <= end_subject
     <= String.length mb.subject. [eptr] starts at Feptr >= 0 (mb
     invariant). *)
  (* pcre2_match.c:438-451 — match_ref()'s caseless compare loop, not in
     UTF or UCP mode: fold both code units through the lcc table.
     Returns 0 all matched / -1 no match / 1 partial. *)
  let rec match_ref_ci (p : int) (eptr : int) (length : int) : int =
    if length <= 0 then 0
    else if eptr >= mb.end_subject then 1 (* partial match, 443 *)
    else
      (* safe: eptr < mb.end_subject <= String.length mb.subject (checked
         above), eptr >= 0; p in the captured substring (contract above) *)
      let cc = Char.code (String.unsafe_get mb.subject eptr) in
      let cp = Char.code (String.unsafe_get mb.subject p) in
      if not (Int.equal (Chartables.lcc cp) (Chartables.lcc cc)) then -1
        (* no match, 446-447 *)
      else (match_ref_ci [@tailcall]) (p + 1) (eptr + 1) (length - 1)
  in
  (* pcre2_match.c:460-467 — match_ref()'s caseful compare loop for
     partial matching: unit by unit, checking the subject end before each
     unit. *)
  let rec match_ref_cs_partial (p : int) (eptr : int) (length : int) : int =
    if length <= 0 then 0
    else if eptr >= mb.end_subject then 1 (* partial match, 464 *)
    else if
      not
        (Int.equal
           (* safe: eptr < mb.end_subject <= String.length mb.subject
              (checked above), eptr >= 0; p in the captured substring
              (contract above) *)
           (Char.code (String.unsafe_get mb.subject p))
           (Char.code (String.unsafe_get mb.subject eptr)))
    then -1 (* no match, 465 *)
    else (match_ref_cs_partial [@tailcall]) (p + 1) (eptr + 1) (length - 1)
  in
  (* pcre2_match.c:474 — memcmp(p, eptr, CU2BYTES(length)) != 0 as an
     equality scan (only equality is consulted). Caller checked
     end_subject - eptr >= length. *)
  let rec match_ref_memcmp (p : int) (eptr : int) (length : int) : bool =
    length <= 0
    || Int.equal
         (* safe: eptr + length <= mb.end_subject <= String.length
            mb.subject (caller's 473 check), eptr >= 0; p in the captured
            substring (contract above) *)
         (Char.code (String.unsafe_get mb.subject p))
         (Char.code (String.unsafe_get mb.subject eptr))
       && (match_ref_memcmp [@tailcall]) (p + 1) (eptr + 1) (length - 1)
  in
  (* pcre2_match.c:338-481 — match_ref(): match a back-reference at frame
     [f]'s Feptr. Called only when it is known that the offset lies
     within the offsets that have so far been used in the match (or for
     the unset-group entry check).

     Arguments (360-362): offset = Loffset, the index into the frame
     ovector; caseless = Lcaseless; f = the frame (the C's F); lengthptr
     = the out-parameter for the length matched (the per-match
     [ref_length] cell).

     Returns (355-357): = 0 successful match, number of code units
     matched written to [lengthptr]; < 0 no match; > 0 partial match. *)
  let match_ref (f : int) (offset : int) (caseless : bool) (lengthptr : int ref)
      : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:369-380 — deal with an unset group: the default is
       no match, but there is an option to match an empty string. *)
    if
      offset >= fr.(fb + Frames.slot_offset_top)
      || Int.equal fr.(fb + Frames.slot_ovector + offset) Frames.unset
    then
      if not (Int.equal (mb.poptions land Options.match_unset_backref) 0) then (
        lengthptr := 0;
        0 (* match *))
      else -1 (* no match *)
    else
      (* pcre2_match.c:382-386 — separate the caseless and UTF cases for
         speed. *)
      let eptr = fr.(fb + Frames.slot_eptr) in
      let p = mb.start_subject + fr.(fb + Frames.slot_ovector + offset) in
      let length =
        fr.(fb + Frames.slot_ovector + offset + 1)
        - fr.(fb + Frames.slot_ovector + offset)
      in
      if caseless then (
        (* pcre2_match.c:390-434 — the SUPPORT_UNICODE utf/ucp fold arm
           is M6/M7: the OP_REFI/OP_DNREFI dispatch arms return
           [error_unported] before any match_ref call when utf or ucp is
           set, so only the non-UTF/UCP arm (438-451) is reachable
           here. *)
        let rc = match_ref_ci p eptr length in
        (* pcre2_match.c:479 — *lengthptr = eptr - eptr_start: the loop
           advanced eptr one unit per reference unit, so this is exactly
           [length] in the non-UTF arm. *)
        if Int.equal rc 0 then lengthptr := length;
        rc)
      else if not (Int.equal mb.partial 0) then (
        (* pcre2_match.c:454-467 — in the caseful case, just compare the
           code units; when partial matching, do it unit by unit. *)
        let rc = match_ref_cs_partial p eptr length in
        if Int.equal rc 0 then lengthptr := length (* 479, as above *);
        rc)
      else if
        (* pcre2_match.c:469-476 — not partial matching. *)
        mb.end_subject - eptr < length
      then 1 (* partial, 473 *)
      else if not (match_ref_memcmp p eptr length) then -1 (* no match, 474 *)
      else (
        (* pcre2_match.c:475-480 — eptr += length;
           *lengthptr = eptr - eptr_start. *)
        lengthptr := length;
        0)
  in

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
        (* OP_ANY (pcre2_match.c:947-958) — match any single character
           type except newline; have to take care with CRLF newlines and
           partial matching. Falls through to OP_ALLANY. *)
        if utf then
          (* the shared OP_ALLANY tail's ACROSSCHAR advance (970-971):
             M6. *)
          error_unported
        else
          let eptr = fr.(fb + Frames.slot_eptr) in
          if is_newline_at eptr then (backtrack [@tailcall]) f match_nomatch
          else if
            (* pcre2_match.c:949-957 — a CRLF pattern newline with only
               its CR present at the end of the subject could be
               partial. *)
            (not (Int.equal mb.partial 0))
            && Int.equal eptr (mb.end_subject - 1)
            && Int.equal mb.nltype Newline.nltype_fixed
            && Int.equal mb.nllen 2
            && Int.equal
                 (* safe: eptr = end_subject - 1 < String.length
                    mb.subject; 0 <= start_eptr <= eptr (mb invariant) *)
                 (Char.code (String.unsafe_get mb.subject eptr))
                 mb.nl0
          then (
            mb.hitend <- true;
            if mb.partial > 1 then Errors.error_partial
            else (op_allany_tail [@tailcall]) f (* fallthrough *))
          else (op_allany_tail [@tailcall]) f (* fallthrough *)
    | 13 ->
        (* OP_ALLANY (pcre2_match.c:962-973) — match any single character
           whatsoever. *)
        if utf then (* the ACROSSCHAR advance (970-971): M6. *)
          error_unported
        else (op_allany_tail [@tailcall]) f
    | 14 ->
        (* OP_ANYBYTE (pcre2_match.c:981-989) — match a single code unit,
           even in UTF mode. \C compiles to OP_ALLANY in the non-UTF 8-bit
           library (pcre2_compile.c:8174-8176), so this arm is reached
           only from UTF programs (M6) or hand-assembled code. DO NOT
           merge the Feptr++ into the bound check; it must not be updated
           before SCHECK_PARTIAL. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (dispatch [@tailcall]) f)
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
        (* OP_NCLASS, OP_CLASS (pcre2_match.c:1933-1972) — match a
           bit-mapped character class, possibly repeatedly. The two
           opcodes differ only for data characters outside 0-255, which a
           non-UTF 8-bit code unit cannot be (the fc > 255 tests are
           compiled out at PCRE2_CODE_UNIT_WIDTH == 8); the UTF arms
           (1977-1994, 2028-2047, 2085-2114, labels RM200/RM201) are M6.
           Frame temporaries (pcre2_match.c:1925-1930): Lmin/Lmax =
           temp_32[0..1], Lstart_eptr = temp_sptr[0], Lbyte_map_address =
           temp_sptr[1]. *)
        if utf then error_unported
        else (
          (* pcre2_match.c:1936-1937 — save the bitmap address for
             matching; advance past the item: 32 bytes in the 8-bit
             library (32 / sizeof(PCRE2_UCHAR)). *)
          fr.(fb + Frames.slot_temp_sptr_1) <- ecode + 1;
          let ecode = ecode + 1 + 32 in
          (* pcre2_match.c:1939-1971 — look past the end of the item to
             see if there is repeat information following; then obey
             similar code to character type repeats. *)
          let next = Char.code (Bytes.get mb.start_code ecode) in
          if
            (next >= Opcodes.op_crstar && next <= Opcodes.op_crminquery)
            || (next >= Opcodes.op_crposstar && next <= Opcodes.op_crposquery)
          then (
            (* pcre2_match.c:1944-1957 — OP_CRSTAR..OP_CRMINQUERY,
               OP_CRPOSSTAR..OP_CRPOSQUERY: fc = *Fecode++ - OP_CRSTAR
               indexes the rep tables. *)
            let idx = next - Opcodes.op_crstar in
            fr.(fb + Frames.slot_temp_32_0) <- rep_min.(idx) (* Lmin *);
            fr.(fb + Frames.slot_temp_32_1) <- rep_max.(idx) (* Lmax *);
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (class_min [@tailcall]) f 1 rep_typ.(idx))
          else if
            Int.equal next Opcodes.op_crrange
            || Int.equal next Opcodes.op_crminrange
            || Int.equal next Opcodes.op_crposrange
          then (
            (* pcre2_match.c:1959-1966 — Lmin = GET2(Fecode, 1); Lmax =
               GET2(Fecode, 1 + IMM2_SIZE); max 0 => infinity. *)
            let lmax =
              Compile.get2 mb.start_code (ecode + 1 + Limits.imm2_size)
            in
            fr.(fb + Frames.slot_temp_32_0) <-
              Compile.get2 mb.start_code (ecode + 1);
            fr.(fb + Frames.slot_temp_32_1) <-
              (if Int.equal lmax 0 then uint32_max else lmax);
            fr.(fb + Frames.slot_ecode) <- ecode + 1 + (2 * Limits.imm2_size);
            (class_min [@tailcall]) f 1 rep_typ.(next - Opcodes.op_crstar))
          else (
            (* pcre2_match.c:1968-1970 — no repeat follows: Lmin = Lmax =
               1. The C leaves reptype unread (never consulted when Lmin =
               Lmax): 0 is passed, as at OP_EXACT. *)
            fr.(fb + Frames.slot_temp_32_0) <- 1;
            fr.(fb + Frames.slot_temp_32_1) <- 1;
            fr.(fb + Frames.slot_ecode) <- ecode;
            (class_min [@tailcall]) f 1 0))
    | 112 ->
        (* OP_XCLASS (pcre2_match.c:2175-2302) — STUB: M6 (wide chars). *)
        error_unported
    | 6 ->
        (* OP_NOT_DIGIT (pcre2_match.c:2305-2315) — match various
           character types when PCRE2_UCP is not set (2298-2303). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then (* GETCHARINCTEST's UTF decode: M6. *)
          error_unported
        else
          (* GETCHARINCTEST(fc, Feptr): one code unit, post-increment (the
             character was consulted even when the test fails);
             CHMAX_255(fc) is TRUE for a non-UTF 8-bit code unit
             (pcre2_intmodedep.h:217-219). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          if
            not (Int.equal (Chartables.ctypes fc land Chartables.ctype_digit) 0)
          then (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) f)
    | 7 ->
        (* OP_DIGIT (pcre2_match.c:2317-2327). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then error_unported (* GETCHARINCTEST UTF decode: M6 *)
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          if Int.equal (Chartables.ctypes fc land Chartables.ctype_digit) 0 then
            (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) f)
    | 8 ->
        (* OP_NOT_WHITESPACE (pcre2_match.c:2329-2339). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then error_unported (* GETCHARINCTEST UTF decode: M6 *)
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          if
            not (Int.equal (Chartables.ctypes fc land Chartables.ctype_space) 0)
          then (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) f)
    | 9 ->
        (* OP_WHITESPACE (pcre2_match.c:2341-2351). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then error_unported (* GETCHARINCTEST UTF decode: M6 *)
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          if Int.equal (Chartables.ctypes fc land Chartables.ctype_space) 0 then
            (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) f)
    | 10 ->
        (* OP_NOT_WORDCHAR (pcre2_match.c:2353-2363). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then error_unported (* GETCHARINCTEST UTF decode: M6 *)
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          if not (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0)
          then (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) f)
    | 11 ->
        (* OP_WORDCHAR (pcre2_match.c:2365-2375). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then error_unported (* GETCHARINCTEST UTF decode: M6 *)
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          if Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0 then
            (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) f)
    | 17 ->
        (* OP_ANYNL (pcre2_match.c:2377-2409) — match \R, any newline
           sequence. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then error_unported (* GETCHARINCTEST UTF decode: M6 *)
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          let eptr = eptr + 1 in
          fr.(fb + Frames.slot_eptr) <- eptr;
          if Int.equal fc Newline.char_cr then
            (* pcre2_match.c:2388-2393 — CR: absorb a following LF; a CR
               at the very end of the subject could be partial. *)
            if eptr >= mb.end_subject then
              let rc = scheck_partial mb eptr in
              if rc < 0 then rc
              else (
                fr.(fb + Frames.slot_ecode) <- ecode + 1;
                (dispatch [@tailcall]) f)
            else (
              if
                Int.equal
                  (* safe: eptr < mb.end_subject (checked above) *)
                  (Char.code (String.unsafe_get mb.subject eptr))
                  Newline.char_lf
              then fr.(fb + Frames.slot_eptr) <- eptr + 1;
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) f)
          else if Int.equal fc Newline.char_lf then (
            (* pcre2_match.c:2395-2396 *)
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) f)
          else if
            (* pcre2_match.c:2398-2406 — VT, FF, NEL and (not EBCDIC)
               0x2028/0x2029; the latter two are unreachable from a
               non-UTF 8-bit code unit but transcribed for M6 to share. *)
            Int.equal fc Newline.char_vt
            || Int.equal fc Newline.char_ff
            || Int.equal fc Newline.char_nel
            || Int.equal fc 0x2028 || Int.equal fc 0x2029
          then
            if Int.equal mb.bsr_convention Options.bsr_anycrlf then
              (backtrack [@tailcall]) f match_nomatch
            else (
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) f)
          else (backtrack [@tailcall]) f match_nomatch (* 2386 — default *)
    | 18 -> (
        (* OP_NOT_HSPACE (pcre2_match.c:2412-2425). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then error_unported (* GETCHARINCTEST UTF decode: M6 *)
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (* HSPACE_CASES (pcre2_internal.h:429-431): byte and multibyte
             cases; the multibyte values are unreachable from a non-UTF
             8-bit code unit but transcribed for M6 to share. *)
          match fc with
          | 0x09 | 0x20 | 0xa0 | 0x1680 | 0x180e | 0x2000 | 0x2001 | 0x2002
          | 0x2003 | 0x2004 | 0x2005 | 0x2006 | 0x2007 | 0x2008 | 0x2009
          | 0x200a | 0x202f | 0x205f | 0x3000 ->
              (backtrack [@tailcall]) f match_nomatch
          | _ ->
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) f)
    | 19 -> (
        (* OP_HSPACE (pcre2_match.c:2427-2440). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then error_unported (* GETCHARINCTEST UTF decode: M6 *)
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          match fc with
          | 0x09 | 0x20 | 0xa0 | 0x1680 | 0x180e | 0x2000 | 0x2001 | 0x2002
          | 0x2003 | 0x2004 | 0x2005 | 0x2006 | 0x2007 | 0x2008 | 0x2009
          | 0x200a | 0x202f | 0x205f | 0x3000 ->
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) f
          | _ -> (backtrack [@tailcall]) f match_nomatch)
    | 20 -> (
        (* OP_NOT_VSPACE (pcre2_match.c:2442-2455). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then error_unported (* GETCHARINCTEST UTF decode: M6 *)
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (* VSPACE_CASES (pcre2_internal.h:447-449): LF, VT, FF, CR, NEL
             and the multibyte 0x2028/0x2029 (unreachable non-UTF). *)
          match fc with
          | 0x0a | 0x0b | 0x0c | 0x0d | 0x85 | 0x2028 | 0x2029 ->
              (backtrack [@tailcall]) f match_nomatch
          | _ ->
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) f)
    | 21 -> (
        (* OP_VSPACE (pcre2_match.c:2457-2470). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
        else if utf then error_unported (* GETCHARINCTEST UTF decode: M6 *)
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          match fc with
          | 0x0a | 0x0b | 0x0c | 0x0d | 0x85 | 0x2028 | 0x2029 ->
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) f
          | _ -> (backtrack [@tailcall]) f match_nomatch)
    | 16 | 15 ->
        (* OP_PROP, OP_NOTPROP (pcre2_match.c:2479-2618) — STUB: M7
           (UCP). *)
        error_unported
    | 22 ->
        (* OP_EXTUNI (pcre2_match.c:2621-2648) — STUB: M7 (UCP). *)
        error_unported
    | 93 ->
        (* OP_TYPEEXACT (pcre2_match.c:2651-2654). reptype is never read
           when Lmin = Lmax (the post-min-phase Lmin == Lmax continue
           happens before any strategy dispatch, 3503-3505): 0 is passed,
           as at OP_EXACT. *)
        let n = Compile.get2 mb.start_code (ecode + 1) in
        (repeattype [@tailcall]) f n n 0 (ecode + 1 + Limits.imm2_size)
    | 91 | 92 ->
        (* OP_TYPEUPTO, OP_TYPEMINUPTO (pcre2_match.c:2656-2662). *)
        (repeattype [@tailcall]) f 0
          (Compile.get2 mb.start_code (ecode + 1))
          (if Int.equal op Opcodes.op_typeminupto then reptype_min
           else reptype_max)
          (ecode + 1 + Limits.imm2_size)
    | 94 ->
        (* OP_TYPEPOSSTAR (pcre2_match.c:2664-2669). *)
        (repeattype [@tailcall]) f 0 uint32_max reptype_pos (ecode + 1)
    | 95 ->
        (* OP_TYPEPOSPLUS (pcre2_match.c:2671-2676). *)
        (repeattype [@tailcall]) f 1 uint32_max reptype_pos (ecode + 1)
    | 96 ->
        (* OP_TYPEPOSQUERY (pcre2_match.c:2678-2683). *)
        (repeattype [@tailcall]) f 0 1 reptype_pos (ecode + 1)
    | 97 ->
        (* OP_TYPEPOSUPTO (pcre2_match.c:2685-2690). *)
        (repeattype [@tailcall]) f 0
          (Compile.get2 mb.start_code (ecode + 1))
          reptype_pos
          (ecode + 1 + Limits.imm2_size)
    | 85 | 86 | 87 | 88 | 89 | 90 ->
        (* OP_TYPESTAR, OP_TYPEMINSTAR, OP_TYPEPLUS, OP_TYPEMINPLUS,
           OP_TYPEQUERY, OP_TYPEMINQUERY (pcre2_match.c:2692-2701): fc =
           *Fecode++ - OP_TYPESTAR indexes the rep tables. *)
        let idx = op - Opcodes.op_typestar in
        (repeattype [@tailcall]) f rep_min.(idx) rep_max.(idx) rep_typ.(idx)
          (ecode + 1)
    | 115 | 116 ->
        (* OP_DNREF, OP_DNREFI (pcre2_match.c:4980-5009) — match a back
           reference, possibly repeatedly, for a duplicated named group:
           scan the list of groups to which the name refers, and use the
           first one that is set. Frame temporaries (pcre2_match.c:
           4988-4992): Lmin = temp_32[0], Lmax = temp_32[1], Lcaseless =
           temp_32[2], Lstart = temp_sptr[0], Loffset = temp_size. *)
        let caseless = Int.equal op Opcodes.op_dnrefi in
        if caseless && (utf || ucp) then
          (* match_ref's caseless utf/ucp fold (pcre2_match.c:390-434):
             M6/M7 — gate the whole arm before any state is written, as
             at OP_CHARI. *)
          error_unported
        else (
          fr.(fb + Frames.slot_temp_32_2) <- (if caseless then 1 else 0)
          (* Lcaseless, 4996 *);
          (* pcre2_match.c:4998-5000 *)
          let count =
            Compile.get2 mb.start_code (ecode + 1 + Limits.imm2_size)
          in
          let slot =
            Compile.get2 mb.start_code (ecode + 1) * mb.name_entry_size
          in
          (* pcre2_match.c:5002-5007 — while (count-- > 0): Loffset for
             the first set group, or the last examined entry when none is
             set. The C writes Loffset every iteration; [dnref_scan]
             returns the same final value, written once. count >= 1 in a
             compiled program (the reference names at least one group). *)
          fr.(fb + Frames.slot_temp_size) <-
            dnref_scan f count slot (* Loffset *);
          (* goto REF_REPEAT (5009) with Fecode past the item (5000). *)
          (ref_repeat [@tailcall]) f (ecode + 1 + (2 * Limits.imm2_size)))
    | 113 | 114 ->
        (* OP_REF, OP_REFI (pcre2_match.c:5011-5015) — match a back
           reference, possibly repeatedly: a reference to a numbered
           group or a non-duplicated named group. Frame temporaries as at
           OP_DNREF (pcre2_match.c:4988-4992). *)
        let caseless = Int.equal op Opcodes.op_refi in
        if caseless && (utf || ucp) then
          (* match_ref's caseless utf/ucp fold: M6/M7 — gate as at
             OP_DNREFI. *)
          error_unported
        else (
          fr.(fb + Frames.slot_temp_32_2) <- (if caseless then 1 else 0)
          (* Lcaseless, 5013 *);
          fr.(fb + Frames.slot_temp_size) <-
            (Compile.get2 mb.start_code (ecode + 1) lsl 1) - 2
          (* Loffset, 5014 *);
          (ref_repeat [@tailcall]) f (ecode + 1 + Limits.imm2_size))
    | 151 ->
        (* OP_BRAZERO (pcre2_match.c:5224-5230) — a possibly-zero repeat
           of a bracket group: try to take the group first. Lnext_ecode =
           temp_sptr[0] (5222) holds the bracket position for the RM9
           resume, which skips to after the group on NOMATCH. *)
        fr.(fb + Frames.slot_temp_sptr_0) <- ecode + 1 (* Lnext_ecode *);
        (rmatch [@tailcall]) f (ecode + 1) rm9 0
    | 152 ->
        (* OP_BRAMINZERO (pcre2_match.c:5232-5238) — the minimizing form:
           try the rest of the pattern WITHOUT the group first. Lnext_ecode
           walks from the bracket to its final ket before the RMATCH. *)
        let next = skip_alts (ecode + 1) in
        fr.(fb + Frames.slot_temp_sptr_0) <- next (* Lnext_ecode, post-walk *);
        (rmatch [@tailcall]) f (next + 1 + Limits.link_size) rm10 0
    | 167 ->
        (* OP_SKIPZERO (pcre2_match.c:5242-5246) — a group with a {0}
           quantifier: skip it entirely. Fecode++; walk to the final ket;
           Fecode += 1 + LINK_SIZE. *)
        fr.(fb + Frames.slot_ecode) <-
          skip_alts (ecode + 1) + 1 + Limits.link_size;
        (dispatch [@tailcall]) f
    | 153 ->
        (* OP_BRAPOSZERO (pcre2_match.c:5257-5263) — STUB: M4 possessive
           chunk. *)
        error_unported
    | 136 | 141 | 138 | 143 ->
        (* OP_BRAPOS, OP_SBRAPOS, OP_CBRAPOS, OP_SCBRAPOS
           (pcre2_match.c:5266-5346) — STUB: M4 possessive chunk. *)
        error_unported
    | 135 ->
        (* OP_BRA (pcre2_match.c:5349-5372) — non-capturing brackets that
           cannot match an empty string. When we get to the final
           alternative within the brackets, as long as there are no THENs
           in the pattern, we can optimize by not recording a new
           backtracking point — but not at the very top level, where there
           would be nothing to go back to. *)
        if mb.hasthen || Int.equal fr.(fb + Frames.slot_rdepth) 0 then (
          fr.(fb + Frames.slot_temp_32_0) <- 0 (* Lframe_type *);
          (grouploop [@tailcall]) f)
        else (bra_loop [@tailcall]) f
    | 137 | 142 ->
        (* OP_CBRA, OP_SCBRA (pcre2_match.c:5381-5384) — a capturing
           bracket, other than those that are possessive with an unlimited
           repeat: Lframe_type = GF_CAPTURE | GET2(Fecode, 1+LINK_SIZE). *)
        fr.(fb + Frames.slot_temp_32_0) <-
          Frames.gf_capture
          lor Compile.get2 mb.start_code (ecode + 1 + Limits.link_size);
        (grouploop [@tailcall]) f
    | 133 | 134 | 140 ->
        (* OP_ONCE, OP_SCRIPT_RUN, OP_SBRA (pcre2_match.c:5391-5394) —
           atomic groups and non-capturing brackets that can match an
           empty string must record a backtracking point and set up a
           chained frame: Lframe_type = GF_NOCAPTURE | Fop. The shared
           GROUPLOOP is fully live; OP_ONCE and OP_SCRIPT_RUN differ only
           in their ket actions, stubbed there for M4 / M7. *)
        fr.(fb + Frames.slot_temp_32_0) <- Frames.gf_nocapture lor op;
        (grouploop [@tailcall]) f
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
        (* OP_ALT (pcre2_match.c:5894-5897) — an alternation is the end of
           a branch: record it in branch_end, then scan along to the end
           of the bracketed group. *)
        branch_end := ecode;
        fr.(fb + Frames.slot_ecode) <- skip_alts ecode;
        (dispatch [@tailcall]) f
    | 121 | 123 | 122 | 124 -> (
        (* OP_KET, OP_KETRMIN, OP_KETRMAX, OP_KETRPOS
           (pcre2_match.c:5906-6127) — the end of a parenthesized group.
           For all but OP_BRA and OP_COND, the starting frame was added to
           the chained frames in order to remember the starting subject
           position for the group. *)
        (* pcre2_match.c:5911 *)
        let bracode = ecode - Compile.get mb.start_code (ecode + 1) in
        (* pcre2_match.c:5913-5917 — identify the start of the branch that
           ends at this ket: OP_ALT recorded branch_end (5895) when the
           matched branch ended at an alternation, otherwise the branch
           ends here. branch_start is consumed only by the OP_ASSERTBACK*
           VREVERSE checks (M3/M4): the walk is kept for parity, its
           result unused until those ket actions land. *)
        let _branch_start =
          ket_branch_start bracode
            (if Int.equal !branch_end (-1) then ecode else !branch_end)
        in
        branch_end := -1;
        let bra_op = Char.code (Bytes.get mb.start_code bracode) in
        (* pcre2_match.c:5919-5950 — point N (= p + 1 here) to the frame
           at the start of the most recent group (Flast_group_offset is a
           frame index, frames.ml DEVIATION), and P to its predecessor —
           the frame that dispatched the bracket, whose eptr is the
           subject position at the start of the group — then unchain it.
           P stays NULL (-1) for OP_BRA and OP_COND: their starting frame
           was not recorded. In bounds: every non-BRA/COND bracket arm
           passes a nonzero group_frame_type to RMATCH, so NEW_FRAME
           recorded a valid frame index >= 1 (pcre2_match.c:763-771) that
           the frame copies carried here (complete-program mb
           invariant). *)
        let p =
          if
            (not (Int.equal bra_op Opcodes.op_bra))
            && not (Int.equal bra_op Opcodes.op_cond)
          then (
            let n = fr.(fb + Frames.slot_last_group_offset) in
            fr.(fb + Frames.slot_last_group_offset) <-
              fr.(Frames.base a (n - 1) + Frames.slot_last_group_offset);
            n - 1)
          else -1
        in
        if
          p >= 0
          && Int.equal
               (Frames.gf_idmask
                  fr.(Frames.base a (p + 1) + Frames.slot_group_frame_type))
               Frames.gf_condassert
        then
          (* pcre2_match.c:5934-5948 — the end of an assertion that is a
             condition — STUB: M5 conditionals chunk (GF_CONDASSERT frames
             are only created by OP_COND's assertion conditions,
             5711-5717). *)
          error_unported
        else
          (* pcre2_match.c:5952-6085 — actions relating to the starting
             opcode: switch ( *bracode). *)
          match bra_op with
          | 135 ->
              (* OP_BRA (pcre2_match.c:5964-5984) — nothing to be done
                 unless this is the end of a whole-pattern recursion
                 (recursing in group 0 with OP_END immediately following
                 this ket). *)
              if
                Int.equal fr.(fb + Frames.slot_current_recurse) 0
                && Int.equal
                     (Char.code
                        (Bytes.get mb.start_code (ecode + 1 + Limits.link_size)))
                     Opcodes.op_end
              then
                (* pcre2_match.c:5967-5984 — STUB: M5 recursion chunk
                   (Fcurrent_recurse stays RECURSE_UNSET until OP_RECURSE
                   lands, so this is unreachable now). *)
                error_unported
              else (op_ket_tail [@tailcall]) f p bracode
          | 139 | 144 ->
              (* OP_COND, OP_SCOND (pcre2_match.c:5986-5988) — no need to
                 do anything for these. *)
              (op_ket_tail [@tailcall]) f p bracode
          | 132 | 131 | 129 | 127 | 133 | 130 | 128 | 134 ->
              (* OP_ASSERTBACK_NA, OP_ASSERT_NA, OP_ASSERTBACK, OP_ASSERT,
                 OP_ONCE (pcre2_match.c:5994-6031), OP_ASSERTBACK_NOT,
                 OP_ASSERT_NOT (6037-6043), OP_SCRIPT_RUN (6049-6051) —
                 STUB: M3/M4 lookaround and atomic chunks, M7 script runs
                 (unreachable now: those bracket arms are stubs, except
                 the live OP_ONCE/OP_SCRIPT_RUN heads whose deferral is
                 exactly here). *)
              error_unported
          | 137 | 138 | 142 | 143 ->
              (* OP_CBRA, OP_CBRAPOS, OP_SCBRA, OP_SCBRAPOS
                 (pcre2_match.c:6056-6084). *)
              let number =
                Compile.get2 mb.start_code (bracode + 1 + Limits.link_size)
              in
              if Int.equal fr.(fb + Frames.slot_current_recurse) number then
                (* pcre2_match.c:6062-6075 — a recursively called group:
                   reinstate the previous captures and carry on after the
                   recursion call — STUB: M5 recursion chunk
                   (Fcurrent_recurse stays RECURSE_UNSET until OP_RECURSE
                   lands, so this is unreachable now). *)
                error_unported
              else
                (* pcre2_match.c:6077-6084 — deal with actual capturing.
                   In bounds: 1 <= number <= top_bracket in a compiled
                   program (mb invariant), so offset + 1 <=
                   2 * top_bracket - 1, within the frame's ovector
                   region. *)
                let offset = (number lsl 1) - 2 in
                fr.(fb + Frames.slot_capture_last) <- number;
                fr.(fb + Frames.slot_ovector + offset) <-
                  fr.(Frames.base a p + Frames.slot_eptr) - mb.start_subject;
                fr.(fb + Frames.slot_ovector + offset + 1) <-
                  fr.(fb + Frames.slot_eptr) - mb.start_subject;
                if offset >= fr.(fb + Frames.slot_offset_top) then
                  fr.(fb + Frames.slot_offset_top) <- offset + 2;
                (op_ket_tail [@tailcall]) f p bracode
          | _ ->
              (* OP_SBRA, OP_BRAPOS, OP_SBRAPOS have no case in the C
                 switch: no action relating to the starting opcode. *)
              (op_ket_tail [@tailcall]) f p bracode)
    | 27 ->
        (* OP_CIRC (pcre2_match.c:6133-6137) — start of line, unless
           PCRE2_NOTBOL is set; not multiline mode. *)
        if
          (not (Int.equal fr.(fb + Frames.slot_eptr) mb.start_subject))
          || not (Int.equal (mb.moptions land Options.notbol) 0)
        then (backtrack [@tailcall]) f match_nomatch
        else (
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (dispatch [@tailcall]) f)
    | 1 ->
        (* OP_SOD (pcre2_match.c:6139-6142) — unconditional start of
           subject (\A). *)
        if not (Int.equal fr.(fb + Frames.slot_eptr) mb.start_subject) then
          (backtrack [@tailcall]) f match_nomatch
        else (
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (dispatch [@tailcall]) f)
    | 25 ->
        (* OP_DOLL (pcre2_match.c:6147-6151) — when PCRE2_NOTEOL is unset,
           assert before the subject end, or a terminating newline unless
           PCRE2_DOLLAR_ENDONLY is set. *)
        if not (Int.equal (mb.moptions land Options.noteol) 0) then
          (backtrack [@tailcall]) f match_nomatch
        else if Int.equal (mb.poptions land Options.dollar_endonly) 0 then
          (assert_nl_or_eos [@tailcall]) f (* goto ASSERT_NL_OR_EOS *)
        else (op_eod_tail [@tailcall]) f (* fallthrough to OP_EOD in C *)
    | 24 ->
        (* OP_EOD (pcre2_match.c:6154-6163) — unconditional end of subject
           assertion (\z). *)
        (op_eod_tail [@tailcall]) f
    | 23 ->
        (* OP_EODN (pcre2_match.c:6166-6192) — end of subject or ending \n
           assertion (\Z); the body is shared with OP_DOLL's
           ASSERT_NL_OR_EOS goto target. *)
        (assert_nl_or_eos [@tailcall]) f
    | 28 ->
        (* OP_CIRCM (pcre2_match.c:6200-6211) — start of subject unless
           notbol, or after any newline except for one at the very end,
           unless PCRE2_ALT_CIRCUMFLEX is set. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if
          (not (Int.equal (mb.moptions land Options.notbol) 0))
          && Int.equal eptr mb.start_subject
        then (backtrack [@tailcall]) f match_nomatch
        else if
          (not (Int.equal eptr mb.start_subject))
          && (Int.equal eptr mb.end_subject
              && Int.equal (mb.poptions land Options.alt_circumflex) 0
             || not (was_newline_at eptr))
        then (backtrack [@tailcall]) f match_nomatch
        else (
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (dispatch [@tailcall]) f)
    | 26 ->
        (* OP_DOLLM (pcre2_match.c:6214-6239) — assert before any newline,
           or before end of subject unless noteol is set; multiline
           mode. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr < mb.end_subject then
          if not (is_newline_at eptr) then
            if
              (* pcre2_match.c:6220-6228 — a CRLF pattern newline with
                 only its CR present at the end of the subject could be
                 partial. *)
              (not (Int.equal mb.partial 0))
              && eptr + 1 >= mb.end_subject
              && Int.equal mb.nltype Newline.nltype_fixed
              && Int.equal mb.nllen 2
              && Int.equal
                   (* safe: eptr < mb.end_subject <= String.length
                      mb.subject (checked above); 0 <= start_eptr <= eptr
                      (mb invariant) *)
                   (Char.code (String.unsafe_get mb.subject eptr))
                   mb.nl0
            then (
              mb.hitend <- true;
              if mb.partial > 1 then Errors.error_partial
              else (backtrack [@tailcall]) f match_nomatch)
            else (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) f)
        else if not (Int.equal (mb.moptions land Options.noteol) 0) then
          (backtrack [@tailcall]) f match_nomatch
        else
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) f)
    | 2 ->
        (* OP_SOM (pcre2_match.c:6243-6246) — start of match assertion
           (\G): subject + offset. *)
        if
          not
            (Int.equal
               fr.(fb + Frames.slot_eptr)
               (mb.start_subject + mb.start_offset))
        then (backtrack [@tailcall]) f match_nomatch
        else (
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (dispatch [@tailcall]) f)
    | 3 ->
        (* OP_SET_SOM (pcre2_match.c:6252-6255) — reset the start of match
           point (\K): Fstart_match = Feptr. *)
        fr.(fb + Frames.slot_start_match) <- fr.(fb + Frames.slot_eptr);
        fr.(fb + Frames.slot_ecode) <- ecode + 1;
        (dispatch [@tailcall]) f
    | 4 | 5 | 169 | 170 ->
        (* OP_NOT_WORD_BOUNDARY, OP_WORD_BOUNDARY,
           OP_NOT_UCP_WORD_BOUNDARY, OP_UCP_WORD_BOUNDARY
           (pcre2_match.c:6265-6338) — find out if the previous and
           current characters are "word" characters, remembering the
           earliest and latest consulted characters, then see if the
           situation is what we want. *)
        if
          Int.equal op Opcodes.op_ucp_word_boundary
          || Int.equal op Opcodes.op_not_ucp_word_boundary
        then
          (* pcre2_match.c:6284-6291, 6317-6324 — UCD property probes: M7
             (these opcodes are only compiled under PCRE2_UCP). *)
          error_unported
        else if utf then
          (* pcre2_match.c:6272-6277, 6306-6312 — BACKCHAR/GETCHAR/
             FORWARDCHARTEST: M6. *)
          error_unported
        else
          let eptr = fr.(fb + Frames.slot_eptr) in
          (* pcre2_match.c:6269-6293 — status of the previous character.
             mb->check_subject equals mb->start_subject when not matching
             an invalid-UTF fragment (pcre2_match.c:6795); the separate
             field arrives with M6. *)
          let prev_is_word =
            if Int.equal eptr mb.start_subject then false
            else
              let lastptr = eptr - 1 in
              (* safe: 0 <= start_subject <= lastptr (checked above) and
                 lastptr < eptr <= end_subject <= String.length
                 mb.subject *)
              let fc = Char.code (String.unsafe_get mb.subject lastptr) in
              if lastptr < mb.start_used_ptr then mb.start_used_ptr <- lastptr;
              (* pcre2_match.c:6293 — CHMAX_255(fc) is TRUE for a non-UTF
                 8-bit code unit. *)
              not
                (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0)
          in
          (* pcre2_match.c:6296-6330 — get status of next character. *)
          if eptr >= mb.end_subject then
            let rc = scheck_partial mb eptr in
            if rc < 0 then rc
            else (word_boundary_tail [@tailcall]) f prev_is_word false
          else
            let nextptr = eptr + 1 in
            (* safe: eptr < mb.end_subject <= String.length mb.subject
               (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
            let fc = Char.code (String.unsafe_get mb.subject eptr) in
            if nextptr > mb.last_used_ptr then mb.last_used_ptr <- nextptr;
            (word_boundary_tail [@tailcall]) f prev_is_word
              (not
                 (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0))
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
  (* pcre2_match.c:962-973 — the OP_ALLANY body, shared as the fallthrough
     tail of OP_ANY (fallthrough from OP_ANY in C, 958-960). DO NOT merge
     the Feptr++ into the bound check; it must not be updated before
     SCHECK_PARTIAL (963-967). The UTF ACROSSCHAR advance (970-971) is M6;
     callers exclude utf. *)
  and op_allany_tail (f : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let eptr = fr.(fb + Frames.slot_eptr) in
    if eptr >= mb.end_subject then
      let rc = scheck_partial mb eptr in
      if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
    else (
      fr.(fb + Frames.slot_eptr) <- eptr + 1;
      fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
      (dispatch [@tailcall]) f)
  (* pcre2_match.c:1996-2017 — OP_CLASS/OP_NCLASS, not UTF: first, ensure
     the minimum number of matches are present:
     for (i = 1; i <= Lmin; i++). *)
  and class_min (f : int) (i : int) (reptype : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
      else
        (* pcre2_match.c:2009 — fc = *Feptr++: the post-increment advances
           Feptr even when the bitmap test fails. *)
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        if Int.equal (class_bit f fc) 0 then
          (backtrack [@tailcall]) f match_nomatch
        else (class_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:2019-2021 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:2049-2055 — minimize, not UTF: the for(;;) starts
         with RMATCH(Fecode, RM23); the rest of the loop body is the RM23
         resume arm in [backtrack]. *)
      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm23 0
    else (
      (* pcre2_match.c:2079-2081 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (class_maxscan [@tailcall]) f fr.(fb + Frames.slot_temp_32_0) reptype)
  and class_maxscan (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:2119-2137 — class greedy scan, not UTF:
       for (i = Lmin; i < Lmax; i++); fc = *Feptr with no increment until
       the bitmap test passes. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:2121-2125 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (class_maxend [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        if Int.equal (class_bit f fc) 0 then
          (class_maxend [@tailcall]) f reptype (* break, 2134 *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (class_maxscan [@tailcall]) f (i + 1) reptype)
    else (class_maxend [@tailcall]) f reptype
  and class_maxend (f : int) (reptype : int) : int =
    (* pcre2_match.c:2139-2141 — if possessive, no backing up: continue
       the main loop at the advanced Fecode. *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) f
    else (class_maxbt [@tailcall]) f
  and class_maxbt (f : int) : int =
    (* pcre2_match.c:2143-2150 — while (Feptr >= Lstart_eptr) try the rest
       of the pattern via RMATCH(Fecode, RM24), stepping Feptr back per
       failure; when the loop falls below Lstart_eptr,
       RRETURN(MATCH_NOMATCH). Unlike the char repeats, the minimum
       position is also tried via RMATCH (the >= guard), and the arm then
       fails. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) >= fr.(fb + Frames.slot_temp_sptr_0) then
      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm24 0
    else (backtrack [@tailcall]) f match_nomatch
  (* pcre2_match.c:2703-2723 — REPEATTYPE: common code for all repeated
     character type matches (goto target of the OP_TYPEEXACT..
     OP_TYPEMINQUERY arms). Frame temporaries (pcre2_match.c:2644-2648):
     Lstart_eptr = temp_sptr[0], Lmin/Lmax/Lctype/Lpropvalue =
     temp_32[0..3]. *)
  and repeattype (f : int) (lmin : int) (lmax : int) (reptype : int)
      (ecode : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:2706 — Lctype = *Fecode++: code for the character
       type. *)
    let lctype = Char.code (Bytes.get mb.start_code ecode) in
    let ecode = ecode + 1 in
    if
      Int.equal lctype Opcodes.op_prop
      || Int.equal lctype Opcodes.op_notprop
      || Int.equal lctype Opcodes.op_extuni
    then
      (* pcre2_match.c:2708-2714 — the proptype/Lpropvalue reads and every
         property loop (2726-2977, 3517-3805, 4116-4402), and the
         OP_EXTUNI loops (2979-2996, 3807-3844, 4404-4443): M7. *)
      error_unported
    else if utf then
      (* pcre2_match.c:3003-3250, 3846-3959, 4445-4720 — the `if (utf)`
         min/minimize/maximize switches: M6. Lctype and utf are constant
         for the whole repeat, so this single check covers all three
         phase branches. *)
      error_unported
    else (
      fr.(fb + Frames.slot_temp_32_0) <- lmin (* Lmin *);
      fr.(fb + Frames.slot_temp_32_1) <- lmax (* Lmax *);
      fr.(fb + Frames.slot_temp_32_2) <- lctype (* Lctype *);
      fr.(fb + Frames.slot_ecode) <- ecode;
      (* pcre2_match.c:2717-2723 + 3252-3500 — first, ensure the minimum
         number of matches are present, with the type test done once at
         the start (kept out of the loops): the non-UTF switch(Lctype). *)
      if lmin > 0 then
        if Int.equal lctype Opcodes.op_any then
          (typemin_any [@tailcall]) f 1 reptype
        else if Int.equal lctype Opcodes.op_allany then
          (* pcre2_match.c:3280-3287 — OP_ALLANY: one bound check covers
             the whole block. (The OP_ANYBYTE case is commented out of the
             C, 3289-3301: \C is OP_ALLANY in non-UTF mode, so an
             OP_ANYBYTE Lctype falls to the default INTERNAL error
             below.) *)
          let eptr = fr.(fb + Frames.slot_eptr) in
          if eptr > mb.end_subject - lmin then
            let rc = scheck_partial mb eptr in
            if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_eptr) <- eptr + lmin;
            (repeattype_post_min [@tailcall]) f reptype)
        else if Int.equal lctype Opcodes.op_anynl then
          (typemin_anynl [@tailcall]) f 1 reptype
        else if Int.equal lctype Opcodes.op_not_hspace then
          (typemin_hspace [@tailcall]) f 1 true reptype
        else if Int.equal lctype Opcodes.op_hspace then
          (typemin_hspace [@tailcall]) f 1 false reptype
        else if Int.equal lctype Opcodes.op_not_vspace then
          (typemin_vspace [@tailcall]) f 1 true reptype
        else if Int.equal lctype Opcodes.op_vspace then
          (typemin_vspace [@tailcall]) f 1 false reptype
        else if Int.equal lctype Opcodes.op_not_digit then
          (typemin_ctype [@tailcall]) f 1 Chartables.ctype_digit true reptype
        else if Int.equal lctype Opcodes.op_digit then
          (typemin_ctype [@tailcall]) f 1 Chartables.ctype_digit false reptype
        else if Int.equal lctype Opcodes.op_not_whitespace then
          (typemin_ctype [@tailcall]) f 1 Chartables.ctype_space true reptype
        else if Int.equal lctype Opcodes.op_whitespace then
          (typemin_ctype [@tailcall]) f 1 Chartables.ctype_space false reptype
        else if Int.equal lctype Opcodes.op_not_wordchar then
          (typemin_ctype [@tailcall]) f 1 Chartables.ctype_word true reptype
        else if Int.equal lctype Opcodes.op_wordchar then
          (typemin_ctype [@tailcall]) f 1 Chartables.ctype_word false reptype
        else Errors.error_internal (* 3498-3499 — default *)
      else (repeattype_post_min [@tailcall]) f reptype)
  and typemin_any (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:3258-3278 — min loop for OP_ANY, not UTF: newlines do
       not match, and a CRLF pattern newline with only its CR present at
       the end of the subject could be partial. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
      else if is_newline_at eptr then (backtrack [@tailcall]) f match_nomatch
      else if
        (* pcre2_match.c:3267-3275 *)
        (not (Int.equal mb.partial 0))
        && eptr + 1 >= mb.end_subject
        && Int.equal mb.nltype Newline.nltype_fixed
        && Int.equal mb.nllen 2
        && Int.equal
             (* safe: eptr < mb.end_subject <= String.length mb.subject
                (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
             (Char.code (String.unsafe_get mb.subject eptr))
             mb.nl0
      then (
        mb.hitend <- true;
        if mb.partial > 1 then Errors.error_partial
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemin_any [@tailcall]) f (i + 1) reptype))
      else (
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        (typemin_any [@tailcall]) f (i + 1) reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_anynl (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:3302-3332 — min loop for OP_ANYNL, not UTF:
       switch( *Feptr++ ), post-increment before the tests. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        let eptr = eptr + 1 in
        fr.(fb + Frames.slot_eptr) <- eptr;
        if Int.equal fc Newline.char_cr then (
          (* pcre2_match.c:3315-3317 — CR: absorb a following LF. *)
          if
            eptr < mb.end_subject
            && Int.equal
                 (* safe: eptr < mb.end_subject (checked) *)
                 (Char.code (String.unsafe_get mb.subject eptr))
                 Newline.char_lf
          then fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemin_anynl [@tailcall]) f (i + 1) reptype)
        else if Int.equal fc Newline.char_lf then
          (typemin_anynl [@tailcall]) f (i + 1) reptype
        else if
          (* pcre2_match.c:3322-3330 — VT, FF, NEL (0x2028/0x2029 are
             excluded from 8-bit code-unit switches). *)
          Int.equal fc Newline.char_vt
          || Int.equal fc Newline.char_ff
          || Int.equal fc Newline.char_nel
        then
          if Int.equal mb.bsr_convention Options.bsr_anycrlf then
            (backtrack [@tailcall]) f match_nomatch
          else (typemin_anynl [@tailcall]) f (i + 1) reptype
        else (backtrack [@tailcall]) f match_nomatch (* 3313 — default *))
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_hspace (f : int) (i : int) (negated : bool) (reptype : int) : int
      =
    (* pcre2_match.c:3334-3372 — min loops for OP_NOT_HSPACE / OP_HSPACE,
       not UTF: switch( *Feptr++ ) over HSPACE_BYTE_CASES (the multibyte
       cases are compiled out at width 8), post-increment before the
       test. DEVIATION(structure): the C writes the two polarities as
       separate switches; merged here on [negated], per-iteration reads
       and order identical. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        if Bool.equal (hspace_byte fc) negated then
          (backtrack [@tailcall]) f match_nomatch
        else (typemin_hspace [@tailcall]) f (i + 1) negated reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_vspace (f : int) (i : int) (negated : bool) (reptype : int) : int
      =
    (* pcre2_match.c:3374-3412 — min loops for OP_NOT_VSPACE / OP_VSPACE,
       not UTF: VSPACE_BYTE_CASES. DEVIATION(structure): polarities merged
       on [negated], as at [typemin_hspace]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        if Bool.equal (vspace_byte fc) negated then
          (backtrack [@tailcall]) f match_nomatch
        else (typemin_vspace [@tailcall]) f (i + 1) negated reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_ctype (f : int) (i : int) (mask : int) (negated : bool)
      (reptype : int) : int =
    (* pcre2_match.c:3414-3496 — min loops for OP_NOT_DIGIT..OP_WORDCHAR,
       not UTF: if (MAX_255( *Feptr ) && (mb->ctypes[*Feptr] & ctype_x) !=
       0) RRETURN(MATCH_NOMATCH) for the negated forms, the complement for
       the positive forms; MAX_255() is TRUE in the 8-bit library
       (pcre2_intmodedep.h:212). Feptr++ happens AFTER the test — no
       advance on failure, unlike the single-match arms.
       DEVIATION(structure): the C writes six separate loops; merged here
       on [mask]/[negated], per-iteration reads and order identical. *)
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
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        if
          Bool.equal
            (not (Int.equal (Chartables.ctypes fc land mask) 0))
            negated
        then (backtrack [@tailcall]) f match_nomatch
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemin_ctype [@tailcall]) f (i + 1) mask negated reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and repeattype_post_min (f : int) (reptype : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:3503-3505 — if (Lmin = Lmax) we are done: continue
       with the main loop. *)
    if Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:3507-3512 + 3961-3965 — minimize, not UTF (the
         property/UTF minimize loops are M6/M7, unreachable: [repeattype]
         excludes them): the for(;;) starts with RMATCH(Fecode, RM33); the
         rest of the loop body is the RM33 resume arm in [backtrack]. *)
      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
    else (
      (* pcre2_match.c:4110-4112 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (typemax [@tailcall]) f reptype)
  and typemax (f : int) (reptype : int) : int =
    (* pcre2_match.c:4105-4110 + 4722-4956 — maximize: find the longest
       possible run, with the type test done once at the start: the
       non-UTF switch(Lctype). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let lctype = fr.(fb + Frames.slot_temp_32_2) in
    if Int.equal lctype Opcodes.op_any then
      (typemax_any [@tailcall]) f fr.(fb + Frames.slot_temp_32_0) reptype
    else if
      Int.equal lctype Opcodes.op_allany || Int.equal lctype Opcodes.op_anybyte
    then
      (* pcre2_match.c:4748-4757 — OP_ALLANY, OP_ANYBYTE: fc = Lmax -
         Lmin; take everything, or hit the subject end. *)
      let n =
        fr.(fb + Frames.slot_temp_32_1) - fr.(fb + Frames.slot_temp_32_0)
      in
      let eptr = fr.(fb + Frames.slot_eptr) in
      if n > mb.end_subject - eptr then (
        fr.(fb + Frames.slot_eptr) <- mb.end_subject;
        let rc = scheck_partial mb mb.end_subject in
        if rc < 0 then rc else (typemax_tail [@tailcall]) f reptype)
      else (
        fr.(fb + Frames.slot_eptr) <- eptr + n;
        (typemax_tail [@tailcall]) f reptype)
    else if Int.equal lctype Opcodes.op_anynl then
      (typemax_anynl [@tailcall]) f fr.(fb + Frames.slot_temp_32_0) reptype
    else if Int.equal lctype Opcodes.op_not_hspace then
      (typemax_hspace [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        true reptype
    else if Int.equal lctype Opcodes.op_hspace then
      (typemax_hspace [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        false reptype
    else if Int.equal lctype Opcodes.op_not_vspace then
      (typemax_vspace [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        true reptype
    else if Int.equal lctype Opcodes.op_vspace then
      (typemax_vspace [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        false reptype
    else if Int.equal lctype Opcodes.op_not_digit then
      (typemax_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_digit true reptype
    else if Int.equal lctype Opcodes.op_digit then
      (typemax_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_digit false reptype
    else if Int.equal lctype Opcodes.op_not_whitespace then
      (typemax_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_space true reptype
    else if Int.equal lctype Opcodes.op_whitespace then
      (typemax_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_space false reptype
    else if Int.equal lctype Opcodes.op_not_wordchar then
      (typemax_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_word true reptype
    else if Int.equal lctype Opcodes.op_wordchar then
      (typemax_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_word false reptype
    else Errors.error_internal (* 4954-4955 — default *)
  and typemax_any (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:4726-4746 — maximize scan for OP_ANY, not UTF. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* 4728-4732 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_tail [@tailcall]) f reptype
      else if is_newline_at eptr then
        (typemax_tail [@tailcall]) f reptype (* break, 4734 *)
      else if
        (* 4735-4743 — take care with CRLF partial. *)
        (not (Int.equal mb.partial 0))
        && eptr + 1 >= mb.end_subject
        && Int.equal mb.nltype Newline.nltype_fixed
        && Int.equal mb.nllen 2
        && Int.equal
             (* safe: eptr < mb.end_subject <= String.length mb.subject
                (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
             (Char.code (String.unsafe_get mb.subject eptr))
             mb.nl0
      then (
        mb.hitend <- true;
        if mb.partial > 1 then Errors.error_partial
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemax_any [@tailcall]) f (i + 1) reptype))
      else (
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        (typemax_any [@tailcall]) f (i + 1) reptype)
    else (typemax_tail [@tailcall]) f reptype
  and typemax_anynl (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:4759-4784 — maximize scan for OP_ANYNL, not UTF. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_tail [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        if Int.equal fc Newline.char_cr then (
          (* 4769-4773 — if (++Feptr >= mb->end_subject) break; absorb a
             following LF. *)
          let eptr = eptr + 1 in
          fr.(fb + Frames.slot_eptr) <- eptr;
          if eptr >= mb.end_subject then (typemax_tail [@tailcall]) f reptype
          else (
            if
              Int.equal
                (* safe: eptr < mb.end_subject (checked) *)
                (Char.code (String.unsafe_get mb.subject eptr))
                Newline.char_lf
            then fr.(fb + Frames.slot_eptr) <- eptr + 1;
            (typemax_anynl [@tailcall]) f (i + 1) reptype))
        else if
          (* 4776-4781 — break unless LF, or VT/FF/NEL outside the ANYCRLF
             convention (0x2028/0x2029 are excluded at width 8). *)
          (not (Int.equal fc Newline.char_lf))
          && (Int.equal mb.bsr_convention Options.bsr_anycrlf
             || (not (Int.equal fc Newline.char_vt))
                && (not (Int.equal fc Newline.char_ff))
                && not (Int.equal fc Newline.char_nel))
        then (typemax_tail [@tailcall]) f reptype
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemax_anynl [@tailcall]) f (i + 1) reptype)
    else (typemax_tail [@tailcall]) f reptype
  and typemax_hspace (f : int) (i : int) (negated : bool) (reptype : int) : int
      =
    (* pcre2_match.c:4786-4827 — maximize scans for OP_NOT_HSPACE /
       OP_HSPACE, not UTF (the ENDLOOP00/ENDLOOP01 gotos are the breaks
       here). DEVIATION(structure): polarities merged on [negated]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_tail [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        if Bool.equal (hspace_byte fc) negated then
          (typemax_tail [@tailcall]) f reptype
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemax_hspace [@tailcall]) f (i + 1) negated reptype)
    else (typemax_tail [@tailcall]) f reptype
  and typemax_vspace (f : int) (i : int) (negated : bool) (reptype : int) : int
      =
    (* pcre2_match.c:4828-4869 — maximize scans for OP_NOT_VSPACE /
       OP_VSPACE, not UTF (ENDLOOP02/ENDLOOP03). DEVIATION(structure):
       polarities merged on [negated]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_tail [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        if Bool.equal (vspace_byte fc) negated then
          (typemax_tail [@tailcall]) f reptype
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemax_vspace [@tailcall]) f (i + 1) negated reptype)
    else (typemax_tail [@tailcall]) f reptype
  and typemax_ctype (f : int) (i : int) (mask : int) (negated : bool)
      (reptype : int) : int =
    (* pcre2_match.c:4870-4953 — maximize scans for OP_NOT_DIGIT..
       OP_WORDCHAR, not UTF; MAX_255() is TRUE in the 8-bit library.
       DEVIATION(structure): the C writes six separate loops; merged here
       on [mask]/[negated], per-iteration reads and order identical. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_tail [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        if
          Bool.equal
            (not (Int.equal (Chartables.ctypes fc land mask) 0))
            negated
        then (typemax_tail [@tailcall]) f reptype
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemax_ctype [@tailcall]) f (i + 1) mask negated reptype)
    else (typemax_tail [@tailcall]) f reptype
  and typemax_tail (f : int) (reptype : int) : int =
    (* pcre2_match.c:4958 — if possessive, no backing up: continue the
       main loop at the advanced Fecode. *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) f
    else (typemax_bt [@tailcall]) f
  and typemax_bt (f : int) : int =
    (* pcre2_match.c:4960-4968 — the maximize backtracking for(;;) head:
       the minimum position Lstart_eptr is tried in place (break -> main
       loop); every position above it via RMATCH(Fecode, RM34). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if Int.equal fr.(fb + Frames.slot_eptr) fr.(fb + Frames.slot_temp_sptr_0)
    then (dispatch [@tailcall]) f
    else (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm34 0
  (* pcre2_match.c:5002-5007 — the OP_DNREF group-list walk: return
     Loffset for the first group in the list that is set, or for the last
     examined entry when none is set (the C's while (count-- > 0) loop
     leaves Loffset on whichever entry it stopped at). GET2(slot, 0) is
     the group number at the head of a name-table entry. *)
  and dnref_scan (f : int) (count : int) (slot : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let loffset = (Compile.get2 mb.name_table slot lsl 1) - 2 (* 5004 *) in
    if
      count > 1
      && not
           (loffset < fr.(fb + Frames.slot_offset_top)
           && not
                (Int.equal fr.(fb + Frames.slot_ovector + loffset) Frames.unset)
           )
    then (dnref_scan [@tailcall]) f (count - 1) (slot + mb.name_entry_size)
    else loffset
  (* pcre2_match.c:5017-5057 — REF_REPEAT: set up for repetition, or
     handle the non-repeated case. The repeat opcodes are read from the
     code FOLLOWING the item, as for the class repeats; [ecode] is the
     position past the OP_REF/OP_DNREF item (the C's advanced Fecode).
     The maximum and minimum are kept in the frame temporaries Lmin/Lmax
     (temp_32[0..1]). *)
  and ref_repeat (f : int) (ecode : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let next = Char.code (Bytes.get mb.start_code ecode) in
    if next >= Opcodes.op_crstar && next <= Opcodes.op_crminquery then (
      (* pcre2_match.c:5024-5034 — OP_CRSTAR..OP_CRMINQUERY: fc =
         *Fecode++ - OP_CRSTAR indexes the rep tables. (Unlike the class
         repeats, no OP_CRPOS forms appear here: possessive ref repeats
         are compiled as atomic groups.) *)
      let idx = next - Opcodes.op_crstar in
      fr.(fb + Frames.slot_temp_32_0) <- rep_min.(idx) (* Lmin *);
      fr.(fb + Frames.slot_temp_32_1) <- rep_max.(idx) (* Lmax *);
      fr.(fb + Frames.slot_ecode) <- ecode + 1;
      (ref_repeat_head [@tailcall]) f rep_typ.(idx))
    else if
      Int.equal next Opcodes.op_crrange || Int.equal next Opcodes.op_crminrange
    then (
      (* pcre2_match.c:5036-5043 — Lmin = GET2(Fecode, 1); Lmax =
         GET2(Fecode, 1 + IMM2_SIZE); max 0 => infinity. *)
      let lmax = Compile.get2 mb.start_code (ecode + 1 + Limits.imm2_size) in
      fr.(fb + Frames.slot_temp_32_0) <- Compile.get2 mb.start_code (ecode + 1);
      fr.(fb + Frames.slot_temp_32_1) <-
        (if Int.equal lmax 0 then uint32_max else lmax);
      fr.(fb + Frames.slot_ecode) <- ecode + 1 + (2 * Limits.imm2_size);
      (ref_repeat_head [@tailcall]) f rep_typ.(next - Opcodes.op_crstar))
    else (
      (* pcre2_match.c:5045-5056 — default: no repeat follows; match the
         reference once and continue with the main loop. *)
      fr.(fb + Frames.slot_ecode) <- ecode;
      let rrc =
        match_ref f
          fr.(fb + Frames.slot_temp_size)
          (not (Int.equal fr.(fb + Frames.slot_temp_32_2) 0))
          ref_length
      in
      if not (Int.equal rrc 0) then (
        if rrc > 0 then
          fr.(fb + Frames.slot_eptr) <- mb.end_subject (* partial, 5050 *);
        (* CHECK_PARTIAL() (pcre2_match.c:531-535, site 5051) *)
        let feptr = fr.(fb + Frames.slot_eptr) in
        let rc =
          if feptr >= mb.end_subject then scheck_partial mb feptr else 0
        in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch)
      else (
        fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) + !ref_length;
        (dispatch [@tailcall]) f (* continue, 5055-5056 *)))
  (* pcre2_match.c:5059-5074 — handle repeated back references. If a set
     group has length zero, just continue with the main loop, because it
     matches however many times. For an unset reference, if the minimum
     is zero, we can also just continue. We can also continue if
     PCRE2_MATCH_UNSET_BACKREF is set, because this makes unset groups
     behave as a zero-length group. For any other unset cases, carrying
     on will result in NOMATCH (the min loop's match_ref fails). *)
  and ref_repeat_head (f : int) (reptype : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let loffset = fr.(fb + Frames.slot_temp_size) in
    if
      loffset < fr.(fb + Frames.slot_offset_top)
      && not (Int.equal fr.(fb + Frames.slot_ovector + loffset) Frames.unset)
    then
      if
        Int.equal
          fr.(fb + Frames.slot_ovector + loffset)
          fr.(fb + Frames.slot_ovector + loffset + 1)
      then (dispatch [@tailcall]) f (* zero-length group: continue, 5068 *)
      else (ref_min [@tailcall]) f 1 reptype
    else if
      (* pcre2_match.c:5070-5074 — group is not set. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) 0
      || not (Int.equal (mb.poptions land Options.match_unset_backref) 0)
    then (dispatch [@tailcall]) f
    else (ref_min [@tailcall]) f 1 reptype
  (* pcre2_match.c:5076-5124 — first, ensure the minimum number of
     matches are present: for (i = 1; i <= Lmin; i++); then dispatch on
     the repeat strategy. *)
  and ref_min (f : int) (i : int) (reptype : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then
      let rrc =
        match_ref f
          fr.(fb + Frames.slot_temp_size)
          (not (Int.equal fr.(fb + Frames.slot_temp_32_2) 0))
          ref_length
      in
      if not (Int.equal rrc 0) then (
        if rrc > 0 then
          fr.(fb + Frames.slot_eptr) <- mb.end_subject (* partial, 5084 *);
        (* CHECK_PARTIAL() (pcre2_match.c:531-535, site 5085) *)
        let feptr = fr.(fb + Frames.slot_eptr) in
        let rc =
          if feptr >= mb.end_subject then scheck_partial mb feptr else 0
        in
        if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch)
      else (
        fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) + !ref_length;
        (ref_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:5091-5093 — if min = max, we are done; they are
         not both allowed to be zero. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:5095-5115 — if minimizing, keep trying and
         advancing the pointer: the for(;;) starts with RMATCH(Fecode,
         RM20); the rest of the loop body is the RM20 resume arm in
         [backtrack]. *)
      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm20 0
    else
      (* pcre2_match.c:5117-5124 — if maximizing, find the longest string
         and work backwards, as long as the matched lengths for each
         iteration are the same: Lstart = Feptr; Flength =
         Fovector[Loffset+1] - Fovector[Loffset]. In bounds: the maximize
         phase is only reached for a SET group — [ref_repeat_head]
         continues past unset groups when Lmin = 0 or under
         MATCH_UNSET_BACKREF, and otherwise the min loop above NOMATCHes
         on the unset reference first (Lmin >= 1 there). *)
      let loffset = fr.(fb + Frames.slot_temp_size) in
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr)
      (* Lstart *);
      fr.(fb + Frames.slot_length) <-
        fr.(fb + Frames.slot_ovector + loffset + 1)
        - fr.(fb + Frames.slot_ovector + loffset);
      (ref_max_scan [@tailcall]) f fr.(fb + Frames.slot_temp_32_0) true
  (* pcre2_match.c:5126-5146 — the maximize scan: for (i = Lmin; i <
     Lmax; i++), tracking whether every iteration matched the same number
     of code units. [samelengths] is the C's local BOOL (5122): constant
     across the scan — no RMATCH intervenes — so a parameter suffices. *)
  and ref_max_scan (f : int) (i : int) (samelengths : bool) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then (
      let rrc =
        match_ref f
          fr.(fb + Frames.slot_temp_size)
          (not (Int.equal fr.(fb + Frames.slot_temp_32_2) 0))
          ref_length
      in
      if not (Int.equal rrc 0) then
        (* pcre2_match.c:5130-5142 — can't use CHECK_PARTIAL because we
           don't want to update Feptr in the soft partial matching
           case. *)
        if
          rrc > 0
          && (not (Int.equal mb.partial 0))
          && mb.end_subject > mb.start_used_ptr
        then (
          mb.hitend <- true;
          if mb.partial > 1 then Errors.error_partial (* return, 5139 *)
          else (ref_max_end [@tailcall]) f i samelengths (* break, 5141 *))
        else (ref_max_end [@tailcall]) f i samelengths (* break, 5141 *)
      else
        (* pcre2_match.c:5144-5145 *)
        let samelengths =
          if not (Int.equal !ref_length fr.(fb + Frames.slot_length)) then false
          else samelengths
        in
        fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) + !ref_length;
        (ref_max_scan [@tailcall]) f (i + 1) samelengths)
    else (ref_max_end [@tailcall]) f i samelengths
  (* pcre2_match.c:5148-5186 — after the scan: if the length matched for
     each repetition is the same as the length of the captured group, we
     can easily work backwards (the normal case); the rare non-matching
     lengths case (caseless UTF pairs of case-equivalent characters with
     different unit counts) re-matches fewer and fewer times. *)
  and ref_max_end (f : int) (i : int) (samelengths : bool) : int =
    if samelengths then (ref_max_same_bt [@tailcall]) f
    else
      (* pcre2_match.c:5167-5172 — Lmax = i; the for(;;) starts with
         RMATCH(Fecode, RM22); the rest of the loop body is the RM22
         resume arm in [backtrack]. *)
      let fr = a.Frames.frames in
      let fb = Frames.base a f in
      fr.(fb + Frames.slot_temp_32_1) <- i (* Lmax *);
      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm22 0
  (* pcre2_match.c:5154-5162 + 5186 — the samelengths backtracking while
     head: while (Feptr >= Lstart) try the rest of the pattern via
     RMATCH(Fecode, RM21), stepping Feptr back by Flength per failure
     (the RM21 resume arm); when Feptr falls below Lstart,
     RRETURN(MATCH_NOMATCH). *)
  and ref_max_same_bt (f : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) >= fr.(fb + Frames.slot_temp_sptr_0) then
      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm21 0
    else (backtrack [@tailcall]) f match_nomatch
  (* pcre2_match.c:5177-5182 — the non-samelengths re-scan: for (i =
     Lmin; i < Lmax; i++) re-match the reference — match_ref is known to
     succeed every time (5164-5165), so its return code is discarded,
     exactly as the C's (void) cast — then try the rest of the pattern
     again (RM22). *)
  and ref_max_rescan (f : int) (i : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then (
      let (_ : int) =
        match_ref f
          fr.(fb + Frames.slot_temp_size)
          (not (Int.equal fr.(fb + Frames.slot_temp_32_2) 0))
          ref_length
      in
      fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) + !ref_length;
      (ref_max_rescan [@tailcall]) f (i + 1))
    else (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm22 0
  and word_boundary_tail (f : int) (prev_is_word : bool) (cur_is_word : bool) :
      int =
    (* pcre2_match.c:6330-6333 — now see if the situation is what we want:
       *Fecode++ advances past the opcode either way; RRETURN(NOMATCH)
       when a boundary opcode sees cur == prev, or a not-boundary opcode
       sees cur != prev. The UCP variants are excluded by the caller
       (M7). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
    if
      if Int.equal fr.(fb + Frames.slot_op) Opcodes.op_word_boundary then
        Bool.equal cur_is_word prev_is_word
      else not (Bool.equal cur_is_word prev_is_word)
    then (backtrack [@tailcall]) f match_nomatch
    else (dispatch [@tailcall]) f
  and op_eod_tail (f : int) : int =
    (* pcre2_match.c:6154-6163 — the OP_EOD body, shared as the
       fallthrough tail of OP_DOLL under PCRE2_DOLLAR_ENDONLY (fallthrough
       from OP_DOLL in C, 6149-6152). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) < mb.true_end_subject then
      (backtrack [@tailcall]) f match_nomatch
    else if not (Int.equal mb.partial 0) then (
      mb.hitend <- true;
      if mb.partial > 1 then Errors.error_partial
      else (
        fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
        (dispatch [@tailcall]) f))
    else (
      fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
      (dispatch [@tailcall]) f)
  and assert_nl_or_eos (f : int) : int =
    (* pcre2_match.c:6166-6192 — the OP_EODN body (\Z), also the
       ASSERT_NL_OR_EOS goto target of OP_DOLL (6149): end of subject, or
       a newline that is the last thing in the subject. IS_NEWLINE may
       update mb.nllen (NLTYPE_ANY/ANYCRLF), which the end_subject - nllen
       comparison then reads — the C's evaluation order. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let eptr = fr.(fb + Frames.slot_eptr) in
    if
      eptr < mb.end_subject
      && ((not (is_newline_at eptr))
         || not (Int.equal eptr (mb.end_subject - mb.nllen)))
    then
      if
        (* pcre2_match.c:6171-6180 — a CRLF pattern newline with only its
           CR present at the end of the subject could be partial. *)
        (not (Int.equal mb.partial 0))
        && eptr + 1 >= mb.end_subject
        && Int.equal mb.nltype Newline.nltype_fixed
        && Int.equal mb.nllen 2
        && Int.equal
             (* safe: eptr < mb.end_subject (first conjunct above) <=
                String.length mb.subject; 0 <= start_eptr <= eptr (mb
                invariant) *)
             (Char.code (String.unsafe_get mb.subject eptr))
             mb.nl0
      then (
        mb.hitend <- true;
        if mb.partial > 1 then Errors.error_partial
        else (backtrack [@tailcall]) f match_nomatch)
      else (backtrack [@tailcall]) f match_nomatch
    else if
      (* pcre2_match.c:6184-6190 — either at end of string or \n before
         end. *)
      not (Int.equal mb.partial 0)
    then (
      mb.hitend <- true;
      if mb.partial > 1 then Errors.error_partial
      else (
        fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
        (dispatch [@tailcall]) f))
    else (
      fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
      (dispatch [@tailcall]) f)
  and skip_alts (e : int) : int =
    (* The branch-chain walk `do Fecode += GET(Fecode, 1); while ( *Fecode
       == OP_ALT)` shared by OP_BRAZERO (pcre2_match.c:5228), OP_BRAMINZERO
       (5234), OP_SKIPZERO (5244) and OP_ALT (5896): step at least once,
       then keep following links while sitting on an OP_ALT; returns the
       final ket's offset. Terminates: branch links in a complete compiled
       program chain from the bracket to its ket (mb invariant). *)
    let e = e + Compile.get mb.start_code (e + 1) in
    if Int.equal (Char.code (Bytes.get mb.start_code e)) Opcodes.op_alt then
      (skip_alts [@tailcall]) e
    else e
  and ket_branch_start (bs : int) (be : int) : int =
    (* pcre2_match.c:5914-5916 — branch_start = bracode; while
       (branch_start + GET(branch_start, 1) != branch_end) branch_start +=
       GET(branch_start, 1). Terminates: [be] is either this group's ket
       or an OP_ALT recorded by the just-executed OP_ALT arm of the same
       group, both on the bracket's branch chain (mb invariant). *)
    let nx = bs + Compile.get mb.start_code (bs + 1) in
    if Int.equal nx be then bs else (ket_branch_start [@tailcall]) nx be
  and bra_loop (f : int) : int =
    (* pcre2_match.c:5356-5372 — the OP_BRA branch loop (no THEN in the
       pattern, not at the top level): remember the next branch in
       Lnext_branch (temp_sptr[0], 5347); if this is not the final branch,
       record a backtracking point for it (RM1), otherwise run the final
       branch at this level without a new frame. Entered with Fecode at
       the bracket (first call) or at an OP_ALT (RM1 resume);
       OP_lengths[*Fecode] steps over either item. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let ecode = fr.(fb + Frames.slot_ecode) in
    let next_branch = ecode + Compile.get mb.start_code (ecode + 1) in
    fr.(fb + Frames.slot_temp_sptr_0) <- next_branch (* Lnext_branch *);
    if
      not
        (Int.equal
           (Char.code (Bytes.get mb.start_code next_branch))
           Opcodes.op_alt)
    then (
      (* pcre2_match.c:5369-5371 — hit the start of the final branch:
         continue at this level. *)
      fr.(fb + Frames.slot_ecode) <-
        ecode + Opcodes.op_lengths.(Char.code (Bytes.get mb.start_code ecode));
      (dispatch [@tailcall]) f)
    else
      (* pcre2_match.c:5361-5364 — never the final branch; no MATCH_THEN
         test needed because this code is not used when there is a THEN in
         the pattern. *)
      (rmatch [@tailcall]) f
        (ecode + Opcodes.op_lengths.(Char.code (Bytes.get mb.start_code ecode)))
        rm1 0
  and grouploop (f : int) : int =
    (* pcre2_match.c:5396-5400 — the GROUPLOOP head shared by OP_BRA (top
       level / THEN present), OP_CBRA/OP_SCBRA and OP_ONCE/OP_SCRIPT_RUN/
       OP_SBRA: record a backtracking point for the branch, passing the
       group frame type saved in Lframe_type (temp_32[0], 5346). Entered
       with Fecode at the bracket (first call) or at an OP_ALT (RM2
       resume); OP_lengths[*Fecode] steps over either item. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let ecode = fr.(fb + Frames.slot_ecode) in
    (rmatch [@tailcall]) f
      (ecode + Opcodes.op_lengths.(Char.code (Bytes.get mb.start_code ecode)))
      rm2
      fr.(fb + Frames.slot_temp_32_0)
  and op_ket_tail (f : int) (p : int) (bracode : int) : int =
    (* pcre2_match.c:6087-6127 — the common ket tail after the *bracode
       switch. [p] is the C's P as a frame index, -1 = NULL. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let ecode = fr.(fb + Frames.slot_ecode) in
    if Int.equal (Char.code (Bytes.get mb.start_code ecode)) Opcodes.op_ketrpos
    then
      (* pcre2_match.c:6092-6098 — OP_KETRPOS: copy the frame data back to
         P and return MATCH_KETRPOS so the repeats are done one at a time
         from the outer level — STUB: M4 possessive chunk (OP_KETRPOS is
         only compiled for the BRAPOS groups, whose bracket arms are
         stubs). *)
      error_unported
    else if
      (* pcre2_match.c:6100-6121 — a non-repeating ket needs no special
         action, just continuing at this level. This also happens for the
         repeating kets if the group matched no characters, in order to
         forcibly break infinite loops. Otherwise, the repeating kets try
         the rest of the pattern or restart from the preceding bracket, in
         the appropriate order. *)
      (not (Int.equal fr.(fb + Frames.slot_op) Opcodes.op_ket))
      && (Int.equal p (-1)
         || not
              (Int.equal
                 fr.(fb + Frames.slot_eptr)
                 fr.(Frames.base a p + Frames.slot_eptr)))
    then
      if Int.equal fr.(fb + Frames.slot_op) Opcodes.op_ketrmin then
        (* pcre2_match.c:6109-6111 — try the rest of the pattern first
           (minimizing). *)
        (rmatch [@tailcall]) f (ecode + 1 + Limits.link_size) rm6 0
      else
        (* pcre2_match.c:6117-6119 — repeat the maximum number of times
           (KETRMAX): restart from the preceding bracket first. *)
        (rmatch [@tailcall]) f bracode rm7 0
    else (
      (* pcre2_match.c:6123-6126 — carry on at this level for a
         non-repeating ket, or after matching an empty string, or after
         repeating for a maximum number of times. *)
      fr.(fb + Frames.slot_ecode) <- ecode + 1 + Limits.link_size;
      (dispatch [@tailcall]) f)
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
      | 1 ->
          (* L_RM1 (pcre2_match.c:5365-5366) — OP_BRA optimized branch
             walk: the branch failed; move Fecode to the next branch
             (saved in Lnext_branch) and loop. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else (
            fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_temp_sptr_0);
            (bra_loop [@tailcall]) f)
      | 2 ->
          (* L_RM2 (pcre2_match.c:5401-5410) — GROUPLOOP: the branch
             failed. The MATCH_THEN handling (5401-5407) consults
             mb->verb_ecode_ptr: M5 verbs chunk — no opcode can return
             MATCH_THEN until then; site kept for evaluation order. Then
             advance to the next alternative, failing the group when there
             is none. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else
            let ecode =
              fr.(fb + Frames.slot_ecode)
              + Compile.get mb.start_code (fr.(fb + Frames.slot_ecode) + 1)
            in
            fr.(fb + Frames.slot_ecode) <- ecode;
            if
              not
                (Int.equal
                   (Char.code (Bytes.get mb.start_code ecode))
                   Opcodes.op_alt)
            then (backtrack [@tailcall]) f match_nomatch
            else (grouploop [@tailcall]) f
      | 6 ->
          (* L_RM6 (pcre2_match.c:6112-6114) — KETRMIN: the rest of the
             pattern failed; go back to the preceding bracket and iterate
             the group once more in this frame (Fecode -= GET(Fecode, 1)),
             then break out of the ket processing. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else
            let ecode = fr.(fb + Frames.slot_ecode) in
            fr.(fb + Frames.slot_ecode) <-
              ecode - Compile.get mb.start_code (ecode + 1);
            (dispatch [@tailcall]) f
      | 7 ->
          (* L_RM7 (pcre2_match.c:6120-6126) — KETRMAX: the re-iteration
             failed; carry on at this level after the group
             (Fecode += 1 + LINK_SIZE). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else (
            fr.(fb + Frames.slot_ecode) <-
              fr.(fb + Frames.slot_ecode) + 1 + Limits.link_size;
            (dispatch [@tailcall]) f)
      | 9 ->
          (* L_RM9 (pcre2_match.c:5227-5229) — BRAZERO: taking the group
             failed; walk Lnext_ecode from the bracket to its final ket
             and continue after the group (zero repeat). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else
            let next = skip_alts fr.(fb + Frames.slot_temp_sptr_0) in
            fr.(fb + Frames.slot_temp_sptr_0) <- next
            (* Lnext_ecode, post-walk *);
            fr.(fb + Frames.slot_ecode) <- next + 1 + Limits.link_size;
            (dispatch [@tailcall]) f
      | 10 ->
          (* L_RM10 (pcre2_match.c:5236-5237) — BRAMINZERO: the rest of
             the pattern without the group failed; step into the group
             (Fecode++). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else (
            fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
            (dispatch [@tailcall]) f)
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
      | 23 ->
          (* L_RM23 (pcre2_match.c:2053-2071) — class repeat, minimize,
             not UTF: the tail failed; take one more matching character
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
                (* pcre2_match.c:2062 — fc = *Feptr++: post-increment. *)
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let fc = Char.code (String.unsafe_get mb.subject eptr) in
                fr.(fb + Frames.slot_eptr) <- eptr + 1;
                if Int.equal (class_bit f fc) 0 then
                  (backtrack [@tailcall]) f match_nomatch
                else (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm23 0
      | 24 ->
          (* L_RM24 (pcre2_match.c:2145-2147) — class repeat, maximize:
             rrc test, then Feptr--, then back to the while head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else (
            fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - 1;
            (class_maxbt [@tailcall]) f)
      | 33 ->
          (* L_RM33 (pcre2_match.c:3965-4100) — type repeat, minimize, not
             UTF: the switch(Lctype) is re-tested every iteration (the C
             note at 3507-3510: all four temp_32 slots are in use, so no
             local "notmatch"). *)
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
                let lctype = fr.(fb + Frames.slot_temp_32_2) in
                (* pcre2_match.c:3973-3974 *)
                if Int.equal lctype Opcodes.op_any && is_newline_at eptr then
                  (backtrack [@tailcall]) f match_nomatch
                else
                  (* pcre2_match.c:3975 — fc = *Feptr++. *)
                  (* safe: eptr < mb.end_subject <= String.length
                     mb.subject (checked above); 0 <= start_eptr <= eptr
                     (mb invariant) *)
                  let fc = Char.code (String.unsafe_get mb.subject eptr) in
                  let eptr = eptr + 1 in
                  fr.(fb + Frames.slot_eptr) <- eptr;
                  (* pcre2_match.c:3976-4098 — switch(Lctype). *)
                  if Int.equal lctype Opcodes.op_any then
                    (* 3978-3989 — the non-NL case; take care with CRLF
                       partial (Feptr is already past fc, hence the >=
                       end_subject form). *)
                    if
                      (not (Int.equal mb.partial 0))
                      && eptr >= mb.end_subject
                      && Int.equal mb.nltype Newline.nltype_fixed
                      && Int.equal mb.nllen 2 && Int.equal fc mb.nl0
                    then (
                      mb.hitend <- true;
                      if mb.partial > 1 then Errors.error_partial
                      else
                        (rmatch [@tailcall]) f
                          fr.(fb + Frames.slot_ecode)
                          rm33 0)
                    else
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                  else if
                    (* 3990-3992 *)
                    Int.equal lctype Opcodes.op_allany
                    || Int.equal lctype Opcodes.op_anybyte
                  then (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                  else if Int.equal lctype Opcodes.op_anynl then
                    (* 3994-4017 *)
                    if Int.equal fc Newline.char_cr then (
                      if
                        eptr < mb.end_subject
                        && Int.equal
                             (* safe: eptr < mb.end_subject (checked) *)
                             (Char.code (String.unsafe_get mb.subject eptr))
                             Newline.char_lf
                      then fr.(fb + Frames.slot_eptr) <- eptr + 1;
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0)
                    else if Int.equal fc Newline.char_lf then
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                    else if
                      Int.equal fc Newline.char_vt
                      || Int.equal fc Newline.char_ff
                      || Int.equal fc Newline.char_nel
                    then
                      if Int.equal mb.bsr_convention Options.bsr_anycrlf then
                        (backtrack [@tailcall]) f match_nomatch
                      else
                        (rmatch [@tailcall]) f
                          fr.(fb + Frames.slot_ecode)
                          rm33 0
                    else (backtrack [@tailcall]) f match_nomatch (* 3996 *)
                  else if Int.equal lctype Opcodes.op_not_hspace then
                    (* 4019-4029 *)
                    if hspace_byte fc then
                      (backtrack [@tailcall]) f match_nomatch
                    else
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                  else if Int.equal lctype Opcodes.op_hspace then
                    (* 4031-4041 *)
                    if hspace_byte fc then
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                    else (backtrack [@tailcall]) f match_nomatch
                  else if Int.equal lctype Opcodes.op_not_vspace then
                    (* 4043-4053 *)
                    if vspace_byte fc then
                      (backtrack [@tailcall]) f match_nomatch
                    else
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                  else if Int.equal lctype Opcodes.op_vspace then
                    (* 4055-4065 *)
                    if vspace_byte fc then
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                    else (backtrack [@tailcall]) f match_nomatch
                  else if Int.equal lctype Opcodes.op_not_digit then
                    (* 4067-4070 *)
                    if
                      not
                        (Int.equal
                           (Chartables.ctypes fc land Chartables.ctype_digit)
                           0)
                    then (backtrack [@tailcall]) f match_nomatch
                    else
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                  else if Int.equal lctype Opcodes.op_digit then
                    (* 4072-4075 *)
                    if
                      Int.equal
                        (Chartables.ctypes fc land Chartables.ctype_digit)
                        0
                    then (backtrack [@tailcall]) f match_nomatch
                    else
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                  else if Int.equal lctype Opcodes.op_not_whitespace then
                    (* 4077-4080 *)
                    if
                      not
                        (Int.equal
                           (Chartables.ctypes fc land Chartables.ctype_space)
                           0)
                    then (backtrack [@tailcall]) f match_nomatch
                    else
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                  else if Int.equal lctype Opcodes.op_whitespace then
                    (* 4082-4085 *)
                    if
                      Int.equal
                        (Chartables.ctypes fc land Chartables.ctype_space)
                        0
                    then (backtrack [@tailcall]) f match_nomatch
                    else
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                  else if Int.equal lctype Opcodes.op_not_wordchar then
                    (* 4087-4090 *)
                    if
                      not
                        (Int.equal
                           (Chartables.ctypes fc land Chartables.ctype_word)
                           0)
                    then (backtrack [@tailcall]) f match_nomatch
                    else
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                  else if Int.equal lctype Opcodes.op_wordchar then
                    (* 4092-4095 *)
                    if
                      Int.equal
                        (Chartables.ctypes fc land Chartables.ctype_word)
                        0
                    then (backtrack [@tailcall]) f match_nomatch
                    else
                      (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm33 0
                  else Errors.error_internal (* 4097-4099 — default *)
      | 34 ->
          (* L_RM34 (pcre2_match.c:4963-4967) — type repeat, maximize: rrc
             test, Feptr--, then the \R CRLF double step (backing into the
             middle of a CRLF pair is not a valid \R position), then back
             to the for(;;) head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else
            let eptr = fr.(fb + Frames.slot_eptr) - 1 in
            fr.(fb + Frames.slot_eptr) <- eptr;
            if
              Int.equal fr.(fb + Frames.slot_temp_32_2) Opcodes.op_anynl
              && eptr > fr.(fb + Frames.slot_temp_sptr_0)
              && Int.equal
                   (* safe: Lstart_eptr <= eptr (guard above, so eptr >=
                      0) and eptr < the pre-decrement position <=
                      end_subject <= String.length mb.subject; eptr - 1
                      >= Lstart_eptr >= 0 *)
                   (Char.code (String.unsafe_get mb.subject eptr))
                   Newline.char_lf
              && Int.equal
                   (Char.code (String.unsafe_get mb.subject (eptr - 1)))
                   Newline.char_cr
            then fr.(fb + Frames.slot_eptr) <- eptr - 1;
            (typemax_bt [@tailcall]) f
      | 20 ->
          (* L_RM20 (pcre2_match.c:5102-5112) — backref repeat, minimize:
             the tail failed; unless the maximum is reached, take one
             more copy of the reference and try again. Lmin++ compares
             the OLD count and bumps it regardless (5104). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) f match_nomatch
            else
              let rrc2 =
                match_ref f
                  fr.(fb + Frames.slot_temp_size)
                  (not (Int.equal fr.(fb + Frames.slot_temp_32_2) 0))
                  ref_length
              in
              if not (Int.equal rrc2 0) then (
                if rrc2 > 0 then fr.(fb + Frames.slot_eptr) <- mb.end_subject
                  (* partial, 5108 *);
                (* CHECK_PARTIAL() (pcre2_match.c:531-535, site 5109) *)
                let feptr = fr.(fb + Frames.slot_eptr) in
                let rc =
                  if feptr >= mb.end_subject then scheck_partial mb feptr else 0
                in
                if rc < 0 then rc else (backtrack [@tailcall]) f match_nomatch)
              else (
                fr.(fb + Frames.slot_eptr) <-
                  fr.(fb + Frames.slot_eptr) + !ref_length;
                (rmatch [@tailcall]) f fr.(fb + Frames.slot_ecode) rm20 0)
      | 21 ->
          (* L_RM21 (pcre2_match.c:5158-5160) — backref repeat, maximize,
             same lengths: rrc test, then Feptr -= Flength, then back to
             the while head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else (
            fr.(fb + Frames.slot_eptr) <-
              fr.(fb + Frames.slot_eptr) - fr.(fb + Frames.slot_length);
            (ref_max_same_bt [@tailcall]) f)
      | 22 ->
          (* L_RM22 (pcre2_match.c:5172-5182) — backref repeat, maximize,
             differing lengths: fail after the minimal repetition when
             Feptr is back at Lstart (break -> RRETURN(MATCH_NOMATCH),
             5174/5186); otherwise re-scan one fewer repetition from
             Lstart. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) f rrc
          else if
            Int.equal
              fr.(fb + Frames.slot_eptr)
              fr.(fb + Frames.slot_temp_sptr_0)
          then (backtrack [@tailcall]) f match_nomatch
          else (
            fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_temp_sptr_0)
            (* Feptr = Lstart, 5175 *);
            fr.(fb + Frames.slot_temp_32_1) <-
              fr.(fb + Frames.slot_temp_32_1) - 1 (* Lmax--, 5176 *);
            (ref_max_rescan [@tailcall]) f fr.(fb + Frames.slot_temp_32_0))
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

(* ---------- Match a Regular Expression (the driver) ---------- *)

(* pcre2_internal.h:537 — PCRE2_HASTHEN: the pattern contains ( *THEN).
   Compile does not set this bit until the M5 verbs chunk; the other
   re->flags bits the driver reads live in Compile (firstset & co). *)
let flag_hasthen = 0x00001000

(* pcre2_internal.h:566-575 — REQ_CU_MAX, 8-bit library: the maximum
   remaining length of subject we are prepared to search for a req_unit
   match from an anchored pattern (memchr() is used and is fast in 8-bit
   mode). *)
let req_cu_max = 5000

(* memchr(p, c, n) over the subject: the offset of the first occurrence of
   code unit [c] in subject[p .. p+n), or -1 (the C's NULL). Caller
   contract: 0 <= p and p + n <= String.length subject (every driver call
   site passes n = e - p for some e <= end_subject <= String.length
   subject and p >= 0). *)
let memchr_subject (subject : string) (p : int) (c : int) (n : int) : int =
  let e = p + n in
  let rec scan (i : int) : int =
    if i >= e then -1
    else if
      (* safe: p <= i < e <= String.length subject and p >= 0 (caller
         contract above) *)
      Int.equal (Char.code (String.unsafe_get subject i)) c
    then i
    else (scan [@tailcall]) (i + 1)
  in
  scan p

(* pcre2_match.c:6505-6532 — pcre2_match(): apply a compiled pattern to a
   subject string and pick out portions of the string if it matches. Two
   elements in the vector are set for each substring: the offsets to the
   start and end of the substring.

   Arguments (6513-6520; length is String.length subject at this seam):
     re            the compiled expression
     subject       the subject string
     start_offset  where to start in the subject string
     options       option bits (widened once at the seam, Options.of_int32)
     match_data    the match data block (results)

   Returns (6522-6527): > 0 => success; value is the number of ovector
   pairs filled; = 0 => success, but ovector is not big enough; -1 =>
   failed to match (PCRE2_ERROR_NOMATCH); -2 => partial match
   (PCRE2_ERROR_PARTIAL); < -2 => some kind of unexpected problem.

   Boundary notes for the OCaml seam (locals and early checks, 6534-6619):
   - there is no JIT in this port: use_jit (6563-6565) and the whole JIT
     matching block (6639-6646, 6691-6788) do not exist;
   - subject == NULL / code == NULL / match_data == NULL (6591-6599): N/A,
     none can be NULL at this seam (PCRE2_ERROR_NULL is unreachable);
   - length == PCRE2_ZERO_TERMINATED (6603-6607): N/A, the seam always
     passes an explicit-length string;
   - magic_number (6612-6614) and the code unit width check (6616-6619):
     N/A, [re] is a typed Compile.re. *)
let pcre2_match (re : Compile.re) ~(subject : string) ~(start_offset : int)
    ~(options : int) (match_data : match_data) : int =
  let length = String.length subject in
  (* pcre2_match.c:6595-6597 — plausibility checks: undefined public match
     option bits (port-conventions §5: -34 at match time). *)
  if not (Int.equal (options land lnot Options.public_match_options) 0) then
    Errors.error_badoption
  else if
    (* pcre2_match.c:6601-6610 — start_match/req_cu_ptr become the
       bump-along loop's starting state below; start_offset is PCRE2_SIZE
       (unsigned), so a negative OCaml int is a huge unsigned value, also
       > length (port-conventions §5: negative start offset, or offset >
       subject length -> -33). This is the check that establishes the
       0 <= start_eptr <= end_subject invariant [match_] relies on. *)
    start_offset < 0 || start_offset > length
  then Errors.error_badoffset
  else
    (* pcre2_match.c:6608 *)
    let true_end_subject = length in
    (* pcre2_match.c:6621-6637 — transfer the pattern's ( *NOTEMPTY) /
       ( *NOTEMPTY_ATSTART) flag bits into the options for this function:
       options |= (re->flags & FF) / ((FF & (~FF+1)) / (OO & (~OO+1))).
       x & (~x+1) extracts the lowest set bit (= x land (-x) on OCaml
       ints): the divisor is 0x10000 / 4 = 0x4000. *)
    let ff = Compile.notempty_set lor Compile.ne_atst_set in
    let oo = Options.notempty lor Options.notempty_atstart in
    let options =
      options lor (re.Compile.flags land ff / (ff land -ff / (oo land -oo)))
    in
    (* pcre2_match.c:6648-6654 — initialize UTF/UCP parameters.
       allow_invalid (6652, PCRE2_MATCH_INVALID_UTF) is M6 with the rest of
       the invalid-UTF machinery. *)
    let utf = not (Int.equal (re.Compile.overall_options land Options.utf) 0) in
    let ucp = not (Int.equal (re.Compile.overall_options land Options.ucp) 0) in
    (* pcre2_match.c:6656-6659 — convert the partial matching flags into an
       integer. *)
    let partial =
      if not (Int.equal (options land Options.partial_hard) 0) then 2
      else if not (Int.equal (options land Options.partial_soft) 0) then 1
      else 0
    in
    if
      (* pcre2_match.c:6661-6666 — partial matching and PCRE2_ENDANCHORED
         are currently not allowed at the same time. *)
      (not (Int.equal partial 0))
      && not
           (Int.equal
              (re.Compile.overall_options lor options land Options.endanchored)
              0)
    then Errors.error_badoption
    else (
      (* pcre2_match.c:6668-6673 — an offset limit without
         PCRE2_USE_OFFSET_LIMIT is PCRE2_ERROR_BADOFFSETLIMIT. No match
         context exists at this seam yet (the mcontext knobs are M8+), so
         mcontext == NULL and the check cannot fire. *)
      (* pcre2_match.c:6675-6684 — PCRE2_MD_COPIED_SUBJECT bookkeeping and
         match_data->subject = NULL: dropped (GC strings, no subject
         pointer in the OCaml match_data). *)
      (* pcre2_match.c:6686-6688 — zero the error offset in case the first
         code unit is invalid UTF. *)
      match_data.startchar <- 0;
      (* pcre2_match.c:6691-6788 — JIT matching: no JIT in this port. *)
      (* pcre2_match.c:6795 + 6813-6929 — mb->check_subject and the UTF
         subject validity check. DEFERRED (M6, docs/ocaml-engine/07-utf8.md):
         PCRE2_UTF patterns do not yet compile (Parse defers them), so no
         UTF subject reaches this point. The M6 chunk ports PRIV(valid_utf),
         the max_lookbehind walk-back, PCRE2_ERROR_BADUTFOFFSET, the
         invalid-UTF fragment protocol (FRAGMENT_RESTART, 7140/7646-7701)
         and fragment_options, and owns the mb->check_subject field. *)
      (* pcre2_match.c:6931-6939 — a NULL match context means "use a
         default context"; the memory control functions are dropped (GC). *)
      (* pcre2_match.c:6941-6945 *)
      let anchored =
        not
          (Int.equal
             (re.Compile.overall_options lor options land Options.anchored)
             0)
      in
      let firstline =
        (not anchored)
        && not (Int.equal (re.Compile.overall_options land Options.firstline) 0)
      in
      let startline =
        not (Int.equal (re.Compile.flags land Compile.startline) 0)
      in
      (* mcontext->offset_limit is PCRE2_UNSET in the default match context
         (the offset-limit knob is M8+). *)
      let bumpalong_limit = true_end_subject in
      (* pcre2_match.c:6947-6960 — the callout block and the mb callout
         fields: callout support is M8; no callout block exists (see the
         RETURN_SWITCH note in [backtrack]). *)
      (* pcre2_match.c:6981-7017 — process the \R and newline settings
         (bsr goes straight into the mb literal below). The C switch's
         default returns PCRE2_ERROR_INTERNAL. *)
      let nltype = ref Newline.nltype_fixed in
      let nllen = ref 0 in
      let nl0 = ref 0 in
      let nl1 = ref 0 in
      let nl_valid = ref true in
      let nlc = re.Compile.newline_convention in
      if Int.equal nlc Options.newline_cr then (
        nllen := 1;
        nl0 := Newline.char_cr)
      else if Int.equal nlc Options.newline_lf then (
        nllen := 1;
        nl0 := Newline.char_lf)
      else if Int.equal nlc Options.newline_nul then (
        nllen := 1;
        nl0 := 0 (* CHAR_NUL *))
      else if Int.equal nlc Options.newline_crlf then (
        nllen := 2;
        nl0 := Newline.char_cr;
        nl1 := Newline.char_lf)
      else if Int.equal nlc Options.newline_any then
        nltype := Newline.nltype_any
      else if Int.equal nlc Options.newline_anycrlf then
        nltype := Newline.nltype_anycrlf
      else nl_valid := false;
      if not !nl_valid then Errors.error_internal
      else
        (* pcre2_match.c:7036-7046 — limits set in the pattern override the
           match context only if they are smaller. The default match
           context carries the build defaults (module Limits); the
           re->limit_xxx fields are 0xffff_ffff unless ( *LIMIT_...=) set
           them, so the unsigned < holds on plain ints. *)
        let heap_limit =
          if Limits.heap_limit < re.Compile.limit_heap then Limits.heap_limit
          else re.Compile.limit_heap
        in
        let match_limit =
          if Limits.match_limit < re.Compile.limit_match then Limits.match_limit
          else re.Compile.limit_match
        in
        let match_limit_depth =
          if Limits.match_limit_depth < re.Compile.limit_depth then
            Limits.match_limit_depth
          else re.Compile.limit_depth
        in
        (* pcre2_match.c:7019-7034 + 7048-7083 — frame_size, the initial
           frames-vector sizing under the heap limit, and the frame-0
           ovector unset fill all live in Frames.create. DEVIATION
           (frames.ml): no frames vector is cached across matches, so the
           keep-if-big-enough path (7062-7077) always reallocates. *)
        match Frames.create ~top_bracket:re.Compile.top_bracket ~heap_limit with
        | Error e -> e
        | Ok a ->
            (* pcre2_match.c:6956-6979 + 6658 + 6983 + 7039-7046 — fill in
               the fields of the match block, except for moptions,
               start_used_ptr, last_used_ptr, match_call_count and
               end_offset_top, which are set per attempt (7499-7508).
               mb->ignore_skip_arg (6970) and mb->skip_arg_count are M5
               fields (see the match_block comment). *)
            let mb =
              {
                match_limit;
                match_limit_depth;
                match_call_count = 0;
                hitend = false;
                hasthen =
                  not (Int.equal (re.Compile.flags land flag_hasthen) 0)
                  (* 6966 *);
                allowemptypartial =
                  re.Compile.max_lookbehind > 0
                  || not
                       (Int.equal (re.Compile.flags land Compile.match_empty) 0)
                  (* 6967-6968 *);
                subject;
                start_offset (* 6963 *);
                end_offset_top = 0;
                partial (* 6658-6659 *);
                bsr_convention = re.Compile.bsr_convention (* 6983 *);
                name_count = re.Compile.name_count (* 6977 *);
                name_entry_size = re.Compile.name_entry_size (* 6978 *);
                name_table = re.Compile.name_table (* 6976 *);
                start_code = re.Compile.code (* 6979: codestart = offset 0 *);
                start_subject = 0 (* 6962 *);
                end_subject =
                  length (* 6964; M6 shortens this for invalid-UTF fragments *);
                true_end_subject (* 6965 *);
                end_match_ptr = 0;
                start_used_ptr = 0;
                last_used_ptr = 0;
                mark = Frames.unset;
                nomatch_mark = Frames.unset (* 6971: in case never set *);
                moptions = 0 (* 6956-6957: gets set later, per attempt *);
                poptions = re.Compile.overall_options (* 6969 *);
                nltype = !nltype;
                nllen = !nllen;
                nl0 = !nl0;
                nl1 = !nl1;
              }
            in
            (* pcre2_internal.h:496-521 — IS_NEWLINE(p) / WAS_NEWLINE(p)
               for the driver's own scan sites (7176/7184, 7328/7336,
               7588): the same transcription as inside [match_] (NLBLOCK is
               mb, PSEND is end_subject, PSSTART is start_subject,
               pcre2_match.c:60-66), with PRIV(is_newline)'s out-parameter
               copied to mb.nllen on TRUE returns only. *)
            let nl_scratch = ref 0 in
            let is_newline_at (p : int) : bool =
              if not (Int.equal mb.nltype Newline.nltype_fixed) then (
                p < mb.end_subject
                &&
                let hit =
                  Newline.is_newline mb.subject mb.nltype p mb.end_subject
                    nl_scratch utf
                in
                if hit then mb.nllen <- !nl_scratch;
                hit)
              else
                p <= mb.end_subject - mb.nllen
                && Int.equal (Char.code subject.[p]) mb.nl0
                && (Int.equal mb.nllen 1
                   || Int.equal (Char.code subject.[p + 1]) mb.nl1)
            in
            let was_newline_at (p : int) : bool =
              if not (Int.equal mb.nltype Newline.nltype_fixed) then (
                p > mb.start_subject
                &&
                let hit =
                  Newline.was_newline mb.subject mb.nltype p mb.start_subject
                    nl_scratch utf
                in
                if hit then mb.nllen <- !nl_scratch;
                hit)
              else
                p >= mb.start_subject + mb.nllen
                && Int.equal (Char.code subject.[p - mb.nllen]) mb.nl0
                && (Int.equal mb.nllen 1
                   || Int.equal (Char.code subject.[p - mb.nllen + 1]) mb.nl1)
            in
            (* pcre2_match.c:7085-7089 — pointers to the individual
               character tables: dropped, this port always reads the
               default tables through Chartables (as in Compile). *)
            (* pcre2_match.c:7091-7112 — set up the first code unit to
               match, if available. If there's no first code unit there may
               be a bitmap of possible first characters (filled in by
               PRIV(study) — M5; PCRE2_FIRSTMAPSET is never set until
               then). first_cu/first_cu2 are PCRE2_UCHAR: the C assignments
               truncate to the code-unit width (land 0xff). *)
            let has_first_cu =
              not (Int.equal (re.Compile.flags land Compile.firstset) 0)
            in
            let first_cu, first_cu2 =
              if has_first_cu then
                let fc = re.Compile.first_codeunit land 0xff (* 7097 cast *) in
                if
                  not
                    (Int.equal (re.Compile.flags land Compile.firstcaseless) 0)
                then
                  let fc2 = Chartables.fcc fc in
                  (* pcre2_match.c:7101-7107 — 8-bit library: the UCD other
                     case applies only when UCP is set without UTF. *)
                  let fc2 =
                    if fc > 127 && ucp && not utf then
                      Ucd.othercase fc land 0xff (* PCRE2_UCHAR truncation *)
                    else fc2
                  in
                  (fc, fc2)
                else (fc, fc)
              else (0, 0)
            in
            let use_start_bits =
              (not has_first_cu) && (not startline)
              && not (Int.equal (re.Compile.flags land Compile.firstmapset) 0)
            in
            (* pcre2_match.c:7114-7131 — there may also be a "last known
               required character" set. *)
            let has_req_cu =
              not (Int.equal (re.Compile.flags land Compile.lastset) 0)
            in
            let req_cu, req_cu2 =
              if has_req_cu then
                let rc_ = re.Compile.last_codeunit land 0xff (* 7119 cast *) in
                if
                  not (Int.equal (re.Compile.flags land Compile.lastcaseless) 0)
                then
                  let rc2 = Chartables.fcc rc_ in
                  (* pcre2_match.c:7123-7129 — as for first_cu2 above. *)
                  let rc2 =
                    if rc_ > 127 && ucp && not utf then
                      Ucd.othercase rc_ land 0xff (* PCRE2_UCHAR truncation *)
                    else rc2
                  in
                  (rc_, rc2)
                else (rc_, rc_)
              else (0, 0)
            in
            (* pcre2_match.c:7136-7149 — loop state for the unanchored
               bump-along attempts. FRAGMENT_RESTART (7140) is the M6
               invalid-UTF re-entry point. start_partial/match_partial are
               subject positions with -1 = NULL; the 8-bit memchr caches
               likewise. *)
            let start_partial = ref (-1) in
            let match_partial = ref (-1) in
            mb.hitend <- false (* 7144 *);
            let memchr_found_first_cu = ref (-1) in
            let memchr_found_first_cu2 = ref (-1) in
            (* pcre2_match.c:7151-7617 + 7637-7768 — the bump-along for(;;)
               loop and its ENDLOOP epilogue, as mutually tail-recursive
               functions (port-conventions §2): [bump_top] is the loop head
               (the start-of-match optimizations, 7155-7481);
               [first_cu_tail] the shared "required first code unit not
               found" break check (7300-7315, 7368-7374); [tail_opts] the
               minlength/req_cu block (7378-7481); [attempt] the bumpalong
               limit check, per-attempt resets, match() call and rc switch
               (7485-7577); [bump_bottom] the loop bottom (7579-7616);
               [endloop] the epilogue (7637-7768). Every C `break` /
               `goto ENDLOOP` with rc becomes a tail call to [endloop]. *)
            let rec bump_top (start_match : int) (req_cu_ptr : int) : int =
              (* pcre2_match.c:7155-7162 — the start-of-match optimizations
                 can be disabled at compile time. *)
              if
                not
                  (Int.equal
                     (re.Compile.overall_options land Options.no_start_optimize)
                     0)
              then (attempt [@tailcall]) start_match req_cu_ptr
              else
                (* pcre2_match.c:7164-7186 — if firstline is TRUE, the
                   start of the match is constrained to the first line of a
                   multiline string: temporarily adjust end_subject so that
                   the first-code-unit scans stop at a newline. *)
                let end_subject =
                  if firstline then (
                    let t = ref start_match in
                    if utf then
                      while !t < mb.end_subject && not (is_newline_at !t) do
                        incr t;
                        (* ACROSSCHAR(t < end_subject, t, t++) (7179) *)
                        while
                          !t < mb.end_subject
                          && Int.equal (Char.code subject.[!t] land 0xc0) 0x80
                        do
                          incr t
                        done
                      done
                    else
                      while !t < mb.end_subject && not (is_newline_at !t) do
                        incr t
                      done;
                    !t)
                  else mb.end_subject
                in
                if anchored then
                  (* pcre2_match.c:7188-7215 — anchored: check the first
                     code unit if one is recorded. This may seem pointless
                     but it can help in detecting a no match case without
                     scanning for the required code unit. *)
                  if has_first_cu || use_start_bits then
                    let ok = start_match < end_subject in
                    let ok =
                      if ok then
                        let c = Char.code subject.[start_match] in
                        let ok =
                          has_first_cu
                          && (Int.equal c first_cu || Int.equal c first_cu2)
                        in
                        if (not ok) && use_start_bits then
                          not
                            (Int.equal
                               (Char.code
                                  (Bytes.get re.Compile.start_bitmap (c lsr 3))
                               land (1 lsl (c land 7)))
                               0)
                        else ok
                      else false
                    in
                    if not ok then
                      (endloop [@tailcall]) match_nomatch start_match
                    else (tail_opts [@tailcall]) start_match req_cu_ptr
                  else (tail_opts [@tailcall]) start_match req_cu_ptr
                else if
                  (* pcre2_match.c:7217-7221 — not anchored: advance to a
                     unique first code unit if there is one. *)
                  has_first_cu
                then
                  if not (Int.equal first_cu first_cu2) then
                    (* pcre2_match.c:7223-7284 — caseless: in 8-bit mode
                       memchr() is called twice to find the earliest
                       occurrence of the code unit in either of its cases,
                       with caching of previously found positions (a huge
                       difference when only one case is present in a very
                       long subject). *)
                    let searchlength = end_subject - start_match in
                    (* pcre2_match.c:7246-7261 — if we haven't got a
                       previously found position for first_cu, or if the
                       current starting position is later, do a search; a
                       miss is cached as end_subject. If the start is
                       before a previously found position, reuse it (NULL
                       if that previous search failed). *)
                    let pp1 =
                      if
                        !memchr_found_first_cu < 0
                        || start_match > !memchr_found_first_cu
                      then (
                        let r =
                          memchr_subject subject start_match first_cu
                            searchlength
                        in
                        memchr_found_first_cu :=
                          if r < 0 then end_subject else r;
                        r)
                      else if Int.equal !memchr_found_first_cu end_subject then
                        -1
                      else !memchr_found_first_cu
                    in
                    (* pcre2_match.c:7263-7273 — the same for the other
                       case. *)
                    let pp2 =
                      if
                        !memchr_found_first_cu2 < 0
                        || start_match > !memchr_found_first_cu2
                      then (
                        let r =
                          memchr_subject subject start_match first_cu2
                            searchlength
                        in
                        memchr_found_first_cu2 :=
                          if r < 0 then end_subject else r;
                        r)
                      else if Int.equal !memchr_found_first_cu2 end_subject then
                        -1
                      else !memchr_found_first_cu2
                    in
                    (* pcre2_match.c:7275-7281 — set the start to the end
                       of the subject if neither case was found; otherwise
                       use the earlier found point. *)
                    let start_match =
                      if pp1 < 0 then if pp2 < 0 then end_subject else pp2
                      else if pp2 < 0 || pp1 < pp2 then pp1
                      else pp2
                    in
                    (first_cu_tail [@tailcall]) start_match req_cu_ptr
                  else
                    (* pcre2_match.c:7286-7298 — the caseful case is much
                       simpler. *)
                    let r =
                      memchr_subject subject start_match first_cu
                        (end_subject - start_match)
                    in
                    let start_match = if r < 0 then end_subject else r in
                    (first_cu_tail [@tailcall]) start_match req_cu_ptr
                else if startline then (
                  (* pcre2_match.c:7318-7349 — if there's no first code
                     unit, advance to just after a linebreak for a
                     multiline match if required. *)
                  let sm = ref start_match in
                  if !sm > mb.start_subject + start_offset then (
                    if utf then
                      while !sm < end_subject && not (was_newline_at !sm) do
                        incr sm;
                        (* ACROSSCHAR (7331) *)
                        while
                          !sm < end_subject
                          && Int.equal (Char.code subject.[!sm] land 0xc0) 0x80
                        do
                          incr sm
                        done
                      done
                    else
                      while !sm < end_subject && not (was_newline_at !sm) do
                        incr sm
                      done;
                    (* pcre2_match.c:7339-7347 — if we have just passed a
                       CR and the newline option is ANY or ANYCRLF, and we
                       are now at a LF, advance the match position by one
                       more code unit. *)
                    if
                      Int.equal (Char.code subject.[!sm - 1]) Newline.char_cr
                      && (Int.equal mb.nltype Newline.nltype_any
                         || Int.equal mb.nltype Newline.nltype_anycrlf)
                      && !sm < end_subject
                      && Int.equal (Char.code subject.[!sm]) Newline.char_lf
                    then incr sm);
                  (tail_opts [@tailcall]) !sm req_cu_ptr)
                else if use_start_bits then (
                  (* pcre2_match.c:7351-7375 — if there's no first code
                     unit or a requirement for a multiline line start,
                     advance to a non-unique first code unit if any have
                     been identified (the bitmap contains only 256 bits;
                     8-bit code units index it directly). *)
                  let sm = ref start_match in
                  let hit = ref false in
                  while (not !hit) && !sm < end_subject do
                    let c = Char.code subject.[!sm] in
                    if
                      not
                        (Int.equal
                           (Char.code
                              (Bytes.get re.Compile.start_bitmap (c lsr 3))
                           land (1 lsl (c land 7)))
                           0)
                    then hit := true
                    else incr sm
                  done;
                  (* pcre2_match.c:7368-7374 — see the comment in
                     [first_cu_tail]. *)
                  (first_cu_tail [@tailcall]) !sm req_cu_ptr)
                else (tail_opts [@tailcall]) start_match req_cu_ptr
            and first_cu_tail (start_match : int) (req_cu_ptr : int) : int =
              (* pcre2_match.c:7300-7315 (and 7368-7374) — if we can't find
                 the required first code unit, having reached the true end
                 of the subject, break the bumpalong loop to force a match
                 failure, except when doing partial matching (consider
                 /(?<=abc)def/ partially matching "abc"). If we have not
                 reached the true end of the subject (PCRE2_FIRSTLINE
                 temporarily modified end_subject) we also let the cycle
                 run: the matching string is legitimately allowed to start
                 with the first code unit of a newline. *)
              if Int.equal mb.partial 0 && start_match >= mb.end_subject then
                (endloop [@tailcall]) match_nomatch start_match
              else (tail_opts [@tailcall]) start_match req_cu_ptr
            and tail_opts (start_match : int) (req_cu_ptr : int) : int =
              (* pcre2_match.c:7378-7380 — restore fudged end_subject: from
                 here on every use reads mb.end_subject. *)
              (* pcre2_match.c:7382-7397 — the following two optimizations
                 must be disabled for partial matching. The minimum
                 matching length is a lower bound; no string of that length
                 (treated as code units) may actually match. *)
              if Int.equal mb.partial 0 then
                if mb.end_subject - start_match < re.Compile.minlength then
                  (endloop [@tailcall]) match_nomatch start_match
                else
                  (* pcre2_match.c:7399-7427 — if req_cu is set, that code
                     unit must appear in the subject for the (non-partial)
                     match to succeed. If the first code unit is set,
                     req_cu must be later in the subject. The search can be
                     skipped if the code unit was found later than the
                     current starting point in a previous iteration of the
                     bumpalong loop; and it is not done at all when the
                     remaining subject is very long (REQ_CU_MAX for
                     anchored patterns, REQ_CU_MAX * 1000 otherwise). *)
                  let p = start_match + if has_first_cu then 1 else 0 in
                  if has_req_cu && p > req_cu_ptr then
                    let check_length = mb.end_subject - start_match in
                    if
                      check_length < req_cu_max
                      || ((not anchored) && check_length < req_cu_max * 1000)
                    then
                      let p =
                        if not (Int.equal req_cu req_cu2) then
                          (* pcre2_match.c:7429-7446 — caseless: memchr for
                             each case in turn (only presence matters, not
                             the first occurrence). *)
                          let pp = p in
                          let r =
                            memchr_subject subject pp req_cu
                              (mb.end_subject - pp)
                          in
                          if r < 0 then
                            let r2 =
                              memchr_subject subject pp req_cu2
                                (mb.end_subject - pp)
                            in
                            if r2 < 0 then mb.end_subject else r2
                          else r
                        else
                          (* pcre2_match.c:7448-7462 — the caseful case. *)
                          let r =
                            memchr_subject subject p req_cu (mb.end_subject - p)
                          in
                          if r < 0 then mb.end_subject else r
                      in
                      if p >= mb.end_subject then
                        (* pcre2_match.c:7464-7471 — if we can't find the
                           required code unit, break the bumpalong loop,
                           forcing a match failure. *)
                        (endloop [@tailcall]) match_nomatch start_match
                      else
                        (* pcre2_match.c:7473-7478 — save the point where
                           we found it, so that we don't search again next
                           time round the loop if the start hasn't yet
                           passed this code unit. *)
                        (attempt [@tailcall]) start_match p
                    else (attempt [@tailcall]) start_match req_cu_ptr
                  else (attempt [@tailcall]) start_match req_cu_ptr
              else (attempt [@tailcall]) start_match req_cu_ptr
            and attempt (start_match : int) (req_cu_ptr : int) : int =
              (* pcre2_match.c:7485-7491 — give no match if we have passed
                 the bumpalong limit. *)
              if start_match > bumpalong_limit then
                (endloop [@tailcall]) match_nomatch start_match
              else (
                (* pcre2_match.c:7493-7497 — cb.start_match and
                   PCRE2_CALLOUT_STARTMATCH: callouts are M8. *)
                (* pcre2_match.c:7499-7508 — per-attempt mb resets.
                   fragment_options (7502) is M6; mb->skip_arg_count (7508)
                   is an M5 field. *)
                mb.start_used_ptr <- start_match;
                mb.last_used_ptr <- start_match;
                mb.moptions <- options;
                mb.match_call_count <- 0;
                mb.end_offset_top <- 0;
                (* pcre2_match.c:7514-7515 — run the match. *)
                let rc =
                  match_ ~start_eptr:start_match ~start_ecode:0
                    ~top_bracket:re.Compile.top_bracket a match_data mb
                in
                (* pcre2_match.c:7521-7525 — if "hitend" is set, remember
                   the first starting point for which a partial match was
                   found. *)
                if mb.hitend && !start_partial < 0 then (
                  start_partial := mb.start_used_ptr;
                  match_partial := start_match);
                (* pcre2_match.c:7527-7577 — switch (rc). The
                   MATCH_SKIP_ARG (7529-7539) and MATCH_SKIP (7541-7550)
                   cases are produced only by the ( *SKIP) verb arms — the
                   M5 verbs chunk, which also owns mb->verb_skip_ptr and
                   mb->ignore_skip_arg; until then those codes cannot reach
                   here (the verb arms are stubs) and would take the
                   default arm. *)
                if
                  Int.equal rc match_nomatch || Int.equal rc match_prune
                  || Int.equal rc match_then
                then (
                  (* pcre2_match.c:7552-7565 — NOMATCH and PRUNE advance by
                     one character; THEN at this level acts exactly like
                     PRUNE. (Unset ignore SKIP-with-argument: M5 field.) *)
                  let new_start_match = ref (start_match + 1) in
                  if utf then
                    (* ACROSSCHAR(new_start_match < end_subject, ...)
                       (7561-7563) *)
                    while
                      !new_start_match < mb.end_subject
                      && Int.equal
                           (Char.code subject.[!new_start_match] land 0xc0)
                           0x80
                    do
                      incr new_start_match
                    done;
                  (bump_bottom [@tailcall]) start_match !new_start_match
                    req_cu_ptr)
                else if Int.equal rc match_commit then
                  (* pcre2_match.c:7567-7571 — COMMIT disables the
                     bumpalong, but otherwise behaves as NOMATCH. *)
                  (endloop [@tailcall]) match_nomatch start_match
                else
                  (* pcre2_match.c:7573-7576 — any other return is either a
                     match, or some kind of error. *)
                  (endloop [@tailcall]) rc start_match)
            and bump_bottom (start_match : int) (new_start_match : int)
                (req_cu_ptr : int) : int =
              (* pcre2_match.c:7579-7582 — control reaches here for the
                 various types of "no match at this point" result; the C
                 resets rc to MATCH_NOMATCH, which every onward path here
                 passes explicitly. *)
              (* pcre2_match.c:7584-7588 — if PCRE2_FIRSTLINE is set, the
                 match must happen before or at the first newline in the
                 subject (though it may continue over the newline).
                 Therefore, if we have just failed to match, starting at a
                 newline, do not continue. *)
              if firstline && is_newline_at start_match then
                (endloop [@tailcall]) match_nomatch start_match
              else
                (* pcre2_match.c:7590-7592 — advance to new matching
                   position. *)
                let start_match = new_start_match in
                (* pcre2_match.c:7594-7597 — break the loop if the pattern
                   is anchored or if we have passed the end of the
                   subject. *)
                if anchored || start_match > mb.end_subject then
                  (endloop [@tailcall]) match_nomatch start_match
                else
                  (* pcre2_match.c:7599-7614 — if we have just passed a CR
                     and we are now at a LF, and the pattern does not
                     contain any explicit matches for \r or \n, and the
                     newline option is CRLF or ANY or ANYCRLF, advance the
                     match position by one more code unit. In normal
                     matching start_match will always be greater than the
                     first position at this stage, but a failed *SKIP can
                     cause a return at the same point, which is why the
                     first test exists. *)
                  let start_match =
                    if
                      start_match > mb.start_subject + start_offset
                      && Int.equal
                           (Char.code subject.[start_match - 1])
                           Newline.char_cr
                      && start_match < mb.end_subject
                      && Int.equal
                           (Char.code subject.[start_match])
                           Newline.char_lf
                      && Int.equal (re.Compile.flags land Compile.hascrorlf) 0
                      && (Int.equal mb.nltype Newline.nltype_any
                         || Int.equal mb.nltype Newline.nltype_anycrlf
                         || Int.equal mb.nllen 2)
                    then start_match + 1
                    else start_match
                  in
                  (* pcre2_match.c:7616 — reset for start of next match
                     attempt. *)
                  mb.mark <- Frames.unset;
                  (bump_top [@tailcall]) start_match req_cu_ptr
            and endloop (rc : int) (start_match : int) : int =
              (* pcre2_match.c:7637-7701 — ENDLOOP. The invalid-UTF
                 fragment carry-on (7646-7701) is M6: until then
                 end_subject == true_end_subject always holds here. *)
              (* pcre2_match.c:7703-7707 — fill in fields that are always
                 returned in the match data (code and matchedby have no
                 meaning at this seam). *)
              match_data.mark <- mb.mark;
              if Int.equal rc match_match then (
                (* pcre2_match.c:7709-7735 — handle a fully successful
                   match: the return code is the number of captured
                   strings, or 0 if there were too many to fit into the
                   ovector. PCRE2_COPY_MATCHED_SUBJECT (7723-7732) is a
                   no-op at this seam (GC strings; subject_length is
                   dropped with the subject pointer). *)
                match_data.rc <-
                  (if mb.end_offset_top >= 2 * match_data.oveccount then 0
                   else (mb.end_offset_top / 2) + 1);
                match_data.startchar <- start_match (* 7719 *);
                match_data.leftchar <- mb.start_used_ptr (* 7720 *);
                match_data.rightchar <-
                  (if mb.last_used_ptr > mb.end_match_ptr then mb.last_used_ptr
                   else mb.end_match_ptr)
                (* 7721-7722 *);
                match_data.rc)
              else (
                (* pcre2_match.c:7737-7741 — a partial match, an error, or
                   failure at all permitted starting positions: any mark
                   data is in the nomatch_mark field. *)
                match_data.mark <- mb.nomatch_mark;
                if
                  (not (Int.equal rc match_nomatch))
                  && not (Int.equal rc Errors.error_partial)
                then (
                  (* pcre2_match.c:7743-7745 — for anything other than
                     nomatch or partial match, just return the code. *)
                  match_data.rc <- rc;
                  match_data.rc)
                else if !match_partial >= 0 then (
                  (* pcre2_match.c:7747-7762 — handle a partial match. If a
                     "soft" partial match was requested, searching for a
                     complete match will have continued, and rc here is
                     MATCH_NOMATCH; for a "hard" one it is already
                     PCRE2_ERROR_PARTIAL. *)
                  match_data.ovector.(0) <- !match_partial;
                  match_data.ovector.(1) <- mb.end_subject;
                  match_data.startchar <- !match_partial;
                  match_data.leftchar <- !start_partial;
                  match_data.rightchar <- mb.end_subject;
                  match_data.rc <- Errors.error_partial;
                  match_data.rc)
                else (
                  (* pcre2_match.c:7764-7766 — else this is the classic
                     nomatch case. *)
                  match_data.rc <- Errors.error_nomatch;
                  match_data.rc))
            in
            (* pcre2_match.c:6601-6602 + 7151 — enter the bumpalong loop:
               start_match = subject + start_offset, req_cu_ptr one before
               it. *)
            (bump_top [@tailcall]) start_offset (start_offset - 1))

(* ---------- Test-only single-attempt entry ---------- *)

(* TEST-ONLY: run ONE match attempt of [code] against [subject] starting
   at [start], returning [match_]'s raw (rc, ovector) — MATCH_MATCH (1) /
   MATCH_NOMATCH (0) / negative codes, NOT the driver's pair-count
   protocol. The real driver is [pcre2_match] above; this single-attempt
   entry is kept because the module-initialization asserts below pin
   [match_]'s per-attempt semantics against the C oracle at exact
   positions (DEVIATION from the chunk plan, which wanted this to delegate
   to the driver: the driver's bump-along loop and its rc mapping would
   change every hand-derived expected value — anchored-attempt NOMATCHes
   become bumped-along searches, soft partials become -2 — so the asserts'
   entry keeps calling [match_] directly). The ONLY argument validation is
   the BADOFFSET check on [start] (pcre2_match.c:6610 — it establishes the
   0 <= start_eptr <= end_subject invariant [match_] relies on); there is
   NO bump-along loop, NO anchored/startline/first-cu start optimization
   and NO NOTEMPTY retry here — callers position [start] themselves.

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
       driver-written fields (rc/startchar/leftchar/rightchar/mark) are
       dead here: this entry returns [match_]'s raw rc, not the driver
       protocol. *)
    {
      ovector = Array.make (2 * oveccount) Frames.unset;
      oveccount;
      rc = 0;
      startchar = 0;
      leftchar = 0;
      rightchar = 0;
      mark = Frames.unset;
    }
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
  (* Class / typed-repeat / anchor opcodes now guarding live behavior
     (classes + typed repeats and anchors chunks). *)
  assert (Int.equal Opcodes.op_sod 1);
  assert (Int.equal Opcodes.op_som 2);
  assert (Int.equal Opcodes.op_set_som 3);
  assert (Int.equal Opcodes.op_not_word_boundary 4);
  assert (Int.equal Opcodes.op_word_boundary 5);
  assert (Int.equal Opcodes.op_not_digit 6);
  assert (Int.equal Opcodes.op_digit 7);
  assert (Int.equal Opcodes.op_not_whitespace 8);
  assert (Int.equal Opcodes.op_whitespace 9);
  assert (Int.equal Opcodes.op_not_wordchar 10);
  assert (Int.equal Opcodes.op_wordchar 11);
  assert (Int.equal Opcodes.op_any 12);
  assert (Int.equal Opcodes.op_allany 13);
  assert (Int.equal Opcodes.op_anybyte 14);
  assert (Int.equal Opcodes.op_notprop 15);
  assert (Int.equal Opcodes.op_prop 16);
  assert (Int.equal Opcodes.op_anynl 17);
  assert (Int.equal Opcodes.op_not_hspace 18);
  assert (Int.equal Opcodes.op_hspace 19);
  assert (Int.equal Opcodes.op_not_vspace 20);
  assert (Int.equal Opcodes.op_vspace 21);
  assert (Int.equal Opcodes.op_extuni 22);
  assert (Int.equal Opcodes.op_eodn 23);
  assert (Int.equal Opcodes.op_eod 24);
  assert (Int.equal Opcodes.op_doll 25);
  assert (Int.equal Opcodes.op_dollm 26);
  assert (Int.equal Opcodes.op_circ 27);
  assert (Int.equal Opcodes.op_circm 28);
  assert (Int.equal Opcodes.op_typestar 85);
  assert (Int.equal Opcodes.op_typeminstar 86);
  assert (Int.equal Opcodes.op_typeplus 87);
  assert (Int.equal Opcodes.op_typeminplus 88);
  assert (Int.equal Opcodes.op_typequery 89);
  assert (Int.equal Opcodes.op_typeminquery 90);
  assert (Int.equal Opcodes.op_typeupto 91);
  assert (Int.equal Opcodes.op_typeminupto 92);
  assert (Int.equal Opcodes.op_typeexact 93);
  assert (Int.equal Opcodes.op_typeposstar 94);
  assert (Int.equal Opcodes.op_typeposplus 95);
  assert (Int.equal Opcodes.op_typeposquery 96);
  assert (Int.equal Opcodes.op_typeposupto 97);
  assert (Int.equal Opcodes.op_crminquery 103);
  assert (Int.equal Opcodes.op_crrange 104);
  assert (Int.equal Opcodes.op_crminrange 105);
  assert (Int.equal Opcodes.op_crposstar 106);
  assert (Int.equal Opcodes.op_crposquery 108);
  assert (Int.equal Opcodes.op_class 110);
  assert (Int.equal Opcodes.op_nclass 111);
  (* Backreference opcodes now guarding live behavior (backreferences
     chunk). *)
  assert (Int.equal Opcodes.op_ref 113);
  assert (Int.equal Opcodes.op_refi 114);
  assert (Int.equal Opcodes.op_dnref 115);
  assert (Int.equal Opcodes.op_dnrefi 116);
  assert (Int.equal Opcodes.op_not_ucp_word_boundary 169);
  assert (Int.equal Opcodes.op_ucp_word_boundary 170);
  (* Bracket / alternation / ket opcodes now guarding live behavior
     (brackets chunk), including the *bracode switch literals inside the
     OP_KET arm. *)
  assert (Int.equal Opcodes.op_alt 120);
  assert (Int.equal Opcodes.op_ket 121);
  assert (Int.equal Opcodes.op_ketrmax 122);
  assert (Int.equal Opcodes.op_ketrmin 123);
  assert (Int.equal Opcodes.op_ketrpos 124);
  assert (Int.equal Opcodes.op_assert 127);
  assert (Int.equal Opcodes.op_assert_not 128);
  assert (Int.equal Opcodes.op_assertback 129);
  assert (Int.equal Opcodes.op_assertback_not 130);
  assert (Int.equal Opcodes.op_assert_na 131);
  assert (Int.equal Opcodes.op_assertback_na 132);
  assert (Int.equal Opcodes.op_once 133);
  assert (Int.equal Opcodes.op_script_run 134);
  assert (Int.equal Opcodes.op_bra 135);
  assert (Int.equal Opcodes.op_brapos 136);
  assert (Int.equal Opcodes.op_cbra 137);
  assert (Int.equal Opcodes.op_cbrapos 138);
  assert (Int.equal Opcodes.op_cond 139);
  assert (Int.equal Opcodes.op_sbra 140);
  assert (Int.equal Opcodes.op_sbrapos 141);
  assert (Int.equal Opcodes.op_scbra 142);
  assert (Int.equal Opcodes.op_scbrapos 143);
  assert (Int.equal Opcodes.op_scond 144);
  assert (Int.equal Opcodes.op_brazero 151);
  assert (Int.equal Opcodes.op_braminzero 152);
  assert (Int.equal Opcodes.op_skipzero 167);
  (* OP_lengths entries the bracket arms step by (pcre2_tables.c OP_lengths
     via Opcodes.op_lengths): 1+LINK_SIZE for BRA/ALT-class items,
     1+LINK_SIZE+IMM2_SIZE for the capturing brackets. *)
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_bra) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_alt) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_sbra) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_once) (1 + Limits.link_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_script_run) (1 + Limits.link_size));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_cbra)
      (1 + Limits.link_size + Limits.imm2_size));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_scbra)
      (1 + Limits.link_size + Limits.imm2_size));
  (* Newline character constants consumed by the ANY/ANYNL/EODN arms
     (pcre2_internal.h:678-699). *)
  assert (Int.equal Newline.char_lf 0x0a);
  assert (Int.equal Newline.char_vt 0x0b);
  assert (Int.equal Newline.char_ff 0x0c);
  assert (Int.equal Newline.char_cr 0x0d);
  assert (Int.equal Newline.char_nel 0x85);
  (* hspace_byte/vspace_byte agree with HSPACE_LIST/VSPACE_LIST
     (pcre2_tables.c:66-67 via Tables) restricted to code units < 256. *)
  for c = 0 to 255 do
    assert (
      Bool.equal (hspace_byte c)
        (Array.exists (fun v -> Int.equal v c) Tables.hspace_list));
    assert (
      Bool.equal (vspace_byte c)
        (Array.exists (fun v -> Int.equal v c) Tables.vspace_list))
  done;
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

(* Hand-assembled programs: these predate the brackets chunk and exercise
   OP_CHAR / OP_END directly, without the OP_BRA..OP_KET wrapper the
   compiler always emits (pcre2_compile.c:10570-10604) — still valid
   programs, kept as the minimal drivers of those arms. *)
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
   scaffolding marker (this last check is removed with the stub —
   OP_ASSERT belongs to the M4 lookaround chunk). *)
let () =
  (match
     match_internal ~code:(mk_code [ Opcodes.op_define ]) ~top_bracket:0 "a" 0
   with
  | rc, _ -> assert (Int.equal rc Errors.error_internal));
  match
    match_internal
      ~code:(mk_code [ Opcodes.op_assert; 0; 3; Opcodes.op_end ])
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
          {
            ovector = Array.make 2 Frames.unset;
            oveccount = 1;
            rc = 0;
            startchar = 0;
            leftchar = 0;
            rightchar = 0;
            mark = Frames.unset;
          }
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

(* TEST-ONLY: real compiled programs for the repeat arms, predating the
   brackets chunk. The compiler always wraps the pattern in OP_BRA ..
   OP_KET (pcre2_compile.c:10570-10604); these asserts strip the wrapper —
   asserting its exact shape — and terminate the body with OP_END, so
   they exercise the repeat arms without any bracket opcode in the
   program. Returns (body code, top_bracket). *)
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

(* Bit-mapped classes (OP_CLASS/OP_NCLASS) over compiled bodies: the CR*
   repeat-info decode, all three match strategies (exact/min-only,
   minimize RM23, maximize RM24), the possessive forms, and the
   SCHECK_PARTIAL sites inside the loops. Anchored attempts at fixed
   offsets, as above. *)
let () =
  let run ?moptions pat subj start =
    let code, top_bracket = compile_body pat in
    match_internal ?moptions ~code ~top_bracket subj start
  in
  let expect_match ?moptions pat subj start e =
    match run ?moptions pat subj start with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal ov.(0) start);
        assert (Int.equal ov.(1) e)
  in
  let expect_nomatch ?moptions pat subj start =
    match run ?moptions pat subj start with
    | rc, _ -> assert (Int.equal rc match_nomatch)
  in
  let expect_hard_partial pat subj start =
    match run ~moptions:Options.partial_hard pat subj start with
    | rc, _ -> assert (Int.equal rc Errors.error_partial)
  in
  (* OP_CRPLUS greedy: min 1 then scan, backtrack in the RM24 loop. *)
  expect_match "[a-c]+d" "abcd" 0 4;
  expect_nomatch "[a-c]+d" "d" 0;
  (* No repeat info: Lmin = Lmax = 1. *)
  expect_match "[a-c]x" "bx" 0 2;
  expect_nomatch "[a-c]x" "dx" 0;
  (* OP_CRRANGE greedy: {2,4}. *)
  expect_match "[a-c]{2,4}d" "abd" 0 3;
  expect_match "[a-c]{2,4}d" "abcad" 0 5;
  expect_nomatch "[a-c]{2,4}d" "ad" 0;
  expect_match "[a-c]{2,4}d" "aabd" 0 4 (* RM24 back-off *);
  expect_nomatch "[a-c]{2,4}d" "abcabd" 0 (* RM24 descends past Lstart *);
  (* OP_CRMINRANGE (RM23) incl. the Lmin++ >= Lmax refusal. *)
  expect_match "[a-c]{2,4}?d" "abcd" 0 4;
  expect_nomatch "[a-c]{2,4}?d" "abcabd" 0;
  (* OP_CRSTAR / OP_CRMINSTAR / OP_CRQUERY. *)
  expect_match "[a-c]*d" "d" 0 1;
  expect_match "[a-c]*?d" "abcd" 0 4;
  expect_match "[a-c]?d" "ad" 0 2;
  expect_match "[a-c]?d" "d" 0 1;
  (* OP_CRPOSSTAR / OP_CRPOSRANGE: no backing up. *)
  expect_match "[a-c]*+d" "abcd" 0 4;
  expect_nomatch "[a-c]*+cd" "abcd" 0;
  expect_match "[a-c]{2,4}+d" "abcd" 0 4;
  expect_nomatch "[a-c]{2,4}+cd" "abcd" 0;
  (* OP_NCLASS (multi-character negated class). *)
  expect_match "[^ab]+b" "xyzb" 0 4;
  expect_nomatch "[^ab]+b" "ab" 0;
  expect_match "[^ab]{2}" "xy" 0 2;
  (* One-char negated class repeats ride OP_NOT (chars chunk); keep them
     covered from this chunk's dispatch list. *)
  expect_match "[^a]*a" "xxa" 0 3;
  expect_match "[^a]*?a" "xxa" 0 3;
  (* SCHECK_PARTIAL: the min loop, the greedy scan, and the RM23 resume. *)
  expect_hard_partial "[a-c]{3}" "ab" 0;
  expect_hard_partial "[a-c]*d" "abc" 0;
  expect_hard_partial "[a-c]+?d" "ab" 0

(* Character types: OP_ANY/OP_ALLANY singles and the REPEATTYPE machinery
   (min loops, minimize RM33, maximize RM34, possessive), plus the \d \w
   \s \h \v \R single-match arms. *)
let () =
  let run ?moptions ?poptions pat subj start =
    let code, top_bracket = compile_body pat in
    match_internal ?moptions ?poptions ~code ~top_bracket subj start
  in
  let expect_match ?moptions ?poptions pat subj start e =
    match run ?moptions ?poptions pat subj start with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal ov.(0) start);
        assert (Int.equal ov.(1) e)
  in
  let expect_nomatch ?moptions ?poptions pat subj start =
    match run ?moptions ?poptions pat subj start with
    | rc, _ -> assert (Int.equal rc match_nomatch)
  in
  let expect_hard_partial pat subj start =
    match run ~moptions:Options.partial_hard pat subj start with
    | rc, _ -> assert (Int.equal rc Errors.error_partial)
  in
  (* OP_ANY refuses the (fixed-LF) newline; OP_ALLANY takes it. *)
  expect_match ".b" "ab" 0 2;
  expect_nomatch ".b" "\nb" 0;
  expect_match "(?s).b" "\nb" 0 2;
  (* Typed repeat strategies over \d: TYPEEXACT + TYPESTAR/TYPEUPTO. *)
  expect_match "\\d{2,}x" "12x" 0 3;
  expect_match "\\d{2,}x" "12345x" 0 6;
  expect_nomatch "\\d{2,}x" "1x" 0;
  expect_match "\\d{2,4}y" "123y" 0 4 (* TYPEUPTO scan + RM34 back-off *);
  expect_match "\\d*0" "100" 0 3 (* RM34 greedy back-off *);
  expect_match "\\d*?x" "12x" 0 3 (* TYPEMINSTAR, RM33 *);
  expect_nomatch "\\d{2,3}?x" "1234x" 0 (* RM33 Lmin++ >= Lmax refusal *);
  expect_match "\\d*+x" "12x" 0 3 (* TYPEPOSSTAR *);
  expect_nomatch "\\d*+1" "11" 0;
  (* OP_ANY typed repeats: the min-loop newline stop and the maximize
     newline stop. *)
  expect_match ".*b" "aaab" 0 4;
  expect_nomatch ".+b" "\nab" 0;
  expect_match ".*" "ab\ncd" 0 2;
  expect_match "(?s).*" "ab\ncd" 0 5 (* ALLANY maximize: take all *);
  expect_match "(?s).{2}x" "\n\nx" 0 3 (* ALLANY min: Feptr += Lmin *);
  (* The ALLANY min phase is ONE up-front bound check: with nothing yet
     consumed (Feptr = start_used_ptr) SCHECK_PARTIAL does not fire and
     the result is a plain NOMATCH even under hard partial — oracle
     confirmed. After a consumed character it is a hard partial. *)
  (match run ~moptions:Options.partial_hard "(?s).{3}" "ab" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  expect_hard_partial "a(?s).{2}" "ab" 0;
  expect_hard_partial ".*" "ab" 0;
  (* Single-match type arms. *)
  expect_match "\\w" "_" 0 1;
  expect_nomatch "\\W" "_" 0;
  expect_match "\\W" "-" 0 1;
  expect_match "\\s" " " 0 1;
  expect_nomatch "\\S" " " 0;
  expect_match "\\S" "x" 0 1;
  expect_match "\\d" "5" 0 1;
  expect_match "\\D" "a" 0 1;
  expect_nomatch "\\D" "5" 0;
  expect_match "\\h" "\t" 0 1;
  expect_match "\\h" "\xa0" 0 1 (* NBSP byte *);
  expect_nomatch "\\h" "\n" 0;
  expect_match "\\H" "\n" 0 1;
  expect_nomatch "\\H" " " 0;
  expect_match "\\v" "\n" 0 1;
  expect_match "\\v" "\x85" 0 1 (* NEL byte *);
  expect_nomatch "\\v" "\t" 0;
  expect_match "\\V" "\t" 0 1;
  expect_nomatch "\\V" "\x0c" 0;
  (* Typed h/v/s/w repeats: min loops, maximize scans, minimize. *)
  expect_match "\\h{2}x" " \tx" 0 3;
  expect_match "\\h*x" "  x" 0 3;
  expect_match "\\v+x" "\n\x0bx" 0 3;
  expect_match "\\H*?x" "abx" 0 3;
  expect_match "\\V{2}" "ab" 0 2;
  expect_match "\\s*x" " \tx" 0 3;
  expect_match "\\w+?-" "ab-" 0 3;
  expect_hard_partial "\\d{3}" "12" 0;
  expect_hard_partial "\\d+" "12" 0;
  (* \R (OP_ANYNL): BSR_UNICODE convention in match_internal. *)
  expect_match "\\R" "\r\n" 0 2;
  expect_match "\\R" "\rx" 0 1;
  expect_match "\\R" "\x0b" 0 1;
  expect_match "\\R" "\x85" 0 1;
  expect_nomatch "\\R" "a" 0;
  expect_hard_partial "\\R" "\r" 0 (* SCHECK_PARTIAL in the CR arm *);
  (* \R typed repeats: the min-loop CRLF absorb, the maximize scan, the
     lazy RM33 CR sub-case, and the RM34 CRLF double step (without it the
     greedy back-off would land between CR and LF and match at 2). *)
  expect_match "\\R{2}x" "\r\n\nx" 0 4;
  expect_match "\\R+" "\n\r\n\x0b" 0 4;
  expect_match "\\R*?z" "\r\nz" 0 3;
  expect_match "\\R{0,9}\\n" "\n\r\n" 0 1

(* Anchors and simple assertions: OP_CIRC/OP_CIRCM, OP_DOLL/OP_DOLLM,
   OP_SOD, OP_SOM, OP_SET_SOM, OP_EOD, OP_EODN, and the \b/\B word
   boundaries. *)
let () =
  let run ?moptions ?poptions ?start_offset pat subj start =
    let code, top_bracket = compile_body pat in
    match_internal ?moptions ?poptions ?start_offset ~code ~top_bracket subj
      start
  in
  let expect_match ?moptions ?poptions ?start_offset pat subj start e =
    match run ?moptions ?poptions ?start_offset pat subj start with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal ov.(0) start);
        assert (Int.equal ov.(1) e)
  in
  let expect_nomatch ?moptions ?poptions ?start_offset pat subj start =
    match run ?moptions ?poptions ?start_offset pat subj start with
    | rc, _ -> assert (Int.equal rc match_nomatch)
  in
  let expect_hard_partial pat subj start =
    match run ~moptions:Options.partial_hard pat subj start with
    | rc, _ -> assert (Int.equal rc Errors.error_partial)
  in
  (* OP_CIRC: start of subject only, killed by NOTBOL. *)
  expect_match "^a" "a" 0 1;
  expect_nomatch ~moptions:Options.notbol "^a" "a" 0;
  expect_nomatch "^a" "aa" 1;
  (* OP_CIRCM: after any newline; NOTBOL only kills the subject start;
     not after a final newline unless ALT_CIRCUMFLEX. *)
  expect_match "(?m)^b" "a\nb" 2 3;
  expect_nomatch "(?m)^b" "ab" 1;
  expect_nomatch ~moptions:Options.notbol "(?m)^a" "a" 0;
  expect_match ~moptions:Options.notbol "(?m)^b" "a\nb" 2 3;
  expect_nomatch "(?m)^" "a\n" 2;
  expect_match ~poptions:Options.alt_circumflex "(?m)^" "a\n" 2 2;
  (* OP_DOLL -> ASSERT_NL_OR_EOS: end of subject or the newline that ends
     it; NOTEOL kills it; DOLLAR_ENDONLY reroutes to the OP_EOD body. *)
  expect_match "a$" "a" 0 1;
  expect_nomatch ~moptions:Options.noteol "a$" "a" 0;
  expect_match "a$" "a\n" 0 1;
  expect_nomatch "a$" "a\nb" 0;
  expect_nomatch ~poptions:Options.dollar_endonly "a$" "a\n" 0;
  expect_match ~poptions:Options.dollar_endonly "a$" "a" 0 1;
  (* OP_DOLLM: before any newline, or at the end unless NOTEOL; the end
     position hits SCHECK_PARTIAL. *)
  expect_match "(?m)a$" "a\nb" 0 1;
  expect_nomatch "(?m)a$" "ab" 0;
  expect_match "(?m)a$" "a" 0 1;
  expect_nomatch ~moptions:Options.noteol "(?m)a$" "a" 0;
  expect_hard_partial "(?m)a$" "a" 0;
  (* OP_SOD (\A). *)
  expect_match "\\Aa" "aa" 0 1;
  expect_nomatch "\\Aa" "aa" 1;
  (* OP_SOM (\G): subject + start_offset. *)
  expect_match "\\Ga" "aa" 1 2;
  expect_nomatch ~start_offset:0 "\\Ga" "aa" 1;
  (* OP_SET_SOM (\K): resets ovector.(0). *)
  (match run "a\\Kb" "ab" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 1);
      assert (Int.equal ov.(1) 2));
  (* OP_EOD (\z): true end only; hard partial at the end. *)
  expect_match "a\\z" "a" 0 1;
  expect_nomatch "a\\z" "a\n" 0;
  expect_hard_partial "a\\z" "a" 0;
  (* OP_EODN (\Z): end or the final newline. *)
  expect_match "a\\Z" "a" 0 1;
  expect_match "a\\Z" "a\n" 0 1;
  expect_nomatch "a\\Z" "a\nb" 0;
  expect_hard_partial "a\\Z" "a" 0;
  (* Word boundaries at start / middle / end; \B; the end-of-subject peek
     hits SCHECK_PARTIAL. *)
  expect_match "\\bword\\b" "word" 0 4;
  expect_match "\\bword\\b" "a word z" 2 6;
  expect_nomatch "\\bword\\b" "words" 0;
  expect_nomatch "\\bord" "word" 1;
  expect_match "\\Bord" "word" 1 4;
  expect_nomatch "\\Bword" "word" 0;
  expect_match "x\\b" "x" 0 1;
  expect_hard_partial "x\\b" "x" 0

(* OP_ANYBYTE: unreachable from the non-UTF compiler (\C compiles to
   OP_ALLANY, pcre2_compile.c:8174-8176), exercised hand-assembled. *)
let () =
  let anyb = mk_code [ Opcodes.op_anybyte; Opcodes.op_end ] in
  (match match_internal ~code:anyb ~top_bracket:0 "\n" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 1));
  match match_internal ~code:anyb ~top_bracket:0 "" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch)

(* TEST-ONLY (brackets chunk): whole compiled programs — the OP_BRA ..
   OP_KET wrapper and every inner bracket now run for real. Returns
   (code, top_bracket). *)
let compile_whole (pattern : string) : Bytes.t * int =
  match Compile.pcre2_compile pattern ~options:0 with
  | Error _ -> assert false
  | Ok re -> (re.Compile.code, re.Compile.top_bracket)

(* The backtracking gauntlet: brackets, alternation, kets and the
   zero-repeat wrappers over whole compiled programs. EVERY expected
   ovector below is pinned against the C oracle (pcre2test_ml with the
   allcaptures/allaftertext modifiers); anchored attempts at offset 0
   (match_internal has no bump-along loop). *)
let () =
  let run ?moptions ?oveccount pat subj =
    let code, top_bracket = compile_whole pat in
    let oveccount =
      match oveccount with Some c -> c | None -> top_bracket + 1
    in
    match_internal ?moptions ~oveccount ~code ~top_bracket subj 0
  in
  (* Every pair of the (top_bracket + 1)-pair ovector must equal the
     oracle's, including unset (-1, -1) pairs. *)
  let expect_ov pat subj expected =
    match run pat subj with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal (Array.length ov) (Array.length expected));
        Array.iteri (fun i e -> assert (Int.equal ov.(i) e)) expected
  in
  let expect_nomatch pat subj =
    match run pat subj with rc, _ -> assert (Int.equal rc match_nomatch)
  in
  (* Alternative walk + capture — oracle: 0: bc, 1: b. *)
  expect_ov "(a|b)c" "bc" [| 0; 2; 0; 1 |];
  (* Backtrack across a group boundary — oracle: 0: aaab, 1: aa, 2: a
     (the first greedy a+ backs off from 3 to 2; the capture is rewritten
     at each ket pass). *)
  expect_ov "(a+)(a+)b" "aaab" [| 0; 4; 0; 2; 2; 3 |];
  (* Empty-loop protection at KETRMAX — oracle: 0: aaab, 1: "" before b
     (i.e. (3,3)): the second iteration matches empty at 3, its ket
     writes the capture, and the Feptr == P->eptr test then carries on at
     this level instead of looping forever. *)
  expect_ov "(a*)*b" "aaab" [| 0; 4; 3; 3 |];
  (* Same protection through a bounded inner repeat — oracle: 1: ""
     before b. *)
  expect_ov "(a{0,2})*b" "aaab" [| 0; 4; 3; 3 |];
  (* Leftmost-first alternation semantics — oracle: 1: a, 2: bcd. *)
  expect_ov "(a|ab)(c|bcd)" "abcd" [| 0; 4; 0; 1; 1; 4 |];
  (* Unset optional group — oracle: 0: y, 1: <unset>. *)
  expect_ov "(x)?y" "y" [| 0; 1; -1; -1 |];
  (* Bounded group repeat — oracle: 0: aa, 1: a with nothing after
     (the last taken iteration, (1,2)). *)
  expect_ov "(a){0,3}" "aa" [| 0; 2; 1; 2 |];
  (* Zero iterations of the same — oracle: empty match, 1: <unset>. *)
  expect_ov "(a){0,3}" "b" [| 0; 0; -1; -1 |];
  (* Per-iteration capture semantics — oracle: 0: ab, 1: b, 2: a: the
     second iteration overwrites group 1, while group 2 survives from the
     first iteration (frame ovector copies carry it forward). *)
  expect_ov "((a)|b)+" "ab" [| 0; 2; 1; 2; 0; 1 |];
  (* Nested alternated captures — oracle: 1: a, 2: b. *)
  expect_ov "(?:(a)|(b))+" "ab" [| 0; 2; 0; 1; 1; 2 |];
  (* KETRMIN lazy group repeat with a capture — oracle: 0: aab, 1: a
     with only b after (i.e. (1,2)). *)
  expect_ov "(a+?)+?b" "aab" [| 0; 3; 1; 2 |];
  (* KETRMIN empty-match protection — oracle: 0: b, 1: "" at 0: the lazy
     group matches empty once and the Feptr == P->eptr test carries on at
     this level. *)
  expect_ov "(a*?)+?b" "b" [| 0; 1; 0; 0 |];
  (* BRAZERO: with and without the group — oracle: 0: d / 0: abcd. *)
  expect_ov "(?:abc)?d" "d" [| 0; 1 |];
  expect_ov "(?:abc)?d" "abcd" [| 0; 4 |];
  (* BRAMINZERO: the skip-the-group try succeeds on "d"; on "abcd" the
     RM10 resume steps into the group — oracle: 0: d / 0: abcd. *)
  expect_ov "(?:abc)??d" "d" [| 0; 1 |];
  expect_ov "(?:abc)??d" "abcd" [| 0; 4 |];
  (* SKIPZERO: a {0} group is skipped entirely — oracle: 0: b,
     1: <unset>. *)
  expect_ov "(a){0}b" "b" [| 0; 1; -1; -1 |];
  (* KETRMIN re-iteration (RM6: back to the bracket in the same frame) —
     oracle: 0: ababc. *)
  expect_ov "(?:ab)+?c" "ababc" [| 0; 5 |];
  (* KETRMAX releasing an iteration (RM7 NOMATCH resume): three
     iterations fail the tail, two match — oracle: 0: aaaaab. *)
  expect_ov "(?:aa)+ab" "aaaaab" [| 0; 6 |];
  (* Inner non-capturing alternation at depth > 0: the OP_BRA optimized
     branch walk (RM1) twice, then the final branch running in place —
     oracle: 0: xcd. *)
  expect_ov "x(?:a|b|c)d" "xcd" [| 0; 3 |];
  (* GROUPLOOP: no alternative left fails the group
     (pcre2_match.c:5410). *)
  expect_nomatch "(?:a|b)" "c";
  expect_nomatch "(a|b)c" "bd";
  (* Top-level alternation with captures on distinct branches — oracle:
     1: <unset>, 2: b: a dynamic "gap" below offset_top stays unset. *)
  expect_ov "(a)|(b)" "b" [| 0; 1; -1; -1; 0; 1 |];
  (* Nested captures — oracle: 0: abc, 1: ab, 2: a. *)
  expect_ov "((a)b)c" "abc" [| 0; 3; 0; 2; 0; 1 |];
  (* Adjacent captures — oracle: 1: a, 2: b. *)
  expect_ov "(a)(b)" "ab" [| 0; 2; 0; 1; 1; 2 |];
  (* Oveccount clipping with real captures (pcre2_match.c:934-939):
     oveccount 2 keeps group 1 and clips group 2. *)
  match run ~oveccount:2 "(a)(b)" "ab" with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal (Array.length ov) 4);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 2);
      assert (Int.equal ov.(2) 0);
      assert (Int.equal ov.(3) 1)

(* Driver checks (pcre2_match, this chunk): the bump-along protocol, rc
   conventions, option/offset validation, partial bookkeeping and the
   start-of-match optimizations over whole compiled patterns. Expected
   values pinned against the C oracle (pcre2test on the real 10.44
   library). *)
let () =
  let compile pat =
    match Compile.pcre2_compile pat ~options:0 with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run ?(options = 0) ?oveccount (re : Compile.re) subj start =
    let oveccount =
      match oveccount with Some c -> c | None -> re.Compile.top_bracket + 1
    in
    let m =
      {
        ovector = Array.make (2 * oveccount) Frames.unset;
        oveccount;
        rc = 0;
        startchar = 0;
        leftchar = 0;
        rightchar = 0;
        mark = Frames.unset;
      }
    in
    let rc = pcre2_match re ~subject:subj ~start_offset:start ~options m in
    (rc, m)
  in
  let abc = compile "abc" in
  (* Bump-along success: /abc/ finds the match away from the start; rc is
     the pair count (1), startchar the attempt start (pcre2_match.c:7716-
     7719). first_cu 'a' is set, so this also runs the caseful memchr
     advance (7286-7298). *)
  (match run abc "xxabcy" 0 with
  | rc, m ->
      assert (Int.equal rc 1);
      assert (Int.equal m.ovector.(0) 2);
      assert (Int.equal m.ovector.(1) 5);
      assert (Int.equal m.startchar 2);
      assert (Int.equal m.leftchar 2);
      assert (Int.equal m.rightchar 5));
  (* Classic nomatch (7764-7766). *)
  (match run abc "abd" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
  (* Undefined match option bits -> -34 (6595-6597); PCRE2_SUBSTITUTE_GLOBAL
     is not a public match option. *)
  (match run ~options:0x00000100 abc "abc" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_badoption));
  (* Partial + ENDANCHORED -> -34 (6661-6666). *)
  (match
     run ~options:(Options.partial_hard lor Options.endanchored) abc "abc" 0
   with
  | rc, _ -> assert (Int.equal rc Errors.error_badoption));
  (* BADOFFSET, both unsigned shapes (6610). *)
  (match run abc "abc" 4 with
  | rc, _ -> assert (Int.equal rc Errors.error_badoffset));
  (match run abc "abc" (-1) with
  | rc, _ -> assert (Int.equal rc Errors.error_badoffset));
  (* PCRE2_ANCHORED as a match option: one attempt only (7594-7597). *)
  (match run ~options:Options.anchored abc "xabc" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
  (match run ~options:Options.anchored abc "abcx" 0 with
  | rc, m ->
      assert (Int.equal rc 1);
      assert (Int.equal m.ovector.(1) 3));
  (* Soft partial: the bump-along continues after hitend; ENDLOOP promotes
     the remembered first partial position (7521-7525, 7747-7762): oracle
     "Partial match: ab" at 1..3. *)
  (match run ~options:Options.partial_soft abc "xab" 0 with
  | rc, m ->
      assert (Int.equal rc Errors.error_partial);
      assert (Int.equal m.ovector.(0) 1);
      assert (Int.equal m.ovector.(1) 3);
      assert (Int.equal m.startchar 1);
      assert (Int.equal m.leftchar 1);
      assert (Int.equal m.rightchar 3));
  (* Hard partial: match_ returns PCRE2_ERROR_PARTIAL immediately; same
     result surface. *)
  (match run ~options:Options.partial_hard abc "xab" 0 with
  | rc, m ->
      assert (Int.equal rc Errors.error_partial);
      assert (Int.equal m.ovector.(0) 1);
      assert (Int.equal m.ovector.(1) 3));
  (* A partial position found mid-bump is remembered and promoted at
     ENDLOOP after the remaining attempts fail (7521-7525, 7747-7762):
     oracle /abcd/ partial_soft on "xyz ab" -> Partial match: ab (4..6). *)
  (match run ~options:Options.partial_soft (compile "abcd") "xyz ab" 0 with
  | rc, m ->
      assert (Int.equal rc Errors.error_partial);
      assert (Int.equal m.ovector.(0) 4);
      assert (Int.equal m.ovector.(1) 6);
      assert (Int.equal m.startchar 4));
  (* rc is end_offset_top/2 + 1, counting unset middle groups below the
     high-water mark (7716-7717): oracle /(a)|(b)/ on "b" prints 0..2 with
     1: <unset>. *)
  (match run (compile "(a)|(b)") "b" 0 with
  | rc, m ->
      assert (Int.equal rc 3);
      assert (Int.equal m.ovector.(0) 0);
      assert (Int.equal m.ovector.(1) 1);
      assert (Int.equal m.ovector.(2) Frames.unset);
      assert (Int.equal m.ovector.(3) Frames.unset);
      assert (Int.equal m.ovector.(4) 0);
      assert (Int.equal m.ovector.(5) 1));
  (* ...and stops at the highest CLOSED group: /(a)(b)?/ on "a" -> rc 2,
     the group-2 pair unset-filled by the OP_END copy-out (934-939). *)
  (match run (compile "(a)(b)?") "a" 0 with
  | rc, m ->
      assert (Int.equal rc 2);
      assert (Int.equal m.ovector.(2) 0);
      assert (Int.equal m.ovector.(3) 1);
      assert (Int.equal m.ovector.(4) Frames.unset);
      assert (Int.equal m.ovector.(5) Frames.unset));
  (* Ovector too small: rc = 0 (7716-7717). *)
  (match run ~oveccount:1 (compile "(a)(b)") "ab" 0 with
  | rc, m ->
      assert (Int.equal rc 0);
      assert (Int.equal m.rc 0);
      assert (Int.equal m.ovector.(0) 0);
      assert (Int.equal m.ovector.(1) 2));
  (* startchar is the attempt start, NOT ovector[0] (\K moves the latter
     only): oracle /a\Kbc/ on "xabc" -> 0: bc with startchar 1. *)
  (match run (compile "a\\Kbc") "xabc" 0 with
  | rc, m ->
      assert (Int.equal rc 1);
      assert (Int.equal m.ovector.(0) 2);
      assert (Int.equal m.ovector.(1) 4);
      assert (Int.equal m.startchar 1));
  (* ( *NOTEMPTY) flag transfer into the match options (6621-6635): a*
     never returns the empty match, so "b" is a nomatch but "ab" matches
     the "a". *)
  (let ne = compile "(*NOTEMPTY)a*" in
   (match run ne "b" 0 with
   | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
   match run ne "ab" 0 with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (Int.equal m.ovector.(0) 0);
       assert (Int.equal m.ovector.(1) 1));
  (* PCRE2_FIRSTLINE: the match must start at or before the first newline
     (7164-7186 clamp + 7584-7588 break): oracle nomatch on "ab\nabc",
     match on "abc\nx". *)
  (let fl =
     match Compile.pcre2_compile "abc" ~options:Options.firstline with
     | Error _ -> assert false
     | Ok re -> re
   in
   (match run fl "ab\nabc" 0 with
   | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
   match run fl "abc\nx" 0 with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (Int.equal m.ovector.(1) 3));
  (* PCRE2_STARTLINE bump (7318-7349): /^abc/m has no first_cu; the
     startline scan advances straight past the newline. *)
  (let ml =
     match Compile.pcre2_compile "^abc" ~options:Options.multiline with
     | Error _ -> assert false
     | Ok re -> re
   in
   assert (not (Int.equal (ml.Compile.flags land Compile.startline) 0));
   match run ml "xyz\nabc" 0 with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (Int.equal m.ovector.(0) 4);
       assert (Int.equal m.ovector.(1) 7));
  (* ( *LIMIT_MATCH=n): per-attempt limit -> PCRE2_ERROR_MATCHLIMIT
     surfaces through the default switch arm (7575-7576). NO_START_OPT is
     needed: with it off, the req_cu search for the absent 'z' would
     break the loop with NOMATCH before any attempt (oracle confirmed
     both behaviors). *)
  (match
     run (compile "(*LIMIT_MATCH=10)(*NO_START_OPT)(a+)+z") "aaaaaaaaaaaa" 0
   with
  | rc, _ -> assert (Int.equal rc Errors.error_matchlimit));
  (match run (compile "(*LIMIT_MATCH=10)(a+)+z") "aaaaaaaaaaaa" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
  (* NOTEMPTY_ATSTART: only the start-of-match empty match is rejected
     (881-895 read mb.start_offset): oracle /a*/ on "bc" with
     notempty_atstart matches "" at 1. *)
  match run ~options:Options.notempty_atstart (compile "a*") "bc" 0 with
  | rc, m ->
      assert (Int.equal rc 1);
      assert (Int.equal m.ovector.(0) 1);
      assert (Int.equal m.ovector.(1) 1)

(* Backreferences (this chunk): match_ref + the OP_REF/OP_REFI/OP_DNREF/
   OP_DNREFI arms with their repeat strategies (min-only, minimize RM20,
   maximize RM21/RM22), the unset-reference rules with and without
   PCRE2_MATCH_UNSET_BACKREF, the zero-length-reference loop break, and
   the partial sites. Whole compiled patterns through the [pcre2_match]
   driver; EVERY expected value below is pinned against the C oracle
   (pcre2test on the real 10.44 library). *)
let () =
  let compile ?(options = 0) pat =
    match Compile.pcre2_compile pat ~options with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run ?(options = 0) (re : Compile.re) subj =
    let oveccount = re.Compile.top_bracket + 1 in
    let m =
      {
        ovector = Array.make (2 * oveccount) Frames.unset;
        oveccount;
        rc = 0;
        startchar = 0;
        leftchar = 0;
        rightchar = 0;
        mark = Frames.unset;
      }
    in
    let rc = pcre2_match re ~subject:subj ~start_offset:0 ~options m in
    (rc, m)
  in
  let expect_ov ?options re subj expected =
    match run ?options re subj with
    | rc, m ->
        assert (rc > 0);
        assert (Int.equal (Array.length m.ovector) (Array.length expected));
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch ?options re subj =
    match run ?options re subj with
    | rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let expect_partial ?options re subj s e =
    match run ?options re subj with
    | rc, m ->
        assert (Int.equal rc Errors.error_partial);
        assert (Int.equal m.ovector.(0) s);
        assert (Int.equal m.ovector.(1) e)
  in
  (* Single copy, no repeat — oracle: 0: aa, 1: a / no match. *)
  (let re = compile "(a)\\1" in
   expect_ov re "aa" [| 0; 2; 0; 1 |];
   expect_nomatch re "ab");
  (* Greedy range OP_CRRANGE (RM21 back-off): {2,4} takes the maximum
     available then backs off in reference-length steps — oracle: aaa /
     aaaa / aaaaa (of "aaaaaa": 1 + 4 copies) / no match on aa. *)
  (let re = compile "(a)\\1{2,4}" in
   expect_ov re "aaa" [| 0; 3; 0; 1 |];
   expect_ov re "aaaa" [| 0; 4; 0; 1 |];
   expect_ov re "aaaaaa" [| 0; 5; 0; 1 |];
   expect_nomatch re "aa");
  (* Minimize OP_CRMINRANGE (RM20): grows one copy at a time up to the
     bound — oracle: aaab -> 0: aaab; aaaaab -> 0: aaaaab. *)
  (let re = compile "(a)\\1{2,4}?b" in
   expect_ov re "aaab" [| 0; 4; 0; 1 |];
   expect_ov re "aaaaab" [| 0; 6; 0; 1 |]);
  (* Caseless reference (OP_REFI, the lcc fold in match_ref) — oracle:
     0: abAB, 1: ab. *)
  (let re = compile "(?i)(ab)\\1" in
   expect_ov re "abAB" [| 0; 4; 0; 2 |];
   expect_ov re "abab" [| 0; 4; 0; 2 |]);
  (* Unset reference, single copy: default no match; with
     PCRE2_MATCH_UNSET_BACKREF (a compile option, read from poptions) it
     matches empty — oracle: no match / 0: "" with 1: <unset>. *)
  expect_nomatch (compile "(a)?\\1") "b";
  expect_ov
    (compile ~options:Options.match_unset_backref "(a)?\\1")
    "b" [| 0; 0; -1; -1 |];
  (* Zero-length reference under * must break the repeat loop instead of
     looping forever (the set-group length-0 continue, 5068) — oracle:
     0: x, 1: "". *)
  expect_ov (compile "()\\1*x") "x" [| 0; 1; 0; 0 |];
  (* Greedy star over a set reference with RM21 back-off releasing one
     copy — oracle: 0: aaa, 1: a. *)
  expect_ov (compile "(a)\\1*a") "aaa" [| 0; 3; 0; 1 |];
  (* OP_CRRANGE max 0 => infinity ({2,}) — oracle: 0: aaab / no match. *)
  (let re = compile "(a)\\1{2,}b" in
   expect_ov re "aaab" [| 0; 4; 0; 1 |];
   expect_nomatch re "aab");
  (* OP_CRPLUS / OP_CRQUERY forms — oracle: aab/ab both match for ?,
     + needs one copy. *)
  (let re = compile "(a)\\1?b" in
   expect_ov re "aab" [| 0; 3; 0; 1 |];
   expect_ov re "ab" [| 0; 2; 0; 1 |]);
  (let re = compile "(a)\\1+b" in
   expect_ov re "aab" [| 0; 3; 0; 1 |];
   expect_nomatch re "ab");
  (* Unset reference under a repeat: Lmin = 0 continues (5072-5073) —
     oracle: 0: b; Lmin > 0 fails in the min loop — oracle: no match —
     unless MATCH_UNSET_BACKREF also continues — oracle: 0: b. *)
  expect_ov (compile "(a)?\\1{0,3}b") "b" [| 0; 1; -1; -1 |];
  expect_nomatch (compile "(a)?\\1{2,3}b") "b";
  expect_ov
    (compile ~options:Options.match_unset_backref "(a)?\\1{2,3}b")
    "b" [| 0; 1; -1; -1 |];
  (* Duplicate names (OP_DNREF): the group-list scan uses the first SET
     group — oracle: aa -> 1: a (group 2 unset); bb -> 1: <unset>, 2: b;
     ab -> no match. *)
  (let re = compile "(?J)(?:(?<n>a)|(?<n>b))\\k<n>" in
   expect_ov re "aa" [| 0; 2; 0; 1; -1; -1 |];
   expect_ov re "bb" [| 0; 2; -1; -1; 0; 1 |];
   expect_nomatch re "ab");
  (* Partial sites. Single copy (CHECK_PARTIAL after Feptr = end_subject,
     5050-5051) — oracle: Partial match: aba (hard and soft) and ab. *)
  (let re = compile "(ab)\\1" in
   expect_partial ~options:Options.partial_hard re "aba" 0 3;
   expect_partial ~options:Options.partial_soft re "aba" 0 3;
   expect_partial ~options:Options.partial_hard re "ab" 0 2);
  (* The min loop's CHECK_PARTIAL (5084-5085) — oracle: Partial match:
     aaa. *)
  expect_partial ~options:Options.partial_hard (compile "(a)\\1{3}") "aaa" 0 3;
  (* Caseless repeat, fixed count: {2} = CRRANGE(2,2), so Lmin = Lmax and
     the arm exits at the Lmin == Lmax continue (5093); the partial fires
     in the min loop's CHECK_PARTIAL (5084-5085) — oracle: 0: abABab then
     Partial match: abABa. COVERAGE TODO: the maximize scan's hard-partial
     branch (5135-5139) needs Lmin < Lmax hitting subject end, e.g.
     (?i)(ab)\\1{2,4} on "abABa"-class input — pin via oracle when the M4
     possessive-ref work touches this region. *)
  let re = compile "(?i)(ab)\\1{2}" in
  expect_ov re "abABab" [| 0; 6; 0; 2 |];
  expect_partial ~options:Options.partial_hard re "abABa" 0 5
