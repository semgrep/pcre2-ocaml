(* Interpreter dispatch skeleton for the pure-OCaml PCRE2 10.44 port
   (8-bit library).

   Ported from vendor/pcre2/src/pcre2_match.c: the match() function shell
   and first-frame setup (565-657), the RMATCH/RRETURN backtracking
   protocol as data (550-562; MATCH_RECURSE 662-754 via Frames.push;
   NEW_FRAME 758-783; RETURN_SWITCH 6462-6501), the main dispatch loop
   (790-798, 6445-6457), OP_END (876-940), the char family OP_CHAR/
   OP_CHARI/OP_NOT/OP_NOTI and the single-character repeat machinery
   REPEATCHAR/REPEATNOTCHAR with resume labels RM25-RM32 and (UTF)
   RM202-RM207 (992-1916, incl. the UCP-without-UTF arms), the bit-mapped
   class family OP_CLASS/OP_NCLASS with resume labels RM23/RM24 and (UTF)
   RM200/RM201 (1919-2172), OP_XCLASS with RM100/RM101 (2175-2302; the
   XCL item matcher is xclass.ml),
   the character-type singles OP_ANY..OP_VSPACE (943-989, 2305-2476),
   OP_PROP/OP_NOTPROP (2479-2614) and
   the TYPE repeat machinery REPEATTYPE with resume labels RM33/RM34,
   (UTF) RM219/RM221 and (properties) RM208-RM217/RM222-RM225
   (2651-5005), OP_EXTUNI and its repeat loops with resume labels
   RM218/RM220 (2617-2635, 2976-2996, 3804-3827, 4401-4467; the cluster
   stepper is extuni.ml), the
   backreference family
   match_ref and OP_REF/OP_REFI/OP_DNREF/OP_DNREFI with the repeat
   machinery and resume labels RM20-RM22 (338-481, 4980-5194, including
   the Unicode caseless fold 388-434), the anchors and simple
   assertions OP_CIRC(M)/OP_DOLL(M)/OP_SOD/OP_SOM/OP_SET_SOM/OP_EOD/
   OP_EODN/word boundaries incl. the UCP variants (6132-6338), the
   bracket/alternation/ket family OP_BRAZERO/OP_BRAMINZERO/OP_SKIPZERO
   (5224-5246), OP_BRA/OP_CBRA/OP_SCBRA and the shared GROUPLOOP
   (5349-5411, incl. the OP_ONCE/OP_SCRIPT_RUN/OP_SBRA head at 5391-5394;
   the OP_SCRIPT_RUN ket action at 6045-6051 calls script_run.ml), OP_ALT
   and OP_KET/
   OP_KETRMIN/OP_KETRMAX with resume labels RM1/RM2/RM6/RM7/RM9/RM10
   (5893-6127), the conditional/recursion family — OP_COND/OP_SCOND with
   the condition opcodes and the assertion-condition protocol
   (5603-5778, resume labels RM5/RM35), OP_RECURSE with RECURSELOOP
   detection (5417-5497, resume label RM11), OP_FAIL (6359-6360), and
   their ket actions (the GF_CONDASSERT return 5934-5948, whole-pattern
   recursion 5955-5984, recursed-group capture reinstate 6056-6074) —
   the lookaround/atomic/possessive family — OP_ASSERT_ACCEPT (832-840), the
   possessive brackets OP_BRAPOSZERO/OP_BRAPOS/OP_SBRAPOS/OP_CBRAPOS/
   OP_SCBRAPOS with resume label RM8 (5249-5334), the assertion brackets
   OP_ASSERT/OP_ASSERTBACK/OP_ASSERT_NA/OP_ASSERTBACK_NA (RM3) and
   OP_ASSERT_NOT/OP_ASSERTBACK_NOT (RM4) (5508-5586), the lookbehind
   steppers OP_REVERSE/OP_VREVERSE with resume label RM37 (5787-5888,
   both UTF and non-UTF arms), and their ket actions including OP_KETRPOS
   and the ONCE/assertion backtrack discard (5990-6098) —
   the backtracking-verb family — OP_CLOSE/OP_ACCEPT (800-874, sharing
   the OP_END tail), the verb opcodes OP_MARK/OP_COMMIT(_ARG)/
   OP_PRUNE(_ARG)/OP_SKIP(_ARG)/OP_THEN(_ARG) with resume labels
   RM12-RM19/RM36 (6336-6442), the MATCH_THEN branch-scope checks in the
   RM2/RM8/RM11 resumes (5202-5211, 5305-5313, 5401-5407, 5471-5488) and
   the driver's verb rc switch (7527-7577) —
   the UTF-mode machinery — the subject validity check with
   mb->check_subject and PCRE2_ERROR_BADUTFOFFSET (6795-6929), the
   PCRE2_MATCH_INVALID_UTF fragment protocol (skipped_bad_start
   6828-6836, the fragment validation loop 6889-6928, FRAGMENT_RESTART
   7139-7148 and the ENDLOOP fragment carry-on 7646-7701) and
   the bump-along/FIRSTLINE/STARTLINE ACROSSCHAR stepping (7174-7180,
   7326-7332, 7561-7563) —
   and the match_block structure (pcre2_intmodedep.h:864-906).
   Every other opcode
   that the C switch handles gets an exhaustive STUB arm returning the
   distinctive [error_unported] marker; the remaining M7-M8 chunks
   REPLACE arm bodies only.

   Control-flow translation (port-conventions §2, §6): the C's goto graph
   MATCH_RECURSE / NEW_FRAME / main-loop switch / RETURN_SWITCH becomes
   four mutually tail-recursive functions — [rmatch], [new_frame],
   [dispatch], [backtrack] — every cross-call annotated [@tailcall], so
   the matcher is ONE iterative loop over the Frames arena: depth = frame
   index, constant OCaml stack, no exception crosses the loop (limits and
   growth failures return negative PCRE2 codes as values).

   RMATCH(ra, rb) at a C opcode arm becomes
     (rmatch [@tailcall]) st f ra rb group_frame_type
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
   - pcre2_callout_block *cb (903), void *callout_data (904),
     int ( *callout)(...) (905) — dropped: this library's API has no
     callout surface, so mb->callout is always NULL and no callout block
     is ever consulted (see [do_callout_length]). *)
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
  mutable check_subject : int;
      (* PCRE2_SPTR check_subject (885): where UTF-checked from — equal to
         start_subject except in UTF mode with a nonzero start offset,
         where the driver backs it up over the maximum lookbehind
         (pcre2_match.c:6795, 6851, 6862-6871); lookbehinds and the
         word-boundary previous-character probe stop here. Mutable
         because the MATCH_INVALID_UTF fragment carry-on re-points it at
         each new fragment's start (pcre2_match.c:7674). *)
  mutable end_subject : int;
      (* PCRE2_SPTR end_subject (886): usable end; mutable because the
         MATCH_INVALID_UTF fragment carry-on shortens/restores it per
         fragment (pcre2_match.c:7682, 7692) *)
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
  mutable verb_ecode_ptr : int;
      (* PCRE2_SPTR verb_ecode_ptr (893): for passing back info — the
         code offset of the OP_THEN/OP_THEN_ARG opcode that triggered a
         MATCH_THEN return (pcre2_match.c:6432/6440); -1 = never set (the
         C leaves it uninitialized; it is only read after a MATCH_THEN
         set it) *)
  mutable verb_skip_ptr : int;
      (* PCRE2_SPTR verb_skip_ptr (894): for passing back a ( *SKIP)
         position or name. Dual use, exactly as the C: a SUBJECT offset
         while a MATCH_SKIP is being passed back (pcre2_match.c:6354,
         6395, consumed by the driver at 7545-7547), a CODE offset of the
         skip name while a MATCH_SKIP_ARG is (6422, consumed by the
         OP_MARK resume at 6351-6352); -1 = never set *)
  mutable verb_current_recurse : int;
      (* uint32_t verb_current_recurse (895): current recursion group
         when a ( *VERB) backtrack return happens — set alongside every
         MATCH_COMMIT..MATCH_THEN return, consumed by the OP_RECURSE
         resume (pcre2_match.c:5481-5482) *)
  mutable moptions : int;
      (* uint32_t moptions (896): match options; mutable because the
         driver's bump-along loop resets it per attempt
         (pcre2_match.c:7502-7504) *)
  poptions : int; (* uint32_t poptions (897): pattern options *)
  mutable skip_arg_count : int;
      (* uint32_t skip_arg_count (898): for counting SKIP_ARGs — reset
         per attempt by the driver (pcre2_match.c:7508), bumped by the
         OP_SKIP_ARG arm (6408) *)
  mutable ignore_skip_arg : int;
      (* uint32_t ignore_skip_arg (899): for re-run when a SKIP arg name
         was not found — 0 at driver entry (pcre2_match.c:6970), set to
         skip_arg_count when a MATCH_SKIP_ARG reaches the top (7538),
         reset to 0 on a normal bump-along (7558) *)
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

(* DEVIATION (perf): the C keeps match()'s locals on its stack frame and
   passes mb explicitly; OCaml closes over them, which allocated the whole
   function nest per attempt. This record is that environment, allocated
   once per pcre2_match/match_internal call and threaded explicitly.
   Field order: the hottest pointers (mb, arena) come first for the
   smallest load offsets. The mutable fields are ints only — stores are
   setfield_imm, no caml_modify write barrier after construction — and
   every heap-pointing field is immutable. The frames int array is
   deliberately NOT cached here: Frames growth re-points arena.frames
   (frames.ml:350), so it must be re-read through [arena] at function
   heads. *)
type match_state = {
  mb : match_block;
  arena : Frames.t;
  match_data : match_data;
  top_bracket : int;
  utf : bool; (* pcre2_match.c:630-637 *)
  ucp : bool;
  nl_scratch : int ref;
      (* out-parameter scratch for IS_NEWLINE/WAS_NEWLINE
         (pcre2_internal.h:496-521); write-before-read, never reset *)
  ref_length : int ref;
      (* pcre2_match.c:613 + 353 — PCRE2_SIZE length: the match()-local
         that the backreference arms pass to match_ref() as its lengthptr
         out-parameter (both the C's `length` at 5047 and the block-local
         `slength`s at 5080/5101/5128/5179 land here); write-before-read
         (the C's uninitialized local), never reset. A single cell
         suffices because match_ref never re-enters the dispatch loop, so
         at most one call's result is ever pending. *)
  mutable branch_end : int; (* pcre2_match.c:609; -1 = NULL *)
  mutable assert_accept_frame : int; (* pcre2_match.c:604; -1 = NULL *)
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

(* pcre2_internal.h:416-431 — HSPACE_CASES: the byte cases plus
   HSPACE_MULTIBYTE_CASES, for the UTF repeat loops that switch on a
   decoded character. *)
let hspace_char (c : int) : bool =
  match c with
  | 0x09 | 0x20 | 0xa0 | 0x1680 | 0x180e | 0x2000 | 0x2001 | 0x2002 | 0x2003
  | 0x2004 | 0x2005 | 0x2006 | 0x2007 | 0x2008 | 0x2009 | 0x200a | 0x202f
  | 0x205f | 0x3000 ->
      true
  | _ -> false

(* pcre2_internal.h:433-449 — VSPACE_CASES: the byte cases plus
   VSPACE_MULTIBYTE_CASES (U+2028 LS, U+2029 PS). *)
let vspace_char (c : int) : bool =
  match c with
  | 0x0a | 0x0b | 0x0c | 0x0d | 0x85 | 0x2028 | 0x2029 -> true
  | _ -> false

(* pcre2_match.c:2576-2583 (= 2903-2916, 3723-3736, 4311-4319) — the
   PT_CLIST scan: cp = PRIV(ucd_caseless_sets) + <property value>;
   for (;;) { if (fc < *cp) <no match>; if (fc == *cp++) <match>; }.
   Terminates inside the table: the sets are ascending and end with
   NOTACHAR = 0xffffffff (pcre2_ucd.c:114-143), which exceeds every
   decoded character (getutf8 yields at most 0x7fffffff), so the fc < *cp
   exit always fires by the set's end. *)
let rec prop_clist_member (fc : int) (cp : int) : bool =
  let v = Ucd_tables.ucd_caseless_sets.(cp) in
  if fc < v then false
  else if Int.equal fc v then true
  else (prop_clist_member [@tailcall]) fc (cp + 1)

(* pcre2_match.c:2493-2610 — the Unicode property test shared by the
   OP_PROP/OP_NOTPROP single-character arm and the property repeat loops
   (min 2726-2974, minimize 3515-3801, maximize 4115-4381): does
   character [c] have property ([ptype], [pdata])?

   DEVIATION(structure): the C repeats this switch inline at each of
   those four sites with the notmatch / (Lctype == OP_NOTPROP)
   comparison folded into every case; factored here once — each call
   site keeps its own comparison, control flow and evaluation order.
   Cases in the C's order, with the C's exact category groupings; the
   PT_CLIST arms' fc < */== *cp exits reduce to set membership compared
   against notmatch at the call site, exactly as the other cases.
   Callers exclude property types above PT_BOOL (the C switches'
   defaults: PCRE2_ERROR_INTERNAL). The C fetches the ucd_record once
   per character (GET_UCD); cases probing several record fields do the
   same through [Ucd.record_index]. *)
let prop_test (c : int) (ptype : int) (pdata : int) : bool =
  if Int.equal ptype Opcodes.pt_any then true (* 2495-2497 *)
  else if Int.equal ptype Opcodes.pt_lamp then
    (* pcre2_match.c:2499-2505 *)
    let chartype = Ucd.chartype c in
    Int.equal chartype Ucp.ucp_lu
    || Int.equal chartype Ucp.ucp_ll
    || Int.equal chartype Ucp.ucp_lt
  else if Int.equal ptype Opcodes.pt_gc then
    (* pcre2_match.c:2507-2510 — Fecode[2] ==
       PRIV(ucp_gentype)[prop->chartype]. *)
    Int.equal pdata Tables.ucp_gentype.(Ucd.chartype c)
  else if Int.equal ptype Opcodes.pt_pc then
    (* pcre2_match.c:2512-2515 *)
    Int.equal pdata (Ucd.chartype c)
  else if Int.equal ptype Opcodes.pt_sc then
    (* pcre2_match.c:2517-2520 *)
    Int.equal pdata (Ucd.script c)
  else if Int.equal ptype Opcodes.pt_scx then
    (* pcre2_match.c:2522-2528 — script match, or the Script Extensions
       set bit (MAPBIT bound proof: Ucd.script_set_contains). *)
    let ri = Ucd.record_index c in
    Int.equal pdata (Ucd_tables.script ri) || Ucd.script_set_contains ri pdata
  else if Int.equal ptype Opcodes.pt_alnum then
    (* pcre2_match.c:2532-2537 — these are specials. *)
    let gentype = Tables.ucp_gentype.(Ucd.chartype c) in
    Int.equal gentype Ucp.ucp_l || Int.equal gentype Ucp.ucp_n
  else if Int.equal ptype Opcodes.pt_space || Int.equal ptype Opcodes.pt_pxspace
  then
    (* pcre2_match.c:2539-2557 — Perl space and POSIX space are identical
       since Perl 5.18 / PCRE 8.34: the HSPACE/VSPACE cases, else general
       category Z. *)
    hspace_char c || vspace_char c
    || Int.equal Tables.ucp_gentype.(Ucd.chartype c) Ucp.ucp_z
  else if Int.equal ptype Opcodes.pt_word then
    (* pcre2_match.c:2559-2566 *)
    let chartype = Ucd.chartype c in
    let gentype = Tables.ucp_gentype.(chartype) in
    Int.equal gentype Ucp.ucp_l
    || Int.equal gentype Ucp.ucp_n
    || Int.equal chartype Ucp.ucp_mn
    || Int.equal chartype Ucp.ucp_pc
  else if Int.equal ptype Opcodes.pt_clist then
    (* pcre2_match.c:2568-2584 (the width-32 MAX_UTF guard is compiled
       out in the 8-bit library) *)
    prop_clist_member c pdata
  else if Int.equal ptype Opcodes.pt_ucnc then
    (* pcre2_match.c:2586-2591 — CHAR_DOLLAR_SIGN 0x24,
       CHAR_COMMERCIAL_AT 0x40, CHAR_GRAVE_ACCENT 0x60. *)
    Int.equal c 0x24 || Int.equal c 0x40 || Int.equal c 0x60
    || (c >= 0xa0 && c <= 0xd7ff)
    || c >= 0xe000
  else if Int.equal ptype Opcodes.pt_bidicl then
    (* pcre2_match.c:2593-2596 — UCD_BIDICLASS_PROP(prop) == Fecode[2]. *)
    Int.equal (Ucd.bidiclass c) pdata
  else
    (* PT_BOOL (pcre2_match.c:2598-2604) — MAPBIT over the
       Boolean-property set (bound proof: Ucd.boolprop_set_contains);
       the callers' ptype <= PT_BOOL gate makes this the last case. *)
    Ucd.boolprop_set_contains (Ucd.record_index c) pdata

(* pcre2_intmodedep.h:341-345 — BACKCHAR(eptr) over the subject: if the
   position is not at the start of a character, move it back until it is.
   DEVIATION (defined behavior where the C is undefined, the Utf.peek
   precedent): the walk is clamped at position 0 and out-of-range positions
   read as 0 (not a continuation byte, so the walk stops). On a valid UTF
   subject — every path except PCRE2_NO_UTF_CHECK with garbage — the C's
   unclamped walk never leaves the string either: it stops at the
   character's lead byte, and truncated tail sequences cannot push a
   position past the end because Valid_utf has excluded them. *)
let backchar_subject (s : string) (pos : int) : int =
  let len = String.length s in
  let p = ref pos in
  while
    !p > 0 && !p < len
    (* safe: 0 < !p < len checked in the conjuncts above *)
    && Int.equal (Char.code (String.unsafe_get s !p) land 0xc0) 0x80
  do
    decr p
  done;
  !p

(* GETCHAR (pcre2_intmodedep.h:298-303) over the subject with the lead
   byte read through Utf.peek: for the few sites where a decode may be
   asked for at a position outside the subject — at or past the end under
   PCRE2_NO_UTF_CHECK garbage, or at the pinned position -1 from the
   word-boundary previous-character probe (the C reads unowned memory in
   both cases — see the Utf.peek DEVIATION note; peek returns 0 there, so
   the decode yields fc = 0). On every valid-UTF path the position is in
   bounds and this is exactly GETCHAR. *)
let getchar_subject (s : string) (pos : int) : int =
  let c = Utf.peek s pos in
  if c >= 0xc0 then Utf.getutf8 c s pos else c

(* pcre2_ord2utf.c:80-97 — PRIV(ord2utf)(othercase, Foccu): the same
   encoder as Utf.ord2utf, depositing the code units into frame arena
   slots (the C's PCRE2_UCHAR occu[6], Frames.slot_occu) instead of a
   Bytes buffer, so the encoding survives RMATCH in its frame like every
   other frame temporary. Returns the number of code units used. *)
let ord2utf_slots (cvalue : int) (fr : int array) (base : int) : int =
  (* pcre2_ord2utf.c:86-88 — find the number of extra bytes. *)
  let i = ref 0 in
  let brk = ref false in
  while (not !brk) && !i < Tables.utf8_table1_size do
    if cvalue <= Tables.utf8_table1.(!i) then brk := true else incr i
  done;
  let i = !i in
  (* pcre2_ord2utf.c:89-95 — fill in the bytes from the end backwards,
     then the first byte. In bounds: i <= 5 and the occu region is 6
     slots (frames.ml layout table). *)
  let cv = ref cvalue in
  for j = i downto 1 do
    fr.(base + j) <- 0x80 lor (!cv land 0x3f);
    cv := !cv lsr 6
  done;
  fr.(base) <- Tables.utf8_table2.(i) lor !cv land 0xff;
  i + 1

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

(* ---------- match() helpers ---------- *)

(* The functions below are match()-locals/macros in the C; they are
   module-level here (DEVIATION (perf) — see [match_state]) so [match_]'s
   dispatch loop builds no closures per attempt. Per-exec state reaches
   them through an explicit [match_state] or [match_block] first
   parameter; they are always fully applied. *)

(* pcre2_internal.h:496-507 — IS_NEWLINE(p): NLBLOCK is mb, PSEND is
   end_subject (pcre2_match.c:60-66). For the non-fixed conventions
   PRIV(is_newline) writes the length of the newline it found through
   &mb->nllen; [st.nl_scratch] is that out-parameter, preallocated once
   per exec in [match_state] so the dispatch loop stays allocation-free,
   and copied to mb.nllen exactly when the C writes it (TRUE returns only
   — Newline.is_newline leaves the ref untouched on FALSE). *)
let is_newline_at (st : match_state) (p : int) : bool =
  let mb = st.mb in
  let nl_scratch = st.nl_scratch in
  let utf = st.utf in
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
       || Int.equal (Char.code (String.unsafe_get mb.subject (p + 1))) mb.nl1)

(* pcre2_internal.h:510-521 — WAS_NEWLINE(p): PSSTART is start_subject
   (pcre2_match.c:60-66). *)
let was_newline_at (st : match_state) (p : int) : bool =
  let mb = st.mb in
  let nl_scratch = st.nl_scratch in
  let utf = st.utf in
  if not (Int.equal mb.nltype Newline.nltype_fixed) then (
    p > mb.start_subject
    &&
    let hit =
      Newline.was_newline mb.subject mb.nltype p mb.start_subject nl_scratch utf
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

(* pcre2_match.c:1930 + 2014 — Lbyte_map[fc/8] & (1u << (fc&7)): probe
   the 32-byte class bitmap saved at Lbyte_map_address (temp_sptr[1]).
   In bounds: the map is part of the OP_CLASS/OP_NCLASS item in the
   compiled program (mb invariant) and fc lsr 3 <= 31. *)
let class_bit (st : match_state) (f : int) (fc : int) : int =
  let mb = st.mb in
  let a = st.arena in
  let fr = a.Frames.frames in
  let fb = Frames.base a f in
  Char.code
    (Bytes.get mb.start_code (fr.(fb + Frames.slot_temp_sptr_1) + (fc lsr 3)))
  land (1 lsl (fc land 7))

(* GETCHARINCTEST(fc, Feptr); Feptr = PRIV(extuni)(fc, Feptr,
   mb->start_subject, mb->end_subject, utf, NULL) — the shared body of
   the four OP_EXTUNI stepping sites (pcre2_match.c:2629-2631 =
   2990-2992 = 3821-3823 = 4415-4417): one grapheme cluster from [eptr],
   returning the position after it. Module-level (the [is_newline_at]
   precedent), so the dispatch loop stays allocation-free. Caller
   contract: eptr < mb.end_subject. *)
let extuni_step (st : match_state) (eptr : int) : int =
  let mb = st.mb in
  let utf = st.utf in
  (* safe: eptr < mb.end_subject <= String.length mb.subject (caller
     contract); 0 <= start_eptr <= eptr (mb invariant) *)
  let c0 = Char.code (String.unsafe_get mb.subject eptr) in
  let fc = if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
  let eptr' =
    if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0 else eptr + 1
  in
  Extuni.extuni fc mb.subject eptr' mb.start_subject mb.end_subject utf

(* pcre2_string_utils.c:101-112 — PRIV(strcmp) over two zero-terminated
   verb names stored in the compiled code, consulted only for equality
   (the `== 0` test at the OP_MARK resume, pcre2_match.c:6351-6352).
   Terminates: verb names are emitted with a terminating zero
   (pcre2_compile.c:6571-6572; mb complete-program invariant). *)
let rec strcmp_code_eq (mb : match_block) (p1 : int) (p2 : int) : bool =
  let c1 = Char.code (Bytes.get mb.start_code p1) in
  let c2 = Char.code (Bytes.get mb.start_code p2) in
  if Int.equal c1 0 && Int.equal c2 0 then true
  else if not (Int.equal c1 c2) then false
  else (strcmp_code_eq [@tailcall]) mb (p1 + 1) (p2 + 1)

(* Bounds contract shared by the three match_ref compare loops below
   ([match_ref] establishes it): [p] ranges over the captured substring
   — p0 = start_subject + Fovector[Loffset] up to (exclusive)
   start_subject + Fovector[Loffset+1] — whose bounds are former eptr
   values recorded as eptr - start_subject by the capturing-ket writes
   (pcre2_match.c:6077-6084), so 0 <= start_subject <= p and, while
   length > 0, p < start_subject + Fovector[Loffset+1] <= end_subject
   <= String.length mb.subject. [eptr] starts at Feptr >= 0 (mb
   invariant). *)
(* pcre2_match.c:424-429 — match_ref()'s caseless-set probe: walk the
   NOTACHAR-terminated ascending list at [pp] in
   Ucd_tables.ucd_caseless_sets; `if (c < *pp) return -1` is the
   no-match exit (entries ascend and end with NOTACHAR = 0xffffffff,
   larger than any code point). *)
let rec caseless_set_member (c : int) (pp : int) : bool =
  let v = Ucd_tables.ucd_caseless_sets.(pp) in
  if c < v then false
  else if Int.equal c v then true
  else (caseless_set_member [@tailcall]) c (pp + 1)

(* pcre2_match.c:390-434 — match_ref()'s caseless compare loop in UTF
   and/or UCP mode: match characters up to the end of the REFERENCE
   (p < endptr — the number of subject code units matched may differ,
   e.g. U+023A/U+2C65 have different UTF-8 lengths, so the length is
   checked along the reference, not the subject). On success the number
   of subject code units consumed (eptr - eptr_start) goes through
   [lengthptr] (the C's shared exit at 479). Returns 0 matched / -1 no
   match / 1 partial. *)
let rec match_ref_ci_uni (mb : match_block) ~(utf : bool) (p : int)
    (endptr : int) (eptr : int) (eptr_start : int) (lengthptr : int ref) : int =
  if p >= endptr then (
    lengthptr := eptr - eptr_start (* 479 *);
    0)
  else if eptr >= mb.end_subject then 1 (* partial match, 408 *)
  else
    (* pcre2_match.c:410-419 — if (utf) GETCHARINC(c, eptr);
       GETCHARINC(d, p); else one code unit each. Lead bytes in bounds:
       eptr < end_subject (checked above) and p < endptr <=
       end_subject (the reference bounds contract above). *)
    let ce = Char.code (String.unsafe_get mb.subject eptr) in
    let c = if utf && ce >= 0xc0 then Utf.getutf8 ce mb.subject eptr else ce in
    let eptr' =
      if utf && ce >= 0xc0 then eptr + 1 + Utf.get_extralen ce else eptr + 1
    in
    let cp = Char.code (String.unsafe_get mb.subject p) in
    let d = if utf && cp >= 0xc0 then Utf.getutf8 cp mb.subject p else cp in
    let p' = if utf && cp >= 0xc0 then p + 1 + Utf.get_extralen cp else p + 1 in
    (* pcre2_match.c:421-431 — ur = GET_UCD(d); other case, then the
       caseless set. *)
    if
      Int.equal c d
      || Int.equal c (Ucd.othercase d)
      || caseless_set_member c (Ucd.caseset d)
    then
      (match_ref_ci_uni [@tailcall]) mb ~utf p' endptr eptr' eptr_start
        lengthptr
    else -1 (* no match, 427 *)

(* pcre2_match.c:438-451 — match_ref()'s caseless compare loop, not in
   UTF or UCP mode: fold both code units through the lcc table.
   Returns 0 all matched / -1 no match / 1 partial. *)
let rec match_ref_ci (mb : match_block) (p : int) (eptr : int) (length : int) :
    int =
  if length <= 0 then 0
  else if eptr >= mb.end_subject then 1 (* partial match, 443 *)
  else
    (* safe: eptr < mb.end_subject <= String.length mb.subject (checked
       above), eptr >= 0; p in the captured substring (contract above) *)
    let cc = Char.code (String.unsafe_get mb.subject eptr) in
    let cp = Char.code (String.unsafe_get mb.subject p) in
    if not (Int.equal (Chartables.lcc cp) (Chartables.lcc cc)) then -1
      (* no match, 446-447 *)
    else (match_ref_ci [@tailcall]) mb (p + 1) (eptr + 1) (length - 1)

(* pcre2_match.c:460-467 — match_ref()'s caseful compare loop for
   partial matching: unit by unit, checking the subject end before each
   unit. *)
let rec match_ref_cs_partial (mb : match_block) (p : int) (eptr : int)
    (length : int) : int =
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
  else (match_ref_cs_partial [@tailcall]) mb (p + 1) (eptr + 1) (length - 1)

(* pcre2_match.c:474 — memcmp(p, eptr, CU2BYTES(length)) != 0 as an
   equality scan (only equality is consulted). Caller checked
   end_subject - eptr >= length. *)
let rec match_ref_memcmp (mb : match_block) (p : int) (eptr : int)
    (length : int) : bool =
  length <= 0
  || Int.equal
       (* safe: eptr + length <= mb.end_subject <= String.length
          mb.subject (caller's 473 check), eptr >= 0; p in the captured
          substring (contract above) *)
       (Char.code (String.unsafe_get mb.subject p))
       (Char.code (String.unsafe_get mb.subject eptr))
     && (match_ref_memcmp [@tailcall]) mb (p + 1) (eptr + 1) (length - 1)

(* pcre2_match.c:1299 (and 1321, 1341) — memcmp(Feptr, Lcharptr,
   CU2BYTES(Flength)) == 0: compare [n] code units of the compiled
   pattern at [cpos] with the subject at [spos]. Caller checked
   spos + n <= end_subject; cpos + n is inside the pattern literal
   (complete-program invariant). *)
let rec code_eq_subject (mb : match_block) (cpos : int) (spos : int) (n : int) :
    bool =
  n <= 0
  || Int.equal
       (Char.code (Bytes.get mb.start_code cpos))
       (* safe: spos + n <= mb.end_subject <= String.length mb.subject
          (caller's bound) and spos >= 0 (an eptr; mb invariant) *)
       (Char.code (String.unsafe_get mb.subject spos))
     && (code_eq_subject [@tailcall]) mb (cpos + 1) (spos + 1) (n - 1)

(* pcre2_match.c:1301-1302 (and 1323-1324, 1343-1345) — memcmp(Feptr,
   Foccu, CU2BYTES(Loclength)) == 0: as [code_eq_subject] but against
   the frame's occu slots (one code unit per slot). *)
let rec occu_eq_subject (mb : match_block) (fr : int array) (opos : int)
    (spos : int) (n : int) : bool =
  n <= 0
  || Int.equal fr.(opos)
       (* safe: spos + n <= mb.end_subject <= String.length mb.subject
          (caller's bound) and spos >= 0 (an eptr; mb invariant) *)
       (Char.code (String.unsafe_get mb.subject spos))
     && (occu_eq_subject [@tailcall]) mb fr (opos + 1) (spos + 1) (n - 1)

(* pcre2_match.c:338-481 — match_ref(): match a back-reference at frame
   [f]'s Feptr. Called only when it is known that the offset lies
   within the offsets that have so far been used in the match (or for
   the unset-group entry check).

   Arguments (360-362): offset = Loffset, the index into the frame
   ovector; caseless = Lcaseless; f = the frame (the C's F); lengthptr
   = the out-parameter for the length matched (the per-exec
   [st.ref_length] cell).

   Returns (355-357): = 0 successful match, number of code units
   matched written to [lengthptr]; < 0 no match; > 0 partial match. *)
let match_ref (st : match_state) (f : int) (offset : int) (caseless : bool)
    (lengthptr : int ref) : int =
  let mb = st.mb in
  let a = st.arena in
  let utf = st.utf in
  let ucp = st.ucp in
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
      if utf || ucp then
        (* pcre2_match.c:388-434 — the SUPPORT_UNICODE utf/ucp fold:
           character-wise compare with UCD other-case and caseless-set
           fallback; [match_ref_ci_uni] writes lengthptr itself on
           success (the consumed subject length can differ from the
           reference length in UTF mode). *)
        match_ref_ci_uni mb ~utf p (p + length) eptr eptr lengthptr
      else
        (* pcre2_match.c:438-451 — not in UTF or UCP mode. *)
        let rc = match_ref_ci mb p eptr length in
        (* pcre2_match.c:479 — *lengthptr = eptr - eptr_start: the loop
           advanced eptr one unit per reference unit, so this is exactly
           [length] in the non-UTF arm. *)
        if Int.equal rc 0 then lengthptr := length;
        rc)
    else if not (Int.equal mb.partial 0) then (
      (* pcre2_match.c:454-467 — in the caseful case, just compare the
         code units; when partial matching, do it unit by unit. *)
      let rc = match_ref_cs_partial mb p eptr length in
      if Int.equal rc 0 then lengthptr := length (* 479, as above *);
      rc)
    else if
      (* pcre2_match.c:469-476 — not partial matching. *)
      mb.end_subject - eptr < length
    then 1 (* partial, 473 *)
    else if not (match_ref_memcmp mb p eptr length) then -1 (* no match, 474 *)
    else (
      (* pcre2_match.c:475-480 — eptr += length;
         *lengthptr = eptr - eptr_start. *)
      lengthptr := length;
      0)

(* pcre2_match.c:254-334 — do_callout: process a callout, whether
   "standalone" or at the start of a conditional group. [ecode] (Fecode)
   points to either OP_CALLOUT or OP_CALLOUT_STR. Returns the length of
   the callout item (the C's *lengthptr, pcre2_match.c:280-281). The C
   then returns the return from the callout function, or 0 if no callout
   function exists (pcre2_match.c:283); this library's API has no callout
   surface, so mb->callout is always NULL and the callout return is
   always 0 — the rest of the C body (callout block setup and invocation,
   pcre2_match.c:285-333) is unreachable and not ported. *)
let do_callout_length (mb : match_block) (ecode : int) : int =
  if Int.equal (Char.code (Bytes.get mb.start_code ecode)) Opcodes.op_callout
  then Opcodes.op_lengths.(Opcodes.op_callout)
  else Compile.get mb.start_code (ecode + 1 + (2 * Limits.link_size))

(* ---------- Match from current position ---------- *)

(* pcre2_match.c:566-6502 — match(): run one match attempt at a single
   starting point in the subject.

   Arguments (pcre2_match.c:578-584; frame_size lives inside the arena):
     start_eptr   starting character in subject (offset)
     start_ecode  starting position in compiled code (offset)
     st           the per-exec [match_state]: top_bracket (number of
                  capturing parentheses), the backtracking-frame arena
                  (match_data->heapframes), match_data (where to write
                  the resulting ovector) and mb (the "static" variables
                  block)

   Returns (pcre2_match.c:586-591): MATCH_MATCH (1) if matched,
   MATCH_NOMATCH (0) if failed to match, a negative MATCH_xxx value for
   PRUNE, SKIP, etc, or a negative PCRE2_ERROR_xxx value if aborted by an
   error condition. *)
let match_ (st : match_state) ~(start_eptr : int) ~(start_ecode : int) : int =
  (* pcre2_match.c:630-637 — UTF and UCP flags (computed per exec into
     [st] from mb.poptions). Field punning keeps every downstream name
     unchanged; the arena field reintroduces its local name [a]. *)
  let { mb; arena = a; utf; ucp; ref_length; _ } = st in
  (* pcre2_match.c:609 — PCRE2_SPTR branch_end = NULL: a match()-local;
     C:607 says such locals do not NEED to survive RMATCH, but this one in
     fact persists for the whole match() invocation — and must (the
     ALT->KET adjacency protocol reads it after resumes). OP_ALT records
     the end of the matched branch in it (5895); the OP_KET branch_start
     scan consumes and resets it (5913-5917). -1 = NULL (code offsets are
     >= 0). Lives in [st] (per exec) so the dispatch loop stays
     allocation-free; reset here, at every match() entry. *)
  st.branch_end <- -1;
  (* pcre2_match.c:604 — heapframe *assert_accept_frame = NULL: for
     passing back a frame with captures. Set by OP_ASSERT_ACCEPT (839),
     consumed by the MATCH_ACCEPT handling in the assertion resume code
     (RM3 5520-5528; RM5 is M5). A frame index here (-1 = NULL); like
     [branch_end], a match()-local that persists across RMATCH, living in
     [st] and reset at every match() entry. *)
  st.assert_accept_frame <- -1;

  (* pcre2_match.c:550-556 + 662-773 + 790-798 + 6462-6501 — the goto
     graph as four mutually tail-recursive functions over the frame index
     [f] (the C's F pointer). All state lives in the arena slots and in
     [mb]/[match_data]; the loop body allocates nothing. *)
  let rec rmatch (st : match_state) (f : int) (ra : int) (rb : int)
      (group_frame_type : int) : int =
    let a = st.arena in
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
    if n < 0 then n else (new_frame [@tailcall]) st n ra group_frame_type
  and new_frame (st : match_state) (f : int) (ecode : int)
      (group_frame_type : int) : int =
    let mb = st.mb in
    let a = st.arena in
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
    else (dispatch [@tailcall]) st f
  and dispatch (st : match_state) (f : int) : int =
    let mb = st.mb in
    let a = st.arena in
    let utf = st.utf in
    let ucp = st.ucp in
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:790-798 — the main processing loop: Fop =
       (uint8_t)( *Fecode); switch (Fop). Arms that `break` in the C set
       slot_ecode and tail-call [dispatch]; RRETURN(x) is
       (backtrack [@tailcall]) st f x. Reading the code unit is in bounds:
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
        (* OP_CLOSE (pcre2_match.c:800-829) — before OP_ACCEPT there may
           be any number of OP_CLOSE opcodes, to close any currently open
           capturing brackets. Unlike reaching the end of a group, we have
           to search back for the relevant frame in case other types of
           group that use chained frames have intervened (multiple
           OP_CLOSEs always come innermost first, matching the chain
           order). Ignored in a recursion, because captures are not
           passed out of recursions. *)
        if Int.equal fr.(fb + Frames.slot_current_recurse) Frames.recurse_unset
        then (
          let number = Compile.get2 mb.start_code (ecode + 1) in
          let p =
            close_frame_scan st
              fr.(fb + Frames.slot_last_group_offset)
              (Frames.gf_capture lor number)
          in
          if p < 0 then p (* PCRE2_ERROR_INTERNAL: return, NOT RRETURN (816) *)
          else
            (* pcre2_match.c:822-826 — record the capture. In bounds:
               1 <= number <= top_bracket in a compiled program (mb
               invariant), so offset + 1 <= 2 * top_bracket - 1, within
               the frame's ovector region. *)
            let offset = (number lsl 1) - 2 in
            fr.(fb + Frames.slot_capture_last) <- number;
            fr.(fb + Frames.slot_ovector + offset) <-
              fr.(Frames.base a p + Frames.slot_eptr) - mb.start_subject;
            fr.(fb + Frames.slot_ovector + offset + 1) <-
              fr.(fb + Frames.slot_eptr) - mb.start_subject;
            if offset >= fr.(fb + Frames.slot_offset_top) then
              fr.(fb + Frames.slot_offset_top) <- offset + 2;
            (* pcre2_match.c:828-829 *)
            fr.(fb + Frames.slot_ecode) <- ecode + Opcodes.op_lengths.(op);
            (dispatch [@tailcall]) st f)
        else (
          (* pcre2_match.c:828-829 *)
          fr.(fb + Frames.slot_ecode) <- ecode + Opcodes.op_lengths.(op);
          (dispatch [@tailcall]) st f)
    | 165 ->
        (* OP_ASSERT_ACCEPT (pcre2_match.c:832-840) — real or forced end
           of the pattern, assertion, or recursion: in an assertion ACCEPT,
           update the last used pointer and remember the current frame so
           that the captures and mark can be fished out of it. *)
        if fr.(fb + Frames.slot_eptr) > mb.last_used_ptr then
          mb.last_used_ptr <- fr.(fb + Frames.slot_eptr);
        st.assert_accept_frame <- f;
        (backtrack [@tailcall]) st f match_accept
    | 164 ->
        (* OP_ACCEPT (pcre2_match.c:842-874) — for ACCEPT within a
           recursion, we have to find the most recent recursion. If not
           in a recursion, fall through to code that is common with
           OP_END. *)
        if
          not
            (Int.equal
               fr.(fb + Frames.slot_current_recurse)
               Frames.recurse_unset)
        then (
          let p =
            accept_frame_scan st fr.(fb + Frames.slot_last_group_offset)
          in
          if p < 0 then p (* PCRE2_ERROR_INTERNAL: return, NOT RRETURN (855) *)
          else
            (* pcre2_match.c:862-872 — [p] (the C's P) is the frame that
               dispatched OP_RECURSE (the frame before the GF_RECURSE
               frame): go back there, copying the current subject
               position and mark, and the start_match position (\K might
               have changed it), and then move on past the OP_RECURSE
               (P->ecode still points at it — RMATCH stored the branch
               start in the NEW frame only). *)
            let pb = Frames.base a p in
            fr.(pb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr);
            fr.(pb + Frames.slot_mark) <- fr.(fb + Frames.slot_mark);
            fr.(pb + Frames.slot_start_match) <-
              fr.(fb + Frames.slot_start_match);
            fr.(pb + Frames.slot_ecode) <-
              fr.(pb + Frames.slot_ecode) + 1 + Limits.link_size;
            (dispatch [@tailcall]) st p)
        else
          (* fallthrough to OP_END in C (874) *)
          (op_end_tail [@tailcall]) st f
    | 0 ->
        (* OP_END (pcre2_match.c:876-940) — OP_END itself can never be
           reached within a recursion because that is picked up when the
           OP_KET that always precedes OP_END is reached. *)
        (op_end_tail [@tailcall]) st f
    | 12 ->
        (* OP_ANY (pcre2_match.c:947-958) — match any single character
           type except newline; have to take care with CRLF newlines and
           partial matching. Falls through to OP_ALLANY (whose shared
           tail carries the UTF ACROSSCHAR advance). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if is_newline_at st eptr then (backtrack [@tailcall]) st f match_nomatch
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
        (op_allany_tail [@tailcall]) f
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
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (dispatch [@tailcall]) st f)
    | 29 ->
        (* OP_CHAR (pcre2_match.c:992-1025) — match a single character,
           casefully. *)
        if utf then
          (* pcre2_match.c:996-1011 — UTF mode: Flength = 1; Fecode++;
             GETCHARLEN(fc, Fecode, Flength) (only the length is consumed
             — the compare below is unit by unit); then bound-check and
             compare Flength code units. *)
          let c0 = Char.code (Bytes.get mb.start_code (ecode + 1)) in
          let flength = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
          let eptr = fr.(fb + Frames.slot_eptr) in
          if flength > mb.end_subject - eptr then
            (* pcre2_match.c:1002-1006 — CHECK_PARTIAL() (531-535: only
               when Feptr is at or past end_subject — a mid-subject
               too-short tail is a plain NOMATCH), then no match. *)
            let rc =
              if eptr >= mb.end_subject then scheck_partial mb eptr else 0
            in
            if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
          else
            (* pcre2_match.c:1007-1010 — for (; Flength > 0; Flength--)
               if ( *Fecode++ != UCHAR21INC(Feptr)) RRETURN(MATCH_NOMATCH):
               both post-increments advance even when the unit fails, so
               Feptr ends one past the mismatching unit. *)
            (op_char_utf_cmp [@tailcall]) f (ecode + 1) eptr flength
        else if
          (* pcre2_match.c:1015-1021 — not UTF mode. *)
          mb.end_subject - fr.(fb + Frames.slot_eptr) < 1
        then
          (* SCHECK_PARTIAL(), then no match. *)
          let rc = scheck_partial mb fr.(fb + Frames.slot_eptr) in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
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
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 2;
            (dispatch [@tailcall]) st f)
    | 30 ->
        (* OP_CHARI (pcre2_match.c:1028-1104) — match a single character,
           caselessly. If we are at the end of the subject, give up
           immediately. We get here only when the pattern character has at
           most one other case. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          (* pcre2_match.c:1035-1039 — SCHECK_PARTIAL(), then no match. *)
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else if utf then (
          (* pcre2_match.c:1041-1073 — UTF mode: Flength = 1; Fecode++;
             GETCHARLEN(fc, Fecode, Flength). *)
          let c0 = Char.code (Bytes.get mb.start_code (ecode + 1)) in
          let fc =
            if c0 >= 0xc0 then Utf.getutf8_bytes c0 mb.start_code (ecode + 1)
            else c0
          in
          let flength = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
          if fc < 128 then
            (* pcre2_match.c:1048-1059 — the pattern character's other
               case (if any) is also < 128: use the fast lookup table on
               one code unit. We checked above that there is at least one
               character left in the subject. *)
            (* safe: eptr < mb.end_subject <= String.length mb.subject
               (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
            let cc = Char.code (String.unsafe_get mb.subject eptr) in
            if not (Int.equal (Chartables.lcc fc) (Chartables.lcc cc)) then
              (backtrack [@tailcall]) st f match_nomatch
            else (
              fr.(fb + Frames.slot_eptr) <- eptr + 1;
              fr.(fb + Frames.slot_ecode) <- ecode + 2;
              (dispatch [@tailcall]) st f)
          else
            (* pcre2_match.c:1061-1072 — pick up the subject character
               (GETCHARINC(dc, Feptr): Feptr advances even when the test
               fails) and use Unicode property support to test its other
               case. *)
            let s0 = Char.code (String.unsafe_get mb.subject eptr) in
            let dc =
              if s0 >= 0xc0 then Utf.getutf8 s0 mb.subject eptr else s0
            in
            let eptr' =
              if s0 >= 0xc0 then eptr + 1 + Utf.get_extralen s0 else eptr + 1
            in
            fr.(fb + Frames.slot_eptr) <- eptr';
            if (not (Int.equal dc fc)) && not (Int.equal dc (Ucd.othercase fc))
            then (backtrack [@tailcall]) st f match_nomatch
            else (
              (* Fecode += Flength (on top of the initial Fecode++). *)
              fr.(fb + Frames.slot_ecode) <- ecode + 1 + flength;
              (dispatch [@tailcall]) st f))
        else if ucp then
          (* pcre2_match.c:1075-1092 — if UCP is set without UTF we must
             do the same as above, but with one character per code unit.
             Feptr++/Fecode += 2 happen after the tests: nothing advances
             on NOMATCH (unlike the UTF arm's GETCHARINC). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let cc = Char.code (String.unsafe_get mb.subject eptr) in
          let fc = Char.code (Bytes.get mb.start_code (ecode + 1)) in
          if
            if fc < 128 then
              (* pcre2_match.c:1082-1085 — mb->lcc[fc] !=
                 TABLE_GET(cc, mb->lcc, cc). *)
              not (Int.equal (Chartables.lcc fc) (Chartables.lcc cc))
            else
              (* pcre2_match.c:1086-1089 — cc != fc && cc !=
                 UCD_OTHERCASE(fc). *)
              (not (Int.equal cc fc)) && not (Int.equal cc (Ucd.othercase fc))
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            (* pcre2_match.c:1090-1091 *)
            fr.(fb + Frames.slot_eptr) <- eptr + 1;
            fr.(fb + Frames.slot_ecode) <- ecode + 2;
            (dispatch [@tailcall]) st f)
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
            (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_eptr) <- eptr + 1;
            fr.(fb + Frames.slot_ecode) <- ecode + 2;
            (dispatch [@tailcall]) st f)
    | 31 | 32 ->
        (* OP_NOT, OP_NOTI (pcre2_match.c:1107-1174) — match not a single
           character. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          (* pcre2_match.c:1112-1116 — SCHECK_PARTIAL(), then no match. *)
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else if utf then (
          (* pcre2_match.c:1118-1137 — UTF mode: Fecode++;
             GETCHARINC(ch, Fecode); GETCHARINC(fc, Feptr); both advances
             happen before the tests. *)
          let c0 = Char.code (Bytes.get mb.start_code (ecode + 1)) in
          let ch =
            if c0 >= 0xc0 then Utf.getutf8_bytes c0 mb.start_code (ecode + 1)
            else c0
          in
          let ecode' =
            if c0 >= 0xc0 then ecode + 2 + Utf.get_extralen c0 else ecode + 2
          in
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let s0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc = if s0 >= 0xc0 then Utf.getutf8 s0 mb.subject eptr else s0 in
          fr.(fb + Frames.slot_eptr) <-
            (if s0 >= 0xc0 then eptr + 1 + Utf.get_extralen s0 else eptr + 1);
          if Int.equal ch fc then
            (* pcre2_match.c:1126-1128 — caseful match. *)
            (backtrack [@tailcall]) st f match_nomatch
          else if Int.equal op Opcodes.op_noti then
            (* pcre2_match.c:1129-1136 — caseless: fold ch through
               UCD_OTHERCASE (> 127) or the fcc table. *)
            let ch = if ch > 127 then Ucd.othercase ch else Chartables.fcc ch in
            if Int.equal ch fc then (backtrack [@tailcall]) st f match_nomatch
            else (
              fr.(fb + Frames.slot_ecode) <- ecode';
              (dispatch [@tailcall]) st f)
          else (
            fr.(fb + Frames.slot_ecode) <- ecode';
            (dispatch [@tailcall]) st f))
        else if ucp then (
          (* pcre2_match.c:1139-1160 — UCP without UTF is as above, but
             with one character per code unit. *)
          (* fc = UCHAR21INC(Feptr) (1144) — the post-increment advances
             Feptr even when the test fails: the character was consulted,
             and RETURN_SWITCH's last_used_ptr update (6470) must see
             it. *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          let ch = Char.code (Bytes.get mb.start_code (ecode + 1)) in
          if Int.equal ch fc then
            (* pcre2_match.c:1148-1151 — caseful match. *)
            (backtrack [@tailcall]) st f match_nomatch
          else if Int.equal op Opcodes.op_noti then
            (* pcre2_match.c:1152-1159 — caseless: fold ch through
               UCD_OTHERCASE (> 127) or the fcc table. *)
            let ch = if ch > 127 then Ucd.othercase ch else Chartables.fcc ch in
            if Int.equal ch fc then (backtrack [@tailcall]) st f match_nomatch
            else (
              fr.(fb + Frames.slot_ecode) <- ecode + 2;
              (dispatch [@tailcall]) st f)
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 2;
            (dispatch [@tailcall]) st f))
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
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 2;
            (dispatch [@tailcall]) st f)
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
           opcodes differ only for data characters outside 0-255 (in the
           8-bit library only reachable via the UTF decode): OP_NCLASS
           matches them, OP_CLASS fails. Frame temporaries
           (pcre2_match.c:1925-1930): Lmin/Lmax = temp_32[0..1],
           Lstart_eptr = temp_sptr[0], Lbyte_map_address = temp_sptr[1]. *)
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
          if utf then (class_utf_min [@tailcall]) f 1 rep_typ.(idx)
          else (class_min [@tailcall]) f 1 rep_typ.(idx))
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
          if utf then
            (class_utf_min [@tailcall]) f 1 rep_typ.(next - Opcodes.op_crstar)
          else (class_min [@tailcall]) f 1 rep_typ.(next - Opcodes.op_crstar))
        else (
          (* pcre2_match.c:1968-1970 — no repeat follows: Lmin = Lmax =
             1. The C leaves reptype unread (never consulted when Lmin =
             Lmax): 0 is passed, as at OP_EXACT. *)
          fr.(fb + Frames.slot_temp_32_0) <- 1;
          fr.(fb + Frames.slot_temp_32_1) <- 1;
          fr.(fb + Frames.slot_ecode) <- ecode;
          if utf then (class_utf_min [@tailcall]) f 1 0
          else (class_min [@tailcall]) f 1 0)
    | 112 ->
        (* OP_XCLASS (pcre2_match.c:2175-2216) — match an extended
           character class (in the 8-bit library, encountered only when
           UTF-8 mode is supported). Frame temporaries
           (pcre2_match.c:2166-2169): Lstart_eptr = temp_sptr[0],
           Lxclass_data = temp_sptr[1], Lmin/Lmax = temp_32[0..1]. *)
        fr.(fb + Frames.slot_temp_sptr_1) <- ecode + 1 + Limits.link_size
        (* Lxclass_data: the flag code unit (2178) *);
        (* pcre2_match.c:2179 — advance past the item. *)
        let ecode = ecode + Compile.get mb.start_code (ecode + 1) in
        (* pcre2_match.c:2181-2216 — repeat information, as for
           OP_CLASS. *)
        let next = Char.code (Bytes.get mb.start_code ecode) in
        if
          (next >= Opcodes.op_crstar && next <= Opcodes.op_crminquery)
          || (next >= Opcodes.op_crposstar && next <= Opcodes.op_crposquery)
        then (
          (* pcre2_match.c:2183-2196 *)
          let idx = next - Opcodes.op_crstar in
          fr.(fb + Frames.slot_temp_32_0) <- rep_min.(idx) (* Lmin *);
          fr.(fb + Frames.slot_temp_32_1) <- rep_max.(idx) (* Lmax *);
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (xclass_min [@tailcall]) f 1 rep_typ.(idx))
        else if
          Int.equal next Opcodes.op_crrange
          || Int.equal next Opcodes.op_crminrange
          || Int.equal next Opcodes.op_crposrange
        then (
          (* pcre2_match.c:2198-2206 *)
          let lmax =
            Compile.get2 mb.start_code (ecode + 1 + Limits.imm2_size)
          in
          fr.(fb + Frames.slot_temp_32_0) <-
            Compile.get2 mb.start_code (ecode + 1);
          fr.(fb + Frames.slot_temp_32_1) <-
            (if Int.equal lmax 0 then uint32_max else lmax);
          fr.(fb + Frames.slot_ecode) <- ecode + 1 + (2 * Limits.imm2_size);
          (xclass_min [@tailcall]) f 1 rep_typ.(next - Opcodes.op_crstar))
        else (
          (* pcre2_match.c:2208-2210 — no repeat follows. *)
          fr.(fb + Frames.slot_temp_32_0) <- 1;
          fr.(fb + Frames.slot_temp_32_1) <- 1;
          fr.(fb + Frames.slot_ecode) <- ecode;
          (xclass_min [@tailcall]) f 1 0)
    | 6 ->
        (* OP_NOT_DIGIT (pcre2_match.c:2305-2315) — match various
           character types when PCRE2_UCP is not set (2298-2303). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr) (pcre2_intmodedep.h:319-324): the
             advance happens even when the test fails (the character was
             consulted); CHMAX_255(fc) is fc <= 255 with Unicode support
             (pcre2_intmodedep.h:212-219). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          if
            fc <= 255
            && not
                 (Int.equal
                    (Chartables.ctypes fc land Chartables.ctype_digit)
                    0)
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) st f)
    | 7 ->
        (* OP_DIGIT (pcre2_match.c:2317-2327). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr); !CHMAX_255(fc) fails the positive
             type (pcre2_match.c:2324-2325). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          if
            fc > 255
            || Int.equal (Chartables.ctypes fc land Chartables.ctype_digit) 0
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) st f)
    | 8 ->
        (* OP_NOT_WHITESPACE (pcre2_match.c:2329-2339). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr) + CHMAX_255 guard
             (pcre2_match.c:2335-2337). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          if
            fc <= 255
            && not
                 (Int.equal
                    (Chartables.ctypes fc land Chartables.ctype_space)
                    0)
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) st f)
    | 9 ->
        (* OP_WHITESPACE (pcre2_match.c:2341-2351). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr); !CHMAX_255(fc) fails
             (pcre2_match.c:2347-2349). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          if
            fc > 255
            || Int.equal (Chartables.ctypes fc land Chartables.ctype_space) 0
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) st f)
    | 10 ->
        (* OP_NOT_WORDCHAR (pcre2_match.c:2353-2363). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr) + CHMAX_255 guard
             (pcre2_match.c:2359-2361). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          if
            fc <= 255
            && not
                 (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0)
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) st f)
    | 11 ->
        (* OP_WORDCHAR (pcre2_match.c:2365-2375). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr); !CHMAX_255(fc) fails
             (pcre2_match.c:2371-2373). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          if
            fc > 255
            || Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) st f)
    | 17 ->
        (* OP_ANYNL (pcre2_match.c:2377-2409) — match \R, any newline
           sequence. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr) (pcre2_match.c:2383). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          let eptr =
            if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
            else eptr + 1
          in
          fr.(fb + Frames.slot_eptr) <- eptr;
          if Int.equal fc Newline.char_cr then
            (* pcre2_match.c:2388-2393 — CR: absorb a following LF; a CR
               at the very end of the subject could be partial. *)
            if eptr >= mb.end_subject then
              let rc = scheck_partial mb eptr in
              if rc < 0 then rc
              else (
                fr.(fb + Frames.slot_ecode) <- ecode + 1;
                (dispatch [@tailcall]) st f)
            else (
              if
                Int.equal
                  (* safe: eptr < mb.end_subject (checked above) *)
                  (Char.code (String.unsafe_get mb.subject eptr))
                  Newline.char_lf
              then fr.(fb + Frames.slot_eptr) <- eptr + 1;
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) st f)
          else if Int.equal fc Newline.char_lf then (
            (* pcre2_match.c:2395-2396 *)
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) st f)
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
              (backtrack [@tailcall]) st f match_nomatch
            else (
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) st f)
          else (backtrack [@tailcall]) st f match_nomatch (* 2386 — default *)
    | 18 -> (
        (* OP_NOT_HSPACE (pcre2_match.c:2412-2425). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr) (pcre2_match.c:2418). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          (* HSPACE_CASES (pcre2_internal.h:429-431): byte and multibyte
             cases. *)
          match fc with
          | 0x09 | 0x20 | 0xa0 | 0x1680 | 0x180e | 0x2000 | 0x2001 | 0x2002
          | 0x2003 | 0x2004 | 0x2005 | 0x2006 | 0x2007 | 0x2008 | 0x2009
          | 0x200a | 0x202f | 0x205f | 0x3000 ->
              (backtrack [@tailcall]) st f match_nomatch
          | _ ->
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) st f)
    | 19 -> (
        (* OP_HSPACE (pcre2_match.c:2427-2440). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr) (pcre2_match.c:2433). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          match fc with
          | 0x09 | 0x20 | 0xa0 | 0x1680 | 0x180e | 0x2000 | 0x2001 | 0x2002
          | 0x2003 | 0x2004 | 0x2005 | 0x2006 | 0x2007 | 0x2008 | 0x2009
          | 0x200a | 0x202f | 0x205f | 0x3000 ->
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) st f
          | _ -> (backtrack [@tailcall]) st f match_nomatch)
    | 20 -> (
        (* OP_NOT_VSPACE (pcre2_match.c:2442-2455). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr) (pcre2_match.c:2448). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          (* VSPACE_CASES (pcre2_internal.h:447-449): LF, VT, FF, CR, NEL
             and the multibyte 0x2028/0x2029. *)
          match fc with
          | 0x0a | 0x0b | 0x0c | 0x0d | 0x85 | 0x2028 | 0x2029 ->
              (backtrack [@tailcall]) st f match_nomatch
          | _ ->
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) st f)
    | 21 -> (
        (* OP_VSPACE (pcre2_match.c:2457-2470). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr) (pcre2_match.c:2463). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          match fc with
          | 0x0a | 0x0b | 0x0c | 0x0d | 0x85 | 0x2028 | 0x2029 ->
              fr.(fb + Frames.slot_ecode) <- ecode + 1;
              (dispatch [@tailcall]) st f
          | _ -> (backtrack [@tailcall]) st f match_nomatch)
    | 16 | 15 ->
        (* OP_PROP, OP_NOTPROP (pcre2_match.c:2479-2614) — check the next
           character by Unicode property. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          (* pcre2_match.c:2481-2485 — SCHECK_PARTIAL(), then no match. *)
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* GETCHARINCTEST(fc, Feptr) (2486). *)
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          (* pcre2_match.c:2488-2610 — BOOL notmatch = Fop == OP_NOTPROP;
             switch(Fecode[1]) with value Fecode[2], via [prop_test]; a
             property type above PT_BOOL is the switch default:
             PCRE2_ERROR_INTERNAL (2606-2609, direct return, NOT
             RRETURN). *)
          let ptype = Char.code (Bytes.get mb.start_code (ecode + 1)) in
          if ptype > Opcodes.pt_bool then Errors.error_internal
          else if
            Bool.equal
              (prop_test fc ptype
                 (Char.code (Bytes.get mb.start_code (ecode + 2))))
              (Int.equal op Opcodes.op_notprop)
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            (* Fecode += 3 (2612). *)
            fr.(fb + Frames.slot_ecode) <- ecode + 3;
            (dispatch [@tailcall]) st f)
    | 22 ->
        (* OP_EXTUNI (pcre2_match.c:2617-2635) — match an extended
           Unicode sequence. We will get here only if the support is in
           the binary (always, in this port). *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          (* pcre2_match.c:2622-2626 — SCHECK_PARTIAL(), then no match. *)
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* pcre2_match.c:2627-2632 — GETCHARINCTEST(fc, Feptr); Feptr =
             PRIV(extuni)(...). *)
          let eptr' = extuni_step st eptr in
          fr.(fb + Frames.slot_eptr) <- eptr';
          (* CHECK_PARTIAL() (2633). *)
          let rc =
            if eptr' >= mb.end_subject then scheck_partial mb eptr' else 0
          in
          if rc < 0 then rc
          else (
            (* Fecode++ (2634). *)
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) st f)
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
        fr.(fb + Frames.slot_temp_32_2) <- (if caseless then 1 else 0)
        (* Lcaseless, 4996 *);
        (* pcre2_match.c:4998-5000 *)
        let count = Compile.get2 mb.start_code (ecode + 1 + Limits.imm2_size) in
        let slot =
          Compile.get2 mb.start_code (ecode + 1) * mb.name_entry_size
        in
        (* pcre2_match.c:5002-5007 — while (count-- > 0): Loffset for
           the first set group, or the last examined entry when none is
           set. The C writes Loffset every iteration; [dnref_scan]
           returns the same final value, written once. count >= 1 in a
           compiled program (the reference names at least one group). *)
        fr.(fb + Frames.slot_temp_size) <- dnref_scan f count slot (* Loffset *);
        (* goto REF_REPEAT (5009) with Fecode past the item (5000). *)
        (ref_repeat [@tailcall]) f (ecode + 1 + (2 * Limits.imm2_size))
    | 113 | 114 ->
        (* OP_REF, OP_REFI (pcre2_match.c:5011-5015) — match a back
           reference, possibly repeatedly: a reference to a numbered
           group or a non-duplicated named group. Frame temporaries as at
           OP_DNREF (pcre2_match.c:4988-4992). *)
        let caseless = Int.equal op Opcodes.op_refi in
        fr.(fb + Frames.slot_temp_32_2) <- (if caseless then 1 else 0)
        (* Lcaseless, 5013 *);
        fr.(fb + Frames.slot_temp_size) <-
          (Compile.get2 mb.start_code (ecode + 1) lsl 1) - 2
        (* Loffset, 5014 *);
        (ref_repeat [@tailcall]) f (ecode + 1 + Limits.imm2_size)
    | 151 ->
        (* OP_BRAZERO (pcre2_match.c:5224-5230) — a possibly-zero repeat
           of a bracket group: try to take the group first. Lnext_ecode =
           temp_sptr[0] (5222) holds the bracket position for the RM9
           resume, which skips to after the group on NOMATCH. *)
        fr.(fb + Frames.slot_temp_sptr_0) <- ecode + 1 (* Lnext_ecode *);
        (rmatch [@tailcall]) st f (ecode + 1) rm9 0
    | 152 ->
        (* OP_BRAMINZERO (pcre2_match.c:5232-5238) — the minimizing form:
           try the rest of the pattern WITHOUT the group first. Lnext_ecode
           walks from the bracket to its final ket before the RMATCH. *)
        let next = skip_alts (ecode + 1) in
        fr.(fb + Frames.slot_temp_sptr_0) <- next (* Lnext_ecode, post-walk *);
        (rmatch [@tailcall]) st f (next + 1 + Limits.link_size) rm10 0
    | 167 ->
        (* OP_SKIPZERO (pcre2_match.c:5242-5246) — a group with a {0}
           quantifier: skip it entirely. Fecode++; walk to the final ket;
           Fecode += 1 + LINK_SIZE. *)
        fr.(fb + Frames.slot_ecode) <-
          skip_alts (ecode + 1) + 1 + Limits.link_size;
        (dispatch [@tailcall]) st f
    | 153 ->
        (* OP_BRAPOSZERO (pcre2_match.c:5260-5265) — a possessive group
           with a zero repeat allowed: step onto the bracket, then enter
           the possessive protocol via the capture / non-capture heads.
           Frame temporaries (pcre2_match.c:5254-5258): Lframe_type /
           Lmatched_once / Lzero_allowed = temp_32[0..2], Lstart_eptr =
           temp_sptr[0], Lstart_group = temp_sptr[1]. *)
        fr.(fb + Frames.slot_temp_32_2) <- 1 (* Lzero_allowed = TRUE *);
        let ecode = ecode + 1 in
        fr.(fb + Frames.slot_ecode) <- ecode;
        let next = Char.code (Bytes.get mb.start_code ecode) in
        if
          Int.equal next Opcodes.op_cbrapos
          || Int.equal next Opcodes.op_scbrapos
        then (
          (* goto POSSESSIVE_CAPTURE (pcre2_match.c:5279-5281): number =
             GET2(Fecode, 1+LINK_SIZE); Lframe_type = GF_CAPTURE |
             number. *)
          fr.(fb + Frames.slot_temp_32_0) <-
            Frames.gf_capture
            lor Compile.get2 mb.start_code (ecode + 1 + Limits.link_size);
          (possessive_group [@tailcall]) f)
        else (
          (* goto POSSESSIVE_NON_CAPTURE (pcre2_match.c:5271-5272). *)
          fr.(fb + Frames.slot_temp_32_0) <- Frames.gf_nocapture;
          (possessive_group [@tailcall]) f)
    | 136 | 141 ->
        (* OP_BRAPOS, OP_SBRAPOS (pcre2_match.c:5267-5273) — possessive
           brackets with an unlimited repeat, non-capturing: their end is
           always OP_KETRPOS, which returns MATCH_KETRPOS without going
           further in the pattern. *)
        fr.(fb + Frames.slot_temp_32_2) <- 0 (* Lzero_allowed = FALSE *);
        (* POSSESSIVE_NON_CAPTURE (pcre2_match.c:5271-5272). *)
        fr.(fb + Frames.slot_temp_32_0) <- Frames.gf_nocapture
        (* Lframe_type *);
        (possessive_group [@tailcall]) f
    | 138 | 143 ->
        (* OP_CBRAPOS, OP_SCBRAPOS (pcre2_match.c:5275-5281) — the
           capturing possessive brackets. *)
        fr.(fb + Frames.slot_temp_32_2) <- 0 (* Lzero_allowed = FALSE *);
        (* POSSESSIVE_CAPTURE (pcre2_match.c:5279-5281). *)
        fr.(fb + Frames.slot_temp_32_0) <-
          Frames.gf_capture
          lor Compile.get2 mb.start_code (ecode + 1 + Limits.link_size);
        (possessive_group [@tailcall]) f
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
           chained frame: Lframe_type = GF_NOCAPTURE | Fop. OP_ONCE and
           OP_SCRIPT_RUN differ only in their ket actions. *)
        fr.(fb + Frames.slot_temp_32_0) <- Frames.gf_nocapture lor op;
        (grouploop [@tailcall]) f
    | 117 ->
        (* OP_RECURSE (pcre2_match.c:5417-5497) — pattern recursion either
           matches the current regex, or some subexpression. The offset
           data is the offset to the starting bracket from the start of
           the whole pattern, so that it works from duplicated
           subpatterns. For a whole-pattern recursion, we have to infer
           the number zero. Frame temporaries (pcre2_match.c:5424-5425):
           Lframe_type = temp_32[0], Lstart_branch = temp_sptr[0]. *)
        let bracode = Compile.get mb.start_code (ecode + 1) in
        let number =
          if Int.equal bracode 0 then 0
          else Compile.get2 mb.start_code (bracode + 1 + Limits.link_size)
        in
        (* pcre2_match.c:5431-5454 — if we are already in a pattern
           recursion, check for repeating the same one without changing
           the subject pointer or the last referenced character in the
           subject. This should catch convoluted mutual recursions; some
           simple cases are caught at compile time. However, there are
           rare cases when this check needs to be turned off; actual
           recursion loops are then caught by the match or heap limits. *)
        let rc =
          if
            not
              (Int.equal
                 fr.(fb + Frames.slot_current_recurse)
                 Frames.recurse_unset)
          then
            recurse_loop_check f fr.(fb + Frames.slot_last_group_offset) number
          else 0
        in
        if rc < 0 then rc (* direct return, NOT RRETURN (5449) *)
        else (
          (* pcre2_match.c:5456-5461 — remember the current last
             referenced character and then run the recursion branch by
             branch. *)
          fr.(fb + Frames.slot_recurse_last_used) <- mb.last_used_ptr;
          fr.(fb + Frames.slot_temp_sptr_0) <- bracode (* Lstart_branch *);
          fr.(fb + Frames.slot_temp_32_0) <- Frames.gf_recurse lor number
          (* Lframe_type *);
          (recurse_loop [@tailcall]) f)
    | 127 | 129 | 131 | 132 ->
        (* OP_ASSERT, OP_ASSERTBACK, OP_ASSERT_NA, OP_ASSERTBACK_NA
           (pcre2_match.c:5511-5536) — positive assertions: loop over the
           branches, recording a backtracking point for each (RM3). A
           branch that matches runs forward past the assertion's ket (its
           ket action restores Feptr and, for the atomic kinds, discards
           the intermediate backtracking points); only failure comes back
           here. Lframe_type = temp_32[0] (5509). *)
        fr.(fb + Frames.slot_temp_32_0) <- Frames.gf_nocapture lor op
        (* Lframe_type *);
        (assert_loop [@tailcall]) f
    | 128 | 130 ->
        (* OP_ASSERT_NOT, OP_ASSERTBACK_NOT (pcre2_match.c:5547-5584) —
           negative assertions: loop for each non-matching branch as for
           positive assertions (RM4); a matching branch fails the
           assertion (rrc inverted at the resume). Lframe_type =
           temp_32[0] (5545). *)
        fr.(fb + Frames.slot_temp_32_0) <- Frames.gf_nocapture lor op
        (* Lframe_type *);
        (assert_not_loop [@tailcall]) f
    | 118 | 119 ->
        (* OP_CALLOUT, OP_CALLOUT_STR (pcre2_match.c:5590-5600) — the
           callout item calls an external function, if one is provided,
           passing details of the match so far. This is mainly for
           debugging, though the function is able to force a failure. No
           callout function can be installed in this port, so rrc =
           do_callout(F, mb, &length) is always 0 (pcre2_match.c:283): the
           rrc > 0 RRETURN(MATCH_NOMATCH) and rrc < 0 RRETURN(rrc) exits
           (5597-5598) are unreachable. *)
        fr.(fb + Frames.slot_ecode) <- ecode + do_callout_length mb ecode
        (* Fecode += length (5599) *);
        (dispatch [@tailcall]) st f
    | 139 | 144 ->
        (* OP_COND, OP_SCOND (pcre2_match.c:5603-5778) — conditional
           group: compilation checked that there are no more than two
           branches. If the condition is false, skipping the first branch
           takes us past the end of the item if there is only one branch,
           but that's exactly what we want. *)
        (* pcre2_match.c:5612-5621 — Flength (frame slot: it must survive
           the assertion-condition RMATCH) will be added to Fecode when
           the condition is false, to get to the second branch. Setting
           it to the offset to the ALT or KET, then incrementing Fecode
           achieves this effect. However, if the second branch is
           non-existent, we must point to the KET so that the end of the
           group is correctly processed. We now have Fecode pointing to
           the condition or callout. *)
        let flength = Compile.get mb.start_code (ecode + 1) in
        let flength =
          if
            not
              (Int.equal
                 (Char.code (Bytes.get mb.start_code (ecode + flength)))
                 Opcodes.op_alt)
          then flength - (1 + Limits.link_size)
          else flength
        in
        fr.(fb + Frames.slot_length) <- flength;
        let ecode = ecode + 1 + Limits.link_size in
        fr.(fb + Frames.slot_ecode) <- ecode;
        (* pcre2_match.c:5623-5638 — because of the way auto-callout works
           during compile, a callout item is inserted between OP_COND and
           an assertion condition. Such a callout can also be inserted
           manually. rrc = do_callout(F, mb, &length) is always 0 here (no
           callout function can be installed, pcre2_match.c:283), so the
           rrc > 0 / rrc < 0 RRETURNs (5630-5631) are unreachable. Advance
           Fecode past the callout, so it now points to the condition; we
           must adjust Flength so that the value of Fecode+Flength is
           unchanged. *)
        let ecode =
          let op0 = Char.code (Bytes.get mb.start_code ecode) in
          if
            Int.equal op0 Opcodes.op_callout
            || Int.equal op0 Opcodes.op_callout_str
          then (
            let length = do_callout_length mb ecode in
            fr.(fb + Frames.slot_ecode) <- ecode + length (* Fecode (5636) *);
            fr.(fb + Frames.slot_length) <-
              fr.(fb + Frames.slot_length) - length
            (* Flength (5637) *);
            ecode + length)
          else ecode
        in
        (* pcre2_match.c:5640-5643 — test the various possible
           conditions: condition = FALSE; switch ( *Fecode ). *)
        let cond_op = Char.code (Bytes.get mb.start_code ecode) in
        if Int.equal cond_op Opcodes.op_rref then
          (* OP_RREF (pcre2_match.c:5645-5651) — group recursion test. *)
          let condition =
            (not
               (Int.equal
                  fr.(fb + Frames.slot_current_recurse)
                  Frames.recurse_unset))
            &&
            let number = Compile.get2 mb.start_code (ecode + 1) in
            Int.equal number Opcodes.rref_any
            || Int.equal number fr.(fb + Frames.slot_current_recurse)
          in
          (cond_choose [@tailcall]) f condition
        else if Int.equal cond_op Opcodes.op_dnrref then
          (* OP_DNRREF (pcre2_match.c:5653-5666) — duplicate named group
             recursion test: scan the list of groups to which the name
             refers for the current recursion number. *)
          let condition =
            (not
               (Int.equal
                  fr.(fb + Frames.slot_current_recurse)
                  Frames.recurse_unset))
            && dnrref_scan
                 fr.(fb + Frames.slot_current_recurse)
                 (Compile.get2 mb.start_code (ecode + 1 + Limits.imm2_size))
                 (Compile.get2 mb.start_code (ecode + 1) * mb.name_entry_size)
          in
          (cond_choose [@tailcall]) f condition
        else if Int.equal cond_op Opcodes.op_cref then
          (* OP_CREF (pcre2_match.c:5668-5671) — numbered group used
             test: offset = doubled ref number. *)
          let offset = (Compile.get2 mb.start_code (ecode + 1) lsl 1) - 2 in
          let condition =
            offset < fr.(fb + Frames.slot_offset_top)
            && not
                 (Int.equal fr.(fb + Frames.slot_ovector + offset) Frames.unset)
          in
          (cond_choose [@tailcall]) f condition
        else if Int.equal cond_op Opcodes.op_dncref then
          (* OP_DNCREF (pcre2_match.c:5673-5685) — duplicate named group
             used test: scan the list of groups to which the name refers
             for one that is set. *)
          let condition =
            dncref_scan f
              (Compile.get2 mb.start_code (ecode + 1 + Limits.imm2_size))
              (Compile.get2 mb.start_code (ecode + 1) * mb.name_entry_size)
          in
          (cond_choose [@tailcall]) f condition
        else if
          Int.equal cond_op Opcodes.op_false
          || Int.equal cond_op Opcodes.op_fail
        then
          (* OP_FALSE, OP_FAIL (pcre2_match.c:5687-5689) — the assertion
             (?!) becomes OP_FAIL; condition stays FALSE. *)
          (cond_choose [@tailcall]) f false
        else if Int.equal cond_op Opcodes.op_true then
          (* OP_TRUE (pcre2_match.c:5691-5693). *)
          (cond_choose [@tailcall]) f true
        else (
          (* default (pcre2_match.c:5694-5757) — the condition is an
             assertion: run code similar to the assertion code above.
             Lpositive = temp_32[0], Lstart_branch = temp_sptr[0]
             (5698-5699). Fecode stays on the condition opcode for the
             whole branch loop. *)
          fr.(fb + Frames.slot_temp_32_0) <-
            (if
               Int.equal cond_op Opcodes.op_assert
               || Int.equal cond_op Opcodes.op_assertback
             then 1
             else 0)
          (* Lpositive *);
          fr.(fb + Frames.slot_temp_sptr_0) <- ecode (* Lstart_branch *);
          (cond_assert_loop [@tailcall]) f)
    | 125 ->
        (* OP_REVERSE (pcre2_match.c:5793-5819) — move the subject pointer
           back by one fixed amount, at the start of each fixed-length
           branch of a lookbehind assertion. If we are too close to the
           start to move back, fail. *)
        let number = Compile.get2 mb.start_code (ecode + 1) in
        if utf then
          (* pcre2_match.c:5795-5804 — UTF: move back [number]
             CHARACTERS, stepping with BACKCHAR, failing at
             mb->check_subject (lookbehinds may go back no further than
             the UTF-checked region). *)
          (op_reverse_utf_loop [@tailcall]) f number
        else if
          (* pcre2_match.c:5808-5813 — no UTF support, or not in UTF mode:
             count is code unit count. *)
          number > fr.(fb + Frames.slot_eptr) - mb.start_subject
        then (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - number;
          (op_reverse_tail [@tailcall]) f)
    | 126 ->
        (* OP_VREVERSE (pcre2_match.c:5834-5883) — move the subject
           pointer back by a variable amount, at the start of each
           variable-length branch of a lookbehind assertion; a loop tries
           matching the branch after moving back different numbers of
           characters. Frame temporaries (5830-5832): Lmin/Lmax =
           temp_32[0..1], Leptr = temp_sptr[0]. *)
        fr.(fb + Frames.slot_temp_32_0) <- Compile.get2 mb.start_code (ecode + 1)
        (* Lmin *);
        fr.(fb + Frames.slot_temp_32_1) <-
          Compile.get2 mb.start_code (ecode + 1 + Limits.imm2_size)
        (* Lmax *);
        fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr)
        (* Leptr *);
        (* pcre2_match.c:5839-5841 — move back by the maximum branch
           length and then work forwards; this ensures that items such as
           \d{3,5} get the maximum length, which is relevant for captures,
           and makes for Perl compatibility. *)
        if utf then
          (* pcre2_match.c:5843-5857 — UTF: step back by characters with
             BACKCHAR, stopping at the subject start (the BACKCHAR walk
             itself is bounded there — see the DEVIATION note on
             [op_vreverse_utf_loop]); NOMATCH when even the minimum
             cannot be reached, and Lmax is clamped to the characters
             actually available. *)
          (op_vreverse_utf_loop [@tailcall]) f 0
            (ecode + 1 + (2 * Limits.imm2_size))
        else
          (* pcre2_match.c:5861-5869 — no UTF support or not in UTF
             mode. *)
          let diff = fr.(fb + Frames.slot_eptr) - mb.start_subject in
          let available =
            if diff > 65535 then 65535 else if diff > 0 then diff else 0
          in
          if fr.(fb + Frames.slot_temp_32_0) > available then
            (backtrack [@tailcall]) st f match_nomatch
          else (
            if fr.(fb + Frames.slot_temp_32_1) > available then
              fr.(fb + Frames.slot_temp_32_1) <- available;
            fr.(fb + Frames.slot_eptr) <-
              fr.(fb + Frames.slot_eptr) - fr.(fb + Frames.slot_temp_32_1);
            (* pcre2_match.c:5871-5876 — now try matching, moving forward
               one character on failure, until we reach the minimum back
               length: the for(;;) starts with RMATCH(Fecode + 1 +
               2*IMM2_SIZE, RM37); the rest of the loop body is the RM37
               resume arm in [backtrack]. *)
            (rmatch [@tailcall]) st f
              (ecode + 1 + (2 * Limits.imm2_size))
              rm37 0)
    | 120 ->
        (* OP_ALT (pcre2_match.c:5894-5897) — an alternation is the end of
           a branch: record it in branch_end, then scan along to the end
           of the bracketed group. *)
        st.branch_end <- ecode;
        fr.(fb + Frames.slot_ecode) <- skip_alts ecode;
        (dispatch [@tailcall]) st f
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
           ends here. branch_start is consumed by the OP_ASSERTBACK*
           VREVERSE end-point checks below (5995, 6009, 6038). *)
        let branch_start =
          ket_branch_start bracode
            (if Int.equal st.branch_end (-1) then ecode else st.branch_end)
        in
        st.branch_end <- -1;
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
        then (
          (* pcre2_match.c:5934-5948 — the end of an assertion that is a
             condition: return a match, discarding any intermediate
             backtracking points. Copy back the mark setting and the
             captures into the frame before N (= P, the frame that
             dispatched OP_COND and owns the RM5 resume) so that they are
             set on return. Doing this for all assertions, both positive
             and negative, seems to match what Perl does. Fback_frame =
             F - P (frame units here, frames.ml DEVIATION). *)
          let pb = Frames.base a p in
          Array.blit fr (fb + Frames.slot_ovector) fr (pb + Frames.slot_ovector)
            fr.(fb + Frames.slot_offset_top);
          fr.(pb + Frames.slot_offset_top) <- fr.(fb + Frames.slot_offset_top);
          fr.(pb + Frames.slot_mark) <- fr.(fb + Frames.slot_mark);
          fr.(fb + Frames.slot_back_frame) <- f - p;
          (backtrack [@tailcall]) st f match_match)
        else
          (* pcre2_match.c:5952-6085 — actions relating to the starting
             opcode: switch ( *bracode). *)
          match bra_op with
          | 135 ->
              (* OP_BRA (pcre2_match.c:5955-5966) — whole pattern
                 recursion is handled as a recursion into group 0, but
                 the entire pattern is wrapped in OP_BRA/OP_KET rather
                 than a capturing group, so the end of such a recursion
                 must be handled here. It is detected by checking for an
                 immediately following OP_END when we are recursing in
                 group 0. If this is not the end of a whole-pattern
                 recursion, there is nothing to be done. *)
              if
                Int.equal fr.(fb + Frames.slot_current_recurse) 0
                && Int.equal
                     (Char.code
                        (Bytes.get mb.start_code (ecode + 1 + Limits.link_size)))
                     Opcodes.op_end
              then (
                (* pcre2_match.c:5967-5973 — it is the end of
                   whole-pattern recursion: point N to the most recent
                   group frame (the GF_RECURSE frame recorded by
                   OP_RECURSE's RM11 RMATCH) and P to its predecessor
                   (the frame that dispatched OP_RECURSE), and unchain
                   it. The C's PCRE2_ERROR_INTERNAL is a direct return,
                   NOT RRETURN (5970). *)
                let offset = fr.(fb + Frames.slot_last_group_offset) in
                if Int.equal offset Frames.unset then Errors.error_internal
                else
                  let pb = Frames.base a (offset - 1) in
                  fr.(fb + Frames.slot_last_group_offset) <-
                    fr.(pb + Frames.slot_last_group_offset);
                  (* pcre2_match.c:5975-5984 — reinstate the previous set
                     of captures and then carry on after the recursion
                     call. In bounds: offset_top <= 2 * top_bracket in
                     every frame. *)
                  Array.blit fr (pb + Frames.slot_ovector) fr
                    (fb + Frames.slot_ovector)
                    fr.(fb + Frames.slot_offset_top);
                  fr.(fb + Frames.slot_offset_top) <-
                    fr.(pb + Frames.slot_offset_top);
                  fr.(fb + Frames.slot_capture_last) <-
                    fr.(pb + Frames.slot_capture_last);
                  fr.(fb + Frames.slot_current_recurse) <-
                    fr.(pb + Frames.slot_current_recurse);
                  (* P->ecode is the OP_RECURSE position (RMATCH stored
                     the branch start in the NEW frame only); step past
                     the 1 + LINK_SIZE item and continue with the next
                     opcode. *)
                  fr.(fb + Frames.slot_ecode) <-
                    fr.(pb + Frames.slot_ecode) + 1 + Limits.link_size;
                  (dispatch [@tailcall]) st f)
              else (op_ket_tail [@tailcall]) f p bracode
          | 139 | 144 ->
              (* OP_COND, OP_SCOND (pcre2_match.c:5986-5988) — no need to
                 do anything for these. *)
              (op_ket_tail [@tailcall]) f p bracode
          | 132 ->
              (* OP_ASSERTBACK_NA (pcre2_match.c:5994-5997) — non-atomic
                 positive assertions are like OP_BRA, except that the
                 subject pointer must be put back to where it was at the
                 start of the assertion. For a variable lookbehind, check
                 its end point. *)
              if
                Int.equal
                  (Char.code
                     (Bytes.get mb.start_code
                        (branch_start + 1 + Limits.link_size)))
                  Opcodes.op_vreverse
                && not
                     (Int.equal
                        fr.(fb + Frames.slot_eptr)
                        fr.(Frames.base a p + Frames.slot_eptr))
              then (backtrack [@tailcall]) st f match_nomatch
              else (op_ket_assert_na [@tailcall]) f p bracode
                (* fallthrough from OP_ASSERTBACK_NA in C *)
          | 131 ->
              (* OP_ASSERT_NA (pcre2_match.c:5999-6002). *)
              (op_ket_assert_na [@tailcall]) f p bracode
          | 129 ->
              (* OP_ASSERTBACK (pcre2_match.c:6008-6011) — atomic positive
                 assertions are like OP_ONCE, except that in addition the
                 subject pointer must be put back to where it was at the
                 start of the assertion. For a variable lookbehind, check
                 its end point. *)
              if
                Int.equal
                  (Char.code
                     (Bytes.get mb.start_code
                        (branch_start + 1 + Limits.link_size)))
                  Opcodes.op_vreverse
                && not
                     (Int.equal
                        fr.(fb + Frames.slot_eptr)
                        fr.(Frames.base a p + Frames.slot_eptr))
              then (backtrack [@tailcall]) st f match_nomatch
              else (op_ket_assert [@tailcall]) f p bracode
                (* fallthrough from OP_ASSERTBACK in C *)
          | 127 ->
              (* OP_ASSERT (pcre2_match.c:6013-6016). *)
              (op_ket_assert [@tailcall]) f p bracode
          | 133 ->
              (* OP_ONCE (pcre2_match.c:6023-6031). *)
              (op_ket_once [@tailcall]) f p bracode
          | 130 ->
              (* OP_ASSERTBACK_NOT (pcre2_match.c:6037-6040) — a matching
                 negative assertion returns MATCH, which is turned into
                 NOMATCH at the assertion level (the RM4 resume). For a
                 variable lookbehind, check its end point. *)
              if
                Int.equal
                  (Char.code
                     (Bytes.get mb.start_code
                        (branch_start + 1 + Limits.link_size)))
                  Opcodes.op_vreverse
                && not
                     (Int.equal
                        fr.(fb + Frames.slot_eptr)
                        fr.(Frames.base a p + Frames.slot_eptr))
              then (backtrack [@tailcall]) st f match_nomatch
              else
                (* fallthrough from OP_ASSERTBACK_NOT in C *)
                (backtrack [@tailcall]) st f match_match
          | 128 ->
              (* OP_ASSERT_NOT (pcre2_match.c:6042-6043) —
                 RRETURN(MATCH_MATCH). *)
              (backtrack [@tailcall]) st f match_match
          | 134 ->
              (* OP_SCRIPT_RUN (pcre2_match.c:6045-6051) — at the end of
                 a script run, apply the script-checking rules to the
                 group's matched substring, P->eptr .. Feptr. This code
                 will never be exercised if Unicode support is not
                 compiled (always compiled in this port). In bounds:
                 p >= 0 — OP_SCRIPT_RUN is not OP_BRA/OP_COND, so the
                 unchain above found the recorded group frame. *)
              if
                not
                  (Script_run.script_run mb.subject
                     fr.(Frames.base a p + Frames.slot_eptr)
                     fr.(fb + Frames.slot_eptr)
                     utf)
              then (backtrack [@tailcall]) st f match_nomatch
              else (op_ket_tail [@tailcall]) f p bracode
          | 137 | 138 | 142 | 143 ->
              (* OP_CBRA, OP_CBRAPOS, OP_SCBRA, OP_SCBRAPOS
                 (pcre2_match.c:6056-6084). *)
              let number =
                Compile.get2 mb.start_code (bracode + 1 + Limits.link_size)
              in
              if Int.equal fr.(fb + Frames.slot_current_recurse) number then (
                (* pcre2_match.c:6062-6074 — handle a recursively called
                   group: reinstate the previous set of captures and then
                   carry on after the recursion call. The C recomputes
                   P = N - frame_size (6067); the generic unchain above
                   already established that as [p] (>= 0: the recursed
                   group's frame is the GF_RECURSE frame recorded by
                   OP_RECURSE's RM11 RMATCH). In bounds: offset_top <=
                   2 * top_bracket in every frame. *)
                let pb = Frames.base a p in
                Array.blit fr (pb + Frames.slot_ovector) fr
                  (fb + Frames.slot_ovector)
                  fr.(fb + Frames.slot_offset_top);
                fr.(fb + Frames.slot_offset_top) <-
                  fr.(pb + Frames.slot_offset_top);
                fr.(fb + Frames.slot_capture_last) <-
                  fr.(pb + Frames.slot_capture_last);
                fr.(fb + Frames.slot_current_recurse) <-
                  fr.(pb + Frames.slot_current_recurse);
                (* P->ecode is the OP_RECURSE position; step past the
                   1 + LINK_SIZE item and continue with the next
                   opcode. *)
                fr.(fb + Frames.slot_ecode) <-
                  fr.(pb + Frames.slot_ecode) + 1 + Limits.link_size;
                (dispatch [@tailcall]) st f)
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
        then (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (dispatch [@tailcall]) st f)
    | 1 ->
        (* OP_SOD (pcre2_match.c:6139-6142) — unconditional start of
           subject (\A). *)
        if not (Int.equal fr.(fb + Frames.slot_eptr) mb.start_subject) then
          (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (dispatch [@tailcall]) st f)
    | 25 ->
        (* OP_DOLL (pcre2_match.c:6147-6151) — when PCRE2_NOTEOL is unset,
           assert before the subject end, or a terminating newline unless
           PCRE2_DOLLAR_ENDONLY is set. *)
        if not (Int.equal (mb.moptions land Options.noteol) 0) then
          (backtrack [@tailcall]) st f match_nomatch
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
        then (backtrack [@tailcall]) st f match_nomatch
        else if
          (not (Int.equal eptr mb.start_subject))
          && (Int.equal eptr mb.end_subject
              && Int.equal (mb.poptions land Options.alt_circumflex) 0
             || not (was_newline_at st eptr))
        then (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (dispatch [@tailcall]) st f)
    | 26 ->
        (* OP_DOLLM (pcre2_match.c:6214-6239) — assert before any newline,
           or before end of subject unless noteol is set; multiline
           mode. *)
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr < mb.end_subject then
          if not (is_newline_at st eptr) then
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
              else (backtrack [@tailcall]) st f match_nomatch)
            else (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) st f)
        else if not (Int.equal (mb.moptions land Options.noteol) 0) then
          (backtrack [@tailcall]) st f match_nomatch
        else
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc
          else (
            fr.(fb + Frames.slot_ecode) <- ecode + 1;
            (dispatch [@tailcall]) st f)
    | 2 ->
        (* OP_SOM (pcre2_match.c:6243-6246) — start of match assertion
           (\G): subject + offset. *)
        if
          not
            (Int.equal
               fr.(fb + Frames.slot_eptr)
               (mb.start_subject + mb.start_offset))
        then (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_ecode) <- ecode + 1;
          (dispatch [@tailcall]) st f)
    | 3 ->
        (* OP_SET_SOM (pcre2_match.c:6252-6255) — reset the start of match
           point (\K): Fstart_match = Feptr. *)
        fr.(fb + Frames.slot_start_match) <- fr.(fb + Frames.slot_eptr);
        fr.(fb + Frames.slot_ecode) <- ecode + 1;
        (dispatch [@tailcall]) st f
    | 4 | 5 | 169 | 170 ->
        (* OP_NOT_WORD_BOUNDARY, OP_WORD_BOUNDARY,
           OP_NOT_UCP_WORD_BOUNDARY, OP_UCP_WORD_BOUNDARY
           (pcre2_match.c:6258-6333) — find out if the previous and
           current characters are "word" characters, remembering the
           earliest and latest consulted characters, then see if the
           situation is what we want. Characters > 255 are assumed to be
           "non-word" characters when PCRE2_UCP is not set; when it is
           set (the *_UCP_* opcodes), Unicode properties are used, even
           when not in UTF mode. *)
        let ucp_op =
          Int.equal op Opcodes.op_ucp_word_boundary
          || Int.equal op Opcodes.op_not_ucp_word_boundary
        in
        let eptr = fr.(fb + Frames.slot_eptr) in
        (* pcre2_match.c:6269-6293 — status of the previous character:
           none at mb->check_subject (= start_subject except in UTF mode
           with a nonzero start offset). *)
        let prev_is_word =
          if Int.equal eptr mb.check_subject then false
          else if utf then (
            (* pcre2_match.c:6271-6277 — lastptr = Feptr - 1;
               BACKCHAR(lastptr); GETCHAR(fc, lastptr). Invariant: eptr >=
               start_subject = 0 (all reverse ops floor there), but eptr
               CAN sit below check_subject — 10.44's max_lookbehind does
               not count nested lookbehinds (pcre2_compile.c:9604-9612),
               so OP_VREVERSE legitimately walks below it on valid UTF —
               and eptr <> check_subject above therefore does NOT give
               eptr - 1 >= check_subject.
               DEVIATION (defined behavior where the C is undefined; the
               same BACKCHAR-before-subject family and crossing pin as
               op_vreverse_utf_loop): the C's read goes below the subject
               in two ways — (a) eptr = 0 < check_subject (valid UTF,
               nested lookbehind: lastptr = subject - 1 directly), and
               (b) eptr > 0 with the code units from start_subject up to
               eptr - 1 all continuation bytes (invalid prefix; the
               unbounded BACKCHAR at 6275 crosses). In both, the padded
               oracle's walk stops on the slack zero at subject[-1] and
               GETCHAR reads fc = 0 there. Pinned identically: a start
               below 0, or a bounded walk landing on a continuation byte
               (exactly the crossing condition), resolves to lastptr =
               -1, where getchar_subject reads the pinned 0 (Utf.peek);
               start_used_ptr may become -1 exactly as the C's
               subject - 1 does (its consumers are order comparisons and
               raw offset copies, like the C's pointer compares). For a
               non-crossing walk the reads are in-string. *)
            let lastptr =
              if eptr - 1 < 0 then -1
              else
                let p = backchar_subject mb.subject (eptr - 1) in
                if
                  (* safe: 0 <= p <= eptr - 1 < eptr <= mb.end_subject <=
                     String.length mb.subject *)
                  Int.equal
                    (Char.code (String.unsafe_get mb.subject p) land 0xc0)
                    0x80
                then -1
                else p
            in
            let fc = getchar_subject mb.subject lastptr in
            if lastptr < mb.start_used_ptr then mb.start_used_ptr <- lastptr;
            if ucp_op then
              (* pcre2_match.c:6283-6289 — the UCD chartype/category
                 probe. *)
              let chartype = Ucd.chartype fc in
              let category = Tables.ucp_gentype.(chartype) in
              Int.equal category Ucp.ucp_l
              || Int.equal category Ucp.ucp_n
              || Int.equal chartype Ucp.ucp_mn
              || Int.equal chartype Ucp.ucp_pc
            else
              (* pcre2_match.c:6292 — CHMAX_255(fc) && ctype_word. *)
              fc <= 255
              && not
                   (Int.equal
                      (Chartables.ctypes fc land Chartables.ctype_word)
                      0))
          else
            let lastptr = eptr - 1 in
            (* safe: non-UTF mode never moves check_subject off the
               subject start (pcre2_match.c:6795; only the UTF check
               block, 6809-6928, and the invalid-UTF fragment restart,
               7674 / interpreter.ml mirror in the allow_invalid path,
               change it -- both UTF-only), so check_subject =
               start_subject = 0 and eptr <> check_subject above gives
               0 <= lastptr; lastptr < eptr <= end_subject <=
               String.length mb.subject *)
            let fc = Char.code (String.unsafe_get mb.subject lastptr) in
            if lastptr < mb.start_used_ptr then mb.start_used_ptr <- lastptr;
            if ucp_op then
              (* pcre2_match.c:6283-6289 — the UCD probe, without UTF. *)
              let chartype = Ucd.chartype fc in
              let category = Tables.ucp_gentype.(chartype) in
              Int.equal category Ucp.ucp_l
              || Int.equal category Ucp.ucp_n
              || Int.equal chartype Ucp.ucp_mn
              || Int.equal chartype Ucp.ucp_pc
            else
              (* pcre2_match.c:6292 — CHMAX_255(fc) is TRUE for a non-UTF
                 8-bit code unit. *)
              not
                (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0)
        in
        (* pcre2_match.c:6295-6326 — get status of next character. *)
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc
          else (word_boundary_tail [@tailcall]) f prev_is_word false
        else if utf then (
          (* pcre2_match.c:6304-6312 — nextptr = Feptr + 1;
             FORWARDCHARTEST(nextptr, mb->end_subject);
             GETCHAR(fc, Feptr). *)
          let nextptr =
            Utf.forwardchartest mb.subject (eptr + 1) mb.end_subject
          in
          let fc = Utf.getchar mb.subject eptr in
          if nextptr > mb.last_used_ptr then mb.last_used_ptr <- nextptr;
          if ucp_op then
            (* pcre2_match.c:6316-6322 — the UCD probe. *)
            let chartype = Ucd.chartype fc in
            let category = Tables.ucp_gentype.(chartype) in
            (word_boundary_tail [@tailcall]) f prev_is_word
              (Int.equal category Ucp.ucp_l
              || Int.equal category Ucp.ucp_n
              || Int.equal chartype Ucp.ucp_mn
              || Int.equal chartype Ucp.ucp_pc)
          else
            (* pcre2_match.c:6325 — CHMAX_255(fc) && ctype_word. *)
            (word_boundary_tail [@tailcall]) f prev_is_word
              (fc <= 255
              && not
                   (Int.equal
                      (Chartables.ctypes fc land Chartables.ctype_word)
                      0)))
        else
          let nextptr = eptr + 1 in
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let fc = Char.code (String.unsafe_get mb.subject eptr) in
          if nextptr > mb.last_used_ptr then mb.last_used_ptr <- nextptr;
          if ucp_op then
            (* pcre2_match.c:6316-6322 — the UCD probe, without UTF. *)
            let chartype = Ucd.chartype fc in
            let category = Tables.ucp_gentype.(chartype) in
            (word_boundary_tail [@tailcall]) f prev_is_word
              (Int.equal category Ucp.ucp_l
              || Int.equal category Ucp.ucp_n
              || Int.equal chartype Ucp.ucp_mn
              || Int.equal chartype Ucp.ucp_pc)
          else
            (word_boundary_tail [@tailcall]) f prev_is_word
              (not
                 (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0))
    | 154 ->
        (* OP_MARK (pcre2_match.c:6340-6342) — backtracking ( *VERB)s,
           with and without arguments: if the pattern is successfully
           matched, we do not come back from RMATCH. Fmark =
           mb->nomatch_mark = Fecode + 2; the RM12 resume handles the
           returned-MATCH_SKIP_ARG interception. *)
        fr.(fb + Frames.slot_mark) <- ecode + 2;
        mb.nomatch_mark <- ecode + 2;
        (rmatch [@tailcall]) st f
          (ecode + Opcodes.op_lengths.(op)
          + Char.code (Bytes.get mb.start_code (ecode + 1)))
          rm12 0
    | 163 ->
        (* OP_FAIL (pcre2_match.c:6359-6360) — RRETURN(MATCH_NOMATCH). *)
        (backtrack [@tailcall]) st f match_nomatch
    | 161 ->
        (* OP_COMMIT (pcre2_match.c:6362-6370) — the RM13 resume records
           the current recursing group number in mb->verb_current_recurse
           when the MATCH_COMMIT backtracking return is given, enabling
           the recurse processing to catch verbs from within the
           recursion. *)
        (rmatch [@tailcall]) st f (ecode + Opcodes.op_lengths.(op)) rm13 0
    | 162 ->
        (* OP_COMMIT_ARG (pcre2_match.c:6372-6377) — as OP_COMMIT but
           with a mark argument. *)
        fr.(fb + Frames.slot_mark) <- ecode + 2;
        mb.nomatch_mark <- ecode + 2;
        (rmatch [@tailcall]) st f
          (ecode + Opcodes.op_lengths.(op)
          + Char.code (Bytes.get mb.start_code (ecode + 1)))
          rm36 0
    | 155 ->
        (* OP_PRUNE (pcre2_match.c:6379-6383). *)
        (rmatch [@tailcall]) st f (ecode + Opcodes.op_lengths.(op)) rm14 0
    | 156 ->
        (* OP_PRUNE_ARG (pcre2_match.c:6385-6390). *)
        fr.(fb + Frames.slot_mark) <- ecode + 2;
        mb.nomatch_mark <- ecode + 2;
        (rmatch [@tailcall]) st f
          (ecode + Opcodes.op_lengths.(op)
          + Char.code (Bytes.get mb.start_code (ecode + 1)))
          rm15 0
    | 157 ->
        (* OP_SKIP (pcre2_match.c:6392-6397). *)
        (rmatch [@tailcall]) st f (ecode + Opcodes.op_lengths.(op)) rm16 0
    | 158 ->
        (* OP_SKIP_ARG (pcre2_match.c:6399-6414) — note that, for Perl
           compatibility, SKIP with an argument does NOT set
           nomatch_mark. When a pattern match ends with a SKIP_ARG for
           which there was no matching mark, the match is re-run with
           mb->ignore_skip_arg set to the count of the one that failed:
           SKIP_ARGs up to that count are executed as no-ops. *)
        mb.skip_arg_count <- mb.skip_arg_count + 1;
        if mb.skip_arg_count <= mb.ignore_skip_arg then (
          fr.(fb + Frames.slot_ecode) <-
            ecode + Opcodes.op_lengths.(op)
            + Char.code (Bytes.get mb.start_code (ecode + 1));
          (dispatch [@tailcall]) st f)
        else
          (rmatch [@tailcall]) st f
            (ecode + Opcodes.op_lengths.(op)
            + Char.code (Bytes.get mb.start_code (ecode + 1)))
            rm17 0
    | 159 ->
        (* OP_THEN (pcre2_match.c:6426-6434) — the RM18 resume passes
           back the address of the opcode, so that the branch in which it
           occurs can be determined. *)
        (rmatch [@tailcall]) st f (ecode + Opcodes.op_lengths.(op)) rm18 0
    | 160 ->
        (* OP_THEN_ARG (pcre2_match.c:6436-6442). *)
        fr.(fb + Frames.slot_mark) <- ecode + 2;
        mb.nomatch_mark <- ecode + 2;
        (rmatch [@tailcall]) st f
          (ecode + Opcodes.op_lengths.(op)
          + Char.code (Bytes.get mb.start_code (ecode + 1)))
          rm19 0
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
  and op_end_tail (st : match_state) (f : int) : int =
    (* pcre2_match.c:876-940 — the OP_END body, shared with OP_ACCEPT
       (which falls through into it in the C, 874-877). *)
    let mb = st.mb in
    let a = st.arena in
    let match_data = st.match_data in
    let top_bracket = st.top_bracket in
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
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
    then (backtrack [@tailcall]) st f match_nomatch
      (* pcre2_match.c:897-917 — fail if PCRE2_ENDANCHORED is set and
         the end of the match is not the end of the subject. After
         ( *ACCEPT) we fail the entire match at this position (direct
         return) but backtrack if we've reached the end of the
         pattern. *)
    else if
      fr.(fb + Frames.slot_eptr) < mb.end_subject
      && not
           (Int.equal (mb.moptions lor mb.poptions land Options.endanchored) 0)
    then
      if Int.equal fr.(fb + Frames.slot_op) Opcodes.op_end then
        (backtrack [@tailcall]) st f match_nomatch
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
      match_data.ovector.(1) <- fr.(fb + Frames.slot_eptr) - mb.start_subject;
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
  and close_frame_scan (st : match_state) (offset : int) (want : int) : int =
    let a = st.arena in
    (* pcre2_match.c:813-821 — the OP_CLOSE walk back over the chained
       group frames (offset is a frame index here, frames.ml DEVIATION;
       -1 = PCRE2_UNSET): find the frame N whose group_frame_type is
       exactly [want] (GF_CAPTURE | number) and return its predecessor P
       (the frame that dispatched the bracket, whose eptr is the subject
       position at the group start), or PCRE2_ERROR_INTERNAL — a direct
       return in the C (816), not RRETURN. Terminates: the
       last_group_offset chain strictly descends to PCRE2_UNSET. *)
    if Int.equal offset Frames.unset then Errors.error_internal
    else
      let fr = a.Frames.frames in
      if Int.equal fr.(Frames.base a offset + Frames.slot_group_frame_type) want
      then offset - 1
      else
        (close_frame_scan [@tailcall]) st
          fr.(Frames.base a (offset - 1) + Frames.slot_last_group_offset)
          want
  and accept_frame_scan (st : match_state) (offset : int) : int =
    let a = st.arena in
    (* pcre2_match.c:852-860 — the OP_ACCEPT walk back over the chained
       group frames: find the most recent recursion frame N (any frame
       whose GF_IDMASK is GF_RECURSE) and return its predecessor P (the
       frame at the OP_RECURSE position), or PCRE2_ERROR_INTERNAL — a
       direct return in the C (855), not RRETURN. Terminates as
       [close_frame_scan]. *)
    if Int.equal offset Frames.unset then Errors.error_internal
    else
      let fr = a.Frames.frames in
      if
        Int.equal
          (Frames.gf_idmask
             fr.(Frames.base a offset + Frames.slot_group_frame_type))
          Frames.gf_recurse
      then offset - 1
      else
        (accept_frame_scan [@tailcall]) st
          fr.(Frames.base a (offset - 1) + Frames.slot_last_group_offset)
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
      (* pcre2_match.c:1277-1283 — UTF mode: Flength = 1; Lcharptr =
         Fecode; GETCHARLEN(fc, Fecode, Flength); Fecode += Flength. *)
      let c0 = Char.code (Bytes.get mb.start_code ecode) in
      let fc =
        if c0 >= 0xc0 then Utf.getutf8_bytes c0 mb.start_code ecode else c0
      in
      let flength = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
      if flength > 1 then (
        (* pcre2_match.c:1285-1367 — multi-code-unit character matching,
           caseful and caseless. Frame temporaries: Flength = length,
           Lcharptr = temp_sptr[1], Loclength = temp_size, occu[] holds
           the other case's code units. *)
        let fr = a.Frames.frames in
        let fb = Frames.base a f in
        fr.(fb + Frames.slot_length) <- flength (* Flength *);
        fr.(fb + Frames.slot_temp_sptr_1) <- ecode (* Lcharptr *);
        fr.(fb + Frames.slot_temp_32_0) <- lmin (* Lmin *);
        fr.(fb + Frames.slot_temp_32_1) <- lmax (* Lmax *);
        fr.(fb + Frames.slot_ecode) <- ecode + flength;
        (* pcre2_match.c:1289-1294 — if caseless and the character has an
           other case, encode it into Foccu; Loclength = its length,
           else 0. *)
        if fr.(fb + Frames.slot_op) >= Opcodes.op_stari then
          let othercase = Ucd.othercase fc in
          fr.(fb + Frames.slot_temp_size) <-
            (if not (Int.equal othercase fc) then
               ord2utf_slots othercase fr (fb + Frames.slot_occu)
             else 0)
        else fr.(fb + Frames.slot_temp_size) <- 0;
        (repeatchar_wide_min [@tailcall]) f 1 reptype)
      else
        (* pcre2_match.c:1370-1371 — length of UTF character is 1: put it
           into the preserved variable and fall through to the non-UTF
           code. *)
        (repeatchar_tail [@tailcall]) f lmin lmax reptype fc (ecode + 1)
    else
      (* pcre2_match.c:1378-1381 — when not in UTF mode, load a
         single-code-unit character: Lc = *Fecode++. *)
      (repeatchar_tail [@tailcall]) f lmin lmax reptype
        (Char.code (Bytes.get mb.start_code ecode))
        (ecode + 1)
  (* pcre2_match.c:1298-1303 (= 1319-1328, 1339-1349) — one attempted
     match of the wide repeated character: the pattern character itself
     (Flength units at Lcharptr), else its other case (Loclength units in
     Foccu). Advances Feptr and returns true on a match; returns false
     (Feptr untouched) otherwise. *)
  and repeatchar_wide_try (f : int) : bool =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let eptr = fr.(fb + Frames.slot_eptr) in
    let flength = fr.(fb + Frames.slot_length) in
    if
      eptr <= mb.end_subject - flength
      && code_eq_subject mb fr.(fb + Frames.slot_temp_sptr_1) eptr flength
    then (
      fr.(fb + Frames.slot_eptr) <- eptr + flength;
      true)
    else
      let loclength = fr.(fb + Frames.slot_temp_size) in
      if
        loclength > 0
        && eptr <= mb.end_subject - loclength
        && occu_eq_subject mb fr (fb + Frames.slot_occu) eptr loclength
      then (
        fr.(fb + Frames.slot_eptr) <- eptr + loclength;
        true)
      else false
  and repeatchar_wide_min (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1296-1304 — wide char: ensure the minimum number of
       matches are present. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then
      if repeatchar_wide_try f then
        (repeatchar_wide_min [@tailcall]) f (i + 1) reptype
      else
        (* CHECK_PARTIAL() (pcre2_match.c:531-535, site 1301); then no
           match. *)
        let feptr = fr.(fb + Frames.slot_eptr) in
        let rc =
          if feptr >= mb.end_subject then scheck_partial mb feptr else 0
        in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
    else if
      (* pcre2_match.c:1306 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1308-1330 — minimize: the for(;;) starts with
         RMATCH(Fecode, RM202); the rest of the loop body is the RM202
         resume arm in [backtrack]. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm202 0
    else (
      (* pcre2_match.c:1332-1334 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (repeatchar_wide_maxscan [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        reptype)
  and repeatchar_wide_maxscan (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1334-1350 — wide char greedy scan:
       for (i = Lmin; i < Lmax; i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      if repeatchar_wide_try f then
        (repeatchar_wide_maxscan [@tailcall]) f (i + 1) reptype
      else
        (* CHECK_PARTIAL() (site 1347); break. *)
        let feptr = fr.(fb + Frames.slot_eptr) in
        let rc =
          if feptr >= mb.end_subject then scheck_partial mb feptr else 0
        in
        if rc < 0 then rc else (repeatchar_wide_maxend [@tailcall]) f reptype
    else (repeatchar_wide_maxend [@tailcall]) f reptype
  and repeatchar_wide_maxend (f : int) (reptype : int) : int =
    (* pcre2_match.c:1352 — if possessive, no backing up: fall out of the
       arm (break, 1364) and continue the main loop. *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (repeatchar_wide_maxbt [@tailcall]) f
  and repeatchar_wide_maxbt (f : int) : int =
    (* pcre2_match.c:1352-1363 — the wide-char maximize backtracking
       for(;;) head: after \C in UTF mode Lstart_eptr might be in the
       middle of a Unicode character, so <= ensures backtracking doesn't
       go too far. The minimum position is tried in place (break -> main
       loop, 1364); every position above it via RMATCH(Fecode, RM203). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) <= fr.(fb + Frames.slot_temp_sptr_0) then
      (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm203 0
  and repeatchar_tail (f : int) (lmin : int) (lmax : int) (reptype : int)
      (lc : int) (ecode : int) : int =
    (* pcre2_match.c:1376-1400 — the shared single-code-unit tail (the
       UTF single-unit case falls through here with Lc < 128, so the
       byte-wise loops below stay valid in UTF mode: no match can start
       inside a multi-byte character because its lead byte is >= 0xc0).
       The Lmin/Lmax stores transcribe the assignments in the opcode arms
       (1188-1257). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    fr.(fb + Frames.slot_temp_32_0) <- lmin (* Lmin *);
    fr.(fb + Frames.slot_temp_32_1) <- lmax (* Lmax *);
    fr.(fb + Frames.slot_temp_32_2) <- lc (* Lc *);
    fr.(fb + Frames.slot_ecode) <- ecode;
    if fr.(fb + Frames.slot_op) >= Opcodes.op_stari then
      (* pcre2_match.c:1383-1400 — caseless comparison: Loc is the other
         case. *)
      if ucp && (not utf) && lc > 127 then (
        (* pcre2_match.c:1388-1389 — Loc = UCD_OTHERCASE(Lc). *)
        fr.(fb + Frames.slot_temp_32_3) <- Ucd.othercase lc (* Loc *);
        (repeatchar_ci_min [@tailcall]) f 1 reptype)
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
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if
          (not (Int.equal fr.(fb + Frames.slot_temp_32_2) cc))
          && not (Int.equal fr.(fb + Frames.slot_temp_32_3) cc)
        then (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (repeatchar_ci_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:1414 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1416-1421 — minimize: the for(;;) starts with
         RMATCH(Fecode, RM25); the rest of the loop body is the RM25
         resume arm in [backtrack]. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm25 0
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
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (repeatchar_ci_maxbt [@tailcall]) f
  and repeatchar_ci_maxbt (f : int) : int =
    (* pcre2_match.c:1451-1457 — the caseless maximize backtracking
       for(;;) head: the minimum position Lstart_eptr is tried in place
       (break -> main loop); every position above it via RMATCH. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if Int.equal fr.(fb + Frames.slot_eptr) fr.(fb + Frames.slot_temp_sptr_0)
    then (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm26 0
  and repeatchar_cs_min (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1461-1473 — caseful comparisons (includes all
       multi-byte characters): for (i = 1; i <= Lmin; i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* pcre2_match.c:1472 — if (Lc != UCHAR21INCTEST(Feptr))
           RRETURN(MATCH_NOMATCH): the post-increment advances Feptr even
           when the test fails. *)
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        if not (Int.equal fr.(fb + Frames.slot_temp_32_2) cc) then
          (backtrack [@tailcall]) st f match_nomatch
        else (repeatchar_cs_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:1475 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1477-1481 — minimize: RMATCH(Fecode, RM27); the
         rest of the loop body is the RM27 resume arm. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm27 0
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
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (repeatchar_cs_maxbt [@tailcall]) f
  and repeatchar_cs_maxbt (f : int) : int =
    (* pcre2_match.c:1508-1514 — the caseful maximize backtracking for(;;)
       head. The C's `<=` guard (vs `==` at 1453) is transcribed as-is. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) <= fr.(fb + Frames.slot_temp_sptr_0) then
      (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm28 0
  (* pcre2_match.c:1528-1634 — REPEATNOTCHAR: common code for all repeated
     single-character non-matches (goto target of the
     OP_NOTEXACT..OP_NOTMINQUERY arms). Almost a repeat of REPEATCHAR,
     kept separate exactly as the C keeps it (1529-1534). Frame
     temporaries: Lstart_eptr = temp_sptr[0], Lmin/Lmax/Lc/Loc =
     temp_32[0..3] (pcre2_match.c:1536-1540). *)
  and repeatnotchar (f : int) (lmin : int) (lmax : int) (reptype : int)
      (ecode : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:1615-1616 — GETCHARINCTEST(Lc, Fecode): the UTF
       decode of the pattern character. Lmin/Lmax stores transcribe the
       opcode arms (1542-1611). *)
    let c0 = Char.code (Bytes.get mb.start_code ecode) in
    let lc =
      if utf && c0 >= 0xc0 then Utf.getutf8_bytes c0 mb.start_code ecode else c0
    in
    let ecode =
      if utf && c0 >= 0xc0 then ecode + 1 + Utf.get_extralen c0 else ecode + 1
    in
    fr.(fb + Frames.slot_temp_32_0) <- lmin (* Lmin *);
    fr.(fb + Frames.slot_temp_32_1) <- lmax (* Lmax *);
    fr.(fb + Frames.slot_temp_32_2) <- lc (* Lc *);
    fr.(fb + Frames.slot_ecode) <- ecode;
    if fr.(fb + Frames.slot_op) >= Opcodes.op_notstari then (
      (* pcre2_match.c:1626-1634 — caseless: Loc is the other case, via
         UCD_OTHERCASE when (utf || ucp) && Lc > 127, else the fcc table
         (TABLE_GET is a plain index in the 8-bit library; Lc <= 255
         whenever the UCD arm is not taken). *)
      fr.(fb + Frames.slot_temp_32_3) <-
        (if (utf || ucp) && lc > 127 then Ucd.othercase lc
         else Chartables.fcc lc)
      (* Loc *);
      if utf then (repeatnotchar_ci_min_utf [@tailcall]) f 1 reptype
      else (repeatnotchar_ci_min [@tailcall]) f 1 reptype)
    else if utf then (repeatnotchar_cs_min_utf [@tailcall]) f 1 reptype
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
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if
          Int.equal fr.(fb + Frames.slot_temp_32_2) cc
          || Int.equal fr.(fb + Frames.slot_temp_32_3) cc
        then (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (repeatnotchar_ci_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:1668 — if (Lmin == Lmax) continue: finished for
         exact count. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1693-1697 — minimize, not UTF: RMATCH(Fecode,
         RM29); the rest of the loop body is the RM29 resume arm. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm29 0
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
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (repeatnotchar_ci_maxbt [@tailcall]) f
  and repeatnotchar_ci_maxbt (f : int) : int =
    (* pcre2_match.c:1763-1769 — the caseless NOT maximize backtracking
       for(;;) head. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if Int.equal fr.(fb + Frames.slot_eptr) fr.(fb + Frames.slot_temp_sptr_0)
    then (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm30 0
  and repeatnotchar_cs_min (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1795-1806 — caseful, not UTF: for (i = 1; i <= Lmin;
       i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* pcre2_match.c:1804 — if (Lc == *Feptr++)
           RRETURN(MATCH_NOMATCH): the post-increment advances Feptr even
           when the test fails. *)
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        if Int.equal fr.(fb + Frames.slot_temp_32_2) cc then
          (backtrack [@tailcall]) st f match_nomatch
        else (repeatnotchar_cs_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:1808 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1832-1836 — minimize, not UTF: RMATCH(Fecode,
         RM31); the rest of the loop body is the RM31 resume arm. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm31 0
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
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (repeatnotchar_cs_maxbt [@tailcall]) f
  and repeatnotchar_cs_maxbt (f : int) : int =
    (* pcre2_match.c:1900-1906 — the caseful NOT maximize backtracking
       for(;;) head. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if Int.equal fr.(fb + Frames.slot_eptr) fr.(fb + Frames.slot_temp_sptr_0)
    then (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm32 0
  and repeatnotchar_ci_min_utf (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1637-1650 — caseless NOT min loop, UTF mode:
       GETCHARINC(d, Feptr) — the advance happens even when the test
       fails. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let d = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        fr.(fb + Frames.slot_eptr) <-
          (if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0 else eptr + 1);
        if
          Int.equal fr.(fb + Frames.slot_temp_32_2) d
          || Int.equal fr.(fb + Frames.slot_temp_32_3) d
        then (backtrack [@tailcall]) st f match_nomatch
        else (repeatnotchar_ci_min_utf [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:1668 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1672-1689 — minimize, UTF: RMATCH(Fecode, RM204);
         the rest of the loop body is the RM204 resume arm. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm204 0
    else (
      (* pcre2_match.c:1714-1716 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (repeatnotchar_ci_maxscan_utf [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        reptype)
  and repeatnotchar_ci_maxscan_utf (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1718-1730 — caseless NOT greedy scan, UTF:
       GETCHARLEN(d, Feptr, len); no advance until the character
       passes. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:1722-1726 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc
        else (repeatnotchar_ci_maxend_utf [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let d = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        let len = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
        if
          Int.equal fr.(fb + Frames.slot_temp_32_2) d
          || Int.equal fr.(fb + Frames.slot_temp_32_3) d
        then (repeatnotchar_ci_maxend_utf [@tailcall]) f reptype (* break *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + len;
          (repeatnotchar_ci_maxscan_utf [@tailcall]) f (i + 1) reptype)
    else (repeatnotchar_ci_maxend_utf [@tailcall]) f reptype
  and repeatnotchar_ci_maxend_utf (f : int) (reptype : int) : int =
    (* pcre2_match.c:1734 — possessive: fall out (break, 1910). *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (repeatnotchar_ci_maxbt_utf [@tailcall]) f
  and repeatnotchar_ci_maxbt_utf (f : int) : int =
    (* pcre2_match.c:1734-1741 — the caseless NOT maximize backtracking
       for(;;) head, UTF: after \C, Lstart_eptr might be mid-character,
       so <= guards the boundary; positions above it via RMATCH(Fecode,
       RM205), the minimum position in place (break -> main loop). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) <= fr.(fb + Frames.slot_temp_sptr_0) then
      (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm205 0
  and repeatnotchar_cs_min_utf (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1779-1792 — caseful NOT min loop, UTF:
       GETCHARINC(d, Feptr). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let d = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        fr.(fb + Frames.slot_eptr) <-
          (if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0 else eptr + 1);
        if Int.equal fr.(fb + Frames.slot_temp_32_2) d then
          (backtrack [@tailcall]) st f match_nomatch
        else (repeatnotchar_cs_min_utf [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:1808 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:1812-1829 — minimize, UTF: RMATCH(Fecode, RM206);
         the rest of the loop body is the RM206 resume arm. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm206 0
    else (
      (* pcre2_match.c:1852-1854 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (repeatnotchar_cs_maxscan_utf [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        reptype)
  and repeatnotchar_cs_maxscan_utf (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:1857-1870 — caseful NOT greedy scan, UTF:
       GETCHARLEN(d, Feptr, len). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:1861-1865 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc
        else (repeatnotchar_cs_maxend_utf [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let d = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        let len = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
        if Int.equal fr.(fb + Frames.slot_temp_32_2) d then
          (repeatnotchar_cs_maxend_utf [@tailcall]) f reptype (* break *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + len;
          (repeatnotchar_cs_maxscan_utf [@tailcall]) f (i + 1) reptype)
    else (repeatnotchar_cs_maxend_utf [@tailcall]) f reptype
  and repeatnotchar_cs_maxend_utf (f : int) (reptype : int) : int =
    (* pcre2_match.c:1873 — possessive: fall out (break, 1910). *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (repeatnotchar_cs_maxbt_utf [@tailcall]) f
  and repeatnotchar_cs_maxbt_utf (f : int) : int =
    (* pcre2_match.c:1873-1885 — the caseful NOT maximize backtracking
       for(;;) head, UTF (RM207), as [repeatnotchar_ci_maxbt_utf]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) <= fr.(fb + Frames.slot_temp_sptr_0) then
      (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm207 0
  (* pcre2_match.c:1007-1010 — OP_CHAR's UTF compare loop: for (; Flength
     > 0; Flength--) if ( *Fecode++ != UCHAR21INC(Feptr))
     RRETURN(MATCH_NOMATCH). Feptr's post-increment lands one past every
     compared unit, including the mismatching one; Fecode advances to the
     opcode's end on success (the initial Fecode++ is in [cpos]'s starting
     value). *)
  and op_char_utf_cmp (f : int) (cpos : int) (eptr : int) (n : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if n <= 0 then (
      fr.(fb + Frames.slot_ecode) <- cpos;
      (dispatch [@tailcall]) st f)
    else (
      fr.(fb + Frames.slot_eptr) <- eptr + 1;
      if
        not
          (Int.equal
             (Char.code (Bytes.get mb.start_code cpos))
             (* safe: the caller checked Flength <= end_subject - eptr0,
                so eptr < eptr0 + Flength <= mb.end_subject <=
                String.length mb.subject; eptr >= eptr0 >= 0 (mb
                invariant) *)
             (Char.code (String.unsafe_get mb.subject eptr)))
      then (backtrack [@tailcall]) st f match_nomatch
      else (op_char_utf_cmp [@tailcall]) f (cpos + 1) (eptr + 1) (n - 1))
  (* pcre2_match.c:962-973 — the OP_ALLANY body, shared as the fallthrough
     tail of OP_ANY (fallthrough from OP_ANY in C, 958-960). DO NOT merge
     the Feptr++ into the bound check; it must not be updated before
     SCHECK_PARTIAL (963-967). *)
  and op_allany_tail (f : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let eptr = fr.(fb + Frames.slot_eptr) in
    if eptr >= mb.end_subject then
      let rc = scheck_partial mb eptr in
      if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
    else
      (* pcre2_match.c:970-971 — if (utf) ACROSSCHAR(Feptr <
         mb->end_subject, Feptr, Feptr++) (pcre2_intmodedep.h:352-353 =
         FORWARDCHARTEST bounded by end_subject). *)
      let eptr = eptr + 1 in
      fr.(fb + Frames.slot_eptr) <-
        (if utf then Utf.forwardchartest mb.subject eptr mb.end_subject
         else eptr);
      fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
      (dispatch [@tailcall]) st f
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
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* pcre2_match.c:2009 — fc = *Feptr++: the post-increment advances
           Feptr even when the bitmap test fails. *)
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        if Int.equal (class_bit st f fc) 0 then
          (backtrack [@tailcall]) st f match_nomatch
        else (class_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:2019-2021 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:2049-2055 — minimize, not UTF: the for(;;) starts
         with RMATCH(Fecode, RM23); the rest of the loop body is the RM23
         resume arm in [backtrack]. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm23 0
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
        if Int.equal (class_bit st f fc) 0 then
          (class_maxend [@tailcall]) f reptype (* break, 2134 *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (class_maxscan [@tailcall]) f (i + 1) reptype)
    else (class_maxend [@tailcall]) f reptype
  and class_maxend (f : int) (reptype : int) : int =
    (* pcre2_match.c:2139-2141 — if possessive, no backing up: continue
       the main loop at the advanced Fecode. *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
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
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm24 0
    else (backtrack [@tailcall]) st f match_nomatch
  (* pcre2_match.c:1977-1995 — OP_CLASS/OP_NCLASS min loop, UTF mode:
     GETCHARINC(fc, Feptr); characters > 255 fail OP_CLASS but match
     OP_NCLASS (only the bitmapped 0-255 range is negatable). *)
  and class_utf_min (f : int) (i : int) (reptype : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        fr.(fb + Frames.slot_eptr) <-
          (if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0 else eptr + 1);
        if fc > 255 then
          if Int.equal fr.(fb + Frames.slot_op) Opcodes.op_class then
            (backtrack [@tailcall]) st f match_nomatch
          else (class_utf_min [@tailcall]) f (i + 1) reptype
        else if Int.equal (class_bit st f fc) 0 then
          (backtrack [@tailcall]) st f match_nomatch
        else (class_utf_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:2019-2021 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:2028-2047 — minimize, UTF: the for(;;) starts with
         RMATCH(Fecode, RM200); the rest of the loop body is the RM200
         resume arm in [backtrack]. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm200 0
    else (
      (* pcre2_match.c:2079-2081 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (class_utf_maxscan [@tailcall]) f fr.(fb + Frames.slot_temp_32_0) reptype)
  and class_utf_maxscan (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:2084-2105 — class greedy scan, UTF:
       for (i = Lmin; i < Lmax; i++) with GETCHARLEN (no advance until the
       character passes). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:2087-2091 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (class_utf_maxend [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        let len = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
        if
          if fc > 255 then Int.equal fr.(fb + Frames.slot_op) Opcodes.op_class
          else Int.equal (class_bit st f fc) 0
        then (class_utf_maxend [@tailcall]) f reptype (* break *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + len;
          (class_utf_maxscan [@tailcall]) f (i + 1) reptype)
    else (class_utf_maxend [@tailcall]) f reptype
  and class_utf_maxend (f : int) (reptype : int) : int =
    (* pcre2_match.c:2107 — if possessive, no backing up: continue the
       main loop at the advanced Fecode. *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else
      (* pcre2_match.c:2110-2117 — the backtracking for(;;) RMATCHes
         FIRST (unlike the non-UTF while head): RMATCH(Fecode, RM201);
         the post-RMATCH steps live in the RM201 resume arm. *)
      let fr = a.Frames.frames in
      (rmatch [@tailcall]) st f fr.(Frames.base a f + Frames.slot_ecode) rm201 0
  (* pcre2_match.c:2218-2224 — OP_XCLASS min loop:
     GETCHARINCTEST(fc, Feptr); PRIV(xclass)(fc, Lxclass_data, utf). *)
  and xclass_min (f : int) (i : int) (reptype : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc =
          if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
        in
        fr.(fb + Frames.slot_eptr) <-
          (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
           else eptr + 1);
        if
          not
            (Xclass.xclass fc mb.start_code
               fr.(fb + Frames.slot_temp_sptr_1)
               utf)
        then (backtrack [@tailcall]) st f match_nomatch
        else (xclass_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:2226-2228 — if (Lmin == Lmax) continue. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:2233-2248 — minimize: the for(;;) starts with
         RMATCH(Fecode, RM100); the rest of the loop body is the RM100
         resume arm in [backtrack]. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm100 0
    else (
      (* pcre2_match.c:2253-2255 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (xclass_maxscan [@tailcall]) f fr.(fb + Frames.slot_temp_32_0) reptype)
  and xclass_maxscan (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:2256-2271 — xclass greedy scan:
       for (i = Lmin; i < Lmax; i++) with GETCHARLENTEST. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:2259-2263 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (xclass_maxend [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc =
          if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
        in
        let len = if utf && c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
        if
          not
            (Xclass.xclass fc mb.start_code
               fr.(fb + Frames.slot_temp_sptr_1)
               utf)
        then (xclass_maxend [@tailcall]) f reptype (* break, 2268 *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + len;
          (xclass_maxscan [@tailcall]) f (i + 1) reptype)
    else (xclass_maxend [@tailcall]) f reptype
  and xclass_maxend (f : int) (reptype : int) : int =
    (* pcre2_match.c:2273 — if possessive, no backing up. *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else
      (* pcre2_match.c:2278-2287 — the backtracking for(;;) RMATCHes
         FIRST: RMATCH(Fecode, RM101); the post-RMATCH steps live in the
         RM101 resume arm. *)
      let fr = a.Frames.frames in
      (rmatch [@tailcall]) st f fr.(Frames.base a f + Frames.slot_ecode) rm101 0
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
    if Int.equal lctype Opcodes.op_prop || Int.equal lctype Opcodes.op_notprop
    then (
      (* pcre2_match.c:2708-2714 — proptype = *Fecode++; Lpropvalue =
         *Fecode++. proptype does NOT need a frame slot (C:2641-2643: it
         is not used within an RMATCH loop — each minimize resume label
         RM208-RM217/RM223-RM225 stands for one property type). *)
      let proptype = Char.code (Bytes.get mb.start_code ecode) in
      let lpropvalue = Char.code (Bytes.get mb.start_code (ecode + 1)) in
      let ecode = ecode + 2 in
      fr.(fb + Frames.slot_temp_32_0) <- lmin (* Lmin *);
      fr.(fb + Frames.slot_temp_32_1) <- lmax (* Lmax *);
      fr.(fb + Frames.slot_temp_32_2) <- lctype (* Lctype *);
      fr.(fb + Frames.slot_temp_32_3) <- lpropvalue (* Lpropvalue *);
      fr.(fb + Frames.slot_ecode) <- ecode;
      (* pcre2_match.c:2717-2727 — first, ensure the minimum number of
         matches are present: property tests in all modes. *)
      if lmin > 0 then
        if proptype > Opcodes.pt_bool then Errors.error_internal
          (* 2969-2972 — this should not occur *)
        else if
          Int.equal proptype Opcodes.pt_any
          && Int.equal lctype Opcodes.op_notprop
        then
          (* pcre2_match.c:2731-2732 — PT_ANY hoists the notmatch check
             ahead of its min loop: NOMATCH before consuming anything. *)
          (backtrack [@tailcall]) st f match_nomatch
        else (propmin [@tailcall]) f 1 proptype reptype
      else (repeattype_prop_post_min [@tailcall]) f proptype reptype)
    else if Int.equal lctype Opcodes.op_extuni then (
      (* pcre2_match.c:2976-2996 — match extended Unicode sequences: the
         min loop; the strategy dispatch and the RM218/RM220 loops are
         [extuni_post_min] and the resume arms in [backtrack]. *)
      fr.(fb + Frames.slot_temp_32_0) <- lmin (* Lmin *);
      fr.(fb + Frames.slot_temp_32_1) <- lmax (* Lmax *);
      fr.(fb + Frames.slot_temp_32_2) <- lctype (* Lctype *);
      fr.(fb + Frames.slot_ecode) <- ecode;
      if lmin > 0 then (typemin_extuni [@tailcall]) f 1 reptype
      else (extuni_post_min [@tailcall]) f reptype)
    else (
      fr.(fb + Frames.slot_temp_32_0) <- lmin (* Lmin *);
      fr.(fb + Frames.slot_temp_32_1) <- lmax (* Lmax *);
      fr.(fb + Frames.slot_temp_32_2) <- lctype (* Lctype *);
      fr.(fb + Frames.slot_ecode) <- ecode;
      (* pcre2_match.c:2717-2723 — first, ensure the minimum number of
         matches are present, with the type test done once at the start
         (kept out of the loops): the UTF switch(Lctype) at 3003-3250 or
         the non-UTF one at 3252-3500. *)
      if lmin > 0 then
        if utf then
          if Int.equal lctype Opcodes.op_any then
            (typemin_utf_any [@tailcall]) f 1 reptype
          else if Int.equal lctype Opcodes.op_allany then
            (typemin_utf_allany [@tailcall]) f 1 reptype
          else if Int.equal lctype Opcodes.op_anybyte then
            (* pcre2_match.c:3041-3044 — OP_ANYBYTE: code units, not
               characters; one bound check covers the block (no
               SCHECK_PARTIAL in this case). *)
            let eptr = fr.(fb + Frames.slot_eptr) in
            if eptr > mb.end_subject - lmin then
              (backtrack [@tailcall]) st f match_nomatch
            else (
              fr.(fb + Frames.slot_eptr) <- eptr + lmin;
              (repeattype_post_min [@tailcall]) f reptype)
          else if Int.equal lctype Opcodes.op_anynl then
            (typemin_utf_anynl [@tailcall]) f 1 reptype
          else if Int.equal lctype Opcodes.op_not_hspace then
            (typemin_utf_hspace [@tailcall]) f 1 true reptype
          else if Int.equal lctype Opcodes.op_hspace then
            (typemin_utf_hspace [@tailcall]) f 1 false reptype
          else if Int.equal lctype Opcodes.op_not_vspace then
            (typemin_utf_vspace [@tailcall]) f 1 true reptype
          else if Int.equal lctype Opcodes.op_vspace then
            (typemin_utf_vspace [@tailcall]) f 1 false reptype
          else if Int.equal lctype Opcodes.op_not_digit then
            (typemin_utf_notdigit [@tailcall]) f 1 reptype
          else if Int.equal lctype Opcodes.op_digit then
            (typemin_utf_pos_ctype [@tailcall]) f 1 Chartables.ctype_digit
              reptype
          else if Int.equal lctype Opcodes.op_not_whitespace then
            (typemin_utf_neg_ctype [@tailcall]) f 1 Chartables.ctype_space
              reptype
          else if Int.equal lctype Opcodes.op_whitespace then
            (typemin_utf_pos_ctype [@tailcall]) f 1 Chartables.ctype_space
              reptype
          else if Int.equal lctype Opcodes.op_not_wordchar then
            (typemin_utf_neg_ctype [@tailcall]) f 1 Chartables.ctype_word
              reptype
          else if Int.equal lctype Opcodes.op_wordchar then
            (typemin_utf_pos_ctype [@tailcall]) f 1 Chartables.ctype_word
              reptype
          else Errors.error_internal (* 3247-3248 — default *)
        else if Int.equal lctype Opcodes.op_any then
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
            if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
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
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else if is_newline_at st eptr then
        (backtrack [@tailcall]) st f match_nomatch
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
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
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
            (backtrack [@tailcall]) st f match_nomatch
          else (typemin_anynl [@tailcall]) f (i + 1) reptype
        else (backtrack [@tailcall]) st f match_nomatch (* 3313 — default *))
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
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        if Bool.equal (hspace_byte fc) negated then
          (backtrack [@tailcall]) st f match_nomatch
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
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        fr.(fb + Frames.slot_eptr) <- eptr + 1;
        if Bool.equal (vspace_byte fc) negated then
          (backtrack [@tailcall]) st f match_nomatch
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
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let fc = Char.code (String.unsafe_get mb.subject eptr) in
        if
          Bool.equal
            (not (Int.equal (Chartables.ctypes fc land mask) 0))
            negated
        then (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemin_ctype [@tailcall]) f (i + 1) mask negated reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and propmin (f : int) (i : int) (ptype : int) (reptype : int) : int =
    (* pcre2_match.c:2726-2974 — the property min loops (property tests
       in all modes): for (i = 1; i <= Lmin; i++) { bound check +
       SCHECK_PARTIAL; GETCHARINCTEST(fc, Feptr); <property test> ==
       notmatch -> RRETURN(MATCH_NOMATCH) }.
       DEVIATION(structure): the C writes 13 property-specific loops with
       `BOOL notmatch = Lctype == OP_NOTPROP` hoisted (2728); merged here
       on [ptype] through [prop_test] — per-iteration reads and
       evaluation order identical. PT_ANY's pre-loop notmatch check
       (2731-2732) is done by [repeattype] before entering; the caller
       also excluded property types above PT_BOOL (2971-2972). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc =
          if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
        in
        fr.(fb + Frames.slot_eptr) <-
          (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
           else eptr + 1);
        if
          Bool.equal
            (prop_test fc ptype fr.(fb + Frames.slot_temp_32_3))
            (Int.equal fr.(fb + Frames.slot_temp_32_2) Opcodes.op_notprop)
        then (backtrack [@tailcall]) st f match_nomatch
        else (propmin [@tailcall]) f (i + 1) ptype reptype)
    else (repeattype_prop_post_min [@tailcall]) f ptype reptype
  and repeattype_prop_post_min (f : int) (ptype : int) (reptype : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:3503-3505 — if (Lmin = Lmax) we are done: continue
       with the main loop. *)
    if Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:3507-3517 — minimize: each property type's for(;;)
         starts with an RMATCH at its own resume label (the label stands
         for the un-framed proptype); the rest of each loop body is the
         shared property-minimize resume in [backtrack]. RMATCH sites:
         3522 RM208 PT_ANY, 3539 RM209 PT_LAMP, 3559 RM210 PT_GC, 3576
         RM211 PT_PC, 3593 RM212 PT_SC, 3612 RM225 PT_SCX, 3633 RM213
         PT_ALNUM, 3656 RM214 PT_SPACE/PT_PXSPACE, 3684 RM215 PT_WORD,
         3707 RM216 PT_CLIST, 3743 RM217 PT_UCNC, 3762 RM224 PT_BIDICL,
         3781 RM223 PT_BOOL; the switch default (3798-3800) is
         PCRE2_ERROR_INTERNAL, hit before any RMATCH. *)
      let rm =
        if Int.equal ptype Opcodes.pt_any then rm208
        else if Int.equal ptype Opcodes.pt_lamp then rm209
        else if Int.equal ptype Opcodes.pt_gc then rm210
        else if Int.equal ptype Opcodes.pt_pc then rm211
        else if Int.equal ptype Opcodes.pt_sc then rm212
        else if Int.equal ptype Opcodes.pt_scx then rm225
        else if Int.equal ptype Opcodes.pt_alnum then rm213
        else if
          Int.equal ptype Opcodes.pt_space || Int.equal ptype Opcodes.pt_pxspace
        then rm214
        else if Int.equal ptype Opcodes.pt_word then rm215
        else if Int.equal ptype Opcodes.pt_clist then rm216
        else if Int.equal ptype Opcodes.pt_ucnc then rm217
        else if Int.equal ptype Opcodes.pt_bidicl then rm224
        else if Int.equal ptype Opcodes.pt_bool then rm223
        else -1
      in
      if rm < 0 then Errors.error_internal
      else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm 0
    else (
      (* pcre2_match.c:4110-4118 — maximize: Lstart_eptr = Feptr; the
         property maximize switch's default (4379-4380) is
         PCRE2_ERROR_INTERNAL, hit before any scan step. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      if ptype > Opcodes.pt_bool then Errors.error_internal
      else
        (propmax_scan [@tailcall]) f
          fr.(fb + Frames.slot_temp_32_0)
          ptype reptype)
  and propmax_scan (f : int) (i : int) (ptype : int) (reptype : int) : int =
    (* pcre2_match.c:4115-4381 — the property maximize scans:
       for (i = Lmin; i < Lmax; i++) { bound check + SCHECK_PARTIAL ->
       break; GETCHARLENTEST(fc, Feptr, len); <property test> == notmatch
       -> break; Feptr += len }.
       DEVIATION(structure): the C writes 13 loops (with the PT_SPACE
       ENDLOOP99 and PT_CLIST GOT_MAX gotos as loop exits); merged here
       on [ptype] through [prop_test] — per-iteration reads and
       evaluation order identical. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (propmax_tail [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc =
          if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
        in
        let len = if utf && c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
        if
          Bool.equal
            (prop_test fc ptype fr.(fb + Frames.slot_temp_32_3))
            (Int.equal fr.(fb + Frames.slot_temp_32_2) Opcodes.op_notprop)
        then (propmax_tail [@tailcall]) f reptype (* break *)
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + len;
          (propmax_scan [@tailcall]) f (i + 1) ptype reptype)
    else (propmax_tail [@tailcall]) f reptype
  and propmax_tail (f : int) (reptype : int) : int =
    (* pcre2_match.c:4383-4385 — Feptr is now past the end of the maximum
       run; if possessive, no backtracking: continue the main loop at the
       advanced Fecode. *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (propmax_bt [@tailcall]) f
  and propmax_bt (f : int) : int =
    (* pcre2_match.c:4387-4398 — the property maximize backtracking
       for(;;) head: after \C in UTF mode, Lstart_eptr might be in the
       middle of a Unicode character, so <= ensures backtracking doesn't
       go too far. The minimum position is tried in place (break -> main
       loop); every position above it via RMATCH(Fecode, RM222). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) <= fr.(fb + Frames.slot_temp_sptr_0) then
      (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm222 0
  and prop_min_resume (f : int) (rrc : int) (ptype : int) (rmlabel : int) : int
      =
    (* pcre2_match.c:3519-3796 — the shared body of the 13 property
       minimize for(;;) loops after their RMATCH ([rmlabel] identifies
       the loop and re-enters it): if (rrc != MATCH_NOMATCH) RRETURN(rrc);
       if (Lmin++ >= Lmax) RRETURN(MATCH_NOMATCH); bound check +
       SCHECK_PARTIAL; GETCHARINCTEST(fc, Feptr); <property test> ==
       (Lctype == OP_NOTPROP) -> RRETURN(MATCH_NOMATCH); loop.
       DEVIATION(structure): merged on [ptype] through [prop_test], as at
       [propmin]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if not (Int.equal rrc match_nomatch) then (backtrack [@tailcall]) st f rrc
    else
      let lmin = fr.(fb + Frames.slot_temp_32_0) in
      fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
      if lmin >= fr.(fb + Frames.slot_temp_32_1) then
        (backtrack [@tailcall]) st f match_nomatch
      else
        let eptr = fr.(fb + Frames.slot_eptr) in
        if eptr >= mb.end_subject then
          let rc = scheck_partial mb eptr in
          if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
        else
          (* safe: eptr < mb.end_subject <= String.length mb.subject
             (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let fc =
            if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
          in
          fr.(fb + Frames.slot_eptr) <-
            (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
             else eptr + 1);
          if
            Bool.equal
              (prop_test fc ptype fr.(fb + Frames.slot_temp_32_3))
              (Int.equal fr.(fb + Frames.slot_temp_32_2) Opcodes.op_notprop)
          then (backtrack [@tailcall]) st f match_nomatch
          else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rmlabel 0
  and typemin_extuni (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:2979-2996 — OP_EXTUNI: ensure the minimum number of
       clusters are present: for (i = 1; i <= Lmin; i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:2983-2987 — SCHECK_PARTIAL(), then no match. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* pcre2_match.c:2988-2993 — GETCHARINCTEST(fc, Feptr); Feptr =
           PRIV(extuni)(...). *)
        let eptr' = extuni_step st eptr in
        fr.(fb + Frames.slot_eptr) <- eptr';
        (* CHECK_PARTIAL() (2994). *)
        let rc =
          if eptr' >= mb.end_subject then scheck_partial mb eptr' else 0
        in
        if rc < 0 then rc else (typemin_extuni [@tailcall]) f (i + 1) reptype)
    else (extuni_post_min [@tailcall]) f reptype
  and extuni_post_min (f : int) (reptype : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:3503-3505 — if (Lmin = Lmax) we are done: continue
       with the main loop. *)
    if Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:3804-3811 — minimize: the for(;;) starts with
         RMATCH(Fecode, RM218); the rest of the loop body is the RM218
         resume arm in [backtrack]. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm218 0
    else (
      (* pcre2_match.c:4110-4112 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      (extuni_maxscan [@tailcall]) f fr.(fb + Frames.slot_temp_32_0) reptype)
  and extuni_maxscan (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:4401-4420 — match extended Unicode grapheme
       clusters, maximize: for (i = Lmin; i < Lmax; i++). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        (* pcre2_match.c:4408-4412 — SCHECK_PARTIAL(); break. *)
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (extuni_maxend [@tailcall]) f reptype
      else
        (* pcre2_match.c:4413-4418 — GETCHARINCTEST(fc, Feptr); Feptr =
           PRIV(extuni)(...). *)
        let eptr' = extuni_step st eptr in
        fr.(fb + Frames.slot_eptr) <- eptr';
        (* CHECK_PARTIAL() (4419). *)
        let rc =
          if eptr' >= mb.end_subject then scheck_partial mb eptr' else 0
        in
        if rc < 0 then rc else (extuni_maxscan [@tailcall]) f (i + 1) reptype)
    else (extuni_maxend [@tailcall]) f reptype
  and extuni_maxend (f : int) (reptype : int) : int =
    (* pcre2_match.c:4422-4424 — Feptr is now past the end of the maximum
       run; if possessive, no backtracking: continue the main loop at the
       advanced Fecode. *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (extuni_maxbt [@tailcall]) f
  and extuni_maxbt (f : int) : int =
    (* pcre2_match.c:4426-4437 — the cluster maximize backtracking
       for(;;) head: we use <= Lstart_eptr rather than == to detect the
       start of the run while backtracking, because the use of \C in UTF
       mode can cause BACKCHAR to move back past Lstart_eptr. The minimum
       position is tried in place (break -> main loop); every position
       above it via RMATCH(Fecode, RM220), whose resume steps back one
       cluster (the RM220 arm + [extuni_bt_inner]). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) <= fr.(fb + Frames.slot_temp_sptr_0) then
      (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm220 0
  and extuni_bt_inner (f : int) (rgb : int) : int =
    (* pcre2_match.c:4452-4465 — the inner for(;;) of the RM220 resume:
       walk further back while the pair table forbids a break between the
       character before Feptr and the one at Feptr ([rgb], a plain local
       in the C too — it never survives an RMATCH). NOTE: this re-walk
       uses only the pair table, not the ZWJ/RI special rules — the C's
       approach, transcribed as-is. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let eptr = fr.(fb + Frames.slot_eptr) in
    if eptr <= fr.(fb + Frames.slot_temp_sptr_0) then
      (* At start of char run: break to the outer loop head. *)
      (extuni_maxbt [@tailcall]) f
    else
      (* pcre2_match.c:4455-4460 — fptr = Feptr - 1; if (!utf) fc = *fptr;
         else { BACKCHAR(fptr); GETCHAR(fc, fptr); }. eptr - 1 >=
         Lstart_eptr >= 0 (guard above); backchar_subject/getchar_subject
         clamp their reads (non-UTF read: fptr < eptr <= a former
         in-bounds position < String.length mb.subject). *)
      let fptr = eptr - 1 in
      let fptr = if utf then backchar_subject mb.subject fptr else fptr in
      let fc =
        if utf then getchar_subject mb.subject fptr
        else Char.code (String.unsafe_get mb.subject fptr)
      in
      let lgb = Ucd.gbprop fc in
      (* pcre2_match.c:4461-4464 *)
      if Int.equal (Tables.ucp_gbtable.(lgb) land (1 lsl rgb)) 0 then
        (extuni_maxbt [@tailcall]) f
      else (
        fr.(fb + Frames.slot_eptr) <- fptr;
        (extuni_bt_inner [@tailcall]) f lgb)
  and repeattype_post_min (f : int) (reptype : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    (* pcre2_match.c:3503-3505 — if (Lmin = Lmax) we are done: continue
       with the main loop. *)
    if Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      if utf then
        (* pcre2_match.c:3834-3959 — minimize, UTF (non-property types):
           the for(;;) starts with RMATCH(Fecode, RM219); the rest of the
           loop body is the RM219 resume arm in [backtrack]. *)
        (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm219 0
      else
        (* pcre2_match.c:3507-3512 + 3961-3965 — minimize, not UTF (the
           property types route through [repeattype_prop_post_min], never
           here): the for(;;) starts with RMATCH(Fecode, RM33);
           the rest of the loop body is the RM33 resume arm in
           [backtrack]. *)
        (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm33 0
    else (
      (* pcre2_match.c:4110-4112 — maximize: Lstart_eptr = Feptr. *)
      fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr);
      if utf then (typemax_utf [@tailcall]) f reptype
      else (typemax [@tailcall]) f reptype)
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
      else if is_newline_at st eptr then
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
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (typemax_bt [@tailcall]) f
  and typemax_bt (f : int) : int =
    (* pcre2_match.c:4960-4968 — the maximize backtracking for(;;) head:
       the minimum position Lstart_eptr is tried in place (break -> main
       loop); every position above it via RMATCH(Fecode, RM34). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if Int.equal fr.(fb + Frames.slot_eptr) fr.(fb + Frames.slot_temp_sptr_0)
    then (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm34 0
  and typemin_utf_any (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:3005-3026 — min loop for OP_ANY, UTF: newlines do
       not match; a CRLF pattern newline with only its CR present at the
       end of the subject could be partial; ACROSSCHAR advances over the
       character's remaining code units. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else if is_newline_at st eptr then
        (backtrack [@tailcall]) st f match_nomatch
      else if
        (* pcre2_match.c:3014-3022 *)
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
          fr.(fb + Frames.slot_eptr) <-
            Utf.forwardchartest mb.subject (eptr + 1) mb.end_subject;
          (typemin_utf_any [@tailcall]) f (i + 1) reptype))
      else (
        (* Feptr++; ACROSSCHAR (3024-3025). *)
        fr.(fb + Frames.slot_eptr) <-
          Utf.forwardchartest mb.subject (eptr + 1) mb.end_subject;
        (typemin_utf_any [@tailcall]) f (i + 1) reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_utf_allany (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:3028-3039 — min loop for OP_ALLANY, UTF. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else (
        fr.(fb + Frames.slot_eptr) <-
          Utf.forwardchartest mb.subject (eptr + 1) mb.end_subject;
        (typemin_utf_allany [@tailcall]) f (i + 1) reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_utf_anynl (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:3046-3076 — min loop for OP_ANYNL, UTF:
       GETCHARINC(fc, Feptr) before the tests; the multibyte
       0x2028/0x2029 are live here. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        let eptr =
          if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0 else eptr + 1
        in
        fr.(fb + Frames.slot_eptr) <- eptr;
        if Int.equal fc Newline.char_cr then (
          (* pcre2_match.c:3059-3061 — CR: absorb a following LF. *)
          if
            eptr < mb.end_subject
            && Int.equal
                 (* safe: eptr < mb.end_subject (checked) *)
                 (Char.code (String.unsafe_get mb.subject eptr))
                 Newline.char_lf
          then fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemin_utf_anynl [@tailcall]) f (i + 1) reptype)
        else if Int.equal fc Newline.char_lf then
          (typemin_utf_anynl [@tailcall]) f (i + 1) reptype
        else if
          (* pcre2_match.c:3066-3074 — VT, FF, NEL, LS, PS. *)
          Int.equal fc Newline.char_vt
          || Int.equal fc Newline.char_ff
          || Int.equal fc Newline.char_nel
          || Int.equal fc 0x2028 || Int.equal fc 0x2029
        then
          if Int.equal mb.bsr_convention Options.bsr_anycrlf then
            (backtrack [@tailcall]) st f match_nomatch
          else (typemin_utf_anynl [@tailcall]) f (i + 1) reptype
        else (backtrack [@tailcall]) st f match_nomatch (* 3055 — default *))
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_utf_hspace (f : int) (i : int) (negated : bool) (reptype : int) :
      int =
    (* pcre2_match.c:3079-3112 — min loops for OP_NOT_HSPACE / OP_HSPACE,
       UTF: GETCHARINC then the full HSPACE_CASES.
       DEVIATION(structure): polarities merged on [negated], as in the
       non-UTF [typemin_hspace]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        fr.(fb + Frames.slot_eptr) <-
          (if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0 else eptr + 1);
        if Bool.equal (hspace_char fc) negated then
          (backtrack [@tailcall]) st f match_nomatch
        else (typemin_utf_hspace [@tailcall]) f (i + 1) negated reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_utf_vspace (f : int) (i : int) (negated : bool) (reptype : int) :
      int =
    (* pcre2_match.c:3113-3146 — min loops for OP_NOT_VSPACE / OP_VSPACE,
       UTF: full VSPACE_CASES. DEVIATION(structure): polarities merged on
       [negated]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        fr.(fb + Frames.slot_eptr) <-
          (if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0 else eptr + 1);
        if Bool.equal (vspace_char fc) negated then
          (backtrack [@tailcall]) st f match_nomatch
        else (typemin_utf_vspace [@tailcall]) f (i + 1) negated reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_utf_notdigit (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:3147-3160 — min loop for OP_NOT_DIGIT, UTF: the full
       character is decoded (GETCHARINC) and tested with the 128
       boundary. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then (
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        fr.(fb + Frames.slot_eptr) <-
          (if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0 else eptr + 1);
        if
          fc < 128
          && not
               (Int.equal (Chartables.ctypes fc land Chartables.ctype_digit) 0)
        then (backtrack [@tailcall]) st f match_nomatch
        else (typemin_utf_notdigit [@tailcall]) f (i + 1) reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_utf_pos_ctype (f : int) (i : int) (mask : int) (reptype : int) :
      int =
    (* pcre2_match.c:3161-3177 (OP_DIGIT), 3195-3211 (OP_WHITESPACE),
       3229-3245 (OP_WORDCHAR) — positive-type min loops, UTF: only the
       first code unit is read (cc = UCHAR21(Feptr)); a match implies
       cc < 128, which has no more code units, so Feptr++ suffices ("no
       need to skip more code units - we know it has only one").
       DEVIATION(structure): the three loops merged on [mask], as in the
       non-UTF [typemin_ctype]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if cc >= 128 || Int.equal (Chartables.ctypes cc land mask) 0 then
          (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + 1;
          (typemin_utf_pos_ctype [@tailcall]) f (i + 1) mask reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and typemin_utf_neg_ctype (f : int) (i : int) (mask : int) (reptype : int) :
      int =
    (* pcre2_match.c:3178-3194 (OP_NOT_WHITESPACE), 3212-3228
       (OP_NOT_WORDCHAR) — negative-type min loops, UTF: only the first
       code unit is read; a lead unit >= 128 is never of the type, and
       ACROSSCHAR then skips the rest of the character.
       DEVIATION(structure): merged on [mask]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if cc < 128 && not (Int.equal (Chartables.ctypes cc land mask) 0) then
          (backtrack [@tailcall]) st f match_nomatch
        else (
          (* Feptr++; ACROSSCHAR. *)
          fr.(fb + Frames.slot_eptr) <-
            Utf.forwardchartest mb.subject (eptr + 1) mb.end_subject;
          (typemin_utf_neg_ctype [@tailcall]) f (i + 1) mask reptype)
    else (repeattype_post_min [@tailcall]) f reptype
  and typemax_utf (f : int) (reptype : int) : int =
    (* pcre2_match.c:4473-4705 — maximize, UTF: find the longest possible
       run, with the type test done once at the start: the UTF
       switch(Lctype). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let lctype = fr.(fb + Frames.slot_temp_32_2) in
    if Int.equal lctype Opcodes.op_any then
      (typemax_utf_any [@tailcall]) f fr.(fb + Frames.slot_temp_32_0) reptype
    else if Int.equal lctype Opcodes.op_allany then
      if fr.(fb + Frames.slot_temp_32_1) < uint32_max then
        (* pcre2_match.c:4501-4511 — bounded: step by characters. *)
        (typemax_utf_allany [@tailcall]) f
          fr.(fb + Frames.slot_temp_32_0)
          reptype
      else (
        (* pcre2_match.c:4513-4517 — unlimited UTF-8 repeat: take
           everything. *)
        fr.(fb + Frames.slot_eptr) <- mb.end_subject;
        let rc = scheck_partial mb mb.end_subject in
        if rc < 0 then rc else (typemax_utf_tail [@tailcall]) f reptype)
    else if Int.equal lctype Opcodes.op_anybyte then
      (* pcre2_match.c:4520-4529 — the "byte" case is the same as
         non-UTF: fc = Lmax - Lmin. *)
      let n =
        fr.(fb + Frames.slot_temp_32_1) - fr.(fb + Frames.slot_temp_32_0)
      in
      let eptr = fr.(fb + Frames.slot_eptr) in
      if n > mb.end_subject - eptr then (
        fr.(fb + Frames.slot_eptr) <- mb.end_subject;
        let rc = scheck_partial mb mb.end_subject in
        if rc < 0 then rc else (typemax_utf_tail [@tailcall]) f reptype)
      else (
        fr.(fb + Frames.slot_eptr) <- eptr + n;
        (typemax_utf_tail [@tailcall]) f reptype)
    else if Int.equal lctype Opcodes.op_anynl then
      (typemax_utf_anynl [@tailcall]) f fr.(fb + Frames.slot_temp_32_0) reptype
    else if Int.equal lctype Opcodes.op_not_hspace then
      (typemax_utf_hspace [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        true reptype
    else if Int.equal lctype Opcodes.op_hspace then
      (typemax_utf_hspace [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        false reptype
    else if Int.equal lctype Opcodes.op_not_vspace then
      (typemax_utf_vspace [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        true reptype
    else if Int.equal lctype Opcodes.op_vspace then
      (typemax_utf_vspace [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        false reptype
    else if Int.equal lctype Opcodes.op_not_digit then
      (typemax_utf_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_digit true reptype
    else if Int.equal lctype Opcodes.op_digit then
      (typemax_utf_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_digit false reptype
    else if Int.equal lctype Opcodes.op_not_whitespace then
      (typemax_utf_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_space true reptype
    else if Int.equal lctype Opcodes.op_whitespace then
      (typemax_utf_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_space false reptype
    else if Int.equal lctype Opcodes.op_not_wordchar then
      (typemax_utf_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_word true reptype
    else if Int.equal lctype Opcodes.op_wordchar then
      (typemax_utf_ctype [@tailcall]) f
        fr.(fb + Frames.slot_temp_32_0)
        Chartables.ctype_word false reptype
    else Errors.error_internal (* 4697-4698 — default *)
  and typemax_utf_any (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:4476-4498 — maximize scan for OP_ANY, UTF. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_utf_tail [@tailcall]) f reptype
      else if is_newline_at st eptr then
        (typemax_utf_tail [@tailcall]) f reptype (* break, 4484 *)
      else if
        (* pcre2_match.c:4485-4493 — take care with CRLF partial. *)
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
          fr.(fb + Frames.slot_eptr) <-
            Utf.forwardchartest mb.subject (eptr + 1) mb.end_subject;
          (typemax_utf_any [@tailcall]) f (i + 1) reptype))
      else (
        (* Feptr++; ACROSSCHAR (4495-4496). *)
        fr.(fb + Frames.slot_eptr) <-
          Utf.forwardchartest mb.subject (eptr + 1) mb.end_subject;
        (typemax_utf_any [@tailcall]) f (i + 1) reptype)
    else (typemax_utf_tail [@tailcall]) f reptype
  and typemax_utf_allany (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:4501-4511 — bounded maximize scan for OP_ALLANY,
       UTF. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_utf_tail [@tailcall]) f reptype
      else (
        fr.(fb + Frames.slot_eptr) <-
          Utf.forwardchartest mb.subject (eptr + 1) mb.end_subject;
        (typemax_utf_allany [@tailcall]) f (i + 1) reptype)
    else (typemax_utf_tail [@tailcall]) f reptype
  and typemax_utf_anynl (f : int) (i : int) (reptype : int) : int =
    (* pcre2_match.c:4531-4557 — maximize scan for OP_ANYNL, UTF (the
       multibyte 0x2028/0x2029 are live, unlike the non-UTF loop). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_utf_tail [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        let len = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
        if Int.equal fc Newline.char_cr then (
          (* 4540-4544 — if (++Feptr >= mb->end_subject) break; absorb a
             following LF. *)
          let eptr = eptr + 1 in
          fr.(fb + Frames.slot_eptr) <- eptr;
          if eptr >= mb.end_subject then
            (typemax_utf_tail [@tailcall]) f reptype
          else (
            if
              Int.equal
                (* safe: eptr < mb.end_subject (checked) *)
                (Char.code (String.unsafe_get mb.subject eptr))
                Newline.char_lf
            then fr.(fb + Frames.slot_eptr) <- eptr + 1;
            (typemax_utf_anynl [@tailcall]) f (i + 1) reptype))
        else if
          (* 4546-4555 — break unless LF, or VT/FF/NEL/LS/PS outside the
             ANYCRLF convention. *)
          (not (Int.equal fc Newline.char_lf))
          && (Int.equal mb.bsr_convention Options.bsr_anycrlf
             || (not (Int.equal fc Newline.char_vt))
                && (not (Int.equal fc Newline.char_ff))
                && (not (Int.equal fc Newline.char_nel))
                && (not (Int.equal fc 0x2028))
                && not (Int.equal fc 0x2029))
        then (typemax_utf_tail [@tailcall]) f reptype
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + len;
          (typemax_utf_anynl [@tailcall]) f (i + 1) reptype)
    else (typemax_utf_tail [@tailcall]) f reptype
  and typemax_utf_hspace (f : int) (i : int) (negated : bool) (reptype : int) :
      int =
    (* pcre2_match.c:4559-4579 — maximize scans for OP_NOT_HSPACE /
       OP_HSPACE, UTF: gotspace == (Lctype == OP_NOT_HSPACE) breaks.
       DEVIATION(structure): polarities merged on [negated]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_utf_tail [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        let len = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
        if Bool.equal (hspace_char fc) negated then
          (typemax_utf_tail [@tailcall]) f reptype
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + len;
          (typemax_utf_hspace [@tailcall]) f (i + 1) negated reptype)
    else (typemax_utf_tail [@tailcall]) f reptype
  and typemax_utf_vspace (f : int) (i : int) (negated : bool) (reptype : int) :
      int =
    (* pcre2_match.c:4581-4601 — maximize scans for OP_NOT_VSPACE /
       OP_VSPACE, UTF. DEVIATION(structure): polarities merged on
       [negated]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_utf_tail [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        let len = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
        if Bool.equal (vspace_char fc) negated then
          (typemax_utf_tail [@tailcall]) f reptype
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + len;
          (typemax_utf_vspace [@tailcall]) f (i + 1) negated reptype)
    else (typemax_utf_tail [@tailcall]) f reptype
  and typemax_utf_ctype (f : int) (i : int) (mask : int) (negated : bool)
      (reptype : int) : int =
    (* pcre2_match.c:4603-4700 — maximize scans for OP_NOT_DIGIT ..
       OP_WORDCHAR, UTF: GETCHARLEN with the 256 boundary.
       DEVIATION(structure): the six loops merged on [mask]/[negated]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr >= mb.end_subject then
        let rc = scheck_partial mb eptr in
        if rc < 0 then rc else (typemax_utf_tail [@tailcall]) f reptype
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject
           (checked above); 0 <= start_eptr <= eptr (mb invariant) *)
        let c0 = Char.code (String.unsafe_get mb.subject eptr) in
        let fc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
        let len = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
        let is_type =
          fc < 256 && not (Int.equal (Chartables.ctypes fc land mask) 0)
        in
        if Bool.equal is_type negated then
          (typemax_utf_tail [@tailcall]) f reptype
        else (
          fr.(fb + Frames.slot_eptr) <- eptr + len;
          (typemax_utf_ctype [@tailcall]) f (i + 1) mask negated reptype)
    else (typemax_utf_tail [@tailcall]) f reptype
  and typemax_utf_tail (f : int) (reptype : int) : int =
    (* pcre2_match.c:4701 — if possessive, no backing up. *)
    if Int.equal reptype reptype_pos then (dispatch [@tailcall]) st f
    else (typemax_utf_bt [@tailcall]) f
  and typemax_utf_bt (f : int) : int =
    (* pcre2_match.c:4707-4719 — the UTF maximize backtracking for(;;)
       head: after \C, Lstart_eptr might be in the middle of a Unicode
       character, so <= guards the boundary. The minimum position is
       tried in place (break -> main loop); every position above it via
       RMATCH(Fecode, RM221). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) <= fr.(fb + Frames.slot_temp_sptr_0) then
      (dispatch [@tailcall]) st f
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm221 0
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
        match_ref st f
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
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch)
      else (
        fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) + !ref_length;
        (dispatch [@tailcall]) st f (* continue, 5055-5056 *)))
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
      then (dispatch [@tailcall]) st f (* zero-length group: continue, 5068 *)
      else (ref_min [@tailcall]) f 1 reptype
    else if
      (* pcre2_match.c:5070-5074 — group is not set. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) 0
      || not (Int.equal (mb.poptions land Options.match_unset_backref) 0)
    then (dispatch [@tailcall]) st f
    else (ref_min [@tailcall]) f 1 reptype
  (* pcre2_match.c:5076-5124 — first, ensure the minimum number of
     matches are present: for (i = 1; i <= Lmin; i++); then dispatch on
     the repeat strategy. *)
  and ref_min (f : int) (i : int) (reptype : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i <= fr.(fb + Frames.slot_temp_32_0) then
      let rrc =
        match_ref st f
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
        if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch)
      else (
        fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) + !ref_length;
        (ref_min [@tailcall]) f (i + 1) reptype)
    else if
      (* pcre2_match.c:5091-5093 — if min = max, we are done; they are
         not both allowed to be zero. *)
      Int.equal fr.(fb + Frames.slot_temp_32_0) fr.(fb + Frames.slot_temp_32_1)
    then (dispatch [@tailcall]) st f
    else if Int.equal reptype reptype_min then
      (* pcre2_match.c:5095-5115 — if minimizing, keep trying and
         advancing the pointer: the for(;;) starts with RMATCH(Fecode,
         RM20); the rest of the loop body is the RM20 resume arm in
         [backtrack]. *)
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm20 0
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
        match_ref st f
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
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm22 0
  (* pcre2_match.c:5154-5162 + 5186 — the samelengths backtracking while
     head: while (Feptr >= Lstart) try the rest of the pattern via
     RMATCH(Fecode, RM21), stepping Feptr back by Flength per failure
     (the RM21 resume arm); when Feptr falls below Lstart,
     RRETURN(MATCH_NOMATCH). *)
  and ref_max_same_bt (f : int) : int =
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) >= fr.(fb + Frames.slot_temp_sptr_0) then
      (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm21 0
    else (backtrack [@tailcall]) st f match_nomatch
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
        match_ref st f
          fr.(fb + Frames.slot_temp_size)
          (not (Int.equal fr.(fb + Frames.slot_temp_32_2) 0))
          ref_length
      in
      fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) + !ref_length;
      (ref_max_rescan [@tailcall]) f (i + 1))
    else (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm22 0
  and word_boundary_tail (f : int) (prev_is_word : bool) (cur_is_word : bool) :
      int =
    (* pcre2_match.c:6328-6333 — now see if the situation is what we want:
       ( *Fecode++ == OP_WORD_BOUNDARY || Fop == OP_UCP_WORD_BOUNDARY) ?
       cur_is_word == prev_is_word : cur_is_word != prev_is_word —
       *Fecode++ advances past the opcode either way; RRETURN(NOMATCH)
       when a boundary opcode sees cur == prev, or a not-boundary opcode
       sees cur != prev. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
    if
      if
        Int.equal fr.(fb + Frames.slot_op) Opcodes.op_word_boundary
        || Int.equal fr.(fb + Frames.slot_op) Opcodes.op_ucp_word_boundary
      then Bool.equal cur_is_word prev_is_word
      else not (Bool.equal cur_is_word prev_is_word)
    then (backtrack [@tailcall]) st f match_nomatch
    else (dispatch [@tailcall]) st f
  and op_eod_tail (f : int) : int =
    (* pcre2_match.c:6154-6163 — the OP_EOD body, shared as the
       fallthrough tail of OP_DOLL under PCRE2_DOLLAR_ENDONLY (fallthrough
       from OP_DOLL in C, 6149-6152). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) < mb.true_end_subject then
      (backtrack [@tailcall]) st f match_nomatch
    else if not (Int.equal mb.partial 0) then (
      mb.hitend <- true;
      if mb.partial > 1 then Errors.error_partial
      else (
        fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
        (dispatch [@tailcall]) st f))
    else (
      fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
      (dispatch [@tailcall]) st f)
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
      && ((not (is_newline_at st eptr))
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
        else (backtrack [@tailcall]) st f match_nomatch)
      else (backtrack [@tailcall]) st f match_nomatch
    else if
      (* pcre2_match.c:6184-6190 — either at end of string or \n before
         end. *)
      not (Int.equal mb.partial 0)
    then (
      mb.hitend <- true;
      if mb.partial > 1 then Errors.error_partial
      else (
        fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
        (dispatch [@tailcall]) st f))
    else (
      fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
      (dispatch [@tailcall]) st f)
  and op_reverse_utf_loop (f : int) (number : int) : int =
    (* pcre2_match.c:5797-5804 — OP_REVERSE, UTF: while (number-- > 0)
       fail when Feptr <= mb->check_subject, else Feptr--;
       BACKCHAR(Feptr). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if number > 0 then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if eptr <= mb.check_subject then
        (backtrack [@tailcall]) st f match_nomatch
      else (
        (* eptr - 1 >= check_subject >= 0 (guard above); backchar_subject
           clamps its reads. *)
        fr.(fb + Frames.slot_eptr) <- backchar_subject mb.subject (eptr - 1);
        (op_reverse_utf_loop [@tailcall]) f (number - 1))
    else (op_reverse_tail [@tailcall]) f
  and op_reverse_tail (f : int) : int =
    (* pcre2_match.c:5815-5818 — save the earliest consulted character,
       then skip to next opcode (shared by the UTF and non-UTF OP_REVERSE
       arms; Fecode still sits on the opcode). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let eptr = fr.(fb + Frames.slot_eptr) in
    if eptr < mb.start_used_ptr then mb.start_used_ptr <- eptr;
    fr.(fb + Frames.slot_ecode) <-
      fr.(fb + Frames.slot_ecode) + 1 + Limits.imm2_size;
    (dispatch [@tailcall]) st f
  and op_vreverse_utf_loop (f : int) (i : int) (branch_ecode : int) : int =
    (* pcre2_match.c:5845-5856 — OP_VREVERSE, UTF: for (i = 0; i < Lmax;
       i++) stop at the subject start (NOMATCH below the minimum,
       otherwise Lmax = i, 5848-5853), stepping back with `Feptr--;
       BACKCHAR(Feptr)` (5854-5855); then fall into the shared RM37 loop
       (5871-5876).
       DEVIATION (defined behavior where the C is undefined; third member
       of the BACKCHAR-before-subject family — Newline.was_newline and
       the oracle's zero slack padding): C's BACKCHAR at 5855 is the
       unbounded continuation-byte walk (pcre2_intmodedep.h:345). When
       the code units from start_subject up to the walk position are ALL
       continuation bytes — an invalid-UTF prefix, which no option
       excludes: (a) PCRE2_MATCH_INVALID_UTF starts matching after such
       a prefix (bad-start skip, 6829-6836); (b) plain PCRE2_UTF with a
       nonzero start offset validates only from check_subject
       (pcre2_match.c:6891), so an all-continuation prefix BELOW
       check_subject goes unchecked and a nested VREVERSE (the
       max_lookbehind undercount below) walks into it, e.g.
       /(?<=(?<=(a{3,5}))a)/utf on "\x80\x80aaaaaa" at offset 7,
       check_subject = 2; (c) NO_UTF_CHECK garbage. Only a subject
       actually validated from position 0 cannot trigger it — the walk
       crosses below the subject start and C reads out of bounds, then
       matches the branch from subject-1 (ovector entries at -1 ==
       PCRE2_UNSET; fuzz seed 20260708 case 125828, minimized:
       /(?<=(.)?)/match_invalid_utf on "\x80"). Pin: option-INDEPENDENT
       — it keys on the bytes alone, exactly like the C walk it bounds,
       and must never move behind an option gate (that would reintroduce
       the UB divergence on the routes above). The walk is bounded at
       start_subject, and if the
       bounded walk still lands on a continuation byte (exactly the
       crossing condition) this back-step cannot be completed — same
       effect as reaching start_subject in the 5848-5853 too-few/cap
       logic, with eptr left at its pre-step value. The stop test itself
       stays C-literal (start_subject, NOT check_subject): a
       check_subject floor was REJECTED because it changes DEFINED
       behavior — 10.44's max_lookbehind does not count nested
       lookbehinds ("A nested lookbehind does not contribute any length",
       pcre2_compile.c:9604-9612), so on valid UTF with a large start
       offset an inner lookbehind legitimately walks below check_subject
       (e.g. /(?<=(?<=(a{3,4}))a)/ on "aaaaaa" at offset 5: real C gives
       group 1 = (0,4), a check_subject floor would give (1,4)) and the
       engine must too. The dev-only oracle carries the matching pin
       (oracle/patches/pcre2-10.44-oracle-vreverse-backchar-bound
       .patch). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if i < fr.(fb + Frames.slot_temp_32_1) then
      let eptr = fr.(fb + Frames.slot_eptr) in
      if Int.equal eptr mb.start_subject then
        if i < fr.(fb + Frames.slot_temp_32_0) then
          (backtrack [@tailcall]) st f match_nomatch
        else (
          fr.(fb + Frames.slot_temp_32_1) <- i (* Lmax = i *);
          (rmatch [@tailcall]) st f branch_ecode rm37 0)
      else
        (* eptr - 1 >= start_subject = 0 (eptr <> start_subject above and
           eptr >= start_subject by the mb invariant); backchar_subject
           bounds the walk at 0. *)
        let p = backchar_subject mb.subject (eptr - 1) in
        if
          (* safe: 0 <= p <= eptr - 1 < eptr <= mb.end_subject <=
             String.length mb.subject *)
          Int.equal (Char.code (String.unsafe_get mb.subject p) land 0xc0) 0x80
        then
          (* pin: the bounded walk landed on a continuation byte, so C's
             unbounded BACKCHAR would cross below the subject — the step
             cannot be completed; same cap logic as the start_subject arm
             above (5848-5853), eptr unchanged. *)
          if i < fr.(fb + Frames.slot_temp_32_0) then
            (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_temp_32_1) <- i (* Lmax = i *);
            (rmatch [@tailcall]) st f branch_ecode rm37 0)
        else (
          fr.(fb + Frames.slot_eptr) <- p;
          (op_vreverse_utf_loop [@tailcall]) f (i + 1) branch_ecode)
    else
      (* pcre2_match.c:5871-5876 — now try matching (RM37). *)
      (rmatch [@tailcall]) st f branch_ecode rm37 0
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
  and possessive_group (f : int) : int =
    (* pcre2_match.c:5283-5285 — POSSESSIVE_GROUP: the shared body of the
       possessive-bracket arms (Lframe_type and Lzero_allowed already set
       by the entering arm). Lmatched_once = FALSE; Lstart_group =
       Fecode. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    fr.(fb + Frames.slot_temp_32_1) <- 0 (* Lmatched_once = FALSE *);
    fr.(fb + Frames.slot_temp_sptr_1) <- fr.(fb + Frames.slot_ecode)
    (* Lstart_group *);
    (possessive_group_loop [@tailcall]) f
  and possessive_group_loop (f : int) : int =
    (* pcre2_match.c:5287-5291 — the for(;;) head: remember the subject
       position at the group start, then record a backtracking point for
       the branch, passing the remembered group frame type (RM8); the rest
       of the loop body is the RM8 resume arm in [backtrack]. Entered with
       Fecode at the bracket (first call and each MATCH_KETRPOS iteration)
       or at an OP_ALT (branch-failure walk); OP_lengths[*Fecode] steps
       over either item. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    fr.(fb + Frames.slot_temp_sptr_0) <- fr.(fb + Frames.slot_eptr)
    (* Lstart_eptr *);
    let ecode = fr.(fb + Frames.slot_ecode) in
    (rmatch [@tailcall]) st f
      (ecode + Opcodes.op_lengths.(Char.code (Bytes.get mb.start_code ecode)))
      rm8
      fr.(fb + Frames.slot_temp_32_0)
  and possessive_group_done (f : int) : int =
    (* pcre2_match.c:5320-5328 — out of the branch loop: success if
       matched something or zero repeat allowed (continue after the final
       OP_KETRPOS, where Fecode now points); otherwise the group fails. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if
      (not (Int.equal fr.(fb + Frames.slot_temp_32_1) 0))
      (* Lmatched_once *)
      || not (Int.equal fr.(fb + Frames.slot_temp_32_2) 0)
      (* Lzero_allowed *)
    then (
      fr.(fb + Frames.slot_ecode) <-
        fr.(fb + Frames.slot_ecode) + 1 + Limits.link_size;
      (dispatch [@tailcall]) st f)
    else (backtrack [@tailcall]) st f match_nomatch
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
      (dispatch [@tailcall]) st f)
    else
      (* pcre2_match.c:5361-5364 — never the final branch; no MATCH_THEN
         test needed because this code is not used when there is a THEN in
         the pattern. *)
      (rmatch [@tailcall]) st f
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
    (rmatch [@tailcall]) st f
      (ecode + Opcodes.op_lengths.(Char.code (Bytes.get mb.start_code ecode)))
      rm2
      fr.(fb + Frames.slot_temp_32_0)
  and assert_loop (f : int) : int =
    (* pcre2_match.c:5516-5519 — the positive-assertion for(;;) head:
       record a backtracking point for the branch, passing the group frame
       type saved in Lframe_type (temp_32[0], 5509); the rest of the loop
       body is the RM3 resume arm in [backtrack]. Entered with Fecode at
       the assertion bracket (first call) or at an OP_ALT (RM3 branch
       walk); OP_lengths[*Fecode] steps over either item. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let ecode = fr.(fb + Frames.slot_ecode) in
    (rmatch [@tailcall]) st f
      (ecode + Opcodes.op_lengths.(Char.code (Bytes.get mb.start_code ecode)))
      rm3
      fr.(fb + Frames.slot_temp_32_0)
  and assert_not_loop (f : int) : int =
    (* pcre2_match.c:5551-5554 — the negative-assertion for(;;) head, as
       [assert_loop] but with the RM4 resume (the rrc switch lives in
       [backtrack]). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let ecode = fr.(fb + Frames.slot_ecode) in
    (rmatch [@tailcall]) st f
      (ecode + Opcodes.op_lengths.(Char.code (Bytes.get mb.start_code ecode)))
      rm4
      fr.(fb + Frames.slot_temp_32_0)
  and recurse_loop_check (f : int) (offset : int) (number : int) : int =
    (* pcre2_match.c:5438-5454 — the OP_RECURSE loop-detection walk over
       the chained group frames: offset starts at Flast_group_offset (a
       frame index here, frames.ml DEVIATION; -1 = PCRE2_UNSET), N is the
       frame at [offset] and P its predecessor. When the most recent
       GF_RECURSE frame for the same group [number] is found, repeating
       it without changing the subject pointer or the last referenced
       character is PCRE2_ERROR_RECURSELOOP — a direct return in the C
       (5449), not RRETURN — unless PCRE2_DISABLE_RECURSELOOP_CHECK is
       set; either way the walk stops there (break, 5450). Returns 0 to
       continue with the recursion. Terminates: the last_group_offset
       chain strictly descends to PCRE2_UNSET. *)
    if Int.equal offset Frames.unset then 0
    else
      let fr = a.Frames.frames in
      let nb = Frames.base a offset in
      let pb = Frames.base a (offset - 1) in
      if
        Int.equal
          fr.(nb + Frames.slot_group_frame_type)
          (Frames.gf_recurse lor number)
      then
        if
          Int.equal
            fr.(Frames.base a f + Frames.slot_eptr)
            fr.(pb + Frames.slot_eptr)
          && Int.equal mb.last_used_ptr fr.(pb + Frames.slot_recurse_last_used)
          && Int.equal (mb.moptions land Options.disable_recurseloop_check) 0
        then Errors.error_recurseloop
        else 0 (* break, 5450 *)
      else
        (recurse_loop_check [@tailcall]) f
          fr.(pb + Frames.slot_last_group_offset)
          number
  and recurse_loop (f : int) : int =
    (* pcre2_match.c:5463-5468 — the OP_RECURSE for(;;) head: run the
       branch at Lstart_branch, passing the remembered GF_RECURSE frame
       type (RM11); the rest of the loop body is the RM11 resume arm in
       [backtrack]. Entered with Lstart_branch at the recursed bracket
       (first call) or at an OP_ALT (RM11 branch walk);
       OP_lengths[*Lstart_branch] steps over either item. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let start_branch = fr.(fb + Frames.slot_temp_sptr_0) in
    (rmatch [@tailcall]) st f
      (start_branch
      + Opcodes.op_lengths.(Char.code (Bytes.get mb.start_code start_branch)))
      rm11
      fr.(fb + Frames.slot_temp_32_0)
  and recurse_advance_branch (f : int) (next_ecode : int) : int =
    (* pcre2_match.c:5494-5495 — the RM11 branch walk: Lstart_branch =
       next_ecode; fail the whole recursion when there is no next
       alternative. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    fr.(fb + Frames.slot_temp_sptr_0) <- next_ecode (* Lstart_branch *);
    if
      not
        (Int.equal
           (Char.code (Bytes.get mb.start_code next_ecode))
           Opcodes.op_alt)
    then (backtrack [@tailcall]) st f match_nomatch
    else (recurse_loop [@tailcall]) f
  and dnrref_scan (current_recurse : int) (count : int) (slot : int) : bool =
    (* pcre2_match.c:5656-5665 — the OP_DNRREF group-list walk:
       while (count-- > 0) test each group number the name refers to
       against the current recursion. GET2(slot, 0) is the group number
       at the head of a name-table entry. *)
    if count <= 0 then false
    else if Int.equal (Compile.get2 mb.name_table slot) current_recurse then
      true
    else
      (dnrref_scan [@tailcall]) current_recurse (count - 1)
        (slot + mb.name_entry_size)
  and dncref_scan (f : int) (count : int) (slot : int) : bool =
    (* pcre2_match.c:5675-5684 — the OP_DNCREF group-list walk:
       while (count-- > 0) test whether any group the name refers to is
       set, exactly as OP_CREF tests its single group. *)
    if count <= 0 then false
    else
      let fr = a.Frames.frames in
      let fb = Frames.base a f in
      let offset = (Compile.get2 mb.name_table slot lsl 1) - 2 in
      if
        offset < fr.(fb + Frames.slot_offset_top)
        && not (Int.equal fr.(fb + Frames.slot_ovector + offset) Frames.unset)
      then true
      else (dncref_scan [@tailcall]) f (count - 1) (slot + mb.name_entry_size)
  and cond_assert_loop (f : int) : int =
    (* pcre2_match.c:5705-5708 — the assertion-condition for(;;) head:
       record a backtracking point for the branch at Lstart_branch with a
       GF_CONDASSERT frame carrying the condition opcode (Fecode stays on
       that opcode for the whole loop); the rest of the loop body is the
       RM5 resume arm in [backtrack]. Entered with Lstart_branch at the
       assertion bracket (first call) or at an OP_ALT (RM5 branch walk);
       OP_lengths[*Lstart_branch] steps over either item. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let start_branch = fr.(fb + Frames.slot_temp_sptr_0) in
    (rmatch [@tailcall]) st f
      (start_branch
      + Opcodes.op_lengths.(Char.code (Bytes.get mb.start_code start_branch)))
      rm5
      (Frames.gf_condassert
      lor Char.code (Bytes.get mb.start_code fr.(fb + Frames.slot_ecode)))
  and cond_assert_end (f : int) (condition : bool) : int =
    (* pcre2_match.c:5750-5756 — out of the branch loop: if the condition
       is true, find the end of the assertion so that advancing past it
       gets us to the start of the first branch: do Fecode +=
       GET(Fecode, 1); while ( *Fecode == OP_ALT ) — the [skip_alts]
       walk. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if condition then
      fr.(fb + Frames.slot_ecode) <- skip_alts fr.(fb + Frames.slot_ecode);
    (cond_choose [@tailcall]) f condition
  and cond_choose (f : int) (condition : bool) : int =
    (* pcre2_match.c:5763-5778 — choose branch according to the
       condition: Fecode += condition ? OP_lengths[*Fecode] : Flength.
       If the opcode is OP_SCOND it means we are at a repeated
       conditional group that might match an empty string: we must
       descend a level so that the start is remembered for checking
       (RM35 — its resume is RRETURN(rrc)); for OP_COND we can just
       continue at this level. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let ecode = fr.(fb + Frames.slot_ecode) in
    let ecode =
      if condition then
        ecode + Opcodes.op_lengths.(Char.code (Bytes.get mb.start_code ecode))
      else ecode + fr.(fb + Frames.slot_length)
    in
    fr.(fb + Frames.slot_ecode) <- ecode;
    if Int.equal fr.(fb + Frames.slot_op) Opcodes.op_scond then
      (rmatch [@tailcall]) st f ecode rm35
        (Frames.gf_nocapture lor Opcodes.op_scond)
    else (dispatch [@tailcall]) st f
  and once_adjust (e : int) : int =
    (* pcre2_match.c:6025-6030 — adjust the code pointer within the
       backtrack frame so that it points to the final branch: for(;;)
       { y = GET(P->ecode, 1); if ((P->ecode)[y] != OP_ALT) break;
       P->ecode += y; }. Terminates: branch links chain from the bracket
       to its ket in a complete compiled program (mb invariant). *)
    let y = Compile.get mb.start_code (e + 1) in
    if
      not
        (Int.equal (Char.code (Bytes.get mb.start_code (e + y))) Opcodes.op_alt)
    then e
    else (once_adjust [@tailcall]) (e + y)
  and op_ket_assert_na (f : int) (p : int) (bracode : int) : int =
    (* pcre2_match.c:5999-6002 — the shared OP_ASSERT_NA tail (fallthrough
       target of OP_ASSERTBACK_NA): non-atomic positive assertions are
       like OP_BRA, except that the subject pointer must be put back to
       where it was at the start of the assertion. [p] >= 0: the assertion
       bracket arms always pass a nonzero group_frame_type (mb
       invariant). *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) > mb.last_used_ptr then
      mb.last_used_ptr <- fr.(fb + Frames.slot_eptr);
    fr.(fb + Frames.slot_eptr) <- fr.(Frames.base a p + Frames.slot_eptr);
    (op_ket_tail [@tailcall]) f p bracode
  and op_ket_assert (f : int) (p : int) (bracode : int) : int =
    (* pcre2_match.c:6013-6016 — the shared OP_ASSERT tail (fallthrough
       target of OP_ASSERTBACK): as OP_ASSERT_NA, then fall through to the
       OP_ONCE backtrack discard. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    if fr.(fb + Frames.slot_eptr) > mb.last_used_ptr then
      mb.last_used_ptr <- fr.(fb + Frames.slot_eptr);
    fr.(fb + Frames.slot_eptr) <- fr.(Frames.base a p + Frames.slot_eptr);
    (op_ket_once [@tailcall]) f p bracode (* fallthrough from OP_ASSERT in C *)
  and op_ket_once (f : int) (p : int) (bracode : int) : int =
    (* pcre2_match.c:6023-6031 — the OP_ONCE ket action (also the
       fallthrough tail of OP_ASSERT/OP_ASSERTBACK): for an atomic group,
       discard internal backtracking points by making a later RRETURN from
       this frame jump straight back to P (Fback_frame = F - P, in frame
       units here — frames.ml DEVIATION), and ensure that any remaining
       branches within the top-level of the group are not tried by
       adjusting the code pointer within the backtrack frame so that it
       points to the final branch. [p] >= 0 as at [op_ket_assert_na]. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    fr.(fb + Frames.slot_back_frame) <- f - p;
    let pb = Frames.base a p in
    fr.(pb + Frames.slot_ecode) <- once_adjust fr.(pb + Frames.slot_ecode);
    (op_ket_tail [@tailcall]) f p bracode
  and op_ket_tail (f : int) (p : int) (bracode : int) : int =
    (* pcre2_match.c:6087-6127 — the common ket tail after the *bracode
       switch. [p] is the C's P as a frame index, -1 = NULL. *)
    let fr = a.Frames.frames in
    let fb = Frames.base a f in
    let ecode = fr.(fb + Frames.slot_ecode) in
    if Int.equal (Char.code (Bytes.get mb.start_code ecode)) Opcodes.op_ketrpos
    then (
      (* pcre2_match.c:6092-6098 — OP_KETRPOS is a possessive repeating
         ket: remember the current position by copying the frame's whole
         copied region (frame_copy_size bytes from the eptr field = slots
         slot_eptr .. frame end) back to P, and return MATCH_KETRPOS. This
         makes it possible to do the repeats one at a time from the outer
         level (the RM8 resume). This must precede the empty string test —
         in this case that test is done at the outer level. [p] >= 0:
         OP_KETRPOS is only compiled as the ket of the BRAPOS bracket
         family, never OP_BRA/OP_COND (mb invariant), so the P computation
         above took the non-NULL branch. *)
      Array.blit fr (fb + Frames.slot_eptr) fr
        (Frames.base a p + Frames.slot_eptr)
        (a.Frames.frame_size_ints - Frames.slot_eptr);
      (backtrack [@tailcall]) st f match_ketrpos)
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
        (rmatch [@tailcall]) st f (ecode + 1 + Limits.link_size) rm6 0
      else
        (* pcre2_match.c:6117-6119 — repeat the maximum number of times
           (KETRMAX): restart from the preceding bracket first. *)
        (rmatch [@tailcall]) st f bracode rm7 0
    else (
      (* pcre2_match.c:6123-6126 — carry on at this level for a
         non-repeating ket, or after matching an empty string, or after
         repeating for a maximum number of times. *)
      fr.(fb + Frames.slot_ecode) <- ecode + 1 + Limits.link_size;
      (dispatch [@tailcall]) st f)
  and backtrack (st : match_state) (f : int) (rrc : int) : int =
    (* pcre2_match.c:6461-6501 — RETURN_SWITCH: the RRETURN() macro jumps
       here with the return value in rrc; Freturn_id says which L_RM##
       label to resume at, and Frdepth is the frame's index. *)
    let mb = st.mb in
    let a = st.arena in
    let utf = st.utf in
    let ref_length = st.ref_length in
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
         (callout_flags is only ever read by an installed callout function,
         and mb->callout is always NULL); site kept for evaluation order. *)
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
           RM208-RM217 : 3522-3743 property repeat minimize, in PT
             order ANY LAMP GC PC SC ALNUM SPACE WORD CLIST UCNC
           RM218 : 3811 EXTUNI min      RM219 : 3836 type min UTF
           RM220 : 4437 EXTUNI max      RM221 : 4710 type max UTF
           RM222 : 4394 property max    RM223 : 3781 PT_BOOL min
           RM224 : 3762 PT_BIDICL min   RM225 : 3612 PT_SCX min *)
      let fb = Frames.base a f in
      match fr.(fb + Frames.slot_return_id) with
      | 1 ->
          (* L_RM1 (pcre2_match.c:5365-5366) — OP_BRA optimized branch
             walk: the branch failed; move Fecode to the next branch
             (saved in Lnext_branch) and loop. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_temp_sptr_0);
            (bra_loop [@tailcall]) f)
      | 2 ->
          (* L_RM2 (pcre2_match.c:5401-5410) — GROUPLOOP: the branch
             failed. If the result is MATCH_THEN, check whether the
             ( *THEN) is within the current branch by comparing the
             address of the OP_THEN that is passed back with the end of
             the branch; if it is, and the branch is one of two or more
             alternatives, convert to NOMATCH so that normal backtracking
             happens from now on (5202-5211). Then advance to the next
             alternative, failing the group when there is none. *)
          let rrc =
            if Int.equal rrc match_then then
              let ecode = fr.(fb + Frames.slot_ecode) in
              let next_ecode = ecode + Compile.get mb.start_code (ecode + 1) in
              if
                mb.verb_ecode_ptr < next_ecode
                && (Int.equal
                      (Char.code (Bytes.get mb.start_code ecode))
                      Opcodes.op_alt
                   || Int.equal
                        (Char.code (Bytes.get mb.start_code next_ecode))
                        Opcodes.op_alt)
              then match_nomatch
              else rrc
            else rrc
          in
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
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
            then (backtrack [@tailcall]) st f match_nomatch
            else (grouploop [@tailcall]) f
      | 6 ->
          (* L_RM6 (pcre2_match.c:6112-6114) — KETRMIN: the rest of the
             pattern failed; go back to the preceding bracket and iterate
             the group once more in this frame (Fecode -= GET(Fecode, 1)),
             then break out of the ket processing. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let ecode = fr.(fb + Frames.slot_ecode) in
            fr.(fb + Frames.slot_ecode) <-
              ecode - Compile.get mb.start_code (ecode + 1);
            (dispatch [@tailcall]) st f
      | 7 ->
          (* L_RM7 (pcre2_match.c:6120-6126) — KETRMAX: the re-iteration
             failed; carry on at this level after the group
             (Fecode += 1 + LINK_SIZE). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            fr.(fb + Frames.slot_ecode) <-
              fr.(fb + Frames.slot_ecode) + 1 + Limits.link_size;
            (dispatch [@tailcall]) st f)
      | 9 ->
          (* L_RM9 (pcre2_match.c:5227-5229) — BRAZERO: taking the group
             failed; walk Lnext_ecode from the bracket to its final ket
             and continue after the group (zero repeat). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let next = skip_alts fr.(fb + Frames.slot_temp_sptr_0) in
            fr.(fb + Frames.slot_temp_sptr_0) <- next
            (* Lnext_ecode, post-walk *);
            fr.(fb + Frames.slot_ecode) <- next + 1 + Limits.link_size;
            (dispatch [@tailcall]) st f
      | 10 ->
          (* L_RM10 (pcre2_match.c:5236-5237) — BRAMINZERO: the rest of
             the pattern without the group failed; step into the group
             (Fecode++). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_ecode) + 1;
            (dispatch [@tailcall]) st f)
      | 25 ->
          (* L_RM25 (pcre2_match.c:1421-1431) — caseless char repeat,
             minimize: the tail failed; take one more matching character
             and try again. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let cc = Char.code (String.unsafe_get mb.subject eptr) in
                if
                  (not (Int.equal fr.(fb + Frames.slot_temp_32_2) cc))
                  && not (Int.equal fr.(fb + Frames.slot_temp_32_3) cc)
                then (backtrack [@tailcall]) st f match_nomatch
                else (
                  fr.(fb + Frames.slot_eptr) <- eptr + 1;
                  (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm25 0)
      | 26 ->
          (* L_RM26 (pcre2_match.c:1454-1456) — caseless char repeat,
             maximize: Feptr-- BEFORE the rrc test (the C's order), then
             back to the for(;;) head. *)
          fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - 1;
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (repeatchar_ci_maxbt [@tailcall]) f
      | 27 ->
          (* L_RM27 (pcre2_match.c:1481-1489) — caseful char repeat,
             minimize. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                (* pcre2_match.c:1489 — if (Lc != UCHAR21INCTEST(Feptr))
                   RRETURN(MATCH_NOMATCH): post-increment. *)
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let cc = Char.code (String.unsafe_get mb.subject eptr) in
                fr.(fb + Frames.slot_eptr) <- eptr + 1;
                if not (Int.equal fr.(fb + Frames.slot_temp_32_2) cc) then
                  (backtrack [@tailcall]) st f match_nomatch
                else
                  (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm27 0
      | 28 ->
          (* L_RM28 (pcre2_match.c:1511-1513) — caseful char repeat,
             maximize: Feptr-- before the rrc test, then the for(;;)
             head. *)
          fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - 1;
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (repeatchar_cs_maxbt [@tailcall]) f
      | 29 ->
          (* L_RM29 (pcre2_match.c:1697-1706) — caseless NOT repeat,
             minimize. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let cc = Char.code (String.unsafe_get mb.subject eptr) in
                if
                  Int.equal fr.(fb + Frames.slot_temp_32_2) cc
                  || Int.equal fr.(fb + Frames.slot_temp_32_3) cc
                then (backtrack [@tailcall]) st f match_nomatch
                else (
                  fr.(fb + Frames.slot_eptr) <- eptr + 1;
                  (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm29 0)
      | 30 ->
          (* L_RM30 (pcre2_match.c:1766-1768) — caseless NOT repeat,
             maximize: rrc test BEFORE Feptr-- (opposite order to
             RM26/RM28), then the for(;;) head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - 1;
            (repeatnotchar_ci_maxbt [@tailcall]) f)
      | 31 ->
          (* L_RM31 (pcre2_match.c:1836-1844) — caseful NOT repeat,
             minimize. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                (* pcre2_match.c:1844 — if (Lc == *Feptr++)
                   RRETURN(MATCH_NOMATCH): post-increment. *)
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let cc = Char.code (String.unsafe_get mb.subject eptr) in
                fr.(fb + Frames.slot_eptr) <- eptr + 1;
                if Int.equal fr.(fb + Frames.slot_temp_32_2) cc then
                  (backtrack [@tailcall]) st f match_nomatch
                else
                  (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm31 0
      | 32 ->
          (* L_RM32 (pcre2_match.c:1903-1905) — caseful NOT repeat,
             maximize: rrc test before Feptr--, then the for(;;) head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - 1;
            (repeatnotchar_cs_maxbt [@tailcall]) f)
      | 23 ->
          (* L_RM23 (pcre2_match.c:2053-2071) — class repeat, minimize,
             not UTF: the tail failed; take one more matching character
             and try again. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                (* pcre2_match.c:2062 — fc = *Feptr++: post-increment. *)
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let fc = Char.code (String.unsafe_get mb.subject eptr) in
                fr.(fb + Frames.slot_eptr) <- eptr + 1;
                if Int.equal (class_bit st f fc) 0 then
                  (backtrack [@tailcall]) st f match_nomatch
                else
                  (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm23 0
      | 24 ->
          (* L_RM24 (pcre2_match.c:2145-2147) — class repeat, maximize:
             rrc test, then Feptr--, then back to the while head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_eptr) - 1;
            (class_maxbt [@tailcall]) f)
      | 33 ->
          (* L_RM33 (pcre2_match.c:3965-4100) — type repeat, minimize, not
             UTF: the switch(Lctype) is re-tested every iteration (the C
             note at 3507-3510: all four temp_32 slots are in use, so no
             local "notmatch"). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                let lctype = fr.(fb + Frames.slot_temp_32_2) in
                (* pcre2_match.c:3973-3974 *)
                if Int.equal lctype Opcodes.op_any && is_newline_at st eptr then
                  (backtrack [@tailcall]) st f match_nomatch
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
                        (rmatch [@tailcall]) st f
                          fr.(fb + Frames.slot_ecode)
                          rm33 0)
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                  else if
                    (* 3990-3992 *)
                    Int.equal lctype Opcodes.op_allany
                    || Int.equal lctype Opcodes.op_anybyte
                  then
                    (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm33 0
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
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0)
                    else if Int.equal fc Newline.char_lf then
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                    else if
                      Int.equal fc Newline.char_vt
                      || Int.equal fc Newline.char_ff
                      || Int.equal fc Newline.char_nel
                    then
                      if Int.equal mb.bsr_convention Options.bsr_anycrlf then
                        (backtrack [@tailcall]) st f match_nomatch
                      else
                        (rmatch [@tailcall]) st f
                          fr.(fb + Frames.slot_ecode)
                          rm33 0
                    else (backtrack [@tailcall]) st f match_nomatch (* 3996 *)
                  else if Int.equal lctype Opcodes.op_not_hspace then
                    (* 4019-4029 *)
                    if hspace_byte fc then
                      (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                  else if Int.equal lctype Opcodes.op_hspace then
                    (* 4031-4041 *)
                    if hspace_byte fc then
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                    else (backtrack [@tailcall]) st f match_nomatch
                  else if Int.equal lctype Opcodes.op_not_vspace then
                    (* 4043-4053 *)
                    if vspace_byte fc then
                      (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                  else if Int.equal lctype Opcodes.op_vspace then
                    (* 4055-4065 *)
                    if vspace_byte fc then
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                    else (backtrack [@tailcall]) st f match_nomatch
                  else if Int.equal lctype Opcodes.op_not_digit then
                    (* 4067-4070 *)
                    if
                      not
                        (Int.equal
                           (Chartables.ctypes fc land Chartables.ctype_digit)
                           0)
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                  else if Int.equal lctype Opcodes.op_digit then
                    (* 4072-4075 *)
                    if
                      Int.equal
                        (Chartables.ctypes fc land Chartables.ctype_digit)
                        0
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                  else if Int.equal lctype Opcodes.op_not_whitespace then
                    (* 4077-4080 *)
                    if
                      not
                        (Int.equal
                           (Chartables.ctypes fc land Chartables.ctype_space)
                           0)
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                  else if Int.equal lctype Opcodes.op_whitespace then
                    (* 4082-4085 *)
                    if
                      Int.equal
                        (Chartables.ctypes fc land Chartables.ctype_space)
                        0
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                  else if Int.equal lctype Opcodes.op_not_wordchar then
                    (* 4087-4090 *)
                    if
                      not
                        (Int.equal
                           (Chartables.ctypes fc land Chartables.ctype_word)
                           0)
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                  else if Int.equal lctype Opcodes.op_wordchar then
                    (* 4092-4095 *)
                    if
                      Int.equal
                        (Chartables.ctypes fc land Chartables.ctype_word)
                        0
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm33 0
                  else Errors.error_internal (* 4097-4099 — default *)
      | 34 ->
          (* L_RM34 (pcre2_match.c:4963-4967) — type repeat, maximize: rrc
             test, Feptr--, then the \R CRLF double step (backing into the
             middle of a CRLF pair is not a valid \R position), then back
             to the for(;;) head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
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
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let rrc2 =
                match_ref st f
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
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch)
              else (
                fr.(fb + Frames.slot_eptr) <-
                  fr.(fb + Frames.slot_eptr) + !ref_length;
                (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm20 0)
      | 21 ->
          (* L_RM21 (pcre2_match.c:5158-5160) — backref repeat, maximize,
             same lengths: rrc test, then Feptr -= Flength, then back to
             the while head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
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
            (backtrack [@tailcall]) st f rrc
          else if
            Int.equal
              fr.(fb + Frames.slot_eptr)
              fr.(fb + Frames.slot_temp_sptr_0)
          then (backtrack [@tailcall]) st f match_nomatch
          else (
            fr.(fb + Frames.slot_eptr) <- fr.(fb + Frames.slot_temp_sptr_0)
            (* Feptr = Lstart, 5175 *);
            fr.(fb + Frames.slot_temp_32_1) <-
              fr.(fb + Frames.slot_temp_32_1) - 1 (* Lmax--, 5176 *);
            (ref_max_rescan [@tailcall]) f fr.(fb + Frames.slot_temp_32_0))
      | 37 ->
          (* L_RM37 (pcre2_match.c:5877-5882) — OP_VREVERSE: the branch
             failed at this back length; move forward one character (Lmax--
             compares the OLD value and decrements regardless) until we
             reach the minimum back length; in UTF mode the step crosses
             the character's remaining code units (FORWARDCHARTEST,
             5881). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmax = fr.(fb + Frames.slot_temp_32_1) in
            fr.(fb + Frames.slot_temp_32_1) <- lmax - 1 (* Lmax-- *);
            if lmax <= fr.(fb + Frames.slot_temp_32_0) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) + 1 in
              fr.(fb + Frames.slot_eptr) <-
                (if utf then Utf.forwardchartest mb.subject eptr mb.end_subject
                 else eptr);
              (rmatch [@tailcall]) st f
                (fr.(fb + Frames.slot_ecode) + 1 + (2 * Limits.imm2_size))
                rm37 0
      | 3 ->
          (* L_RM3 (pcre2_match.c:5520-5535) — positive assertion: the
             branch backtracked all the way out. MATCH_ACCEPT means
             ( *ACCEPT) ended the assertion with a match: fish the captures
             and mark out of the remembered frame, then continue after the
             group. Anything but NOMATCH/THEN passes back; otherwise try
             the next branch, failing the assertion when there is none.
             PCRE2 doesn't allow the effect of ( *THEN) to escape beyond an
             assertion, so it is treated as NOMATCH (5503-5507). *)
          if Int.equal rrc match_accept then (
            (* pcre2_match.c:5522-5527 — memcpy(Fovector,
               assert_accept_frame->ovector, assert_accept_frame->
               offset_top * sizeof(PCRE2_SIZE)). In bounds:
               [assert_accept_frame] is a valid frame index (MATCH_ACCEPT
               is only produced by the OP_ASSERT_ACCEPT arm, which sets
               it) and offset_top <= 2 * top_bracket in both frames. *)
            let ab = Frames.base a st.assert_accept_frame in
            Array.blit fr (ab + Frames.slot_ovector) fr
              (fb + Frames.slot_ovector)
              fr.(ab + Frames.slot_offset_top);
            fr.(fb + Frames.slot_offset_top) <- fr.(ab + Frames.slot_offset_top);
            fr.(fb + Frames.slot_mark) <- fr.(ab + Frames.slot_mark);
            (* pcre2_match.c:5534-5535 — break out of the branch loop:
               skip to the end of the group and continue after it. *)
            fr.(fb + Frames.slot_ecode) <-
              skip_alts fr.(fb + Frames.slot_ecode) + 1 + Limits.link_size;
            (dispatch [@tailcall]) st f)
          else if
            (not (Int.equal rrc match_nomatch))
            && not (Int.equal rrc match_then)
          then (backtrack [@tailcall]) st f rrc
          else
            (* pcre2_match.c:5530-5531 *)
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
            then (backtrack [@tailcall]) st f match_nomatch
            else (assert_loop [@tailcall]) f
      | 4 ->
          (* L_RM4 (pcre2_match.c:5555-5583) — negative assertion: switch
             (rrc). A match (or assertion ACCEPT) means the assertion
             fails; NOMATCH/THEN try the next branch; COMMIT/SKIP/PRUNE
             force the assertion to fail without checking other branches,
             which is success for a negative assertion (sites in the C's
             case order). *)
          if Int.equal rrc match_accept || Int.equal rrc match_match then
            (* pcre2_match.c:5557-5559 — assertion matched, therefore it
               fails. *)
            (backtrack [@tailcall]) st f match_nomatch
          else if Int.equal rrc match_nomatch || Int.equal rrc match_then then (
            (* pcre2_match.c:5561-5565 — branch failed, try next if
               present. *)
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
            then (
              (* ASSERT_NOT_FAILED (pcre2_match.c:5582-5583) — none of the
                 branches have matched: success for a negative assertion,
                 so carry on. *)
              fr.(fb + Frames.slot_ecode) <- ecode + 1 + Limits.link_size;
              (dispatch [@tailcall]) st f)
            else (assert_not_loop [@tailcall]) f)
          else if
            Int.equal rrc match_commit || Int.equal rrc match_skip
            || Int.equal rrc match_prune
          then (
            (* pcre2_match.c:5567-5571 — assertion forced to fail,
               therefore continue: skip to the end of the group, then
               ASSERT_NOT_FAILED. *)
            let ecode = skip_alts fr.(fb + Frames.slot_ecode) in
            fr.(fb + Frames.slot_ecode) <- ecode + 1 + Limits.link_size;
            (dispatch [@tailcall]) st f)
          else
            (* pcre2_match.c:5573-5574 — pass back any other return. *)
            (backtrack [@tailcall]) st f rrc
      | 8 ->
          (* L_RM8 (pcre2_match.c:5292-5317) — possessive group: the
             iteration came back. MATCH_KETRPOS means one iteration
             matched (its frame data was copied back here by the
             OP_KETRPOS ket): unless it was empty (skip to the end to
             forcibly break the loop), start the next iteration from the
             bracket. A MATCH_THEN from within the current branch of a
             multi-branch group converts to NOMATCH (5305-5313; see the
             RM2 comment). On NOMATCH, walk to the next alternative,
             leaving the loop when there is none. *)
          if Int.equal rrc match_ketrpos then (
            fr.(fb + Frames.slot_temp_32_1) <- 1 (* Lmatched_once = TRUE *);
            if
              Int.equal
                fr.(fb + Frames.slot_eptr)
                fr.(fb + Frames.slot_temp_sptr_0)
            then (
              (* pcre2_match.c:5295-5299 — empty match; skip to end. *)
              fr.(fb + Frames.slot_ecode) <-
                skip_alts fr.(fb + Frames.slot_ecode);
              (possessive_group_done [@tailcall]) f)
            else (
              (* pcre2_match.c:5301-5302 *)
              fr.(fb + Frames.slot_ecode) <- fr.(fb + Frames.slot_temp_sptr_1)
              (* Lstart_group *);
              (possessive_group_loop [@tailcall]) f))
          else
            (* pcre2_match.c:5305-5313 — see the comment at RM2 about
               handling THEN. *)
            let rrc =
              if Int.equal rrc match_then then
                let ecode = fr.(fb + Frames.slot_ecode) in
                let next_ecode =
                  ecode + Compile.get mb.start_code (ecode + 1)
                in
                if
                  mb.verb_ecode_ptr < next_ecode
                  && (Int.equal
                        (Char.code (Bytes.get mb.start_code ecode))
                        Opcodes.op_alt
                     || Int.equal
                          (Char.code (Bytes.get mb.start_code next_ecode))
                          Opcodes.op_alt)
                then match_nomatch
                else rrc
              else rrc
            in
            if not (Int.equal rrc match_nomatch) then
              (* pcre2_match.c:5315 *)
              (backtrack [@tailcall]) st f rrc
            else
              (* pcre2_match.c:5316-5317 *)
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
              then (possessive_group_done [@tailcall]) f
              else (possessive_group_loop [@tailcall]) f
      | 5 ->
          (* L_RM5 (pcre2_match.c:5710-5745) — an assertion-condition
             branch came back: switch (rrc). *)
          if Int.equal rrc match_accept then (
            (* pcre2_match.c:5712-5717 — MATCH_ACCEPT: save captures from
               the frame remembered by OP_ASSERT_ACCEPT. In bounds: as at
               the RM3 resume ([assert_accept_frame] is a valid frame
               index and offset_top <= 2 * top_bracket in both frames). *)
            let ab = Frames.base a st.assert_accept_frame in
            Array.blit fr (ab + Frames.slot_ovector) fr
              (fb + Frames.slot_ovector)
              fr.(ab + Frames.slot_offset_top);
            fr.(fb + Frames.slot_offset_top) <- fr.(ab + Frames.slot_offset_top);
            (* fallthrough from MATCH_ACCEPT in C (5719-5724): in the
               case of a match, the captures have already been put into
               the current frame — condition = Lpositive (TRUE for a
               positive assertion), then out of the branch loop. *)
            (cond_assert_end [@tailcall]) f
              (not (Int.equal fr.(fb + Frames.slot_temp_32_0) 0)))
          else if Int.equal rrc match_match then
            (* pcre2_match.c:5722-5724 — condition = Lpositive. *)
            (cond_assert_end [@tailcall]) f
              (not (Int.equal fr.(fb + Frames.slot_temp_32_0) 0))
          else if Int.equal rrc match_nomatch || Int.equal rrc match_then then (
            (* pcre2_match.c:5726-5734 — PCRE doesn't allow the effect of
               ( *THEN) to escape beyond an assertion; it is therefore
               always treated as NOMATCH. Try the next branch if present;
               otherwise condition = !Lpositive (TRUE for a negative
               assertion). *)
            let sb =
              fr.(fb + Frames.slot_temp_sptr_0)
              + Compile.get mb.start_code (fr.(fb + Frames.slot_temp_sptr_0) + 1)
            in
            fr.(fb + Frames.slot_temp_sptr_0) <- sb (* Lstart_branch *);
            if Int.equal (Char.code (Bytes.get mb.start_code sb)) Opcodes.op_alt
            then (cond_assert_loop [@tailcall]) f
            else
              (cond_assert_end [@tailcall]) f
                (Int.equal fr.(fb + Frames.slot_temp_32_0) 0))
          else if
            Int.equal rrc match_commit || Int.equal rrc match_skip
            || Int.equal rrc match_prune
          then
            (* pcre2_match.c:5736-5742 — these force no match without
               checking other branches: condition = !Lpositive (kept in
               the C's case order). *)
            (cond_assert_end [@tailcall]) f
              (Int.equal fr.(fb + Frames.slot_temp_32_0) 0)
          else
            (* pcre2_match.c:5744-5745 — default: pass back any other
               return. *)
            (backtrack [@tailcall]) st f rrc
      | 11 ->
          (* L_RM11 (pcre2_match.c:5469-5495) — a recursion branch came
             back: next_ecode = Lstart_branch + GET(Lstart_branch, 1).
             Handle backtracking verbs, which are defined in a range that
             can easily be tested for: PCRE does not allow THEN, SKIP,
             PRUNE or COMMIT to escape beyond a recursion; they cause a
             NOMATCH for the entire recursion. When one of these verbs
             triggers, the current recursion group number was recorded in
             mb->verb_current_recurse: if it matches the recursion we are
             processing, the verb happened within the recursion and we
             must deal with it (a THEN within the current branch of a
             multi-branch recursion just fails that branch — see the RM2
             comment); otherwise it happened after the recursion
             completed, and is passed back (5471-5488). Carrying on after
             ( *ACCEPT) in a recursion is handled in the OP_ACCEPT code;
             nothing needs to be done here (5490-5491). Anything but
             NOMATCH passes back; otherwise try the next branch, failing
             the recursion when there is none. *)
          let start_branch = fr.(fb + Frames.slot_temp_sptr_0) in
          let next_ecode =
            start_branch + Compile.get mb.start_code (start_branch + 1)
          in
          if
            rrc >= match_backtrack_min && rrc <= match_backtrack_max
            && Int.equal mb.verb_current_recurse
                 (fr.(fb + Frames.slot_temp_32_0) lxor Frames.gf_recurse)
          then
            if
              Int.equal rrc match_then
              && mb.verb_ecode_ptr < next_ecode
              && (Int.equal
                    (Char.code (Bytes.get mb.start_code start_branch))
                    Opcodes.op_alt
                 || Int.equal
                      (Char.code (Bytes.get mb.start_code next_ecode))
                      Opcodes.op_alt)
            then
              (* rrc = MATCH_NOMATCH (5486): fall into the branch walk
                 below. *)
              (recurse_advance_branch [@tailcall]) f next_ecode
            else (backtrack [@tailcall]) st f match_nomatch
          else if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (recurse_advance_branch [@tailcall]) f next_ecode
      | 35 ->
          (* L_RM35 (pcre2_match.c:5776) — OP_SCOND descend:
             RRETURN(rrc). *)
          (backtrack [@tailcall]) st f rrc
      | 12 ->
          (* L_RM12 (pcre2_match.c:6344-6357) — OP_MARK: a return of
             MATCH_SKIP_ARG means that matching failed at SKIP with an
             argument, and we must check whether that argument matches
             this MARK's argument (passed back in mb->verb_skip_ptr as a
             code offset). If it does, return MATCH_SKIP with
             mb->verb_skip_ptr now pointing to the subject position that
             corresponds to this mark; otherwise pass back the return
             code unaltered. *)
          if
            Int.equal rrc match_skip_arg
            && strcmp_code_eq mb
                 (fr.(fb + Frames.slot_ecode) + 2)
                 mb.verb_skip_ptr
          then (
            mb.verb_skip_ptr <- fr.(fb + Frames.slot_eptr)
            (* pass back current position *);
            (backtrack [@tailcall]) st f match_skip)
          else (backtrack [@tailcall]) st f rrc
      | 13 | 36 ->
          (* L_RM13 (pcre2_match.c:6368-6370) — OP_COMMIT — and L_RM36
             (6375-6377) — OP_COMMIT_ARG: identical resumes. Record the
             current recursing group number in mb->verb_current_recurse,
             so that the recurse processing can catch verbs from within
             the recursion (6362-6364). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            mb.verb_current_recurse <- fr.(fb + Frames.slot_current_recurse);
            (backtrack [@tailcall]) st f match_commit)
      | 14 | 15 ->
          (* L_RM14 (pcre2_match.c:6381-6383) — OP_PRUNE — and L_RM15
             (6388-6390) — OP_PRUNE_ARG: identical resumes. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            mb.verb_current_recurse <- fr.(fb + Frames.slot_current_recurse);
            (backtrack [@tailcall]) st f match_prune)
      | 16 ->
          (* L_RM16 (pcre2_match.c:6394-6397) — OP_SKIP: pass back the
             current subject position in mb->verb_skip_ptr. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            mb.verb_skip_ptr <- fr.(fb + Frames.slot_eptr);
            mb.verb_current_recurse <- fr.(fb + Frames.slot_current_recurse);
            (backtrack [@tailcall]) st f match_skip)
      | 17 ->
          (* L_RM17 (pcre2_match.c:6415-6424) — OP_SKIP_ARG: pass back
             the current skip name (a code offset) and return the special
             MATCH_SKIP_ARG return code. This will either be caught by a
             matching MARK (the RM12 resume), or get to the top, where it
             causes a rematch with mb->ignore_skip_arg set to the value
             of mb->skip_arg_count. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            mb.verb_skip_ptr <- fr.(fb + Frames.slot_ecode) + 2;
            mb.verb_current_recurse <- fr.(fb + Frames.slot_current_recurse);
            (backtrack [@tailcall]) st f match_skip_arg)
      | 18 | 19 ->
          (* L_RM18 (pcre2_match.c:6431-6434) — OP_THEN — and L_RM19
             (6439-6442) — OP_THEN_ARG: identical resumes. Pass back the
             address of the opcode, so that the branch in which it occurs
             can be determined (6426-6428). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            mb.verb_ecode_ptr <- fr.(fb + Frames.slot_ecode);
            mb.verb_current_recurse <- fr.(fb + Frames.slot_current_recurse);
            (backtrack [@tailcall]) st f match_then)
      | 100 ->
          (* RM100 (pcre2_match.c:2233-2248) — OP_XCLASS repeat, minimize:
             the tail failed; take one more matching character and try
             again. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                (* GETCHARINCTEST(fc, Feptr) (2244). *)
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let c0 = Char.code (String.unsafe_get mb.subject eptr) in
                let fc =
                  if utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr
                  else c0
                in
                fr.(fb + Frames.slot_eptr) <-
                  (if utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
                   else eptr + 1);
                if
                  not
                    (Xclass.xclass fc mb.start_code
                       fr.(fb + Frames.slot_temp_sptr_1)
                       utf)
                then (backtrack [@tailcall]) st f match_nomatch
                else
                  (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm100 0
      | 101 ->
          (* RM101 (pcre2_match.c:2278-2287) — OP_XCLASS repeat, maximize:
             if (Feptr-- <= Lstart_eptr) break — the OLD value is
             compared, the decrement happens regardless — then BACKCHAR
             under utf and back to the RMATCH; the break falls to
             RRETURN(MATCH_NOMATCH) (2287). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let eptr = fr.(fb + Frames.slot_eptr) in
            fr.(fb + Frames.slot_eptr) <- eptr - 1;
            if eptr <= fr.(fb + Frames.slot_temp_sptr_0) then
              (backtrack [@tailcall]) st f match_nomatch
            else (
              (* eptr - 1 >= Lstart_eptr >= 0 (guard above);
                 backchar_subject clamps its reads. *)
              if utf then
                fr.(fb + Frames.slot_eptr) <-
                  backchar_subject mb.subject (eptr - 1);
              (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm101 0)
      | 200 ->
          (* RM200 (pcre2_match.c:2028-2047) — class repeat, minimize,
             UTF: take one more matching character (GETCHARINC; chars
             > 255 fail OP_CLASS, match OP_NCLASS) and try again. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let c0 = Char.code (String.unsafe_get mb.subject eptr) in
                let fc =
                  if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
                in
                fr.(fb + Frames.slot_eptr) <-
                  (if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
                   else eptr + 1);
                if fc > 255 then
                  if Int.equal fr.(fb + Frames.slot_op) Opcodes.op_class then
                    (backtrack [@tailcall]) st f match_nomatch
                  else
                    (rmatch [@tailcall]) st f
                      fr.(fb + Frames.slot_ecode)
                      rm200 0
                else if Int.equal (class_bit st f fc) 0 then
                  (backtrack [@tailcall]) st f match_nomatch
                else
                  (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm200 0
      | 201 ->
          (* RM201 (pcre2_match.c:2110-2117) — class repeat, maximize,
             UTF: if (Feptr-- <= Lstart_eptr) break (post-decrement:
             compare the OLD value, decrement regardless), then
             BACKCHAR(Feptr) and back to the RMATCH; the break falls to
             RRETURN(MATCH_NOMATCH) (2150). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let eptr = fr.(fb + Frames.slot_eptr) in
            fr.(fb + Frames.slot_eptr) <- eptr - 1;
            if eptr <= fr.(fb + Frames.slot_temp_sptr_0) then
              (backtrack [@tailcall]) st f match_nomatch
            else (
              (* eptr - 1 >= Lstart_eptr >= 0 (guard above);
                 backchar_subject clamps its reads. *)
              fr.(fb + Frames.slot_eptr) <-
                backchar_subject mb.subject (eptr - 1);
              (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm201 0)
      | 202 ->
          (* RM202 (pcre2_match.c:1315-1329) — wide char repeat, minimize:
             take one more copy of the character (or its other case) and
             try again. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else if repeatchar_wide_try f then
              (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm202 0
            else
              (* CHECK_PARTIAL() (site 1326); then no match. *)
              let feptr = fr.(fb + Frames.slot_eptr) in
              let rc =
                if feptr >= mb.end_subject then scheck_partial mb feptr else 0
              in
              if rc < 0 then rc else (backtrack [@tailcall]) st f match_nomatch
      | 203 ->
          (* RM203 (pcre2_match.c:1361-1363) — wide char repeat, maximize:
             rrc test, then Feptr--; BACKCHAR(Feptr), then back to the
             for(;;) head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            (* eptr - 1 >= Lstart_eptr >= 0 (the loop head only RMATCHes
               when Feptr > Lstart_eptr); backchar_subject clamps its
               reads. *)
            fr.(fb + Frames.slot_eptr) <-
              backchar_subject mb.subject (fr.(fb + Frames.slot_eptr) - 1);
            (repeatchar_wide_maxbt [@tailcall]) f)
      | 204 ->
          (* RM204 (pcre2_match.c:1676-1689) — caseless NOT repeat,
             minimize, UTF. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                (* GETCHARINC(d, Feptr) (1687). *)
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let c0 = Char.code (String.unsafe_get mb.subject eptr) in
                let d =
                  if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
                in
                fr.(fb + Frames.slot_eptr) <-
                  (if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
                   else eptr + 1);
                if
                  Int.equal fr.(fb + Frames.slot_temp_32_2) d
                  || Int.equal fr.(fb + Frames.slot_temp_32_3) d
                then (backtrack [@tailcall]) st f match_nomatch
                else
                  (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm204 0
      | 205 ->
          (* RM205 (pcre2_match.c:1742-1747) — caseless NOT repeat,
             maximize, UTF: rrc test, Feptr--; BACKCHAR(Feptr), then the
             for(;;) head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            (* eptr - 1 >= Lstart_eptr >= 0 (the loop head only RMATCHes
               when Feptr > Lstart_eptr); backchar_subject clamps its
               reads. *)
            fr.(fb + Frames.slot_eptr) <-
              backchar_subject mb.subject (fr.(fb + Frames.slot_eptr) - 1);
            (repeatnotchar_ci_maxbt_utf [@tailcall]) f)
      | 206 ->
          (* RM206 (pcre2_match.c:1816-1829) — caseful NOT repeat,
             minimize, UTF. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                (* GETCHARINC(d, Feptr) (1827). *)
                (* safe: eptr < mb.end_subject <= String.length mb.subject
                   (checked above); 0 <= start_eptr <= eptr (mb
                   invariant) *)
                let c0 = Char.code (String.unsafe_get mb.subject eptr) in
                let d =
                  if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
                in
                fr.(fb + Frames.slot_eptr) <-
                  (if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
                   else eptr + 1);
                if Int.equal fr.(fb + Frames.slot_temp_32_2) d then
                  (backtrack [@tailcall]) st f match_nomatch
                else
                  (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm206 0
      | 207 ->
          (* RM207 (pcre2_match.c:1880-1885) — caseful NOT repeat,
             maximize, UTF: as RM205. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            fr.(fb + Frames.slot_eptr) <-
              backchar_subject mb.subject (fr.(fb + Frames.slot_eptr) - 1);
            (repeatnotchar_cs_maxbt_utf [@tailcall]) f)
      | 219 ->
          if
            (* RM219 (pcre2_match.c:3836-3959) — type repeat, minimize, UTF
               (non-property types): the switch(Lctype) is re-tested every
               iteration. *)
            not (Int.equal rrc match_nomatch)
          then (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                let lctype = fr.(fb + Frames.slot_temp_32_2) in
                (* pcre2_match.c:3846 *)
                if Int.equal lctype Opcodes.op_any && is_newline_at st eptr then
                  (backtrack [@tailcall]) st f match_nomatch
                else
                  (* pcre2_match.c:3847 — GETCHARINC(fc, Feptr). *)
                  (* safe: eptr < mb.end_subject <= String.length
                     mb.subject (checked above); 0 <= start_eptr <= eptr
                     (mb invariant) *)
                  let c0 = Char.code (String.unsafe_get mb.subject eptr) in
                  let fc =
                    if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0
                  in
                  let eptr =
                    if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0
                    else eptr + 1
                  in
                  fr.(fb + Frames.slot_eptr) <- eptr;
                  (* pcre2_match.c:3848-3955 — switch(Lctype). *)
                  if Int.equal lctype Opcodes.op_any then
                    (* 3850-3860 — the non-NL case; take care with CRLF
                       partial. *)
                    if
                      (not (Int.equal mb.partial 0))
                      && eptr >= mb.end_subject
                      && Int.equal mb.nltype Newline.nltype_fixed
                      && Int.equal mb.nllen 2 && Int.equal fc mb.nl0
                    then (
                      mb.hitend <- true;
                      if mb.partial > 1 then Errors.error_partial
                      else
                        (rmatch [@tailcall]) st f
                          fr.(fb + Frames.slot_ecode)
                          rm219 0)
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                  else if
                    (* 3862-3864 *)
                    Int.equal lctype Opcodes.op_allany
                    || Int.equal lctype Opcodes.op_anybyte
                  then
                    (rmatch [@tailcall]) st f
                      fr.(fb + Frames.slot_ecode)
                      rm219 0
                  else if Int.equal lctype Opcodes.op_anynl then
                    (* 3866-3889 *)
                    if Int.equal fc Newline.char_cr then (
                      if
                        eptr < mb.end_subject
                        && Int.equal
                             (* safe: eptr < mb.end_subject (checked) *)
                             (Char.code (String.unsafe_get mb.subject eptr))
                             Newline.char_lf
                      then fr.(fb + Frames.slot_eptr) <- eptr + 1;
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0)
                    else if Int.equal fc Newline.char_lf then
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                    else if
                      Int.equal fc Newline.char_vt
                      || Int.equal fc Newline.char_ff
                      || Int.equal fc Newline.char_nel
                      || Int.equal fc 0x2028 || Int.equal fc 0x2029
                    then
                      if Int.equal mb.bsr_convention Options.bsr_anycrlf then
                        (backtrack [@tailcall]) st f match_nomatch
                      else
                        (rmatch [@tailcall]) st f
                          fr.(fb + Frames.slot_ecode)
                          rm219 0
                    else (backtrack [@tailcall]) st f match_nomatch (* 3869 *)
                  else if Int.equal lctype Opcodes.op_not_hspace then
                    (* 3891-3896 *)
                    if hspace_char fc then
                      (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                  else if Int.equal lctype Opcodes.op_hspace then
                    (* 3898-3903 *)
                    if hspace_char fc then
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                    else (backtrack [@tailcall]) st f match_nomatch
                  else if Int.equal lctype Opcodes.op_not_vspace then
                    (* 3905-3910 *)
                    if vspace_char fc then
                      (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                  else if Int.equal lctype Opcodes.op_vspace then
                    (* 3912-3917 *)
                    if vspace_char fc then
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                    else (backtrack [@tailcall]) st f match_nomatch
                  else if Int.equal lctype Opcodes.op_not_digit then
                    (* 3919-3922 — the 256 boundary in the minimize
                       loops. *)
                    if
                      fc < 256
                      && not
                           (Int.equal
                              (Chartables.ctypes fc land Chartables.ctype_digit)
                              0)
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                  else if Int.equal lctype Opcodes.op_digit then
                    (* 3924-3927 *)
                    if
                      fc >= 256
                      || Int.equal
                           (Chartables.ctypes fc land Chartables.ctype_digit)
                           0
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                  else if Int.equal lctype Opcodes.op_not_whitespace then
                    (* 3929-3932 *)
                    if
                      fc < 256
                      && not
                           (Int.equal
                              (Chartables.ctypes fc land Chartables.ctype_space)
                              0)
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                  else if Int.equal lctype Opcodes.op_whitespace then
                    (* 3934-3937 *)
                    if
                      fc >= 256
                      || Int.equal
                           (Chartables.ctypes fc land Chartables.ctype_space)
                           0
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                  else if Int.equal lctype Opcodes.op_not_wordchar then
                    (* 3939-3942 *)
                    if
                      fc < 256
                      && not
                           (Int.equal
                              (Chartables.ctypes fc land Chartables.ctype_word)
                              0)
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                  else if Int.equal lctype Opcodes.op_wordchar then
                    (* 3944-3947 *)
                    if
                      fc >= 256
                      || Int.equal
                           (Chartables.ctypes fc land Chartables.ctype_word)
                           0
                    then (backtrack [@tailcall]) st f match_nomatch
                    else
                      (rmatch [@tailcall]) st f
                        fr.(fb + Frames.slot_ecode)
                        rm219 0
                  else Errors.error_internal (* 3949-3950 — default *)
      | 221 ->
          (* RM221 (pcre2_match.c:4710-4718) — type repeat, maximize, UTF:
             rrc test, Feptr--; BACKCHAR(Feptr), then the \R CRLF double
             step (backing into the middle of a CRLF pair is not a valid
             \R position), then back to the for(;;) head. The UCHAR21
             probes go through Utf.peek: under NO_UTF_CHECK garbage the
             positions can sit at/past the subject end (the C reads
             unowned memory there). *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            (* eptr - 1 >= Lstart_eptr >= 0 (the loop head only RMATCHes
               when Feptr > Lstart_eptr); backchar_subject clamps its
               reads. *)
            let eptr =
              backchar_subject mb.subject (fr.(fb + Frames.slot_eptr) - 1)
            in
            fr.(fb + Frames.slot_eptr) <- eptr;
            if
              Int.equal fr.(fb + Frames.slot_temp_32_2) Opcodes.op_anynl
              && eptr > fr.(fb + Frames.slot_temp_sptr_0)
              && Int.equal (Utf.peek mb.subject eptr) Newline.char_lf
              && Int.equal (Utf.peek mb.subject (eptr - 1)) Newline.char_cr
            then fr.(fb + Frames.slot_eptr) <- eptr - 1;
            (typemax_utf_bt [@tailcall]) f
      | 208 ->
          (* RM208 (pcre2_match.c:3519-3532) — PT_ANY minimize (the
             prop_test-true == OP_NOTPROP comparison is the C's `if
             (Lctype == OP_NOTPROP) RRETURN(MATCH_NOMATCH)` at 3531). *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_any rm208
      | 209 ->
          (* RM209 (pcre2_match.c:3535-3553) — PT_LAMP minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_lamp rm209
      | 210 ->
          (* RM210 (pcre2_match.c:3556-3570) — PT_GC minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_gc rm210
      | 211 ->
          (* RM211 (pcre2_match.c:3573-3587) — PT_PC minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_pc rm211
      | 212 ->
          (* RM212 (pcre2_match.c:3590-3604) — PT_SC minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_sc rm212
      | 213 ->
          (* RM213 (pcre2_match.c:3629-3645) — PT_ALNUM minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_alnum rm213
      | 214 ->
          (* RM214 (pcre2_match.c:3652-3677) — PT_SPACE/PT_PXSPACE
             minimize (one shared loop in the C too). *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_space rm214
      | 215 ->
          (* RM215 (pcre2_match.c:3680-3700) — PT_WORD minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_word rm215
      | 216 ->
          (* RM216 (pcre2_match.c:3703-3737) — PT_CLIST minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_clist rm216
      | 217 ->
          (* RM217 (pcre2_match.c:3740-3756) — PT_UCNC minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_ucnc rm217
      | 223 ->
          (* RM223 (pcre2_match.c:3776-3795) — PT_BOOL minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_bool rm223
      | 224 ->
          (* RM224 (pcre2_match.c:3759-3773) — PT_BIDICL minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_bidicl rm224
      | 225 ->
          (* RM225 (pcre2_match.c:3607-3626) — PT_SCX minimize. *)
          (prop_min_resume [@tailcall]) f rrc Opcodes.pt_scx rm225
      | 222 ->
          (* RM222 (pcre2_match.c:4391-4398) — property repeat, maximize:
             rrc test, then Feptr--; if (utf) BACKCHAR(Feptr); back to
             the for(;;) head. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else (
            (* eptr - 1 >= Lstart_eptr >= 0 (the loop head only RMATCHes
               when Feptr > Lstart_eptr); backchar_subject clamps its
               reads. *)
            fr.(fb + Frames.slot_eptr) <-
              (if utf then
                 backchar_subject mb.subject (fr.(fb + Frames.slot_eptr) - 1)
               else fr.(fb + Frames.slot_eptr) - 1);
            (propmax_bt [@tailcall]) f)
      | 218 ->
          (* RM218 (pcre2_match.c:3807-3827) — OP_EXTUNI minimize resume:
             if (rrc != MATCH_NOMATCH) RRETURN(rrc); if (Lmin++ >= Lmax)
             RRETURN(MATCH_NOMATCH); bound check + SCHECK_PARTIAL; one
             cluster step; CHECK_PARTIAL; back to the RMATCH. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            let lmin = fr.(fb + Frames.slot_temp_32_0) in
            fr.(fb + Frames.slot_temp_32_0) <- lmin + 1 (* Lmin++ *);
            if lmin >= fr.(fb + Frames.slot_temp_32_1) then
              (backtrack [@tailcall]) st f match_nomatch
            else
              let eptr = fr.(fb + Frames.slot_eptr) in
              if eptr >= mb.end_subject then
                (* pcre2_match.c:3814-3818 — SCHECK_PARTIAL(), then no
                   match. *)
                let rc = scheck_partial mb eptr in
                if rc < 0 then rc
                else (backtrack [@tailcall]) st f match_nomatch
              else
                (* pcre2_match.c:3819-3824 — GETCHARINCTEST(fc, Feptr);
                   Feptr = PRIV(extuni)(...). *)
                let eptr' = extuni_step st eptr in
                fr.(fb + Frames.slot_eptr) <- eptr';
                (* CHECK_PARTIAL() (3825). *)
                let rc =
                  if eptr' >= mb.end_subject then scheck_partial mb eptr' else 0
                in
                if rc < 0 then rc
                else
                  (rmatch [@tailcall]) st f fr.(fb + Frames.slot_ecode) rm218 0
      | 220 ->
          (* RM220 (pcre2_match.c:4437-4466) — OP_EXTUNI maximize resume:
             backtracking over an extended grapheme cluster involves
             inspecting the previous two characters (if present) to see
             if a break is permitted between them. *)
          if not (Int.equal rrc match_nomatch) then
            (backtrack [@tailcall]) st f rrc
          else
            (* pcre2_match.c:4444-4450 — Feptr--; if (!utf) fc = *Feptr;
               else { BACKCHAR(Feptr); GETCHAR(fc, Feptr); }; rgb =
               UCD_GRAPHBREAK(fc). eptr - 1 >= Lstart_eptr >= 0 (the loop
               head only RMATCHes when Feptr > Lstart_eptr);
               backchar_subject/getchar_subject clamp their reads (the
               non-UTF read: eptr - 1 < eptr <= mb.end_subject <=
               String.length mb.subject — non-UTF steps are single code
               units). *)
            let eptr = fr.(fb + Frames.slot_eptr) - 1 in
            let eptr = if utf then backchar_subject mb.subject eptr else eptr in
            let fc =
              if utf then getchar_subject mb.subject eptr
              else Char.code (String.unsafe_get mb.subject eptr)
            in
            fr.(fb + Frames.slot_eptr) <- eptr;
            (extuni_bt_inner [@tailcall]) f (Ucd.gbprop fc)
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
  (new_frame [@tailcall]) st 0 start_ecode 0

(* ---------- Match a Regular Expression (the driver) ---------- *)

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
    (* pcre2_match.c:6648-6654 — initialize UTF/UCP parameters. *)
    let utf = not (Int.equal (re.Compile.overall_options land Options.utf) 0) in
    let allow_invalid =
      not
        (Int.equal
           (re.Compile.overall_options land Options.match_invalid_utf)
           0)
    in
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
      (* pcre2_match.c:6601 + 6608 — start_match and end_subject are the
         C driver locals; refs because the invalid-UTF handling below
         (and, for end_subject, the fragment carry-on's mb writes)
         mutates them. *)
      let start_match = ref start_offset in
      let end_subject = ref length in
      (* pcre2_match.c:6575 — uint32_t fragment_options = 0; set to
         NOTEOL / NOTBOL / NOTBOL|NOTEOL per fragment when handling
         invalid UTF, OR-ed into mb->moptions per attempt (7502). *)
      let fragment_options = ref 0 in
      (* pcre2_match.c:6795 — proceed with non-JIT matching: the default
         is to allow lookbehinds to the start of the subject
         (mb->check_subject = subject); a UTF check with a non-zero
         offset may change this below. *)
      (* pcre2_match.c:6807-6929 — if a UTF subject string was not
         checked for validity, check it here. Only the portion that might
         be inspected during matching is checked — from the offset minus
         the maximum lookbehind (a number of CHARACTERS) to the end.
         Support for invalid UTF (PCRE2_MATCH_INVALID_UTF) forces a
         check, overriding PCRE2_NO_UTF_CHECK. Returns < 0: the error to
         return; >= 0: the check_subject offset. *)
      let check_subject_or_err =
        if
          utf
          && (Int.equal (options land Options.no_utf_check) 0 || allow_invalid)
        then (
          (* pcre2_match.c:6820-6845 — for 8-bit UTF, check that the
             first code unit is a valid character start. If we are
             handling invalid UTF, just skip over such code units.
             Otherwise, give an appropriate error. *)
          let skipped_bad_start = ref false in
          let first_cu_err =
            if allow_invalid then (
              (* pcre2_match.c:6828-6836 *)
              while
                !start_match < !end_subject
                && Utf.not_firstcu (Char.code subject.[!start_match])
              do
                incr start_match;
                skipped_bad_start := true
              done;
              0)
            else if
              (* pcre2_match.c:6837-6845 — check that the first code unit
                 is a valid character start; give an appropriate error
                 otherwise (start_match = subject + start_offset). *)
              !start_match < !end_subject
              && Utf.not_firstcu (Char.code subject.[!start_match])
            then
              if start_offset > 0 then Errors.error_badutfoffset
              else Errors.error_utf8_err20 (* isolated 0x80 byte *)
            else 0
          in
          if first_cu_err < 0 then first_cu_err
          else
            (* pcre2_match.c:6847-6851 — the mb->check_subject field
               points to the start of UTF checking; lookbehinds can go
               back no further than this. *)
            let cs = ref !start_match in
            (* pcre2_match.c:6853-6872 — move back by the maximum
               lookbehind, just in case it happens at the very start of
               matching, but don't do this if we skipped bad code units
               above: for (i = re->max_lookbehind; i > 0 &&
               mb->check_subject > subject; i--) step back one character
               (skipping continuation bytes). *)
            (if not !skipped_bad_start then
               let i = ref re.Compile.max_lookbehind in
               while !i > 0 && !cs > 0 do
                 decr cs;
                 while
                   !cs > 0 && Int.equal (Char.code subject.[!cs] land 0xc0) 0x80
                 do
                   decr cs
                 done;
                 decr i
               done);
            (* pcre2_match.c:6885-6928 — validate the relevant portion of
               the subject. There's a loop in case we encounter bad UTF
               in the characters preceding start_match which we are
               scanning because of a lookbehind. Returns 0 to proceed
               with the match, < 0 to fail with that error. *)
            let rec validate () : int =
              let erroroffset = ref 0 in
              let rc =
                Valid_utf.valid_utf subject ~start:!cs ~length:(length - !cs)
                  erroroffset
              in
              match_data.rc <- rc (* 6891: the out variable *);
              if Int.equal rc 0 then 0 (* valid UTF string (6894) *)
              else (
                (* pcre2_match.c:6896-6903 — invalid UTF string: adjust
                   the offset to be absolute in the whole string. If we
                   are handling invalid UTF strings, set end_subject to
                   stop before the bad code unit; otherwise return the
                   error. The C's `match_data->rc > 0` guard cannot
                   fire: valid_utf returns 0 or a negative UTF8_ERR
                   code. *)
                match_data.startchar <- !erroroffset + !cs;
                if not allow_invalid then rc
                else (
                  end_subject := match_data.startchar;
                  if !end_subject < !start_match then (
                    (* pcre2_match.c:6905-6918 — the end precedes
                       start_match: there is invalid UTF in the extra
                       code units we reversed over because of a
                       lookbehind. Advance past the first bad code unit,
                       then skip invalid character starting code units,
                       and try again with the original end point. *)
                    cs := !end_subject + 1;
                    while
                      !cs < !start_match
                      && Utf.not_firstcu (Char.code subject.[!cs])
                    do
                      incr cs
                    done;
                    end_subject := true_end_subject;
                    (validate [@tailcall]) ())
                  else (
                    (* pcre2_match.c:6920-6926 — otherwise, set the not
                       end of line option, and do the match. *)
                    fragment_options := Options.noteol;
                    0)))
            in
            let v = validate () in
            if v < 0 then v else !cs)
        else 0 (* mb->check_subject = subject (6795) *)
      in
      if check_subject_or_err < 0 then check_subject_or_err
      else
        let check_subject = check_subject_or_err in
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
          && not
               (Int.equal (re.Compile.overall_options land Options.firstline) 0)
        in
        let startline =
          not (Int.equal (re.Compile.flags land Compile.startline) 0)
        in
        (* mcontext->offset_limit is PCRE2_UNSET in the default match context
           (the offset-limit knob is M8+). *)
        let bumpalong_limit = true_end_subject in
        (* pcre2_match.c:6947-6960 — the callout block and the mb callout
           fields: no callout block exists in this port (mb->callout is
           always NULL — the C fields it feeds are only read by an
           installed callout function, see [do_callout_length]). *)
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
            if Limits.match_limit < re.Compile.limit_match then
              Limits.match_limit
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
          match
            Frames.create ~top_bracket:re.Compile.top_bracket ~heap_limit
          with
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
                    not (Int.equal (re.Compile.flags land Compile.hasthen) 0)
                    (* 6966 *);
                  allowemptypartial =
                    re.Compile.max_lookbehind > 0
                    || not
                         (Int.equal
                            (re.Compile.flags land Compile.match_empty)
                            0)
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
                  check_subject (* 6795 / 6851: computed above *);
                  end_subject =
                    !end_subject
                    (* 6964; shortened to the first fragment's end above
                       when handling invalid UTF *);
                  true_end_subject (* 6965 *);
                  end_match_ptr = 0;
                  start_used_ptr = 0;
                  last_used_ptr = 0;
                  mark = Frames.unset;
                  nomatch_mark = Frames.unset (* 6971: in case never set *);
                  verb_ecode_ptr = Frames.unset;
                  verb_skip_ptr = Frames.unset;
                  verb_current_recurse =
                    Frames.recurse_unset
                    (* the C leaves these three uninitialized: they are only
                       read after a verb return has set them *);
                  moptions = 0 (* 6956-6957: gets set later, per attempt *);
                  poptions = re.Compile.overall_options (* 6969 *);
                  skip_arg_count = 0 (* reset per attempt, 7508 *);
                  ignore_skip_arg = 0 (* 6970 *);
                  nltype = !nltype;
                  nllen = !nllen;
                  nl0 = !nl0;
                  nl1 = !nl1;
                }
              in
              (* The per-exec [match_state] threaded through [match_]
                 (DEVIATION (perf) — see the type). utf/ucp: mb.poptions =
                 re->overall_options (pcre2_match.c:6969), so the values
                 computed above (6648-6654) equal match()'s own derivation
                 from mb->poptions (630-637). *)
              let st =
                {
                  mb;
                  arena = a;
                  match_data;
                  top_bracket = re.Compile.top_bracket;
                  utf;
                  ucp;
                  nl_scratch = ref 0;
                  ref_length = ref 0;
                  branch_end = -1;
                  assert_accept_frame = -1;
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
                     || Int.equal (Char.code subject.[p - mb.nllen + 1]) mb.nl1
                     )
              in
              (* pcre2_match.c:7085-7089 — pointers to the individual
                 character tables: dropped, this port always reads the
                 default tables through Chartables (as in Compile). *)
              (* pcre2_match.c:7091-7112 — set up the first code unit to
                 match, if available. If there's no first code unit there may
                 be a bitmap of possible first characters (filled in by
                 Study.set_start_bits when it flags PCRE2_FIRSTMAPSET).
                 first_cu/first_cu2 are PCRE2_UCHAR: the C assignments
                 truncate to the code-unit width (land 0xff). *)
              let has_first_cu =
                not (Int.equal (re.Compile.flags land Compile.firstset) 0)
              in
              let first_cu, first_cu2 =
                if has_first_cu then
                  let fc =
                    re.Compile.first_codeunit land 0xff (* 7097 cast *)
                  in
                  if
                    not
                      (Int.equal
                         (re.Compile.flags land Compile.firstcaseless)
                         0)
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
                  let rc_ =
                    re.Compile.last_codeunit land 0xff (* 7119 cast *)
                  in
                  if
                    not
                      (Int.equal (re.Compile.flags land Compile.lastcaseless) 0)
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
                 bump-along attempts, reset at FRAGMENT_RESTART (7140).
                 start_partial/match_partial are subject positions with
                 -1 = NULL; the 8-bit memchr caches likewise. The initial
                 values here are dead: [fragment_restart] (re)sets them
                 on entry. *)
              let start_partial = ref (-1) in
              let match_partial = ref (-1) in
              let memchr_found_first_cu = ref (-1) in
              let memchr_found_first_cu2 = ref (-1) in
              (* pcre2_match.c:7151-7617 + 7637-7768 — the bump-along for(;;)
                 loop and its ENDLOOP epilogue, as mutually tail-recursive
                 functions (port-conventions §2): [fragment_restart] is the
                 FRAGMENT_RESTART label + fall-through loop-state resets
                 (7139-7148); [bump_top] is the loop head
                 (the start-of-match optimizations, 7155-7481);
                 [first_cu_tail] the shared "required first code unit not
                 found" break check (7300-7315, 7368-7374); [tail_opts] the
                 minlength/req_cu block (7378-7481); [attempt] the bumpalong
                 limit check, per-attempt resets, match() call and rc switch
                 (7485-7577); [bump_bottom] the loop bottom (7579-7616);
                 [endloop] the ENDLOOP invalid-UTF fragment carry-on check
                 (7639-7648) with [next_fragment] as its for(;;) body
                 (7650-7699); [endloop_tail] the epilogue proper
                 (7703-7768). Every C `break` / `goto ENDLOOP` with rc
                 becomes a tail call to [endloop]; req_cu_ptr is threaded
                 through it because the C local survives a
                 `goto FRAGMENT_RESTART`. *)
              let rec fragment_restart (start_match : int) (req_cu_ptr : int) :
                  int =
                (* pcre2_match.c:7139-7148 — FRAGMENT_RESTART: (re)set the
                   per-fragment loop state, then enter the bump-along
                   loop. Also the initial entry point (the C falls
                   through the label). *)
                start_partial := -1;
                match_partial := -1;
                mb.hitend <- false (* 7144 *);
                memchr_found_first_cu := -1;
                memchr_found_first_cu2 := -1;
                (bump_top [@tailcall]) start_match req_cu_ptr
              and bump_top (start_match : int) (req_cu_ptr : int) : int =
                (* pcre2_match.c:7155-7162 — the start-of-match optimizations
                   can be disabled at compile time. *)
                if
                  not
                    (Int.equal
                       (re.Compile.overall_options
                      land Options.no_start_optimize)
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
                          req_cu_ptr
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
                        else if Int.equal !memchr_found_first_cu end_subject
                        then -1
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
                        else if Int.equal !memchr_found_first_cu2 end_subject
                        then -1
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
                            && Int.equal
                                 (Char.code subject.[!sm] land 0xc0)
                                 0x80
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
                  (endloop [@tailcall]) match_nomatch start_match req_cu_ptr
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
                    (endloop [@tailcall]) match_nomatch start_match req_cu_ptr
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
                              memchr_subject subject p req_cu
                                (mb.end_subject - p)
                            in
                            if r < 0 then mb.end_subject else r
                        in
                        if p >= mb.end_subject then
                          (* pcre2_match.c:7464-7471 — if we can't find the
                             required code unit, break the bumpalong loop,
                             forcing a match failure. *)
                          (endloop [@tailcall]) match_nomatch start_match
                            req_cu_ptr
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
                  (endloop [@tailcall]) match_nomatch start_match req_cu_ptr
                else (
                  (* pcre2_match.c:7493-7497 — cb.start_match and
                     PCRE2_CALLOUT_STARTMATCH: no callout block exists in
                     this port (only an installed callout function would
                     read them, and mb->callout is always NULL). *)
                  (* pcre2_match.c:7499-7508 — per-attempt mb resets;
                     mb->moptions = options | fragment_options (7502). *)
                  mb.start_used_ptr <- start_match;
                  mb.last_used_ptr <- start_match;
                  mb.moptions <- options lor !fragment_options;
                  mb.match_call_count <- 0;
                  mb.end_offset_top <- 0;
                  mb.skip_arg_count <- 0;
                  (* pcre2_match.c:7514-7515 — run the match. *)
                  let rc = match_ st ~start_eptr:start_match ~start_ecode:0 in
                  (* pcre2_match.c:7521-7525 — if "hitend" is set, remember
                     the first starting point for which a partial match was
                     found. *)
                  if mb.hitend && !start_partial < 0 then (
                    start_partial := mb.start_used_ptr;
                    match_partial := start_match);
                  (* pcre2_match.c:7527-7577 — switch (rc). *)
                  if Int.equal rc match_skip_arg then (
                    (* pcre2_match.c:7529-7539 — if MATCH_SKIP_ARG reaches
                       this level it means that a MARK that matched the
                       SKIP's arg was not found. In this circumstance, Perl
                       ignores the SKIP entirely: re-do the match at the
                       same point, with a flag to force SKIP with an
                       argument to be ignored. Just treating this case as
                       NOMATCH does not work because it does not check
                       other alternatives in patterns such as
                       A( *SKIP:A)B|AC when the subject is AC. *)
                    mb.ignore_skip_arg <- mb.skip_arg_count;
                    (bump_bottom [@tailcall]) start_match start_match req_cu_ptr)
                  else if
                    Int.equal rc match_skip && mb.verb_skip_ptr > start_match
                  then
                    (* pcre2_match.c:7541-7549 — SKIP passes back the next
                       starting point explicitly; if it is no greater than
                       the match we have just done, fall through and treat
                       it as NOMATCH. *)
                    (bump_bottom [@tailcall]) start_match mb.verb_skip_ptr
                      req_cu_ptr
                  else if
                    Int.equal rc match_nomatch || Int.equal rc match_prune
                    || Int.equal rc match_then
                    || Int.equal rc match_skip (* fallthrough from 7550 *)
                  then (
                    (* pcre2_match.c:7552-7565 — NOMATCH and PRUNE advance by
                       one character; THEN at this level acts exactly like
                       PRUNE. Unset ignore SKIP-with-argument. *)
                    mb.ignore_skip_arg <- 0;
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
                    (endloop [@tailcall]) match_nomatch start_match req_cu_ptr
                  else
                    (* pcre2_match.c:7573-7576 — any other return is either a
                       match, or some kind of error. *)
                    (endloop [@tailcall]) rc start_match req_cu_ptr)
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
                  (endloop [@tailcall]) match_nomatch start_match req_cu_ptr
                else
                  (* pcre2_match.c:7590-7592 — advance to new matching
                     position. *)
                  let start_match = new_start_match in
                  (* pcre2_match.c:7594-7597 — break the loop if the pattern
                     is anchored or if we have passed the end of the
                     subject. *)
                  if anchored || start_match > mb.end_subject then
                    (endloop [@tailcall]) match_nomatch start_match req_cu_ptr
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
              and endloop (rc : int) (start_match : int) (req_cu_ptr : int) :
                  int =
                (* pcre2_match.c:7637-7648 — ENDLOOP. If end_subject !=
                   true_end_subject, it means we are handling invalid UTF,
                   and have just processed a non-terminal fragment. If
                   this resulted in no match or a partial match we must
                   carry on to the next fragment (a partial match is
                   returned to the caller only at the very end of the
                   subject). *)
                if
                  utf
                  && (not (Int.equal mb.end_subject true_end_subject))
                  && (Int.equal rc match_nomatch
                     || Int.equal rc Errors.error_partial)
                then (next_fragment [@tailcall]) mb.end_subject req_cu_ptr
                else (endloop_tail [@tailcall]) rc start_match
              and next_fragment (frag_end : int) (req_cu_ptr : int) : int =
                (* pcre2_match.c:7650-7699 — the fragment carry-on
                   for(;;): a loop is used to avoid trying to match
                   against empty fragments; if the pattern can match an
                   empty string it would have done so already. Each
                   iteration enters with the previous fragment's end in
                   [frag_end] (the C's end_subject). *)
                (* pcre2_match.c:7652-7659 — advance past the first bad
                   code unit, and then skip invalid character starting
                   code units in 8-bit mode. *)
                let sm = ref (frag_end + 1) in
                while
                  !sm < true_end_subject
                  && Utf.not_firstcu (Char.code subject.[!sm])
                do
                  incr sm
                done;
                if !sm >= true_end_subject then (
                  (* pcre2_match.c:7662-7670 — we have hit the end of the
                     subject: there isn't another non-empty fragment, so
                     give up. rc = MATCH_NOMATCH in case it was partial;
                     match_partial = NULL. *)
                  match_partial := -1;
                  (endloop_tail [@tailcall]) match_nomatch !sm)
                else (
                  (* pcre2_match.c:7672-7676 — check the rest of the
                     subject. *)
                  mb.check_subject <- !sm;
                  let erroroffset = ref 0 in
                  let vrc =
                    Valid_utf.valid_utf subject ~start:!sm
                      ~length:(length - !sm) erroroffset
                  in
                  if Int.equal vrc 0 then (
                    (* pcre2_match.c:7678-7685 — the rest of the subject
                       is valid UTF. *)
                    mb.end_subject <- true_end_subject;
                    fragment_options := Options.notbol;
                    (fragment_restart [@tailcall]) !sm req_cu_ptr)
                  else (
                    (* pcre2_match.c:7687-7698 — a subsequent UTF error
                       has been found (valid_utf returns 0 or a negative
                       code, so the C's rc-sign split is 0 / < 0): if the
                       next fragment is non-empty, set up to process it;
                       otherwise let the loop advance. The C wrote the
                       fragment-relative error offset into
                       match_data->startchar through the out-pointer at
                       7675-7676 (dead unless an error return follows). *)
                    match_data.startchar <- !erroroffset;
                    mb.end_subject <- !sm + !erroroffset;
                    if mb.end_subject > !sm then (
                      fragment_options := Options.notbol lor Options.noteol;
                      (fragment_restart [@tailcall]) !sm req_cu_ptr)
                    else (next_fragment [@tailcall]) mb.end_subject req_cu_ptr))
              and endloop_tail (rc : int) (start_match : int) : int =
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
                    (if mb.last_used_ptr > mb.end_match_ptr then
                       mb.last_used_ptr
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
              (* pcre2_match.c:6601-6602 + 7139-7151 — enter the bumpalong
                 loop through the FRAGMENT_RESTART fall-through resets:
                 start_match is subject + start_offset possibly advanced
                 past bad starting code units (6828-6836); req_cu_ptr is
                 one before the ORIGINAL start_match (6602, set before
                 the UTF checks). *)
              (fragment_restart [@tailcall]) !start_match (start_offset - 1))

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
        check_subject =
          0 (* pcre2_match.c:6795 — no UTF check in this test-only entry *);
        end_subject = String.length subject (* pcre2_match.c:6964 *);
        true_end_subject = String.length subject (* pcre2_match.c:6965 *);
        end_match_ptr = 0;
        start_used_ptr = start (* pcre2_match.c:7499 *);
        last_used_ptr = start (* pcre2_match.c:7500 *);
        mark = Frames.unset;
        nomatch_mark = Frames.unset (* pcre2_match.c:6971 *);
        verb_ecode_ptr = Frames.unset;
        verb_skip_ptr = Frames.unset;
        verb_current_recurse = Frames.recurse_unset;
        moptions (* pcre2_match.c:7504 *);
        poptions (* pcre2_match.c:6969 *);
        skip_arg_count = 0 (* pcre2_match.c:7508 *);
        ignore_skip_arg = 0 (* pcre2_match.c:6970 *);
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
        (* The per-exec [match_state] (DEVIATION (perf) — see the type).
           utf/ucp: mb.poptions = poptions here, so this equals match()'s
           derivation from mb->poptions (pcre2_match.c:630-637). *)
        let st =
          {
            mb;
            arena = a;
            match_data;
            top_bracket;
            utf = not (Int.equal (poptions land Options.utf) 0);
            ucp = not (Int.equal (poptions land Options.ucp) 0);
            nl_scratch = ref 0;
            ref_length = ref 0;
            branch_end = -1;
            assert_accept_frame = -1;
          }
        in
        let rc = match_ st ~start_eptr:start ~start_ecode:0 in
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
  (* Lookaround / atomic / possessive opcodes now guarding live behavior
     (lookaround-atomic-possessive chunk). *)
  assert (Int.equal Opcodes.op_reverse 125);
  assert (Int.equal Opcodes.op_vreverse 126);
  assert (Int.equal Opcodes.op_braposzero 153);
  assert (Int.equal Opcodes.op_assert_accept 165);
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
      Opcodes.op_lengths.(Opcodes.op_cbra)
      Opcodes.op_lengths.(Opcodes.op_cbrapos));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_scbra)
      (1 + Limits.link_size + Limits.imm2_size));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_scbrapos)
      (1 + Limits.link_size + Limits.imm2_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_brapos) (1 + Limits.link_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_sbrapos) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_assert) (1 + Limits.link_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_assert_not) (1 + Limits.link_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_assertback) (1 + Limits.link_size));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_assertback_not)
      (1 + Limits.link_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_assert_na) (1 + Limits.link_size));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_assertback_na)
      (1 + Limits.link_size));
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
  (* hspace_char/vspace_char (the UTF loops' full HSPACE_CASES/
     VSPACE_CASES) agree with the complete lists over every value the
     decoders can produce in that range (the lists' NOTACHAR terminator
     never equals a code point). *)
  for c = 0 to 0x3100 do
    assert (
      Bool.equal (hspace_char c)
        (Array.exists (fun v -> Int.equal v c) Tables.hspace_list));
    assert (
      Bool.equal (vspace_char c)
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
  (* Conditional / recursion opcodes now guarding live behavior
     (conditionals/recursion match chunk): the condition opcodes read
     inside OP_COND (pcre2_internal.h:1607-1612), OP_RECURSE, OP_FAIL,
     RREF_ANY (pcre2_internal.h:1816), and the OP_lengths entries the
     condition dispatch steps by (pcre2_internal.h:1786-1803). *)
  assert (Int.equal Opcodes.op_recurse 117);
  assert (Int.equal Opcodes.op_dncref 146);
  assert (Int.equal Opcodes.op_rref 147);
  assert (Int.equal Opcodes.op_dnrref 148);
  assert (Int.equal Opcodes.op_false 149);
  assert (Int.equal Opcodes.op_fail 163);
  assert (Int.equal Opcodes.rref_any 0xffff);
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_recurse) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_cond) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_scond) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_cref) (1 + Limits.imm2_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_dncref) (1 + (2 * Limits.imm2_size)));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_rref) (1 + Limits.imm2_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_dnrref) (1 + (2 * Limits.imm2_size)));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_false) 1);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_true) 1);
  (* Backtracking-verb opcodes now guarding live behavior (verbs match
     chunk), and the OP_lengths entries the verb dispatch and OP_CLOSE
     step by (pcre2_internal.h:1805-1810). *)
  assert (Int.equal Opcodes.op_mark 154);
  assert (Int.equal Opcodes.op_prune 155);
  assert (Int.equal Opcodes.op_prune_arg 156);
  assert (Int.equal Opcodes.op_skip 157);
  assert (Int.equal Opcodes.op_skip_arg 158);
  assert (Int.equal Opcodes.op_then 159);
  assert (Int.equal Opcodes.op_then_arg 160);
  assert (Int.equal Opcodes.op_commit 161);
  assert (Int.equal Opcodes.op_commit_arg 162);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_mark) 3);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_prune) 1);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_prune_arg) 3);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_skip) 1);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_skip_arg) 3);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_then) 1);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_then_arg) 3);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_commit) 1);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_commit_arg) 3);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_close) (1 + Limits.imm2_size));
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

(* Malformed programs: OP_DEFINE has no case in the C switch ->
   PCRE2_ERROR_INTERNAL. *)
let () =
  match
    match_internal ~code:(mk_code [ Opcodes.op_define ]) ~top_bracket:0 "a" 0
  with
  | rc, _ -> assert (Int.equal rc Errors.error_internal)

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
      check_subject = 0;
      end_subject = String.length subject;
      true_end_subject = String.length subject;
      end_match_ptr = 0;
      start_used_ptr = start;
      last_used_ptr = start;
      mark = Frames.unset;
      nomatch_mark = Frames.unset;
      verb_ecode_ptr = Frames.unset;
      verb_skip_ptr = Frames.unset;
      verb_current_recurse = Frames.recurse_unset;
      moptions;
      poptions = 0;
      skip_arg_count = 0;
      ignore_skip_arg = 0;
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
        (* The per-exec [match_state]; utf/ucp from mb.poptions = 0 here
           (pcre2_match.c:630-637). *)
        let st =
          {
            mb;
            arena = a;
            match_data =
              {
                ovector = Array.make 2 Frames.unset;
                oveccount = 1;
                rc = 0;
                startchar = 0;
                leftchar = 0;
                rightchar = 0;
                mark = Frames.unset;
              };
            top_bracket = 0;
            utf = false;
            ucp = false;
            nl_scratch = ref 0;
            ref_length = ref 0;
            branch_end = -1;
            assert_accept_frame = -1;
          }
        in
        match_ st ~start_eptr:mb.start_used_ptr ~start_ecode:0
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

(* Lookaround, atomic groups and possessive brackets (this chunk): the
   OP_ASSERT*/OP_ASSERT*_NOT dispatch arms with RM3/RM4, OP_ASSERT_ACCEPT
   plumbing, OP_REVERSE/OP_VREVERSE (RM37), the assertion/ONCE ket actions
   (eptr restore + atomic backtrack discard), and the BRAPOS possessive
   protocol (RM8 + OP_KETRPOS). Whole compiled patterns through the
   [pcre2_match] driver; EVERY expected value below is pinned against the
   C oracle (pcre2test on the real 10.44 library). *)
let () =
  let compile pat =
    match Compile.pcre2_compile pat ~options:0 with
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
  (* Positive lookahead with captures inside AND after: the frontier unit
     testinput1:28 — oracle: 0: abde, 1: de, 2: abd, 3: e (captures made
     inside a matched positive assertion persist: execution continues
     forward in the deeper frames). *)
  expect_ov (compile "^(?=ab(de))(abd)(e)") "abde" [| 0; 4; 2; 4; 0; 3; 3; 4 |];
  expect_ov (compile "(?=(a))a") "a" [| 0; 1; 0; 1 |];
  (* Negative lookahead — oracle: /(?!x)a/ on "a" -> 0: a. *)
  expect_ov (compile "(?!x)a") "a" [| 0; 1 |];
  (* Captures made inside a FAILED branch of a negative assertion roll
     back (they live in discarded deeper frames) — oracle: 0: ac,
     group 1 unset. *)
  expect_ov (compile "(?!(a)b)ac") "ac" [| 0; 2; -1; -1 |];
  (* Positive lookbehind (OP_REVERSE fixed step) — oracle: 0: c at 2. *)
  expect_ov (compile "(?<=ab)c") "abc" [| 2; 3 |];
  (* Negative lookbehind, incl. the "not enough characters before the
     start" NOMATCH inside the branch making the assertion succeed —
     oracle: 0: b / no match. *)
  (let re = compile "(?<!a)b" in
   expect_ov re "cb" [| 1; 2 |];
   expect_ov re "b" [| 0; 1 |];
   expect_nomatch re "ab");
  (* Lookahead and partial matching: SCHECK_PARTIAL fires inside the
     assertion — oracle: Partial match: x / 0: x. *)
  (let re = compile "x(?=y)" in
   expect_partial ~options:Options.partial_hard re "x" 0 1;
   expect_ov re "xy" [| 0; 1 |]);
  expect_partial ~options:Options.partial_hard (compile "(?=abc)") "ab" 0 2;
  (* Atomic group: no backing into the consumed run — oracle: no match /
     0: aab, and the captured run survives the ket — oracle: 1: aa. *)
  expect_nomatch (compile "(?>a+)ab") "aaab";
  expect_ov (compile "(?>a+)b") "aab" [| 0; 3 |];
  expect_ov (compile "(?>(a+))b") "aab" [| 0; 3; 0; 2 |];
  (* The ONCE ket's P->ecode adjustment: remaining branches within the
     atomic group are not tried — oracle: no match. *)
  expect_nomatch (compile "(?>a|ab)c") "abc";
  (* Possessive brackets (BRAPOS protocol): one committed iteration at a
     time — oracle: 0: aaab, 1: aaa / no match. *)
  (let re = compile "(a+)*+b" in
   expect_ov re "aaab" [| 0; 4; 0; 3 |];
   expect_nomatch re "aaac");
  (* CBRAPOS per-iteration capture, carried by the OP_KETRPOS frame copy —
     oracle: 0: aab, 1: a (the last iteration). *)
  expect_ov (compile "(a)*+b") "aab" [| 0; 3; 1; 2 |];
  (* Captures from distinct branches both survive across iterations —
     oracle: 0: abc, 1: a, 2: b. *)
  expect_ov (compile "(?:(a)|(b))*+c") "abc" [| 0; 3; 0; 1; 1; 2 |];
  (* The empty-iteration break (Feptr == Lstart_eptr skips to the end) —
     oracle: 0: b, 1: "" / 0: aab, 1: "" (after the aa iteration, the
     empty one). *)
  (let re = compile "(a*)*+b" in
   expect_ov re "b" [| 0; 1; 0; 0 |];
   expect_ov re "aab" [| 0; 3; 2; 2 |]);
  (* BRAPOSZERO zero-repeat: the group of /(?:a|ab)*+c/ matches zero times
     at offset 2 after the committed 'a' iteration kills offset 0 —
     oracle: 0: c. *)
  expect_ov (compile "(?:a|ab)*+c") "abc" [| 2; 3 |];
  (* Variable lookbehind (OP_VREVERSE + RM37): maximum length first —
     oracle: 0: x / no match (min 2 > 1 available). *)
  (let re = compile "(?<=a{2,4})x" in
   expect_ov re "aaax" [| 3; 4 |];
   expect_nomatch re "ax");
  (* Perl-compatible maximum-length rule for captures in a variable
     lookbehind — oracle: 0: x, 1: aaaa. *)
  expect_ov (compile "(?<=(a{2,4}))x") "aaaaax" [| 5; 6; 1; 5 |];
  (* Lookbehind captures survive; hard partial does not fire once the
     match completes — oracle: 0: d, 1: abc. *)
  expect_ov ~options:Options.partial_hard (compile "(?<=(abc))d") "abcd"
    [| 3; 4; 0; 3 |]

(* Conditionals and recursion (this chunk): the OP_COND/OP_SCOND
   condition opcodes (OP_CREF/OP_DNCREF/OP_RREF/OP_DNRREF/OP_FALSE/
   OP_TRUE), the assertion-condition protocol (RM5 + the GF_CONDASSERT
   ket return), OP_RECURSE with the RM11 branch loop and the recursion
   ket actions (whole-pattern end + recursed-group capture reinstate,
   i.e. Perl's captures-discard-on-exit semantics), RECURSELOOP (-52)
   detection with its PCRE2_DISABLE_RECURSELOOP_CHECK gate, OP_FAIL, and
   the quantified-recursion compile case. Whole compiled patterns through
   the [pcre2_match] driver (limit knobs via [match_internal]); EVERY
   expected value below is pinned against the C oracle (pcre2test on the
   real 10.44 library). *)
let () =
  let compile pat =
    match Compile.pcre2_compile pat ~options:0 with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run (re : Compile.re) subj =
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
    let rc = pcre2_match re ~subject:subj ~start_offset:0 ~options:0 m in
    (rc, m)
  in
  let expect_ov re subj expected =
    match run re subj with
    | rc, m ->
        assert (rc > 0);
        assert (Int.equal (Array.length m.ovector) (Array.length expected));
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch re subj =
    match run re subj with rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let expect_rc re subj code =
    match run re subj with rc, _ -> assert (Int.equal rc code)
  in
  (* Group-set condition (OP_CREF), both paths — oracle: aA -> 0: aA,
     1: a; bB -> 0: bB, 1: <unset>; aB and bA -> no match. *)
  (let re = compile "(?:(a)|b)(?(1)A|B)" in
   expect_ov re "aA" [| 0; 2; 0; 1 |];
   expect_ov re "bB" [| 0; 2; -1; -1 |];
   expect_nomatch re "aB";
   expect_nomatch re "bA");
  (* Unset optional group takes the false branch — oracle: 0: c,
     1: <unset>. *)
  (let re = compile "(a)?(?(1)b|c)" in
   expect_ov re "ab" [| 0; 2; 0; 1 |];
   expect_ov re "c" [| 0; 1; -1; -1 |]);
  (* Named-group condition (a non-duplicated name compiles to OP_CREF) —
     oracle: 0: ab, 1: a. *)
  expect_ov (compile "(?<n>a)(?(<n>)b|c)") "ab" [| 0; 2; 0; 1 |];
  (* Duplicate-name condition (the OP_DNCREF group-list scan) — oracle:
     aX -> 1: a, 2: <unset>; bX -> 1: <unset>, 2: b. *)
  (let re = compile "(?J)(?:(?<n>a)|(?<n>b))(?(<n>)X|Y)" in
   expect_ov re "aX" [| 0; 2; 0; 1; -1; -1 |];
   expect_ov re "bX" [| 0; 2; -1; -1; 0; 1 |]);
  (* Recursion condition (OP_RREF): false outside a recursion — oracle:
     0: x — and true inside one — oracle: 0: ba. *)
  expect_ov (compile "(?(R)r|x)") "x" [| 0; 1 |];
  expect_ov (compile "(?(R)a|b(?R))") "ba" [| 0; 2 |];
  (* Assertion conditions (GF_CONDASSERT frames + RM5; the positive kind
     rewrites Fecode past the assertion when true) — oracle: ab / x
     match, a fails; the negative kind picks the first branch on x. *)
  (let re = compile "(?(?=ab)ab|x)" in
   expect_ov re "ab" [| 0; 2 |];
   expect_ov re "x" [| 0; 1 |];
   expect_nomatch re "a");
  (let re = compile "(?(?!a)x|ab)" in
   expect_ov re "x" [| 0; 1 |];
   expect_ov re "ab" [| 0; 2 |]);
  (* DEFINE (OP_FALSE, single-branch skip) + named recursion — oracle:
     0: abab, 1: <unset>. *)
  expect_ov (compile "(?(DEFINE)(?<f>ab))(?&f)+") "abab" [| 0; 4; -1; -1 |];
  (* VERSION conditions compile to OP_TRUE — oracle: 0: yes. *)
  expect_ov (compile "(?(VERSION>=10)yes|no)") "yes" [| 0; 3 |];
  (* OP_SCOND: a repeated conditional group that might match an empty
     string descends a level (RM35) — oracle: 0: abc, 1: a / 0: ac,
     1: a. *)
  (let re = compile "(a)(?(1)b|)*c" in
   expect_ov re "abc" [| 0; 3; 0; 1 |];
   expect_ov re "ac" [| 0; 2; 0; 1 |]);
  (* Numbered recursion, nested calls — oracle: 0: aabb, 1: aabb; the
     anchored variant fails on an unbalanced subject. *)
  (let re = compile "^(a(?1)?b)$" in
   expect_ov re "aabb" [| 0; 4; 0; 4 |];
   expect_nomatch re "aab");
  (* Whole-pattern recursion: the OP_BRA ket action reinstates and
     carries on after the (?R) call — oracle: 0: abcabc. *)
  expect_ov (compile "abc(?R)?") "abcabc" [| 0; 6 |];
  (* Captures made inside a recursion are discarded on exit (Perl
     semantics, the recursed-group ket reinstate): group 2 is set only
     inside the (?&f) call, so \2 is unset outside and the reference
     fails — oracle: no match on both. *)
  (let re = compile "(?(DEFINE)(?<f>(a)))(?&f)\\2" in
   expect_nomatch re "a";
   expect_nomatch re "aa");
  (* The reinstate also restores the PREVIOUS captures: after (?1)
     returns, group 1 is unset again and is then captured by the
     top-level (a) — oracle: 0: aa, 1: a (the second 'a'). *)
  expect_ov (compile "(?1)(a)") "aa" [| 0; 2; 1; 2 |];
  (* Recursion in an alternation branch — oracle: 0: a, 1: a, 2: a. *)
  expect_ov (compile "((a)|(?1)b)") "ab" [| 0; 1; 0; 1; 0; 1 |];
  (* PCRE2_ERROR_RECURSELOOP (-52), a direct return: repeating the same
     group's recursion at the same subject position with the same last
     consulted character — oracle: error -52 for all three (the third
     recurses group 1 at position 0 twice via the second branch). *)
  expect_rc (compile "(?R)") "a" Errors.error_recurseloop;
  expect_rc (compile "((?1))") "a" Errors.error_recurseloop;
  expect_rc (compile "((a)|(?1)b)") "b" Errors.error_recurseloop;
  (* PCRE2_DISABLE_RECURSELOOP_CHECK: the -52 check is skipped and the
     runaway recursion is caught by the limits instead — oracle
     (pcre2test disable_recurseloop_check with depth_limit=100 /
     heap_limit=1): -53 / -63. *)
  (let code, top_bracket = compile_whole "(?R)" in
   (match
      match_internal ~moptions:Options.disable_recurseloop_check
        ~match_limit_depth:100 ~code ~top_bracket "a" 0
    with
   | rc, _ -> assert (Int.equal rc Errors.error_depthlimit));
   match
     match_internal ~moptions:Options.disable_recurseloop_check ~heap_limit:1
       ~code ~top_bracket "a" 0
   with
   | rc, _ -> assert (Int.equal rc Errors.error_heaplimit));
  (* OP_FAIL: the empty negative lookahead (?!) compiles to it — oracle:
     no match. *)
  expect_nomatch (compile "Z(?!)") "Z";
  (* Quantified recursion (compile-side replication + OP_BRA wrap +
     bracket-repeat fallthrough): fixed, lazy, ranged, possessive,
     query and star forms — oracle values from pcre2test. *)
  expect_ov (compile "(x)(?1){2}") "xxx" [| 0; 3; 0; 1 |];
  expect_ov (compile "(x)(?1){2}?y") "xxxy" [| 0; 4; 0; 1 |];
  expect_ov (compile "(x)(?1){2,4}y") "xxxxxy" [| 0; 6; 0; 1 |];
  expect_ov (compile "(x)(?1)++y") "xxxy" [| 0; 4; 0; 1 |];
  expect_ov (compile "(x)(?1)?y") "xy" [| 0; 2; 0; 1 |];
  expect_ov (compile "(x)(?1)*y") "xy" [| 0; 2; 0; 1 |]

(* Backtracking verbs (this chunk): the verb dispatch arms OP_MARK/
   OP_COMMIT(_ARG)/OP_PRUNE(_ARG)/OP_SKIP(_ARG)/OP_THEN(_ARG) with the
   RM12-RM19/RM36 resumes, the MATCH_THEN branch-scope checks (RM2/RM8/
   RM11), OP_CLOSE/OP_ACCEPT (incl. the in-recursion ACCEPT walk), and
   the driver's verb rc switch (MATCH_SKIP new-start, MATCH_SKIP_ARG
   ignore/re-run, PRUNE/THEN-as-NOMATCH, COMMIT bump suppression). EVERY
   expected value below is pinned against the C oracle (pcre2test on the
   real 10.44 library, via test/pcre2test/pcre2test_ml.exe
   --driver=oracle). *)
let () =
  let compile pat =
    match Compile.pcre2_compile pat ~options:0 with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run (re : Compile.re) subj =
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
    let rc = pcre2_match re ~subject:subj ~start_offset:0 ~options:0 m in
    (rc, m)
  in
  let expect_ov re subj expected =
    match run re subj with
    | rc, m ->
        assert (rc > 0);
        assert (Int.equal (Array.length m.ovector) (Array.length expected));
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch re subj =
    match run re subj with rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  (* The match-data mark decoded to its name: m.mark points past the
     length code unit of the verb-name item in the compiled code
     (pcre2_match.c:6341, Fecode + 2), so the length is mark[-1] — the
     protocol pcre2test's PCHARSV(mark, -1, ...) uses; "" = NULL (never
     set). *)
  let mark_name (re : Compile.re) (m : match_data) : string =
    if m.mark < 0 then ""
    else
      Bytes.sub_string re.Compile.code m.mark
        (Char.code (Bytes.get re.Compile.code (m.mark - 1)))
  in
  (* The frontier units (testinput1:833-835 region): verbs backtracked
     into via ( *FAIL) — oracle: no match for all three. *)
  expect_nomatch (compile "a+b?(*PRUNE)c+(*FAIL)") "aaabccc";
  expect_nomatch (compile "a+b?(*COMMIT)c+(*FAIL)") "aaabccc";
  expect_nomatch (compile "a+b?(*SKIP)c+(*FAIL)") "aaabcccaaabccc";
  (* COMMIT suppresses the bump-along (the driver's MATCH_COMMIT arm,
     7567-7571): /a+( *COMMIT)b/ fails outright on "aacaab" where /a+b/
     bumps along and matches at 3 — oracle: no match / 0: aab. COMMIT is
     never reached on starts that fail before it — oracle: 0: aab. *)
  expect_nomatch (compile "a+(*COMMIT)b") "aacaab";
  expect_ov (compile "a+b") "aacaab" [| 3; 6 |];
  expect_ov (compile "a+(*COMMIT)b") "xxaab" [| 2; 5 |];
  (* COMMIT inside a group: branch 2 is still tried when branch 1 fails
     BEFORE the COMMIT — oracle: 0: Cx, 1: C — but a backtrack through
     COMMIT kills everything — oracle: no match. *)
  (let re = compile "(A(*COMMIT)B|C)x" in
   expect_ov re "Cx" [| 0; 2; 0; 1 |];
   expect_nomatch re "ABC");
  (* MARK: both branch paths set the mark (OP_MARK + RM12), and a failed
     match passes back mb->nomatch_mark (driver 7741) — oracle: 0: a with
     MK: A / 0: b with MK: B / No match, mark = B. *)
  (let re = compile "(*MARK:A)a|(*MARK:B)b" in
   (match run re "a" with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (String.equal (mark_name re m) "A"));
   (match run re "b" with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (String.equal (mark_name re m) "B"));
   match run re "c" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) "B"));
  (* COMMIT_ARG sets the mark on the success path — oracle: 0: a with
     MK: X; on "b" the first-code-unit optimization means no attempt ever
     runs, so no mark — oracle: No match (no mark). *)
  (let re = compile "(*COMMIT:X)a" in
   (match run re "a" with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (String.equal (mark_name re m) "X"));
   match run re "b" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) ""));
  (* The classic PRUNE/THEN distinction: PRUNE abandons the whole start
     position (no second branch, no bump-along success), THEN only the
     current alternative — oracle: no match / 0: ac. *)
  expect_nomatch (compile "a(*PRUNE)b|ac") "ac";
  expect_ov (compile "a(*THEN)b|ac") "ac" [| 0; 2 |];
  (* PRUNE_ARG sets the nomatch mark — oracle: No match, mark = A on
     "ACB"; success mark on "ACAB" — oracle: 0: AB with MK: A. On "AC"
     the req-cu ('B') optimization means no attempt ever runs, so no
     mark at all — oracle: No match (no mark). *)
  (let re = compile "A(*PRUNE:A)B" in
   (match run re "ACB" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) "A"));
   (match run re "ACAB" with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (String.equal (mark_name re m) "A"));
   match run re "AC" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) ""));
  (* SKIP passes back the new start point (driver 7544-7549): the
     bump-along jumps to the SKIP position, so the a+c branch is never
     tried at a start where it could succeed — oracle: no match on both
     subjects. *)
  (let re = compile "aaaaa(*SKIP)b|a+c" in
   expect_nomatch re "aaaaac";
   expect_nomatch re "aaaaacaaaab");
  (* SKIP:name matched by a MARK (the RM12 interception turns
     MATCH_SKIP_ARG into MATCH_SKIP at the mark's subject position, which
     is not past this start, so it acts like NOMATCH — but the SKIP_ARG
     return means the second branch is never tried) — oracle: No match,
     mark = x. *)
  (let re = compile "(*MARK:x)a(*SKIP:x)b|a+c" in
   match run re "aaaac" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) "x"));
  (* SKIP:name with NO matching mark: MATCH_SKIP_ARG reaches the top and
     the driver re-runs the same start with mb->ignore_skip_arg set
     (7529-7539), so the SKIP is a no-op and the second branch matches —
     oracle: 0: aaaac. *)
  expect_ov (compile "a(*SKIP:x)b|a+c") "aaaac" [| 0; 5 |];
  (* THEN is bounded by its branch (the RM2 verb_ecode_ptr check): in a
     multi-branch group a THEN in the last branch just fails the group —
     oracle: no match on both the anchored and unanchored forms. *)
  expect_nomatch (compile "^(A(*THEN)B|C(*THEN)D)") "CB";
  expect_nomatch (compile "(?:A(*THEN)B|C(*THEN)D)") "CB";
  (* SKIP may not escape a recursion (the RM11 verb-range check turns it
     into NOMATCH for the entire recursion) — oracle: no match. *)
  expect_nomatch (compile "(?(DEFINE)(?<t>a|b(*SKIP)c))x(?&t)") "xb";
  (* ACCEPT in a recursion ends the recursion, not the whole match (the
     OP_ACCEPT walk over the GF_RECURSE frames), and its captures are
     discarded on exit like any recursion — oracle: 0: ax, group 1 unset
     (both shapes). *)
  expect_ov
    (compile "(?(DEFINE)(?<f>a(*ACCEPT)z))(?&f)x")
    "ax" [| 0; 2; -1; -1 |];
  expect_ov (compile "(?1)x(?:(a(*ACCEPT)zz)){0}") "ax" [| 0; 2; -1; -1 |];
  (* OP_CLOSE before a top-level ACCEPT writes the still-open captures
     from the chained group frames (P->eptr .. Feptr) — oracle: 0: AB,
     1: AB, 2: B, 3: <unset>. *)
  expect_ov
    (compile "(A(A|B(*ACCEPT)|C)D)(E)")
    "AB"
    [| 0; 2; 0; 2; 1; 2; -1; -1 |];
  (* Study's minlength feeds the driver's no-attempt break
     (pcre2_match.c:7382-7397): a subject shorter than the studied
     minimum NOMATCHes with no attempt at all, observable as MARK
     absence. /( *MARK:m)abcd/ has lower bound 4 (find_minlength), so on
     "ab" no attempt runs — oracle: No match (no mark) — while a real
     attempt sets the mark — oracle: 0: abcd with MK: m on "xabcd". *)
  (let re = compile "(*MARK:m)abcd" in
   (match run re "ab" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) ""));
   match run re "xabcd" with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (String.equal (mark_name re m) "m"));
  (* Backreference minlength (pcre2_study.c:548-596 expansion): lower
     bound of /( *MARK:r)(ab)\1/ is 4, so "aba" (3) gets no attempt —
     oracle: No match (no mark) / 0: abab with MK: r. *)
  (let re = compile "(*MARK:r)(ab)\\1" in
   (match run re "aba" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) ""));
   match run re "abab" with
   | rc, m ->
       assert (Int.equal rc 2 (* 0: abab, 1: ab *));
       assert (String.equal (mark_name re m) "r"));
  (* OP_MARK is in set_start_bits' SSB_FAIL list (pcre2_study.c:1023):
     /( *MARK:z)[ab]c/ gets NO start bitmap, so attempts DO run on "xc"
     and the nomatch mark is passed back — oracle: No match, mark = z.
     (Without the verb the [ab] bitmap would suppress the attempts; see
     the compile.ml study asserts.) *)
  let re = compile "(*MARK:z)[ab]c" in
  match run re "xc" with
  | rc, m ->
      assert (Int.equal rc Errors.error_nomatch);
      assert (String.equal (mark_name re m) "z")

(* UTF-8 match arms (this chunk): whole compiled UTF patterns through the
   [pcre2_match] driver — literal/CHARI/NOT decodes, wide char repeats
   (with the occu other-case buffer), class/NCLASS wide-char rules, the
   XCLASS runtime, type escapes over decoded characters, lookbehind
   character stepping, caseless backreferences via UCD, the driver's
   subject validity check, and the bump-along ACROSSCHAR. EVERY expected
   value below is pinned against the C oracle (pcre2test on the real
   10.44 library). *)
let () =
  let compile ?(options = 0) pat =
    match Compile.pcre2_compile pat ~options with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run ?(options = 0) (re : Compile.re) subj start =
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
    let rc = pcre2_match re ~subject:subj ~start_offset:start ~options m in
    (rc, m)
  in
  let expect_ov ?options re subj expected =
    match run ?options re subj 0 with
    | rc, m ->
        assert (rc > 0);
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch ?options re subj =
    match run ?options re subj 0 with
    | rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let u = Options.utf in
  (* OP_CHAR utf unit-compare + first-cu bump-along — oracle /é/utf on
     "café": 0: \x{e9} (bytes 3..5). *)
  expect_ov (compile ~options:u "\xc3\xa9") "caf\xc3\xa9" [| 3; 5 |];
  (* OP_CHARI utf, fc >= 128: other case via UCD — oracle /(?i)É/utf on
     "é": 0: \x{e9}. *)
  expect_ov (compile ~options:u "(?i)\xc3\x89") "\xc3\xa9" [| 0; 2 |];
  (* OP_ANY utf steps over the whole character — oracle /./utf on "é":
     0: \x{e9}. *)
  expect_ov (compile ~options:u ".") "\xc3\xa9" [| 0; 2 |];
  (* OP_NCLASS: a wide character matches a negated bitmap class — oracle
     /[^x]/utf on "é": 0: \x{e9} — and the class utf repeat — oracle
     /[^x]+/utf on "éàz": 0: \x{e9}\x{e0}z. *)
  expect_ov (compile ~options:u "[^x]") "\xc3\xa9" [| 0; 2 |];
  expect_ov (compile ~options:u "[^x]+") "\xc3\xa9\xc3\xa0z" [| 0; 5 |];
  (* OP_XCLASS runtime, hit and miss — oracle /[\x{100}-\x{200}]/utf:
     \x{150} matches, A and \x{201} do not. *)
  (let re = compile ~options:u "[\\x{100}-\\x{200}]" in
   expect_ov re "\xc5\x90" [| 0; 2 |];
   expect_nomatch re "A";
   expect_nomatch re "\xc8\x81");
  (* \d over a wide digit: ASCII semantics without UCP — oracle /\d/utf
     on U+0660 ARABIC-INDIC DIGIT ZERO: no match. *)
  expect_nomatch (compile ~options:u "\\d") "\xd9\xa0";
  (* \H over a decoded NBSP (multibyte HSPACE case) — oracle /\H\h/utf on
     "\x{a0}\x{a0}": no match. *)
  expect_nomatch (compile ~options:u "\\H\\h") "\xc2\xa0\xc2\xa0";
  (* \R matches the wide newline U+2028 under the default BSR — oracle
     /\R/utf on "\x{2028}z": 0: \x{2028}. *)
  expect_ov (compile ~options:u "\\R") "\xe2\x80\xa8z" [| 0; 3 |];
  (* Wide-char repeats: caseful — oracle /é+x/utf on "éééx": 0: whole —
     and caseless with the occu other-case encoding — oracle /(?i)é+x/utf
     on "éÉÉx": 0: whole. *)
  expect_ov
    (compile ~options:u "\xc3\xa9+x")
    "\xc3\xa9\xc3\xa9\xc3\xa9x" [| 0; 7 |];
  expect_ov
    (compile ~options:u "(?i)\xc3\xa9+x")
    "\xc3\xa9\xc3\x89\xc3\x89x" [| 0; 7 |];
  (* Fixed lookbehind steps CHARACTERS (OP_REVERSE utf) — oracle
     /(?<=é)x/utf on "éx": 0: x — and through a multi-byte '.' — oracle
     /(?<=a.b)x/utf on "aébx": 0: x. *)
  expect_ov (compile ~options:u "(?<=\xc3\xa9)x") "\xc3\xa9x" [| 2; 3 |];
  expect_ov (compile ~options:u "(?<=a.b)x") "a\xc3\xa9bx" [| 4; 5 |];
  (* Variable lookbehind (OP_VREVERSE utf + the RM37 FORWARDCHARTEST) —
     oracle /(?<=a\x{100}{2,4})x/utf on "a\x{100}\x{100}\x{100}x":
     0: x. *)
  expect_ov
    (compile ~options:u "(?<=a\\x{100}{2,4})x")
    "a\xc4\x80\xc4\x80\xc4\x80x" [| 7; 8 |];
  (* Caseless backreference via the UCD fold — oracle /(?i)(é)\1/utf on
     "éÉ": 0: \x{e9}\x{c9}, 1: \x{e9} — and a repeated wide backref —
     oracle /X(\x{e1})\1+Y/utf: XáááY matches, "Xááá áY" does not. *)
  expect_ov
    (compile ~options:u "(?i)(\xc3\xa9)\\1")
    "\xc3\xa9\xc3\x89" [| 0; 4; 0; 2 |];
  (let re = compile ~options:u "X(\\x{e1})\\1+Y" in
   expect_ov re "X\xc3\xa1\xc3\xa1\xc3\xa1Y" [| 0; 8; 1; 3 |];
   expect_nomatch re "X\xc3\xa1\xc3\xa1\xc3\xa1 \xc3\xa1Y");
  (* Subject UTF validity check (pcre2_match.c:6807-6929) — oracle:
     /x/utf on "\xc3" -> error -3 (1 byte missing at end) at offset 0;
     with offset=1 into "é" (a continuation byte, start_offset > 0) ->
     -36 BADUTFOFFSET; NO_UTF_CHECK skips the check entirely. *)
  (let re = compile ~options:u "x" in
   (match run re "\xc3" 0 with
   | rc, m ->
       assert (Int.equal rc Errors.error_utf8_err1);
       assert (Int.equal m.startchar 0));
   (match run re "\xc3\xa9x" 1 with
   | rc, _ -> assert (Int.equal rc Errors.error_badutfoffset));
   (match run ~options:Options.no_utf_check re "\xc3\xa9x" 1 with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (Int.equal m.ovector.(0) 2));
   (* An isolated continuation byte at offset 0 is UTF8_ERR20, not
      BADUTFOFFSET (pcre2_match.c:6843-6845). *)
   match run re "\x80x" 0 with
   | rc, _ -> assert (Int.equal rc Errors.error_utf8_err20));
  (* The bump-along advances by whole characters (ACROSSCHAR,
     pcre2_match.c:7561-7563) — oracle /x/utf,no_start_optimize on "ééx":
     0: x at (4,5). *)
  expect_ov
    (compile ~options:(u lor Options.no_start_optimize) "x")
    "\xc3\xa9\xc3\xa9x" [| 4; 5 |]

(* PCRE2_MATCH_INVALID_UTF (this chunk): the driver's invalid-UTF
   fragment machinery — skipped_bad_start (pcre2_match.c:6828-6836), the
   fragment validation loop (6889-6928), FRAGMENT_RESTART (7139-7148) and
   the ENDLOOP fragment carry-on (7646-7701) — through whole compiled
   patterns. EVERY expected value below is pinned against the C oracle
   (pcre2test on the real 10.44 library, via
   test/pcre2test/pcre2test_ml.exe --driver=oracle). *)
let () =
  let compile ?(options = 0) pat =
    match Compile.pcre2_compile pat ~options with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run ?(options = 0) (re : Compile.re) subj start =
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
    let rc = pcre2_match re ~subject:subj ~start_offset:start ~options m in
    (rc, m)
  in
  let expect_ov_at ?options re subj start expected =
    match run ?options re subj start with
    | rc, m ->
        assert (rc > 0);
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch_at ?options re subj start =
    match run ?options re subj start with
    | rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let miu = Options.utf lor Options.match_invalid_utf in
  (* Match in a later fragment (the ENDLOOP carry-on advances past the
     bad code unit and restarts) — oracle /abc/utf,match_invalid_utf on
     "ab\x80abc": 0: abc at (3,6). MATCH_INVALID_UTF implies UTF at
     compile (pcre2_compile.c:10201-10203), so the bare-option pattern
     behaves identically. NOMATCH when the subject is only bad code
     units (skipped_bad_start consumes everything) — oracle: no match. *)
  (let re = compile ~options:miu "abc" in
   expect_ov_at re "ab\x80abc" 0 [| 3; 6 |];
   expect_nomatch_at re "\x80\x80\x80" 0);
  (let re = compile ~options:Options.match_invalid_utf "abc" in
   expect_ov_at re "ab\x80abc" 0 [| 3; 6 |]);
  (* A start offset inside a bad fragment is NOT BADUTFOFFSET under
     MATCH_INVALID_UTF: the skipped_bad_start loop (6828-6836) just
     advances past bad starting code units — oracle: 0: abc for both
     offset=2 into "ab\x80abc" (on the bad byte) and offset=1 into
     "\x80\x80abc" (inside a leading bad run). *)
  (let re = compile ~options:miu "abc" in
   expect_ov_at re "ab\x80abc" 2 [| 3; 6 |];
   expect_ov_at re "\x80\x80abc" 1 [| 2; 5 |]);
  (* ^ does not match at later fragment starts: OP_CIRC demands the true
     subject start (and NOTBOL is set for every fragment after the
     first, 7683/7695) — oracle /^X/utf,match_invalid_utf: no match on
     "\x80X" (skipped-bad start) and on "A\x80X" (carry-on fragment);
     the FIRST fragment does start the subject — oracle
     /^A/utf,match_invalid_utf on "A\x80X": 0: A. *)
  (let re = compile ~options:miu "^X" in
   expect_nomatch_at re "\x80X" 0;
   expect_nomatch_at re "A\x80X" 0);
  expect_ov_at (compile ~options:miu "^A") "A\x80X" 0 [| 0; 1 |];
  (* Partial interplay: a hard partial in the TERMINAL fragment is
     returned (ovector = the fragment tail) — oracle
     /.a/utf,match_invalid_utf on "b\xf0\x91\x88b" ph: Partial match: b
     at (4,5) — but a partial in a NON-terminal fragment is discarded by
     the carry-on (match_partial = NULL, 7665-7670) — oracle
     /.a$/utf,match_invalid_utf on "b\xf0\x91\x98" ph: no match. *)
  (match
     run ~options:Options.partial_hard
       (compile ~options:miu ".a")
       "b\xf0\x91\x88b" 0
   with
  | rc, m ->
      assert (Int.equal rc Errors.error_partial);
      assert (Int.equal m.ovector.(0) 4);
      assert (Int.equal m.ovector.(1) 5));
  (match
     run ~options:Options.partial_hard
       (compile ~options:miu ".a$")
       "b\xf0\x91\x98" 0
   with
  | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
  (* Per-fragment NOTEOL: $ cannot match at a non-terminal fragment's
     end (fragment_options = NOTEOL, 6924 / NOTBOL|NOTEOL, 7695) but can
     at the true subject end — oracle /ab$/utf,match_invalid_utf:
     "ab\x80cdeab" -> 0: ab at (6,8); "ab\x80cde" -> no match. *)
  (let re = compile ~options:miu "ab$" in
   expect_ov_at re "ab\x80cdeab" 0 [| 6; 8 |];
   expect_nomatch_at re "ab\x80cde" 0);
  (* Empty-fragment advance in the carry-on loop (0xff is a character
     STARTING code unit for NOT_FIRSTCU but its fragment is empty, so
     the for(;;) advances again, 7687-7698) — oracle
     /X/utf,match_invalid_utf on "AB\xfe\xffXY": 0: X at (4,5). *)
  expect_ov_at (compile ~options:miu "X") "AB\xfe\xffXY" 0 [| 4; 5 |]

(* Unicode property matching (this chunk): OP_PROP/OP_NOTPROP + the
   property repeat strategies (min/minimize/maximize + RM208-RM217/
   RM222-RM225), XCL_PROP/XCL_NOTPROP items, and the UCP word
   boundaries, through whole compiled patterns. EVERY expected value
   below is pinned against the C oracle (pcre2test on the real 10.44
   library, via test/pcre2test/pcre2test_ml.exe --driver=oracle). *)
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
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch ?options re subj =
    match run ?options re subj with
    | rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let u = Options.utf in
  let uu = Options.utf lor Options.ucp in
  (* PT_GC greedy repeat over mixed scripts — oracle /\p{L}+/utf on
     "abcαβ123": 0: abc\x{3b1}\x{3b2}. *)
  expect_ov (compile ~options:u "\\p{L}+") "abc\xce\xb1\xce\xb2123" [| 0; 7 |];
  (* PT_SC single, hit and miss — oracle /\p{Greek}/utf: "XαY" -> 0:
     \x{3b1}; "XY" -> no match. *)
  (let re = compile ~options:u "\\p{Greek}" in
   expect_ov re "X\xce\xb1Y" [| 1; 3 |];
   expect_nomatch re "XY");
  (* PT_PC (particular category Sc = currency) — oracle /\p{Sc}/utf on
     "x€y": 0: \x{20ac}. *)
  expect_ov (compile ~options:u "\\p{Sc}") "x\xe2\x82\xacy" [| 1; 4 |];
  (* PT_SCX: U+0964 DEVANAGARI DANDA is script Common but Devanagari is
     in its Script Extensions — oracle /\p{scx:Deva}/utf on "\x{964}":
     0: \x{964}. *)
  expect_ov (compile ~options:u "\\p{scx:Deva}") "\xe0\xa5\xa4" [| 0; 3 |];
  (* PT_ALNUM (\p{Xan}) takes ARABIC-INDIC DIGIT ZERO but not '!' —
     oracle /\p{Xan}+/utf on "ab\x{660}12!": 0: ab\x{660}12. *)
  expect_ov (compile ~options:u "\\p{Xan}+") "ab\xd9\xa012!" [| 0; 6 |];
  (* PT_WORD (\p{Xwd}) takes '_' (Pc) and COMBINING GRAVE (Mn) — oracle
     /\p{Xwd}+/utf on "a_\x{300}b c": 0: a_\x{300}b. *)
  expect_ov (compile ~options:u "\\p{Xwd}+") "a_\xcc\x80b c" [| 0; 5 |];
  (* PT_CLIST: MICRO SIGN caselessly matches GREEK SMALL MU through its
     caseless set (the compiler turns multi-case (?i) singles into
     OP_PROP PT_CLIST) — oracle /(?i)µ/utf on "\x{3bc}": 0: \x{3bc} —
     and KELVIN SIGN is in k's set — oracle /(?i)k+/utf on "\x{212a}k":
     0: \x{212a}k. *)
  expect_ov (compile ~options:u "(?i)\xc2\xb5") "\xce\xbc" [| 0; 2 |];
  expect_ov (compile ~options:u "(?i)k+") "\xe2\x84\xaak" [| 0; 4 |];
  (* ...and LATIN SMALL LETTER LONG S in s's set (a PT_CLIST repeat) —
     oracle /(?i)s+/utf on "\x{17f}s": 0: \x{17f}s. *)
  expect_ov (compile ~options:u "(?i)s+") "\xc5\xbfs" [| 0; 3 |];
  (* OP_UCP_WORD_BOUNDARY: é is a word character under UCP, so no
     boundary before 'w' — oracle /\bword\b/ucp,utf: "\x{e9}word\x{e9}"
     -> no match; ".word." -> 0: word — and OP_NOT_UCP_WORD_BOUNDARY —
     oracle /\Bword/ucp,utf on "\x{e9}word": 0: word. *)
  (let re = compile ~options:uu "\\bword\\b" in
   expect_nomatch re "\xc3\xa9word\xc3\xa9";
   expect_ov re ".word." [| 1; 5 |]);
  expect_ov (compile ~options:uu "\\Bword") "\xc3\xa9word" [| 2; 6 |];
  (* PT_BIDICL — oracle /\p{bidi_class:R}/utf on HEBREW ALEF: 0:
     \x{5d0}. *)
  expect_ov (compile ~options:u "\\p{bidi_class:R}") "\xd7\x90" [| 0; 2 |];
  (* PT_BOOL — oracle /\p{Cased}+/utf on "aAα0": 0: aA\x{3b1}. *)
  expect_ov (compile ~options:u "\\p{Cased}+") "aA\xce\xb10" [| 0; 4 |];
  (* PT_PC exact + upto (the {2,} decomposition exercises the property
     min loop then the maximize scan) — oracle /\p{Lu}{2,}/utf on
     "xABCy": 0: ABC. *)
  expect_ov (compile ~options:u "\\p{Lu}{2,}") "xABCy" [| 1; 4 |];
  (* OP_NOTPROP repeat — oracle /\P{L}+/utf on "ab12!;cd": 0: 12!;. *)
  expect_ov (compile ~options:u "\\P{L}+") "ab12!;cd" [| 2; 6 |];
  (* Property minimize resumes (the RM-label loops) — oracle
     /\p{Han}*?X/utf on "\x{2e80}\x{3105}X": 0: X (bump-along past the
     non-Han) and /\p{Greek}+?X/utf on "ααX": 0: whole. *)
  expect_ov
    (compile ~options:u "\\p{Han}*?X")
    "\xe2\xba\x80\xe3\x84\x85X" [| 6; 7 |];
  expect_ov (compile ~options:u "\\p{Greek}+?X") "\xce\xb1\xce\xb1X" [| 0; 5 |];
  (* Property maximize backtrack (RM222): \p{L}+ eats β, then backs up
     one character — oracle /\p{L}+\x{3b2}/utf on "ααβ": 0: whole. *)
  expect_ov
    (compile ~options:u "\\p{L}+\xce\xb2")
    "\xce\xb1\xce\xb1\xce\xb2" [| 0; 6 |];
  (* PT_PC minimize with a bounded range — oracle /\p{Nd}{2,3}?\./utf
     on "\x{660}\x{661}\x{662}.": 0: whole. *)
  expect_ov
    (compile ~options:u "\\p{Nd}{2,3}?\\.")
    "\xd9\xa0\xd9\xa1\xd9\xa2." [| 0; 7 |];
  (* Property min loop count — oracle /\p{L}{3}/utf: "abαc" -> 0:
     ab\x{3b1}; "ab1c" -> no match. *)
  (let re = compile ~options:u "\\p{L}{3}" in
   expect_ov re "ab\xce\xb1c" [| 0; 4 |];
   expect_nomatch re "ab1c");
  (* XCL_PROP inside a class (bitmap + property item) — oracle
     /[\p{L}0]+/utf on "9aα0b8": 0: a\x{3b1}0b. *)
  expect_ov (compile ~options:u "[\\p{L}0]+") "9a\xce\xb10b8" [| 1; 6 |];
  (* Negated class with a property item — oracle /[^\p{L}]+/utf on
     "ab€0\x{300}cd": 0: \x{20ac}0\x{300}. *)
  expect_ov
    (compile ~options:u "[^\\p{L}]+")
    "ab\xe2\x82\xac0\xcc\x80cd" [| 2; 8 |];
  (* The XCLASS-only PT_PXPUNCT (UCP [[:punct:]]) takes FULLWIDTH
     EXCLAMATION MARK — oracle /[[:punct:]]+/ucp,utf on "a\x{ff01}!b":
     0: \x{ff01}!. *)
  expect_ov (compile ~options:uu "[[:punct:]]+") "a\xef\xbc\x81!b" [| 1; 5 |]
