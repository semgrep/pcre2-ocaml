(* Fast-engine fused runner (M11 chunk C2). Engine-native code: the design
   (register-discipline tail loop, save records, shadow limit accounting) has
   no line-for-line PCRE2 counterpart, so per port-conventions.md §9 it cites
   fast-design.md §3/§4. But its OBSERVABLE behavior must be IDENTICAL to the
   interpreter (Pcre2_engine.Interpreter) on accepted patterns; every arm here
   therefore also cites the interpreter arm / pcre2_match.c line it mirrors, so
   the two engines cannot drift (the interpreter is the differential oracle).

   port-conventions.md §5-§8 bind in full: this IS the hot loop.
   - ONE module-level tail-recursive [run]/[backtrack] pair; hot state is
     tail-call int params (pc, eptr, sp, rdepth, mcc); the match block [mb]
     (an arena-slot-equivalent mutable record, reused across execs) carries
     the rest. No exception crosses the loop; limit/heap errors return as
     negative rc values. Zero allocation in the loop; no closures/tuples/
     options per iteration; no polymorphic compare.
   - [String.unsafe_get] appears only under a bounds-proof comment; the C1
     verifier (Ir_verify) is the proof for every IR read (a compiled [Ir.t]
     cannot reach an unhandled instruction or an out-of-range operand).

   SUBSET (chunk C1 IR tags): END, CHAR_RUN, CHARI, BRA, KET, ALT, JMP, and
   the simple anchors SOD/SOM/EOD/EODN/CIRC/DOLL. top_bracket = 0 (no
   captures), non-UTF, non-UCP (compile-level gates in Ir_compile). *)

module Ir = Ir
module Errors = Pcre2_engine.Errors
module Options = Pcre2_engine.Options
module Newline = Pcre2_engine.Newline
module Chartables = Pcre2_engine.Chartables
module Frames = Pcre2_engine.Frames
module Limits = Pcre2_engine.Limits
module Compile = Pcre2_engine.Compile

(* pcre2_match.c:87-88 — the two internal match() return codes used inside
   the loop (interpreter.ml:88-89). MATCH_MATCH = 1, MATCH_NOMATCH = 0. *)
let match_match = 1
let match_nomatch = 0

(* Internal signal returned by [char_run_cmp] for "plain NOMATCH, backtrack",
   distinct from every real PCRE2 error code (which are in [-63, -1]) and
   from any non-negative eptr. *)
let sig_backtrack = min_int

(* ---------- The match block (per-exec, reused across execs) ----------

   The interpreter keeps this state in [match_block] + [driver_state]
   (interpreter.ml:320-433, 8658-...); here it is one flat mutable record so
   the loop allocates nothing. All fields are (re)assigned by [exec] before
   each match; [Runner] caches ONE instance (see [scratch_mb]) gated on the
   save stack's busy flag, mirroring the interpreter's scratch trio
   (interpreter.ml fresh_trio / frames.ml scratch slot). *)
type mb = {
  (* program *)
  mutable code : int array; (* Ir.t code — the flat instruction stream *)
  mutable lit : string; (* Ir.t lit — CHAR_RUN literal pool *)
  (* subject / offsets *)
  mutable subject : string;
  mutable end_subject : int; (* usable end; = length (no UTF fragments) *)
  mutable start_subject : int; (* always 0 in this port *)
  mutable start_offset : int;
  mutable attempt_start : int; (* start_match of the current attempt (\K absent) *)
  mutable start_used_ptr : int; (* = attempt_start; scheck_partial floor *)
  (* option-derived flags (moptions = options; constant per exec, no UTF) *)
  mutable partial : int; (* 0 none / 1 soft / 2 hard *)
  mutable notempty : bool;
  mutable notempty_atstart : bool;
  mutable endanchored : bool; (* (moptions|poptions) & ENDANCHORED *)
  mutable notbol : bool;
  mutable noteol : bool;
  mutable dollar_endonly : bool;
  mutable anchored : bool;
  mutable hascrorlf : bool; (* re.flags & HASCRORLF (CRLF bump-along advance) *)
  mutable allowemptypartial : bool;
  (* start-of-match scan config (pcre2_match.c:7091-7131 / 7155-7481) — needed
     for LIMIT tick parity: skipped attempts do NOT tick, so the fast bump
     must attempt exactly the interpreter's position set (§4). *)
  mutable no_start_optimize : bool;
  mutable has_first_cu : bool;
  mutable first_cu : int;
  mutable first_cu2 : int;
  mutable use_start_bits : bool;
  mutable start_bitmap : Bytes.t;
  mutable startline : bool;
  mutable has_req_cu : bool;
  mutable req_cu : int;
  mutable req_cu2 : int;
  mutable minlength : int;
  (* newline convention *)
  mutable nltype : int;
  mutable nllen : int;
  mutable nl0 : int;
  mutable nl1 : int;
  nl_scratch : int ref; (* is_newline out-param; preallocated once *)
  (* limits + shadow heap accounting (§4) *)
  mutable match_limit : int;
  mutable match_limit_depth : int;
  mutable heap_limit : int; (* KiB *)
  mutable frame_size_bytes : int;
  mutable heapframes_size : int; (* simulated C bytes; only grows within an exec *)
  (* mutable match state *)
  mutable hitend : bool;
  mutable match_partial : int; (* first hitend attempt's start; -1 = unset *)
  (* results *)
  mutable match_start : int;
  mutable match_end : int;
  (* the backtracking save stack *)
  ss : Save_stack.t;
}

let make_mb (ss : Save_stack.t) : mb =
  {
    code = [||];
    lit = "";
    subject = "";
    end_subject = 0;
    start_subject = 0;
    start_offset = 0;
    attempt_start = 0;
    start_used_ptr = 0;
    partial = 0;
    notempty = false;
    notempty_atstart = false;
    endanchored = false;
    notbol = false;
    noteol = false;
    dollar_endonly = false;
    anchored = false;
    hascrorlf = false;
    allowemptypartial = false;
    no_start_optimize = false;
    has_first_cu = false;
    first_cu = 0;
    first_cu2 = 0;
    use_start_bits = false;
    start_bitmap = Bytes.empty;
    startline = false;
    has_req_cu = false;
    req_cu = 0;
    req_cu2 = 0;
    minlength = 0;
    nltype = Newline.nltype_fixed;
    nllen = 1;
    nl0 = Newline.char_lf;
    nl1 = 0;
    nl_scratch = ref 0;
    match_limit = Limits.match_limit;
    match_limit_depth = Limits.match_limit_depth;
    heap_limit = Limits.heap_limit;
    frame_size_bytes = 0;
    heapframes_size = 0;
    hitend = false;
    match_partial = -1;
    match_start = 0;
    match_end = 0;
    ss;
  }

(* ---------- Cold helpers (out-of-line, §3 icache) ---------- *)

(* pcre2_match.c:537-543 SCHECK_PARTIAL (interpreter.ml:643-650). Called at or
   past end_subject: set hitend if something was matched (or an empty hard
   partial is allowed); hard partial returns PCRE2_ERROR_PARTIAL, otherwise 0
   (the caller then backtracks with NOMATCH). *)
let scheck_partial (mb : mb) (feptr : int) : int =
  if
    mb.partial <> 0
    && (feptr > mb.start_used_ptr || mb.allowemptypartial)
  then (
    mb.hitend <- true;
    if mb.partial > 1 then Errors.error_partial else 0)
  else 0

(* pcre2_match.c:668-712 via Frames.grow (interpreter.ml uses Frames.grow):
   the shadow-frame vector is full; double it under the heap limit. Returns 0
   on success (updating [heapframes_size]) or a negative PCRE2 error code as a
   value. [n] is the index of the frame being created (usedsize = n *
   frame_size_bytes), exactly Frames.grow's arithmetic in simulated C bytes
   (frames.ml:412-464). *)
let grow_virtual (mb : mb) (n : int) : int =
  let usedsize = n * mb.frame_size_bytes in
  let newsize =
    if mb.heapframes_size >= max_int / 2 then
      if mb.heapframes_size = max_int - 1 then Errors.error_nomemory
      else max_int - 1
    else mb.heapframes_size * 2
  in
  if newsize < 0 then newsize
  else
    let newsize =
      if newsize / 1024 >= mb.heap_limit then (
        let old_size = mb.heapframes_size / 1024 in
        if mb.heap_limit <= old_size then Errors.error_heaplimit
        else
          let max_delta = 1024 * (mb.heap_limit - old_size) in
          let over_bytes = mb.heapframes_size mod 1024 in
          let max_delta =
            if over_bytes <> 0 then max_delta - (1024 - over_bytes) else max_delta
          in
          mb.heapframes_size + max_delta)
      else newsize
    in
    if newsize < 0 then newsize
    else if newsize - usedsize < mb.frame_size_bytes then Errors.error_heaplimit
    else (
      mb.heapframes_size <- newsize;
      0)

(* pcre2_internal.h:496-507 IS_NEWLINE (interpreter.ml:667-688), non-UTF. The
   NLTYPE_ANY/ANYCRLF path updates mb.nllen via nl_scratch exactly when the C
   writes it (Newline.is_newline only touches the ref on TRUE). *)
let is_newline_at (mb : mb) (p : int) : bool =
  if mb.nltype <> Newline.nltype_fixed then
    p < mb.end_subject
    &&
    let hit =
      Newline.is_newline mb.subject mb.nltype p mb.end_subject mb.nl_scratch false
    in
    if hit then mb.nllen <- !(mb.nl_scratch);
    hit
  else
    p <= mb.end_subject - mb.nllen
    (* safe: 0 <= p and p <= end_subject - nllen < end_subject <=
       String.length subject (nllen >= 1) *)
    && Int.equal (Char.code (String.unsafe_get mb.subject p)) mb.nl0
    && (Int.equal mb.nllen 1
       || Int.equal (Char.code (String.unsafe_get mb.subject (p + 1))) mb.nl1)

(* pcre2_match.c:992-1025 non-UTF OP_CHAR run (interpreter.ml:1509-1539),
   fused: compare [lit_pos, lit_end) against the subject from [eptr]. Each
   code unit mirrors one OP_CHAR — SCHECK_PARTIAL at/past end_subject, then a
   caseful compare. Returns the new eptr (>= 0) on full match, [sig_backtrack]
   on a plain NOMATCH, or a negative error code (PARTIAL) to propagate.
   Module-level + fully applied so the loop builds no closure (§8). *)
let rec char_run_cmp (mb : mb) (lit_pos : int) (lit_end : int) (eptr : int) : int
    =
  if lit_pos >= lit_end then eptr
  else if eptr >= mb.end_subject then
    (* pcre2_match.c:1015-1017 — end_subject - eptr < 1: SCHECK_PARTIAL then
       NOMATCH. *)
    let r = scheck_partial mb eptr in
    if r < 0 then r else sig_backtrack
  else if
    (* pcre2_match.c:1022 — Fecode[1] != *Feptr++ (caseful). safe: lit_pos <
       lit_end <= String.length mb.lit (Ir_verify CHAR_RUN bound); eptr <
       mb.end_subject <= String.length mb.subject (checked above). *)
    not
      (Int.equal
         (Char.code (String.unsafe_get mb.lit lit_pos))
         (Char.code (String.unsafe_get mb.subject eptr)))
  then sig_backtrack
  else (char_run_cmp [@tailcall]) mb (lit_pos + 1) lit_end (eptr + 1)

(* ---------- Shadow limit tick (§4) ---------- *)

(* One tick for a CHILD frame (a group-branch RMATCH: ALT, the top-level
   group's single/last branch, and every non-last branch). Order transcribes
   the C's rmatch (interpreter.ml:1188-1267): (1) the frame-vector push/grow
   heap check (pcre2_match.c:667-668, for frame index [new_rdepth]), THEN
   (2) the match_call_count compare-old-then-bump vs match_limit, THEN (3) the
   new frame's rdepth vs match_limit_depth. Returns the new match_call_count
   on success, or a negative PCRE2 error code as a value. *)
let tick_child (mb : mb) (mcc : int) (new_rdepth : int) : int =
  (* (1) heap: the C creates frame index n = new_rdepth. *)
  let hr =
    if (new_rdepth + 1) * mb.frame_size_bytes >= mb.heapframes_size then
      grow_virtual mb new_rdepth
    else 0
  in
  if hr < 0 then hr (* HEAPLIMIT / NOMEMORY *)
  else if mcc >= mb.match_limit then Errors.error_matchlimit
  else if new_rdepth >= mb.match_limit_depth then Errors.error_depthlimit
  else mcc + 1

(* ---------- The fused loop (§3) ---------- *)

let rec run (mb : mb) (pc : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  let code = mb.code in
  (* safe (every code.() / unsafe_get below): Ir_verify proved [pc] reaches
     only instruction heads whose operands fit in [code], and every jump
     target / advance lands on a head — a compiled Ir.t cannot dispatch on a
     non-head or overrun (fast-design.md §2, ir_verify.ml). *)
  (* The arms dispatch on literal ints (a jump table, like interpreter.ml's
     opcode switch); every literal — here and at the two ALT-detection sites
     (the BRA arm and [backtrack]) — is pinned against its Ir.t_* constant by
     the module-initialization asserts at the bottom of this file. *)
  match code.(pc) with
  | 1 ->
      (* CHAR_RUN (fast-design.md §2; OP_CHAR arm) — fused caseful run. *)
      let lit_off = code.(pc + 1) in
      let len = code.(pc + 2) in
      let e = char_run_cmp mb lit_off (lit_off + len) eptr in
      if e >= 0 then (run [@tailcall]) mb (pc + 3) e sp rdepth mcc
      else if e = sig_backtrack then (backtrack [@tailcall]) mb sp mcc
      else e (* PARTIAL — propagate *)
  | 2 ->
      (* CHARI (fast-design.md §2; non-UTF OP_CHARI arm,
         pcre2_match.c:1097-1103 / interpreter.ml:1619-1634) — one caseless
         code unit via the lowercase table. *)
      if eptr >= mb.end_subject then (
        (* pcre2_match.c:1035-1039 SCHECK_PARTIAL then NOMATCH. *)
        let r = scheck_partial mb eptr in
        if r < 0 then r else (backtrack [@tailcall]) mb sp mcc)
      else
        let pch = code.(pc + 1) in
        (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if not (Int.equal (Chartables.lcc pch) (Chartables.lcc cc)) then
          (backtrack [@tailcall]) mb sp mcc
        else (run [@tailcall]) mb (pc + 2) (eptr + 1) sp rdepth mcc
  | 3 ->
      (* BRA (fast-design.md §2/§4; OP_BRA arm) — group entry. A group whose
         first branch has NO choice point (next head is not ALT) is a
         single-branch group whose sole branch is its LAST: the C ticks + adds
         a frame for it ONLY at the top level (grouploop, rdepth 0,
         interpreter.ml:2416-2432 / pcre2_match.c:5349-5372); nested it runs
         in the same frame (bra_loop last branch). A multi-branch group's
         first branch is handled by the following ALT (which always ticks). *)
      if Int.equal code.(pc + 1) 5 (* next head is ALT *) then
        (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
      else if Int.equal rdepth 0 then
        (* top-level single/last branch: grouploop tick (frame index 1). *)
        let mcc' = tick_child mb mcc 1 in
        if mcc' < 0 then mcc'
        else (run [@tailcall]) mb (pc + 1) eptr sp 1 mcc'
      else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
  | 4 ->
      (* KET (fast-design.md §2) — structural group exit; continue past. *)
      (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
  | 5 ->
      (* ALT (fast-design.md §2/§3/§4) — a choice point for a NON-LAST branch
         (bra_loop/grouploop record a backtracking point, RM1/RM2). Tick +
         enter a child frame (rdepth+1), push the save record [handler; eptr;
         rdepth], fall through into the branch body. *)
      let handler = code.(pc + 1) in
      let mcc' = tick_child mb mcc (rdepth + 1) in
      if mcc' < 0 then mcc'
      else (
        let ss = mb.ss in
        let need = sp + Save_stack.record_width in
        if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
        let d = ss.Save_stack.data in
        (* safe: [grow] above ensured Array.length d >= sp + 3. *)
        Array.unsafe_set d sp handler;
        Array.unsafe_set d (sp + 1) eptr;
        Array.unsafe_set d (sp + 2) rdepth;
        (run [@tailcall]) mb (pc + 2) eptr need (rdepth + 1) mcc')
  | 6 ->
      (* JMP (fast-design.md §2) — end-of-branch jump to the group KET; the
         branch matched, so skip the remaining alternatives. rdepth is
         unchanged (the branch's child frame carries the continuation). *)
      (run [@tailcall]) mb code.(pc + 1) eptr sp rdepth mcc
  | 7 ->
      (* SOD \A (pcre2_match.c:6139-6142 / interpreter.ml:3082-3088). *)
      if not (Int.equal eptr mb.start_subject) then
        (backtrack [@tailcall]) mb sp mcc
      else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
  | 8 ->
      (* SOM \G (pcre2_match.c:6243-6246 / interpreter.ml:3162-3173). *)
      if not (Int.equal eptr (mb.start_subject + mb.start_offset)) then
        (backtrack [@tailcall]) mb sp mcc
      else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
  | 9 ->
      (* EOD \z (pcre2_match.c:6154-6163 / interpreter.ml:6334-6352). *)
      (op_eod [@tailcall]) mb pc eptr sp rdepth mcc
  | 10 ->
      (* EODN \Z (pcre2_match.c:6166-6192 / interpreter.ml:6354-6400). *)
      (assert_nl_or_eos [@tailcall]) mb pc eptr sp rdepth mcc
  | 11 ->
      (* CIRC ^ non-multiline (pcre2_match.c:6133-6137 /
         interpreter.ml:3072-3080). *)
      if (not (Int.equal eptr mb.start_subject)) || mb.notbol then
        (backtrack [@tailcall]) mb sp mcc
      else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
  | 12 ->
      (* DOLL $ non-multiline (pcre2_match.c:6147-6151 /
         interpreter.ml:3089-3097). *)
      if mb.noteol then (backtrack [@tailcall]) mb sp mcc
      else if not mb.dollar_endonly then
        (assert_nl_or_eos [@tailcall]) mb pc eptr sp rdepth mcc
      else (op_eod [@tailcall]) mb pc eptr sp rdepth mcc
  | 0 ->
      (* END (fast-design.md §2; op_end_tail interpreter.ml:3416-3492 /
         pcre2_match.c:876-940) — accept, subject to the empty-match and
         ENDANCHORED rejections. top_bracket = 0: the single ovector pair is
         the overall match. *)
      let sm = mb.attempt_start in
      if
        (* pcre2_match.c:881-895 — NOTEMPTY / NOTEMPTY_ATSTART empty-match
           rejection. *)
        Int.equal eptr sm
        && (mb.notempty
           || (mb.notempty_atstart
              && Int.equal sm (mb.start_subject + mb.start_offset)))
      then (backtrack [@tailcall]) mb sp mcc
      else if
        (* pcre2_match.c:897-917 — ENDANCHORED and not at end (op is OP_END
           here, so RRETURN(NOMATCH)). *)
        eptr < mb.end_subject && mb.endanchored
      then (backtrack [@tailcall]) mb sp mcc
      else (
        mb.match_start <- sm - mb.start_subject;
        mb.match_end <- eptr - mb.start_subject;
        match_match)
  | _ ->
      (* Ir_verify rejects any other tag before the runner sees it; a compiled
         Ir.t cannot reach here (fast-design.md §2). *)
      Errors.error_internal

and backtrack (mb : mb) (sp : int) (mcc : int) : int =
  (* fast-design.md §3 — pop the top save record and resume at its handler.
     An empty stack means the whole attempt has no more alternatives: NOMATCH
     (the C's RRETURN unwinding to frame 0, pcre2_match.c:6471). *)
  if sp <= 0 then match_nomatch
  else
    let d = mb.ss.Save_stack.data in
    let sp' = sp - Save_stack.record_width in
    (* safe: 0 < sp and sp is a multiple of record_width produced only by
       [run]'s ALT push, so sp - 3 >= 0 and slots sp'..sp'+2 were written. *)
    let handler = Array.unsafe_get d sp' in
    let e = Array.unsafe_get d (sp' + 1) in
    let dsaved = Array.unsafe_get d (sp' + 2) in
    let code = mb.code in
    if Int.equal dsaved 0 && not (Int.equal code.(handler) 5 (* ALT *)) then
      (* §4 — the top-level group's LAST branch is reached via the preceding
         ALT's handler (a non-ALT head at controlling depth 0): grouploop ticks
         it (it runs in a child frame at rdepth 1), unlike a nested group's
         last branch (bra_loop, no tick). *)
      let mcc' = tick_child mb mcc 1 in
      if mcc' < 0 then mcc'
      else (run [@tailcall]) mb handler e sp' 1 mcc'
    else (run [@tailcall]) mb handler e sp' dsaved mcc

and op_eod (mb : mb) (pc : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  (* pcre2_match.c:6154-6163 (interpreter.ml:6334-6352) — end of subject.
     true_end_subject = end_subject in this port (no UTF fragments). *)
  if eptr < mb.end_subject then (backtrack [@tailcall]) mb sp mcc
  else if mb.partial <> 0 then (
    mb.hitend <- true;
    if mb.partial > 1 then Errors.error_partial
    else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc)
  else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc

and assert_nl_or_eos (mb : mb) (pc : int) (eptr : int) (sp : int)
    (rdepth : int) (mcc : int) : int =
  (* pcre2_match.c:6166-6192 (interpreter.ml:6354-6400) — end of subject or a
     newline that is the last thing in the subject. IS_NEWLINE may update
     mb.nllen (ANY/ANYCRLF), which the [end_subject - nllen] compare then
     reads — the C's evaluation order. *)
  if
    eptr < mb.end_subject
    && ((not (is_newline_at mb eptr))
       || not (Int.equal eptr (mb.end_subject - mb.nllen)))
  then
    if
      (* pcre2_match.c:6171-6180 — a CRLF newline with only its CR at the end
         could be partial. *)
      mb.partial <> 0
      && eptr + 1 >= mb.end_subject
      && Int.equal mb.nltype Newline.nltype_fixed
      && Int.equal mb.nllen 2
      (* safe: eptr < mb.end_subject (first conjunct) <= String.length
         mb.subject. *)
      && Int.equal (Char.code (String.unsafe_get mb.subject eptr)) mb.nl0
    then (
      mb.hitend <- true;
      if mb.partial > 1 then Errors.error_partial
      else (backtrack [@tailcall]) mb sp mcc)
    else (backtrack [@tailcall]) mb sp mcc
  else if mb.partial <> 0 then (
    mb.hitend <- true;
    if mb.partial > 1 then Errors.error_partial
    else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc)
  else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc

(* ---------- The bump-along driver (pcre2_match.c:7151-7617 + 7637-7766) ----------

   A faithful port of the interpreter's driver (interpreter.ml:9004-9569) for
   the non-UTF subset, MINUS UTF fragments (none here) and firstline (rejected
   at compile). The start-of-match scans (first_cu / start_bits / startline)
   and the minlength / req_cu tail optimizations ARE ported, WITHOUT the
   memchr result-caching (result-identical: same attempt positions). This is
   required for LIMIT tick parity (§4): a skipped attempt does zero ticks, so
   naive bump-along would trip a LIMIT_MATCH cap where the interpreter does not.
   The perf-optimized (cached) versions are chunk L; these scalar ones exist
   for correctness. Module-level + [mb]-only so the loop builds no closure and
   allocates nothing per attempt (§8). *)

let req_cu_max = 5000 (* pcre2_internal.h:566-575 / interpreter.ml:8622 *)

(* memchr over the subject: first index in [lo, hi) with code unit [c], or -1.
   Tail-recursive, allocation-free (interpreter.ml:8629-8641). Caller: 0 <= lo,
   hi <= String.length subject. *)
let rec memchr_sub (subject : string) (c : int) (hi : int) (i : int) : int =
  if i >= hi then -1
  (* safe: lo <= i < hi <= String.length subject. *)
  else if Int.equal (Char.code (String.unsafe_get subject i)) c then i
  else (memchr_sub [@tailcall]) subject c hi (i + 1)

(* pcre2_internal.h:510-521 WAS_NEWLINE(p), non-UTF (interpreter.ml:8850-8867).
   Used by the startline scan. May update mb.nllen (ANY/ANYCRLF). *)
let was_newline_at (mb : mb) (p : int) : bool =
  if mb.nltype <> Newline.nltype_fixed then
    p > mb.start_subject
    &&
    let hit =
      Newline.was_newline mb.subject mb.nltype p mb.start_subject mb.nl_scratch
        false
    in
    if hit then mb.nllen <- !(mb.nl_scratch);
    hit
  else
    p >= mb.start_subject + mb.nllen
    (* safe: 0 <= p - nllen and p - nllen < p <= end_subject <=
       String.length subject. *)
    && Int.equal (Char.code (String.unsafe_get mb.subject (p - mb.nllen))) mb.nl0
    && (Int.equal mb.nllen 1
       || Int.equal
            (Char.code (String.unsafe_get mb.subject (p - mb.nllen + 1)))
            mb.nl1)

(* pcre2_match.c:7737-7766 (interpreter.ml:9539-9569) — the final result when
   no attempt matched: PARTIAL if one was recorded (seam start = match_partial),
   else NOMATCH. *)
let final (mb : mb) : int =
  if mb.match_partial >= 0 then (
    mb.match_start <- mb.match_partial;
    mb.match_end <- mb.end_subject;
    Errors.error_partial)
  else Errors.error_nomatch

let rec bump_top (mb : mb) (start_match : int) (req_cu_ptr : int) : int =
  (* pcre2_match.c:7155-7375 (interpreter.ml:9018-9217) — the loop head:
     start-of-match optimizations (firstline rejected at compile). *)
  if mb.no_start_optimize then (run_attempt [@tailcall]) mb start_match req_cu_ptr
  else if mb.anchored then
    (* pcre2_match.c:7188-7215 — anchored: gate the single attempt on the
       first code unit / start bitmap. Unlike first_cu_tail, this branch has
       NO partial exception (it breaks unconditionally). *)
    if mb.has_first_cu || mb.use_start_bits then
      let ok =
        start_match < mb.end_subject
        &&
        (* safe: start_match < end_subject <= String.length subject. *)
        let c = Char.code (String.unsafe_get mb.subject start_match) in
        (mb.has_first_cu
        && (Int.equal c mb.first_cu || Int.equal c mb.first_cu2))
        || mb.use_start_bits
           && not
                (Int.equal
                   (* safe: c <= 255, so c lsr 3 <= 31 < 32 = |start_bitmap|. *)
                   (Char.code (Bytes.unsafe_get mb.start_bitmap (c lsr 3))
                   land (1 lsl (c land 7)))
                   0)
      in
      if not ok then final mb
      else (tail_opts [@tailcall]) mb start_match req_cu_ptr
    else (tail_opts [@tailcall]) mb start_match req_cu_ptr
  else if mb.has_first_cu then
    (* pcre2_match.c:7217-7298 — advance to a unique first code unit. *)
    let sm =
      if not (Int.equal mb.first_cu mb.first_cu2) then (
        (* caseless: earliest occurrence of either case. *)
        let e = mb.end_subject in
        let p1 = memchr_sub mb.subject mb.first_cu e start_match in
        let p2 = memchr_sub mb.subject mb.first_cu2 e start_match in
        if p1 < 0 then if p2 < 0 then e else p2
        else if p2 < 0 || p1 < p2 then p1
        else p2)
      else
        let r = memchr_sub mb.subject mb.first_cu mb.end_subject start_match in
        if r < 0 then mb.end_subject else r
    in
    (first_cu_tail [@tailcall]) mb sm req_cu_ptr
  else if mb.startline then (
    (* pcre2_match.c:7318-7349 — advance to just after a line break. *)
    let sm = ref start_match in
    if !sm > mb.start_subject + mb.start_offset then (
      while !sm < mb.end_subject && not (was_newline_at mb !sm) do
        incr sm
      done;
      (* pcre2_match.c:7339-7347 — CR then LF under ANY/ANYCRLF: advance one
         more. safe: !sm - 1 >= start_offset >= 0; !sm < end guards subject[!sm]. *)
      if
        Int.equal (Char.code (String.unsafe_get mb.subject (!sm - 1))) Newline.char_cr
        && (Int.equal mb.nltype Newline.nltype_any
           || Int.equal mb.nltype Newline.nltype_anycrlf)
        && !sm < mb.end_subject
        && Int.equal (Char.code (String.unsafe_get mb.subject !sm)) Newline.char_lf
      then incr sm);
    (tail_opts [@tailcall]) mb !sm req_cu_ptr)
  else if mb.use_start_bits then (
    (* pcre2_match.c:7351-7375 — advance to a non-unique first code unit. *)
    let sm = ref start_match in
    let brk = ref false in
    while (not !brk) && !sm < mb.end_subject do
      (* safe: !sm < end_subject <= String.length subject; c <= 255. *)
      let c = Char.code (String.unsafe_get mb.subject !sm) in
      if
        not
          (Int.equal
             (Char.code (Bytes.unsafe_get mb.start_bitmap (c lsr 3))
             land (1 lsl (c land 7)))
             0)
      then brk := true
      else incr sm
    done;
    (first_cu_tail [@tailcall]) mb !sm req_cu_ptr)
  else (tail_opts [@tailcall]) mb start_match req_cu_ptr

and first_cu_tail (mb : mb) (start_match : int) (req_cu_ptr : int) : int =
  (* pcre2_match.c:7300-7315 (interpreter.ml:9219-9233) — break on failure to
     find the first code unit at the true end, EXCEPT for partial matching. *)
  if Int.equal mb.partial 0 && start_match >= mb.end_subject then final mb
  else (tail_opts [@tailcall]) mb start_match req_cu_ptr

and tail_opts (mb : mb) (start_match : int) (req_cu_ptr : int) : int =
  (* pcre2_match.c:7378-7481 (interpreter.ml:9235-9301) — minlength and req_cu.
     Both are DISABLED for partial matching (a lower bound on a COMPLETE
     match), so partial keeps every attempt. *)
  if Int.equal mb.partial 0 then
    if mb.end_subject - start_match < mb.minlength then final mb
    else
      let p = start_match + if mb.has_first_cu then 1 else 0 in
      if mb.has_req_cu && p > req_cu_ptr then
        let check_length = mb.end_subject - start_match in
        if
          check_length < req_cu_max
          || ((not mb.anchored) && check_length < req_cu_max * 1000)
        then
          let np =
            if not (Int.equal mb.req_cu mb.req_cu2) then (
              let r = memchr_sub mb.subject mb.req_cu mb.end_subject p in
              if r < 0 then
                let r2 = memchr_sub mb.subject mb.req_cu2 mb.end_subject p in
                if r2 < 0 then mb.end_subject else r2
              else r)
            else
              let r = memchr_sub mb.subject mb.req_cu mb.end_subject p in
              if r < 0 then mb.end_subject else r
          in
          if np >= mb.end_subject then final mb
          else (run_attempt [@tailcall]) mb start_match np
        else (run_attempt [@tailcall]) mb start_match req_cu_ptr
      else (run_attempt [@tailcall]) mb start_match req_cu_ptr
  else (run_attempt [@tailcall]) mb start_match req_cu_ptr

and run_attempt (mb : mb) (start_match : int) (req_cu_ptr : int) : int =
  (* pcre2_match.c:7485-7577 (interpreter.ml:9303-9381) — bumpalong-limit
     check, per-attempt resets, match(), then the rc switch (only NOMATCH,
     PARTIAL and errors arise in this subset — verbs are Unsupported). *)
  (* bumpalong_limit = true_end_subject = end_subject (no offset limit). *)
  if start_match > mb.end_subject then final mb
  else (
    mb.attempt_start <- start_match;
    mb.start_used_ptr <- start_match;
    let rc =
      (* pcre2_match.c:644-657 + 776-783 — match() enters through frame 0's
         new_frame; its tick (count 0 -> 1, rdepth 0) runs BEFORE any group
         processing (interpreter.ml:8614 / 1301-1306). No heap check for
         frame 0 (create, not push). *)
      if 0 >= mb.match_limit then Errors.error_matchlimit
      else if 0 >= mb.match_limit_depth then Errors.error_depthlimit
      else run mb 0 start_match 0 0 1
    in
    (* pcre2_match.c:7521-7525 — remember the first partial-match start. *)
    if mb.hitend && mb.match_partial < 0 then mb.match_partial <- start_match;
    if Int.equal rc match_match then rc (* mb.match_start/end set by END *)
    else if Int.equal rc Errors.error_partial then (
      (* hard partial: mirror endloop_tail's partial construction. *)
      mb.match_start <- mb.match_partial;
      mb.match_end <- mb.end_subject;
      Errors.error_partial)
    else if not (Int.equal rc match_nomatch) then rc (* limit / heap error *)
    else if mb.anchored then final mb
    else
      (* pcre2_match.c:7552-7614 — NOMATCH advances by one code unit (non-UTF),
         breaks past end_subject, applies the CRLF advance, then re-scans. *)
      let sm2 = start_match + 1 in
      if sm2 > mb.end_subject then final mb
      else
        let sm2 =
          if
            sm2 > mb.start_subject + mb.start_offset
            (* safe: sm2 >= 1 and sm2 <= end_subject <= String.length subject. *)
            && Int.equal
                 (Char.code (String.unsafe_get mb.subject (sm2 - 1)))
                 Newline.char_cr
            && sm2 < mb.end_subject
            && Int.equal
                 (Char.code (String.unsafe_get mb.subject sm2))
                 Newline.char_lf
            && (not mb.hascrorlf)
            && (Int.equal mb.nltype Newline.nltype_any
               || Int.equal mb.nltype Newline.nltype_anycrlf
               || Int.equal mb.nllen 2)
          then sm2 + 1
          else sm2
        in
        (bump_top [@tailcall]) mb sm2 req_cu_ptr)

(* ---------- Cross-exec scratch (mb reuse, §3 / frames.ml scratch trio) ---------- *)

(* ONE cached match block, reused whenever this exec owns the save stack's
   busy flag (the single acquire). A busy slot (concurrent exec) allocates a
   fresh mb + fresh save stack, never written back — mirrors
   interpreter.ml's fresh_trio gated on Frames.holds_scratch. *)
let scratch_mb : mb option ref = ref None

(* ---------- Public exec ----------

   Mirrors Interpreter.pcre2_match's entry checks and result codes
   (pcre2_match.c:6595-6666 / interpreter.ml:9597-9650) for this subset. The
   raw match rc travels in [orc]: 1 (match), -1 (NOMATCH), -2 (PARTIAL), or
   another negative error; [ostart]/[oend] are the ovector pair (meaningful
   for rc = 1 and rc = -2). Values are copied out of the reused [mb] BEFORE
   the scratch is released, so the caller may read them safely. One small
   record per exec (the loop itself allocates nothing). *)
type outcome = { orc : int; ostart : int; oend : int }

let entry_error (rc : int) : outcome = { orc = rc; ostart = 0; oend = 0 }

let exec (ir : Ir.t) ~(subject : string) ~(offset : int) ~(options : int) :
    outcome =
  let re = ir.Ir.re in
  let length = String.length subject in
  (* pcre2_match.c:6595-6597 — undefined public match option bits -> -34. *)
  if not (Int.equal (options land lnot Options.public_match_options) 0) then
    entry_error Errors.error_badoption
    (* pcre2_match.c:6601-6610 — negative offset or offset > length -> -33. *)
  else if offset < 0 || offset > length then entry_error Errors.error_badoffset
  else
    (* pcre2_match.c:6621-6625 — transfer the NOTEMPTY / NOTEMPTY_ATSTART verb
       flag bits into the options (interpreter.ml:9621-9624). *)
    let ff = Compile.notempty_set lor Compile.ne_atst_set in
    let oo = Options.notempty lor Options.notempty_atstart in
    let options =
      options lor (re.Compile.flags land ff / (ff land -ff / (oo land -oo)))
    in
    (* pcre2_match.c:6656-6659 — partial flags to an integer. *)
    let partial =
      if not (Int.equal (options land Options.partial_hard) 0) then 2
      else if not (Int.equal (options land Options.partial_soft) 0) then 1
      else 0
    in
    if
      (* pcre2_match.c:6661-6666 — PARTIAL with ENDANCHORED -> -34. *)
      partial <> 0
      && not
           (Int.equal
              (re.Compile.overall_options lor options land Options.endanchored)
              0)
    then entry_error Errors.error_badoption
    else
      (* pcre2_match.c:7036-7046 — pattern limits override the defaults only
         when smaller (interpreter.ml:9825-9837). *)
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
      let frame_size_bytes = Frames.frame_size_bytes_for ~top_bracket:0 in
      (* pcre2_match.c:7048-7060 via Frames.create (frames.ml:305-321) — the
         initial shadow frame-vector size, capped by the heap limit; may fail
         with HEAPLIMIT before any attempt. *)
      let heapframes_size0 = frame_size_bytes * 10 in
      let heapframes_size0 =
        if heapframes_size0 < Limits.start_frames_size then
          Limits.start_frames_size
        else heapframes_size0
      in
      let heapframes_size0 =
        if heapframes_size0 / 1024 > heap_limit then
          let max_size = 1024 * heap_limit in
          if max_size < frame_size_bytes then Errors.error_heaplimit
          else max_size
        else heapframes_size0
      in
      if heapframes_size0 < 0 then entry_error heapframes_size0 (* HEAPLIMIT *)
      else (
        (* Acquire the scratch bundle (single acquire, gated reuse). *)
        let holds = Save_stack.try_acquire () in
        let mb =
          if holds then
            match !scratch_mb with
            | Some m -> m
            | None ->
                let m = make_mb Save_stack.scratch in
                scratch_mb := Some m;
                m
          else make_mb (Save_stack.fresh ())
        in
        (* Fill the match block (interpreter.ml:9868-9962 field block). *)
        mb.code <- ir.Ir.code;
        mb.lit <- ir.Ir.lit;
        mb.subject <- subject;
        mb.end_subject <- length;
        mb.start_subject <- 0;
        mb.start_offset <- offset;
        mb.partial <- partial;
        mb.notempty <- not (Int.equal (options land Options.notempty) 0);
        mb.notempty_atstart <-
          not (Int.equal (options land Options.notempty_atstart) 0);
        mb.endanchored <-
          not
            (Int.equal
               (re.Compile.overall_options lor options land Options.endanchored)
               0);
        mb.notbol <- not (Int.equal (options land Options.notbol) 0);
        mb.noteol <- not (Int.equal (options land Options.noteol) 0);
        mb.dollar_endonly <-
          not (Int.equal (re.Compile.overall_options land Options.dollar_endonly) 0);
        mb.anchored <-
          not
            (Int.equal
               (re.Compile.overall_options lor options land Options.anchored)
               0);
        mb.hascrorlf <-
          not (Int.equal (re.Compile.flags land Compile.hascrorlf) 0);
        mb.allowemptypartial <-
          re.Compile.max_lookbehind > 0
          || not (Int.equal (re.Compile.flags land Compile.match_empty) 0);
        (* Start-of-match scan config (pcre2_match.c:7091-7131 + 7155-7160 /
           interpreter.ml:9990-9048). first_cu2 / req_cu2 use the plain
           lowercase-to-uppercase fold (Chartables.fcc); the UTF/UCP othercase
           branch (:7101-7107) never applies (subset is non-UTF, non-UCP). *)
        mb.no_start_optimize <-
          not (Int.equal (re.Compile.overall_options land Options.no_start_optimize) 0);
        mb.start_bitmap <- re.Compile.start_bitmap;
        let has_first_cu =
          not (Int.equal (re.Compile.flags land Compile.firstset) 0)
        in
        mb.has_first_cu <- has_first_cu;
        let first_cu = re.Compile.first_codeunit land 0xff in
        mb.first_cu <- first_cu;
        mb.first_cu2 <-
          (if
             has_first_cu
             && not (Int.equal (re.Compile.flags land Compile.firstcaseless) 0)
           then Chartables.fcc first_cu
           else first_cu);
        mb.startline <-
          not (Int.equal (re.Compile.flags land Compile.startline) 0);
        mb.use_start_bits <-
          (not has_first_cu) && (not mb.startline)
          && not (Int.equal (re.Compile.flags land Compile.firstmapset) 0);
        let has_req_cu =
          not (Int.equal (re.Compile.flags land Compile.lastset) 0)
        in
        mb.has_req_cu <- has_req_cu;
        let req_cu = re.Compile.last_codeunit land 0xff in
        mb.req_cu <- req_cu;
        mb.req_cu2 <-
          (if
             has_req_cu
             && not (Int.equal (re.Compile.flags land Compile.lastcaseless) 0)
           then Chartables.fcc req_cu
           else req_cu);
        mb.minlength <- re.Compile.minlength;
        mb.match_limit <- match_limit;
        mb.match_limit_depth <- match_limit_depth;
        mb.heap_limit <- heap_limit;
        mb.frame_size_bytes <- frame_size_bytes;
        mb.heapframes_size <- heapframes_size0;
        mb.hitend <- false;
        mb.match_partial <- -1;
        mb.match_start <- 0;
        mb.match_end <- 0;
        (* Newline convention (interpreter.ml:9937-9964). *)
        let nlc = re.Compile.newline_convention in
        let nl_valid =
          if Int.equal nlc Options.newline_cr then (
            mb.nltype <- Newline.nltype_fixed;
            mb.nllen <- 1;
            mb.nl0 <- Newline.char_cr;
            mb.nl1 <- 0;
            true)
          else if Int.equal nlc Options.newline_lf then (
            mb.nltype <- Newline.nltype_fixed;
            mb.nllen <- 1;
            mb.nl0 <- Newline.char_lf;
            mb.nl1 <- 0;
            true)
          else if Int.equal nlc Options.newline_nul then (
            mb.nltype <- Newline.nltype_fixed;
            mb.nllen <- 1;
            mb.nl0 <- 0;
            mb.nl1 <- 0;
            true)
          else if Int.equal nlc Options.newline_crlf then (
            mb.nltype <- Newline.nltype_fixed;
            mb.nllen <- 2;
            mb.nl0 <- Newline.char_cr;
            mb.nl1 <- Newline.char_lf;
            true)
          else if Int.equal nlc Options.newline_any then (
            mb.nltype <- Newline.nltype_any;
            mb.nllen <- 0;
            mb.nl0 <- 0;
            mb.nl1 <- 0;
            true)
          else if Int.equal nlc Options.newline_anycrlf then (
            mb.nltype <- Newline.nltype_anycrlf;
            mb.nllen <- 0;
            mb.nl0 <- 0;
            mb.nl1 <- 0;
            true)
          else false
        in
        (* pcre2_match.c:6601-6602 + 7139-7151 (interpreter.ml:10100) — enter
           the bump-along loop: start_match = offset, req_cu_ptr one before it. *)
        let rc =
          if not nl_valid then Errors.error_internal
          else bump_top mb offset (offset - 1)
        in
        (* Copy the ovector pair out of the reused [mb] before releasing. *)
        let ostart = mb.match_start and oend = mb.match_end in
        if holds then Save_stack.release ();
        { orc = rc; ostart; oend })

(* ---------- Module-initialization asserts ----------

   Pin every literal int tag used by [run]'s dispatch arms and the two
   ALT-detection sites (the BRA arm's `code.(pc + 1) 5` and [backtrack]'s
   `code.(handler) 5`) against Ir's constants (interpreter.ml's opcode-pin
   style, see its dispatch note): if a tag value in ir.ml ever changes, the
   library fails to load rather than silently mis-dispatching. *)
let () =
  assert (Int.equal Ir.t_end 0);
  assert (Int.equal Ir.t_char_run 1);
  assert (Int.equal Ir.t_chari 2);
  assert (Int.equal Ir.t_bra 3);
  assert (Int.equal Ir.t_ket 4);
  assert (Int.equal Ir.t_alt 5);
  assert (Int.equal Ir.t_jmp 6);
  assert (Int.equal Ir.t_sod 7);
  assert (Int.equal Ir.t_som 8);
  assert (Int.equal Ir.t_eod 9);
  assert (Int.equal Ir.t_eodn 10);
  assert (Int.equal Ir.t_circ 11);
  assert (Int.equal Ir.t_doll 12);
  assert (Int.equal Ir.max_tag 12)
