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
module Opcodes = Pcre2_engine.Opcodes
module Char_predicates = Pcre2_engine.Char_predicates

(* Chunk E — repeat-kind discriminators (fast-design.md §2/§4). [setup_rep]
   maps each REP superinstruction to one of these; the forward min/greedy
   loops and the REP_MIN/REP_MAX backtracks dispatch on it. *)
let rk_char = 0 (* rep_c1/rep_c2/rep_want *)
let rk_ctype = 1 (* rep_mask/rep_want (\d \D \s \S \w \W) *)
let rk_class = 2 (* rep_map_off (bitmap) *)
let rk_hspace = 3 (* rep_want (\h \H) *)
let rk_vspace = 4 (* rep_want (\v \V) *)
let rk_allany = 5 (* OP_ALLANY / OP_ANYBYTE (any code unit) *)
let rk_any = 6 (* OP_ANY (any except newline) *)
let rk_anynl = 7 (* OP_ANYNL (\R, variable length) *)

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
  mutable alt_circumflex : bool; (* PCRE2_ALT_CIRCUMFLEX (OP_CIRCM arm) *)
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
  (* capture ovector (fast-design.md §3). Size = 2 * oveccount, grown/reused
     across execs; slots [0,1] = whole match, [2N,2N+1] = group N. Group slots
     reset to UNSET per attempt; CAP_START/CAP_END write them; the CAP cleanup
     record restores on backtrack. [rc] = the pcre2 pair count at END. *)
  mutable ovector : int array;
  mutable oveccount : int; (* re.top_bracket + 1 (pairs) *)
  mutable rc : int; (* high-water pair count at a successful END *)
  (* Repeated-group iteration-start positions (chunk D2, fast-design.md §3),
     indexed by the group id in t_group_start / t_ket_rmax / t_ket_rmin.
     Slot g holds the eptr at the START of group g's current iteration; a
     t_group_start writes it, the repeating ket reads it for the empty-string
     loop check (pcre2_match.c:6107, Feptr != P->eptr). Written only by g's
     entry and read only by g's ket, so no save/restore is needed. Sized
     [ir.n_groups], reused/grown across execs. *)
  mutable group_start : int array;
  (* Repeat scratch (single-char repeat superinstructions): the invariant
     parameters of the repeat currently being scanned (fast-design.md §2/§4).
     Live only during a repeat's forward min-loop / greedy scan; the REP_MIN
     backtrack re-reads its char test from the IR, so a nested repeat may
     clobber these freely. *)
  mutable rep_pc : int; (* the repeat instruction head *)
  mutable rep_want : bool; (* true = positive (REP/REPI); false = NOT *)
  mutable rep_c1 : int;
  mutable rep_c2 : int; (* = rep_c1 for caseful *)
  mutable rep_lmin : int;
  mutable rep_lmax : int;
  mutable rep_reptype : int; (* 0 min / 1 max / 2 pos *)
  mutable rep_cont : int; (* continuation pc (past the repeat) *)
  mutable rep_floor : int; (* maximize: position after lmin (Lstart_eptr) *)
  (* Chunk E repeat scratch: the [rep_kind] discriminator selects the per-unit
     test (fast-design.md §2/§4). rep_char (0) uses rep_c1/rep_c2/rep_want;
     rep_ctype (1) uses rep_mask/rep_want; rep_class (2) uses rep_map_off;
     rep_hspace (3)/rep_vspace (4) use rep_want; rep_allany (5)/rep_any (6)/
     rep_anynl (7) carry no extra payload. Set by [setup_rep] on the forward
     path and re-derived from the IR on backtrack (a nested repeat in the
     continuation may clobber these freely). *)
  mutable rep_kind : int;
  mutable rep_mask : int; (* ctype mask (rep_ctype) *)
  mutable rep_map_off : int; (* class bitmap byte offset in [bytecode] *)
  (* Chunk E: the shared compiler's bytecode ([re.code]), pinned for the class
     bitmap reads (t_class / t_class_rep); the 32-byte map stays in place. *)
  mutable bytecode : Bytes.t;
  mutable bsr_anycrlf : bool; (* bsr_convention = BSR_ANYCRLF (\R) *)
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
    alt_circumflex = false;
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
    ovector = [||];
    oveccount = 1;
    rc = 1;
    group_start = [||];
    rep_pc = 0;
    rep_want = true;
    rep_c1 = 0;
    rep_c2 = 0;
    rep_lmin = 0;
    rep_lmax = 0;
    rep_reptype = 0;
    rep_cont = 0;
    rep_floor = 0;
    rep_kind = 0;
    rep_mask = 0;
    rep_map_off = 0;
    bytecode = Bytes.empty;
    bsr_anycrlf = false;
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

(* pcre2_internal.h:510-521 WAS_NEWLINE(p), non-UTF (interpreter.ml:8850-8867).
   Used by the OP_CIRCM arm and the startline scan. May update mb.nllen
   (ANY/ANYCRLF). *)
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

(* ---------- Character-type / class predicates (chunk E) ---------- *)

(* pcre2_match.c:1930 + 2014 — Lbyte_map[fc/8] & (1u << (fc&7)): probe the
   32-byte class bitmap at byte offset [off] in [mb.bytecode] (= re.code). In
   non-UTF a code unit is 0..255, so OP_CLASS and OP_NCLASS both reduce to this
   bitmap test (interpreter.ml:721 class_bit / scan_class_min). Returns true
   iff [cc] is in the class. *)
let class_bit_at (mb : mb) (off : int) (cc : int) : bool =
  (* safe: Ir_verify proved off + 32 <= Bytes.length mb.bytecode, and cc <= 255
     so cc lsr 3 <= 31 < 32. *)
  not
    (Int.equal
       (Char.code (Bytes.unsafe_get mb.bytecode (off + (cc lsr 3)))
       land (1 lsl (cc land 7)))
       0)

(* One code unit [cc] against a single character-type opcode that consumes
   exactly one code unit and whose predicate is position-independent
   (pcre2_match.c:2305-2470 non-UTF; the \D \d \S \s \W \w / \h \H \v \V /
   OP_ALLANY / OP_ANYBYTE single arms). OP_ANY (newline-sensitive) and OP_ANYNL
   (variable length) are NOT handled here — their arms are special. Used by the
   single-type arm; the repeat hot loop uses the precomputed rep_mask / rep_kind
   instead (equivalent by construction). *)
let simple_type_match (type_op : int) (cc : int) : bool =
  if Int.equal type_op Opcodes.op_digit then
    not (Int.equal (Chartables.ctypes cc land Chartables.ctype_digit) 0)
  else if Int.equal type_op Opcodes.op_not_digit then
    Int.equal (Chartables.ctypes cc land Chartables.ctype_digit) 0
  else if Int.equal type_op Opcodes.op_whitespace then
    not (Int.equal (Chartables.ctypes cc land Chartables.ctype_space) 0)
  else if Int.equal type_op Opcodes.op_not_whitespace then
    Int.equal (Chartables.ctypes cc land Chartables.ctype_space) 0
  else if Int.equal type_op Opcodes.op_wordchar then
    not (Int.equal (Chartables.ctypes cc land Chartables.ctype_word) 0)
  else if Int.equal type_op Opcodes.op_not_wordchar then
    Int.equal (Chartables.ctypes cc land Chartables.ctype_word) 0
  else if Int.equal type_op Opcodes.op_hspace then Char_predicates.hspace_byte cc
  else if Int.equal type_op Opcodes.op_not_hspace then
    not (Char_predicates.hspace_byte cc)
  else if Int.equal type_op Opcodes.op_vspace then Char_predicates.vspace_byte cc
  else if Int.equal type_op Opcodes.op_not_vspace then
    not (Char_predicates.vspace_byte cc)
  else true (* op_allany / op_anybyte — match any single code unit *)

(* [setup_rep] decodes the REP superinstruction at [pc] into mb.rep_* (chunk E,
   fast-design.md §2/§4). Called on the forward path (op_repeat) and re-called
   on backtrack (KIND_REP_MIN/MAX) to restore the fields a nested repeat may
   have clobbered — it writes only mb.rep_*, so it is idempotent. *)
let setup_rep (mb : mb) (pc : int) : unit =
  let code = mb.code in
  let tag = code.(pc) in
  mb.rep_pc <- pc;
  mb.rep_reptype <- code.(pc + 1);
  mb.rep_lmin <- code.(pc + 2);
  mb.rep_lmax <- code.(pc + 3);
  if Int.equal tag 18 (* t_rep *) then (
    mb.rep_kind <- rk_char;
    mb.rep_c1 <- code.(pc + 4);
    mb.rep_c2 <- code.(pc + 4);
    mb.rep_want <- true;
    mb.rep_cont <- pc + 5)
  else if Int.equal tag 19 (* t_repi *) then (
    mb.rep_kind <- rk_char;
    mb.rep_c1 <- code.(pc + 4);
    mb.rep_c2 <- code.(pc + 5);
    mb.rep_want <- true;
    mb.rep_cont <- pc + 6)
  else if Int.equal tag 20 (* t_notrep *) then (
    mb.rep_kind <- rk_char;
    mb.rep_c1 <- code.(pc + 4);
    mb.rep_c2 <- code.(pc + 4);
    mb.rep_want <- false;
    mb.rep_cont <- pc + 5)
  else if Int.equal tag 21 (* t_notrepi *) then (
    mb.rep_kind <- rk_char;
    mb.rep_c1 <- code.(pc + 4);
    mb.rep_c2 <- code.(pc + 5);
    mb.rep_want <- false;
    mb.rep_cont <- pc + 6)
  else if Int.equal tag 31 (* t_class_rep *) then (
    mb.rep_kind <- rk_class;
    mb.rep_map_off <- code.(pc + 4);
    mb.rep_cont <- pc + 5)
  else (
    (* tag 30 t_type_rep — map the C type opcode to a rep_kind + payload. *)
    let ct = code.(pc + 4) in
    mb.rep_cont <- pc + 5;
    if Int.equal ct Opcodes.op_digit then (
      mb.rep_kind <- rk_ctype;
      mb.rep_mask <- Chartables.ctype_digit;
      mb.rep_want <- true)
    else if Int.equal ct Opcodes.op_not_digit then (
      mb.rep_kind <- rk_ctype;
      mb.rep_mask <- Chartables.ctype_digit;
      mb.rep_want <- false)
    else if Int.equal ct Opcodes.op_whitespace then (
      mb.rep_kind <- rk_ctype;
      mb.rep_mask <- Chartables.ctype_space;
      mb.rep_want <- true)
    else if Int.equal ct Opcodes.op_not_whitespace then (
      mb.rep_kind <- rk_ctype;
      mb.rep_mask <- Chartables.ctype_space;
      mb.rep_want <- false)
    else if Int.equal ct Opcodes.op_wordchar then (
      mb.rep_kind <- rk_ctype;
      mb.rep_mask <- Chartables.ctype_word;
      mb.rep_want <- true)
    else if Int.equal ct Opcodes.op_not_wordchar then (
      mb.rep_kind <- rk_ctype;
      mb.rep_mask <- Chartables.ctype_word;
      mb.rep_want <- false)
    else if Int.equal ct Opcodes.op_hspace then (
      mb.rep_kind <- rk_hspace;
      mb.rep_want <- true)
    else if Int.equal ct Opcodes.op_not_hspace then (
      mb.rep_kind <- rk_hspace;
      mb.rep_want <- false)
    else if Int.equal ct Opcodes.op_vspace then (
      mb.rep_kind <- rk_vspace;
      mb.rep_want <- true)
    else if Int.equal ct Opcodes.op_not_vspace then (
      mb.rep_kind <- rk_vspace;
      mb.rep_want <- false)
    else if Int.equal ct Opcodes.op_anynl then mb.rep_kind <- rk_anynl
    else if Int.equal ct Opcodes.op_any then mb.rep_kind <- rk_any
    else (* op_allany / op_anybyte *) mb.rep_kind <- rk_allany)

(* True iff the repeat instruction at [rep_pc] is a \R (OP_ANYNL) type repeat.
   Used only on the (cold) greedy give-back path. *)
let is_anynl_rep (mb : mb) (rep_pc : int) : bool =
  Int.equal mb.code.(rep_pc) 30 (* t_type_rep *)
  && Int.equal mb.code.(rep_pc + 4) Opcodes.op_anynl

(* The corrected greedy give-back position for a maximizing repeat
   (pcre2_match.c:4963-4967 / RM34). [x] is the candidate position after one
   Feptr-- step; for a \R repeat, backing into the middle of a CRLF pair is not
   a valid \R boundary, so step back once more. Non-\R repeats return [x]. *)
let giveback_pos (mb : mb) (rep_pc : int) (floor : int) (x : int) : int =
  if
    is_anynl_rep mb rep_pc && x > floor && x < mb.end_subject
    (* safe: 0 <= floor < x < end_subject <= String.length subject, so x and
       x-1 are valid indices. *)
    && Int.equal (Char.code (String.unsafe_get mb.subject x)) Newline.char_lf
    && Int.equal
         (Char.code (String.unsafe_get mb.subject (x - 1)))
         Newline.char_cr
  then x - 1
  else x

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

(* pcre2_match.c:919-940 — the rc / end_offset_top a successful match
   returns. end_offset_top = 2 * (highest-numbered group whose start slot is
   set); rc = end_offset_top/2 + 1 (interpreter.ml:9530-9531). On the
   surviving path a completed group's start slot holds a real offset and any
   backtracked-past group was restored to UNSET by its CAP cleanup, so the
   highest set group equals the interpreter's offset_top/2 (fast-design.md
   §3). Scans once per successful match (never in the hot loop). *)
let rc_of_ovector (mb : mb) : int =
  let ov = mb.ovector in
  let rec scan (n : int) : int =
    if n <= 0 then 1
    else if not (Int.equal ov.(2 * n) Frames.unset) then n + 1
    else (scan [@tailcall]) (n - 1)
  in
  scan (mb.oveccount - 1)

(* Push a KIND_CONT record [target; eptr; rdepth; KIND_CONT] at [sp]
   (fast-design.md §3): the chunk-D2 group choice point shared by BRAZERO,
   BRAMINZERO, the greedy KETRMAX give-back and the lazy KETRMIN reiteration.
   Module-level + fully applied so the loop builds no closure (§8); grows the
   save stack cold when a push would overflow. *)
let push_cont (mb : mb) (sp : int) (target : int) (eptr : int) (rdepth : int) :
    unit =
  let ss = mb.ss in
  let need = sp + Save_stack.width_cont in
  if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
  let d = ss.Save_stack.data in
  (* safe: [grow] ensured Array.length d >= sp + width_cont. *)
  Array.unsafe_set d sp target;
  Array.unsafe_set d (sp + 1) eptr;
  Array.unsafe_set d (sp + 2) rdepth;
  Array.unsafe_set d (sp + 3) Save_stack.kind_cont

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
        let need = sp + Save_stack.width_alt in
        if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
        let d = ss.Save_stack.data in
        (* safe: [grow] above ensured Array.length d >= sp + width_alt. Record
           layout (fast-design.md §3): [handler; eptr; rdepth; KIND_ALT]. *)
        Array.unsafe_set d sp handler;
        Array.unsafe_set d (sp + 1) eptr;
        Array.unsafe_set d (sp + 2) rdepth;
        Array.unsafe_set d (sp + 3) Save_stack.kind_alt;
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
  | 13 ->
      (* OP_CIRCM ^ multiline (pcre2_match.c:6200-6211 /
         interpreter.ml:3108-3124). No tick (dispatched in place). *)
      if mb.notbol && Int.equal eptr mb.start_subject then
        (backtrack [@tailcall]) mb sp mcc
      else if
        (not (Int.equal eptr mb.start_subject))
        && ((Int.equal eptr mb.end_subject && not mb.alt_circumflex)
           || not (was_newline_at mb eptr))
      then (backtrack [@tailcall]) mb sp mcc
      else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
  | 14 ->
      (* OP_DOLLM $ multiline (pcre2_match.c:6214-6239 /
         interpreter.ml:3126-3161). *)
      if eptr < mb.end_subject then
        if not (is_newline_at mb eptr) then
          if
            (* pcre2_match.c:6220-6228 — a CRLF pattern newline with only its
               CR present at the end could be partial. *)
            mb.partial <> 0
            && eptr + 1 >= mb.end_subject
            && Int.equal mb.nltype Newline.nltype_fixed
            && Int.equal mb.nllen 2
            (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
            && Int.equal (Char.code (String.unsafe_get mb.subject eptr)) mb.nl0
          then (
            mb.hitend <- true;
            if mb.partial > 1 then Errors.error_partial
            else (backtrack [@tailcall]) mb sp mcc)
          else (backtrack [@tailcall]) mb sp mcc
        else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
      else if mb.noteol then (backtrack [@tailcall]) mb sp mcc
      else
        let r = scheck_partial mb eptr in
        if r < 0 then r else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
  | 15 ->
      (* FAIL (fast-design.md §2/§3) — a grouploop group has exhausted its
         branches: propagate NOMATCH (no tick). Only reachable via a last
         ALT's handler on backtrack; forward flow JMPs over it to the KET. *)
      (backtrack [@tailcall]) mb sp mcc
  | 16 ->
      (* CAP_START (fast-design.md §3; optimized cbracket entry-save
         pcre2_jit_compile.c:11045-11054) — open capture group N. Push a
         cleanup record saving the group's current ovector pair, then set the
         start slot to the current position (used directly as the in-progress
         start, valid because the group is optimized). No tick. *)
      let ovb = code.(pc + 1) in
      let ss = mb.ss in
      let need = sp + Save_stack.width_cap in
      if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
      let d = ss.Save_stack.data in
      let ov = mb.ovector in
      (* safe: [grow] ensured length >= sp + width_cap; ovb in [2, 2*top]
         (Ir_verify), so ovb, ovb+1 < 2*oveccount = Array.length ov. *)
      Array.unsafe_set d sp ovb;
      Array.unsafe_set d (sp + 1) (Array.unsafe_get ov ovb);
      Array.unsafe_set d (sp + 2) (Array.unsafe_get ov (ovb + 1));
      Array.unsafe_set d (sp + 3) Save_stack.kind_cap;
      Array.unsafe_set ov ovb (eptr - mb.start_subject);
      (run [@tailcall]) mb (pc + 2) eptr need rdepth mcc
  | 17 ->
      (* CAP_END (fast-design.md §3; the CBRA/SCBRA ket write
         pcre2_match.c:6077-6084) — close capture group N: record the end
         position. offset_top (the rc high-water) is recomputed at END. No
         tick (the C ket carries on at the same level). *)
      let ovb = code.(pc + 1) in
      (* safe: ovb+1 < 2*oveccount = Array.length mb.ovector (Ir_verify). *)
      Array.unsafe_set mb.ovector (ovb + 1) (eptr - mb.start_subject);
      (run [@tailcall]) mb (pc + 2) eptr sp rdepth mcc
  | 18 | 19 | 20 | 21 | 30 | 31 ->
      (* Repeat superinstructions (fast-design.md §2/§4): char (18-21;
         repeatchar/repeatnotchar interpreter.ml:3549-4128), character type
         (30; repeattype interpreter.ml:4622-...) and class (31; the OP_CLASS
         repeat interpreter.ml:1807-1872) — all non-UTF. *)
      (op_repeat [@tailcall]) mb pc eptr sp rdepth mcc
  | 27 ->
      (* TYPE (fast-design.md §2/§4) — a single character type. *)
      (op_single_type [@tailcall]) mb pc code.(pc + 1) eptr sp rdepth mcc
  | 28 ->
      (* CLASS (fast-design.md §2) — a single 32-byte-bitmap class test
         (OP_CLASS/OP_NCLASS, pcre2_match.c:1933-1972; in non-UTF a lone class
         is class_min with lmin=lmax=1: one code unit, no choice point). *)
      if eptr >= mb.end_subject then (
        let r = scheck_partial mb eptr in
        if r < 0 then r else (backtrack [@tailcall]) mb sp mcc)
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if class_bit_at mb code.(pc + 1) cc then
          (run [@tailcall]) mb (pc + 2) (eptr + 1) sp rdepth mcc
        else (backtrack [@tailcall]) mb sp mcc
  | 29 ->
      (* WORDBOUND \b / \B, non-UCP (fast-design.md §2;
         pcre2_match.c:6258-6333 / interpreter.ml:3181-3332). *)
      (op_wordbound [@tailcall]) mb pc code.(pc + 1) eptr sp rdepth mcc
  | 22 ->
      (* GROUP_START (fast-design.md §3; the C records the group-start eptr in
         the group frame's predecessor P, read at the ket, pcre2_match.c:6081/
         6107) — store this iteration's start position for the empty-string
         loop check, saving the enclosing iteration's start in a KIND_GSTART
         record so backtracking into an earlier iteration's branch restores it
         (the C keeps each iteration's start in its own frame). No tick. *)
      let g = code.(pc + 1) in
      let ss = mb.ss in
      let need = sp + Save_stack.width_gstart in
      if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
      let d = ss.Save_stack.data in
      (* safe: [grow] ensured length >= sp + width_gstart; g in [0, n_groups)
         (Ir_verify) so it indexes mb.group_start. *)
      Array.unsafe_set d sp g;
      Array.unsafe_set d (sp + 1) (Array.unsafe_get mb.group_start g);
      Array.unsafe_set d (sp + 2) Save_stack.kind_gstart;
      Array.unsafe_set mb.group_start g eptr;
      (run [@tailcall]) mb (pc + 2) eptr (sp + Save_stack.width_gstart) rdepth
        mcc
  | 23 ->
      (* BRAZERO (fast-design.md §3; OP_BRAZERO pcre2_match.c:5224-5230) —
         greedy zero-repeat wrapper: RMATCH(bracket, RM9) tries the group
         first (tick, child frame), skips it on backtrack. Push a KIND_CONT
         resuming at [skip] (past the group), then fall through into the group
         body at rdepth+1. *)
      let mcc' = tick_child mb mcc (rdepth + 1) in
      if mcc' < 0 then mcc'
      else (
        push_cont mb sp code.(pc + 1) eptr rdepth;
        (run [@tailcall]) mb (pc + 2) eptr (sp + Save_stack.width_cont)
          (rdepth + 1) mcc')
  | 24 ->
      (* BRAMINZERO (fast-design.md §3; OP_BRAMINZERO pcre2_match.c:5232-5238)
         — lazy zero-repeat wrapper: RMATCH(continuation, RM10) tries the rest
         WITHOUT the group first (tick, child frame), enters the group on
         backtrack. Push a KIND_CONT resuming at the group entry (pc+2), then
         jump to [skip] (the continuation) at rdepth+1. *)
      let mcc' = tick_child mb mcc (rdepth + 1) in
      if mcc' < 0 then mcc'
      else (
        push_cont mb sp (pc + 2) eptr rdepth;
        (run [@tailcall]) mb code.(pc + 1) eptr (sp + Save_stack.width_cont)
          (rdepth + 1) mcc')
  | 25 ->
      (* KET_RMAX (fast-design.md §3/§4; OP_KETRMAX pcre2_match.c:6107-6120) —
         greedy repeating ket. The empty-string loop check
         (Feptr != P->eptr): if this iteration matched empty, forcibly break
         the loop and carry on past the group (no tick). Otherwise record a
         give-back point (KIND_CONT resuming at the continuation pc+3) and try
         one more iteration — RMATCH(bracode, RM7): tick, loop to [entry]. *)
      let g = code.(pc + 2) in
      if g >= 0 && Int.equal eptr (Array.unsafe_get mb.group_start g) then
        (* empty match: break the loop, continue past the group. *)
        (run [@tailcall]) mb (pc + 3) eptr sp rdepth mcc
      else
        let mcc' = tick_child mb mcc (rdepth + 1) in
        if mcc' < 0 then mcc'
        else (
          push_cont mb sp (pc + 3) eptr rdepth;
          (run [@tailcall]) mb code.(pc + 1) eptr (sp + Save_stack.width_cont)
            (rdepth + 1) mcc')
  | 26 ->
      (* KET_RMIN (fast-design.md §3/§4; OP_KETRMIN pcre2_match.c:6109-6114) —
         lazy repeating ket. Empty match forcibly breaks the loop (as KETRMAX).
         Otherwise try the rest of the pattern first — RMATCH(continuation,
         RM6): tick, run pc+3 at rdepth+1 — recording a reiteration point
         (KIND_CONT resuming at [entry]) for backtrack. *)
      let g = code.(pc + 2) in
      if g >= 0 && Int.equal eptr (Array.unsafe_get mb.group_start g) then
        (run [@tailcall]) mb (pc + 3) eptr sp rdepth mcc
      else
        let mcc' = tick_child mb mcc (rdepth + 1) in
        if mcc' < 0 then mcc'
        else (
          push_cont mb sp code.(pc + 1) eptr rdepth;
          (run [@tailcall]) mb (pc + 3) eptr (sp + Save_stack.width_cont)
            (rdepth + 1) mcc')
  | 0 ->
      (* END (fast-design.md §2; op_end_tail interpreter.ml:3416-3492 /
         pcre2_match.c:876-940) — accept, subject to the empty-match and
         ENDANCHORED rejections. *)
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
        (* pcre2_match.c:919-940 — record the whole match in ovector[0,1] and
           compute rc = end_offset_top/2 + 1. The group slots are already in
           place (CAP_END wrote them; the winning path's high-water group N is
           the largest N with a set start slot — equal to the interpreter's
           end_offset_top/2, since a set group completed on the surviving path
           and backtrack-cleanup unset the rest). *)
        let s = sm - mb.start_subject in
        let e = eptr - mb.start_subject in
        mb.match_start <- s;
        mb.match_end <- e;
        mb.ovector.(0) <- s;
        mb.ovector.(1) <- e;
        mb.rc <- rc_of_ovector mb;
        match_match)
  | _ ->
      (* Ir_verify rejects any other tag before the runner sees it; a compiled
         Ir.t cannot reach here (fast-design.md §2). *)
      Errors.error_internal

and backtrack (mb : mb) (sp : int) (mcc : int) : int =
  (* fast-design.md §3 — pop the top save record and act on its KIND. An
     empty stack means the whole attempt has no more alternatives: NOMATCH
     (the C's RRETURN unwinding to frame 0, pcre2_match.c:6471). The KIND is
     the record's TOP slot [data.(sp-1)]; each kind has a fixed width. *)
  if sp <= 0 then match_nomatch
  else
    let d = mb.ss.Save_stack.data in
    (* safe: sp > 0 and sp is the sum of the fixed record widths [run] pushed,
       so sp-1 is a written kind slot and the record base computed below is
       within the reserved region. *)
    let kind = Array.unsafe_get d (sp - 1) in
    if Int.equal kind Save_stack.kind_alt then (
      (* KIND_ALT [handler; eptr; rdepth; kind] — resume the next alternative. *)
      let base = sp - Save_stack.width_alt in
      let handler = Array.unsafe_get d base in
      let e = Array.unsafe_get d (base + 1) in
      let dsaved = Array.unsafe_get d (base + 2) in
      let code = mb.code in
      if
        Int.equal dsaved 0
        && (not (Int.equal code.(handler) 5 (* ALT *)))
        && not (Int.equal code.(handler) 15 (* FAIL *))
      then
        (* §4 — the whole-pattern (bra_loop-lowered) wrapper's LAST branch is
           reached via the preceding ALT's handler (a non-ALT, non-FAIL head at
           controlling depth 0): grouploop ticks it at rdepth 1. A grouploop
           group's FAIL head runs at dsaved >= 1 (it is nested in the wrapper),
           so this tick never fires for it. *)
        let mcc' = tick_child mb mcc 1 in
        if mcc' < 0 then mcc'
        else (run [@tailcall]) mb handler e base 1 mcc'
      else (run [@tailcall]) mb handler e base dsaved mcc)
    else if Int.equal kind Save_stack.kind_cap then (
      (* KIND_CAP [ovbase; old_start; old_end; kind] — restore the capture's
         ovector pair (JIT optimized-cbracket exhaustion restore
         pcre2_jit_compile.c:13443-13452) and keep popping. *)
      let base = sp - Save_stack.width_cap in
      let ovb = Array.unsafe_get d base in
      let ov = mb.ovector in
      (* safe: ovb, ovb+1 written by CAP_START within bounds. *)
      Array.unsafe_set ov ovb (Array.unsafe_get d (base + 1));
      Array.unsafe_set ov (ovb + 1) (Array.unsafe_get d (base + 2));
      (backtrack [@tailcall]) mb base mcc)
    else if Int.equal kind Save_stack.kind_rep_max then
      (* KIND_REP_MAX [rep_pc; try_pos; floor; rdepth; kind] — greedy repeat
         give-back (interpreter.ml maxbt RM26/RM28). *)
      (backtrack_rep_max [@tailcall]) mb sp mcc
    else if Int.equal kind Save_stack.kind_rep_min then
      (* KIND_REP_MIN [rep_pc; count; eptr; rdepth; kind] — minimizing repeat
         extend-by-one (interpreter.ml RM25/RM27). *)
      (backtrack_rep_min [@tailcall]) mb sp mcc
    else if Int.equal kind Save_stack.kind_gstart then (
      (* KIND_GSTART [g; old_start; kind] (fast-design.md §3) — restore group
         g's iteration-start slot to the enclosing iteration's value and keep
         popping (the C's per-frame group start, pcre2_match.c:6107). *)
      let base = sp - Save_stack.width_gstart in
      let g = Array.unsafe_get d base in
      (* safe: g written by GROUP_START, in [0, n_groups). *)
      Array.unsafe_set mb.group_start g (Array.unsafe_get d (base + 1));
      (backtrack [@tailcall]) mb base mcc)
    else
      (* KIND_CONT [target; eptr; rdepth; kind] (fast-design.md §3) — a
         chunk-D2 group choice point: resume at [target] with [eptr]/[rdepth]
         restored and NO tick. This is the C's same-frame `break` after an
         RM9/RM10/RM7 NOMATCH (skip the group / carry on past the group) or
         the KETRMIN reiteration `Fecode -= GET; break`. *)
      let base = sp - Save_stack.width_cont in
      let target = Array.unsafe_get d base in
      let e = Array.unsafe_get d (base + 1) in
      let dsaved = Array.unsafe_get d (base + 2) in
      (run [@tailcall]) mb target e base dsaved mcc

(* KIND_REP_MAX backtrack (fast-design.md §4). Char / type / \R (\R via
   [giveback_pos]) repeats try the continuation one position lower, down to
   [floor] (Lstart_eptr): positions above [floor] run in a child frame
   (tick + rdepth d+1, mirroring the C's typemax_bt / repeatchar maxbt RMATCH);
   at [floor] the C dispatches in place (rdepth d, no tick,
   pcre2_match.c:1451/4960). A CLASS repeat differs: its maxbt while head
   (pcre2_match.c:2143) also RMATCHes the floor position (`Feptr >= Lstart`)
   before failing, so floor is a ticked child, not in-place. *)
and backtrack_rep_max (mb : mb) (sp : int) (mcc : int) : int =
  let d = mb.ss.Save_stack.data in
  let base = sp - Save_stack.width_rep_max in
  let rep_pc = Array.unsafe_get d base in
  let try_pos = Array.unsafe_get d (base + 1) in
  let floor = Array.unsafe_get d (base + 2) in
  let dd = Array.unsafe_get d (base + 3) in
  let cont = rep_pc + Ir.arity.(mb.code.(rep_pc)) in
  if Int.equal mb.code.(rep_pc) 31 (* t_class_rep *) then
    (* CLASS: [floor] is itself a ticked RMATCH; below floor -> NOMATCH. *)
    if try_pos >= floor then (
      let mcc' = tick_child mb mcc (dd + 1) in
      if mcc' < 0 then mcc'
      else (
        Array.unsafe_set d (base + 1) (try_pos - 1);
        (run [@tailcall]) mb cont try_pos sp (dd + 1) mcc'))
    else (backtrack [@tailcall]) mb base mcc (* pop, propagate *)
  else if try_pos > floor then (
    (* char / type / \R: another give-back position (tick, rdepth d+1). Store
       the NEXT give-back position (with the \R mid-CRLF correction). *)
    let mcc' = tick_child mb mcc (dd + 1) in
    if mcc' < 0 then mcc'
    else (
      Array.unsafe_set d (base + 1) (giveback_pos mb rep_pc floor (try_pos - 1));
      (run [@tailcall]) mb cont try_pos sp (dd + 1) mcc'))
  else
    (* try_pos = floor: the minimum-length position, tried in place at rdepth
       d, no tick; pop the record permanently. *)
    (run [@tailcall]) mb cont floor base dd mcc

(* KIND_REP_MIN backtrack (fast-design.md §4): match one more unit at [eptr]
   and retry the continuation (interpreter.ml RM25/RM27 char, RM23 class,
   RM33 type: count++; if count>=Lmax NOMATCH; else match one, RMATCH). Each
   retry is a child-frame tick. Re-derives the repeat's fields from the IR via
   [setup_rep] (a nested repeat may have clobbered the mb.rep fields). *)
and backtrack_rep_min (mb : mb) (sp : int) (mcc : int) : int =
  let d = mb.ss.Save_stack.data in
  let base = sp - Save_stack.width_rep_min in
  let rep_pc = Array.unsafe_get d base in
  let count = Array.unsafe_get d (base + 1) in
  let eptr = Array.unsafe_get d (base + 2) in
  let dd = Array.unsafe_get d (base + 3) in
  setup_rep mb rep_pc;
  if count >= mb.rep_lmax then (backtrack [@tailcall]) mb base mcc
  else
    let k = mb.rep_kind in
    if Int.equal k rk_any then
      (rep_bt_min_any [@tailcall]) mb base count eptr dd mcc
    else if Int.equal k rk_anynl then
      (rep_bt_min_anynl [@tailcall]) mb base count eptr dd mcc
    else if eptr >= mb.end_subject then
      let r = scheck_partial mb eptr in
      if r < 0 then r else (backtrack [@tailcall]) mb base mcc
    else if not (rep_unit_matches mb eptr) then (backtrack [@tailcall]) mb base mcc
    else
      let mcc' = tick_child mb mcc (dd + 1) in
      if mcc' < 0 then mcc'
      else (
        Array.unsafe_set d (base + 1) (count + 1);
        Array.unsafe_set d (base + 2) (eptr + 1);
        (run [@tailcall]) mb mb.rep_cont (eptr + 1) sp (dd + 1) mcc')

(* RM33 for OP_ANY (pcre2_match.c:3973-3989): the reiterated char must not be a
   newline; a CR at the very end could be a partial CRLF. *)
and rep_bt_min_any (mb : mb) (base : int) (count : int) (eptr : int) (dd : int)
    (mcc : int) : int =
  let d = mb.ss.Save_stack.data in
  if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (backtrack [@tailcall]) mb base mcc
  else if is_newline_at mb eptr then (backtrack [@tailcall]) mb base mcc
  else
    let hit =
      mb.partial <> 0
      && eptr + 1 >= mb.end_subject
      && Int.equal mb.nltype Newline.nltype_fixed
      && Int.equal mb.nllen 2
      && Int.equal (Char.code (String.unsafe_get mb.subject eptr)) mb.nl0
    in
    if hit then mb.hitend <- true;
    if hit && mb.partial > 1 then Errors.error_partial
    else
      let mcc' = tick_child mb mcc (dd + 1) in
      if mcc' < 0 then mcc'
      else (
        Array.unsafe_set d (base + 1) (count + 1);
        Array.unsafe_set d (base + 2) (eptr + 1);
        (run [@tailcall]) mb mb.rep_cont (eptr + 1)
          (base + Save_stack.width_rep_min)
          (dd + 1) mcc')

(* RM33 for OP_ANYNL (pcre2_match.c:3994-4017): one more \R sequence (CR then
   optional LF, LF, or VT/FF/NEL outside the ANYCRLF convention). *)
and rep_bt_min_anynl (mb : mb) (base : int) (count : int) (eptr : int)
    (dd : int) (mcc : int) : int =
  if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (backtrack [@tailcall]) mb base mcc
  else
    (* safe: eptr < mb.end_subject <= String.length mb.subject. Branches are
       inlined per case, exactly like the forward loop [rep_min_anynl] — no
       tuple per iteration (§8). *)
    let fc = Char.code (String.unsafe_get mb.subject eptr) in
    let e = eptr + 1 in
    if Int.equal fc Newline.char_cr then
      let e' =
        if
          e < mb.end_subject
          && Int.equal (Char.code (String.unsafe_get mb.subject e)) Newline.char_lf
        then e + 1
        else e
      in
      (rep_bt_min_anynl_step [@tailcall]) mb base count e' dd mcc
    else if Int.equal fc Newline.char_lf then
      (rep_bt_min_anynl_step [@tailcall]) mb base count e dd mcc
    else if
      Int.equal fc Newline.char_vt || Int.equal fc Newline.char_ff
      || Int.equal fc Newline.char_nel
    then
      if mb.bsr_anycrlf then (backtrack [@tailcall]) mb base mcc
      else (rep_bt_min_anynl_step [@tailcall]) mb base count e dd mcc
    else (backtrack [@tailcall]) mb base mcc

(* Shared epilogue of [rep_bt_min_anynl]: one more \R sequence matched, ending
   at [e'] — tick, bump the record's count/eptr, retry the continuation
   (the RM33 RMATCH). All-int tail call, no allocation (§8). *)
and rep_bt_min_anynl_step (mb : mb) (base : int) (count : int) (e' : int)
    (dd : int) (mcc : int) : int =
  let mcc' = tick_child mb mcc (dd + 1) in
  if mcc' < 0 then mcc'
  else (
    let d = mb.ss.Save_stack.data in
    (* safe: base+1/base+2 are inside the popped REP_MIN record [backtrack]
       located at data.(sp-1). *)
    Array.unsafe_set d (base + 1) (count + 1);
    Array.unsafe_set d (base + 2) e';
    (run [@tailcall]) mb mb.rep_cont e'
      (base + Save_stack.width_rep_min)
      (dd + 1) mcc')

(* ---------- Repeat superinstructions (§2/§4) ----------

   Forward execution of one repeat instruction. [setup_rep] stashes the
   invariant parameters in [mb.rep_*]; the tight min/greedy loops pass only the
   varying (i, eptr) + control state. Mirrors repeatchar/repeatnotchar
   (interpreter.ml:3549-4128), the class repeat (interpreter.ml:1807-1872) and
   repeattype (interpreter.ml:4622-...) for the non-UTF case. *)
and op_repeat (mb : mb) (pc : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  setup_rep mb pc;
  let k = mb.rep_kind in
  if Int.equal k rk_allany then (rep_min_allany [@tailcall]) mb eptr sp rdepth mcc
  else if Int.equal k rk_any then (rep_min_any [@tailcall]) mb 0 eptr sp rdepth mcc
  else if Int.equal k rk_anynl then
    (rep_min_anynl [@tailcall]) mb 0 eptr sp rdepth mcc
  else (rep_min_loop [@tailcall]) mb 0 eptr sp rdepth mcc

(* True iff subject[eptr] satisfies the repeat's per-unit test, dispatching on
   [mb.rep_kind] (fast-design.md §4). char: c1/c2 fold-pair vs want; ctype:
   the ctypes mask vs want; class: the bitmap; hspace/vspace: the byte
   predicate vs want; allany: always. safe: caller proves eptr < end_subject.
   \R (rk_anynl) / OP_ANY (rk_any) are NOT here — variable-length / newline-
   sensitive, handled by their dedicated loops. *)
and rep_unit_matches (mb : mb) (eptr : int) : bool =
  let cc = Char.code (String.unsafe_get mb.subject eptr) in
  let k = mb.rep_kind in
  if Int.equal k rk_char then
    Bool.equal (Int.equal cc mb.rep_c1 || Int.equal cc mb.rep_c2) mb.rep_want
  else if Int.equal k rk_ctype then
    Bool.equal
      (not (Int.equal (Chartables.ctypes cc land mb.rep_mask) 0))
      mb.rep_want
  else if Int.equal k rk_class then class_bit_at mb mb.rep_map_off cc
  else if Int.equal k rk_hspace then
    Bool.equal (Char_predicates.hspace_byte cc) mb.rep_want
  else if Int.equal k rk_vspace then
    Bool.equal (Char_predicates.vspace_byte cc) mb.rep_want
  else true (* rk_allany *)

and rep_min_loop (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  (* Ensure the minimum count in place, no ticks (repeatchar_ci_min
     interpreter.ml:3727-3749 / class_min 4351 / typemin_ctype 4894;
     SCHECK_PARTIAL at/past end). char / ctype / class / hspace / vspace. *)
  if i >= mb.rep_lmin then (rep_after_min [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (backtrack [@tailcall]) mb sp mcc
  else if not (rep_unit_matches mb eptr) then (backtrack [@tailcall]) mb sp mcc
  else (rep_min_loop [@tailcall]) mb (i + 1) (eptr + 1) sp rdepth mcc

(* OP_ALLANY / OP_ANYBYTE min loop (pcre2_match.c:3280-3287): one bound check
   for [lmin] code units. The scheck fires at the ORIGINAL eptr (not a
   per-char eptr), which the bulk form preserves. *)
and rep_min_allany (mb : mb) (eptr : int) (sp : int) (rdepth : int) (mcc : int) :
    int =
  if eptr > mb.end_subject - mb.rep_lmin then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (backtrack [@tailcall]) mb sp mcc
  else (rep_after_min [@tailcall]) mb (eptr + mb.rep_lmin) sp rdepth mcc

(* OP_ANY min loop (pcre2_match.c:3258-3278): a newline stops the match; a CR
   at the very end of the subject could be a partial CRLF. *)
and rep_min_any (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if i >= mb.rep_lmin then (rep_after_min [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (backtrack [@tailcall]) mb sp mcc
  else if is_newline_at mb eptr then (backtrack [@tailcall]) mb sp mcc
  else
    let hit =
      mb.partial <> 0
      && eptr + 1 >= mb.end_subject
      && Int.equal mb.nltype Newline.nltype_fixed
      && Int.equal mb.nllen 2
      && Int.equal (Char.code (String.unsafe_get mb.subject eptr)) mb.nl0
    in
    if hit then mb.hitend <- true;
    if hit && mb.partial > 1 then Errors.error_partial
    else (rep_min_any [@tailcall]) mb (i + 1) (eptr + 1) sp rdepth mcc

(* OP_ANYNL min loop (pcre2_match.c:3302-3332): CR absorbs a following LF; LF;
   VT/FF/NEL unless the ANYCRLF convention. *)
and rep_min_anynl (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if i >= mb.rep_lmin then (rep_after_min [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (backtrack [@tailcall]) mb sp mcc
  else
    (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
    let fc = Char.code (String.unsafe_get mb.subject eptr) in
    let e = eptr + 1 in
    if Int.equal fc Newline.char_cr then
      let e' =
        if
          e < mb.end_subject
          && Int.equal (Char.code (String.unsafe_get mb.subject e)) Newline.char_lf
        then e + 1
        else e
      in
      (rep_min_anynl [@tailcall]) mb (i + 1) e' sp rdepth mcc
    else if Int.equal fc Newline.char_lf then
      (rep_min_anynl [@tailcall]) mb (i + 1) e sp rdepth mcc
    else if
      Int.equal fc Newline.char_vt || Int.equal fc Newline.char_ff
      || Int.equal fc Newline.char_nel
    then
      if mb.bsr_anycrlf then (backtrack [@tailcall]) mb sp mcc
      else (rep_min_anynl [@tailcall]) mb (i + 1) e sp rdepth mcc
    else (backtrack [@tailcall]) mb sp mcc

and rep_after_min (mb : mb) (eptr : int) (sp : int) (rdepth : int) (mcc : int) :
    int =
  if Int.equal mb.rep_lmin mb.rep_lmax then
    (* Lmin == Lmax (EXACT / degenerate): continue in place, no tick. *)
    (run [@tailcall]) mb mb.rep_cont eptr sp rdepth mcc
  else if Int.equal mb.rep_reptype Ir.reptype_min then (
    (* Minimize: RMATCH the continuation (RM25/RM27/RM23/RM33) — tick + push
       REP_MIN, run at rdepth+1. *)
    let mcc' = tick_child mb mcc (rdepth + 1) in
    if mcc' < 0 then mcc'
    else (
      let ss = mb.ss in
      let need = sp + Save_stack.width_rep_min in
      if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
      let d = ss.Save_stack.data in
      Array.unsafe_set d sp mb.rep_pc;
      Array.unsafe_set d (sp + 1) mb.rep_lmin (* count = matched so far *);
      Array.unsafe_set d (sp + 2) eptr;
      Array.unsafe_set d (sp + 3) rdepth;
      Array.unsafe_set d (sp + 4) Save_stack.kind_rep_min;
      (run [@tailcall]) mb mb.rep_cont eptr need (rdepth + 1) mcc'))
  else (
    (* Maximize or possessive: greedy scan from Lmin. floor = Lstart_eptr.
       Dispatch to the per-kind greedy scan. *)
    mb.rep_floor <- eptr;
    let k = mb.rep_kind in
    if Int.equal k rk_allany then
      (rep_greedy_allany [@tailcall]) mb eptr sp rdepth mcc
    else if Int.equal k rk_any then
      (rep_greedy_any [@tailcall]) mb mb.rep_lmin eptr sp rdepth mcc
    else if Int.equal k rk_anynl then
      (rep_greedy_anynl [@tailcall]) mb mb.rep_lmin eptr sp rdepth mcc
    else (rep_greedy [@tailcall]) mb mb.rep_lmin eptr sp rdepth mcc)

and rep_greedy (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  (* Greedy scan up to Lmax (repeatchar_ci_maxscan interpreter.ml:3766-3791 /
     class_maxscan 4402 / typemax_ctype 5480): stop at Lmax, at a mismatch, or
     at end_subject (SCHECK_PARTIAL). char / ctype / class / hspace / vspace. *)
  if i >= mb.rep_lmax then (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else if not (rep_unit_matches mb eptr) then
    (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else (rep_greedy [@tailcall]) mb (i + 1) (eptr + 1) sp rdepth mcc

(* OP_ALLANY / OP_ANYBYTE greedy (pcre2_match.c:4748-4757): grab up to
   (Lmax - Lmin) more code units, or to end_subject (with the SCHECK). *)
and rep_greedy_allany (mb : mb) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  let n = mb.rep_lmax - mb.rep_lmin in
  let avail = mb.end_subject - eptr in
  if n > avail then
    let r = scheck_partial mb mb.end_subject in
    if r < 0 then r
    else (rep_greedy_done_dispatch [@tailcall]) mb mb.end_subject sp rdepth mcc
  else (rep_greedy_done_dispatch [@tailcall]) mb (eptr + n) sp rdepth mcc

(* OP_ANY greedy (pcre2_match.c:4726-4746): stop at a newline; a CR at the very
   end could be a partial CRLF. *)
and rep_greedy_any (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if i >= mb.rep_lmax then (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else if is_newline_at mb eptr then
    (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else
    let hit =
      mb.partial <> 0
      && eptr + 1 >= mb.end_subject
      && Int.equal mb.nltype Newline.nltype_fixed
      && Int.equal mb.nllen 2
      && Int.equal (Char.code (String.unsafe_get mb.subject eptr)) mb.nl0
    in
    if hit then mb.hitend <- true;
    if hit && mb.partial > 1 then Errors.error_partial
    else (rep_greedy_any [@tailcall]) mb (i + 1) (eptr + 1) sp rdepth mcc

(* OP_ANYNL greedy (pcre2_match.c:4759-4784): CR (absorb LF; a lone CR at end
   breaks after being consumed), LF, or VT/FF/NEL outside ANYCRLF. *)
and rep_greedy_anynl (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if i >= mb.rep_lmax then (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else
    (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
    let fc = Char.code (String.unsafe_get mb.subject eptr) in
    if Int.equal fc Newline.char_cr then
      let e = eptr + 1 in
      if e >= mb.end_subject then
        (rep_greedy_done_dispatch [@tailcall]) mb e sp rdepth mcc
      else
        let e' =
          if Int.equal (Char.code (String.unsafe_get mb.subject e)) Newline.char_lf
          then e + 1
          else e
        in
        (rep_greedy_anynl [@tailcall]) mb (i + 1) e' sp rdepth mcc
    else if
      (not (Int.equal fc Newline.char_lf))
      && (mb.bsr_anycrlf
         || (not (Int.equal fc Newline.char_vt))
            && (not (Int.equal fc Newline.char_ff))
            && not (Int.equal fc Newline.char_nel))
    then (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
    else (rep_greedy_anynl [@tailcall]) mb (i + 1) (eptr + 1) sp rdepth mcc

(* After a greedy scan to [pmax]: a CLASS repeat backtracks with the floor
   position itself a ticked child (pcre2_match.c:2143 `>=`); every other kind
   tries the floor in place. *)
and rep_greedy_done_dispatch (mb : mb) (pmax : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if Int.equal mb.rep_kind rk_class then
    (rep_greedy_done_class [@tailcall]) mb pmax sp rdepth mcc
  else (rep_greedy_done [@tailcall]) mb pmax sp rdepth mcc

and rep_greedy_done (mb : mb) (pmax : int) (sp : int) (rdepth : int) (mcc : int)
    : int =
  if Int.equal mb.rep_reptype Ir.reptype_pos then
    (* Possessive: no backing up, continue in place (maxend break, no tick). *)
    (run [@tailcall]) mb mb.rep_cont pmax sp rdepth mcc
  else if Int.equal pmax mb.rep_floor then
    (* Maximize matched exactly Lmin: in place at rdepth, no tick (maxbt
       eptr == Lstart_eptr -> dispatch, interpreter.ml:3807-3808 / 5522). *)
    (run [@tailcall]) mb mb.rep_cont pmax sp rdepth mcc
  else
    (* Maximize with extra chars: try the greedy end first (RMATCH -> tick,
       rdepth+1), push REP_MAX to give back down to floor. The first stored
       give-back position carries the \R mid-CRLF correction. *)
    let mcc' = tick_child mb mcc (rdepth + 1) in
    if mcc' < 0 then mcc'
    else (
      let ss = mb.ss in
      let need = sp + Save_stack.width_rep_max in
      if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
      let d = ss.Save_stack.data in
      Array.unsafe_set d sp mb.rep_pc;
      Array.unsafe_set d (sp + 1) (giveback_pos mb mb.rep_pc mb.rep_floor (pmax - 1));
      Array.unsafe_set d (sp + 2) mb.rep_floor;
      Array.unsafe_set d (sp + 3) rdepth;
      Array.unsafe_set d (sp + 4) Save_stack.kind_rep_max;
      (run [@tailcall]) mb mb.rep_cont pmax need (rdepth + 1) mcc')

(* CLASS maximize (pcre2_match.c:2143-2150): the floor position is itself a
   ticked RMATCH (`while Feptr >= Lstart`), so ALWAYS push (even pmax == floor)
   and give back down to and including floor; below floor -> NOMATCH. *)
and rep_greedy_done_class (mb : mb) (pmax : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if Int.equal mb.rep_reptype Ir.reptype_pos then
    (run [@tailcall]) mb mb.rep_cont pmax sp rdepth mcc
  else
    let mcc' = tick_child mb mcc (rdepth + 1) in
    if mcc' < 0 then mcc'
    else (
      let ss = mb.ss in
      let need = sp + Save_stack.width_rep_max in
      if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
      let d = ss.Save_stack.data in
      Array.unsafe_set d sp mb.rep_pc;
      Array.unsafe_set d (sp + 1) (pmax - 1) (* next give-back (class: no CRLF) *);
      Array.unsafe_set d (sp + 2) mb.rep_floor;
      Array.unsafe_set d (sp + 3) rdepth;
      Array.unsafe_set d (sp + 4) Save_stack.kind_rep_max;
      (run [@tailcall]) mb mb.rep_cont pmax need (rdepth + 1) mcc')

(* Single character type (fast-design.md §2; the non-UTF single-type arms
   pcre2_match.c:947-2470). OP_ANY (newline-sensitive) and OP_ANYNL (variable
   length) are special; every other supported type is one code unit tested by
   [simple_type_match]. *)
and op_single_type (mb : mb) (pc : int) (type_op : int) (eptr : int) (sp : int)
    (rdepth : int) (mcc : int) : int =
  if Int.equal type_op Opcodes.op_any then (
    (* OP_ANY (pcre2_match.c:947-958 + op_allany tail 962-973). *)
    if is_newline_at mb eptr then (backtrack [@tailcall]) mb sp mcc
    else if
      mb.partial <> 0
      && Int.equal eptr (mb.end_subject - 1)
      && Int.equal mb.nltype Newline.nltype_fixed
      && Int.equal mb.nllen 2
      (* safe: eptr = end_subject - 1 < String.length subject. *)
      && Int.equal (Char.code (String.unsafe_get mb.subject eptr)) mb.nl0
    then (
      mb.hitend <- true;
      if mb.partial > 1 then Errors.error_partial
      else (run [@tailcall]) mb (pc + 2) (eptr + 1) sp rdepth mcc)
    else if eptr >= mb.end_subject then (
      let r = scheck_partial mb eptr in
      if r < 0 then r else (backtrack [@tailcall]) mb sp mcc)
    else (run [@tailcall]) mb (pc + 2) (eptr + 1) sp rdepth mcc)
  else if Int.equal type_op Opcodes.op_anynl then (
    (* OP_ANYNL \R (pcre2_match.c:2377-2409). *)
    if eptr >= mb.end_subject then (
      let r = scheck_partial mb eptr in
      if r < 0 then r else (backtrack [@tailcall]) mb sp mcc)
    else
      (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
      let fc = Char.code (String.unsafe_get mb.subject eptr) in
      let e = eptr + 1 in
      if Int.equal fc Newline.char_cr then
        if e >= mb.end_subject then (
          (* a lone CR at the end: could be a partial CRLF. *)
          let r = scheck_partial mb e in
          if r < 0 then r else (run [@tailcall]) mb (pc + 2) e sp rdepth mcc)
        else
          let e' =
            if
              Int.equal (Char.code (String.unsafe_get mb.subject e)) Newline.char_lf
            then e + 1
            else e
          in
          (run [@tailcall]) mb (pc + 2) e' sp rdepth mcc
      else if Int.equal fc Newline.char_lf then
        (run [@tailcall]) mb (pc + 2) e sp rdepth mcc
      else if
        Int.equal fc Newline.char_vt || Int.equal fc Newline.char_ff
        || Int.equal fc Newline.char_nel
      then
        if mb.bsr_anycrlf then (backtrack [@tailcall]) mb sp mcc
        else (run [@tailcall]) mb (pc + 2) e sp rdepth mcc
      else (backtrack [@tailcall]) mb sp mcc)
  else if eptr >= mb.end_subject then (
    (* the simple 1-unit predicate types (\d \D \s \S \w \W / \h \H \v \V /
       OP_ALLANY / OP_ANYBYTE): SCHECK_PARTIAL at/past end, then NOMATCH. *)
    let r = scheck_partial mb eptr in
    if r < 0 then r else (backtrack [@tailcall]) mb sp mcc)
  else
    (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
    let cc = Char.code (String.unsafe_get mb.subject eptr) in
    if simple_type_match type_op cc then
      (run [@tailcall]) mb (pc + 2) (eptr + 1) sp rdepth mcc
    else (backtrack [@tailcall]) mb sp mcc

(* WORDBOUND \b / \B, non-UCP (pcre2_match.c:6258-6333 /
   interpreter.ml:3181-3332). [want] = 1 for \b (OP_WORD_BOUNDARY: a boundary
   is required, so NOMATCH when cur == prev), 0 for \B (the reverse). The
   previous-char read lowers [start_used_ptr] (the SCHECK_PARTIAL floor)
   exactly as the C's earliest-consulted tracking (pcre2_match.c:6280); the
   latest-consulted (last_used_ptr) only feeds rightchar/allusedtext, which the
   fast seam does not carry, so it is not tracked. *)
and op_wordbound (mb : mb) (pc : int) (want : int) (eptr : int) (sp : int)
    (rdepth : int) (mcc : int) : int =
  let prev_is_word =
    if Int.equal eptr mb.start_subject then false
    else
      let lastptr = eptr - 1 in
      (* safe: 0 <= lastptr < eptr <= end_subject <= String.length subject. *)
      let fc = Char.code (String.unsafe_get mb.subject lastptr) in
      if lastptr < mb.start_used_ptr then mb.start_used_ptr <- lastptr;
      not (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0)
  in
  if eptr >= mb.end_subject then (
    let r = scheck_partial mb eptr in
    if r < 0 then r
    else (word_bound_test [@tailcall]) mb pc want prev_is_word false eptr sp rdepth mcc)
  else
    (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
    let fc = Char.code (String.unsafe_get mb.subject eptr) in
    let cur_is_word =
      not (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0)
    in
    (word_bound_test [@tailcall]) mb pc want prev_is_word cur_is_word eptr sp
      rdepth mcc

and word_bound_test (mb : mb) (pc : int) (want : int) (prev_is_word : bool)
    (cur_is_word : bool) (eptr : int) (sp : int) (rdepth : int) (mcc : int) : int
    =
  (* pcre2_match.c:6328-6332 — \b: NOMATCH when cur == prev; \B: NOMATCH when
     cur != prev. *)
  let fail =
    if Int.equal want 1 then Bool.equal cur_is_word prev_is_word
    else not (Bool.equal cur_is_word prev_is_word)
  in
  if fail then (backtrack [@tailcall]) mb sp mcc
  else (run [@tailcall]) mb (pc + 2) eptr sp rdepth mcc

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
    (* pcre2_match.c reset_ovector (JIT :14382) / the frame-0 UNSET fill: each
       attempt starts with all group slots UNSET (the whole-match [0,1] is set
       at END). Group slots only: [0,1] are overwritten at END. *)
    for i = 2 to (2 * mb.oveccount) - 1 do
      Array.unsafe_set mb.ovector i Frames.unset
    done;
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
   record per exec (the loop itself allocates nothing).

   For a successful match [orc] is the pcre2 pair count (> 0) and [ovec] the
   2*orc-int ovector (whole match + captures, exactly Engine.exec_full's
   Match.ovector); for a partial [orc] = -2 and [ostart]/[oend] the partial
   bounds; NOMATCH / errors carry only [orc]. *)
type outcome = { orc : int; ostart : int; oend : int; ovec : int array }

let entry_error (rc : int) : outcome =
  { orc = rc; ostart = 0; oend = 0; ovec = [||] }

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
        (* Chunk E: the shared compiler's bytecode (class bitmaps live here) and
           the \R newline convention. *)
        mb.bytecode <- re.Compile.code;
        mb.bsr_anycrlf <-
          Int.equal re.Compile.bsr_convention Options.bsr_anycrlf;
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
        mb.alt_circumflex <-
          not (Int.equal (re.Compile.overall_options land Options.alt_circumflex) 0);
        (* Capture ovector: 2*(top_bracket+1) ints, reused across execs (grown
           if this pattern needs more). Group slots are reset per attempt in
           [run_attempt]; [rc] is set at END. *)
        let oveccount = re.Compile.top_bracket + 1 in
        mb.oveccount <- oveccount;
        if Array.length mb.ovector < 2 * oveccount then
          mb.ovector <- Array.make (2 * oveccount) Frames.unset;
        (* Repeated-group iteration-start scratch (chunk D2): one int per
           empty-check-tracked group; reused/grown across execs. Contents are
           written by t_group_start before any read, so no reset is needed. *)
        if Array.length mb.group_start < ir.Ir.n_groups then
          mb.group_start <- Array.make ir.Ir.n_groups 0;
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
        (* Copy results out of the reused [mb] before releasing the scratch.
           A match returns the pcre2 pair count (mb.rc > 0) and its 2*rc-int
           ovector; every other outcome carries only the whole-match bounds
           (meaningful for a partial). *)
        let ostart = mb.match_start and oend = mb.match_end in
        let orc, ovec =
          if Int.equal rc match_match then (mb.rc, Array.sub mb.ovector 0 (2 * mb.rc))
          else (rc, [||])
        in
        if holds then Save_stack.release ();
        { orc; ostart; oend; ovec })

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
  assert (Int.equal Ir.t_circm 13);
  assert (Int.equal Ir.t_dollm 14);
  assert (Int.equal Ir.t_fail 15);
  assert (Int.equal Ir.t_cap_start 16);
  assert (Int.equal Ir.t_cap_end 17);
  assert (Int.equal Ir.t_rep 18);
  assert (Int.equal Ir.t_repi 19);
  assert (Int.equal Ir.t_notrep 20);
  assert (Int.equal Ir.t_notrepi 21);
  assert (Int.equal Ir.t_group_start 22);
  assert (Int.equal Ir.t_brazero 23);
  assert (Int.equal Ir.t_braminzero 24);
  assert (Int.equal Ir.t_ket_rmax 25);
  assert (Int.equal Ir.t_ket_rmin 26);
  assert (Int.equal Ir.t_type 27);
  assert (Int.equal Ir.t_class 28);
  assert (Int.equal Ir.t_wordbound 29);
  assert (Int.equal Ir.t_type_rep 30);
  assert (Int.equal Ir.t_class_rep 31);
  assert (Int.equal Ir.max_tag 31);
  (* Save-record KIND / width constants the runner inlines as literals. *)
  assert (Int.equal Save_stack.kind_alt 0);
  assert (Int.equal Save_stack.kind_cap 1);
  assert (Int.equal Save_stack.kind_rep_max 2);
  assert (Int.equal Save_stack.kind_rep_min 3);
  assert (Int.equal Save_stack.kind_cont 4);
  assert (Int.equal Save_stack.kind_gstart 5)
