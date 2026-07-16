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
module Utf = Pcre2_engine.Utf
module Ucd = Pcre2_engine.Ucd
module Xclass = Pcre2_engine.Xclass
module Valid_utf = Pcre2_engine.Valid_utf
module Extuni = Pcre2_engine.Extuni
module Tables = Pcre2_engine.Tables
module Script_run = Pcre2_engine.Script_run

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
let rk_xclass = 8 (* OP_XCLASS repeat (chunk I; rep_map_off holds data_off) *)
let rk_prop = 9 (* OP_PROP/OP_NOTPROP repeat (chunk I2; rep_ptype/rep_pdata/rep_want) *)
let rk_extuni = 10 (* OP_EXTUNI repeat (chunk I2; cluster-wise loops) *)

(* OP_ANYBYTE repeat (chunk I2). Split from rk_allany: in UTF the two differ —
   \C's min loop is a bulk bound check WITHOUT SCHECK_PARTIAL
   (pcre2_match.c:3041-3044, unlike OP_ALLANY's per-char SCHECK loop at
   3030-3039), its greedy scan is the byte-bulk form (4834-4845) while
   OP_ALLANY steps characters (4820-4832), and both give back char-wise
   (RM202 BACKCHAR). Non-UTF \C compiles to OP_ALLANY, so rk_anybyte is
   UTF-only in practice. *)
let rk_anybyte = 11

(* pcre2_match.c:87-88 — the two internal match() return codes used inside
   the loop (interpreter.ml:88-89). MATCH_MATCH = 1, MATCH_NOMATCH = 0. *)
let match_match = 1
let match_nomatch = 0

(* Internal signal returned by [char_run_cmp] for "plain NOMATCH, backtrack",
   distinct from every real PCRE2 error code (which are in [-63, -1]) and
   from any non-negative eptr. *)
let sig_backtrack = min_int

(* pcre2_match.c:90-101 — the special internal returns for the backtracking
   verbs (interpreter.ml:95-102). The five MATCH_COMMIT..MATCH_THEN stay in
   sequence and distinct from MATCH_NOMATCH (0) / MATCH_MATCH (1) and from every
   external error code. A verb fires one of these into [backtrack_code], which
   propagates it up the save stack; the driver's rc switch (run_attempt) acts on
   whatever reaches the top. *)
let match_commit = -997
let match_prune = -996
let match_skip = -995
let match_skip_arg = -994
let match_then = -993

(* KIND_VERB [vtype] discriminators (fast-design.md §3). vt_mark reverts the mark
   and catches a name-matching SKIP_ARG; the rest fire their verb code on a
   NOMATCH backtrack. _ARG mark-setting is NOT a separate vtype: the record
   always saves the enclosing mark (old_mark) and restores it, and the forward
   arm sets mb.mark only for the _ARG form (a plain verb's old_mark = the current
   mark, so the restore is a no-op). *)
let vt_mark = 0
let vt_prune = 1
let vt_commit = 2
let vt_skip = 3
let vt_skip_arg = 4
let vt_then = 5

(* pcre2_match.c:2493-2610 — the shared Unicode property test (chunk I2):
   Char_predicates.prop_test is the pure extraction the interpreter also
   aliases, so the two engines share one definition (full citations there). *)
let prop_test = Char_predicates.prop_test

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
  mutable end_subject : int;
      (* usable end: = [true_end_subject] except while matching a non-final
         invalid-UTF fragment (chunk I2), where it stops before the bad code
         unit (pcre2_match.c:6898/7693). *)
  mutable true_end_subject : int;
      (* the actual subject end (pcre2_match.c:6608 / interpreter.ml:362);
         read by OP_EOD (\z, pcre2_match.c:6156) and the fragment carry-on. *)
  mutable start_subject : int; (* always 0 in this port *)
  mutable start_offset : int;
  mutable attempt_start : int; (* start_match of the current attempt (\K absent) *)
  mutable start_used_ptr : int; (* = attempt_start; scheck_partial floor *)
  (* Chunk I — mb->check_subject (pcre2_match.c:6795/6851): the earliest
     position a lookbehind / \b previous-char probe may consult. Non-UTF it is
     the subject start (0); in UTF (with the validity check) it is the start
     offset backed up by max_lookbehind CHARACTERS. Read by [op_wordbound]. *)
  mutable check_subject : int;
  (* Chunk I — UTF-8 mode (re.overall_options & PCRE2_UTF). Constant per exec.
     When set, character reads decode multi-byte UTF-8 and step over whole
     characters; the per-exec UTF validity check ran at the [exec] seam. *)
  mutable utf : bool;
  (* Chunk I2 — UCP mode (re.overall_options & PCRE2_UCP). Constant per exec.
     Selects the Unicode caseless folds/property paths even without UTF:
     CHARI's othercase arm (pcre2_match.c:1075-1092) and match_ref's uni-mode
     caseless compare (pcre2_match.c:388-434). *)
  mutable ucp : bool;
  (* Chunk I2 — match_data->startchar as the C maintains it for ERROR returns:
     0 per exec (pcre2_match.c:6687), the absolute UTF error offset from the
     entry check (6897), or the fragment-relative offset from the carry-on
     validation (7675-7676). Read by [exec]'s outcome for a non-match,
     non-partial negative rc (engine.ml exec_full reads md.startchar the same
     way). *)
  mutable startchar : int;
  (* Chunk I2 — the caller's NOTBOL/NOTEOL option flags, kept apart from the
     effective [notbol]/[noteol] because the invalid-UTF fragment driver ORs
     per-fragment NOTBOL/NOTEOL into the match options (mb->moptions = options
     | fragment_options, pcre2_match.c:7502; fragment_options set at
     6920-6926/7683/7694). [set_frag_options] recomputes the effective flags
     at each fragment restart. *)
  mutable base_notbol : bool;
  mutable base_noteol : bool;
  (* option-derived flags (moptions = options; constant per exec) *)
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
  mutable rep_ci : bool; (* caseless (t_repi/t_notrepi) — the C's Fop >= OP_STARI
                            loop selection, which decides the mismatch-Feptr
                            semantics for last_used_ptr (chunk K1b) *)
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
  (* Chunk I2 — property-repeat payload (rk_prop): the OP_PROP/OP_NOTPROP
     ptype/pdata pair; the NOT sense rides in [rep_want]. *)
  mutable rep_ptype : int;
  mutable rep_pdata : int;
  (* Chunk E: the shared compiler's bytecode ([re.code]), pinned for the class
     bitmap reads (t_class / t_class_rep); the 32-byte map stays in place. *)
  mutable bytecode : Bytes.t;
  mutable bsr_anycrlf : bool; (* bsr_convention = BSR_ANYCRLF (\R) *)
  (* Chunk F — backreferences. [match_unset_backref] is the compile option
     PCRE2_MATCH_UNSET_BACKREF (mb.poptions & ..., interpreter.ml:913/6156):
     an unset reference matches empty rather than failing. [name_table] /
     [name_entry_size] back the OP_DNREF group-list scan (dnref_scan). *)
  mutable match_unset_backref : bool;
  mutable name_table : Bytes.t;
  mutable name_entry_size : int;
  (* [ref_length] is match_ref's out-parameter (the C's *lengthptr): the number
     of subject code units the last successful reference match consumed (= the
     reference length, non-UTF). Written by [match_ref], read at each ref site.
     [cap_start] holds the in-progress start OFFSET of each referenced capture
     group (the JIT's private_data slot for a non-optimized cbracket), indexed
     by ovbase; written by t_cap_start_ref, read by t_cap_end_ref, saved/
     restored by KIND_CAPSTART. Sized 2*oveccount, reused/grown across execs;
     written before any read (like group_start), so no per-attempt reset. *)
  mutable ref_length : int;
  mutable cap_start : int array;
  (* Ref-repeat scratch (t_ref_rep / t_dnref_rep): the invariant parameters of
     the ref repeat currently being run, decoded by [setup_ref_rep] on the
     forward path and re-derived on the RM20 backtrack (a nested repeat may
     clobber them). *)
  mutable ref_pc : int; (* the ref-repeat instruction head (RM20 re-derive) *)
  mutable ref_ovbase : int;
  mutable ref_caseless : bool;
  mutable ref_reptype : int;
  mutable ref_lmin : int;
  mutable ref_lmax : int;
  mutable ref_cont : int; (* continuation pc past the ref-repeat instruction *)
  (* Chunk G — lookaround / atomic groups. [once_base] is the save-stack base
     (sp) of the innermost currently-open atomic construct (OP_ONCE / atomic
     assertion / negative assertion), or -1 if none; it lets the atomic COMMIT
     (t_once_end / t_assert_end / t_nassert_match) find the boundary record
     without a per-loop parameter (fast-design.md §3). Maintained as a stack via
     the boundary records: each pushes the ENCLOSING [once_base] and restores it
     on commit or backtrack-past. *)
  mutable once_base : int;
  (* Chunk H — backtracking control verbs (fast-design.md §3). [mark] is the
     current path's most-recent MARK, a byte OFFSET into [bytecode] (the C's
     per-frame Fmark, pcre2_match.c:6341): set by MARK / _ARG verbs forward,
     restored by KIND_VERB on backtrack-past, reset to UNSET per attempt; read
     at a successful END. [nomatch_mark] is the sticky failure mark (mb's
     nomatch_mark, pcre2_match.c:6341): set forward by MARK / _ARG verbs, NEVER
     restored, reset per EXEC, returned on any non-match. [alt_then_end] is
     Ir.alt_then_end (the THEN scope boundary per ALT pc, copied into the
     KIND_ALT record at push time). *)
  mutable mark : int;
  mutable nomatch_mark : int;
  mutable alt_then_end : int array;
  mutable once_subtype : int array; (* Ir.once_subtype: KIND_ONCE boundary kind *)
  (* [verb_skip_ptr] passes back a SKIP's target position / a SKIP_ARG's name
     offset (the C's mb.verb_skip_ptr); [verb_then_pc] the firing THEN's IR pc
     (the C's mb.verb_ecode_ptr, as an IR index) used by the KIND_ALT scope
     check. [skip_arg_count] counts executed SKIP_ARGs (reset per attempt);
     [ignore_skip_arg] is the rerun threshold (SKIP_ARGs up to it are no-ops),
     set when a MATCH_SKIP_ARG reaches the driver top (pcre2_match.c:6408/7538). *)
  mutable verb_skip_ptr : int;
  mutable verb_then_pc : int;
  mutable skip_arg_count : int;
  mutable ignore_skip_arg : int;
  (* Chunk K1b — pattern recursion (fast-design.md §3). [has_recurse] gates all
     last_used_ptr tracking (only recursive patterns read it, via the RECURSELOOP
     check). [last_used_ptr] is the C's mb.last_used_ptr (pcre2_match.c:6470): the
     rightmost subject position any completed sub-match reached, a monotone max
     reset to start_match per attempt — tracked at every forward-to-backtrack
     transition (a frame return), record_match (OP_END), the positive-assertion
     kets and the word-boundary next-char probe. [current_recurse] is Fcurrent_
     recurse (the group number of the recursion we are inside, or -1 =
     RECURSE_UNSET); [recurse_base] chains the open KIND_RECURSE records for the
     RECURSELOOP walk (pcre2_match.c:5438-5453); [verb_current_recurse] is
     mb->verb_current_recurse (the recursion a firing backtracking verb was in,
     pcre2_match.c:6369). [n_groups] mirrors Ir.n_groups (the group_start size),
     needed to size the FAT record. *)
  mutable has_recurse : bool;
  mutable last_used_ptr : int;
  mutable current_recurse : int;
  mutable recurse_base : int;
  mutable verb_current_recurse : int;
  mutable n_groups : int;
  (* PCRE2_DISABLE_RECURSELOOP_CHECK (a match option, Options
     :87): when set the RECURSELOOP -52 check is suppressed and an infinite
     recursion is instead caught by the match/heap limits (pcre2_match.c:5448). *)
  mutable disable_recurseloop : bool;
  (* the backtracking save stack *)
  ss : Save_stack.t;
}

(* Chunk K1b — RECURSE_UNSET sentinel (the C's 0xffffffff): -1, distinct from
   every real group number (>= 0), so [current_recurse >= 0] means "inside a
   recursion" and the group-0 whole-pattern recursion (number 0) is included. *)
let recurse_unset = -1

let make_mb (ss : Save_stack.t) : mb =
  {
    code = [||];
    lit = "";
    subject = "";
    end_subject = 0;
    true_end_subject = 0;
    start_subject = 0;
    start_offset = 0;
    attempt_start = 0;
    start_used_ptr = 0;
    check_subject = 0;
    utf = false;
    ucp = false;
    startchar = 0;
    base_notbol = false;
    base_noteol = false;
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
    rep_ci = false;
    rep_cont = 0;
    rep_floor = 0;
    rep_kind = 0;
    rep_mask = 0;
    rep_map_off = 0;
    rep_ptype = 0;
    rep_pdata = 0;
    bytecode = Bytes.empty;
    bsr_anycrlf = false;
    match_unset_backref = false;
    name_table = Bytes.empty;
    name_entry_size = 0;
    ref_length = 0;
    cap_start = [||];
    ref_pc = 0;
    ref_ovbase = 0;
    ref_caseless = false;
    ref_reptype = 0;
    ref_lmin = 0;
    ref_lmax = 0;
    ref_cont = 0;
    once_base = -1;
    mark = Frames.unset;
    nomatch_mark = Frames.unset;
    alt_then_end = [||];
    once_subtype = [||];
    verb_skip_ptr = -1;
    verb_then_pc = 0;
    skip_arg_count = 0;
    ignore_skip_arg = 0;
    has_recurse = false;
    last_used_ptr = 0;
    current_recurse = -1;
    recurse_base = -1;
    verb_current_recurse = -1;
    n_groups = 0;
    disable_recurseloop = false;
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
      Newline.is_newline mb.subject mb.nltype p mb.end_subject mb.nl_scratch mb.utf
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
        mb.utf
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

(* ---------- Chunk I: UTF-8 character reads ---------- *)

(* Decode the code point of the character at [eptr] WITHOUT advancing
   (GETCHAR, pcre2_intmodedep.h:298-303). In non-UTF (or an ASCII lead byte)
   this is the single code unit. Utf.getutf8 reads continuation bytes through
   Utf.peek, so even a truncated tail is defined (no exception). safe: caller
   has established eptr < end_subject <= String.length mb.subject. *)
let cur_cp (mb : mb) (eptr : int) : int =
  let c0 = Char.code (String.unsafe_get mb.subject eptr) in
  if mb.utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0

(* Byte length of the character at [eptr] (GET_EXTRALEN + 1; 1 unless UTF
   multi-byte). safe: as [cur_cp]. *)
let cp_len (mb : mb) (eptr : int) : int =
  let c0 = Char.code (String.unsafe_get mb.subject eptr) in
  if mb.utf && c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1

(* pcre2_match.c:962-973 / 3024-3025 / 3038 / 4495-4496 / 4510 — the FORWARD
   advance of OP_ANY / OP_ALLANY in UTF: Feptr++; then ACROSSCHAR(Feptr <
   end_subject, Feptr, Feptr++) (pcre2_intmodedep.h:352-353). The interpreter
   writes this as [Utf.forwardchartest mb.subject (eptr + 1) mb.end_subject]
   (interpreter.ml op_allany_tail:4227-4229, typemin_utf_any/allany:5443-5468,
   typemax_utf_any/allany:5766-5792). It differs from [cp_len] (GETCHARLEN,
   which GETCHARINC uses in the char/type MINIMIZE resume, and which the
   per-char predicate types use): step ONE code unit, THEN skip any trailing
   continuation bytes. At a mid-character position (a continuation byte,
   reachable only after \C splits a UTF character) cp_len returns 1 whereas this
   consumes the rest of the interrupted character. Non-UTF: a plain step of one
   code unit. safe: caller proves eptr < end_subject <= String.length subject.

   Allocation-free: the continuation-byte skip is a module-level tail-recursive
   int loop (NOT a ref/while as in [across_char], which is a per-ATTEMPT
   bump-along helper; this is the per-CHARACTER `.` forward path, so a ref box
   would allocate once per char of a UTF `.*`/`.+`, §8 / the §9 give-back
   hazard's cousin). Mirrors the allocation-free [backchar_sub]. *)
let rec skip_cont_bytes (s : string) (endp : int) (q : int) : int =
  if
    q < endp
    (* safe: q < endp <= String.length s. *)
    && Int.equal (Char.code (String.unsafe_get s q) land 0xc0) 0x80
  then (skip_cont_bytes [@tailcall]) s endp (q + 1)
  else q

let any_advance (mb : mb) (eptr : int) : int =
  let p = eptr + 1 in
  if not mb.utf then p else skip_cont_bytes mb.subject mb.end_subject p

(* pcre2_intmodedep.h:341-345 — BACKCHAR(eptr) over the subject: if [p] is not
   at the start of a character, move it back until it is. Allocation-free
   (tail-recursive int loop — NOT Utf.backchar, whose int-ref would allocate on
   every frame-loop call, §8); clamped at 0 exactly like the interpreter's
   backchar_subject (its DEVIATION note: on valid UTF — every path except
   PCRE2_NO_UTF_CHECK garbage — the walk stops at the lead byte before reaching
   0 anyway). safe: caller establishes 0 <= p < String.length s; the walk only
   decreases p while p > 0. *)
let rec backchar_sub (s : string) (p : int) : int =
  if p > 0 && Int.equal (Char.code (String.unsafe_get s p) land 0xc0) 0x80 then
    (backchar_sub [@tailcall]) s (p - 1)
  else p

(* GETCHAR (pcre2_intmodedep.h:298-303) over the subject with the lead byte
   read through Utf.peek (interpreter.ml getchar_subject): tolerates a pinned
   out-of-range position (peek reads 0 there — the word-boundary prev-probe /
   grapheme back-walk precedent). Allocation-free. *)
let getchar_sub (s : string) (pos : int) : int =
  let c = Utf.peek s pos in
  if c >= 0xc0 then Utf.getutf8 c s pos else c

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
    (* DEVIATION (defined behavior where the C is undefined): this per-BYTE
       check fires SCHECK_PARTIAL whenever the run is exhausted AT the subject
       end, whereas the C's UTF OP_CHAR arm treats a multi-byte pattern char as
       a unit — `if (Flength > end_subject - Feptr) { CHECK_PARTIAL(); ... }`
       (pcre2_match.c:1002-1004), where CHECK_PARTIAL (:531-535) fires only
       when Feptr is AT/PAST end_subject, so a subject ending MID-character
       (some bytes matched, eptr still < end at the char's start) is a plain
       NOMATCH there. The divergence needs a subject truncated mid-character,
       which per-exec validation rejects — it is reachable only under
       PCRE2_NO_UTF_CHECK with invalid UTF, PCRE2's documented undefined
       behavior. On every validated path the two are byte-identical. *)
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

(* Chunk K1b — the C-exact last_used_ptr record position for a FAILED char run
   (OP_CHAR, pcre2_match.c:995-1025). Called only for has_recurse patterns,
   whose runs are DE-FUSED to ONE character (1..4 code units): a length
   shortage (UTF: the whole char does not fit, the CHECK_PARTIAL branch
   :1002-1006; non-UTF: end of subject :1017-1021) leaves Feptr at [eptr];
   a unit mismatch at unit k RRETURNs AFTER the post-increment
   (`*Fecode++ != UCHAR21INC(Feptr)` :1009 / `Fecode[1] != *Feptr++` :1022),
   recording eptr + k + 1. Cold (failure path only); module-level (§9). *)
let rec char_run_cfail_walk (mb : mb) (lp : int) (e : int) : int =
  (* safe: the caller [char_run_cfail] excluded the length shortage, so a
     mismatching unit exists before either bound: lp < |lit| and
     e < end_subject <= |subject| at every probe. *)
  if
    Int.equal
      (Char.code (String.unsafe_get mb.lit lp))
      (Char.code (String.unsafe_get mb.subject e))
  then (char_run_cfail_walk [@tailcall]) mb (lp + 1) (e + 1)
  else e + 1

let char_run_cfail (mb : mb) (lit_pos : int) (lit_end : int) (eptr : int) : int =
  if lit_end - lit_pos > mb.end_subject - eptr then eptr
  else char_run_cfail_walk mb lit_pos eptr

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

(* Chunk I — match a decoded code point [cp] against the OP_CLASS/OP_NCLASS
   32-byte bitmap at byte offset [map_off] in mb.bytecode. A code point > 255 is
   never in the bitmap: it matches iff the opcode is OP_NCLASS (the byte
   immediately before the bitmap, map_off-1 = the OP_CLASS/OP_NCLASS position),
   exactly interpreter.ml:4463-4468 / pcre2_match.c:2022-2024. cp <= 255 uses
   the bitmap. safe: map_off-1 is the opcode byte (Ir compiler set map_off =
   OP position + 1); map_off + 32 <= |bytecode| (Ir_verify). *)
let class_cp_match (mb : mb) (map_off : int) (cp : int) : bool =
  if cp > 255 then
    Int.equal (Char.code (Bytes.get mb.bytecode (map_off - 1))) Opcodes.op_nclass
  else class_bit_at mb map_off cp

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

(* Chunk I — one DECODED code point [cp] against a single character-type opcode
   in UTF mode (the interpreter's UTF single-type arms, pcre2_match.c:2305-2470).
   \d \D \s \S \w \W guard cp <= 255 (Chartables.ctypes is a 0..255 table, and a
   code point > 255 is never in the ASCII ctype classes without UCP — the C's
   CHMAX_255 guard, e.g. :2312); \h \H \v \V use the code-point predicate (the
   HSPACE/VSPACE multibyte cases); OP_ALLANY / OP_ANYBYTE match any character.
   \R (OP_ANYNL) / OP_ANY are handled by their dedicated arms. *)
let simple_type_match_cp (type_op : int) (cp : int) : bool =
  if Int.equal type_op Opcodes.op_digit then
    cp <= 255 && not (Int.equal (Chartables.ctypes cp land Chartables.ctype_digit) 0)
  else if Int.equal type_op Opcodes.op_not_digit then
    not (cp <= 255 && not (Int.equal (Chartables.ctypes cp land Chartables.ctype_digit) 0))
  else if Int.equal type_op Opcodes.op_whitespace then
    cp <= 255 && not (Int.equal (Chartables.ctypes cp land Chartables.ctype_space) 0)
  else if Int.equal type_op Opcodes.op_not_whitespace then
    not (cp <= 255 && not (Int.equal (Chartables.ctypes cp land Chartables.ctype_space) 0))
  else if Int.equal type_op Opcodes.op_wordchar then
    cp <= 255 && not (Int.equal (Chartables.ctypes cp land Chartables.ctype_word) 0)
  else if Int.equal type_op Opcodes.op_not_wordchar then
    not (cp <= 255 && not (Int.equal (Chartables.ctypes cp land Chartables.ctype_word) 0))
  else if Int.equal type_op Opcodes.op_hspace then Char_predicates.hspace_char cp
  else if Int.equal type_op Opcodes.op_not_hspace then
    not (Char_predicates.hspace_char cp)
  else if Int.equal type_op Opcodes.op_vspace then Char_predicates.vspace_char cp
  else if Int.equal type_op Opcodes.op_not_vspace then
    not (Char_predicates.vspace_char cp)
  else true (* op_allany / op_anybyte *)

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
    mb.rep_ci <- false;
    mb.rep_cont <- pc + 5)
  else if Int.equal tag 19 (* t_repi *) then (
    mb.rep_kind <- rk_char;
    mb.rep_c1 <- code.(pc + 4);
    mb.rep_c2 <- code.(pc + 5);
    mb.rep_want <- true;
    mb.rep_ci <- true;
    mb.rep_cont <- pc + 6)
  else if Int.equal tag 20 (* t_notrep *) then (
    mb.rep_kind <- rk_char;
    mb.rep_c1 <- code.(pc + 4);
    mb.rep_c2 <- code.(pc + 4);
    mb.rep_want <- false;
    mb.rep_ci <- false;
    mb.rep_cont <- pc + 5)
  else if Int.equal tag 21 (* t_notrepi *) then (
    mb.rep_kind <- rk_char;
    mb.rep_c1 <- code.(pc + 4);
    mb.rep_c2 <- code.(pc + 5);
    mb.rep_want <- false;
    mb.rep_ci <- true;
    mb.rep_cont <- pc + 6)
  else if Int.equal tag 31 (* t_class_rep *) then (
    mb.rep_kind <- rk_class;
    mb.rep_map_off <- code.(pc + 4);
    mb.rep_cont <- pc + 5)
  else if Int.equal tag 58 (* t_xclass_rep *) then (
    (* Chunk I — store the XCLASS data offset in rep_map_off (reused as an int
       byte offset); rep_unit_utf matches via Xclass.xclass. *)
    mb.rep_kind <- rk_xclass;
    mb.rep_map_off <- code.(pc + 4);
    mb.rep_cont <- pc + 5)
  else if Int.equal tag 60 (* t_prop_rep *) then (
    (* Chunk I2 — property repeat (pcre2_match.c:2708-2714): rep_want is the
       POSITIVE sense (OP_PROP), false for OP_NOTPROP; ptype/pdata feed
       prop_test per unit. *)
    mb.rep_kind <- rk_prop;
    mb.rep_want <- Int.equal code.(pc + 4) 0;
    mb.rep_ptype <- code.(pc + 5);
    mb.rep_pdata <- code.(pc + 6);
    mb.rep_cont <- pc + 7)
  else if Int.equal tag 62 (* t_extuni_rep *) then (
    (* Chunk I2 — \X repeat: cluster-wise dedicated loops. *)
    mb.rep_kind <- rk_extuni;
    mb.rep_cont <- pc + 4)
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
    else if Int.equal ct Opcodes.op_anybyte then
      (* Chunk I2 — \C: split from rk_allany (UTF min/greedy/give-back
         differences; see the rk_anybyte note above). *)
      mb.rep_kind <- rk_anybyte
    else (* op_allany *) mb.rep_kind <- rk_allany)

(* Chunk K1b — the C-exact last_used_ptr record position for a per-unit repeat
   MISMATCH at [p] (end-of-subject failures record [p]: every C loop checks the
   end BEFORE reading). The C's min and minimize loops leave Feptr differently
   at their RRETURN(MATCH_NOMATCH) per family:
   - rk_char POSITIVE (t_rep/t_repi): the caseless loops test before advancing
     (UCHAR21TEST..Feptr++, pcre2_match.c:1402-1413/1429-1431) -> p; the caseful
     UTF wide-char loops memcmp without advancing (:1296-1308/1320-1331) -> p;
     the caseful 1-unit loops UCHAR21INCTEST -> p+1 (:1465-1472/1489).
   - rk_char NEGATED (t_notrep/t_notrepi): UTF GETCHARINC -> p+len
     (:1640-1648/1686-1687 caseless, :1782-1790/1826-1827 caseful); non-UTF
     caseful *Feptr++ -> p+1 (:1797-1804/1844); non-UTF caseless tests before
     advancing -> p (:1656-1663/1705-1706) EXCEPT lmin=lmax=1, which is the
     SINGLE OP_NOT/OP_NOTI lowering (a {1} quantifier folds to the bare opcode,
     so OP_NOTEXACT(I) 1 never compiles): the single arm reads first
     (UCHAR21INC, :1144/:1169) -> p+1.
   - rk_ctype (\d \s \w families): UTF GETCHARINC -> p+len (:3130-3250 min,
     :3846-3847 minimize); non-UTF MIN loop tests without advancing -> p
     (:3414-3437+), but the non-UTF MINIMIZE loop reads first (fc = *Feptr++,
     :3975) -> p+1.
   - rk_class (:1986/2006/2040/2063), rk_xclass (:2214-2222/2244-2245), rk_prop
     (:2733-2758 min / RM208-RM214 minimize — GETCHARINCTEST in ALL modes),
     rk_hspace/rk_vspace (:3080-3128 UTF, `switch( *Feptr++)` :3334-3412
     non-UTF): all read first -> p + len (p+1 non-UTF).
   rk_any / rk_anynl / rk_allany / rk_anybyte / rk_extuni have dedicated loops
   (their sites pass the exact position directly). Gated on has_recurse (the
   only reader of last_used_ptr); p < end_subject at every mismatch site, so
   the cp_len read is safe. *)
let rep_fail_pos (mb : mb) (p : int) (minimize : bool) : int =
  if not mb.has_recurse then p
  else
    let k = mb.rep_kind in
    if Int.equal k rk_char then
      if mb.rep_want then
        if mb.rep_ci then p
        else if mb.utf && mb.rep_c1 > 127 then p (* wide-char memcmp *)
        else p + 1
      else if mb.utf then p + cp_len mb p
      else if
        mb.rep_ci
        && not (Int.equal mb.rep_lmin 1 && Int.equal mb.rep_lmax 1)
      then p
      else p + 1
    else if Int.equal k rk_ctype then
      if mb.utf then p + cp_len mb p else if minimize then p + 1 else p
    else (* class / xclass / prop / hspace / vspace: read-first *)
      p + cp_len mb p

(* True iff the repeat instruction at [rep_pc] is a \R (OP_ANYNL) type repeat.
   Used only on the (cold) greedy give-back path. *)
let is_anynl_rep (mb : mb) (rep_pc : int) : bool =
  Int.equal mb.code.(rep_pc) 30 (* t_type_rep *)
  && Int.equal mb.code.(rep_pc + 4) Opcodes.op_anynl

(* GETCHARINCTEST(fc, Feptr); Feptr = PRIV(extuni)(fc, Feptr,
   mb->start_subject, mb->end_subject, utf, NULL) — the shared body of the
   four OP_EXTUNI stepping sites (pcre2_match.c:2629-2631 = 2990-2992 =
   3821-3823 = 4415-4417; interpreter.ml extuni_step): one grapheme cluster
   from [eptr], returning the position after it. Allocation-free
   (Extuni.extuni is int-only; its local refs are eliminated by the compiler
   — the interpreter runs it inside ITS frame loop under the same §8 pin).
   Caller contract: eptr < mb.end_subject. *)
let extuni_at (mb : mb) (eptr : int) : int =
  (* safe: eptr < mb.end_subject <= String.length mb.subject (caller
     contract). *)
  let c0 = Char.code (String.unsafe_get mb.subject eptr) in
  let fc = if mb.utf && c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
  let e' =
    if mb.utf && c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0 else eptr + 1
  in
  Extuni.extuni fc mb.subject e' mb.start_subject mb.end_subject mb.utf

(* pcre2_match.c:4437-4465 (the RM220 resume + its inner for(;;);
   interpreter.ml RM220 arm + extuni_bt_inner) — step BACK one grapheme
   cluster from [pos] (> [floor]), bounded below by [floor] (Lstart_eptr):
   move back one character, then keep moving left while the grapheme-break
   pair table forbids a break between the previous character and the current
   one. NOTE (transcribed from the C as-is): this re-walk uses only the pair
   table, not the ZWJ/RI special rules. Returns the give-back position
   (>= floor: [pos] and [floor] are character boundaries, so the walk's
   BACKCHARs cannot cross below floor). *)
(* pcre2_match.c:4452-4464 (interpreter.ml extuni_bt_inner) — the inner
   for(;;) of the cluster back-walk: while the grapheme-break pair table
   forbids a break between subject[fptr] and subject[p], keep stepping left.
   Module-level + fully applied so the give-back path builds NO closure (§8 —
   a local `let rec` here would allocate ~6 words per \X give-back step,
   which the "\X cluster give-back" alloc pin measures). *)
let rec extuni_back_walk (mb : mb) (floor : int) (p : int) (rgb : int) : int =
  if p <= floor then p (* at start of char run *)
  else
    let fptr = p - 1 in
    let fptr = if mb.utf then backchar_sub mb.subject fptr else fptr in
    let fc =
      if mb.utf then getchar_sub mb.subject fptr
        (* safe: 0 <= fptr < p <= String.length mb.subject. *)
      else Char.code (String.unsafe_get mb.subject fptr)
    in
    let lgb = Ucd.gbprop fc in
    if Int.equal (Tables.ucp_gbtable.(lgb) land (1 lsl rgb)) 0 then p
    else (extuni_back_walk [@tailcall]) mb floor fptr lgb

let extuni_back (mb : mb) (floor : int) (pos : int) : int =
  (* pcre2_match.c:4444-4450 — Feptr--; if (!utf) fc = *Feptr else
     { BACKCHAR(Feptr); GETCHAR(fc, Feptr); }; rgb = UCD_GRAPHBREAK(fc).
     pos - 1 >= floor >= 0 (caller guarantees pos > floor); backchar_sub /
     getchar_sub clamp their reads. *)
  let p0 = pos - 1 in
  let p0 = if mb.utf then backchar_sub mb.subject p0 else p0 in
  let fc0 =
    if mb.utf then getchar_sub mb.subject p0
      (* safe: 0 <= p0 < pos <= a previously in-bounds position <=
         String.length mb.subject. *)
    else Char.code (String.unsafe_get mb.subject p0)
  in
  extuni_back_walk mb floor p0 (Ucd.gbprop fc0)

(* True iff the repeat instruction at [rep_pc] is a \X (t_extuni_rep) repeat
   (cold give-back path only, chunk I2). *)
let is_extuni_rep (mb : mb) (rep_pc : int) : bool =
  Int.equal mb.code.(rep_pc) 62 (* t_extuni_rep *)

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

(* Chunk I — the give-back position one unit before a char-boundary [pos] that
   the greedy scan reached (pos > floor): a CHARACTER back in UTF (BACKCHAR via
   the allocation-free [backchar_sub], the C's RM34/RM202/RM101/RM201 BACKCHAR,
   pcre2_match.c:1358/4944/2285/2115) or a code unit back otherwise; both with
   the \R mid-CRLF correction (pcre2_match.c:4945-4947 in UTF, 4963-4967
   non-UTF). A \X repeat (chunk I2) steps back a whole grapheme CLUSTER
   instead ([extuni_back], the RM220 resume). Caller guarantees pos > floor, so
   pos-1 >= floor >= 0. *)
let rep_giveback (mb : mb) (rep_pc : int) (floor : int) (pos : int) : int =
  if pos > floor then
    if is_extuni_rep mb rep_pc then extuni_back mb floor pos
    else if mb.utf then
      giveback_pos mb rep_pc floor (backchar_sub mb.subject (pos - 1))
    else giveback_pos mb rep_pc floor (pos - 1)
  else pos - 1 (* class sentinel: pos = floor, store floor-1 (below floor) *)

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

(* Push a KIND_VERB record [vtype; aux; eptr; old_mark; KIND_VERB] at [sp]
   (chunk H, fast-design.md §3). Cold (verb forward path only). *)
let push_verb (mb : mb) (sp : int) (vtype : int) (aux : int) (eptr : int)
    (old_mark : int) : unit =
  let ss = mb.ss in
  let need = sp + Save_stack.width_verb in
  if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
  let d = ss.Save_stack.data in
  (* safe: [grow] ensured Array.length d >= sp + width_verb. *)
  Array.unsafe_set d sp vtype;
  Array.unsafe_set d (sp + 1) aux;
  Array.unsafe_set d (sp + 2) eptr;
  Array.unsafe_set d (sp + 3) old_mark;
  Array.unsafe_set d (sp + 4) Save_stack.kind_verb

(* pcre2_match.c:919-940 (interpreter.ml op_end_tail :3442-3485) — record a
   successful whole-pattern match (shared by OP_END and OP_ACCEPT): the whole
   match in ovector[0,1] and the pcre2 pair count rc (= highest set group + 1).
   The winning path's group slots are already in place; the result mark is
   mb.mark (read by [exec]). Returns MATCH_MATCH. *)
let record_match (mb : mb) (sm : int) (eptr : int) : int =
  (* pcre2_match.c:929 — OP_END updates last_used_ptr (the RECURSELOOP input);
     gated on has_recurse (only recursive patterns read it). *)
  if mb.has_recurse && eptr > mb.last_used_ptr then mb.last_used_ptr <- eptr;
  let s = sm - mb.start_subject in
  let e = eptr - mb.start_subject in
  mb.match_start <- s;
  mb.match_end <- e;
  mb.ovector.(0) <- s;
  mb.ovector.(1) <- e;
  mb.rc <- rc_of_ovector mb;
  match_match

(* ---------- Atomic-construct boundary records (chunk G, §3) ----------

   The atomic constructs (OP_ONCE, atomic assertions, negative assertions) push
   a boundary record that snapshots the group ovector slots [2, 2*oveccount) and
   the enclosing [mb.once_base], and sets [mb.once_base] to its base so the COMMIT
   (t_once_end / t_assert_end / t_nassert_match) can find it. Backtracking PAST
   the boundary restores the snapshot + [mb.once_base] — reproducing the C frame
   arena's whole-scale restore of everything the construct did (its captures and,
   for atomic groups, the abandoned iterations), which the fast engine's shared
   single ovector otherwise cannot roll back after a COMMIT truncated the body's
   per-capture cleanup records (fast-design.md §3). *)

(* Push a KIND_ONCE boundary at [sp]: snapshot ov[2, 2*oveccount) + the enclosing
   [mb.once_base] + the entry [mb.mark] + the entry [eptr] + the [subtype]; set
   [mb.once_base] = sp. Returns the new sp. Cold (atomic entry only). Layout
   (fast-design.md §3, chunk H; chunk K1b added entry_eptr): [ov_snapshot;
   prev_once_base; saved_mark; entry_eptr; subtype; KIND_ONCE], width
   2*oveccount + 3. [saved_mark] is the C's per-frame Fmark restore on
   backtrack-past (the atomic COMMIT truncates any MARK record inside);
   [entry_eptr] is the C frame's Feptr, recorded into last_used_ptr when a VERB
   code passes this boundary (the RRETURN(rrc) from the construct's frame,
   pcre2_match.c:5408/:5529 — chunk K1b, §4); [subtype] (Ir.once_group /
   once_pos_assert) tells [backtrack_code] whether a ( *THEN) reaching this
   boundary escapes (atomic group) or is contained (positive assertion). *)
let push_once (mb : mb) (sp : int) (eptr : int) (subtype : int) : int =
  let ss = mb.ss in
  let nov = 2 * mb.oveccount in
  let need = sp + nov + 3 in
  if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
  let d = ss.Save_stack.data in
  let ov = mb.ovector in
  (* safe: [grow] ensured length >= sp + nov + 3; ov has 2*oveccount = nov slots. *)
  for i = 2 to nov - 1 do
    Array.unsafe_set d (sp + i - 2) (Array.unsafe_get ov i)
  done;
  Array.unsafe_set d (sp + nov - 2) mb.once_base (* prev_once_base *);
  Array.unsafe_set d (sp + nov - 1) mb.mark (* saved_mark *);
  Array.unsafe_set d (sp + nov) eptr (* entry_eptr *);
  Array.unsafe_set d (sp + nov + 1) subtype;
  Array.unsafe_set d (sp + nov + 2) Save_stack.kind_once;
  mb.once_base <- sp;
  need

(* The atomic COMMIT (t_once_end / t_assert_end with atomic=1): the body matched;
   discard its internal choice points by truncating the save stack back to just
   past the KIND_ONCE boundary at [mb.once_base], and restore [mb.once_base] to
   the enclosing value. mb.mark is KEPT (the mark set inside a matched atomic group
   persists forward, like the C's ket continuing in the body's frame). The
   KIND_ONCE record itself STAYS (its snapshot restores captures/mark if the
   construct is later backtracked past). Returns the truncated sp. *)
let once_commit (mb : mb) : int =
  let base = mb.once_base in
  let nov = 2 * mb.oveccount in
  let d = mb.ss.Save_stack.data in
  (* safe: [base] is a KIND_ONCE record base (set by push_once); base + nov - 2
     is its prev_once_base slot. *)
  mb.once_base <- Array.unsafe_get d (base + nov - 2);
  base + nov + 3

(* Push a KIND_NASSERT boundary at [sp]: snapshot ov[2, 2*oveccount) + the
   enclosing [mb.once_base] + the entry [cont]/[eptr]/[rdepth] + the entry
   [mb.mark]; set [mb.once_base] = sp. Returns the new sp. Layout
   (fast-design.md §3, chunk H): [ov_snapshot; prev_once_base; cont; eptr;
   rdepth; saved_mark; KIND_NASSERT], width 2*oveccount + 4. [saved_mark]
   reverts mb.mark when a matched branch fails the assertion (t_nassert_match
   truncates that branch's MARK records). *)
let push_nassert (mb : mb) (sp : int) (cont : int) (eptr : int) (rdepth : int) :
    int =
  let ss = mb.ss in
  let nov = 2 * mb.oveccount in
  let need = sp + nov + 4 in
  if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
  let d = ss.Save_stack.data in
  let ov = mb.ovector in
  (* safe: [grow] ensured length >= sp + nov + 4. *)
  for i = 2 to nov - 1 do
    Array.unsafe_set d (sp + i - 2) (Array.unsafe_get ov i)
  done;
  Array.unsafe_set d (sp + nov - 2) mb.once_base (* prev_once_base *);
  Array.unsafe_set d (sp + nov - 1) cont;
  Array.unsafe_set d (sp + nov) eptr;
  Array.unsafe_set d (sp + nov + 1) rdepth;
  Array.unsafe_set d (sp + nov + 2) mb.mark (* saved_mark *);
  Array.unsafe_set d (sp + nov + 3) Save_stack.kind_nassert;
  mb.once_base <- sp;
  need

(* Push a KIND_POS boundary at [sp] (fast-design.md §3, chunk H): the KIND_ONCE
   snapshot (ovector + saved_mark) + the possessive per-loop state (iter_start =
   [eptr]; matched_once = 0; [zero_allowed]; entry_rdepth = [rdepth]); set
   [mb.once_base] = sp. Returns the new sp. Layout: [ov_snapshot; prev_once_base;
   iter_start; matched_once; zero_allowed; entry_rdepth; saved_mark; KIND_POS],
   width 2*oveccount + 5. [saved_mark] reverts mb.mark on backtrack-past (each
   KETRPOS iteration commit truncates its MARK records, so the mark persists in
   the loop but must revert if the whole group is abandoned). *)
let push_pos (mb : mb) (sp : int) (eptr : int) (rdepth : int) (zero_allowed : int)
    : int =
  let ss = mb.ss in
  let nov = 2 * mb.oveccount in
  let need = sp + nov + 5 in
  if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
  let d = ss.Save_stack.data in
  let ov = mb.ovector in
  (* safe: [grow] ensured length >= sp + nov + 5. *)
  for i = 2 to nov - 1 do
    Array.unsafe_set d (sp + i - 2) (Array.unsafe_get ov i)
  done;
  Array.unsafe_set d (sp + nov - 2) mb.once_base (* prev_once_base *);
  Array.unsafe_set d (sp + nov - 1) eptr (* iter_start *);
  Array.unsafe_set d (sp + nov) 0 (* matched_once *);
  Array.unsafe_set d (sp + nov + 1) zero_allowed;
  Array.unsafe_set d (sp + nov + 2) rdepth (* entry_rdepth *);
  Array.unsafe_set d (sp + nov + 3) mb.mark (* saved_mark *);
  Array.unsafe_set d (sp + nov + 4) Save_stack.kind_pos;
  mb.once_base <- sp;
  need

(* ---------- Pattern recursion (chunk K1b, §3) ---------- *)

(* The array-snapshot size = the ov snapshot ([nov-2]) + the group_start snapshot
   ([n_groups]) + the cap_start snapshot ([nov]). nov = 2*oveccount. Both the FAT
   KIND_RECURSE and the KIND_RECURSE_RET records carry this whole-arrays prefix;
   [recurse_tail_off] is where each record's fixed tail begins. *)
let recurse_tail_off (mb : mb) : int =
  (2 * (2 * mb.oveccount)) - 2 + mb.n_groups

(* KIND_RECURSE total width (arrays prefix + 9 tail slots incl. the kind). *)
let recurse_width (mb : mb) : int = recurse_tail_off mb + 9

(* KIND_RECURSE_RET total width (arrays prefix + number + my_recurse_base + kind). *)
let recurse_ret_width (mb : mb) : int = recurse_tail_off mb + 3

(* Snapshot mb.{ovector[2,nov); group_start; cap_start} into [d] at [base]
   (the arrays prefix shared by both recursion records). *)
let snapshot_arrays (mb : mb) (d : int array) (base : int) : unit =
  let nov = 2 * mb.oveccount in
  let ng = mb.n_groups in
  let ov = mb.ovector in
  for i = 2 to nov - 1 do
    Array.unsafe_set d (base + i - 2) (Array.unsafe_get ov i)
  done;
  let gbase = base + (nov - 2) in
  let gs = mb.group_start in
  for i = 0 to ng - 1 do
    Array.unsafe_set d (gbase + i) (Array.unsafe_get gs i)
  done;
  let cbase = gbase + ng in
  let cs = mb.cap_start in
  for i = 0 to nov - 1 do
    Array.unsafe_set d (cbase + i) (Array.unsafe_get cs i)
  done

(* Restore mb.{ovector[2,nov); group_start; cap_start} FROM [d] at [base]. The
   whole-arrays restore is REQUIRED on a recursion return (not just backtrack):
   the recursion re-entered its group's IR without executing the enclosing
   construct's entry, so its GROUP_START/CAP_START_REF may have clobbered
   group_start/cap_start slots the ENCLOSING construct's ket later reads (the C's
   caller frame keeps its own eptr; here the shared arrays must be reinstated —
   the fast-design.md §3 snapshot proof's recursion revisit). *)
let restore_arrays (mb : mb) (d : int array) (base : int) : unit =
  let nov = 2 * mb.oveccount in
  let ng = mb.n_groups in
  let ov = mb.ovector in
  for i = 2 to nov - 1 do
    Array.unsafe_set ov i (Array.unsafe_get d (base + i - 2))
  done;
  let gbase = base + (nov - 2) in
  let gs = mb.group_start in
  for i = 0 to ng - 1 do
    Array.unsafe_set gs i (Array.unsafe_get d (gbase + i))
  done;
  let cbase = gbase + ng in
  let cs = mb.cap_start in
  for i = 0 to nov - 1 do
    Array.unsafe_set cs i (Array.unsafe_get d (cbase + i))
  done

(* Push a FAT KIND_RECURSE record at [sp] (fast-design.md §3): snapshot the WHOLE
   group ovector [2,nov), [mb.group_start] and [mb.cap_start] (the wholesale-frame
   answer); plus the enclosing once_base / current_recurse / recurse_base, the
   entry mark, the recursion [number], the call position [call_eptr], the
   last_used_ptr snapshot (RECURSELOOP), and the [cont_pc]. Sets
   mb.current_recurse = number, mb.recurse_base = sp. Cold. *)
let push_recurse (mb : mb) (sp : int) (number : int) (call_eptr : int)
    (cont_pc : int) : int =
  let ss = mb.ss in
  let need = sp + recurse_width mb in
  if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
  let d = ss.Save_stack.data in
  (* safe: [grow] ensured the whole record fits. *)
  snapshot_arrays mb d sp;
  let f = sp + recurse_tail_off mb in
  Array.unsafe_set d f mb.once_base;
  Array.unsafe_set d (f + 1) mb.current_recurse;
  Array.unsafe_set d (f + 2) mb.recurse_base;
  Array.unsafe_set d (f + 3) mb.mark;
  Array.unsafe_set d (f + 4) number;
  Array.unsafe_set d (f + 5) call_eptr;
  Array.unsafe_set d (f + 6) mb.last_used_ptr;
  Array.unsafe_set d (f + 7) cont_pc;
  Array.unsafe_set d (f + 8) Save_stack.kind_recurse;
  mb.recurse_base <- sp;
  mb.current_recurse <- number;
  need

(* Restore mb.{arrays; once_base; current_recurse; recurse_base; mark} from the
   FAT KIND_RECURSE record whose base is [b] (backtrack-past / verb-through /
   RECURSELOOP-NOMATCH). *)
let restore_recurse (mb : mb) (b : int) : unit =
  let d = mb.ss.Save_stack.data in
  restore_arrays mb d b;
  let f = b + recurse_tail_off mb in
  mb.once_base <- Array.unsafe_get d f;
  mb.current_recurse <- Array.unsafe_get d (f + 1);
  mb.recurse_base <- Array.unsafe_get d (f + 2);
  mb.mark <- Array.unsafe_get d (f + 3)

(* pcre2_match.c:5438-5453 — the RECURSELOOP walk. Called only when already in a
   recursion (mb.current_recurse >= 0). Walk the chain of open KIND_RECURSE
   records (mb.recurse_base, linked by [prev_recurse_base]) for the INNERMOST one
   whose number == [number] (the C breaks at the first GF_RECURSE|number frame);
   report a loop iff that recursion was dispatched at the SAME eptr AND
   last_used_ptr is unchanged since (and the check is not disabled). *)
(* Module-level walk (port-conventions.md §9 named hazard: a local [let rec]
   here would allocate a closure per RECURSE — cold but on the recursion path,
   caught by the alloc pin). Explicit state args; tail-recursive. *)
let rec recurse_loop_walk (d : int array) (toff : int) (number : int)
    (eptr : int) (last_used : int) (disabled : bool) (base : int) : bool =
  if base < 0 then false
  else
    let f = base + toff in
    if Int.equal (Array.unsafe_get d (f + 4)) number then
      Int.equal eptr (Array.unsafe_get d (f + 5))
      && Int.equal last_used (Array.unsafe_get d (f + 6))
      && not disabled
    else
      (recurse_loop_walk [@tailcall]) d toff number eptr last_used disabled
        (Array.unsafe_get d (f + 2))

let recurse_loop_check (mb : mb) (number : int) (eptr : int) : bool =
  recurse_loop_walk mb.ss.Save_stack.data (recurse_tail_off mb) number eptr
    mb.last_used_ptr mb.disable_recurseloop mb.recurse_base

(* ---------- Backreferences (chunk F, §2/§3) ---------- *)

(* pcre2_match.c:390-434 — match_ref()'s caseless compare loop in UTF and/or
   UCP mode (chunk I2; interpreter.ml match_ref_ci_uni): match characters up
   to the end of the REFERENCE (p < endptr — the number of subject code units
   matched may differ, e.g. U+023A/U+2C65 have different UTF-8 lengths, so
   the length is checked along the reference, not the subject). Each pair
   matches caseful, via UCD other-case, or via the character's caseless set
   (Char_predicates.caseless_set_member). On success the number of subject
   code units consumed goes through [mb.ref_length] (the C's shared exit at
   479). Returns 0 matched / -1 no match / 1 partial. Bounds as [ref_cmp_ci]
   below (p ranges over the captured substring; eptr < end_subject checked
   per character). *)
let rec ref_cmp_ci_uni (mb : mb) (p : int) (endptr : int) (eptr : int)
    (eptr_start : int) : int =
  if p >= endptr then (
    mb.ref_length <- eptr - eptr_start (* 479 *);
    0)
  else if eptr >= mb.end_subject then 1 (* partial match, 408 *)
  else
    (* pcre2_match.c:410-419 — if (utf) GETCHARINC(c, eptr); GETCHARINC(d, p);
       else one code unit each. safe: eptr < end_subject (checked above) and
       p < endptr <= end_subject (the reference bounds contract). *)
    let ce = Char.code (String.unsafe_get mb.subject eptr) in
    let c = if mb.utf && ce >= 0xc0 then Utf.getutf8 ce mb.subject eptr else ce in
    let eptr' =
      if mb.utf && ce >= 0xc0 then eptr + 1 + Utf.get_extralen ce else eptr + 1
    in
    let cp = Char.code (String.unsafe_get mb.subject p) in
    let d = if mb.utf && cp >= 0xc0 then Utf.getutf8 cp mb.subject p else cp in
    let p' =
      if mb.utf && cp >= 0xc0 then p + 1 + Utf.get_extralen cp else p + 1
    in
    (* pcre2_match.c:421-431 — ur = GET_UCD(d); other case, then the caseless
       set. *)
    if
      Int.equal c d
      || Int.equal c (Ucd.othercase d)
      || Char_predicates.caseless_set_member c (Ucd.caseset d)
    then (ref_cmp_ci_uni [@tailcall]) mb p' endptr eptr' eptr_start
    else -1 (* no match, 427 *)

(* pcre2_match.c:438-451 — match_ref()'s caseless compare loop, not in UTF or
   UCP mode: fold both code units through the lcc table (interpreter.ml
   match_ref_ci :815-826). Returns 0 all matched / -1 no match / 1 partial. *)
let rec ref_cmp_ci (mb : mb) (p : int) (eptr : int) (length : int) : int =
  if length <= 0 then 0
  else if eptr >= mb.end_subject then 1 (* partial match, 443 *)
  else
    (* safe: eptr < mb.end_subject <= String.length subject (checked above);
       p in the captured substring [start, end) with end <= end_subject. *)
    let cc = Char.code (String.unsafe_get mb.subject eptr) in
    let cp = Char.code (String.unsafe_get mb.subject p) in
    if not (Int.equal (Chartables.lcc cp) (Chartables.lcc cc)) then -1
    else (ref_cmp_ci [@tailcall]) mb (p + 1) (eptr + 1) (length - 1)

(* pcre2_match.c:460-467 — match_ref()'s caseful compare loop for partial
   matching: unit by unit, checking the subject end before each unit
   (interpreter.ml match_ref_cs_partial :831-844). *)
let rec ref_cmp_cs_partial (mb : mb) (p : int) (eptr : int) (length : int) : int =
  if length <= 0 then 0
  else if eptr >= mb.end_subject then 1 (* partial match, 464 *)
  else if
    (* safe: as ref_cmp_ci. *)
    not
      (Int.equal
         (Char.code (String.unsafe_get mb.subject p))
         (Char.code (String.unsafe_get mb.subject eptr)))
  then -1 (* no match, 465 *)
  else (ref_cmp_cs_partial [@tailcall]) mb (p + 1) (eptr + 1) (length - 1)

(* pcre2_match.c:474 — memcmp(p, eptr, CU2BYTES(length)) != 0 as an equality
   scan (interpreter.ml match_ref_memcmp :849-858). Caller checked
   end_subject - eptr >= length. *)
let rec ref_memcmp (mb : mb) (p : int) (eptr : int) (length : int) : bool =
  length <= 0
  || Int.equal
       (* safe: eptr + length <= end_subject (caller's 473 check); p in the
          captured substring. *)
       (Char.code (String.unsafe_get mb.subject p))
       (Char.code (String.unsafe_get mb.subject eptr))
     && (ref_memcmp [@tailcall]) mb (p + 1) (eptr + 1) (length - 1)

(* pcre2_match.c:360-481 — match_ref() (interpreter.ml match_ref :899-957;
   the UTF/UCP caseless fold landed in chunk I2). Match the reference at group
   [ovbase]
   (fast public convention, group N at [2N,2N+1]) against the subject at
   [eptr]. A referenced group's ovector slots are written ONLY at its close
   (t_cap_end_ref) and restored on backtrack, so [ovector.(ovbase) = UNSET]
   captures exactly the C's `offset >= Foffset_top || Fovector[offset] ==
   PCRE2_UNSET` (unset on the current path, §3). Returns 0 = match (with the
   consumed length in [mb.ref_length]); -1 = no match; 1 = partial. *)
let match_ref (mb : mb) (ovbase : int) (caseless : bool) (eptr : int) : int =
  let ov = mb.ovector in
  (* safe (every ov.() below): ovbase = 2N, 1 <= N <= top_bracket — a REF
     operand (Ir_verify ovbase_ok) or a dnref_scan result (a compiler-generated
     name-table group number), so ovbase, ovbase+1 < 2*oveccount =
     Array.length ov. *)
  (* pcre2_match.c:372-380 — unset group: no match, unless MATCH_UNSET_BACKREF
     (then an empty match). *)
  if Int.equal (Array.unsafe_get ov ovbase) Frames.unset then
    if mb.match_unset_backref then (
      mb.ref_length <- 0;
      0)
    else -1
  else
    (* pcre2_match.c:384-386 — p = start_subject + Fovector[offset]; length =
       Fovector[offset+1] - Fovector[offset]. *)
    let p = mb.start_subject + Array.unsafe_get ov ovbase in
    let length = Array.unsafe_get ov (ovbase + 1) - Array.unsafe_get ov ovbase in
    if caseless then
      if mb.utf || mb.ucp then
        (* pcre2_match.c:388-434 — the SUPPORT_UNICODE utf/ucp fold (chunk
           I2): character-wise compare with UCD other-case and caseless-set
           fallback; [ref_cmp_ci_uni] writes mb.ref_length itself on success
           (the consumed subject length can differ from the reference length
           in UTF mode). *)
        ref_cmp_ci_uni mb p (p + length) eptr eptr
      else (
        (* pcre2_match.c:438-451 (non-UTF/UCP). *)
        let rc = ref_cmp_ci mb p eptr length in
        if Int.equal rc 0 then mb.ref_length <- length;
        rc)
    else if mb.partial <> 0 then (
      (* pcre2_match.c:460-467 — caseful, partial: unit by unit. *)
      let rc = ref_cmp_cs_partial mb p eptr length in
      if Int.equal rc 0 then mb.ref_length <- length;
      rc)
    else if mb.end_subject - eptr < length then 1 (* partial, 473 *)
    else if not (ref_memcmp mb p eptr length) then -1 (* no match, 474 *)
    else (
      mb.ref_length <- length;
      0)

(* pcre2_match.c:5002-5007 — the OP_DNREF group-list walk (interpreter.ml
   dnref_scan :6060-6073): return the ovbase of the first group in the list
   that is SET, or the last examined entry when none is set (the caller then
   applies the unset-reference logic). [slot_base] is the name-table byte
   offset of the first entry; each entry begins with the group NUMBER (GET2).
   count >= 1 in a compiled program. *)
let rec dnref_scan (mb : mb) (remaining : int) (slot : int) : int =
  (* Compile.get2 (safe Bytes.get) reads a 2-byte group number; Ir_verify proved
     the whole list span [slot_base, slot_base+(count-1)*entry_size] is inside
     name_table, so no read raises across the loop. *)
  let ovbase = Compile.get2 mb.name_table slot lsl 1 (* 2N, fast convention *) in
  if remaining <= 1 then ovbase (* last entry (C: count == 1) *)
  else if
    (* safe: ovbase = 2N, 1 <= N <= top_bracket (compiler-generated name-table
       group number), so ovbase < 2*oveccount = Array.length mb.ovector. *)
    not (Int.equal (Array.unsafe_get mb.ovector ovbase) Frames.unset)
  then ovbase (* first set group *)
  else (dnref_scan [@tailcall]) mb (remaining - 1) (slot + mb.name_entry_size)

(* pcre2_match.c:5675-5684 (interpreter.ml dncref_scan :6662-6677) — the
   OP_DNCREF duplicate-name group-list test for a conditional: TRUE iff ANY of
   the [remaining] groups in the name list (starting at byte offset [slot] in the
   name table, each entry beginning with the group NUMBER) is SET. A group N is
   set iff its ovector slot ovector.(2N) <> UNSET (a CREF/DNCREF-referenced group
   is non-optimized — chunk F — so the slot is written only at CLOSE, matching
   the interpreter's `Fovector[offset] != PCRE2_UNSET`). *)
let rec dncref_test (mb : mb) (remaining : int) (slot : int) : bool =
  if remaining <= 0 then false
  else
    (* Compile.get2 (safe Bytes.get); Ir_verify proved the whole list span is
       inside name_table (like t_dnref). *)
    let ovbase = Compile.get2 mb.name_table slot lsl 1 (* 2N *) in
    (* safe: ovbase = 2N, 1 <= N <= top_bracket, so ovbase < Array.length ovector. *)
    if not (Int.equal (Array.unsafe_get mb.ovector ovbase) Frames.unset) then true
    else (dncref_test [@tailcall]) mb (remaining - 1) (slot + mb.name_entry_size)

(* pcre2_match.c:5656-5664 (interpreter.ml dnrref_scan :6649-6660) — the
   OP_DNRREF duplicate-name RECURSION test (chunk K1b): TRUE iff ANY of the
   [remaining] groups in the name list (starting at byte offset [slot], each
   entry beginning with the group NUMBER) equals mb.current_recurse. The caller
   has already verified mb.current_recurse >= 0. *)
let rec dnrref_test (mb : mb) (remaining : int) (slot : int) : bool =
  if remaining <= 0 then false
  else
    (* Compile.get2 (safe Bytes.get); Ir_verify proved the list span is inside
       name_table (COND_DNRREF is verified like COND_DNCREF). *)
    let num = Compile.get2 mb.name_table slot in
    if Int.equal num mb.current_recurse then true
    else (dnrref_test [@tailcall]) mb (remaining - 1) (slot + mb.name_entry_size)

(* [setup_ref_rep] decodes the ref-repeat instruction at [pc] into mb.ref_*
   (fast-design.md §2/§3). Called on the forward path (op_ref_repeat) and
   re-called on the RM20 backtrack (KIND_REF_MIN) to restore the fields a
   nested ref may have clobbered — it writes only mb.ref_*, so it is
   idempotent. For a DNREF the ovbase is re-resolved by dnref_scan (the
   referenced group precedes the ref, so its ovector is stable across the
   repeat's backtracking, §3). *)
let setup_ref_rep (mb : mb) (pc : int) : unit =
  let code = mb.code in
  let tag = code.(pc) in
  mb.ref_pc <- pc;
  mb.ref_reptype <- code.(pc + 1);
  mb.ref_lmin <- code.(pc + 2);
  mb.ref_lmax <- code.(pc + 3);
  if Int.equal tag 33 (* t_ref_rep *) then (
    mb.ref_ovbase <- code.(pc + 4);
    mb.ref_caseless <- Int.equal code.(pc + 5) 1;
    mb.ref_cont <- pc + 6)
  else (
    (* tag 35 t_dnref_rep. *)
    mb.ref_ovbase <- dnref_scan mb code.(pc + 5) code.(pc + 4);
    mb.ref_caseless <- Int.equal code.(pc + 6) 1;
    mb.ref_cont <- pc + 7)

(* pcre2_match.c:5177-5182 (interpreter.ml ref_max_rescan) — the RM22 resume's
   forward re-scan: for (i = Lmin; i < Lmax; i++) re-match the reference —
   match_ref is known to succeed every time (5164-5165), so its return code is
   discarded, exactly as the C's (void) cast — and advance by the consumed
   length. Reads the repeat invariants from mb.ref_* (set by [setup_ref_rep]
   just before). Module-level + fully applied so the RM22 backtrack builds NO
   closure (§8 — a local `let rec` here would allocate ~6 words per RM22
   give-back step, which the RM22 alloc pin measures). Returns the position
   after [lmax_cur] - i copies from [e]. *)
let rec ref_max2_rescan (mb : mb) (lmax_cur : int) (i : int) (e : int) : int =
  if i < lmax_cur then (
    let (_ : int) = match_ref mb mb.ref_ovbase mb.ref_caseless e in
    (ref_max2_rescan [@tailcall]) mb lmax_cur (i + 1) (e + mb.ref_length))
  else e

(* pcre2_match.c:6352 PRIV(strcmp) (interpreter.ml strcmp_code_eq :747-752): a
   zero-terminated code-unit compare of two verb names at byte offsets [p1]/[p2]
   in [bytecode]. Used by the MARK arm (KIND_VERB vt_mark) to test whether a
   MARK's argument matches a firing SKIP_ARG's argument. Cold (verb path only). *)
let rec strcmp_code_eq (mb : mb) (p1 : int) (p2 : int) : bool =
  let c1 = Char.code (Bytes.get mb.bytecode p1) in
  let c2 = Char.code (Bytes.get mb.bytecode p2) in
  if Int.equal c1 0 && Int.equal c2 0 then true
  else if not (Int.equal c1 c2) then false
  else (strcmp_code_eq [@tailcall]) mb (p1 + 1) (p2 + 1)

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
      else if e = sig_backtrack then
        (* Chunk K1b — a has_recurse run is DE-FUSED to one character, and the
           C's OP_CHAR mismatch RRETURNs after the post-increment
           (pcre2_match.c:1009/:1022): record eptr + k + 1 for a mismatch at
           unit k, eptr for a length shortage (char_run_cfail). Non-recursive
           patterns skip the rescan (bt does not record for them). *)
        (bt [@tailcall]) mb
          (if mb.has_recurse then char_run_cfail mb lit_off (lit_off + len) eptr
           else eptr)
          sp mcc
      else e (* PARTIAL — propagate *)
  | 2 ->
      (* CHARI (fast-design.md §2; non-UTF OP_CHARI arm,
         pcre2_match.c:1097-1103 / interpreter.ml:1619-1634) — one caseless
         code unit via the lowercase table. *)
      if eptr >= mb.end_subject then (
        (* pcre2_match.c:1035-1039 SCHECK_PARTIAL then NOMATCH. *)
        let r = scheck_partial mb eptr in
        if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc)
      else
        let pch = code.(pc + 1) in
        if mb.utf && pch >= 128 then (
          (* pcre2_match.c:1061-1072 (interpreter.ml:1571-1587) — UTF, pattern
             char whose other case may be > 127: read the subject char
             (GETCHARINC) and test it against the pattern char or its Unicode
             other case. safe: eptr < end_subject <= String.length subject. *)
          let c0 = Char.code (String.unsafe_get mb.subject eptr) in
          let dc = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
          let e' = if c0 >= 0xc0 then eptr + 1 + Utf.get_extralen c0 else eptr + 1 in
          if Int.equal dc pch || Int.equal dc (Ucd.othercase pch) then
            (run [@tailcall]) mb (pc + 2) e' sp rdepth mcc
          else
            (* Chunk K1b — this OP_CHARI subcase GETCHARINCs before the test
               (pcre2_match.c:1069-1071), so the mismatch records the post-read
               position e' (last_used_ptr); the other CHARI subcases test before
               advancing (:1056-1058/:1084-1090/:1099-1101) and record eptr. *)
            (bt [@tailcall]) mb e' sp mcc)
        else if mb.ucp && pch >= 128 then
          (* pcre2_match.c:1075-1092 (interpreter.ml:1579-1602) — UCP without
             UTF (chunk I2): one character per code unit; cc != fc && cc !=
             UCD_OTHERCASE(fc) is NOMATCH (the pattern char's other case may
             be > 255 and then never equals a code unit). safe: eptr <
             end_subject <= String.length subject. *)
          let cc = Char.code (String.unsafe_get mb.subject eptr) in
          if (not (Int.equal cc pch)) && not (Int.equal cc (Ucd.othercase pch))
          then (bt [@tailcall]) mb eptr sp mcc
          else (run [@tailcall]) mb (pc + 2) (eptr + 1) sp rdepth mcc
        else
          (* pcre2_match.c:1097-1103 non-UTF (and UTF with pattern char < 128,
             :1557-1570): fold one code unit through the lcc table. safe: eptr <
             end_subject <= String.length subject. *)
          let cc = Char.code (String.unsafe_get mb.subject eptr) in
          if not (Int.equal (Chartables.lcc pch) (Chartables.lcc cc)) then
            (bt [@tailcall]) mb eptr sp mcc
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
      (* KET (fast-design.md §2) — structural group exit; continue past. Chunk
         K1b: the WHOLE-PATTERN ket (the one immediately before END) is the
         group-0 recursion return point ((?R)/(?0), pcre2_match.c:5964-5984):
         when mb.current_recurse = 0 it RETURNS to the (?R) continuation rather
         than reaching END. current_recurse = 0 arises only inside a group-0
         recursion, so normal matching (current_recurse = -1) falls straight
         through. *)
      if Int.equal mb.current_recurse 0 && Int.equal code.(pc + 1) 0 (* END *)
      then (recurse_return [@tailcall]) mb eptr sp rdepth mcc
      else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
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
           layout (fast-design.md §3, chunk H): [handler; eptr; rdepth; then_end;
           KIND_ALT]. [then_end] is this ALT's THEN scope boundary
           (mb.alt_then_end.(pc)); the hot NOMATCH backtrack reads only
           handler/eptr/rdepth at slots 0/1/2. *)
        Array.unsafe_set d sp handler;
        Array.unsafe_set d (sp + 1) eptr;
        Array.unsafe_set d (sp + 2) rdepth;
        Array.unsafe_set d (sp + 3) (Array.unsafe_get mb.alt_then_end pc);
        Array.unsafe_set d (sp + 4) Save_stack.kind_alt;
        (run [@tailcall]) mb (pc + 2) eptr need (rdepth + 1) mcc')
  | 6 ->
      (* JMP (fast-design.md §2) — end-of-branch jump to the group KET; the
         branch matched, so skip the remaining alternatives. rdepth is
         unchanged (the branch's child frame carries the continuation). *)
      (run [@tailcall]) mb code.(pc + 1) eptr sp rdepth mcc
  | 7 ->
      (* SOD \A (pcre2_match.c:6139-6142 / interpreter.ml:3082-3088). *)
      if not (Int.equal eptr mb.start_subject) then
        (bt [@tailcall]) mb eptr sp mcc
      else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
  | 8 ->
      (* SOM \G (pcre2_match.c:6243-6246 / interpreter.ml:3162-3173). *)
      if not (Int.equal eptr (mb.start_subject + mb.start_offset)) then
        (bt [@tailcall]) mb eptr sp mcc
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
        (bt [@tailcall]) mb eptr sp mcc
      else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
  | 12 ->
      (* DOLL $ non-multiline (pcre2_match.c:6147-6151 /
         interpreter.ml:3089-3097). *)
      if mb.noteol then (bt [@tailcall]) mb eptr sp mcc
      else if not mb.dollar_endonly then
        (assert_nl_or_eos [@tailcall]) mb pc eptr sp rdepth mcc
      else (op_eod [@tailcall]) mb pc eptr sp rdepth mcc
  | 13 ->
      (* OP_CIRCM ^ multiline (pcre2_match.c:6200-6211 /
         interpreter.ml:3108-3124). No tick (dispatched in place). *)
      if mb.notbol && Int.equal eptr mb.start_subject then
        (bt [@tailcall]) mb eptr sp mcc
      else if
        (not (Int.equal eptr mb.start_subject))
        && ((Int.equal eptr mb.end_subject && not mb.alt_circumflex)
           || not (was_newline_at mb eptr))
      then (bt [@tailcall]) mb eptr sp mcc
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
            else (bt [@tailcall]) mb eptr sp mcc)
          else (bt [@tailcall]) mb eptr sp mcc
        else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
      else if mb.noteol then (bt [@tailcall]) mb eptr sp mcc
      else
        let r = scheck_partial mb eptr in
        if r < 0 then r else (run [@tailcall]) mb (pc + 1) eptr sp rdepth mcc
  | 15 ->
      (* FAIL (fast-design.md §2/§3) — a grouploop group / positive assertion /
         atomic group / script run has exhausted its branches, or a ( *FAIL)
         verb: propagate NOMATCH (no tick). Only reachable via a last ALT's
         handler on backtrack (or, for OP_FAIL, forward flow); forward flow
         otherwise JMPs over it to the KET. Routed through [bt]: the C's
         counterpart at each of these sites is RRETURN(MATCH_NOMATCH) from the
         frame whose Feptr = the ALT-restored eptr (grouploop pcre2_match.c:5410,
         positive assertion :5531, OP_FAIL :6360), so RETURN_SWITCH (:6470)
         records it in last_used_ptr. *)
      (bt [@tailcall]) mb eptr sp mcc
  | 73 ->
      (* FAIL_NASSERT (chunk K1b) — a NEGATIVE assertion's / assertion
         CONDITION's branch exhaustion: as FAIL but WITHOUT the last_used_ptr
         record. The C's exhaustion there is a same-frame transfer with NO
         RRETURN (ASSERT_NOT_FAILED, pcre2_match.c:5561-5564 -> :5582; the
         condition-assertion `condition = !Lpositive; break`, :5729-5734), so
         the assertion frame's entry Feptr is never recorded — recording it here
         would OVER-approximate last_used_ptr for a negative LOOKBEHIND (whose
         branches fail strictly below entry) and shift the RECURSELOOP -52
         depth. The backtrack below reaches the KIND_NASSERT boundary =
         assertion success / the nomatch branch. *)
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
      (* Chunk K1b — when this capture's group N (= ovb/2) is the one currently
         being recursed (mb.current_recurse == N), the ket is a recursion RETURN
         (pcre2_match.c:6065-6074), NOT a capture write: group N does not capture
         during its own recursion (the call entered at t_bra, skipping t_cap_start)
         and the captures are restored to pre-call. current_recurse >= 1 only
         inside such a recursion, so normal matching falls to the write. *)
      if Int.equal mb.current_recurse (ovb lsr 1) then
        (recurse_return [@tailcall]) mb eptr sp rdepth mcc
      else (
        (* safe: ovb+1 < 2*oveccount = Array.length mb.ovector (Ir_verify). *)
        Array.unsafe_set mb.ovector (ovb + 1) (eptr - mb.start_subject);
        (run [@tailcall]) mb (pc + 2) eptr sp rdepth mcc)
  | 18 | 19 | 20 | 21 | 30 | 31 ->
      (* Repeat superinstructions (fast-design.md §2/§4): char (18-21;
         repeatchar/repeatnotchar interpreter.ml:3549-4128), character type
         (30; repeattype interpreter.ml:4606-...) and class (31; the OP_CLASS
         repeat interpreter.ml:1807-1872). *)
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
        if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc)
      else if mb.utf then
        (* Chunk I — UTF: decode the code point; a code point > 255 matches only
           OP_NCLASS (interpreter.ml:4463-4468). safe: eptr < end_subject. *)
        let cp = cur_cp mb eptr in
        if class_cp_match mb code.(pc + 1) cp then
          (run [@tailcall]) mb (pc + 2) (eptr + cp_len mb eptr) sp rdepth mcc
        else
          (* Chunk K1b — the C's class loop GETCHARINCs before the bitmap test
             (pcre2_match.c:1986-1992): a mismatch records the post-read
             position (last_used_ptr). *)
          (bt [@tailcall]) mb (eptr + cp_len mb eptr) sp mcc
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
        let cc = Char.code (String.unsafe_get mb.subject eptr) in
        if class_bit_at mb code.(pc + 1) cc then
          (run [@tailcall]) mb (pc + 2) (eptr + 1) sp rdepth mcc
        else
          (* Chunk K1b — fc = *Feptr++ before the test (pcre2_match.c:2006):
             record eptr + 1. *)
          (bt [@tailcall]) mb (eptr + 1) sp mcc
  | 57 ->
      (* XCLASS (chunk I; OP_XCLASS single, pcre2_match.c:2175-2224) — a lone
         extended class (lmin=lmax=1: one character, no choice point, no tick).
         Read the code point and test via the self-contained Xclass.xclass
         against the class data offset (code.(pc+1)). *)
      if eptr >= mb.end_subject then (
        let r = scheck_partial mb eptr in
        if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc)
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
        let cp = cur_cp mb eptr in
        if Xclass.xclass cp mb.bytecode code.(pc + 1) mb.utf then
          (run [@tailcall]) mb (pc + 2) (eptr + cp_len mb eptr) sp rdepth mcc
        else
          (* Chunk K1b — GETCHARINCTEST before the xclass test
             (pcre2_match.c:2221-2222): record the post-read position. *)
          (bt [@tailcall]) mb (eptr + cp_len mb eptr) sp mcc
  | 58 ->
      (* XCLASS_REP (chunk I; OP_XCLASS + OP_CR* repeat) — routes through the
         shared repeat machinery (rk_xclass). *)
      (op_repeat [@tailcall]) mb pc eptr sp rdepth mcc
  | 59 ->
      (* PROP (chunk I2; OP_PROP/OP_NOTPROP single, pcre2_match.c:2479-2614 /
         interpreter.ml:2207-2242) — one character-property test: decode the
         character (GETCHARINCTEST) and compare prop_test against the NOTPROP
         sense. One character, no choice point, no tick. (The C's
         ptype > PT_BOOL internal-error default is unreachable: Ir_verify
         proved ptype <= PT_BOOL.) *)
      if eptr >= mb.end_subject then (
        (* pcre2_match.c:2481-2485 — SCHECK_PARTIAL(), then no match. *)
        let r = scheck_partial mb eptr in
        if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc)
      else
        (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
        let fc = cur_cp mb eptr in
        if
          Bool.equal
            (prop_test fc code.(pc + 2) code.(pc + 3))
            (Int.equal code.(pc + 1) 1)
        then
          (* Chunk K1b — GETCHARINCTEST before the property test
             (pcre2_match.c:2486-2488): record the post-read position. *)
          (bt [@tailcall]) mb (eptr + cp_len mb eptr) sp mcc
        else
          (run [@tailcall]) mb (pc + 4) (eptr + cp_len mb eptr) sp rdepth mcc
  | 60 | 62 ->
      (* PROP_REP / EXTUNI_REP (chunk I2) — property / grapheme-cluster type
         repeats through the shared repeat machinery (rk_prop / rk_extuni). *)
      (op_repeat [@tailcall]) mb pc eptr sp rdepth mcc
  | 61 ->
      (* EXTUNI (chunk I2; OP_EXTUNI single, pcre2_match.c:2617-2635 /
         interpreter.ml:2243-2265) — match one extended grapheme cluster: step
         it (extuni_at), then CHECK_PARTIAL. No choice point, no tick. *)
      if eptr >= mb.end_subject then (
        (* pcre2_match.c:2622-2626 — SCHECK_PARTIAL(), then no match. *)
        let r = scheck_partial mb eptr in
        if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc)
      else
        (* pcre2_match.c:2627-2632 — GETCHARINCTEST + PRIV(extuni). *)
        let e' = extuni_at mb eptr in
        (* CHECK_PARTIAL() (2633). *)
        let r = if e' >= mb.end_subject then scheck_partial mb e' else 0 in
        if r < 0 then r else (run [@tailcall]) mb (pc + 1) e' sp rdepth mcc
  | 29 ->
      (* WORDBOUND \b / \B, ASCII or UCP per the operand's bit 1 (fast-design
         .md §2; pcre2_match.c:6250-6333 / interpreter.ml:3181-3332). *)
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
  | 32 ->
      (* REF (fast-design.md §2/§3; OP_REF single, pcre2_match.c:5045-5056) —
         match a numbered backref once, then continue. No choice point, no
         tick; on failure/partial: CHECK_PARTIAL then NOMATCH. *)
      (op_single_ref [@tailcall]) mb (pc + 3) code.(pc + 1)
        (Int.equal code.(pc + 2) 1)
        eptr sp rdepth mcc
  | 34 ->
      (* DNREF (fast-design.md §2/§3; OP_DNREF single, pcre2_match.c:4994-5009 +
         5045-5056) — resolve the duplicate-named group list to the first set
         group (dnref_scan), then as REF. *)
      let ovb = dnref_scan mb code.(pc + 2) code.(pc + 1) in
      (op_single_ref [@tailcall]) mb (pc + 4) ovb
        (Int.equal code.(pc + 3) 1)
        eptr sp rdepth mcc
  | 33 | 35 ->
      (* REF_REP / DNREF_REP (fast-design.md §2/§3/§4; the OP_CR* ref repeat
         forms, pcre2_match.c:5017-5162). *)
      (op_ref_repeat [@tailcall]) mb pc eptr sp rdepth mcc
  | 36 ->
      (* CAP_START_REF (fast-design.md §3; the JIT non-optimized cbracket entry
         pcre2_jit_compile.c:11055-11061) — open a REFERENCED capture group N.
         Keep the start OFFSET in mb.cap_start (private scratch, NOT the ovector
         slot, which must stay UNSET until CLOSE so a mid-match backref sees
         only closed values); push a KIND_CAPSTART saving the enclosing
         cap_start value. No ovector write, no tick. *)
      let ovb = code.(pc + 1) in
      let ss = mb.ss in
      let need = sp + Save_stack.width_capstart in
      if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
      let d = ss.Save_stack.data in
      (* safe: [grow] ensured length >= sp + width_capstart; ovb in [2, 2*top]
         (Ir_verify) < 2*oveccount = Array.length mb.cap_start. *)
      Array.unsafe_set d sp ovb;
      Array.unsafe_set d (sp + 1) (Array.unsafe_get mb.cap_start ovb);
      Array.unsafe_set d (sp + 2) Save_stack.kind_capstart;
      Array.unsafe_set mb.cap_start ovb (eptr - mb.start_subject);
      (run [@tailcall]) mb (pc + 2) eptr (sp + Save_stack.width_capstart) rdepth
        mcc
  | 37 ->
      (* CAP_END_REF (fast-design.md §3; the JIT non-optimized cbracket close
         pcre2_jit_compile.c:10692-10702 / the C ket write pcre2_match.c:
         6081-6082) — close a REFERENCED capture: push a KIND_CAP saving the OLD
         ovector pair, then set ovector[ovb] = cap_start (the private start) and
         ovector[ovb+1] = current position. On backtrack past this point the
         KIND_CAP restores the pair (so a re-entered earlier iteration sees the
         group unset until it re-closes). No tick. *)
      let ovb = code.(pc + 1) in
      (* Chunk K1b — as t_cap_end, a recursion return of this referenced group. *)
      if Int.equal mb.current_recurse (ovb lsr 1) then
        (recurse_return [@tailcall]) mb eptr sp rdepth mcc
      else (
        let ss = mb.ss in
        let need = sp + Save_stack.width_cap in
        if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
        let d = ss.Save_stack.data in
        let ov = mb.ovector in
        (* safe: [grow] ensured length >= sp + width_cap; ovb, ovb+1 < 2*oveccount
           and ovb < Array.length mb.cap_start. *)
        Array.unsafe_set d sp ovb;
        Array.unsafe_set d (sp + 1) (Array.unsafe_get ov ovb);
        Array.unsafe_set d (sp + 2) (Array.unsafe_get ov (ovb + 1));
        Array.unsafe_set d (sp + 3) Save_stack.kind_cap;
        Array.unsafe_set ov ovb (Array.unsafe_get mb.cap_start ovb);
        Array.unsafe_set ov (ovb + 1) (eptr - mb.start_subject);
        (run [@tailcall]) mb (pc + 2) eptr (sp + Save_stack.width_cap) rdepth mcc)
  | 38 ->
      (* REVERSE (fast-design.md §2/§3; OP_REVERSE pcre2_match.c:5793-5819 /
         interpreter.ml:2632-2651) — fixed lookbehind step-back. In UTF the
         count is CHARACTERS, walked back one BACKCHAR at a time and floored
         at mb->check_subject (chunk I2; pcre2_match.c:5797-5804 /
         interpreter.ml op_reverse_utf_loop); non-UTF it is a code-unit count
         floored at the subject start (5808-5813). If too close to the start,
         NOMATCH. Save the earliest consulted character (start_used_ptr
         floor), then continue. No tick, no choice point. *)
      let number = code.(pc + 1) in
      if mb.utf then (op_reverse_utf [@tailcall]) mb pc number eptr sp rdepth mcc
      else if number > eptr - mb.start_subject then
        (bt [@tailcall]) mb eptr sp mcc
      else
        let e = eptr - number in
        if e < mb.start_used_ptr then mb.start_used_ptr <- e;
        (run [@tailcall]) mb (pc + 2) e sp rdepth mcc
  | 39 ->
      (* VREVERSE (fast-design.md §2/§3; OP_VREVERSE pcre2_match.c:5834-5883 /
         interpreter.ml:2661-2707) — variable lookbehind step-back. *)
      (op_vreverse [@tailcall]) mb pc eptr sp rdepth mcc
  | 40 ->
      (* ONCE (fast-design.md §2/§3; OP_ONCE / atomic-assertion entry) — push a
         KIND_ONCE boundary (snapshot the group ovector + enclosing once_base),
         set mb.once_base, fall into the body. No tick (the C dispatches OP_ONCE
         in the current frame; the first branch's RMATCH ticks — the body's ALT).
         Also the entry marker for atomic positive assertions (t_once precedes
         their t_group_start); its subtype (mb.once_subtype.(pc)) selects THEN
         containment vs escape at the boundary (chunk H). *)
      let sp' = push_once mb sp eptr (Array.unsafe_get mb.once_subtype pc) in
      (run [@tailcall]) mb (pc + 1) eptr sp' rdepth mcc
  | 41 ->
      (* ONCE_END (fast-design.md §3; OP_ONCE ket pcre2_match.c:6023-6031 /
         interpreter.ml:6887-6902) — atomic group commit: discard the body's
         internal choice points (truncate to the boundary). eptr unchanged (the
         group advanced it). Continue at the current rdepth (the C's ket `break`
         continues in the matching branch's frame). No tick. *)
      let sp' = once_commit mb in
      (run [@tailcall]) mb (pc + 1) eptr sp' rdepth mcc
  | 42 ->
      (* ASSERT_END (fast-design.md §3; the positive-assertion ket
         pcre2_match.c:5999-6016 / interpreter.ml:6857-6902) — restore eptr to
         the assertion's entry position (mb.group_start.(g), written by the
         preceding t_group_start), then continue. For the atomic kinds
         (atomic=1: OP_ASSERT/OP_ASSERTBACK) also commit (like OP_ONCE); the NA
         kinds (atomic=0) keep the body's choice points (non-atomic). No tick
         (the C's ket continues in the matching branch's frame). *)
      let atomic = code.(pc + 1) in
      let g = code.(pc + 2) in
      (* pcre2_match.c:6000/6014 — the assertion's reached position [eptr] is
         consulted before eptr is put back to the entry, raising last_used_ptr
         (the RECURSELOOP input); gated on has_recurse. *)
      if mb.has_recurse && eptr > mb.last_used_ptr then mb.last_used_ptr <- eptr;
      (* safe: g in [0, n_groups) (Ir_verify) indexes mb.group_start. *)
      let e = Array.unsafe_get mb.group_start g in
      if Int.equal atomic 1 then
        let sp' = once_commit mb in
        (run [@tailcall]) mb (pc + 3) e sp' rdepth mcc
      else (run [@tailcall]) mb (pc + 3) e sp rdepth mcc
  | 43 ->
      (* NASSERT (fast-design.md §3; negative-assertion entry
         pcre2_match.c:5547-5554) — push a KIND_NASSERT boundary (snapshot +
         entry cont/eptr/rdepth), set mb.once_base, fall into the body. No tick.
         A matching branch reaches t_nassert_match (fail); exhausting all
         branches backtracks to the KIND_NASSERT record = SUCCESS. *)
      let cont = code.(pc + 2) in
      let sp' = push_nassert mb sp cont eptr rdepth in
      (run [@tailcall]) mb (pc + 3) eptr sp' rdepth mcc
  | 44 ->
      (* NASSERT_MATCH (fast-design.md §3; a negative-assertion branch matched
         pcre2_match.c:5557-5559) — the assertion FAILS. Roll the group ovector
         back to the boundary snapshot, restore mb.once_base, then propagate the
         NOMATCH past the boundary (backtrack below it). No tick. *)
      (* Chunk K1b — the C's negative-assertion ket is RRETURN(MATCH_MATCH)
         (pcre2_match.c:6042-6043), so RETURN_SWITCH (:6470) records the body's
         reached Feptr in last_used_ptr (the RECURSELOOP input) BEFORE the RM4
         arm converts it to NOMATCH at the entry position — record it here (this
         arm is the fast MATCH-return; the reach would otherwise be lost when
         backtracking resumes below the boundary). Gated on has_recurse. *)
      if mb.has_recurse && eptr > mb.last_used_ptr then mb.last_used_ptr <- eptr;
      let base = mb.once_base in
      let nov = 2 * mb.oveccount in
      let d = mb.ss.Save_stack.data in
      let ov = mb.ovector in
      (* safe: [base] is a KIND_NASSERT record base (set by push_nassert); its
         snapshot occupies [base, base + nov - 2), prev_once_base at base + nov - 2
         and saved_mark at base + nov + 2. ov has nov slots. *)
      for i = 2 to nov - 1 do
        Array.unsafe_set ov i (Array.unsafe_get d (base + i - 2))
      done;
      mb.once_base <- Array.unsafe_get d (base + nov - 2);
      mb.mark <- Array.unsafe_get d (base + nov + 2) (* revert Fmark *);
      (backtrack [@tailcall]) mb base mcc
  | 45 ->
      (* ASSERTBACK_CHECK (fast-design.md §3; the variable-lookbehind end-point
         check pcre2_match.c:5995/6009/6038) — a VREVERSE branch is a valid
         lookbehind match only if it ended exactly at the assertion entry
         position; otherwise this back-length fails (backtrack into the VREVERSE
         RM37 loop / next branch). No tick. *)
      let g = code.(pc + 1) in
      (* safe: g in [0, n_groups) (Ir_verify). *)
      if not (Int.equal eptr (Array.unsafe_get mb.group_start g)) then
        (bt [@tailcall]) mb eptr sp mcc
      else (run [@tailcall]) mb (pc + 2) eptr sp rdepth mcc
  | 46 ->
      (* POSSESS (fast-design.md §3; OP_BRAPOS/CBRAPOS/SBRAPOS/SCBRAPOS entry
         pcre2_match.c:5283-5285) — push a KIND_POS boundary (snapshot + per-loop
         state), set mb.once_base, fall into the body at the current rdepth. No
         tick (the first branch's ALT ticks — the RM8 tick). For a CAPTURING
         possessive bracket, record the first iteration's start in mb.cap_start
         (chunk H) so an OP_CLOSE reached at an inner ( *ACCEPT) reads the CURRENT
         iteration start (a possessive capture is non-optimized, so OP_CLOSE uses
         cap_start; its ovector holds only the last COMMITTED iteration). *)
      let cap_ovbase = code.(pc + 1) in
      if cap_ovbase > 0 then
        Array.unsafe_set mb.cap_start cap_ovbase (eptr - mb.start_subject);
      let sp' = push_pos mb sp eptr rdepth code.(pc + 2) in
      (run [@tailcall]) mb (pc + 3) eptr sp' rdepth mcc
  | 47 ->
      (* KETRPOS (fast-design.md §3; OP_KETRPOS pcre2_match.c:5292-5302/6092-6098)
         — a possessive iteration matched: commit it (truncate the body's records)
         and loop back to the body entry, or break on an empty match. *)
      (op_ketrpos [@tailcall]) mb pc eptr mcc
  | 48 ->
      (* POSSESS_DONE (fast-design.md §3; the RM8 loop break + success test
         pcre2_match.c:5320-5328) — the possessive loop ended. Success iff an
         iteration matched or zero repeats are allowed; else the whole group
         fails. No tick (the C's loop `break`). *)
      let base = mb.once_base in
      let nov = 2 * mb.oveccount in
      let d = mb.ss.Save_stack.data in
      (* safe: [base] is a KIND_POS record base; matched_once at base+nov,
         zero_allowed at base+nov+1, prev_once_base at base+nov-2, saved_mark at
         base+nov+3. *)
      let matched_once = Array.unsafe_get d (base + nov) in
      let zero_allowed = Array.unsafe_get d (base + nov + 1) in
      if matched_once <> 0 || zero_allowed <> 0 then (
        (* success: commit — restore once_base, keep mb.mark (the last matched
           iteration's mark persists) and the KIND_POS boundary (its snapshot
           restores captures/mark if the group is later backtracked past). *)
        mb.once_base <- Array.unsafe_get d (base + nov - 2);
        (run [@tailcall]) mb (pc + 1) eptr (base + nov + 5) rdepth mcc)
      else (
        (* failure: restore the snapshot + once_base + mark, propagate NOMATCH. *)
        let ov = mb.ovector in
        for i = 2 to nov - 1 do
          Array.unsafe_set ov i (Array.unsafe_get d (base + i - 2))
        done;
        mb.once_base <- Array.unsafe_get d (base + nov - 2);
        mb.mark <- Array.unsafe_get d (base + nov + 3) (* revert Fmark *);
        (backtrack [@tailcall]) mb base mcc)
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
      then (bt [@tailcall]) mb eptr sp mcc
      else if
        (* pcre2_match.c:897-917 — ENDANCHORED and not at end (op is OP_END
           here, so RRETURN(NOMATCH)). *)
        eptr < mb.end_subject && mb.endanchored
      then (bt [@tailcall]) mb eptr sp mcc
      else
        (* pcre2_match.c:919-940 — record the whole match. The group slots are
           already in place (CAP_END wrote them; the winning path's high-water
           group N is the largest N with a set start slot — equal to the
           interpreter's end_offset_top/2, since a set group completed on the
           surviving path and backtrack-cleanup unset the rest). *)
        record_match mb sm eptr
  | 49 ->
      (* OP_MARK (pcre2_match.c:6340-6342 / interpreter.ml:3322-3333) — set the
         current + nomatch mark to the name offset, push a KIND_VERB (vt_mark)
         that reverts mb.mark on backtrack-past and catches a name-matching
         MATCH_SKIP_ARG (RM12), then RMATCH the continuation. The RMATCH ticks a
         child frame (RM12), so the continuation runs at rdepth+1. *)
      let name_off = code.(pc + 1) in
      let old_mark = mb.mark in
      mb.mark <- name_off;
      mb.nomatch_mark <- name_off;
      let mcc' = tick_child mb mcc (rdepth + 1) in
      if mcc' < 0 then mcc'
      else (
        push_verb mb sp vt_mark name_off eptr old_mark;
        (run [@tailcall]) mb (pc + 2) eptr (sp + Save_stack.width_verb)
          (rdepth + 1) mcc')
  | 50 ->
      (* OP_COMMIT / OP_COMMIT_ARG (pcre2_match.c:6366-6377) — the _ARG form sets
         mark/nomatch_mark (mark_off >= 0). RMATCH the continuation (RM13/RM36
         ticks a child frame); on its exhaustion the KIND_VERB (vt_commit) fires
         MATCH_COMMIT (disable bumpalong). *)
      let mark_off = code.(pc + 1) in
      let old_mark = mb.mark in
      if mark_off >= 0 then (
        mb.mark <- mark_off;
        mb.nomatch_mark <- mark_off);
      let mcc' = tick_child mb mcc (rdepth + 1) in
      if mcc' < 0 then mcc'
      else (
        push_verb mb sp vt_commit 0 eptr old_mark;
        (run [@tailcall]) mb (pc + 2) eptr (sp + Save_stack.width_verb)
          (rdepth + 1) mcc')
  | 51 ->
      (* OP_PRUNE / OP_PRUNE_ARG (pcre2_match.c:6379-6390) — RMATCH the
         continuation (RM14/RM15 tick); fires MATCH_PRUNE (fail to bumpalong). *)
      let mark_off = code.(pc + 1) in
      let old_mark = mb.mark in
      if mark_off >= 0 then (
        mb.mark <- mark_off;
        mb.nomatch_mark <- mark_off);
      let mcc' = tick_child mb mcc (rdepth + 1) in
      if mcc' < 0 then mcc'
      else (
        push_verb mb sp vt_prune 0 eptr old_mark;
        (run [@tailcall]) mb (pc + 2) eptr (sp + Save_stack.width_verb)
          (rdepth + 1) mcc')
  | 52 ->
      (* OP_SKIP (pcre2_match.c:6392-6397) — RMATCH the continuation (RM16 tick);
         fires MATCH_SKIP, passing back the current position (verb_skip_ptr =
         eptr). No mark. *)
      let old_mark = mb.mark in
      let mcc' = tick_child mb mcc (rdepth + 1) in
      if mcc' < 0 then mcc'
      else (
        push_verb mb sp vt_skip 0 eptr old_mark;
        (run [@tailcall]) mb (pc + 1) eptr (sp + Save_stack.width_verb)
          (rdepth + 1) mcc')
  | 53 ->
      (* OP_SKIP_ARG (pcre2_match.c:6407-6424) — the rerun protocol. Count this
         SKIP_ARG; while its count is <= ignore_skip_arg it is a NO-OP (the C's
         `break`: no RMATCH, no tick, same frame). Otherwise RMATCH the
         continuation (RM17 tick) and push a KIND_VERB (vt_skip_arg) that fires
         MATCH_SKIP_ARG (caught by a matching MARK, else it reaches the driver,
         which re-runs with ignore_skip_arg = skip_arg_count). SKIP_ARG does NOT
         set nomatch_mark (Perl compat). *)
      mb.skip_arg_count <- mb.skip_arg_count + 1;
      if mb.skip_arg_count <= mb.ignore_skip_arg then
        (run [@tailcall]) mb (pc + 2) eptr sp rdepth mcc
      else
        let name_off = code.(pc + 1) in
        let old_mark = mb.mark in
        let mcc' = tick_child mb mcc (rdepth + 1) in
        if mcc' < 0 then mcc'
        else (
          push_verb mb sp vt_skip_arg name_off eptr old_mark;
          (run [@tailcall]) mb (pc + 2) eptr (sp + Save_stack.width_verb)
            (rdepth + 1) mcc')
  | 54 ->
      (* OP_THEN / OP_THEN_ARG (pcre2_match.c:6426-6442) — RMATCH the continuation
         (RM18/RM19 tick); fires MATCH_THEN, passing back this opcode's IR pc
         (verb_then_pc) for the enclosing alternation's KIND_ALT scope check. The
         _ARG form sets mark/nomatch_mark. *)
      let mark_off = code.(pc + 1) in
      let old_mark = mb.mark in
      if mark_off >= 0 then (
        mb.mark <- mark_off;
        mb.nomatch_mark <- mark_off);
      let mcc' = tick_child mb mcc (rdepth + 1) in
      if mcc' < 0 then mcc'
      else (
        push_verb mb sp vt_then pc eptr old_mark;
        (run [@tailcall]) mb (pc + 2) eptr (sp + Save_stack.width_verb)
          (rdepth + 1) mcc')
  | 55 ->
      (* OP_ACCEPT (pcre2_match.c:846-940) — chunk K1b: WITHIN a recursion
         (mb.current_recurse >= 0) it is not the whole-match end but a recursion
         RETURN that COMMITS the recursion (pcre2_match.c:847-872); otherwise it
         ends the whole match with the current captures (shares op_end_tail's
         empty-match rejection; ENDANCHORED-and-not-at-end is a DIRECT NOMATCH
         return, pcre2_match.c:916). *)
      if mb.current_recurse >= 0 then
        (accept_recurse_return [@tailcall]) mb eptr rdepth mcc
      else
      let sm = mb.attempt_start in
      if
        Int.equal eptr sm
        && (mb.notempty
           || (mb.notempty_atstart
              && Int.equal sm (mb.start_subject + mb.start_offset)))
      then (bt [@tailcall]) mb eptr sp mcc
      else if eptr < mb.end_subject && mb.endanchored then match_nomatch
      else record_match mb sm eptr
  | 56 ->
      (* OP_CLOSE (pcre2_match.c:809-829) — close an open capture before ACCEPT.
         Push a KIND_CAP so the write is rolled back if the following ACCEPT then
         backtracks (its empty-match rejection). For an optimized capture the
         start is already in ovector[ovb] (CAP_START wrote it); for a referenced
         capture the start is in mb.cap_start[ovb] (written at CAP_START_REF). *)
      let ovb = code.(pc + 1) in
      let referenced = code.(pc + 2) in
      let ss = mb.ss in
      let need = sp + Save_stack.width_cap in
      if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
      let d = ss.Save_stack.data in
      let ov = mb.ovector in
      (* safe: [grow] ensured length >= sp + width_cap; ovb in [2, 2*top]
         (Ir_verify) so ovb, ovb+1 < 2*oveccount and ovb < |cap_start|. *)
      Array.unsafe_set d sp ovb;
      Array.unsafe_set d (sp + 1) (Array.unsafe_get ov ovb);
      Array.unsafe_set d (sp + 2) (Array.unsafe_get ov (ovb + 1));
      Array.unsafe_set d (sp + 3) Save_stack.kind_cap;
      if Int.equal referenced 1 then
        Array.unsafe_set ov ovb (Array.unsafe_get mb.cap_start ovb);
      Array.unsafe_set ov (ovb + 1) (eptr - mb.start_subject);
      (run [@tailcall]) mb (pc + 3) eptr (sp + Save_stack.width_cap) rdepth mcc
  | 63 ->
      (* COND_CREF (fast-design.md §2/§4; OP_CREF pcre2_match.c:5668-5671 /
         interpreter.ml:2506-2514) — numbered-group-set test for a conditional.
         TRUE (group N set: ovector.(ovbase) <> UNSET) falls through to the
         yes-branch; FALSE jumps to [no_target]. No tick, no choice point. *)
      let ovb = code.(pc + 1) in
      (* safe: ovb = 2N, 2 <= ovb < 2*oveccount = Array.length mb.ovector
         (Ir_verify ovbase_ok). *)
      if not (Int.equal (Array.unsafe_get mb.ovector ovb) Frames.unset) then
        (run [@tailcall]) mb (pc + 3) eptr sp rdepth mcc
      else (run [@tailcall]) mb code.(pc + 2) eptr sp rdepth mcc
  | 64 ->
      (* COND_DNCREF (fast-design.md §2/§4; OP_DNCREF pcre2_match.c:5673-5685 /
         interpreter.ml:2515-2524) — duplicate-named-group-set test: TRUE iff any
         group in the name list is set (dncref_test). No tick, no choice point. *)
      if dncref_test mb code.(pc + 2) code.(pc + 1) then
        (run [@tailcall]) mb (pc + 4) eptr sp rdepth mcc
      else (run [@tailcall]) mb code.(pc + 3) eptr sp rdepth mcc
  | 65 ->
      (* COND_FALSE (fast-design.md §2; OP_FALSE/OP_FAIL pcre2_match.c:5687-5689,
         and OP_RREF/OP_DNRREF which are always false without recursion,
         :5645-5666) — the condition is always false: jump to [no_target].
         (?(DEFINE)…) rides this arm (its body is the never-taken yes-branch).
         No tick, no choice point. *)
      (run [@tailcall]) mb code.(pc + 1) eptr sp rdepth mcc
  | 66 ->
      (* COND_ASSERT (fast-design.md §3; the assertion-condition entry
         pcre2_match.c:5701-5708 / interpreter.ml:2534-2548) — push a
         KIND_NASSERT boundary whose [cont] = [nomatch_target] (the branch taken
         when the assertion body does NOT match), set mb.once_base, fall into the
         assertion body (grouploop-style ALT per branch). No tick (the first
         branch's ALT ticks — the C's RM5). Exhausting all branches backtracks to
         the boundary -> backtrack_nassert -> [nomatch_target]; a matching branch
         reaches t_cond_assert_match. *)
      let nomatch_target = code.(pc + 1) in
      let sp' = push_nassert mb sp nomatch_target eptr rdepth in
      (run [@tailcall]) mb (pc + 2) eptr sp' rdepth mcc
  | 67 ->
      (* COND_ASSERT_MATCH (fast-design.md §3; the assertion-condition MATCH
         action pcre2_match.c:5722-5724 + the ket memcpy :5934-5948 /
         interpreter.ml:2685-2716,7733-7736) — an assertion branch matched, so
         the condition is Lpositive: COMMIT the assertion (its captures persist —
         they are already in the shared ovector), restore eptr/rdepth to the
         conditional entry, and jump to [match_target]. The KIND_NASSERT boundary
         is CONVERTED to a KIND_ONCE so a later backtrack PAST the whole
         conditional rolls back the snapshot (rather than re-taking the nomatch
         branch). No tick (the C RRETURNs to the OP_COND frame). *)
      (* Chunk K1b — the C's conditional-assertion ket is RRETURN(MATCH_MATCH)
         (pcre2_match.c:5947), so RETURN_SWITCH (:6470) records the assertion
         body's reached Feptr in last_used_ptr (the RECURSELOOP input) BEFORE
         eptr is put back to the conditional entry — record it here (this arm
         is the fast MATCH-return), gated on has_recurse. *)
      if mb.has_recurse && eptr > mb.last_used_ptr then mb.last_used_ptr <- eptr;
      let match_target = code.(pc + 1) in
      let base = mb.once_base in
      let nov = 2 * mb.oveccount in
      let d = mb.ss.Save_stack.data in
      (* safe: [base] is the KIND_NASSERT boundary base (set by push_nassert via
         t_cond_assert): snapshot [base, base+nov-2), prev_once_base at base+nov-2,
         cont/eptr/rdepth at base+nov-1..base+nov+1, saved_mark at base+nov+2. *)
      let prev = Array.unsafe_get d (base + nov - 2) in
      let entry_eptr = Array.unsafe_get d (base + nov) in
      let entry_rdepth = Array.unsafe_get d (base + nov + 1) in
      let saved_mark = Array.unsafe_get d (base + nov + 2) in
      (* Convert KIND_NASSERT (width nov+4) -> KIND_ONCE (width nov+3): the
         snapshot + prev_once_base slots are identical; rewrite the tail
         (saved_mark at nov-1; the NASSERT's eptr_enter at nov is ALREADY the
         KIND_ONCE entry_eptr slot — chunk K1b; subtype = atomic group; kind)
         and truncate to the boundary. mb.mark is KEPT (the mark set inside the
         matched assertion persists, the C's P->mark = F->mark); the converted
         KIND_ONCE's saved_mark restores the entry mark only if the conditional
         is later backtracked past. *)
      Array.unsafe_set d (base + nov - 1) saved_mark;
      Array.unsafe_set d (base + nov + 1) Ir.once_group;
      Array.unsafe_set d (base + nov + 2) Save_stack.kind_once;
      mb.once_base <- prev;
      (run [@tailcall]) mb match_target entry_eptr (base + nov + 3) entry_rdepth
        mcc
  | 68 ->
      (* SCOND_DESCEND (fast-design.md §4; the OP_SCOND descend RM35
         pcre2_match.c:5772-5776) — a repeated conditional that might match empty
         descends one virtual frame per iteration so the empty-string loop check
         has the iteration start (the C's GF_NOCAPTURE RMATCH(RM35)). One tick
         (like a single-branch grouploop's entry tick), no choice point. *)
      let mcc' = tick_child mb mcc (rdepth + 1) in
      if mcc' < 0 then mcc'
      else (run [@tailcall]) mb (pc + 1) eptr sp (rdepth + 1) mcc'
  | 69 ->
      (* SCRIPT_RUN_END (fast-design.md §2/§3; the OP_SCRIPT_RUN ket
         pcre2_match.c:6045-6051 / interpreter.ml:2882-2897) — a script-run
         branch matched: apply the script-checking rules to the group's matched
         span [group_start.(g), eptr). Feptr is the current position; P->eptr is
         the entry, recorded in mb.group_start.(g) by the preceding
         t_group_start. On failure backtrack (NOMATCH); else continue at the
         current (advanced) eptr. Non-atomic: the body's choice points stay live
         (a failed continuation re-enters the branches and re-checks the new
         span). No tick (the C's ket continues in the matching branch's frame). *)
      let g = code.(pc + 1) in
      (* safe: g in [0, n_groups) (Ir_verify) indexes mb.group_start. *)
      let s = Array.unsafe_get mb.group_start g in
      if Script_run.script_run mb.subject s eptr mb.utf then
        (run [@tailcall]) mb (pc + 2) eptr sp rdepth mcc
      else (bt [@tailcall]) mb eptr sp mcc
  | 70 ->
      (* RECURSE (chunk K1b, fast-design.md §3; OP_RECURSE pcre2_match.c:5427-5497)
         — the subroutine call. If already inside a recursion, run the RECURSELOOP
         check (pcre2_match.c:5438-5453): repeating the SAME group at the SAME
         eptr with last_used_ptr unchanged is PCRE2_ERROR_RECURSELOOP (a DIRECT
         return, not a backtrack). Otherwise push the FAT KIND_RECURSE boundary
         (snapshots + sets mb.current_recurse = number / mb.recurse_base) and jump
         to the group's first-branch entry; its grouploop ALTs tick per branch =
         the C's RM11 (§4). No tick in this arm (the branch ALT ticks). *)
      let number = code.(pc + 1) in
      let entry_pc = code.(pc + 2) in
      if mb.current_recurse >= 0 && recurse_loop_check mb number eptr then
        Errors.error_recurseloop
      else
        let sp' = push_recurse mb sp number eptr (pc + 3) in
        (run [@tailcall]) mb entry_pc eptr sp' rdepth mcc
  | 71 ->
      (* COND_RREF (chunk K1b; OP_RREF pcre2_match.c:5645-5651) — recursion test
         for a conditional: TRUE iff inside a recursion (mb.current_recurse >= 0)
         AND (number = RREF_ANY or number = mb.current_recurse). TRUE falls through
         to the yes-branch; FALSE jumps to [no_target]. No tick, no choice point. *)
      let number = code.(pc + 1) in
      if
        mb.current_recurse >= 0
        && (Int.equal number Ir.rref_any
           || Int.equal number mb.current_recurse)
      then (run [@tailcall]) mb (pc + 3) eptr sp rdepth mcc
      else (run [@tailcall]) mb code.(pc + 2) eptr sp rdepth mcc
  | 72 ->
      (* COND_DNRREF (chunk K1b; OP_DNRREF pcre2_match.c:5653-5666) — duplicate-
         named recursion test: TRUE iff inside a recursion AND any group in the
         name list equals mb.current_recurse (dnrref_test). No tick. *)
      if mb.current_recurse >= 0 && dnrref_test mb code.(pc + 2) code.(pc + 1)
      then (run [@tailcall]) mb (pc + 4) eptr sp rdepth mcc
      else (run [@tailcall]) mb code.(pc + 3) eptr sp rdepth mcc
  | _ ->
      (* Ir_verify rejects any other tag before the runner sees it; a compiled
         Ir.t cannot reach here (fast-design.md §2). *)
      Errors.error_internal

and bt (mb : mb) (eptr : int) (sp : int) (mcc : int) : int =
  (* Chunk K1b — the forward-to-backtrack transition IS the C's per-frame RRETURN
     (pcre2_match.c:6470 RETURN_SWITCH): record the failing position [eptr] in
     mb.last_used_ptr (the RECURSELOOP input, a monotone max), then hand off to
     the ordinary [backtrack]. Gated on has_recurse — only recursive patterns
     read last_used_ptr, so non-recursive matches pay one predictable branch on
     the (already non-hottest) backtrack path. The deepest forward failure of an
     unwind carries the largest eptr, so recording at these transitions matches
     the C's max-over-RRETURNs exactly (the shallower chained returns add no new
     maximum); char runs are de-fused for has_recurse patterns so a mismatch's
     eptr equals the C's per-OP_CHAR Feptr. *)
  if mb.has_recurse && eptr > mb.last_used_ptr then mb.last_used_ptr <- eptr;
  (backtrack [@tailcall]) mb sp mcc

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
    if Int.equal kind Save_stack.kind_alt then
      (* KIND_ALT [handler; eptr; rdepth; then_end; kind] — resume the next
         alternative (shared with the THEN convert in [backtrack_code]). *)
      (resume_alt [@tailcall]) mb (sp - Save_stack.width_alt) mcc
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
    else if Int.equal kind Save_stack.kind_capstart then (
      (* KIND_CAPSTART [ovbase; old_cap_start; kind] (fast-design.md §3; the JIT
         non-optimized cbracket entry restore, pcre2_jit_compile.c:13454-13459)
         — restore a referenced capture's private in-progress start to the
         enclosing value, then keep popping. The ovector was already rolled back
         by the KIND_CAP pushed at CLOSE (popped earlier in this unwind). *)
      let base = sp - Save_stack.width_capstart in
      let ovb = Array.unsafe_get d base in
      (* safe: ovb written by CAP_START_REF, < Array.length mb.cap_start. *)
      Array.unsafe_set mb.cap_start ovb (Array.unsafe_get d (base + 1));
      (backtrack [@tailcall]) mb base mcc)
    else if Int.equal kind Save_stack.kind_ref_min then
      (* KIND_REF_MIN [rep_pc; count; eptr; rdepth; kind] — minimizing ref
         repeat give-one-more (RM20, pcre2_match.c:5097-5113). *)
      (backtrack_ref_min [@tailcall]) mb sp mcc
    else if Int.equal kind Save_stack.kind_ref_max then
      (* KIND_REF_MAX [cont; try_eptr; flength; lstart; rdepth; kind] —
         maximizing ref repeat give-back (RM21, pcre2_match.c:5154-5162). *)
      (backtrack_ref_max [@tailcall]) mb sp mcc
    else if Int.equal kind Save_stack.kind_ref_max2 then
      (* KIND_REF_MAX2 [ref_pc; lmax_cur; try_eptr; lstart; rdepth; kind] —
         differing-lengths maximizing ref repeat (RM22, chunk I2,
         pcre2_match.c:5172-5184). *)
      (backtrack_ref_max2 [@tailcall]) mb sp mcc
    else if Int.equal kind Save_stack.kind_once then
      (* KIND_ONCE — the atomic construct is backtracked PAST (its body failed,
         or it committed and the continuation later failed): restore the group
         ovector snapshot + mb.once_base, then keep popping (fast-design.md §3;
         the C frame arena's wholesale restore of P's ovector/eptr past OP_ONCE,
         pcre2_match.c:6023-6031). *)
      (backtrack_once [@tailcall]) mb sp mcc
    else if Int.equal kind Save_stack.kind_nassert then
      (* KIND_NASSERT — ALL branches of a negative assertion failed = the
         assertion SUCCEEDS (pcre2_match.c:5578-5584 ASSERT_NOT_FAILED): restore
         mb.once_base + the snapshot and continue at the stored [cont] with the
         entry eptr/rdepth. *)
      (backtrack_nassert [@tailcall]) mb sp mcc
    else if Int.equal kind Save_stack.kind_vreverse then
      (* KIND_VREVERSE — a variable-lookbehind branch failed at this back-length
         (RM37, pcre2_match.c:5877-5882): give up one back-step and retry. *)
      (backtrack_vreverse [@tailcall]) mb sp mcc
    else if Int.equal kind Save_stack.kind_pos then
      (* KIND_POS — the possessive group is backtracked past (like KIND_ONCE). *)
      (backtrack_pos [@tailcall]) mb sp mcc
    else if Int.equal kind Save_stack.kind_verb then (
      (* KIND_VERB [vtype; aux; eptr; old_mark; kind] (chunk H, fast-design.md
         §3) — the verb's continuation exhausted (NOMATCH). Revert mb.mark to the
         enclosing value (the C's per-frame Fmark restore on unwind), then FIRE
         the verb code into [backtrack_code] (or, for MARK, just keep
         backtracking — a MARK itself has no failure action, pcre2_match.c:6357
         `RRETURN(rrc)` with rrc = NOMATCH). *)
      let base = sp - Save_stack.width_verb in
      let vtype = Array.unsafe_get d base in
      let aux = Array.unsafe_get d (base + 1) in
      let veptr = Array.unsafe_get d (base + 2) in
      mb.mark <- Array.unsafe_get d (base + 3);
      if Int.equal vtype vt_mark then (backtrack [@tailcall]) mb base mcc
      else (
        (* Chunk K1b — record the recursion the firing verb is in
           (pcre2_match.c:6369): mb.current_recurse at the KIND_VERB pop equals
           its value when the verb was DISPATCHED (all of the continuation's
           recursion records, below-none/above-this, have restored it by now), so
           it is mb->verb_current_recurse. Read at a KIND_RECURSE boundary to
           decide whether the verb NOMATCHes the recursion (contained) or passes
           through (pcre2_match.c:5481-5488). *)
        mb.verb_current_recurse <- mb.current_recurse;
        if Int.equal vtype vt_prune then
          (backtrack_code [@tailcall]) mb base mcc match_prune
        else if Int.equal vtype vt_commit then
          (backtrack_code [@tailcall]) mb base mcc match_commit
        else if Int.equal vtype vt_skip then (
          mb.verb_skip_ptr <- veptr (* pass back current position, 6395 *);
          (backtrack_code [@tailcall]) mb base mcc match_skip)
        else if Int.equal vtype vt_skip_arg then (
          mb.verb_skip_ptr <- aux (* pass back the skip name offset, 6422 *);
          (backtrack_code [@tailcall]) mb base mcc match_skip_arg)
        else (
          (* vt_then: pass back this THEN's IR pc (6432). *)
          mb.verb_then_pc <- aux;
          (backtrack_code [@tailcall]) mb base mcc match_then)))
    else if Int.equal kind Save_stack.kind_recurse then (
      (* KIND_RECURSE — the recursion is backtracked PAST (all its branches
         failed): restore the FAT snapshot + boundary state, then keep popping
         (the C frame arena discarding the recursion's frames, pcre2_match.c:6470
         RRETURN past OP_RECURSE). Chunk K1b — the C's branch exhaustion is
         RRETURN(MATCH_NOMATCH) from the OP_RECURSE frame at the CALL position
         (:5495): record call_eptr (usually already recorded by the recursed
         group's FAIL head, but exact either way — last_used is a monotone max). *)
      let base = sp - recurse_width mb in
      let call_eptr = Array.unsafe_get d (base + recurse_tail_off mb + 5) in
      if mb.has_recurse && call_eptr > mb.last_used_ptr then
        mb.last_used_ptr <- call_eptr;
      restore_recurse mb base;
      (backtrack [@tailcall]) mb base mcc)
    else if Int.equal kind Save_stack.kind_recurse_ret then (
      (* KIND_RECURSE_RET — backtrack INTO a completed recursion body: restore the
         POST-body arrays (ovector + group_start + cap_start) + mb.current_recurse
         / recurse_base, then keep popping (the body's records retry its
         branches). *)
      let base = sp - recurse_ret_width mb in
      let g = base + recurse_tail_off mb in
      restore_arrays mb d base;
      mb.current_recurse <- Array.unsafe_get d g;
      mb.recurse_base <- Array.unsafe_get d (g + 1);
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

(* Resume the next alternative of a KIND_ALT record at [base] (fast-design.md
   §3/§4). Shared by the NOMATCH KIND_ALT backtrack and the THEN convert in
   [backtrack_code]: pop the record, and — with the §4 top-level tick heuristic —
   run its handler at the saved eptr/rdepth. *)
and resume_alt (mb : mb) (base : int) (mcc : int) : int =
  let d = mb.ss.Save_stack.data in
  let handler = Array.unsafe_get d base in
  let e = Array.unsafe_get d (base + 1) in
  let dsaved = Array.unsafe_get d (base + 2) in
  let code = mb.code in
  if
    Int.equal dsaved 0
    && (not (Int.equal code.(handler) 5 (* ALT *)))
    && (not (Int.equal code.(handler) 15 (* FAIL *)))
    && (not (Int.equal code.(handler) 73 (* FAIL_NASSERT — as FAIL (chunk K1b) *)))
    && not
         (Int.equal code.(handler) 48 (* POSSESS_DONE — a loop break, not a
            ticked branch (chunk G); exclude so a dsaved=0 last-ALT handler
            cannot spuriously tick *))
  then
    (* §4 — the whole-pattern wrapper's LAST branch is reached via the preceding
       ALT's handler (a non-ALT, non-FAIL head at controlling depth 0):
       grouploop ticks it at rdepth 1. A grouploop group's FAIL head runs at
       dsaved >= 1 (nested in the wrapper), so this tick never fires for it. *)
    let mcc' = tick_child mb mcc 1 in
    if mcc' < 0 then mcc' else (run [@tailcall]) mb handler e base 1 mcc'
  else (run [@tailcall]) mb handler e base dsaved mcc

(* Chunk K1b — a recursion RETURN (the recursed group's ket / an inner ( *ACCEPT)
   detected mb.current_recurse == number, pcre2_match.c:6065-6074). The
   recursion body's captures do NOT escape: restore the ovector to the pre-call
   snapshot in the FAT record going FORWARD, but first push a KIND_RECURSE_RET
   saving the POST-body ovector so a later backtrack INTO the body re-establishes
   it (the C's body frames keep their own ovector below the returning frame).
   Restore current_recurse / recurse_base to the enclosing values and continue
   past the call ([cont_pc]) at the body's rdepth (the C's `continue` in the same
   frame). mb.mark is KEPT (the recursion's mark persists — the C never restores
   Fmark at the ket-return). No tick. *)
and recurse_return (mb : mb) (eptr : int) (sp : int) (rdepth : int) (mcc : int) :
    int =
  let rb = mb.recurse_base in
  let toff = recurse_tail_off mb in
  let ss = mb.ss in
  let f = rb + toff in
  let number = ss.Save_stack.data.(f + 4) in
  let cont_pc = ss.Save_stack.data.(f + 7) in
  let prev_cr = ss.Save_stack.data.(f + 1) in
  let prev_rb = ss.Save_stack.data.(f + 2) in
  let need = sp + recurse_ret_width mb in
  if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
  let d = ss.Save_stack.data in
  (* Save the POST-body arrays (ovector + group_start + cap_start) for a
     backtrack INTO the body; then restore the PRE-CALL arrays from the FAT
     snapshot (preserved across the grow) — captures do not escape a recursion,
     and the enclosing construct's group_start/cap_start must be reinstated. *)
  snapshot_arrays mb d sp;
  let g = sp + toff in
  d.(g) <- number;
  d.(g + 1) <- rb;
  d.(g + 2) <- Save_stack.kind_recurse_ret;
  restore_arrays mb d rb;
  mb.current_recurse <- prev_cr;
  mb.recurse_base <- prev_rb;
  (run [@tailcall]) mb cont_pc eptr need rdepth mcc

(* Chunk K1b — ( *ACCEPT) inside a recursion (pcre2_match.c:847-872): unlike the
   ket-return this COMMITS the recursion (the C's F = P discards the body frames).
   Restore the pre-call captures + boundary state (once_base / current_recurse /
   recurse_base) from the FAT record, truncate the save stack to the FAT record's
   base (discard the body's choice points AND the FAT record itself — the
   recursion has no more alternatives), and continue past the call at [cont_pc]
   with eptr = the ACCEPT position. mb.mark is KEPT (the C copies Fmark to P->mark,
   :868). No empty/ENDANCHORED test (that is the fall-through top-level ACCEPT). *)
and accept_recurse_return (mb : mb) (eptr : int) (rdepth : int) (mcc : int) : int
    =
  let rb = mb.recurse_base in
  let d = mb.ss.Save_stack.data in
  let f = rb + recurse_tail_off mb in
  let cont_pc = d.(f + 7) in
  (* Restore the pre-call arrays (ovector + group_start + cap_start) + once_base /
     current_recurse / recurse_base; truncate to the FAT record base (discard the
     body's choice points AND the record — the recursion has no more branches). *)
  restore_arrays mb d rb;
  mb.once_base <- d.(f + 0);
  mb.current_recurse <- d.(f + 1);
  mb.recurse_base <- d.(f + 2);
  (run [@tailcall]) mb cont_pc eptr rb rdepth mcc

(* Propagate a backtracking verb code [vcode] (MATCH_COMMIT..MATCH_THEN) up the
   save stack (chunk H, fast-design.md §3). Mirrors the C's RRETURN of a verb
   code through the frame stack (each frame's RM arm does `if rrc != MATCH_NOMATCH
   RRETURN(rrc)` — pass it up — except the THEN scope check and the assertion /
   atomic boundaries). Cold: only reached once a verb fires. Choice-point records
   (ALT/CONT/REP*/REF*/VREVERSE) are DISCARDED (the verb skips them, unlike a
   NOMATCH which would retry); restore-only records (CAP/GSTART/CAPSTART) run their
   restore then keep propagating (the C's frame unwind); boundaries handle the
   code per construct. Reaching sp = 0 returns the code to the driver. *)
and backtrack_code (mb : mb) (sp : int) (mcc : int) (vcode : int) : int =
  if sp <= 0 then vcode
  else
    let d = mb.ss.Save_stack.data in
    let kind = Array.unsafe_get d (sp - 1) in
    if Int.equal kind Save_stack.kind_alt then (
      (* KIND_ALT: THEN is scoped to the innermost enclosing >= 2-branch
         alternation branch (pcre2_match.c:5401-5407); convert it to NOMATCH
         (resume the next alternative) iff the THEN's pc is within this branch
         (< then_end, which is -1 for a single-branch group). Every other verb
         code passes up (discards this choice point). *)
      let base = sp - Save_stack.width_alt in
      if
        Int.equal vcode match_then
        && mb.verb_then_pc < Array.unsafe_get d (base + 3)
      then (resume_alt [@tailcall]) mb base mcc
      else (backtrack_code [@tailcall]) mb base mcc vcode)
    else if Int.equal kind Save_stack.kind_cap then (
      (* Restore the capture's ovector pair (the frame unwind's abandon) and keep
         propagating. *)
      let base = sp - Save_stack.width_cap in
      let ovb = Array.unsafe_get d base in
      let ov = mb.ovector in
      Array.unsafe_set ov ovb (Array.unsafe_get d (base + 1));
      Array.unsafe_set ov (ovb + 1) (Array.unsafe_get d (base + 2));
      (backtrack_code [@tailcall]) mb base mcc vcode)
    else if Int.equal kind Save_stack.kind_gstart then (
      let base = sp - Save_stack.width_gstart in
      Array.unsafe_set mb.group_start
        (Array.unsafe_get d base)
        (Array.unsafe_get d (base + 1));
      (backtrack_code [@tailcall]) mb base mcc vcode)
    else if Int.equal kind Save_stack.kind_capstart then (
      let base = sp - Save_stack.width_capstart in
      Array.unsafe_set mb.cap_start
        (Array.unsafe_get d base)
        (Array.unsafe_get d (base + 1));
      (backtrack_code [@tailcall]) mb base mcc vcode)
    else if Int.equal kind Save_stack.kind_verb then (
      (* A verb's own record on the propagation path (RRETURN passes rrc up). MARK
         reverts its mark and catches a name-matching MATCH_SKIP_ARG, converting
         it to MATCH_SKIP with the position passed back (RM12,
         pcre2_match.c:6351-6356); every other case reverts mark and passes up. *)
      let base = sp - Save_stack.width_verb in
      let vtype = Array.unsafe_get d base in
      let aux = Array.unsafe_get d (base + 1) in
      let veptr = Array.unsafe_get d (base + 2) in
      mb.mark <- Array.unsafe_get d (base + 3);
      if
        Int.equal vtype vt_mark
        && Int.equal vcode match_skip_arg
        && strcmp_code_eq mb aux mb.verb_skip_ptr
      then (
        mb.verb_skip_ptr <- veptr;
        (backtrack_code [@tailcall]) mb base mcc match_skip)
      else (backtrack_code [@tailcall]) mb base mcc vcode)
    else if Int.equal kind Save_stack.kind_once then (
      (* Atomic group / atomic positive assertion boundary. Restore the
         group-ovector snapshot + once_base + mark (as backtrack_once). Then:
         COMMIT/SKIP/PRUNE ESCAPE (pcre2_match.c:5408 RM2 / 5529 RM3
         `RRETURN(rrc)`); THEN ESCAPES an atomic GROUP (RM2's position scope check
         fails for a THEN at/after the group) but is CONTAINED for a positive
         ASSERTION (RM3 treats THEN as NOMATCH, pcre2_match.c:5504-5507/5529) —
         i.e. converted to NOMATCH so backtracking continues BELOW the boundary
         (the assertion "fails"). *)
      let nov = 2 * mb.oveccount in
      let base = sp - (nov + 3) in
      let ov = mb.ovector in
      for i = 2 to nov - 1 do
        Array.unsafe_set ov i (Array.unsafe_get d (base + i - 2))
      done;
      mb.once_base <- Array.unsafe_get d (base + nov - 2);
      mb.mark <- Array.unsafe_get d (base + nov - 1);
      (* Chunk K1b — a verb code passing this boundary is an RRETURN(rrc) FROM
         the construct's frame in the C (:5408 RM2 / :5529 RM3), so
         RETURN_SWITCH (:6470) records the construct's ENTRY Feptr — which can
         EXCEED the verb-fire position when the verb fired inside a LOOKBEHIND
         body (rewound below entry). Record the stored entry_eptr; also for the
         THEN-contained pos-assert case (its committed-body exhaustion is
         RRETURN(MATCH_NOMATCH) at the entry, :5531). Gated on has_recurse. *)
      let entry_eptr = Array.unsafe_get d (base + nov) in
      if mb.has_recurse && entry_eptr > mb.last_used_ptr then
        mb.last_used_ptr <- entry_eptr;
      if
        Int.equal vcode match_then
        && Int.equal (Array.unsafe_get d (base + nov + 1)) Ir.once_pos_assert
      then (backtrack [@tailcall]) mb base mcc (* THEN contained: NOMATCH below *)
      else (backtrack_code [@tailcall]) mb base mcc vcode)
    else if Int.equal kind Save_stack.kind_pos then (
      (* Possessive bracket boundary: verb codes escape (RM8 `RRETURN(rrc)`,
         pcre2_match.c:5315); THEN escapes too (a possessive group is not an
         assertion). Restore the snapshot + once_base + mark (as backtrack_pos). *)
      let nov = 2 * mb.oveccount in
      let base = sp - (nov + 5) in
      let ov = mb.ovector in
      for i = 2 to nov - 1 do
        Array.unsafe_set ov i (Array.unsafe_get d (base + i - 2))
      done;
      mb.once_base <- Array.unsafe_get d (base + nov - 2);
      mb.mark <- Array.unsafe_get d (base + nov + 3);
      (* Chunk K1b — the RM8 pass-up RRETURN(rrc) (:5315) is executed by the
         bracket frame whose Feptr = the CURRENT iteration start (the KETRPOS
         frame copy-back, :6094-6096): record the stored iter_start. *)
      let iter_start = Array.unsafe_get d (base + nov - 1) in
      if mb.has_recurse && iter_start > mb.last_used_ptr then
        mb.last_used_ptr <- iter_start;
      (backtrack_code [@tailcall]) mb base mcc vcode)
    else if Int.equal kind Save_stack.kind_nassert then
      if Int.equal vcode match_skip_arg then (
        (* pcre2_match.c:5573-5574 RM4 `default: RRETURN(rrc)` — MATCH_SKIP_ARG is
           NOT one of the codes that force a negative assertion to succeed (only
           COMMIT/SKIP/PRUNE are, at 5567-5571); it ESCAPES the assertion (to a
           matching MARK or the driver's rerun protocol). Restore the snapshot +
           once_base + mark (abandon the assertion body) and keep propagating. *)
        let nov = 2 * mb.oveccount in
        let base = sp - (nov + 4) in
        let ov = mb.ovector in
        for i = 2 to nov - 1 do
          Array.unsafe_set ov i (Array.unsafe_get d (base + i - 2))
        done;
        mb.once_base <- Array.unsafe_get d (base + nov - 2);
        mb.mark <- Array.unsafe_get d (base + nov + 2);
        (* Chunk K1b — this escape IS an RRETURN from the assertion frame
           (:5574), so it records the assertion's entry Feptr (stored as
           eptr_enter at base + nov). *)
        let entry_eptr = Array.unsafe_get d (base + nov) in
        if mb.has_recurse && entry_eptr > mb.last_used_ptr then
          mb.last_used_ptr <- entry_eptr;
        (backtrack_code [@tailcall]) mb base mcc vcode)
      else
        (* COMMIT/SKIP/PRUNE force the assertion to fail = the negative assertion
           SUCCEEDS (pcre2_match.c:5567-5571 RM4); THEN is treated as NOMATCH = no
           more branches = also SUCCESS (5561-5565 + ASSERT_NOT_FAILED). Each does
           the SUCCESS action (continue at cont with the entry eptr/rdepth),
           consuming the code — identical to the all-branches-failed backtrack.
           Chunk K1b — NO last_used record here: both containment paths are
           SAME-FRAME transfers in the C (the :5570 `goto ASSERT_NOT_FAILED` and
           the :5563-5564 branch walk), with no RRETURN — recording the entry
           would over-approximate (verified: the C never records the OP_ASSERT_NOT
           frame's Feptr on containment). *)
        (backtrack_nassert [@tailcall]) mb sp mcc
    else if Int.equal kind Save_stack.kind_vreverse then
      (backtrack_code [@tailcall]) mb (sp - Save_stack.width_vreverse) mcc vcode
    else if Int.equal kind Save_stack.kind_rep_max then
      (backtrack_code [@tailcall]) mb (sp - Save_stack.width_rep_max) mcc vcode
    else if Int.equal kind Save_stack.kind_rep_min then
      (backtrack_code [@tailcall]) mb (sp - Save_stack.width_rep_min) mcc vcode
    else if Int.equal kind Save_stack.kind_ref_min then
      (backtrack_code [@tailcall]) mb (sp - Save_stack.width_ref_min) mcc vcode
    else if Int.equal kind Save_stack.kind_ref_max then
      (backtrack_code [@tailcall]) mb (sp - Save_stack.width_ref_max) mcc vcode
    else if Int.equal kind Save_stack.kind_ref_max2 then
      (backtrack_code [@tailcall]) mb (sp - Save_stack.width_ref_max2) mcc vcode
    else if Int.equal kind Save_stack.kind_recurse then (
      (* KIND_RECURSE — a verb code reaching the recursion boundary
         (pcre2_match.c:5481-5488). Restore the FAT snapshot + boundary state
         (abandon the recursion's captures, the C's RRETURN past OP_RECURSE). A
         verb that fired WITHIN this recursion (verb_current_recurse == number)
         does NOT escape — it makes the whole recursion NOMATCH (RRETURN(NOMATCH),
         5487): convert to a plain NOMATCH backtrack below the boundary. A verb
         from OUTSIDE this recursion passes through unchanged (5493). *)
      let base = sp - recurse_width mb in
      let number = Array.unsafe_get d (base + recurse_tail_off mb + 4) in
      (* Chunk K1b — both the contained (:5487) and pass-through (:5493) cases
         are RRETURNs from the OP_RECURSE frame at the CALL position: record
         call_eptr (the verb path DISCARDS the group's FAIL head, so this is
         the only record of it here). *)
      let call_eptr = Array.unsafe_get d (base + recurse_tail_off mb + 5) in
      if mb.has_recurse && call_eptr > mb.last_used_ptr then
        mb.last_used_ptr <- call_eptr;
      restore_recurse mb base;
      if Int.equal mb.verb_current_recurse number then
        (backtrack [@tailcall]) mb base mcc
      else (backtrack_code [@tailcall]) mb base mcc vcode)
    else if Int.equal kind Save_stack.kind_recurse_ret then (
      (* KIND_RECURSE_RET — a verb propagating past a COMPLETED recursion body:
         restore the POST-body arrays + current_recurse/recurse_base (enter the
         body context) then keep propagating; the body records + the FAT record
         below revert its captures as the code passes through. *)
      let base = sp - recurse_ret_width mb in
      let g = base + recurse_tail_off mb in
      restore_arrays mb d base;
      mb.current_recurse <- Array.unsafe_get d g;
      mb.recurse_base <- Array.unsafe_get d (g + 1);
      (backtrack_code [@tailcall]) mb base mcc vcode)
    else
      (* KIND_CONT — a group choice point; discard and keep propagating. *)
      (backtrack_code [@tailcall]) mb (sp - Save_stack.width_cont) mcc vcode

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
  if
    Int.equal mb.code.(rep_pc) 31 (* t_class_rep *)
    || Int.equal mb.code.(rep_pc) 58 (* t_xclass_rep *)
  then
    (* CLASS (pcre2_match.c:2143-2148, RM24/RM201) / XCLASS (:2278-2288,
       RM101): [floor] is itself a ticked RMATCH; below floor -> NOMATCH. *)
    if try_pos >= floor then (
      let mcc' = tick_child mb mcc (dd + 1) in
      if mcc' < 0 then mcc'
      else (
        (* one char (UTF) / code unit back; at try_pos = floor stores floor-1. *)
        Array.unsafe_set d (base + 1) (rep_giveback mb rep_pc floor try_pos);
        (run [@tailcall]) mb cont try_pos sp (dd + 1) mcc'))
    else (backtrack [@tailcall]) mb base mcc (* pop, propagate *)
  else if try_pos > floor then (
    (* char / type / \R / \X / property: another give-back position (tick,
       rdepth d+1). Store the NEXT give-back position (one char in UTF, with
       the \R mid-CRLF correction; one CLUSTER for \X). *)
    let mcc' = tick_child mb mcc (dd + 1) in
    if mcc' < 0 then mcc'
    else (
      Array.unsafe_set d (base + 1) (rep_giveback mb rep_pc floor try_pos);
      (run [@tailcall]) mb cont try_pos sp (dd + 1) mcc'))
  else
    (* try_pos <= floor: the minimum-length position, tried IN PLACE at rdepth
       d, no tick; pop the record permanently. try_pos = floor everywhere
       except a \C repeat in UTF (chunk I2), whose char-wise BACKCHAR
       give-back can land BELOW a mid-character floor — the C then dispatches
       at that below-floor Feptr (the `<=` loop-head break,
       pcre2_match.c:4935-4940), so the continuation runs at [try_pos], not
       [floor]. *)
    (run [@tailcall]) mb cont try_pos base dd mcc

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
  if count >= mb.rep_lmax then
    (* Chunk K1b — the C's count exhaustion is `if (Lmin++ >= Lmax)
       RRETURN(MATCH_NOMATCH)` (pcre2_match.c:1483/3967 etc.), an RRETURN from
       the repeat frame at the current extend position: record [eptr]. *)
    (bt [@tailcall]) mb eptr base mcc
  else
    let k = mb.rep_kind in
    if Int.equal k rk_any then
      (rep_bt_min_any [@tailcall]) mb base count eptr dd mcc
    else if Int.equal k rk_anynl then
      (rep_bt_min_anynl [@tailcall]) mb base count eptr dd mcc
    else if Int.equal k rk_extuni then
      (* Chunk I2 — \X minimize extend (RM218, pcre2_match.c:3807-3827):
         bound check + SCHECK_PARTIAL, one cluster step, CHECK_PARTIAL, then
         retry the continuation (tick). *)
      if eptr >= mb.end_subject then
        let r = scheck_partial mb eptr in
        if r < 0 then r else (bt [@tailcall]) mb eptr base mcc
      else
        let e' = extuni_at mb eptr in
        let r = if e' >= mb.end_subject then scheck_partial mb e' else 0 in
        if r < 0 then r
        else
          let mcc' = tick_child mb mcc (dd + 1) in
          if mcc' < 0 then mcc'
          else (
            Array.unsafe_set d (base + 1) (count + 1);
            Array.unsafe_set d (base + 2) e';
            (run [@tailcall]) mb mb.rep_cont e' sp (dd + 1) mcc')
    else if eptr >= mb.end_subject then
      let r = scheck_partial mb eptr in
      if r < 0 then r else (bt [@tailcall]) mb eptr base mcc
    else if mb.utf then
      (* Chunk I — match one more CHARACTER: the UTF minimize resume (RM219,
         pcre2_match.c:3836-3959) GETCHARINCs a full character for EVERY type
         — including OP_ALLANY and OP_ANYBYTE (:3862-3864), even though \C's
         forward min loop is byte-wise; the per-predicate kinds use
         rep_unit_utf. *)
      let l =
        if Int.equal k rk_allany || Int.equal k rk_anybyte then cp_len mb eptr
        else rep_unit_utf mb eptr
      in
      if Int.equal l 0 then
        (* Chunk K1b — record the C-exact mismatch position (minimize=true). *)
        (bt [@tailcall]) mb (rep_fail_pos mb eptr true) base mcc
      else
        let mcc' = tick_child mb mcc (dd + 1) in
        if mcc' < 0 then mcc'
        else (
          Array.unsafe_set d (base + 1) (count + 1);
          Array.unsafe_set d (base + 2) (eptr + l);
          (run [@tailcall]) mb mb.rep_cont (eptr + l) sp (dd + 1) mcc')
    else if not (rep_unit_matches mb eptr) then
      (* Chunk K1b — record the C-exact mismatch position (minimize=true). *)
      (bt [@tailcall]) mb (rep_fail_pos mb eptr true) base mcc
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
    if r < 0 then r else (bt [@tailcall]) mb eptr base mcc
  else if is_newline_at mb eptr then
    (* Chunk K1b — the newline rejection precedes the read
       (pcre2_match.c:3973-3974): record [eptr] unmoved. *)
    (bt [@tailcall]) mb eptr base mcc
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
        (* Chunk I — OP_ANY consumes one CHARACTER (multi-byte in UTF). *)
        let adv = if mb.utf then cp_len mb eptr else 1 in
        Array.unsafe_set d (base + 1) (count + 1);
        Array.unsafe_set d (base + 2) (eptr + adv);
        (run [@tailcall]) mb mb.rep_cont (eptr + adv)
          (base + Save_stack.width_rep_min)
          (dd + 1) mcc')

(* RM33 for OP_ANYNL (pcre2_match.c:3994-4017; the UTF minimize resume RM219
   decodes a code point, :3866-3889 — chunk I2): one more \R sequence (CR then
   optional LF, LF, or VT/FF/NEL/LS/PS outside the ANYCRLF convention). *)
and rep_bt_min_anynl (mb : mb) (base : int) (count : int) (eptr : int)
    (dd : int) (mcc : int) : int =
  if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (bt [@tailcall]) mb eptr base mcc
  else
    (* safe: eptr < mb.end_subject <= String.length mb.subject. Branches are
       inlined per case, exactly like the forward loop [rep_min_anynl] — no
       tuple per iteration (§8). *)
    let fc = if mb.utf then cur_cp mb eptr else Char.code (String.unsafe_get mb.subject eptr) in
    let e = eptr + if mb.utf then cp_len mb eptr else 1 in
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
      || Int.equal fc 0x2028 || Int.equal fc 0x2029
    then
      if mb.bsr_anycrlf then
        (* Chunk K1b — the read precedes the ANYCRLF rejection
           (pcre2_match.c:3866-3886 / :3994-4014): record the post-read [e]. *)
        (bt [@tailcall]) mb e base mcc
      else (rep_bt_min_anynl_step [@tailcall]) mb base count e dd mcc
    else
      (* Chunk K1b — default rejection after the read: record [e]. *)
      (bt [@tailcall]) mb e base mcc

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

(* ---------- Backreference execution (chunk F, §2/§3/§4) ---------- *)

(* pcre2_match.c:5045-5056 — a single (non-repeated) backreference: match_ref
   once, then continue; on rrc != 0 do the partial handling then NOMATCH. No
   choice point, no tick (the C's `continue`). Shared by t_ref (numbered) and
   t_dnref (dup-named, after dnref_scan resolves the ovbase). *)
and op_single_ref (mb : mb) (cont : int) (ovbase : int) (caseless : bool)
    (eptr : int) (sp : int) (rdepth : int) (mcc : int) : int =
  let rrc = match_ref mb ovbase caseless eptr in
  if not (Int.equal rrc 0) then
    (* pcre2_match.c:5050-5052 — if rrc > 0 (partial): Feptr = end_subject;
       then CHECK_PARTIAL; then NOMATCH. Chunk K1b — the RRETURN records that
       moved Feptr (end_subject on a partial-length copy, else unmoved). *)
    let feptr = if rrc > 0 then mb.end_subject else eptr in
    let rc = if feptr >= mb.end_subject then scheck_partial mb feptr else 0 in
    if rc < 0 then rc else (bt [@tailcall]) mb feptr sp mcc
  else (run [@tailcall]) mb cont (eptr + mb.ref_length) sp rdepth mcc

(* pcre2_match.c:5021-5074 — REF_REPEAT + the ref_repeat_head decision
   (interpreter.ml ref_repeat/ref_repeat_head :6081-6158). [setup_ref_rep]
   decodes the repeat; then: a SET zero-length group matches any number of
   times (continue); an unset group with Lmin = 0 or MATCH_UNSET_BACKREF
   continues (empty); otherwise ensure the minimum via [ref_min]. *)
and op_ref_repeat (mb : mb) (pc : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  setup_ref_rep mb pc;
  let ov = mb.ovector in
  let ovbase = mb.ref_ovbase in
  if not (Int.equal (Array.unsafe_get ov ovbase) Frames.unset) then
    (* pcre2_match.c:5066-5069 — group is set. *)
    if
      Int.equal (Array.unsafe_get ov ovbase) (Array.unsafe_get ov (ovbase + 1))
    then (run [@tailcall]) mb mb.ref_cont eptr sp rdepth mcc (* zero length, 5068 *)
    else (ref_min [@tailcall]) mb 1 eptr sp rdepth mcc
  else if Int.equal mb.ref_lmin 0 || mb.match_unset_backref then
    (* pcre2_match.c:5070-5074 — group not set: Lmin = 0 or MATCH_UNSET_BACKREF
       makes it a zero-length match; continue. *)
    (run [@tailcall]) mb mb.ref_cont eptr sp rdepth mcc
  else (ref_min [@tailcall]) mb 1 eptr sp rdepth mcc

(* pcre2_match.c:5076-5124 — ensure the minimum number of matches, then
   dispatch on the repeat strategy (interpreter.ml ref_min :6163-6211). Reads
   the repeat invariants from mb.ref_* (set by [setup_ref_rep]; not clobbered
   until an RMATCH, which happens only after this loop). *)
and ref_min (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if i <= mb.ref_lmin then
    let rrc = match_ref mb mb.ref_ovbase mb.ref_caseless eptr in
    if not (Int.equal rrc 0) then
      (* pcre2_match.c:5082-5087 — partial handling then NOMATCH. Chunk K1b —
         record the moved Feptr (end_subject on a partial copy, else unmoved). *)
      let feptr = if rrc > 0 then mb.end_subject else eptr in
      let rc = if feptr >= mb.end_subject then scheck_partial mb feptr else 0 in
      if rc < 0 then rc else (bt [@tailcall]) mb feptr sp mcc
    else (ref_min [@tailcall]) mb (i + 1) (eptr + mb.ref_length) sp rdepth mcc
  else if Int.equal mb.ref_lmin mb.ref_lmax then
    (* pcre2_match.c:5093 — min == max: done, continue. *)
    (run [@tailcall]) mb mb.ref_cont eptr sp rdepth mcc
  else if Int.equal mb.ref_reptype Ir.reptype_min then (
    (* pcre2_match.c:5097-5102 — minimize: RMATCH(cont, RM20) — tick, push
       KIND_REF_MIN with count = Lmin, run the continuation at rdepth+1. *)
    let mcc' = tick_child mb mcc (rdepth + 1) in
    if mcc' < 0 then mcc'
    else (
      let ss = mb.ss in
      let need = sp + Save_stack.width_ref_min in
      if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
      let d = ss.Save_stack.data in
      Array.unsafe_set d sp mb.ref_pc;
      Array.unsafe_set d (sp + 1) mb.ref_lmin (* count so far *);
      Array.unsafe_set d (sp + 2) eptr;
      Array.unsafe_set d (sp + 3) rdepth;
      Array.unsafe_set d (sp + 4) Save_stack.kind_ref_min;
      (run [@tailcall]) mb mb.ref_cont eptr need (rdepth + 1) mcc'))
  else
    (* pcre2_match.c:5120-5124 — maximize: Lstart = eptr (position after Lmin
       copies); Flength = ref length (> 0 here — a set, non-zero group).
       Outside UTF every iteration matches exactly Flength units (samelengths
       always TRUE); a CASELESS reference in UTF can consume differing unit
       counts per copy, routing to the RM22 rescan path (chunk I2). *)
    let ov = mb.ovector in
    let flength =
      Array.unsafe_get ov (mb.ref_ovbase + 1) - Array.unsafe_get ov mb.ref_ovbase
    in
    (ref_max_scan [@tailcall]) mb mb.ref_lmin flength eptr eptr true sp rdepth
      mcc

(* pcre2_match.c:5126-5146 — the maximize greedy scan (interpreter.ml
   ref_max_scan :6208-6243): match up to Lmax copies, tracking whether every
   copy consumed the same number of units as the captured group ([same], the
   C's samelengths BOOL, 5122/5144 — chunk I2; only a caseless UTF reference
   can differ). [lstart] is the position after Lmin copies (constant); [eptr]
   the running end; [i] the copy count so far. A failing/partial copy breaks
   the scan WITHOUT advancing eptr (the C's "can't use CHECK_PARTIAL because
   we don't want to update Feptr in the soft partial case", 5132-5142). *)
and ref_max_scan (mb : mb) (i : int) (flength : int) (lstart : int) (eptr : int)
    (same : bool) (sp : int) (rdepth : int) (mcc : int) : int =
  if i < mb.ref_lmax then
    let rrc = match_ref mb mb.ref_ovbase mb.ref_caseless eptr in
    if not (Int.equal rrc 0) then (
      (* pcre2_match.c:5130-5142 — partial handling (no eptr update), break. *)
      if
        rrc > 0 && mb.partial <> 0 && mb.end_subject > mb.start_used_ptr
      then (
        mb.hitend <- true;
        if mb.partial > 1 then Errors.error_partial
        else
          (ref_max_end [@tailcall]) mb i flength lstart eptr same sp rdepth mcc)
      else (ref_max_end [@tailcall]) mb i flength lstart eptr same sp rdepth mcc)
    else
      (* pcre2_match.c:5144-5145 — if (slength != Flength) samelengths =
         FALSE; Feptr += slength. *)
      let same = if not (Int.equal mb.ref_length flength) then false else same in
      (ref_max_scan [@tailcall]) mb (i + 1) flength lstart (eptr + mb.ref_length)
        same sp rdepth mcc
  else (ref_max_end [@tailcall]) mb i flength lstart eptr same sp rdepth mcc

(* pcre2_match.c:5148-5186 — after the scan: if the length matched for each
   repetition is the same as the length of the captured group, work backwards
   Flength at a time (RM21); otherwise (chunk I2) the rare differing-lengths
   path re-matches fewer and fewer copies forward from Lstart (RM22). *)
and ref_max_end (mb : mb) (i : int) (flength : int) (lstart : int) (eptr : int)
    (same : bool) (sp : int) (rdepth : int) (mcc : int) : int =
  if same then (
    (* pcre2_match.c:5154-5162 — the samelengths give-back while head: [eptr]
       is the greedy end (>= lstart, so the first RMATCH(RM21) always fires).
       Tick, push KIND_REF_MAX; the RM21 backtrack gives back Flength per
       failure down to and INCLUDING lstart, then NOMATCH (5186). *)
    let mcc' = tick_child mb mcc (rdepth + 1) in
    if mcc' < 0 then mcc'
    else (
      let ss = mb.ss in
      let need = sp + Save_stack.width_ref_max in
      if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
      let d = ss.Save_stack.data in
      Array.unsafe_set d sp mb.ref_cont;
      Array.unsafe_set d (sp + 1) eptr;
      Array.unsafe_set d (sp + 2) flength;
      Array.unsafe_set d (sp + 3) lstart;
      Array.unsafe_set d (sp + 4) rdepth;
      Array.unsafe_set d (sp + 5) Save_stack.kind_ref_max;
      (run [@tailcall]) mb mb.ref_cont eptr need (rdepth + 1) mcc'))
  else (
    (* pcre2_match.c:5164-5172 — differing lengths (chunk I2): Lmax = i (the
       achieved copy count); the for(;;) starts with RMATCH(Fecode, RM22) at
       the greedy end — tick, push KIND_REF_MAX2; each failure re-scans one
       fewer copy from Lstart (the RM22 resume, [backtrack_ref_max2]). *)
    let mcc' = tick_child mb mcc (rdepth + 1) in
    if mcc' < 0 then mcc'
    else (
      let ss = mb.ss in
      let need = sp + Save_stack.width_ref_max2 in
      if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
      let d = ss.Save_stack.data in
      Array.unsafe_set d sp mb.ref_pc;
      Array.unsafe_set d (sp + 1) i (* lmax_cur *);
      Array.unsafe_set d (sp + 2) eptr (* try_eptr *);
      Array.unsafe_set d (sp + 3) lstart;
      Array.unsafe_set d (sp + 4) rdepth;
      Array.unsafe_set d (sp + 5) Save_stack.kind_ref_max2;
      (run [@tailcall]) mb mb.ref_cont eptr need (rdepth + 1) mcc'))

(* KIND_REF_MIN backtrack (RM20, pcre2_match.c:5102-5113 / interpreter.ml
   :7573-7604): the continuation failed; if Lmin < Lmax match one more copy and
   retry. Re-derives the repeat invariants from the IR via [setup_ref_rep] (a
   nested ref may have clobbered the mb.ref fields). *)
and backtrack_ref_min (mb : mb) (sp : int) (mcc : int) : int =
  let d = mb.ss.Save_stack.data in
  let base = sp - Save_stack.width_ref_min in
  let rep_pc = Array.unsafe_get d base in
  let count = Array.unsafe_get d (base + 1) in
  let eptr = Array.unsafe_get d (base + 2) in
  let dd = Array.unsafe_get d (base + 3) in
  setup_ref_rep mb rep_pc;
  if count >= mb.ref_lmax then
    (* pcre2_match.c:5104 — Lmin++ >= Lmax: RRETURN(MATCH_NOMATCH) from the
       repeat frame at the current extend position (chunk K1b: record it). *)
    (bt [@tailcall]) mb eptr base mcc
  else
    let rrc = match_ref mb mb.ref_ovbase mb.ref_caseless eptr in
    if not (Int.equal rrc 0) then
      (* pcre2_match.c:5106-5111 — partial handling then NOMATCH. Chunk K1b —
         record the moved Feptr (end_subject on a partial copy, else unmoved). *)
      let feptr = if rrc > 0 then mb.end_subject else eptr in
      let rc = if feptr >= mb.end_subject then scheck_partial mb feptr else 0 in
      if rc < 0 then rc else (bt [@tailcall]) mb feptr base mcc
    else
      let mcc' = tick_child mb mcc (dd + 1) in
      if mcc' < 0 then mcc'
      else (
        let eptr' = eptr + mb.ref_length in
        Array.unsafe_set d (base + 1) (count + 1);
        Array.unsafe_set d (base + 2) eptr';
        (run [@tailcall]) mb mb.ref_cont eptr' sp (dd + 1) mcc')

(* KIND_REF_MAX backtrack (RM21, pcre2_match.c:5158-5161 / interpreter.ml
   :7605-7614): give back one copy (Feptr -= Flength); while still >= lstart,
   retry the continuation (tick); below lstart -> NOMATCH. *)
and backtrack_ref_max (mb : mb) (sp : int) (mcc : int) : int =
  let d = mb.ss.Save_stack.data in
  let base = sp - Save_stack.width_ref_max in
  let cont = Array.unsafe_get d base in
  let try_eptr = Array.unsafe_get d (base + 1) in
  let flength = Array.unsafe_get d (base + 2) in
  let lstart = Array.unsafe_get d (base + 3) in
  let dd = Array.unsafe_get d (base + 4) in
  let new_eptr = try_eptr - flength in
  if new_eptr >= lstart then (
    let mcc' = tick_child mb mcc (dd + 1) in
    if mcc' < 0 then mcc'
    else (
      Array.unsafe_set d (base + 1) new_eptr;
      (run [@tailcall]) mb cont new_eptr sp (dd + 1) mcc'))
  else (backtrack [@tailcall]) mb base mcc (* Feptr < Lstart: NOMATCH, 5186 *)

(* KIND_REF_MAX2 backtrack (RM22, pcre2_match.c:5172-5184 / interpreter.ml
   :7606-7622 + ref_max_rescan — chunk I2): the differing-lengths maximize.
   Fail once Feptr is back at Lstart (5174); otherwise Feptr = Lstart, Lmax--
   (lmax_cur), re-scan lmax_cur - Lmin copies forward — match_ref is known to
   succeed for those (5164-5165), its return discarded like the C's (void)
   cast — then retry the continuation at the new end (tick). Re-derives the
   repeat invariants from the IR via [setup_ref_rep]. *)
and backtrack_ref_max2 (mb : mb) (sp : int) (mcc : int) : int =
  let d = mb.ss.Save_stack.data in
  let base = sp - Save_stack.width_ref_max2 in
  let ref_pc = Array.unsafe_get d base in
  let lmax_cur = Array.unsafe_get d (base + 1) in
  let try_eptr = Array.unsafe_get d (base + 2) in
  let lstart = Array.unsafe_get d (base + 3) in
  let dd = Array.unsafe_get d (base + 4) in
  if Int.equal try_eptr lstart then (backtrack [@tailcall]) mb base mcc
    (* pcre2_match.c:5174 — fail after the minimal repetition. *)
  else (
    setup_ref_rep mb ref_pc;
    let lmax_cur = lmax_cur - 1 (* Lmax--, 5176 *) in
    (* pcre2_match.c:5177-5182 — for (i = Lmin; i < Lmax; i++): re-match and
       advance (module-level [ref_max2_rescan] — no closure, §8). *)
    let e = ref_max2_rescan mb lmax_cur mb.ref_lmin lstart in
    let mcc' = tick_child mb mcc (dd + 1) in
    if mcc' < 0 then mcc'
    else (
      Array.unsafe_set d (base + 1) lmax_cur;
      Array.unsafe_set d (base + 2) e;
      (run [@tailcall]) mb mb.ref_cont e sp (dd + 1) mcc'))

(* ---------- Lookaround / atomic groups (chunk G, §2/§3) ---------- *)

(* KIND_ONCE backtrack (fast-design.md §3): the atomic construct is backtracked
   PAST. Restore the group-ovector snapshot and mb.once_base, then keep popping
   (propagate NOMATCH below the boundary). This reproduces the C's abandon-and-
   restore-wholesale of everything the atomic group did — its captures (undone by
   the snapshot) and its abandoned iterations (their records were discarded by the
   COMMIT) — since the fast engine's shared single ovector has no per-frame copy
   (pcre2_match.c:6023-6031). eptr on the continued backtrack comes from the lower
   record (as in the C, where backtracking past OP_ONCE lands in P's frame). *)
and backtrack_once (mb : mb) (sp : int) (mcc : int) : int =
  let nov = 2 * mb.oveccount in
  let base = sp - (nov + 3) in
  let d = mb.ss.Save_stack.data in
  let ov = mb.ovector in
  (* safe: [base] is a KIND_ONCE record base; its snapshot occupies
     [base, base + nov - 2), prev_once_base at base + nov - 2 and saved_mark at
     base + nov - 1 (entry_eptr at base + nov and subtype at base + nov + 1,
     unread on a NOMATCH backtrack: the C's exhaustion RRETURN at the entry is
     subsumed — every downstream construct recorded >= its own spine position
     >= this entry when it failed, §4). ov has nov slots. *)
  for i = 2 to nov - 1 do
    Array.unsafe_set ov i (Array.unsafe_get d (base + i - 2))
  done;
  mb.once_base <- Array.unsafe_get d (base + nov - 2);
  mb.mark <- Array.unsafe_get d (base + nov - 1) (* revert Fmark *);
  (backtrack [@tailcall]) mb base mcc

(* KIND_NASSERT backtrack (fast-design.md §3): ALL branches of a negative
   assertion failed = the assertion SUCCEEDS (pcre2_match.c:5578-5584). Restore
   the snapshot (defensive: the failed branches' CAP records already rolled it
   back) and mb.once_base, then continue at the stored [cont] with the entry
   eptr/rdepth. The record is popped (sp = base): the negative assertion is
   atomic (no other way to succeed), so nothing is retried. No tick (the C's RM4
   resume + ASSERT_NOT_FAILED continues in the entry frame). *)
and backtrack_nassert (mb : mb) (sp : int) (mcc : int) : int =
  let nov = 2 * mb.oveccount in
  let base = sp - (nov + 4) in
  let d = mb.ss.Save_stack.data in
  let ov = mb.ovector in
  (* safe: [base] is a KIND_NASSERT record base; snapshot [base, base + nov - 2),
     prev_once_base at base + nov - 2, cont/eptr/rdepth at base + nov - 1 ..
     base + nov + 1, saved_mark at base + nov + 2. ov has nov slots. *)
  for i = 2 to nov - 1 do
    Array.unsafe_set ov i (Array.unsafe_get d (base + i - 2))
  done;
  mb.once_base <- Array.unsafe_get d (base + nov - 2);
  let cont = Array.unsafe_get d (base + nov - 1) in
  let eptr = Array.unsafe_get d (base + nov) in
  let rdepth = Array.unsafe_get d (base + nov + 1) in
  mb.mark <- Array.unsafe_get d (base + nov + 2) (* revert Fmark to entry *);
  (run [@tailcall]) mb cont eptr base rdepth mcc

(* OP_VREVERSE forward (fast-design.md §2/§3; pcre2_match.c:5834-5883,
   interpreter.ml:2661-2707) — variable lookbehind step-back, non-UTF. Move back
   by the maximum available branch length, then work forwards on failure (the
   RM37 loop) down to the minimum. The first attempt is a ticked RMATCH(RM37)
   at rdepth+1. *)
and op_reverse_utf (mb : mb) (pc : int) (number : int) (eptr : int) (sp : int)
    (rdepth : int) (mcc : int) : int =
  (* pcre2_match.c:5797-5804 (interpreter.ml op_reverse_utf_loop) —
     OP_REVERSE, UTF (chunk I2): while (number-- > 0) fail when Feptr <=
     mb->check_subject, else Feptr--; BACKCHAR(Feptr). Then the shared tail
     (5815-5818): lower start_used_ptr and continue. *)
  if number > 0 then
    if eptr <= mb.check_subject then (bt [@tailcall]) mb eptr sp mcc
    else
      (* eptr - 1 >= check_subject >= 0 (guard above); backchar_sub clamps. *)
      (op_reverse_utf [@tailcall]) mb pc (number - 1)
        (backchar_sub mb.subject (eptr - 1))
        sp rdepth mcc
  else (
    if eptr < mb.start_used_ptr then mb.start_used_ptr <- eptr;
    (run [@tailcall]) mb (pc + 2) eptr sp rdepth mcc)

and op_vreverse (mb : mb) (pc : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  let lmin = mb.code.(pc + 1) in
  let lmax = mb.code.(pc + 2) in
  let body_pc = pc + 3 in
  if mb.utf then
    (* pcre2_match.c:5843-5857 (interpreter.ml op_vreverse_utf_loop) — UTF
       (chunk I2): step back by CHARACTERS with BACKCHAR, stopping at the
       subject start (NOMATCH when even the minimum cannot be reached; Lmax
       clamped to the characters actually available). *)
    (op_vreverse_utf [@tailcall]) mb body_pc lmin lmax 0 eptr sp rdepth mcc
  else
    (* pcre2_match.c:5864-5865 — available = clamp(Feptr - start_subject, 0,
       65535). *)
    let diff = eptr - mb.start_subject in
    let available =
      if diff > 65535 then 65535 else if diff > 0 then diff else 0
    in
    if lmin > available then (bt [@tailcall]) mb eptr sp mcc
      (* pcre2_match.c:5866 — too close to the start for even the minimum. *)
    else
      let cur_lmax = if lmax > available then available else lmax in
      (vreverse_try [@tailcall]) mb body_pc cur_lmax lmin (eptr - cur_lmax) sp
        rdepth mcc

and op_vreverse_utf (mb : mb) (body_pc : int) (lmin : int) (lmax : int)
    (i : int) (eptr : int) (sp : int) (rdepth : int) (mcc : int) : int =
  (* pcre2_match.c:5845-5856 (interpreter.ml op_vreverse_utf_loop, chunk I2) —
     for (i = 0; i < Lmax; i++): stop at the subject start (NOMATCH below the
     minimum, otherwise Lmax = i, 5848-5853), stepping back with `Feptr--;
     BACKCHAR(Feptr)` (5854-5855); then fall into the shared RM37 loop
     (5871-5876). DEVIATION (defined behavior where the C is undefined;
     interpreter.ml op_vreverse_utf_loop's pin, transcribed): the C's BACKCHAR
     is an unbounded continuation-byte walk that crosses below the subject
     when the code units from start_subject up are ALL continuation bytes
     (reachable under MATCH_INVALID_UTF's bad-start skip, a nonzero start
     offset's unchecked prefix, or NO_UTF_CHECK garbage). The walk is bounded
     at start_subject, and a bounded walk that still lands on a continuation
     byte (exactly the crossing condition) cannot complete the back-step —
     same effect as reaching start_subject in the 5848-5853 too-few/cap
     logic, eptr left at its pre-step value. The stop test stays C-literal
     (start_subject, NOT check_subject — see the interpreter's note: 10.44's
     max_lookbehind does not count nested lookbehinds, so a nested VREVERSE
     legitimately walks below check_subject on valid UTF). *)
  if i < lmax then
    if Int.equal eptr mb.start_subject then
      if i < lmin then (bt [@tailcall]) mb eptr sp mcc
      else (vreverse_try [@tailcall]) mb body_pc i lmin eptr sp rdepth mcc
    else
      (* eptr - 1 >= start_subject = 0 (eptr <> start_subject above);
         backchar_sub bounds the walk at 0. *)
      let p = backchar_sub mb.subject (eptr - 1) in
      if
        (* safe: 0 <= p <= eptr - 1 < eptr <= mb.end_subject <= String.length
           mb.subject. *)
        Int.equal (Char.code (String.unsafe_get mb.subject p) land 0xc0) 0x80
      then
        (* pin: the bounded walk landed on a continuation byte — the step
           cannot be completed (cap logic as above, eptr unchanged). *)
        if i < lmin then (bt [@tailcall]) mb eptr sp mcc
        else (vreverse_try [@tailcall]) mb body_pc i lmin eptr sp rdepth mcc
      else
        (op_vreverse_utf [@tailcall]) mb body_pc lmin lmax (i + 1) p sp rdepth
          mcc
  else (vreverse_try [@tailcall]) mb body_pc lmax lmin eptr sp rdepth mcc

and vreverse_try (mb : mb) (body_pc : int) (cur_lmax : int) (lmin : int)
    (start_eptr : int) (sp : int) (rdepth : int) (mcc : int) : int =
  (* pcre2_match.c:5871-5876 — now try matching, moving forward one character
     on failure, until we reach the minimum back length: the for(;;) starts
     with RMATCH(Fecode + 1 + 2*IMM2_SIZE, RM37) — tick + push KIND_VREVERSE,
     run the branch body at rdepth+1 (shared by the UTF and non-UTF entry
     paths, chunk I2). *)
  let mcc' = tick_child mb mcc (rdepth + 1) in
  if mcc' < 0 then mcc'
  else (
    let ss = mb.ss in
    let need = sp + Save_stack.width_vreverse in
    if need > Array.length ss.Save_stack.data then Save_stack.grow ss need;
    let d = ss.Save_stack.data in
    (* safe: [grow] ensured length >= sp + width_vreverse. Layout: [body_pc;
       cur_lmax; lmin; cur_eptr; rdepth; KIND_VREVERSE]. *)
    Array.unsafe_set d sp body_pc;
    Array.unsafe_set d (sp + 1) cur_lmax;
    Array.unsafe_set d (sp + 2) lmin;
    Array.unsafe_set d (sp + 3) start_eptr;
    Array.unsafe_set d (sp + 4) rdepth;
    Array.unsafe_set d (sp + 5) Save_stack.kind_vreverse;
    (run [@tailcall]) mb body_pc start_eptr need (rdepth + 1) mcc')

(* KIND_VREVERSE backtrack (RM37, pcre2_match.c:5877-5882): the branch failed at
   this back-length. `if (Lmax-- <= Lmin) NOMATCH` (compare OLD Lmax) else Feptr++
   and retry the body (a ticked RMATCH at rdepth+1). *)
and backtrack_vreverse (mb : mb) (sp : int) (mcc : int) : int =
  let d = mb.ss.Save_stack.data in
  let base = sp - Save_stack.width_vreverse in
  let body_pc = Array.unsafe_get d base in
  let cur_lmax = Array.unsafe_get d (base + 1) in
  let lmin = Array.unsafe_get d (base + 2) in
  let cur_eptr = Array.unsafe_get d (base + 3) in
  let dd = Array.unsafe_get d (base + 4) in
  if cur_lmax <= lmin then (backtrack [@tailcall]) mb base mcc (* NOMATCH, 5878 *)
  else
    let mcc' = tick_child mb mcc (dd + 1) in
    if mcc' < 0 then mcc'
    else (
      (* pcre2_match.c:5880-5881 — Feptr++; in UTF the step crosses the
         character's remaining code units (FORWARDCHARTEST, chunk I2). *)
      let new_eptr =
        if mb.utf then
          Utf.forwardchartest mb.subject (cur_eptr + 1) mb.end_subject
        else cur_eptr + 1
      in
      Array.unsafe_set d (base + 1) (cur_lmax - 1);
      Array.unsafe_set d (base + 3) new_eptr;
      (run [@tailcall]) mb body_pc new_eptr sp (dd + 1) mcc')

(* OP_KETRPOS (fast-design.md §3; pcre2_match.c:5292-5302 / 6092-6098): a
   possessive iteration matched (a branch reached here via JMP). Commit it —
   discard the body's internal choice points by truncating to the KIND_POS
   boundary — writing the capture for a capturing bracket, then loop back to the
   body entry (Fecode = Lstart_group) or, on an empty match, break to
   POSSESS_DONE (pc+3). Reset rdepth to the loop-level entry_rdepth. No tick (the
   next iteration's first ALT ticks). *)
and op_ketrpos (mb : mb) (pc : int) (eptr : int) (mcc : int) : int =
  (* Chunk K1b — the C's OP_KETRPOS is RRETURN(MATCH_KETRPOS)
     (pcre2_match.c:6092-6097, BEFORE the empty test, which runs at the outer
     level), so RETURN_SWITCH (:6470) records the iteration-end Feptr in
     last_used_ptr. The fast loop-back/break stays in-line (no bt transition), so
     record it here — a possessive group can coexist with recursion (only a
     recursion INTO a possessive capture is declined) and the reach feeds the
     RECURSELOOP check. Gated on has_recurse. *)
  if mb.has_recurse && eptr > mb.last_used_ptr then mb.last_used_ptr <- eptr;
  let code = mb.code in
  let body_entry = code.(pc + 1) in
  let cap_ovbase = code.(pc + 2) in
  let base = mb.once_base in
  let nov = 2 * mb.oveccount in
  let d = mb.ss.Save_stack.data in
  (* safe: [base] is a KIND_POS record base; iter_start at base+nov-1,
     matched_once at base+nov, entry_rdepth at base+nov+2. *)
  let iter_start = Array.unsafe_get d (base + nov - 1) in
  let entry_rdepth = Array.unsafe_get d (base + nov + 2) in
  (* pcre2_match.c:6079-6083 — a capturing possessive bracket writes its ovector
     pair each iteration (the last committed iteration wins). The ovector is
     written only HERE (at close), so a mid-body backref sees only closed values
     (the non-optimized-cbracket property, fast-design.md §3). safe: cap_ovbase =
     2N (Ir_verify) < 2*oveccount. *)
  if cap_ovbase > 0 then (
    let ov = mb.ovector in
    Array.unsafe_set ov cap_ovbase (iter_start - mb.start_subject);
    Array.unsafe_set ov (cap_ovbase + 1) (eptr - mb.start_subject));
  Array.unsafe_set d (base + nov) 1 (* Lmatched_once = TRUE, 5294 *);
  let sp' = base + nov + 5 (* commit: truncate to boundary + width_pos *) in
  if Int.equal eptr iter_start then
    (* pcre2_match.c:5295-5299 — empty match: break the loop (to POSSESS_DONE). *)
    (run [@tailcall]) mb (pc + 3) eptr sp' entry_rdepth mcc
  else (
    (* pcre2_match.c:5301-5302 — restart from the bracket for the next iteration. *)
    Array.unsafe_set d (base + nov - 1) eptr (* iter_start = eptr *);
    (* Chunk H — refresh cap_start to the NEW iteration's start (for an OP_CLOSE
       at an inner ( *ACCEPT); see the POSSESS arm). *)
    if cap_ovbase > 0 then
      Array.unsafe_set mb.cap_start cap_ovbase (eptr - mb.start_subject);
    (run [@tailcall]) mb body_entry eptr sp' entry_rdepth mcc)

(* KIND_POS backtrack (fast-design.md §3): the possessive group is backtracked
   past. Restore the group-ovector snapshot + mb.once_base, then keep popping
   (propagate NOMATCH). Identical to KIND_ONCE but for the wider record. *)
and backtrack_pos (mb : mb) (sp : int) (mcc : int) : int =
  let nov = 2 * mb.oveccount in
  let base = sp - (nov + 5) in
  let d = mb.ss.Save_stack.data in
  let ov = mb.ovector in
  (* safe: [base] is a KIND_POS record base; snapshot [base, base + nov - 2),
     prev_once_base at base + nov - 2, saved_mark at base + nov + 3. *)
  for i = 2 to nov - 1 do
    Array.unsafe_set ov i (Array.unsafe_get d (base + i - 2))
  done;
  mb.once_base <- Array.unsafe_get d (base + nov - 2);
  mb.mark <- Array.unsafe_get d (base + nov + 3) (* revert Fmark *);
  (backtrack [@tailcall]) mb base mcc

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
  if Int.equal k rk_allany then
    if mb.utf then
      (* UTF OP_ALLANY min loop: Feptr++; ACROSSCHAR per character
         (pcre2_match.c:3028-3039; interpreter.ml typemin_utf_allany:5453-5469).
         NOT the generic rep_unit_utf/cp_len loop: after \C the eptr can be
         mid-character, where ACROSSCHAR and cp_len disagree (see
         [any_advance]). *)
      (rep_min_allany_utf [@tailcall]) mb 0 eptr sp rdepth mcc
    else (rep_min_allany [@tailcall]) mb eptr sp rdepth mcc
  else if Int.equal k rk_anybyte then
    (* Chunk I2 — \C min loop: one bound check, NO SCHECK_PARTIAL
       (pcre2_match.c:3041-3044, unlike OP_ALLANY). *)
    if eptr > mb.end_subject - mb.rep_lmin then (bt [@tailcall]) mb eptr sp mcc
    else (rep_after_min [@tailcall]) mb (eptr + mb.rep_lmin) sp rdepth mcc
  else if Int.equal k rk_any then (rep_min_any [@tailcall]) mb 0 eptr sp rdepth mcc
  else if Int.equal k rk_anynl then
    (rep_min_anynl [@tailcall]) mb 0 eptr sp rdepth mcc
  else if Int.equal k rk_extuni then
    (rep_min_extuni [@tailcall]) mb 0 eptr sp rdepth mcc
  else if
    (* Chunk I2 — PT_ANY hoists the OP_NOTPROP check ahead of its min loop
       (pcre2_match.c:2731-2732, inside the Lmin > 0 block): NOMATCH before
       consuming anything (and before any SCHECK_PARTIAL). *)
    Int.equal k rk_prop
    && mb.rep_lmin > 0
    && (not mb.rep_want)
    && Int.equal mb.rep_ptype Opcodes.pt_any
  then (bt [@tailcall]) mb eptr sp mcc
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
  else if Int.equal k rk_xclass then
    (* Chunk I — an XCLASS repeat in NON-UTF: the code unit is 0..255; probe via
       the shared Xclass helper (rep_map_off holds the class data offset). *)
    Xclass.xclass cc mb.bytecode mb.rep_map_off mb.utf
  else if Int.equal k rk_hspace then
    Bool.equal (Char_predicates.hspace_byte cc) mb.rep_want
  else if Int.equal k rk_vspace then
    Bool.equal (Char_predicates.vspace_byte cc) mb.rep_want
  else if Int.equal k rk_prop then
    (* Chunk I2 — a property repeat in NON-UTF (UCP mode, or a bare \p): one
       character per code unit; prop_test == notmatch is a mismatch (the
       property min/minimize/maximize loops, pcre2_match.c:2726-2974 /
       3515-3801 / 4115-4381). *)
    Bool.equal (prop_test cc mb.rep_ptype mb.rep_pdata) mb.rep_want
  else true (* rk_allany *)

(* Chunk I — one UTF character at [eptr] against the repeat's per-unit test:
   returns the byte LENGTH consumed on a match (>= 1), else 0. Handles the
   per-character kinds (char / ctype / class / xclass / \h / \v, plus — chunk
   I2 — rk_prop and the UTF rk_allany, which matches any character); the
   variable-length / bulk kinds (rk_any / rk_anynl / rk_anybyte / rk_extuni)
   have dedicated loops. safe: caller proves eptr < end_subject. Mirrors the
   interpreter's UTF single-char / class / xclass tests (getutf8 + code-point
   predicate; ctype guarded cp <= 255 — CHMAX_255). *)
and rep_unit_utf (mb : mb) (eptr : int) : int =
  let c0 = Char.code (String.unsafe_get mb.subject eptr) in
  let cp = if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject eptr else c0 in
  let len = if c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
  let k = mb.rep_kind in
  let ok =
    if Int.equal k rk_char then
      Bool.equal (Int.equal cp mb.rep_c1 || Int.equal cp mb.rep_c2) mb.rep_want
    else if Int.equal k rk_ctype then
      Bool.equal
        (cp <= 255 && not (Int.equal (Chartables.ctypes cp land mb.rep_mask) 0))
        mb.rep_want
    else if Int.equal k rk_class then class_cp_match mb mb.rep_map_off cp
    else if Int.equal k rk_xclass then
      Xclass.xclass cp mb.bytecode mb.rep_map_off mb.utf
    else if Int.equal k rk_hspace then
      Bool.equal (Char_predicates.hspace_char cp) mb.rep_want
    else if Int.equal k rk_vspace then
      Bool.equal (Char_predicates.vspace_char cp) mb.rep_want
    else if Int.equal k rk_prop then
      (* Chunk I2 — property repeat, UTF: prop_test on the decoded point. *)
      Bool.equal (prop_test cp mb.rep_ptype mb.rep_pdata) mb.rep_want
    else true
    (* Total-function fallback for the if-chain. rk_allany UTF (which "matches
       any character") no longer reaches here: it has dedicated ACROSSCHAR loops
       (rep_min_allany_utf / rep_greedy_allany_utf) since cp_len under-consumes
       a \C-split character; rk_any/rk_anynl/rk_anybyte/rk_extuni likewise have
       dedicated loops. *)
  in
  if ok then len else 0

and rep_min_loop (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  (* Ensure the minimum count in place, no ticks (repeatchar_ci_min
     interpreter.ml:3727-3749 / class_min 4351 / typemin_ctype 4894;
     SCHECK_PARTIAL at/past end). char / ctype / class / hspace / vspace. *)
  if i >= mb.rep_lmin then (rep_after_min [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc
  else if mb.utf then
    (* Chunk I — step over one CHARACTER on a match. Chunk K1b — a mismatch
       records the C-exact per-family position (rep_fail_pos, minimize=false). *)
    let l = rep_unit_utf mb eptr in
    if Int.equal l 0 then
      (bt [@tailcall]) mb (rep_fail_pos mb eptr false) sp mcc
    else (rep_min_loop [@tailcall]) mb (i + 1) (eptr + l) sp rdepth mcc
  else if not (rep_unit_matches mb eptr) then
    (bt [@tailcall]) mb (rep_fail_pos mb eptr false) sp mcc
  else (rep_min_loop [@tailcall]) mb (i + 1) (eptr + 1) sp rdepth mcc

(* OP_ALLANY / OP_ANYBYTE min loop (pcre2_match.c:3280-3287): one bound check
   for [lmin] code units. The scheck fires at the ORIGINAL eptr (not a
   per-char eptr), which the bulk form preserves. Non-UTF only (in UTF,
   OP_ANYBYTE keeps this byte-bulk form via op_repeat's rk_anybyte arm, while
   OP_ALLANY steps characters — see [rep_min_allany_utf]). *)
and rep_min_allany (mb : mb) (eptr : int) (sp : int) (rdepth : int) (mcc : int) :
    int =
  if eptr > mb.end_subject - mb.rep_lmin then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc
  else (rep_after_min [@tailcall]) mb (eptr + mb.rep_lmin) sp rdepth mcc

(* UTF OP_ALLANY min loop (pcre2_match.c:3028-3039; interpreter.ml
   typemin_utf_allany:5453-5469): ensure [lmin] characters, each Feptr++;
   ACROSSCHAR ([any_advance]). NO newline test (unlike OP_ANY) and NO per-unit
   predicate — OP_ALLANY matches any character. SCHECK_PARTIAL at/past end
   fires at the current per-char eptr. *)
and rep_min_allany_utf (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if i >= mb.rep_lmin then (rep_after_min [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc
  else
    (rep_min_allany_utf [@tailcall]) mb (i + 1) (any_advance mb eptr) sp rdepth
      mcc

(* OP_ANY min loop (pcre2_match.c:3258-3278): a newline stops the match; a CR
   at the very end of the subject could be a partial CRLF. *)
and rep_min_any (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if i >= mb.rep_lmin then (rep_after_min [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc
  else if is_newline_at mb eptr then (bt [@tailcall]) mb eptr sp mcc
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
      (* OP_ANY forward min: Feptr++; ACROSSCHAR (pcre2_match.c:3024-3025;
         interpreter.ml typemin_utf_any:5448-5449). NOT cp_len: after \C the
         eptr can be mid-character (a continuation byte). *)
      (rep_min_any [@tailcall]) mb (i + 1) (any_advance mb eptr) sp rdepth mcc

(* OP_ANYNL min loop (pcre2_match.c:3302-3332 non-UTF; 2998-3028 UTF, which
   decodes a code point — NEL is 2 bytes and LS/PS U+2028/U+2029 are 3, chunk
   I2): CR absorbs a following LF; LF; VT/FF/NEL/LS/PS unless the ANYCRLF
   convention. *)
and rep_min_anynl (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if i >= mb.rep_lmin then (rep_after_min [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc
  else
    (* safe: eptr < mb.end_subject <= String.length mb.subject. GETCHARINC in
       UTF (chunk I2); one code unit otherwise. *)
    let fc = if mb.utf then cur_cp mb eptr else Char.code (String.unsafe_get mb.subject eptr) in
    let e = eptr + if mb.utf then cp_len mb eptr else 1 in
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
      || Int.equal fc 0x2028 || Int.equal fc 0x2029
    then
      if mb.bsr_anycrlf then
        (* Chunk K1b — GETCHARINC precedes the ANYCRLF rejection
           (pcre2_match.c:3054/3073): record the post-read [e]. *)
        (bt [@tailcall]) mb e sp mcc
      else (rep_min_anynl [@tailcall]) mb (i + 1) e sp rdepth mcc
    else
      (* Chunk K1b — the default rejection follows the read
         (pcre2_match.c:3054-3057 / `switch( *Feptr++)` :3310-3312): record [e]. *)
      (bt [@tailcall]) mb e sp mcc

(* OP_EXTUNI min loop (chunk I2; pcre2_match.c:2979-2996 / interpreter.ml
   typemin_extuni): ensure the minimum number of grapheme clusters are
   present — bound check + SCHECK_PARTIAL, one cluster step (extuni_at), then
   CHECK_PARTIAL (which may return the hard-partial error). *)
and rep_min_extuni (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if i >= mb.rep_lmin then (rep_after_min [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    (* pcre2_match.c:2983-2987 — SCHECK_PARTIAL(), then no match. *)
    let r = scheck_partial mb eptr in
    if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc
  else
    (* pcre2_match.c:2988-2993 — GETCHARINCTEST + PRIV(extuni). *)
    let e' = extuni_at mb eptr in
    (* CHECK_PARTIAL() (2994). *)
    let r = if e' >= mb.end_subject then scheck_partial mb e' else 0 in
    if r < 0 then r
    else (rep_min_extuni [@tailcall]) mb (i + 1) e' sp rdepth mcc

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
      if mb.utf then
        (* UTF OP_ALLANY maximize: Feptr++; ACROSSCHAR per character
           (pcre2_match.c:4501-4518; interpreter.ml typemax_utf_allany:5776-5793).
           The Lmax = UINT32_MAX arm jumps straight to end_subject + SCHECK —
           position- and SCHECK-identical to the per-char loop, which stops at
           end_subject with one SCHECK. NOT the generic cp_len loop (see
           [any_advance]): after \C the eptr can be mid-character. *)
        (rep_greedy_allany_utf [@tailcall]) mb mb.rep_lmin eptr sp rdepth mcc
      else (rep_greedy_allany [@tailcall]) mb eptr sp rdepth mcc
    else if Int.equal k rk_anybyte then
      (* Chunk I2 — \C greedy: the byte-bulk form (pcre2_match.c:4834-4845),
         identical to the non-UTF OP_ALLANY bulk. *)
      (rep_greedy_allany [@tailcall]) mb eptr sp rdepth mcc
    else if Int.equal k rk_any then
      (rep_greedy_any [@tailcall]) mb mb.rep_lmin eptr sp rdepth mcc
    else if Int.equal k rk_anynl then
      (rep_greedy_anynl [@tailcall]) mb mb.rep_lmin eptr sp rdepth mcc
    else if Int.equal k rk_extuni then
      (rep_greedy_extuni [@tailcall]) mb mb.rep_lmin eptr sp rdepth mcc
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
  else if mb.utf then
    (* Chunk I — step over one CHARACTER on a match. *)
    let l = rep_unit_utf mb eptr in
    if Int.equal l 0 then (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
    else (rep_greedy [@tailcall]) mb (i + 1) (eptr + l) sp rdepth mcc
  else if not (rep_unit_matches mb eptr) then
    (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else (rep_greedy [@tailcall]) mb (i + 1) (eptr + 1) sp rdepth mcc

(* OP_ALLANY / OP_ANYBYTE greedy (pcre2_match.c:4748-4757): grab up to
   (Lmax - Lmin) more code units, or to end_subject (with the SCHECK). Non-UTF
   (and, via op_repeat's rk_anybyte arm, \C in UTF — the byte-bulk form). UTF
   OP_ALLANY steps characters via [rep_greedy_allany_utf]. *)
and rep_greedy_allany (mb : mb) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  let n = mb.rep_lmax - mb.rep_lmin in
  let avail = mb.end_subject - eptr in
  if n > avail then
    let r = scheck_partial mb mb.end_subject in
    if r < 0 then r
    else (rep_greedy_done_dispatch [@tailcall]) mb mb.end_subject sp rdepth mcc
  else (rep_greedy_done_dispatch [@tailcall]) mb (eptr + n) sp rdepth mcc

(* UTF OP_ALLANY maximize scan (pcre2_match.c:4501-4518; interpreter.ml
   typemax_utf_allany:5776-5793): grab up to Lmax characters, each Feptr++;
   ACROSSCHAR ([any_advance]), stopping at end_subject with one SCHECK. NO
   newline test and NO per-unit predicate. The C's Lmax = UINT32_MAX arm jumps
   straight to end_subject + SCHECK; this per-char loop reaches the same
   position with the same single SCHECK, so it is position- and
   SCHECK-identical. *)
and rep_greedy_allany_utf (mb : mb) (i : int) (eptr : int) (sp : int)
    (rdepth : int) (mcc : int) : int =
  if i >= mb.rep_lmax then
    (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r
    else (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else
    (rep_greedy_allany_utf [@tailcall]) mb (i + 1) (any_advance mb eptr) sp
      rdepth mcc

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
    else
      (* OP_ANY forward greedy: Feptr++; ACROSSCHAR (pcre2_match.c:4495-4496;
         interpreter.ml typemax_utf_any:5771-5772). NOT cp_len (see
         [any_advance]): after \C the eptr can be mid-character. *)
      (rep_greedy_any [@tailcall]) mb (i + 1) (any_advance mb eptr) sp rdepth mcc

(* OP_ANYNL greedy (pcre2_match.c:4759-4784 non-UTF; 4786-4818 UTF, which
   decodes a code point — chunk I2): CR (absorb LF; a lone CR at end breaks
   after being consumed), LF, or VT/FF/NEL/LS/PS outside ANYCRLF. *)
and rep_greedy_anynl (mb : mb) (i : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if i >= mb.rep_lmax then (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    let r = scheck_partial mb eptr in
    if r < 0 then r else (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else
    (* safe: eptr < mb.end_subject <= String.length mb.subject. GETCHARLEN in
       UTF (chunk I2); one code unit otherwise. *)
    let fc = if mb.utf then cur_cp mb eptr else Char.code (String.unsafe_get mb.subject eptr) in
    if Int.equal fc Newline.char_cr then
      (* pcre2_match.c:4766-4770 — if (++Feptr >= end) break; absorb LF. *)
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
            && (not (Int.equal fc Newline.char_nel))
            && (not (Int.equal fc 0x2028))
            && not (Int.equal fc 0x2029))
    then (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
    else
      (rep_greedy_anynl [@tailcall]) mb (i + 1)
        (eptr + if mb.utf then cp_len mb eptr else 1)
        sp rdepth mcc

(* OP_EXTUNI greedy (chunk I2; pcre2_match.c:4401-4420 / interpreter.ml
   extuni_maxscan): find the longest run of grapheme clusters — bound check +
   SCHECK_PARTIAL breaks the scan; each step is one cluster (extuni_at)
   followed by CHECK_PARTIAL (which may return the hard-partial error). *)
and rep_greedy_extuni (mb : mb) (i : int) (eptr : int) (sp : int)
    (rdepth : int) (mcc : int) : int =
  if i >= mb.rep_lmax then
    (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else if eptr >= mb.end_subject then
    (* pcre2_match.c:4408-4412 — SCHECK_PARTIAL(); break. *)
    let r = scheck_partial mb eptr in
    if r < 0 then r
    else (rep_greedy_done_dispatch [@tailcall]) mb eptr sp rdepth mcc
  else
    (* pcre2_match.c:4413-4418 — GETCHARINCTEST + PRIV(extuni). *)
    let e' = extuni_at mb eptr in
    (* CHECK_PARTIAL() (4419). *)
    let r = if e' >= mb.end_subject then scheck_partial mb e' else 0 in
    if r < 0 then r
    else (rep_greedy_extuni [@tailcall]) mb (i + 1) e' sp rdepth mcc

(* After a greedy scan to [pmax]: a CLASS repeat (pcre2_match.c:2143 `>=`,
   RM24/RM201) AND an XCLASS repeat (pcre2_match.c:2278-2288, RM101 — the same
   RMATCH-first for(;;), floor-inclusive) backtrack with the floor position
   itself a ticked child; every other kind tries the floor in place. *)
and rep_greedy_done_dispatch (mb : mb) (pmax : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  if Int.equal mb.rep_kind rk_class || Int.equal mb.rep_kind rk_xclass then
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
      Array.unsafe_set d (sp + 1) (rep_giveback mb mb.rep_pc mb.rep_floor pmax);
      Array.unsafe_set d (sp + 2) mb.rep_floor;
      Array.unsafe_set d (sp + 3) rdepth;
      Array.unsafe_set d (sp + 4) Save_stack.kind_rep_max;
      (run [@tailcall]) mb mb.rep_cont pmax need (rdepth + 1) mcc')

(* CLASS maximize (pcre2_match.c:2143-2150) / XCLASS maximize
   (pcre2_match.c:2278-2288, RM101): the floor position is itself a ticked
   RMATCH (`while Feptr >= Lstart` / the RMATCH-first for(;;)), so ALWAYS push
   (even pmax == floor) and give back down to and including floor; below floor
   -> NOMATCH. *)
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
      Array.unsafe_set d (sp + 1)
        (rep_giveback mb mb.rep_pc mb.rep_floor pmax)
        (* next give-back: one char (UTF) / code unit back; pmax = floor stores
           floor-1 (below floor -> next backtrack pops) *);
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
    if is_newline_at mb eptr then (bt [@tailcall]) mb eptr sp mcc
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
      if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc)
    else
      (* OP_ANY falls through to the OP_ALLANY tail: Feptr++; ACROSSCHAR
         (pcre2_match.c:962-971; interpreter.ml op_allany_tail:4227-4229). NOT
         cp_len (see [any_advance]): after \C the eptr can be mid-character. *)
      (run [@tailcall]) mb (pc + 2) (any_advance mb eptr) sp rdepth mcc)
  else if Int.equal type_op Opcodes.op_anynl then (
    (* OP_ANYNL \R (pcre2_match.c:2377-2409; GETCHARINCTEST decodes a code
       point in UTF — NEL/LS/PS are multi-byte, chunk I2). *)
    if eptr >= mb.end_subject then (
      let r = scheck_partial mb eptr in
      if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc)
    else
      (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
      let fc = if mb.utf then cur_cp mb eptr else Char.code (String.unsafe_get mb.subject eptr) in
      let e = eptr + if mb.utf then cp_len mb eptr else 1 in
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
        || Int.equal fc 0x2028 || Int.equal fc 0x2029
      then
        if mb.bsr_anycrlf then
          (* Chunk K1b — the ANYCRLF rejection comes after GETCHARINCTEST
             (pcre2_match.c:2383/2406): record the post-read position [e]. *)
          (bt [@tailcall]) mb e sp mcc
        else (run [@tailcall]) mb (pc + 2) e sp rdepth mcc
      else
        (* Chunk K1b — the default rejection also follows the read
           (pcre2_match.c:2383-2386): record [e]. *)
        (bt [@tailcall]) mb e sp mcc)
  else if eptr >= mb.end_subject then (
    (* the simple 1-unit predicate types (\d \D \s \S \w \W / \h \H \v \V /
       OP_ALLANY / OP_ANYBYTE): SCHECK_PARTIAL at/past end, then NOMATCH. *)
    let r = scheck_partial mb eptr in
    if r < 0 then r else (bt [@tailcall]) mb eptr sp mcc)
  else if mb.utf then
    (* Chunk I — decode the code point; predicate is code-point aware. OP_ANYBYTE
       (\C) consumes exactly ONE code unit even in UTF (interpreter.ml:1456-1470);
       every other type consumes the whole character. safe: eptr < end_subject. *)
    let cp = cur_cp mb eptr in
    if simple_type_match_cp type_op cp then
      let e =
        if Int.equal type_op Opcodes.op_anybyte then eptr + 1
          (* \C: exactly one code unit, no ACROSSCHAR (pcre2_match.c:984-988). *)
        else if Int.equal type_op Opcodes.op_allany then any_advance mb eptr
          (* OP_ALLANY: Feptr++; ACROSSCHAR (pcre2_match.c:962-971) — after \C
             the eptr can be mid-character, where cp_len would under-consume. *)
        else eptr + cp_len mb eptr
          (* per-char predicate types: GETCHARINCTEST (pcre2_match.c:2305-2470). *)
      in
      (run [@tailcall]) mb (pc + 2) e sp rdepth mcc
    else
      (* Chunk K1b — every single predicate type GETCHARINCTESTs before its
         test (pcre2_match.c:2305-2470): a mismatch records the post-read
         position (OP_ALLANY/OP_ANYBYTE never mismatch — end-only above). *)
      (bt [@tailcall]) mb (eptr + cp_len mb eptr) sp mcc
  else
    (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
    let cc = Char.code (String.unsafe_get mb.subject eptr) in
    if simple_type_match type_op cc then
      (run [@tailcall]) mb (pc + 2) (eptr + 1) sp rdepth mcc
    else
      (* Chunk K1b — GETCHARINCTEST non-UTF = fc = *Feptr++ (pcre2_match.c:
         2311-2312 etc.): record eptr + 1. *)
      (bt [@tailcall]) mb (eptr + 1) sp mcc

(* WORDBOUND \b / \B, non-UCP (pcre2_match.c:6258-6333 /
   interpreter.ml:3181-3332). [want] = 1 for \b (OP_WORD_BOUNDARY: a boundary
   is required, so NOMATCH when cur == prev), 0 for \B (the reverse). The
   previous-char read lowers [start_used_ptr] (the SCHECK_PARTIAL floor)
   exactly as the C's earliest-consulted tracking (pcre2_match.c:6280); the
   latest-consulted (last_used_ptr) only feeds rightchar/allusedtext, which the
   fast seam does not carry, so it is not tracked. *)
and op_wordbound (mb : mb) (pc : int) (want : int) (eptr : int) (sp : int)
    (rdepth : int) (mcc : int) : int =
  (* [want] bit 0 = boundary wanted, bit 1 = the UCP variant
     (OP_UCP_WORD_BOUNDARY / OP_NOT_UCP_WORD_BOUNDARY, chunk I2): the word
     test uses Unicode properties (Char_predicates.ucp_wordchar,
     pcre2_match.c:6283-6289/6316-6322), even without UTF. *)
  let ucp_op = Int.equal (want land 2) 2 in
  (* pcre2_match.c:6271-6292 — the previous character's word status. The floor
     is mb.check_subject (interpreter.ml:3183): non-UTF this is the subject
     start (0), UTF the backed-up UTF-check start (or an invalid-UTF
     fragment's start, chunk I2). In UTF the previous char is BACKCHAR'd and
     decoded (:6271-6277). Non-UCP: a char > 255 is not a word char
     (CHMAX_255 guard, :6292). *)
  let prev_is_word =
    if Int.equal eptr mb.check_subject then false
    else if mb.utf then (
      (* lastptr = BACKCHAR(eptr - 1) via the allocation-free [backchar_sub]
         (§8 — Utf.backchar's int-ref would allocate per probe); a landing on a
         continuation byte resolves to -1 where Utf.peek reads 0
         (interpreter.ml:3193-3209's crossing pin — reachable via a nested
         lookbehind or an invalid-UTF fragment start). *)
      let lastptr =
        if eptr - 1 < 0 then -1
        else
          let p = backchar_sub mb.subject (eptr - 1) in
          if Int.equal (Utf.peek mb.subject p land 0xc0) 0x80 then -1 else p
      in
      let fc =
        let c0 = Utf.peek mb.subject lastptr in
        if c0 >= 0xc0 then Utf.getutf8 c0 mb.subject lastptr else c0
      in
      if lastptr < mb.start_used_ptr then mb.start_used_ptr <- lastptr;
      if ucp_op then Char_predicates.ucp_wordchar fc
      else
        fc <= 255
        && not (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0))
    else
      let lastptr = eptr - 1 in
      (* safe: 0 <= lastptr < eptr <= end_subject <= String.length subject. *)
      let fc = Char.code (String.unsafe_get mb.subject lastptr) in
      if lastptr < mb.start_used_ptr then mb.start_used_ptr <- lastptr;
      if ucp_op then Char_predicates.ucp_wordchar fc
      else not (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0)
  in
  if eptr >= mb.end_subject then (
    let r = scheck_partial mb eptr in
    if r < 0 then r
    else (word_bound_test [@tailcall]) mb pc want prev_is_word false eptr sp rdepth mcc)
  else (
    (* pcre2_match.c:6304-6314 — the next-character probe consults subject[eptr],
       so nextptr = FORWARDCHARTEST(eptr) (eptr + the char length in UTF, eptr+1
       otherwise) raises last_used_ptr (the RECURSELOOP input); gated on
       has_recurse. *)
    if mb.has_recurse then (
      let nextptr = if mb.utf then eptr + cp_len mb eptr else eptr + 1 in
      if nextptr > mb.last_used_ptr then mb.last_used_ptr <- nextptr);
    (* safe: eptr < mb.end_subject <= String.length mb.subject. *)
    let fc = if mb.utf then cur_cp mb eptr else Char.code (String.unsafe_get mb.subject eptr) in
    let cur_is_word =
      if ucp_op then Char_predicates.ucp_wordchar fc
      else
        fc <= 255
        && not (Int.equal (Chartables.ctypes fc land Chartables.ctype_word) 0)
    in
    (word_bound_test [@tailcall]) mb pc want prev_is_word cur_is_word eptr sp
      rdepth mcc)

and word_bound_test (mb : mb) (pc : int) (want : int) (prev_is_word : bool)
    (cur_is_word : bool) (eptr : int) (sp : int) (rdepth : int) (mcc : int) : int
    =
  (* pcre2_match.c:6328-6332 — \b: NOMATCH when cur == prev; \B: NOMATCH when
     cur != prev ([want] bit 0). *)
  let fail =
    if Int.equal (want land 1) 1 then Bool.equal cur_is_word prev_is_word
    else not (Bool.equal cur_is_word prev_is_word)
  in
  if fail then (bt [@tailcall]) mb eptr sp mcc
  else (run [@tailcall]) mb (pc + 2) eptr sp rdepth mcc

and op_eod (mb : mb) (pc : int) (eptr : int) (sp : int) (rdepth : int)
    (mcc : int) : int =
  (* pcre2_match.c:6154-6163 (interpreter.ml op_eod_tail) — end of subject:
     \z tests the TRUE end (mb->true_end_subject), not a fragment end (chunk
     I2; pcre2_match.c:6156). *)
  if eptr < mb.true_end_subject then (bt [@tailcall]) mb eptr sp mcc
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
      else (bt [@tailcall]) mb eptr sp mcc)
    else (bt [@tailcall]) mb eptr sp mcc
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

(* pcre2_match.c:7703-7766 (interpreter.ml endloop_tail :9499-9553) — the
   driver epilogue for every non-match outcome at this seam (a MATCH returns
   from run_attempt directly; [exec] builds its outcome): pass any real error
   through (7743-7745); construct the partial result when one was recorded
   (7747-7762: ovector[0] = match_partial, ovector[1] = end_subject); else the
   classic NOMATCH (7764-7766). *)
let endloop_tail (mb : mb) (rc : int) : int =
  if
    (not (Int.equal rc match_nomatch))
    && not (Int.equal rc Errors.error_partial)
  then rc
  else if mb.match_partial >= 0 then (
    mb.match_start <- mb.match_partial;
    mb.match_end <- mb.end_subject;
    Errors.error_partial)
  else Errors.error_nomatch

(* pcre2_match.c:7502 — mb->moptions = options | fragment_options: the
   invalid-UTF fragment driver ORs NOTBOL/NOTEOL into the match options per
   fragment (chunk I2; fragment_options set at 6920-6926 / 7683 / 7694). The
   fast mb precomputes the effective notbol/noteol booleans; recompute them
   from the caller's base flags at each fragment (re)start. *)
let set_frag_options (mb : mb) (frag : int) : unit =
  mb.notbol <- mb.base_notbol || not (Int.equal (frag land Options.notbol) 0);
  mb.noteol <- mb.base_noteol || not (Int.equal (frag land Options.noteol) 0)

(* pcre2_intmodedep.h:352-353 ACROSSCHAR — advance [pos] past any UTF-8
   continuation bytes, bounded by end_subject (the bump-along / STARTLINE
   char-wise stepping, pcre2_match.c:7326-7332/7561-7563). Non-UTF: identity. *)
let across_char (mb : mb) (pos : int) : int =
  if mb.utf then (
    let p = ref pos in
    while
      !p < mb.end_subject
      (* safe: !p < end_subject <= String.length subject. *)
      && Int.equal (Char.code (String.unsafe_get mb.subject !p) land 0xc0) 0x80
    do
      incr p
    done;
    !p)
  else pos

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
      if not ok then (endloop [@tailcall]) mb match_nomatch req_cu_ptr
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
      (* pcre2_match.c:7326-7332 — step by CHARACTERS (ACROSSCHAR in UTF). *)
      while !sm < mb.end_subject && not (was_newline_at mb !sm) do
        sm := across_char mb (!sm + 1)
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
  if Int.equal mb.partial 0 && start_match >= mb.end_subject then
    (endloop [@tailcall]) mb match_nomatch req_cu_ptr
  else (tail_opts [@tailcall]) mb start_match req_cu_ptr

and tail_opts (mb : mb) (start_match : int) (req_cu_ptr : int) : int =
  (* pcre2_match.c:7378-7481 (interpreter.ml:9235-9301) — minlength and req_cu.
     Both are DISABLED for partial matching (a lower bound on a COMPLETE
     match), so partial keeps every attempt. *)
  if Int.equal mb.partial 0 then
    if mb.end_subject - start_match < mb.minlength then
      (endloop [@tailcall]) mb match_nomatch req_cu_ptr
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
          if np >= mb.end_subject then
            (endloop [@tailcall]) mb match_nomatch req_cu_ptr
          else (run_attempt [@tailcall]) mb start_match np
        else (run_attempt [@tailcall]) mb start_match req_cu_ptr
      else (run_attempt [@tailcall]) mb start_match req_cu_ptr
  else (run_attempt [@tailcall]) mb start_match req_cu_ptr

and run_attempt (mb : mb) (start_match : int) (req_cu_ptr : int) : int =
  (* pcre2_match.c:7485-7577 (interpreter.ml:9287-9365) — bumpalong-limit
     check, per-attempt resets, match(), then the rc switch. *)
  (* bumpalong_limit = true_end_subject (pcre2_match.c:6945 with an unset
     offset limit); a fragment's shortened end_subject bounds bump_bottom
     instead (7593). *)
  if start_match > mb.true_end_subject then
    (endloop [@tailcall]) mb match_nomatch req_cu_ptr
  else (
    mb.attempt_start <- start_match;
    mb.start_used_ptr <- start_match;
    (* Chunk K1b — per-attempt recursion resets (pcre2_match.c:7500/651-655):
       last_used_ptr = start_match (the RECURSELOOP input's monotone base);
       not-in-a-recursion (current_recurse = RECURSE_UNSET, no open KIND_RECURSE). *)
    mb.last_used_ptr <- start_match;
    mb.current_recurse <- recurse_unset;
    mb.recurse_base <- -1;
    mb.once_base <- -1 (* no open atomic construct at attempt start (chunk G) *);
    mb.skip_arg_count <- 0 (* pcre2_match.c:7513 — per-attempt SKIP_ARG count *);
    mb.mark <- Frames.unset (* frame-0 Fmark reset, pcre2_match.c:8603 site *);
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
    (* pcre2_match.c:7527-7577 (interpreter.ml:9328-9374) — the rc switch,
       including the backtracking-verb returns from [backtrack_code]. *)
    if Int.equal rc match_match then rc (* mb.match_start/end set by END *)
    else if Int.equal rc Errors.error_partial then
      (* hard partial: goto ENDLOOP with the error (pcre2_match.c:7573-7576)
         — in a non-final invalid-UTF fragment the carry-on continues to the
         next fragment (7643-7647, chunk I2); otherwise endloop_tail builds
         the partial result. *)
      (endloop [@tailcall]) mb rc req_cu_ptr
    else if Int.equal rc match_skip_arg then (
      (* pcre2_match.c:7529-7539 — a MARK matching the SKIP arg was not found:
         re-run at the SAME start position with ignore_skip_arg = skip_arg_count
         (SKIP_ARGs up to that count become no-ops). *)
      mb.ignore_skip_arg <- mb.skip_arg_count;
      (bump_bottom [@tailcall]) mb start_match req_cu_ptr)
    else if Int.equal rc match_skip && mb.verb_skip_ptr > start_match then
      (* pcre2_match.c:7541-7549 — SKIP passes back a target > the current start:
         advance directly to it. *)
      (bump_bottom [@tailcall]) mb mb.verb_skip_ptr req_cu_ptr
    else if
      Int.equal rc match_nomatch || Int.equal rc match_prune
      || Int.equal rc match_then
      || Int.equal rc match_skip (* SKIP whose target <= start: fall through *)
    then (
      (* pcre2_match.c:7552-7565 — NOMATCH / PRUNE / THEN (and a fallen-through
         SKIP) advance by one CHARACTER (ACROSSCHAR in UTF). Unset
         ignore_skip_arg. *)
      mb.ignore_skip_arg <- 0;
      (bump_bottom [@tailcall]) mb (across_char mb (start_match + 1)) req_cu_ptr)
    else if Int.equal rc match_commit then
      (* pcre2_match.c:7567-7571 — COMMIT disables bumpalong: rc = NOMATCH,
         goto ENDLOOP. *)
      (endloop [@tailcall]) mb match_nomatch req_cu_ptr
    else
      (* pcre2_match.c:7573-7576 — any other rc (limit / heap error) goes to
         ENDLOOP, which passes it through. *)
      (endloop [@tailcall]) mb rc req_cu_ptr)

and bump_bottom (mb : mb) (new_start_match : int) (req_cu_ptr : int) : int =
  (* pcre2_match.c:7579-7616 (interpreter.ml:9376-9430) — advance to
     [new_start_match] (set by the NOMATCH/verb rc switch), stop if anchored or
     past the end, apply the CRLF skip, reset the per-attempt mark. Firstline is
     declined at compile. *)
  if mb.anchored || new_start_match > mb.end_subject then
    (endloop [@tailcall]) mb match_nomatch req_cu_ptr
  else
    let sm =
      if
        new_start_match > mb.start_subject + mb.start_offset
        (* safe: 1 <= new_start_match <= end_subject <= String.length subject. *)
        && Int.equal
             (Char.code (String.unsafe_get mb.subject (new_start_match - 1)))
             Newline.char_cr
        && new_start_match < mb.end_subject
        && Int.equal
             (Char.code (String.unsafe_get mb.subject new_start_match))
             Newline.char_lf
        && (not mb.hascrorlf)
        && (Int.equal mb.nltype Newline.nltype_any
           || Int.equal mb.nltype Newline.nltype_anycrlf
           || Int.equal mb.nllen 2)
      then new_start_match + 1
      else new_start_match
    in
    (bump_top [@tailcall]) mb sm req_cu_ptr

and fragment_restart (mb : mb) (start_match : int) (req_cu_ptr : int) : int =
  (* pcre2_match.c:7139-7148 (interpreter.ml fragment_restart) —
     FRAGMENT_RESTART: (re)set the per-fragment loop state, then enter the
     bump-along loop. Also the initial entry point (the C falls through the
     label; the 8-bit memchr caches are chunk L, absent here). *)
  mb.match_partial <- -1 (* start_partial = match_partial = NULL, 7141 *);
  mb.hitend <- false (* 7144 *);
  (bump_top [@tailcall]) mb start_match req_cu_ptr

and endloop (mb : mb) (rc : int) (req_cu_ptr : int) : int =
  (* pcre2_match.c:7637-7648 (interpreter.ml endloop) — ENDLOOP: if
     end_subject != true_end_subject we are handling invalid UTF and have just
     processed a non-terminal fragment (chunk I2); on NOMATCH / PARTIAL carry
     on to the next fragment (a partial match is returned to the caller only
     at the very end of the subject). *)
  if
    mb.utf
    && (not (Int.equal mb.end_subject mb.true_end_subject))
    && (Int.equal rc match_nomatch || Int.equal rc Errors.error_partial)
  then (next_fragment [@tailcall]) mb mb.end_subject req_cu_ptr
  else endloop_tail mb rc

and next_fragment (mb : mb) (frag_end : int) (req_cu_ptr : int) : int =
  (* pcre2_match.c:7650-7699 (interpreter.ml next_fragment) — the fragment
     carry-on for(;;): a loop is used to avoid trying to match against empty
     fragments (if the pattern can match an empty string it would have done so
     already). Each iteration enters with the previous fragment's end in
     [frag_end] (the C's end_subject). *)
  (* pcre2_match.c:7652-7659 — advance past the first bad code unit, then skip
     invalid character starting code units. *)
  let sm = ref (frag_end + 1) in
  while
    !sm < mb.true_end_subject && Utf.not_firstcu (Char.code mb.subject.[!sm])
  do
    incr sm
  done;
  if !sm >= mb.true_end_subject then (
    (* pcre2_match.c:7662-7670 — hit the end of the subject: there isn't
       another non-empty fragment, so give up. rc = MATCH_NOMATCH in case it
       was partial; match_partial = NULL. *)
    mb.match_partial <- -1;
    endloop_tail mb match_nomatch)
  else (
    (* pcre2_match.c:7672-7676 — check the rest of the subject. *)
    mb.check_subject <- !sm;
    let erroroffset = ref 0 in
    let vrc =
      Valid_utf.valid_utf mb.subject ~start:!sm
        ~length:(mb.true_end_subject - !sm)
        erroroffset
    in
    if Int.equal vrc 0 then (
      (* pcre2_match.c:7678-7685 — the rest of the subject is valid UTF. *)
      mb.end_subject <- mb.true_end_subject;
      set_frag_options mb Options.notbol;
      (fragment_restart [@tailcall]) mb !sm req_cu_ptr)
    else (
      (* pcre2_match.c:7687-7698 — a subsequent UTF error: if the next
         fragment is non-empty, set up to process it; otherwise let the loop
         advance. The C wrote the fragment-relative error offset into
         match_data->startchar through the out-pointer at 7675-7676 (dead
         unless an error return follows). *)
      mb.startchar <- !erroroffset;
      mb.end_subject <- !sm + !erroroffset;
      if mb.end_subject > !sm then (
        set_frag_options mb (Options.notbol lor Options.noteol);
        (fragment_restart [@tailcall]) mb !sm req_cu_ptr)
      else (next_fragment [@tailcall]) mb mb.end_subject req_cu_ptr))

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
   bounds; NOMATCH / errors carry only [orc]. [omark] is the mark result as a
   byte OFFSET into re.code (chunk H): mb.mark on a successful match, else
   mb.nomatch_mark (pcre2_match.c:7714/7741); -1 = no mark. The seam decodes it
   with mark_of_offset. *)
type outcome = { orc : int; ostart : int; oend : int; ovec : int array; omark : int }

let entry_error (rc : int) : outcome =
  { orc = rc; ostart = 0; oend = 0; ovec = [||]; omark = -1 }

(* Chunk I — a UTF error carries its absolute error offset in [ostart] (the
   seam maps it to Engine.Error's start_char). *)
let entry_error_off (rc : int) (off : int) : outcome =
  { orc = rc; ostart = off; oend = 0; ovec = [||]; omark = -1 }

(* pcre2_match.c:6807-6929 (interpreter.ml:9668-9772) — the per-exec UTF
   check, since chunk I2 including the PCRE2_MATCH_INVALID_UTF path. Returns
   [Ok (start_match, end_subject, check_subject, fragment_options, startchar)]
   or [Error (rc, abs_offset)]:

   - with [allow_invalid], bad start code units are skipped (6828-6836), and
     the validation loop may truncate [end_subject] to stop before a bad code
     unit (6896-6926) — setting NOTEOL as the first fragment's option and
     recording the absolute error offset in [startchar] (the C's
     match_data->startchar, dead unless an error return follows);
   - without it, an invalid first code unit is BADUTFOFFSET (offset > 0) /
     the isolated-0x80 error (offset 0), and an invalid sequence is the
     valid_utf code with its absolute offset (6837-6845 / 6896-6903);
   - [check_subject] is the earliest position a lookbehind / \b prev probe
     may read: [start_match] backed up by max_lookbehind CHARACTERS, skipped
     when bad start units were skipped (6847-6872). *)
let utf_entry_check (re : Compile.re) (subject : string) (offset : int)
    (length : int) (allow_invalid : bool) :
    (int * int * int * int * int, int * int) result =
  let start_match = ref offset in
  let end_subject = ref length in
  let fragment_options = ref 0 in
  let startchar = ref 0 in
  (* pcre2_match.c:6820-6845 — the first code unit must be a character
     start. *)
  let skipped_bad_start = ref false in
  let first_cu_err =
    if allow_invalid then (
      (* pcre2_match.c:6828-6836 — just skip over such code units. *)
      while
        !start_match < !end_subject
        && Utf.not_firstcu (Char.code subject.[!start_match])
      do
        incr start_match;
        skipped_bad_start := true
      done;
      0)
    else if
      !start_match < !end_subject
      && Utf.not_firstcu (Char.code subject.[!start_match])
    then
      if offset > 0 then Errors.error_badutfoffset
      else Errors.error_utf8_err20 (* isolated 0x80 byte *)
    else 0
  in
  if first_cu_err < 0 then Error (first_cu_err, 0)
  else (
    (* pcre2_match.c:6847-6872 — back up max_lookbehind characters (skipping
       continuation bytes), but don't do this if we skipped bad code units
       above. *)
    let cs = ref !start_match in
    (if not !skipped_bad_start then
       let i = ref re.Compile.max_lookbehind in
       while !i > 0 && !cs > 0 do
         decr cs;
         while !cs > 0 && Int.equal (Char.code subject.[!cs] land 0xc0) 0x80 do
           decr cs
         done;
         decr i
       done);
    (* pcre2_match.c:6885-6928 — validate [cs, end); a loop in case we
       encounter bad UTF in the characters preceding start_match which we are
       scanning because of a lookbehind. *)
    let rec validate () : int =
      let erroroffset = ref 0 in
      let vrc =
        Valid_utf.valid_utf subject ~start:!cs ~length:(length - !cs)
          erroroffset
      in
      if Int.equal vrc 0 then 0 (* valid UTF string (6894) *)
      else (
        (* pcre2_match.c:6896-6903 — adjust the offset to be absolute; if
           handling invalid UTF, set end_subject to stop before the bad code
           unit, otherwise return the error. *)
        startchar := !erroroffset + !cs;
        if not allow_invalid then vrc
        else (
          end_subject := !startchar;
          if !end_subject < !start_match then (
            (* pcre2_match.c:6905-6918 — the end precedes start_match: the
               invalid UTF is in the extra code units we reversed over
               because of a lookbehind. Advance past the first bad code unit,
               skip invalid character starting code units, and try again with
               the original end point. *)
            cs := !end_subject + 1;
            while
              !cs < !start_match && Utf.not_firstcu (Char.code subject.[!cs])
            do
              incr cs
            done;
            end_subject := length;
            (validate [@tailcall]) ())
          else (
            (* pcre2_match.c:6920-6926 — set the not end of line option and
               do the match. *)
            fragment_options := Options.noteol;
            0)))
    in
    let v = validate () in
    if v < 0 then Error (v, !startchar)
    else Ok (!start_match, !end_subject, !cs, !fragment_options, !startchar))

let exec (ir : Ir.t) ~(subject : string) ~(offset : int) ~(options : int) :
    outcome =
  let re = ir.Ir.re in
  let length = String.length subject in
  let utf = not (Int.equal (re.Compile.overall_options land Options.utf) 0) in
  let ucp = not (Int.equal (re.Compile.overall_options land Options.ucp) 0) in
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
      (* pcre2_match.c:6807-6929 — the per-exec UTF validity check (chunk I;
         I2 added the PCRE2_MATCH_INVALID_UTF path, which forces the check
         even under NO_UTF_CHECK, 6811). A UTF error short-circuits here with
         its absolute offset; otherwise the check yields the (possibly
         advanced) start, the (possibly truncated) fragment end, the
         lookbehind / \b prev-probe floor, the first fragment's NOTBOL/NOTEOL
         options and the recorded startchar. *)
      let allow_invalid =
        not
          (Int.equal
             (re.Compile.overall_options land Options.match_invalid_utf)
             0)
      in
      match
        if
          utf
          && (Int.equal (options land Options.no_utf_check) 0 || allow_invalid)
        then utf_entry_check re subject offset length allow_invalid
        else Ok (offset, length, 0, 0, 0)
        (* mb->check_subject = subject (pcre2_match.c:6795) *)
      with
      | Error (rc, off) -> entry_error_off rc off
      | Ok (start_match0, end_subject0, check_subject, frag_opts0, startchar0)
        ->
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
      if heapframes_size0 < 0 then
        (* HEAPLIMIT before any attempt; startchar as the check left it
           (pcre2_match.c:6897 sets it even when allow_invalid proceeds). *)
        entry_error_off heapframes_size0 startchar0
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
        (* Chunk H — verb state. [alt_then_end] is per-pattern (the THEN scope
           boundaries). [nomatch_mark]/[ignore_skip_arg] are per-EXEC (sticky
           across bump attempts, pcre2_match.c:6971-6972); [mark]/[skip_arg_count]
           reset per attempt in run_attempt; [verb_skip_ptr] reset defensively. *)
        mb.alt_then_end <- ir.Ir.alt_then_end;
        mb.once_subtype <- ir.Ir.once_subtype;
        mb.mark <- Frames.unset;
        mb.nomatch_mark <- Frames.unset;
        mb.verb_skip_ptr <- -1;
        mb.verb_then_pc <- 0;
        mb.ignore_skip_arg <- 0;
        mb.skip_arg_count <- 0;
        (* Chunk K1b — recursion state (fast-design.md §3). [has_recurse] gates
           last_used_ptr tracking; [n_groups] sizes the FAT record's group_start
           snapshot; [disable_recurseloop] is the PCRE2_DISABLE_RECURSELOOP_CHECK
           match option (pcre2_match.c:5448). current_recurse/recurse_base reset
           per attempt in run_attempt; verb_current_recurse reset defensively. *)
        mb.has_recurse <- ir.Ir.has_recurse;
        mb.n_groups <- ir.Ir.n_groups;
        mb.verb_current_recurse <- -1;
        mb.disable_recurseloop <-
          not (Int.equal (options land Options.disable_recurseloop_check) 0);
        (* Chunk E: the shared compiler's bytecode (class bitmaps live here) and
           the \R newline convention. *)
        mb.bytecode <- re.Compile.code;
        mb.bsr_anycrlf <-
          Int.equal re.Compile.bsr_convention Options.bsr_anycrlf;
        (* Chunk F — backreferences. MATCH_UNSET_BACKREF is a compile option
           (mb.poptions = re.overall_options, interpreter.ml:9910/913). The name
           table + entry size back the OP_DNREF group-list scan. *)
        mb.match_unset_backref <-
          not (Int.equal (re.Compile.overall_options land Options.match_unset_backref) 0);
        mb.name_table <- re.Compile.name_table;
        mb.name_entry_size <- re.Compile.name_entry_size;
        mb.subject <- subject;
        mb.end_subject <- end_subject0;
        mb.true_end_subject <- length;
        mb.start_subject <- 0;
        mb.start_offset <- offset;
        (* Chunk I — UTF mode + the lookbehind / \b prev-probe floor. Chunk
           I2 — UCP mode + the startchar cell (see the mb fields). *)
        mb.utf <- utf;
        mb.ucp <- ucp;
        mb.check_subject <- check_subject;
        mb.startchar <- startchar0;
        mb.partial <- partial;
        mb.notempty <- not (Int.equal (options land Options.notempty) 0);
        mb.notempty_atstart <-
          not (Int.equal (options land Options.notempty_atstart) 0);
        mb.endanchored <-
          not
            (Int.equal
               (re.Compile.overall_options lor options land Options.endanchored)
               0);
        (* Chunk I2 — the effective NOTBOL/NOTEOL are base option flags OR the
           current fragment's options (pcre2_match.c:7502); the first
           fragment's arrive from the entry check. *)
        mb.base_notbol <- not (Int.equal (options land Options.notbol) 0);
        mb.base_noteol <- not (Int.equal (options land Options.noteol) 0);
        set_frag_options mb frag_opts0;
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
        (* Chunk F: referenced-capture in-progress starts (t_cap_start_ref),
           indexed by ovbase; sized like the ovector, reused/grown across execs.
           Written before any read (like group_start), so no per-attempt reset. *)
        if Array.length mb.cap_start < 2 * oveccount then
          mb.cap_start <- Array.make (2 * oveccount) 0;
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
           interpreter.ml:9968-10032). first_cu2 / req_cu2 use the fcc fold;
           in the 8-bit library the UCD other case applies only when UCP is
           set without UTF (:7101-7107 / :7123-7129, PCRE2_UCHAR truncation —
           chunk I2). *)
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
           then
             if first_cu > 127 && ucp && not utf then
               Ucd.othercase first_cu land 0xff
             else Chartables.fcc first_cu
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
           then
             if req_cu > 127 && ucp && not utf then
               Ucd.othercase req_cu land 0xff
             else Chartables.fcc req_cu
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
        (* pcre2_match.c:7139-7151 (interpreter.ml:10084) — enter the
           bump-along loop through FRAGMENT_RESTART (also the initial entry):
           start_match from the entry check (it may have skipped bad code
           units under MATCH_INVALID_UTF), req_cu_ptr one before the ORIGINAL
           offset (the C local). *)
        let rc =
          if not nl_valid then Errors.error_internal
          else fragment_restart mb start_match0 (offset - 1)
        in
        (* Copy results out of the reused [mb] before releasing the scratch.
           A match returns the pcre2 pair count (mb.rc > 0) and its 2*rc-int
           ovector; every other outcome carries only the whole-match bounds
           (meaningful for a partial). *)
        (* [ostart]: the match/partial start for those outcomes; for any other
           negative rc the C's match_data->startchar (the UTF error offset the
           check may have recorded — pcre2_match.c:6897/7675; 0 otherwise),
           exactly what engine.ml exec_full surfaces for Error. *)
        let ostart =
          if Int.equal rc match_match || Int.equal rc Errors.error_partial then
            mb.match_start
          else mb.startchar
        and oend = mb.match_end in
        let orc, ovec =
          if Int.equal rc match_match then (mb.rc, Array.sub mb.ovector 0 (2 * mb.rc))
          else (rc, [||])
        in
        (* pcre2_match.c:7714 / 7741 — the mark: the winning path's mb.mark on a
           full match, else the sticky nomatch_mark on any non-match. *)
        let omark = if Int.equal rc match_match then mb.mark else mb.nomatch_mark in
        if holds then Save_stack.release ();
        { orc; ostart; oend; ovec; omark })

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
  assert (Int.equal Ir.t_ref 32);
  assert (Int.equal Ir.t_ref_rep 33);
  assert (Int.equal Ir.t_dnref 34);
  assert (Int.equal Ir.t_dnref_rep 35);
  assert (Int.equal Ir.t_cap_start_ref 36);
  assert (Int.equal Ir.t_cap_end_ref 37);
  assert (Int.equal Ir.t_reverse 38);
  assert (Int.equal Ir.t_vreverse 39);
  assert (Int.equal Ir.t_once 40);
  assert (Int.equal Ir.t_once_end 41);
  assert (Int.equal Ir.t_assert_end 42);
  assert (Int.equal Ir.t_nassert 43);
  assert (Int.equal Ir.t_nassert_match 44);
  assert (Int.equal Ir.t_assertback_check 45);
  assert (Int.equal Ir.t_possess 46);
  assert (Int.equal Ir.t_ketrpos 47);
  assert (Int.equal Ir.t_possess_done 48);
  assert (Int.equal Ir.t_mark 49);
  assert (Int.equal Ir.t_commit 50);
  assert (Int.equal Ir.t_prune 51);
  assert (Int.equal Ir.t_skip 52);
  assert (Int.equal Ir.t_skip_arg 53);
  assert (Int.equal Ir.t_then 54);
  assert (Int.equal Ir.t_accept 55);
  assert (Int.equal Ir.t_close 56);
  assert (Int.equal Ir.t_xclass 57);
  assert (Int.equal Ir.t_xclass_rep 58);
  assert (Int.equal Ir.t_prop 59);
  assert (Int.equal Ir.t_prop_rep 60);
  assert (Int.equal Ir.t_extuni 61);
  assert (Int.equal Ir.t_extuni_rep 62);
  assert (Int.equal Ir.t_cond_cref 63);
  assert (Int.equal Ir.t_cond_dncref 64);
  assert (Int.equal Ir.t_cond_false 65);
  assert (Int.equal Ir.t_cond_assert 66);
  assert (Int.equal Ir.t_cond_assert_match 67);
  assert (Int.equal Ir.t_scond_descend 68);
  assert (Int.equal Ir.t_script_run_end 69);
  assert (Int.equal Ir.t_recurse 70);
  assert (Int.equal Ir.t_cond_rref 71);
  assert (Int.equal Ir.t_cond_dnrref 72);
  assert (Int.equal Ir.t_fail_nassert 73);
  assert (Int.equal Ir.max_tag 73);
  (* Save-record KIND / width constants the runner inlines as literals. *)
  assert (Int.equal Save_stack.kind_alt 0);
  assert (Int.equal Save_stack.kind_cap 1);
  assert (Int.equal Save_stack.kind_rep_max 2);
  assert (Int.equal Save_stack.kind_rep_min 3);
  assert (Int.equal Save_stack.kind_cont 4);
  assert (Int.equal Save_stack.kind_gstart 5);
  assert (Int.equal Save_stack.kind_capstart 6);
  assert (Int.equal Save_stack.kind_ref_min 7);
  assert (Int.equal Save_stack.kind_ref_max 8);
  assert (Int.equal Save_stack.kind_once 9);
  assert (Int.equal Save_stack.kind_nassert 10);
  assert (Int.equal Save_stack.kind_vreverse 11);
  assert (Int.equal Save_stack.kind_pos 12);
  assert (Int.equal Save_stack.kind_verb 13);
  assert (Int.equal Save_stack.kind_ref_max2 14);
  assert (Int.equal Save_stack.kind_recurse 15);
  assert (Int.equal Save_stack.kind_recurse_ret 16);
  (* Chunk H widened KIND_ALT to carry the THEN scope boundary. *)
  assert (Int.equal Save_stack.width_alt 5)
