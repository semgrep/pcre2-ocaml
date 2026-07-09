(* Compile phase of the pure-OCaml PCRE2 10.44 port: META stream -> bytecode.

   Ported from the back half of vendor/pcre2/src/pcre2_compile.c. This
   module currently holds the compile-phase foundations: the workspace and
   length-overflow constants, the first/required code-unit flag values, the
   LINK_SIZE / IMM2_SIZE store-load primitives (PUT/GET, PUT2/GET2 and
   their INC forms) over the Bytes.t code buffer, the compile_context and
   compile_block records with their pcre2_compile() defaults, and the
   small helpers compile_branch and its callers need early
   (check_workspace_overflow, first_significant_code,
   find_dupname_details), plus compile_branch (chars/escapes, classes,
   repeats, and plain capture/non-capture groups with their bracket
   repeats live) and compile_regex (branch linking, OP_ALT/OP_KET chains;
   mutually recursive with compile_branch exactly as in the C). The
   pcre2_compile() driver is the remaining M1 compile chunk
   (docs/ocaml-engine/02-core-compile-match.md).

   The compiled pattern is a Bytes.t of 8-bit code units (this port is the
   8-bit library, LINK_SIZE = 2); the C's code pointers become int offsets
   into that buffer. *)

(* ---------- Code parameters (pcre2_compile.c:143-200) ---------- *)

(* pcre2_compile.c:151-166 — COMPILE_WORK_SIZE: the size (in code units) of
   the workspace. During the first compiling phase, when determining how
   much memory is required, the regex is partly compiled into this space,
   but the compiled parts are discarded as soon as they can be, so that
   hopefully there will never be an overrun. The code does, however, check
   for an overrun. The size depends on LINK_SIZE. *)
let compile_work_size = 3000 * Limits.link_size

(* pcre2_compile.c:177-180 — the overrun tests check for a slightly smaller
   size so that they detect the overrun before it actually does run off the
   end of the data block. *)
let work_size_safety_margin = 100

(* pcre2_compile.c:195-200 — OFLOW_MAX: maximum length value to check
   against when making sure that the variable that holds the compiled
   pattern length does not overflow. A bit less than INT_MAX (the C int
   maximum, 0x7fffffff) to allow for adding in group terminating code
   units, so that they don't have to be checked every time. *)
let oflow_max = 0x7fff_ffff - 20

(* pcre2_compile.c:388-396 — values and flags for the unsigned xxcuflags
   variables that accompany the xxcu variables, which are concerned with
   first and required code units. A value greater than or equal to REQ_NONE
   means "no code unit set"; otherwise the matching xxcu variable is set,
   and the low valued bits are relevant. (Unsigned uint32 comparisons hold
   on these plain-int encodings: 0xffff_ffff is the largest.) *)
let req_unset = 0xffff_ffff (* Not yet found anything *)
let req_none = 0xffff_fffe (* Found not fixed character *)
let req_caseless = 0x0000_0001 (* Code unit in xxcu is caseless *)
let req_vary = 0x0000_0002 (* Code unit is followed by non-literal *)

(* pcre2_internal.h:536,547 — compiled-pattern flag bits recorded in
   cb->external_flags by compile_branch (the parse-phase bits it sets live
   in Parse: hasbkporx, jchanged, dupcapused). *)
let hascrorlf = 0x0000_0800 (* PCRE2_HASCRORLF: explicit \r or \n in pattern *)
let hasbkc = 0x0040_0000 (* PCRE2_HASBKC: contains \C *)

(* pcre2_internal.h:526-548 — the remaining re->flags bits the
   pcre2_compile() driver sets (names drop the PCRE2_ prefix). The
   serialization/allocator bits (PCRE2_DEREF_TABLES) have no role in the
   pure port. *)
let mode8 = 0x0000_0001 (* PCRE2_MODE8: compiled in 8 bit mode *)
let firstset = 0x0000_0010 (* PCRE2_FIRSTSET: first_codeunit is set *)
let firstcaseless = 0x0000_0020 (* PCRE2_FIRSTCASELESS *)
let firstmapset = 0x0000_0040 (* PCRE2_FIRSTMAPSET: start bitmap set (M5) *)
let lastset = 0x0000_0080 (* PCRE2_LASTSET: last_codeunit is set *)
let lastcaseless = 0x0000_0100 (* PCRE2_LASTCASELESS *)
let startline = 0x0000_0200 (* PCRE2_STARTLINE: start after \n for multiline *)
let match_empty = 0x0000_2000 (* PCRE2_MATCH_EMPTY: can match empty string *)
let bsr_set = 0x0000_4000 (* PCRE2_BSR_SET: BSR was set in the pattern *)
let nl_set = 0x0000_8000 (* PCRE2_NL_SET: newline was set in the pattern *)
let notempty_set = 0x0001_0000 (* PCRE2_NOTEMPTY_SET: ( *NOTEMPTY) used *)
let ne_atst_set = 0x0002_0000 (* PCRE2_NE_ATST_SET: ( *NOTEMPTY_ATSTART) *)
let nojit = 0x0008_0000 (* PCRE2_NOJIT: ( *NOJIT) used *)
let hasaccept = 0x0080_0000 (* PCRE2_HASACCEPT: contains ( *ACCEPT) (M5) *)

(* ---------- LINK_SIZE / IMM2_SIZE store-load primitives ---------- *)

(* pcre2_intmodedep.h:104-107 — PUT(a,n,d) for the 8-bit library with
   LINK_SIZE = 2: a link offset is stored big-endian in two code units
   (port-conventions.md §3 pins these exact shapes). The (PCRE2_UCHAR)
   casts become `land 0xff` masks. *)
let put (code : Bytes.t) (n : int) (d : int) : unit =
  Bytes.set code n (Char.chr ((d lsr 8) land 0xff));
  Bytes.set code (n + 1) (Char.chr (d land 0xff))

(* pcre2_intmodedep.h:108-109 — GET(a,n): fetch a LINK_SIZE = 2 big-endian
   offset. Char.code yields 0..255, so the result is already "unsigned". *)
let get (code : Bytes.t) (n : int) : int =
  (Char.code (Bytes.get code n) lsl 8) lor Char.code (Bytes.get code (n + 1))

(* pcre2_intmodedep.h:192-195 — PUT2/GET2: 16-bit quantities (repeat
   counts, capture numbers, ...) that do not change when LINK_SIZE changes;
   IMM2_SIZE = 2 code units in 8-bit mode, same big-endian layout as
   PUT/GET. Kept as separate functions because the C keeps separate
   macros. *)
let put2 (code : Bytes.t) (n : int) (d : int) : unit =
  Bytes.set code n (Char.chr ((d lsr 8) land 0xff));
  Bytes.set code (n + 1) (Char.chr (d land 0xff))

let get2 (code : Bytes.t) (n : int) : int =
  (Char.code (Bytes.get code n) lsl 8) lor Char.code (Bytes.get code (n + 1))

(* pcre2_intmodedep.h:547-548 — PUTINC(a,n,d) / PUT2INC(a,n,d): store, then
   advance the write pointer by LINK_SIZE / IMM2_SIZE. The C advances the
   code pointer `a` itself (call sites pass n = 0); here the write cursor
   is an int-offset ref into the code buffer. *)
let putinc (code : Bytes.t) (cursor : int ref) (d : int) : unit =
  put code !cursor d;
  cursor := !cursor + Limits.link_size

let put2inc (code : Bytes.t) (cursor : int ref) (d : int) : unit =
  put2 code !cursor d;
  cursor := !cursor + Limits.imm2_size

(* ---------- Compile context ---------- *)

(* pcre2_intmodedep.h:563-577 — pcre2_real_compile_context.
   DEVIATION: the memctl (custom allocator) and stack_guard callback
   fields are dropped — a pure-OCaml port has no custom allocators, and
   compile contexts beyond these knobs are out of scope (see
   docs/ocaml-engine/00-architecture.md). The tables pointer is likewise
   dropped: this port always uses the default character tables (the
   Chartables module). *)
type compile_context = {
  max_pattern_length : int; (* PCRE2_SIZE *)
  max_pattern_compiled_length : int; (* PCRE2_SIZE *)
  bsr_convention : int; (* uint16_t *)
  newline_convention : int; (* uint16_t *)
  parens_nest_limit : int; (* uint32_t *)
  extra_options : int; (* uint32_t *)
  max_varlookbehind : int; (* uint32_t *)
}

(* pcre2_context.c:130-145 — the default compile context, used when no
   context is supplied to pcre2_compile(). Length limits are PCRE2_UNSET;
   BSR_DEFAULT = PCRE2_BSR_UNICODE (pcre2_internal.h:255-261, non-EBCDIC
   build); NEWLINE_DEFAULT = 2 = PCRE2_NEWLINE_LF (config.h.generic:235). *)
let default_compile_context =
  {
    max_pattern_length = Parse.pcre2_unset;
    max_pattern_compiled_length = Parse.pcre2_unset;
    bsr_convention = Options.bsr_unicode;
    newline_convention = Options.newline_lf;
    parens_nest_limit = Limits.parens_nest_limit;
    extra_options = 0;
    max_varlookbehind = Limits.max_varlookbehind;
  }

(* ---------- Open-capture chain ---------- *)

(* pcre2_internal.h:1829-1837 — structure for building a chain of open
   capturing subpatterns during compiling, so that instructions to close
   them can be compiled when ( *ACCEPT) is encountered. Not a compile_block
   field: it is threaded through compile_branch/compile_regex as a
   parameter, exactly as the C's open_capitem *open_caps
   (pcre2_compile.c:8388-8453); NULL becomes None. *)
type open_capitem = {
  next : open_capitem option; (* Chain link *)
  number : int; (* uint16_t: capture number *)
  assert_depth : int; (* uint16_t: assertion depth when opened *)
}

(* pcre2_intmodedep.h:704-707 — structure for maintaining a chain of
   pointers to the currently incomplete branches, for testing for left
   recursion (see could_be_empty_branch, M1 compile_regex chunk). The C's
   PCRE2_UCHAR *current_branch becomes an offset into cb.start_code (always
   assigned before read in compile_regex, so plain int); NULL is represented
   as None only where the C passes NULL chain pointers: the [outer] field and
   bcptr parameters ([branch_chain option]). *)
type branch_chain = {
  outer : branch_chain option;
  mutable current_branch : int;
}

(* ---------- Compile block ---------- *)

(* pcre2_intmodedep.h:719-763 — structure for passing "static" information
   around between the functions doing the compiling, so that they are
   thread-safe. C pointers into the pattern / parsed pattern / name table
   become int offsets into the corresponding string / int array / Bytes.t.
   Fields owned by later milestones are marked M2..M5 so the record stays
   stable across the compile-phase chunks.
   DEVIATION: the lcc/fcc/cbits/ctypes table pointers
   (pcre2_intmodedep.h:724-727) are dropped — this port always reads the
   default tables through the Chartables module (custom character tables
   are out of scope). *)
type compile_block = {
  cx : compile_context; (* Points to the compile context *)
  mutable start_workspace : Bytes.t;
      (* The working space (its start is
         offset 0) *)
  mutable start_code : Bytes.t;
      (* The buffer holding the compiled code: aliases start_workspace
         during the pre-compile phase, and is the real code buffer during
         the second phase (pcre2_compile.c:10268,10675). Code offsets index
         this buffer, offset 0 = C's cb->start_code. *)
  pattern : string; (* cb->start_pattern; indices are pattern offsets *)
  end_pattern : int; (* one past the last code unit of the pattern *)
  mutable name_table : Bytes.t;
      (* The name/number table: names_found entries of name_entry_size code
         units each — a GET2 group number, then the name, NUL-terminated *)
  mutable workspace_size : int; (* Size of workspace (code units) *)
  small_ref_offset : int array; (* Offsets for \1 to \9 (10 entries) *)
  mutable erroroffset : int; (* Offset of error in pattern *)
  mutable names_found : int; (* uint16_t: number of entries so far *)
  mutable name_entry_size : int; (* uint16_t: size of each entry *)
  mutable parens_depth : int; (* uint16_t: depth of nested parentheses *)
  mutable assert_depth : int; (* uint16_t: depth of nested assertions (M4) *)
  mutable named_groups : Parse.named_group array;
      (* Points to vector in pre-compile *)
  mutable named_group_list_size : int; (* Number of entries in the list *)
  mutable external_options : int; (* External (initial) options *)
  mutable external_flags : int; (* External flag bits to be set *)
  mutable bracount : int; (* Count of capturing parentheses *)
  mutable lastcapture : int; (* Last capture encountered (M2) *)
  mutable parsed_pattern : int array; (* Parsed pattern buffer *)
  mutable parsed_pattern_end : int;
      (* Parsed pattern should not get here: index one past the last
         element, as in Parse.parse_context *)
  mutable groupinfo : int array;
      (* Group info vector (M3: check_lookbehinds/get_grouplength) *)
  mutable top_backref : int; (* Maximum back reference (M2) *)
  mutable backref_map : int; (* Bitmap of low back refs (M2) *)
  mutable nltype : int; (* Newline type *)
  mutable nllen : int; (* Newline string length *)
  mutable class_range_start : int; (* Overall class range start *)
  mutable class_range_end : int; (* Overall class range end *)
  mutable nl0 : int; (* cb->nl[0]: newline string when fixed length *)
  mutable nl1 : int;
      (* cb->nl[1] — the C's nl is PCRE2_UCHAR[4], but only elements 0..1
         are ever written or read (nllen is 1 or 2,
         pcre2_compile.c:10436-10457); two int fields as in
         Parse.parse_context *)
  mutable req_varyopt : int; (* "After variable item" flag for reqbyte *)
  mutable max_varlookbehind : int; (* Limit for variable lookbehinds (M3) *)
  mutable max_lookbehind : int;
      (* Maximum lookbehind encountered (characters) (M3) *)
  mutable had_accept : bool; (* ( *ACCEPT) encountered (M5) *)
  mutable had_pruneorskip : bool; (* ( *PRUNE) or ( *SKIP) encountered (M5) *)
  mutable had_recurse : bool;
      (* Had a pattern recursion or subroutine call (M5) *)
  mutable dupnames : bool; (* Duplicate names exist *)
}

(* pcre2_compile.c:10238-10290 — "Initialize the 'static' compile data":
   pcre2_compile()'s cb setup. The character-table pointers (10240-10245)
   are the Chartables module here (see the record's DEVIATION note).
   had_accept/had_pruneorskip are not part of this C block — the C first
   assigns them just before the real compile phase
   (pcre2_compile.c:10677-10678) — so false here is unobservable. The
   newline fields carry the build defaults (PCRE2_NEWLINE_LF) until the
   driver chunk ports the newline/BSR resolution
   (pcre2_compile.c:10428-10470), exactly as Parse.make_context does. *)
let make_block ?(cx : compile_context = default_compile_context)
    (pattern : string) ~(options : int) : compile_block =
  let cworkspace = Bytes.make compile_work_size '\000' in
  {
    cx (* pcre2_compile.c:10249 *);
    start_workspace = cworkspace (* pcre2_compile.c:10270 *);
    start_code = cworkspace (* pcre2_compile.c:10268 *);
    pattern (* pcre2_compile.c:10269, cb.start_pattern *);
    end_pattern = String.length pattern (* pcre2_compile.c:10251 *);
    name_table = Bytes.empty (* pcre2_compile.c:10261, NULL *);
    workspace_size = compile_work_size (* pcre2_compile.c:10271 *);
    small_ref_offset =
      Array.make 10 Parse.pcre2_unset (* pcre2_compile.c:10280-10290 *);
    erroroffset = 0 (* pcre2_compile.c:10252 *);
    names_found = 0 (* pcre2_compile.c:10264 *);
    name_entry_size = 0 (* pcre2_compile.c:10260 *);
    parens_depth = 0 (* pcre2_compile.c:10265 *);
    assert_depth = 0 (* pcre2_compile.c:10247 *);
    (* pcre2_compile.c:10168,10262 — the initial NAMED_GROUP_LIST_SIZE
       entries; sharing one dummy record across the fresh slots is safe for
       the same reason as in Parse.make_context. *)
    named_groups =
      Array.make Parse.named_group_list_size
        { Parse.name = 0; number = 0; length = 0; isdup = false };
    named_group_list_size =
      Parse.named_group_list_size (* pcre2_compile.c:10263 *);
    external_options = options (* pcre2_compile.c:10254 *);
    external_flags = 0 (* pcre2_compile.c:10253 *);
    bracount = 0 (* pcre2_compile.c:10248 *);
    lastcapture = 0 (* pcre2_compile.c:10257 *);
    parsed_pattern =
      [||]
      (* pcre2_compile.c:10266; sized by the driver via
         Parse.allocate_parsed_pattern *);
    parsed_pattern_end = 0;
    groupinfo =
      [||]
      (* pcre2_compile.c:10255; sized 2*bracount+1 by the driver
         (pcre2_compile.c:10530-10550, M3) *);
    top_backref = 0 (* pcre2_compile.c:10277 *);
    backref_map = 0 (* pcre2_compile.c:10278 *);
    nltype = Parse.nltype_fixed (* pcre2_compile.c:10435 *);
    nllen = 1 (* pcre2_compile.c:10444, PCRE2_NEWLINE_LF arm *);
    class_range_start =
      0
      (* not initialized in the C: always written by add_to_class
         (pcre2_compile.c:5440-5441) before any read (5265,5304) *);
    class_range_end = 0;
    nl0 = 0x0a (* CHAR_NL, pcre2_compile.c:10445 *);
    nl1 = 0;
    req_varyopt = 0 (* pcre2_compile.c:10267 *);
    max_varlookbehind = cx.max_varlookbehind (* pcre2_compile.c:10259 *);
    max_lookbehind = 0 (* pcre2_compile.c:10258 *);
    had_accept = false (* pcre2_compile.c:10677 *);
    had_pruneorskip = false (* pcre2_compile.c:10678 *);
    had_recurse = false (* pcre2_compile.c:10256 *);
    dupnames = false (* pcre2_compile.c:10250 *);
  }

(* ---------- Workspace overrun guard ---------- *)

(* pcre2_compile.c:5748-5759 — the check at the top of compile_branch's
   loop during the pre-compile phase (lengthptr != NULL): "If we are in the
   pre-compile phase ... Check for overrun". `code` is the write offset
   into cb.start_code, which aliases cb.start_workspace during that phase,
   so the C's pointer comparisons against start_workspace + workspace_size
   become plain offset comparisons. Returns 0 when there is room, otherwise
   the error code compile_branch must set before returning 0: ERR52 (would
   run off the workspace) or ERR86 (inside the safety margin). *)
let check_workspace_overflow (cb : compile_block) (code : int) : int =
  if code > cb.workspace_size - work_size_safety_margin then
    if code >= cb.workspace_size then Errors.err52 else Errors.err86
  else 0

(* ---------- Find first significant opcode ---------- *)

(* pcre2_compile.c:5043-5122 — first_significant_code. This is called by
   several functions that scan a compiled expression looking for a fixed
   first character, or an anchoring opcode etc. It skips over things that
   do not influence this. For some calls, it makes sense to skip negative
   forward and all backward assertions, and also the \b assertion; for
   others it does not.

   Arguments:
     code        the compiled-code buffer
     pos         offset of the start of the group (C: the code pointer)
     skipassert  true if certain assertions are to be skipped

   Returns:      offset of the first significant opcode *)
let first_significant_code (code : Bytes.t) (pos : int) ~(skipassert : bool) :
    int =
  let rec loop pos =
    let op = Char.code (Bytes.get code pos) in
    if
      (* pcre2_compile.c:5067-5074 *)
      Int.equal op Opcodes.op_assert_not
      || Int.equal op Opcodes.op_assertback
      || Int.equal op Opcodes.op_assertback_not
      || Int.equal op Opcodes.op_assertback_na
    then
      if not skipassert then pos
      else
        (* do code += GET(code, 1); while ( *code == OP_ALT); *)
        let rec skip_alts p =
          let p = p + get code (p + 1) in
          if Int.equal (Char.code (Bytes.get code p)) Opcodes.op_alt then
            (skip_alts [@tailcall]) p
          else p
        in
        let p = skip_alts pos in
        (loop [@tailcall])
          (p + Opcodes.op_lengths.(Char.code (Bytes.get code p)))
    else if
      (* pcre2_compile.c:5076-5081 *)
      Int.equal op Opcodes.op_word_boundary
      || Int.equal op Opcodes.op_not_word_boundary
      || Int.equal op Opcodes.op_ucp_word_boundary
      || Int.equal op Opcodes.op_not_ucp_word_boundary
    then
      if not skipassert then pos
      else
        (* fallthrough to the OP_CALLOUT..OP_TRUE group in C *)
        (loop [@tailcall]) (pos + Opcodes.op_lengths.(op))
    else if
      (* pcre2_compile.c:5083-5091 *)
      Int.equal op Opcodes.op_callout
      || Int.equal op Opcodes.op_cref
      || Int.equal op Opcodes.op_dncref
      || Int.equal op Opcodes.op_rref
      || Int.equal op Opcodes.op_dnrref
      || Int.equal op Opcodes.op_false
      || Int.equal op Opcodes.op_true
    then (loop [@tailcall]) (pos + Opcodes.op_lengths.(op))
    else if Int.equal op Opcodes.op_callout_str then
      (* pcre2_compile.c:5093-5095 *)
      (loop [@tailcall]) (pos + get code (pos + 1 + (2 * Limits.link_size)))
    else if Int.equal op Opcodes.op_skipzero then
      (* pcre2_compile.c:5097-5099 *)
      (loop [@tailcall]) (pos + 2 + get code (pos + 2) + Limits.link_size)
    else if Int.equal op Opcodes.op_cond || Int.equal op Opcodes.op_scond then
      (* pcre2_compile.c:5101-5107 *)
      if
        (not
           (Int.equal
              (Char.code (Bytes.get code (pos + 1 + Limits.link_size)))
              Opcodes.op_false))
        (* Not DEFINE *)
        || not
             (Int.equal
                (Char.code (Bytes.get code (pos + get code (pos + 1))))
                Opcodes.op_ket)
        (* More than one branch *)
      then pos
      else (loop [@tailcall]) (pos + get code (pos + 1) + 1 + Limits.link_size)
    else if
      (* pcre2_compile.c:5109-5115 *)
      Int.equal op Opcodes.op_mark
      || Int.equal op Opcodes.op_commit_arg
      || Int.equal op Opcodes.op_prune_arg
      || Int.equal op Opcodes.op_skip_arg
      || Int.equal op Opcodes.op_then_arg
    then
      (loop [@tailcall])
        (pos + Char.code (Bytes.get code (pos + 1)) + Opcodes.op_lengths.(op))
    else (* pcre2_compile.c:5117-5118, default *) pos
  in
  loop pos

(* ---------- Find details of duplicate group names ---------- *)

(* pcre2_compile.c:5534-5600 — find_dupname_details. This is called from
   compile_branch() when it needs to know the index and count of duplicates
   in the names table when processing named backreferences, either
   directly, or as conditions.

   Arguments:
     name          pattern offset of the name (C: PCRE2_SPTR into pattern)
     length        the length of the name
     indexptr      where to put the index
     countptr      where to put the count of duplicates
     errorcodeptr  where to put an error code
     cb            the compile block

   Returns:        true if OK, false if not, error code set *)
let find_dupname_details ~(name : int) ~(length : int) (indexptr : int ref)
    (countptr : int ref) (errorcodeptr : int ref) (cb : compile_block) : bool =
  (* PRIV(strncmp)(name, slot+IMM2_SIZE, length) == 0 &&
     slot[IMM2_SIZE+length] == 0 (pcre2_compile.c:5565-5566 and its
     negation at 5594-5595; strncmp is pcre2_string_utils.c:156-167 — only
     the equality result is used here). *)
  let name_matches slot =
    let rec cmp i =
      if i >= length then
        Int.equal
          (Char.code
             (Bytes.get cb.name_table (slot + Limits.imm2_size + length)))
          0
      else if
        Char.equal
          cb.pattern.[name + i]
          (Bytes.get cb.name_table (slot + Limits.imm2_size + i))
      then (cmp [@tailcall]) (i + 1)
      else false
    in
    cmp 0
  in
  (* Find the first entry in the table (pcre2_compile.c:5561-5568). *)
  let i = ref 0 in
  let slot = ref 0 in
  let found = ref false in
  while (not !found) && !i < cb.names_found do
    if name_matches !slot then found := true
    else (
      slot := !slot + cb.name_entry_size;
      incr i)
  done;
  if !i >= cb.names_found then (
    (* This should not occur, because this function is called only when we
       know we have duplicate names. Give an internal error
       (pcre2_compile.c:5570-5578). *)
    errorcodeptr := Errors.err53;
    cb.erroroffset <- name (* name - cb->start_pattern *);
    false)
  else (
    (* Record the index and then see how many duplicates there are,
       updating the backref map and maximum back reference as we do
       (pcre2_compile.c:5580-5599). *)
    indexptr := !i;
    let count = ref 0 in
    let break = ref false in
    while not !break do
      incr count;
      let groupnumber = get2 cb.name_table !slot in
      cb.backref_map <-
        (cb.backref_map lor if groupnumber < 32 then 1 lsl groupnumber else 1);
      if groupnumber > cb.top_backref then cb.top_backref <- groupnumber;
      incr i;
      if !i >= cb.names_found then break := true
      else (
        slot := !slot + cb.name_entry_size;
        if not (name_matches !slot) then break := true)
    done;
    countptr := !count;
    true)

(* ---------- Class-compilation helpers ---------- *)

(* pcre2_internal.h:1927 — MAX_NON_UTF_CHAR: the largest character value
   that can be handled when not in UTF mode; 0xff in the 8-bit library. *)
let max_non_utf_char = 0xff

(* pcre2_compile.c:377-386 — SETBIT: set an individual bit in a class
   bitmap (bits run from the least significant end of each byte). *)
let setbit (map : Bytes.t) (b : int) : unit =
  let i = b lsr 3 in
  Bytes.set map i
    (Char.chr (Char.code (Bytes.get map i) lor (1 lsl (b land 7))))

(* pcre2_compile.c:709-713 — indices into posix_class_maps triples for the
   classes compile_branch treats specially (PC_DIGIT/PC_XDIGIT live in
   Parse, which owns their parse-phase use). *)
let pc_graph = 8
let pc_print = 9
let pc_punct = 10

(* pcre2_compile.c:715-740 — table of class bit maps for each POSIX class.
   Each class is formed from a base map, with an optional addition or
   removal of another map. Then, for some classes, there is some additional
   tweaking: for [:blank:] the vertical space characters are removed, and
   for [:alpha:] and [:alnum:] the underscore character is removed. The
   triples in the table consist of the base map offset, second map offset
   or -1 if no second map, and a non-negative value for map addition or a
   negative value for map subtraction (if there are two maps). The absolute
   value of the third field has these meanings: 0 => no tweaking, 1 =>
   remove vertical space characters, 2 => remove underscore. *)
let posix_class_maps =
  [|
    Chartables.cbit_word;
    Chartables.cbit_digit;
    -2 (* alpha *);
    Chartables.cbit_lower;
    -1;
    0 (* lower *);
    Chartables.cbit_upper;
    -1;
    0 (* upper *);
    Chartables.cbit_word;
    -1;
    2 (* alnum - word without underscore *);
    Chartables.cbit_print;
    Chartables.cbit_cntrl;
    0 (* ascii *);
    Chartables.cbit_space;
    -1;
    1 (* blank - a GNU extension *);
    Chartables.cbit_cntrl;
    -1;
    0 (* cntrl *);
    Chartables.cbit_digit;
    -1;
    0 (* digit *);
    Chartables.cbit_graph;
    -1;
    0 (* graph *);
    Chartables.cbit_print;
    -1;
    0 (* print *);
    Chartables.cbit_punct;
    -1;
    0 (* punct *);
    Chartables.cbit_space;
    -1;
    0 (* space *);
    Chartables.cbit_word;
    -1;
    0 (* word - a Perl extension *);
    Chartables.cbit_xdigit;
    -1;
    0 (* xdigit *);
  |]

(* pcre2_compile.c:5206-5365 — add_to_class_internal. This function
   packages up the logic of adding a character or range of characters to a
   class. The character values in the arguments will be within the valid
   values for the current mode. This function is called only from within
   the "add to class" group of functions; the external entry point is
   add_to_class(). Returns the number of < 256 characters added.

   uchardptr (the XCLASS extra-data write cursor, an offset into
   cb.start_code) is threaded through unused until M6: in the 8-bit library
   only the UTF arm writes through it (pcre2_compile.c:5322-5336).

   DEVIATION: an errorcodeptr argument is appended (the C signature has
   none). Two Unicode paths are later-milestone chunks and defer loudly,
   identically in both compile phases, instead of emitting:
   - the caseless UTF/UCP closure (pcre2_compile.c:5246-5282,
     get_othercase_range / add_list_to_class_internal / ucd_caseless_sets)
     is M7 (docs/ocaml-engine/08-ucp.md) with M6 for the UTF side;
   - the extra-data emission for wide characters under UTF
     (pcre2_compile.c:5322-5336, XCL_SINGLE/XCL_RANGE + ord2utf) is M6
     XCLASS work (docs/ocaml-engine/07-utf.md).
   Callers propagate the deferral from compile_branch's CONTINUE_CLASS
   point. The n8 value returned after a deferral is meaningless: the
   compile is abandoned with the error. *)
let add_to_class_internal (classbits : Bytes.t) (_uchardptr : int ref)
    (options : int) (_xoptions : int) (cb : compile_block)
    (errorcodeptr : int ref) (start : int) (end_ : int) : int =
  (* pcre2_compile.c:5235-5237 *)
  let classbits_end = if end_ <= 0xff then end_ else 0xff in
  let n8 = ref 0 in
  (* pcre2_compile.c:5239-5295 — if caseless matching is required, scan the
     range and process alternate cases. *)
  if not (Int.equal (options land Options.caseless) 0) then
    if not (Int.equal (options land (Options.utf lor Options.ucp)) 0) then
      (* pcre2_compile.c:5247-5282 — Unicode caseless closure: deferred
         (see the DEVIATION note above). *)
      errorcodeptr := Parse.err_deferred
    else
      (* pcre2_compile.c:5288-5294 — not UTF mode. Loop bound: c <= 0xff,
         so Chartables.fcc's 0..255 precondition holds. *)
      for c = start to classbits_end do
        setbit classbits (Chartables.fcc c);
        incr n8
      done;
  if not (Int.equal !errorcodeptr 0) then !n8
  else
    (* pcre2_compile.c:5297-5302 — now handle the originally supplied
       range. Adjust the final value according to the bit length. *)
    let end_ =
      if Int.equal (options land Options.utf) 0 && end_ > max_non_utf_char then
        max_non_utf_char
      else end_
    in
    (* pcre2_compile.c:5304 *)
    if start > cb.class_range_start && end_ < cb.class_range_end then !n8
    else (
      (* pcre2_compile.c:5306-5313 — use the bitmap for characters < 256.
         Regardless of start, c will always be <= 255. *)
      for c = start to classbits_end do
        setbit classbits c;
        incr n8
      done;
      (* pcre2_compile.c:5315-5318 — otherwise use extra data. *)
      let start = if start <= 0xff then 0xff + 1 else start in
      if end_ >= start then
        if not (Int.equal (options land Options.utf) 0) then
          (* pcre2_compile.c:5322-5336 — XCL_SINGLE/XCL_RANGE via ord2utf:
             deferred (see the DEVIATION note above). *)
          errorcodeptr := Parse.err_deferred
          (* pcre2_compile.c:5340-5344 — without UTF support, character
             values are constrained by the bit length: in the 8-bit library
             there is nothing to do (end_ was clamped to 0xff above). *);
      !n8)

(* pcre2_compile.c:5416-5444 — add_to_class: external entry point for
   adding a range to a class. Sets the overall range so that the internal
   functions can try to avoid duplication when handling case-independence.
   Returns the number of < 256 characters added. *)
let add_to_class (classbits : Bytes.t) (uchardptr : int ref) (options : int)
    (xoptions : int) (cb : compile_block) (errorcodeptr : int ref) (start : int)
    (end_ : int) : int =
  cb.class_range_start <- start;
  cb.class_range_end <- end_;
  add_to_class_internal classbits uchardptr options xoptions cb errorcodeptr
    start end_

(* pcre2_compile.c:5447-5491 — add_list_to_class: add a list of horizontal
   or vertical whitespace characters to a class. The list (p, an ascending
   NOTACHAR-terminated int array) must be in order so that ranges of
   characters can be detected and handled appropriately. except is a
   character to omit (NOTACHAR to omit none). The C walks the pointer p;
   here pi is the index into p. *)
let add_list_to_class (classbits : Bytes.t) (uchardptr : int ref)
    (options : int) (xoptions : int) (cb : compile_block)
    (errorcodeptr : int ref) (p : int array) (except : int) : int =
  let n8 = ref 0 in
  let pi = ref 0 in
  while p.(!pi) < Tables.notachar do
    let n = ref 0 in
    if not (Int.equal p.(!pi) except) then (
      while Int.equal p.(!pi + !n + 1) (p.(!pi) + !n + 1) do
        incr n
      done;
      cb.class_range_start <- p.(!pi);
      cb.class_range_end <- p.(!pi + !n);
      n8 :=
        !n8
        + add_to_class_internal classbits uchardptr options xoptions cb
            errorcodeptr p.(!pi)
            p.(!pi + !n));
    pi := !pi + !n + 1
  done;
  !n8

(* pcre2_compile.c:5495-5530 — add_not_list_to_class: add the complement of
   a list of horizontal or vertical whitespace to a class. The list must be
   in order. *)
let add_not_list_to_class (classbits : Bytes.t) (uchardptr : int ref)
    (options : int) (xoptions : int) (cb : compile_block)
    (errorcodeptr : int ref) (p : int array) : int =
  let utf = not (Int.equal (options land Options.utf) 0) in
  let n8 = ref 0 in
  let pi = ref 0 in
  if p.(0) > 0 then
    n8 :=
      !n8
      + add_to_class classbits uchardptr options xoptions cb errorcodeptr 0
          (p.(0) - 1);
  while p.(!pi) < Tables.notachar do
    while Int.equal p.(!pi + 1) (p.(!pi) + 1) do
      incr pi
    done;
    n8 :=
      !n8
      + add_to_class classbits uchardptr options xoptions cb errorcodeptr
          (p.(!pi) + 1)
          (if Int.equal p.(!pi + 1) Tables.notachar then
             if utf then 0x10ffff else 0xffffffff
           else p.(!pi + 1) - 1);
    incr pi
  done;
  !n8

(* ---------- Repeat-compilation tables ---------- *)

(* pcre2_compile.c:687-691 — offsets from OP_STAR for case-independent and
   negative repeat opcodes. Indexed by op_previous - OP_CHAR for previous
   items OP_CHAR/OP_CHARI/OP_NOT/OP_NOTI. *)
let chartypeoffset =
  [|
    Opcodes.op_star - Opcodes.op_star;
    Opcodes.op_stari - Opcodes.op_star;
    Opcodes.op_notstar - Opcodes.op_star;
    Opcodes.op_notstari - Opcodes.op_star;
  |]

(* pcre2_compile.c:861-917 — this table is used when converting repeating
   opcodes into possessified versions as a result of an explicit possessive
   quantifier such as ++. A zero value means there is no possessified
   version - in those cases the item in question must be wrapped in ONCE
   brackets. The table is truncated at OP_CALLOUT because all relevant
   opcodes are less than that. *)
let opcode_possessify =
  [|
    (* 0 - 15 *)
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    (* 16 - 31 *)
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0;
    0 (* NOTI *);
    Opcodes.op_posstar;
    0 (* STAR, MINSTAR *);
    Opcodes.op_posplus;
    0 (* PLUS, MINPLUS *);
    Opcodes.op_posquery;
    0 (* QUERY, MINQUERY *);
    Opcodes.op_posupto;
    0 (* UPTO, MINUPTO *);
    0 (* EXACT *);
    0;
    0;
    0;
    0 (* POS{STAR,PLUS,QUERY,UPTO} *);
    Opcodes.op_posstari;
    0 (* STARI, MINSTARI *);
    Opcodes.op_posplusi;
    0 (* PLUSI, MINPLUSI *);
    Opcodes.op_posqueryi;
    0 (* QUERYI, MINQUERYI *);
    Opcodes.op_posuptoi;
    0 (* UPTOI, MINUPTOI *);
    0 (* EXACTI *);
    0;
    0;
    0;
    0 (* POS{STARI,PLUSI,QUERYI,UPTOI} *);
    Opcodes.op_notposstar;
    0 (* NOTSTAR, NOTMINSTAR *);
    Opcodes.op_notposplus;
    0 (* NOTPLUS, NOTMINPLUS *);
    Opcodes.op_notposquery;
    0 (* NOTQUERY, NOTMINQUERY *);
    Opcodes.op_notposupto;
    0 (* NOTUPTO, NOTMINUPTO *);
    0 (* NOTEXACT *);
    0;
    0;
    0;
    0 (* NOTPOS{STAR,PLUS,QUERY,UPTO} *);
    Opcodes.op_notposstari;
    0 (* NOTSTARI, NOTMINSTARI *);
    Opcodes.op_notposplusi;
    0 (* NOTPLUSI, NOTMINPLUSI *);
    Opcodes.op_notposqueryi;
    0 (* NOTQUERYI, NOTMINQUERYI *);
    Opcodes.op_notposuptoi;
    0 (* NOTUPTOI, NOTMINUPTOI *);
    0 (* NOTEXACTI *);
    0;
    0;
    0;
    0 (* NOTPOS{STARI,PLUSI,QUERYI,UPTOI} *);
    Opcodes.op_typeposstar;
    0 (* TYPESTAR, TYPEMINSTAR *);
    Opcodes.op_typeposplus;
    0 (* TYPEPLUS, TYPEMINPLUS *);
    Opcodes.op_typeposquery;
    0 (* TYPEQUERY, TYPEMINQUERY *);
    Opcodes.op_typeposupto;
    0 (* TYPEUPTO, TYPEMINUPTO *);
    0 (* TYPEEXACT *);
    0;
    0;
    0;
    0 (* TYPEPOS{STAR,PLUS,QUERY,UPTO} *);
    Opcodes.op_crposstar;
    0 (* CRSTAR, CRMINSTAR *);
    Opcodes.op_crposplus;
    0 (* CRPLUS, CRMINPLUS *);
    Opcodes.op_crposquery;
    0 (* CRQUERY, CRMINQUERY *);
    Opcodes.op_crposrange;
    0 (* CRRANGE, CRMINRANGE *);
    0;
    0;
    0;
    0 (* CRPOS{STAR,PLUS,QUERY,RANGE} *);
    0;
    0;
    0 (* CLASS, NCLASS, XCLASS *);
    0;
    0 (* REF, REFI *);
    0;
    0 (* DNREF, DNREFI *);
    0;
    0 (* RECURSE, CALLOUT *);
  |]

(* ---------- Compile one branch ---------- *)

(* pcre2_compile.c:5604-5640 — compile_branch. Scan the parsed pattern,
   compiling it into the a vector of PCRE2_UCHAR. If the options are changed
   during the branch, the pointer is used to change the external options
   bits. This function is used during the pre-compile phase when we are
   trying to find out the amount of memory needed, as well as during the
   real compile phase. The value of lengthptr distinguishes the two phases.

   Out-parameter mapping (each C pointer-to-uint32/pointer-to-pointer
   argument becomes an int ref holding the same value; code pointers are
   offsets into cb.start_code, parsed-pattern pointers are indices into
   cb.parsed_pattern):
     optionsptr        pointer to the option bits
     xoptionsptr       pointer to the extra option bits
     codeptr           points to the pointer to the current code point
     pptrptr           points to the current parsed pattern pointer
     errorcodeptr      points to error code variable
     firstcuptr        place to put the first required code unit
     firstcuflagsptr   place to put the first code unit flags
     reqcuptr          place to put the last required code unit
     reqcuflagsptr     place to put the last required code unit flags
     bcptr             points to current branch chain (None = NULL)
     open_caps         points to current capitem (None = NULL)
     cb                contains pointers to tables etc.
     lengthptr         None during the real compile phase (C: NULL)
                       Some length-accumulator ref during pre-compile phase

   Returns:            0 There's been an error, *errorcodeptr is non-zero
                      +1 Success, this branch must match at least one char
                      -1 Success, this branch may match an empty string

   Chunk boundary (M1 chunks "compile_branch A".."D" + the M2 backref
   chunk): chars/escapes, classes, repeats, plain capture/non-capture
   groups (with bracket repeats) and back references (numeric and by
   name) are live. The arms for conditionals/lookarounds (M4/M5), verbs,
   recursion and string callouts are deferred — they fail loudly with
   Parse.err_deferred (identically in both phases, before any
   phase-dependent work); within the repeat arm, an OP_RECURSE previous
   item likewise defers (M5). The C local `offset` (pcre2_compile.c:5658)
   becomes a per-arm let binding in the backref arms; the still-deferred
   conditional arms bring their own uses with them. The class locals
   (negate_class, should_flip_negation,
   match_all_or_no_wide_chars, class_has_8bitchar, xclass, xclass_has_prop,
   class_uchardata, classbits; pcre2_compile.c:5672,5690-5693,5725-5733)
   live in the class arm, which is where the C first assigns them each
   iteration; the per-branch classbits[32] buffer (5672) becomes a fresh
   32-byte Bytes per class, standing in for the C's memset (6026). *)
let rec compile_branch (optionsptr : int ref) (xoptionsptr : int ref)
    (codeptr : int ref) (pptrptr : int ref) (errorcodeptr : int ref)
    (firstcuptr : int ref) (firstcuflagsptr : int ref) (reqcuptr : int ref)
    (reqcuflagsptr : int ref) (bcptr : branch_chain option)
    (open_caps : open_capitem option) (cb : compile_block)
    (lengthptr : int ref option) : int =
  (* pcre2_compile.c:5642-5670 — locals (see the chunk-boundary note above
     for the ones deferred with their arms). *)
  let okreturn = ref (-1) in
  let options = ref !optionsptr in
  (* May change dynamically *)
  let xoptions = ref !xoptionsptr in
  (* May change dynamically *)
  let firstcu = ref 0 and reqcu = ref 0 in
  let zeroreqcu = ref 0 and zerofirstcu = ref 0 in
  let pptr = ref !pptrptr in
  let firstcuflags = ref 0 and reqcuflags = ref 0 in
  let zeroreqcuflags = ref 0 and zerofirstcuflags = ref 0 in
  let req_caseopt = ref 0 in
  let code = ref !codeptr in
  let last_code = ref !code in
  let orig_code = !code in
  let previous = ref (-1) in
  (* PCRE2_UCHAR *previous = NULL; set for non-quantifier items
     (pcre2_compile.c:5800-5804), read by the repeat arms (chunk C) *)
  (* pcre2_compile.c:5644,5659,5666 — group_return, length_prevgroup and
     groupsetfirstcu are function-level: the group arm writes them and the
     bracket-repeat arm (7554,7569,7604,7705) reads them on a later
     iteration. *)
  let group_return = ref 0 in
  let length_prevgroup = ref 0 in
  let groupsetfirstcu = ref false in
  let matched_char = ref false in
  let previous_matched_char = ref false in
  let had_accept = ref false in
  let reset_caseful = ref false in

  (* pcre2_compile.c:5674-5683 — we can fish out the UTF setting once and
     for all into a BOOL, but we must not do this for other options (e.g.
     PCRE2_EXTENDED) that may change dynamically as we process the
     pattern. *)
  let utf = not (Int.equal (!options land Options.utf) 0) in
  let ucp = not (Int.equal (!options land Options.ucp) 0) in

  (* pcre2_compile.c:5696-5699 — set up the default and non-default
     settings for greediness. Read by the repeat arms (chunk C); updated
     here and by META_OPTIONS. *)
  let greedy_default =
    ref (if not (Int.equal (!options land Options.ungreedy) 0) then 1 else 0)
  in
  let greedy_non_default = ref (!greedy_default lxor 1) in

  (* pcre2_compile.c:5701-5711 — initialize no first unit, no required
     unit. REQ_UNSET means "no char matching encountered yet". It gets
     changed to REQ_NONE if we hit something that matches a non-fixed first
     unit; reqcu just remains unset if we never find one.

     When we hit a repeat whose minimum is zero, we may have to adjust
     these values to take the zero repeat into account. This is implemented
     by setting them to zerofirstcu and zeroreqcu when such a repeat is
     encountered. The individual item types that can be repeated set these
     backoff variables appropriately. *)
  firstcu := 0;
  reqcu := 0;
  zerofirstcu := 0;
  zeroreqcu := 0;
  firstcuflags := req_unset;
  reqcuflags := req_unset;
  zerofirstcuflags := req_unset;
  zeroreqcuflags := req_unset;

  (* pcre2_compile.c:5713-5719 — the variable req_caseopt contains either
     the REQ_CASELESS bit or zero, according to the current setting of the
     caseless flag. ... This is used only for ASCII characters. *)
  req_caseopt :=
    if not (Int.equal (!options land Options.caseless) 0) then req_caseless
    else 0;

  (* *code++ = x: single-code-unit store then advance; the C's implicit
     (PCRE2_UCHAR) truncation is the land 0xff. In the pre-compile phase the
     write goes to the workspace (guarded by check_workspace_overflow at
     the loop head); in the real phase the buffer was sized by that pass. *)
  let emit_cu (x : int) : unit =
    Bytes.set cb.start_code !code (Char.chr (x land 0xff));
    incr code
  in

  (* The C's `return okreturn` / `return 0` exits from inside the loop.
     Local exception, never escapes this function (port-conventions §2;
     compile phase only — nothing here crosses the interpreter loop). *)
  let exception Return of int in
  let return_from_branch (rc : int) : 'a = raise_notrace (Return rc) in

  (* The repetition arm's forward `goto END_REPEAT`
     (pcre2_compile.c:7277,7327,7329,7352,7771,7800,7832): jumps to the
     arm's epilogue, skipping the possessive-quantifier post-pass. Local
     exception caught at the end of that arm only (port-conventions §2). *)
  let exception End_repeat in
  (* pcre2_compile.c:8259-8336 — the CLASS_CASELESS_CHAR label: caseful
     matches, or caseless and not one of the multicase characters. Entered
     by fallthrough from NORMAL_CHAR_SET below, and by goto from a positive
     class that contains only case-partners of a character with just two
     cases (chunk B); matched_char has already been set TRUE and options
     fudged if necessary. *)
  let class_caseless_char (meta : int) : unit =
    (* pcre2_compile.c:8261-8270 — get the character's code units into
       mcbuffer, with the length in mclength. When not in UTF mode, the
       length is always 1.
       DEVIATION: the utf arm (mclength = PRIV(ord2utf)(meta, mcbuffer)) is
       the M6 chunk (multi-byte literal emission, byte-read policy); until
       it lands, UTF compilation fails loudly here rather than emitting a
       truncated character. mcbuffer[8] reduces to its element 0: with
       mclength = 1 the C never touches the rest. *)
    if utf then (
      errorcodeptr := Parse.err_deferred;
      return_from_branch 0);
    let mclength = 1 in
    let mcbuffer0 = meta land 0xff in

    (* pcre2_compile.c:8272-8276 — generate the appropriate code. *)
    emit_cu
      (if not (Int.equal (!options land Options.caseless) 0) then
         Opcodes.op_chari
       else Opcodes.op_char);
    emit_cu mcbuffer0;

    (* pcre2_compile.c:8278-8281 — remember if \r or \n were seen. *)
    if Int.equal mcbuffer0 0x0d || Int.equal mcbuffer0 0x0a then
      cb.external_flags <- cb.external_flags lor hascrorlf;

    (* pcre2_compile.c:8283-8309 — set the first and required code units
       appropriately. If no previous first code unit, set it from this
       character, but revert to none on a zero repeat. Otherwise, leave the
       firstcu value alone, and don't change it on a zero repeat. *)
    if Int.equal !firstcuflags req_unset then (
      zerofirstcuflags := req_none;
      zeroreqcu := !reqcu;
      zeroreqcuflags := !reqcuflags;

      (* If the character is more than one code unit long, we can set a
         single firstcu only if it is not to be matched caselessly. *)
      if Int.equal mclength 1 || Int.equal !req_caseopt 0 then (
        firstcu := mcbuffer0;
        firstcuflags := !req_caseopt;
        if not (Int.equal mclength 1) then (
          (* safe: code > orig_code — at least two units emitted above *)
          reqcu := Char.code (Bytes.get cb.start_code (!code - 1));
          reqcuflags := cb.req_varyopt))
      else (
        firstcuflags := req_none;
        reqcuflags := req_none))
    else (
      (* pcre2_compile.c:8311-8325 — firstcu was previously set; we can
         set reqcu only if the length is 1 or the matching is caseful. *)
      zerofirstcu := !firstcu;
      zerofirstcuflags := !firstcuflags;
      zeroreqcu := !reqcu;
      zeroreqcuflags := !reqcuflags;
      if Int.equal mclength 1 || Int.equal !req_caseopt 0 then (
        reqcu := Char.code (Bytes.get cb.start_code (!code - 1));
        reqcuflags := !req_caseopt lor cb.req_varyopt));

    (* pcre2_compile.c:8327-8334 — if caselessness was temporarily
       instated, reset it. *)
    if !reset_caseful then (
      options := !options land lnot Options.caseless;
      req_caseopt := 0;
      reset_caseful := false)
  in

  (* pcre2_compile.c:8228-8257 — the NORMAL_CHAR_SET label: character is
     already in meta. Falls through into CLASS_CASELESS_CHAR. *)
  let normal_char_set (meta : int) : unit =
    matched_char := true;
    (* pcre2_compile.c:8231-8252 — for caseless UTF or UCP mode, check
       whether this character has more than one other case (UCD_CASESET);
       if so, generate a special OP_PROP PT_CLIST item instead of OP_CHARI.
       DEVIATION: that block is Unicode machinery (M6 utf / M7 ucp); until
       those chunks land, caseless literal compilation under PCRE2_UTF or
       PCRE2_UCP fails loudly here instead of silently skipping the
       multicase check. Unreachable in M1's ASCII scope. *)
    if (utf || ucp) && not (Int.equal (!options land Options.caseless) 0) then (
      errorcodeptr := Parse.err_deferred;
      return_from_branch 0);
    (* fallthrough to CLASS_CASELESS_CHAR in C *)
    class_caseless_char meta
  in

  (* pcre2_compile.c:8226-8227 — the NORMAL_CHAR label: get the full 32
     bits (meta as dispatched below holds only META_CODE, zero for
     literals). *)
  let normal_char () : unit = normal_char_set cb.parsed_pattern.(!pptr) in

  (* pcre2_compile.c:6810-7015 — the GROUP_PROCESS_NOTE_EMPTY /
     GROUP_PROCESS labels: process a nested bracketed regex. The nesting
     depth is maintained for the benefit of the stackguard function. The
     test for too deep nesting is now done in parse_regex(). Assertion and
     DEFINE groups come to GROUP_PROCESS; others come to
     GROUP_PROCESS_NOTE_EMPTY, to indicate that we need to take note of
     whether or not they may match an empty string. The C's per-iteration
     note_group_empty (reset FALSE at 5808, set TRUE at 6817) and skipunits
     (reset 0 at 5809, set by META_CAPTURE at 8095) become parameters:
     each goto site passes the values in force when it jumps. bravalue
     (pcre2_compile.c:5642) is set by every jumping arm, so it is a
     parameter too. In M1 only META_NOCAPTURE (OP_BRA) and META_CAPTURE
     (OP_CBRA) reach here; the lookaround/conditional/script-run callers
     arrive with M4/M5. *)
  let group_process ~(note_group_empty : bool) ~(bravalue : int)
      ~(skipunits : int) : unit =
    (* pcre2_compile.c:6819-6825 *)
    cb.parens_depth <- cb.parens_depth + 1;
    Bytes.set cb.start_code !code (Char.chr (bravalue land 0xff));
    pptr := !pptr + 1;
    let tempcode = ref !code in
    let tempreqvary = cb.req_varyopt (* Save value before group *) in
    length_prevgroup := 0 (* Initialize for pre-compile phase *);

    (* pcre2_compile.c:5736,5739 — the sub* out-cells for compile_regex. *)
    let subfirstcu = ref 0 and subreqcu = ref 0 in
    let subfirstcuflags = ref 0 and subreqcuflags = ref 0 in

    (* pcre2_compile.c:6827-6845 *)
    group_return :=
      compile_regex !options !xoptions tempcode pptr errorcodeptr ~skipunits
        subfirstcu subfirstcuflags subreqcu subreqcuflags bcptr open_caps cb
        (match lengthptr with
        | None -> None (* Actual compile phase *)
        | Some _ -> Some length_prevgroup (* Pre-compile phase *));
    if Int.equal !group_return 0 then return_from_branch 0 (* Error *);

    (* pcre2_compile.c:6847 *)
    cb.parens_depth <- cb.parens_depth - 1;

    (* pcre2_compile.c:6849-6854 — if that was a non-conditional
       significant group (not an assertion, not a DEFINE) that matches at
       least one character, then the current item matches a character.
       Conditionals are handled below. *)
    if
      note_group_empty
      && (not (Int.equal bravalue Opcodes.op_cond))
      && !group_return > 0
    then matched_char := true;

    (* pcre2_compile.c:6856-6859 — if we've just compiled an assertion,
       pop the assert depth. *)
    if bravalue >= Opcodes.op_assert && bravalue <= Opcodes.op_assertback_na
    then cb.assert_depth <- cb.assert_depth - 1;

    (* pcre2_compile.c:6861-6913 — for a conditional bracket, check that
       there are no more than two branches in the group (ERR27), or just
       one if it's a DEFINE group (ERR54, then the OP_DEFINE-to-OP_FALSE
       rewrite and bravalue = OP_DEFINE). M5 owns conditionals
       (docs/ocaml-engine/05-conditionals-recursion.md); unreachable here
       until the META_COND arms stop deferring, kept loud (both phases,
       where the C checks only in the real phase) so M5 cannot silently
       miss it. *)
    if Int.equal bravalue Opcodes.op_cond then (
      errorcodeptr := Parse.err_deferred;
      return_from_branch 0);

    match lengthptr with
    | Some length ->
        (* pcre2_compile.c:6915-6933 — in the pre-compile phase, update
           the length by the length of the group, less the brackets at
           either end. Then reduce the compiled code to just a set of
           non-capturing brackets so that it doesn't use much memory if it
           is duplicated by a quantifier. *)
        if oflow_max - !length < !length_prevgroup - 2 - (2 * Limits.link_size)
        then (
          errorcodeptr := Errors.err20;
          return_from_branch 0);
        length := !length + !length_prevgroup - 2 - (2 * Limits.link_size);
        incr code (* This already contains bravalue *);
        putinc cb.start_code code (1 + Limits.link_size);
        emit_cu Opcodes.op_ket;
        putinc cb.start_code code (1 + Limits.link_size)
        (* break: no need to waste time with special character handling *)
    | None ->
        (* pcre2_compile.c:6935-6937 — otherwise update the main code
           pointer to the end of the group. *)
        code := !tempcode;

        (* pcre2_compile.c:6939-6942 — for a DEFINE group, required and
           first character settings are not relevant. bravalue only
           becomes OP_DEFINE inside the conditional block above (M5). *)
        if not (Int.equal bravalue Opcodes.op_define) then (
          (* pcre2_compile.c:6944-6955 — handle updating of the required
             and first code units for other types of group. Update for
             normal brackets of all kinds, and conditions with two
             branches (see code above). If the bracket is followed by a
             quantifier with zero repeat, we have to back off. Hence the
             definition of zeroreqcu and zerofirstcu outside the main loop
             so that they can be accessed for the back off. *)
          zeroreqcu := !reqcu;
          zeroreqcuflags := !reqcuflags;
          zerofirstcu := !firstcu;
          zerofirstcuflags := !firstcuflags;
          groupsetfirstcu := false;

          if bravalue >= Opcodes.op_once (* Not an assertion *) then (
            (* pcre2_compile.c:6959-6975 — if we have not yet set a
               firstcu in this branch, take it from the subpattern,
               remembering that it was set here so that a repeat of more
               than one can replicate it as reqcu if necessary. If the
               subpattern has no firstcu, set "none" for the whole branch.
               In both cases, a zero repeat forces firstcu to "none". *)
            if
              Int.equal !firstcuflags req_unset
              && not (Int.equal !subfirstcuflags req_unset)
            then (
              if !subfirstcuflags < req_none then (
                firstcu := !subfirstcu;
                firstcuflags := !subfirstcuflags;
                groupsetfirstcu := true)
              else firstcuflags := req_none;
              zerofirstcuflags := req_none)
            else if
              (* pcre2_compile.c:6977-6985 — if firstcu was previously
                 set, convert the subpattern's firstcu into reqcu if there
                 wasn't one, using the vary flag that was in existence
                 beforehand. *)
              !subfirstcuflags < req_none && !subreqcuflags >= req_none
            then (
              subreqcu := !subfirstcu;
              subreqcuflags := !subfirstcuflags lor tempreqvary);

            (* pcre2_compile.c:6987-6994 — if the subpattern set a
               required code unit (or set a first code unit that isn't
               really the first code unit - see above), set it. *)
            if !subreqcuflags < req_none then (
              reqcu := !subreqcu;
              reqcuflags := !subreqcuflags))
          else if
            (* pcre2_compile.c:6997-7013 — for a forward assertion, we
               take the reqcu, if set, provided that the group has also
               set a firstcu. This can be helpful if the pattern that
               follows the assertion doesn't set a different char. For
               example, it's useful for /(?=abcde).+/. We can't set
               firstcu for an assertion, however because it leads to
               incorrect effect for patterns such as /(?=a)a.+/ when the
               "real" "a" would then become a reqcu instead of a firstcu.
               This is overcome by a scan at the end if there's no
               firstcu, looking for an asserted first char. A similar
               effect for patterns like /(?=.*X)X$/ means we must only
               take the reqcu when the group also set a firstcu.
               Otherwise, in that example, 'X' ends up set for both. *)
            (Int.equal bravalue Opcodes.op_assert
            || Int.equal bravalue Opcodes.op_assert_na)
            && !subreqcuflags < req_none
            && !subfirstcuflags < req_none
          then (
            reqcu := !subreqcu;
            reqcuflags := !subreqcuflags))
    (* pcre2_compile.c:7015 — break: end of nested group handling *)
  in

  (* pcre2_compile.c:8040-8058 — HANDLE_SINGLE_REFERENCE: reached by
     falling through from META_BACKREF, and by goto from named backref
     handling when the reference is to a single group (that is, not to a
     duplicated name). The back reference data will have already been
     updated. We must disable firstcu if not set, to cope with cases like
     (?=(\w+))\1: which would otherwise set ':' later. *)
  let handle_single_reference (meta_arg : int) : unit =
    if Int.equal !firstcuflags req_unset then (
      zerofirstcuflags := req_none;
      firstcuflags := req_none);
    emit_cu
      (if not (Int.equal (!options land Options.caseless) 0) then
         Opcodes.op_refi
       else Opcodes.op_ref);
    put2inc cb.start_code code meta_arg;

    (* pcre2_compile.c:8051-8057 — update the map of back references, and
       keep the highest one. We could do this in parse_regex() for
       numerical back references, but not for named back references,
       because we don't know the numbers to which named back references
       refer. So we do it all in this function. *)
    cb.backref_map <-
      (cb.backref_map lor if meta_arg < 32 then 1 lsl meta_arg else 1);
    if meta_arg > cb.top_backref then cb.top_backref <- meta_arg
  in

  (* pcre2_compile.c:5721-5723 — switch on next META item until the end of
     the branch: for (;; pptr++). *)
  try
    while true do
      (* pcre2_compile.c:5743-5746 — get next META item in the pattern and
         its potential argument. *)
      let meta = Parse.meta_code cb.parsed_pattern.(!pptr) in
      let meta_arg = Parse.meta_data cb.parsed_pattern.(!pptr) in

      (* pcre2_compile.c:5748-5793 — if we are in the pre-compile phase,
         accumulate the length used for the previous cycle of this loop,
         unless the next item is a quantifier. *)
      (match lengthptr with
      | Some length ->
          (* pcre2_compile.c:5753-5759 — check for overrun. *)
          let overflow_err = check_workspace_overflow cb !code in
          if not (Int.equal overflow_err 0) then (
            errorcodeptr := overflow_err;
            return_from_branch 0);

          (* pcre2_compile.c:5761-5767 — there is at least one situation
             where code goes backwards: this is the case of a zero
             quantifier after a class (e.g. [ab]{0}). ... don't ever reduce
             the length at this point. *)
          if !code < !last_code then code := !last_code;

          (* pcre2_compile.c:5769-5787 — if the next thing is not a
             quantifier, we add the length of the previous item into the
             total, and reset the code pointer to the start of the
             workspace. Otherwise leave the previous item available to be
             quantified. *)
          if
            meta < Parse.meta_first_quantifier
            || meta > Parse.meta_last_quantifier
          then (
            if oflow_max - !length < !code - orig_code then (
              errorcodeptr := Errors.err20 (* Integer overflow *);
              return_from_branch 0);
            length := !length + (!code - orig_code);
            if !length > Limits.max_pattern_size then (
              errorcodeptr := Errors.err20 (* Pattern is too large *);
              return_from_branch 0);
            code := orig_code);

          (* pcre2_compile.c:5789-5792 — remember where this code item
             starts so we can catch the "backwards" case above next time
             round. *)
          last_code := !code
      | None -> ());

      (* pcre2_compile.c:5795-5804 — process the next parsed pattern item.
         If it is not a quantifier, remember where it starts so that it can
         be quantified when a quantifier follows. *)
      if meta < Parse.meta_first_quantifier || meta > Parse.meta_last_quantifier
      then (
        previous := !code;
        if !matched_char && not !had_accept then okreturn := 1);

      (* pcre2_compile.c:5806-5809. The C's per-iteration resets
         note_group_empty = FALSE and skipunits = 0 are the default
         parameter values the group_process call sites pass (see its
         header note). *)
      previous_matched_char := !matched_char;
      matched_char := false;

      (* switch(meta) (pcre2_compile.c:5811) *)
      if
        (* pcre2_compile.c:5813-5825 — the branch terminates at pattern
           end or | or ) *)
        Int.equal meta Parse.meta_end
        || Int.equal meta Parse.meta_alt
        || Int.equal meta Parse.meta_ket
      then (
        firstcuptr := !firstcu;
        firstcuflagsptr := !firstcuflags;
        reqcuptr := !reqcu;
        reqcuflagsptr := !reqcuflags;
        codeptr := !code;
        pptrptr := !pptr;
        return_from_branch !okreturn)
      else if Int.equal meta Parse.meta_circumflex then
        if
          (* pcre2_compile.c:5828-5840 — handle single-character
             metacharacters. In multiline mode, ^ disables the setting of any
             following char as a first character. *)
          not (Int.equal (!options land Options.multiline) 0)
        then (
          if Int.equal !firstcuflags req_unset then (
            zerofirstcuflags := req_none;
            firstcuflags := req_none);
          emit_cu Opcodes.op_circm)
        else emit_cu Opcodes.op_circ
      else if Int.equal meta Parse.meta_dollar then
        (* pcre2_compile.c:5842-5844 *)
        emit_cu
          (if not (Int.equal (!options land Options.multiline) 0) then
             Opcodes.op_dollm
           else Opcodes.op_doll)
      else if Int.equal meta Parse.meta_dot then (
        (* pcre2_compile.c:5846-5857 — there can never be a first char if
           '.' is first, whatever happens about repeats. The value of reqcu
           doesn't change either. *)
        matched_char := true;
        if Int.equal !firstcuflags req_unset then firstcuflags := req_none;
        zerofirstcu := !firstcu;
        zerofirstcuflags := !firstcuflags;
        zeroreqcu := !reqcu;
        zeroreqcuflags := !reqcuflags;
        emit_cu
          (if not (Int.equal (!options land Options.dotall) 0) then
             Opcodes.op_allany
           else Opcodes.op_any))
      else if
        Int.equal meta Parse.meta_class_empty
        || Int.equal meta Parse.meta_class_empty_not
      then (
        (* pcre2_compile.c:5860-5873 — empty character classes are allowed
           if PCRE2_ALLOW_EMPTY_CLASS is set. Otherwise, an initial ']' is
           taken as a data character. When empty classes are allowed, []
           must always fail, so generate OP_FAIL, whereas [^] must match
           any character, so generate OP_ALLANY. *)
        matched_char := true;
        emit_cu
          (if Int.equal meta Parse.meta_class_empty_not then Opcodes.op_allany
           else Opcodes.op_fail);
        if Int.equal !firstcuflags req_unset then firstcuflags := req_none;
        zerofirstcu := !firstcu;
        zerofirstcuflags := !firstcuflags)
      else if
        Int.equal meta Parse.meta_class_not || Int.equal meta Parse.meta_class
      then (
        (* pcre2_compile.c:5876-6484 — non-empty character class. If the
           included characters are all < 256, we build a 32-byte bitmap of
           the permitted characters, except in the special case where there
           is only one such character. For negated classes, we build the
           map as usual, then invert it at the end. However, we use a
           different opcode so that data characters > 255 can be handled
           correctly.

           If the class contains characters outside the 0-255 range, a
           different opcode is compiled (OP_XCLASS — M6, deferred; see the
           deferral notes below). *)
        matched_char := true;
        let negate_class = Int.equal meta Parse.meta_class_not in

        (* pcre2_compile.c:5894-5947 — we can optimize the case of a single
           character in a class by generating OP_CHAR or OP_CHARI if it's
           positive, or OP_NOT or OP_NOTI if it's negative. In the negative
           case there can be no first char if this item is first, whatever
           repeat count may follow. In the case of reqcu, save the previous
           value for reinstating. *)
        if
          cb.parsed_pattern.(!pptr + 1) < Parse.meta_end
          && Int.equal cb.parsed_pattern.(!pptr + 2) Parse.meta_class_end
        then (
          let c = cb.parsed_pattern.(!pptr + 1) in
          pptr := !pptr + 2 (* Move on to class end *);
          if Int.equal meta Parse.meta_class then
            (* pcre2_compile.c:5911-5915 — a positive one-char class can be
               handled as a normal literal character: meta = c;
               goto NORMAL_CHAR_SET. *)
            normal_char_set c
          else (
            (* pcre2_compile.c:5917-5923 — handle a negative one-character
               class. *)
            zeroreqcu := !reqcu;
            zeroreqcuflags := !reqcuflags;
            if Int.equal !firstcuflags req_unset then firstcuflags := req_none;
            zerofirstcu := !firstcu;
            zerofirstcuflags := !firstcuflags;

            (* pcre2_compile.c:5925-5941 — for caseless UTF or UCP mode,
               check whether this character has more than one other case.
               If so, generate a special OP_NOTPROP item instead of
               OP_NOTI. When restricted by PCRE2_EXTRA_CASELESS_RESTRICT,
               ignore any caseless set that starts with an ASCII character.
               DEVIATION: the OP_NOTPROP PT_CLIST emission is Unicode
               property machinery (M7, docs/ocaml-engine/08-ucp.md); until
               it lands this case fails loudly instead. *)
            (if
               (utf || ucp)
               && not (Int.equal (!options land Options.caseless) 0)
             then
               let d = Ucd.caseset c in
               if
                 (not (Int.equal d 0))
                 && (Int.equal
                       (!xoptions land Options.extra_caseless_restrict)
                       0
                    || Ucd_tables.ucd_caseless_sets.(d) > 127)
               then (
                 errorcodeptr := Parse.err_deferred;
                 return_from_branch 0));

            (* pcre2_compile.c:5942-5946 — char has only one other (usable)
               case, or UCP not available. *)
            emit_cu
              (if not (Int.equal (!options land Options.caseless) 0) then
                 Opcodes.op_noti
               else Opcodes.op_not);
            (* code += PUTCHAR(c, code) — pcre2_intmodedep.h:357-358: with
               utf and c > 127 this is a multi-code-unit ord2utf store.
               DEVIATION: ord2utf emission is M6 (docs/ocaml-engine/
               07-utf.md); defer loudly rather than truncate. *)
            if utf && c > 127 then (
              errorcodeptr := Parse.err_deferred;
              return_from_branch 0);
            emit_cu c (* We are finished with this class *)))
        else
          (* pcre2_compile.c:5949-5992 — handle character classes that
             contain more than just one literal character. If there are
             exactly two characters in a positive class, see if they are
             case partners. This can be optimized to generate a caseless
             single character match (which also sets first/required code
             units if relevant). When casing restrictions apply, ignore a
             caseless set if both characters are ASCII. *)
          let caseless_pair =
            if
              Int.equal meta Parse.meta_class
              && cb.parsed_pattern.(!pptr + 1) < Parse.meta_end
              && cb.parsed_pattern.(!pptr + 2) < Parse.meta_end
              && Int.equal cb.parsed_pattern.(!pptr + 3) Parse.meta_class_end
            then
              let c = cb.parsed_pattern.(!pptr + 1) in
              if
                Int.equal (Ucd.caseset c) 0
                || (not
                      (Int.equal
                         (!xoptions land Options.extra_caseless_restrict)
                         0))
                   && c < 128
                   && cb.parsed_pattern.(!pptr + 2) < 128
              then
                (* pcre2_compile.c:5969-5977 — find the other case: the UCD
                   for high code points under UTF/UCP, otherwise the fcc
                   table (TABLE_GET(c, cb->fcc, c); c <= 255 whenever this
                   arm is reached in the 8-bit library). *)
                let d =
                  if (utf || ucp) && c > 127 then Ucd.othercase c
                  else Chartables.fcc c
                in
                if
                  (not (Int.equal c d))
                  && Int.equal cb.parsed_pattern.(!pptr + 2) d
                then (
                  (* pcre2_compile.c:5979-5990 *)
                  pptr := !pptr + 3 (* Move on to class end *);
                  if Int.equal (!options land Options.caseless) 0 then (
                    reset_caseful := true;
                    options := !options lor Options.caseless;
                    req_caseopt := req_caseless);
                  (* goto CLASS_CASELESS_CHAR *)
                  class_caseless_char c;
                  true)
                else false
              else false
            else false
          in
          if not caseless_pair then (
            (* pcre2_compile.c:5994-6000 — if a non-extended class contains
               a negative special such as \S, we need to flip the negation
               flag at the end, so that support for characters > 255 works
               correctly (they are all included in the class). An extended
               class may need to insert specific matching or non-matching
               code for wide characters. *)
            let should_flip_negation = ref false in
            let match_all_or_no_wide_chars = ref false in

            (* pcre2_compile.c:6002-6009 — extended class (xclass) will be
               used when characters > 255 might match. class_uchardata is
               the XCLASS extra-data write cursor (offset into
               cb.start_code); in M1 nothing ever writes through it — every
               producer defers loudly first — but the plumbing is kept so
               M6 lands in the C's shape. *)
            let xclass = ref false in
            let class_uchardata =
              ref (!code + Limits.link_size + 2)
              (* For XCLASS items *)
            in
            let class_uchardata_base = !class_uchardata (* Save the start *) in

            (* pcre2_compile.c:6011-6019 — for optimization purposes, we
               track some properties of the class: class_has_8bitchar will
               be non-zero if the class contains at least one character
               with a code point less than 256; xclass_has_prop will be
               TRUE if Unicode property checks are present in the class.
               Both are only consumed by the OP_XCLASS emission (M6);
               tracked here so the class loop matches the C. *)
            let class_has_8bitchar = ref 0 in
            let xclass_has_prop = ref false in

            (* pcre2_compile.c:6021-6026 — initialize the 256-bit (32-byte)
               bit map to all zeros (fresh buffer = the C's memset of the
               function-scope classbits). *)
            let classbits = Bytes.make 32 '\000' in

            (* pcre2_compile.c:6262-6346 — the CLASS_LITERAL label: a
               literal character, possibly the start of a range. At parse
               time there are checks for out-of-order characters, for
               ranges where the two characters are equal, and for hyphens
               that cannot indicate a range, so no checking is needed
               here. Factored as a function because both the META_BIGVALUE
               arm and the plain-literal default reach it (goto
               CLASS_LITERAL). *)
            let class_literal (c : int) : unit =
              (* pcre2_compile.c:6274-6276 — remember if \r or \n were
                 explicitly used. *)
              if Int.equal c 0x0d || Int.equal c 0x0a then
                cb.external_flags <- cb.external_flags lor hascrorlf;

              (* pcre2_compile.c:6278-6338 — process a character range. *)
              if
                Int.equal cb.parsed_pattern.(!pptr + 1) Parse.meta_range_literal
                || Int.equal
                     cb.parsed_pattern.(!pptr + 1)
                     Parse.meta_range_escaped
              then (
                pptr := !pptr + 2;
                let d = ref cb.parsed_pattern.(!pptr) in
                if Int.equal !d Parse.meta_bigvalue then (
                  pptr := !pptr + 1;
                  d := cb.parsed_pattern.(!pptr));

                (* pcre2_compile.c:6289-6291 — remember an explicit \r or
                   \n, and add the range to the class. *)
                if Int.equal !d 0x0d || Int.equal !d 0x0a then
                  cb.external_flags <- cb.external_flags lor hascrorlf;

                (* pcre2_compile.c:6293-6336 — the EBCDIC special-range
                   block (6298-6331) does not apply: not an EBCDIC
                   environment. *)
                class_has_8bitchar :=
                  !class_has_8bitchar
                  + add_to_class classbits class_uchardata !options !xoptions cb
                      errorcodeptr c !d
                (* goto CONTINUE_CLASS *))
              else
                (* pcre2_compile.c:6341-6345 — handle a single character. *)
                class_has_8bitchar :=
                  !class_has_8bitchar
                  + add_to_class classbits class_uchardata !options !xoptions cb
                      errorcodeptr c c
            in

            (* pcre2_compile.c:6028-6371 — process items until
               META_CLASS_END is reached:
               while ((meta = *(++pptr)) != META_CLASS_END). *)
            let done_ = ref false in
            while not !done_ do
              pptr := !pptr + 1;
              let item = cb.parsed_pattern.(!pptr) in
              if Int.equal item Parse.meta_class_end then done_ := true
              else (
                if
                  (* pcre2_compile.c:6032-6143 — handle POSIX classes such
                     as [:alpha:] etc. *)
                  Int.equal item Parse.meta_posix
                  || Int.equal item Parse.meta_posix_neg
                then (
                  let local_negate = Int.equal item Parse.meta_posix_neg in
                  pptr := !pptr + 1;
                  let posix_class = ref cb.parsed_pattern.(!pptr) in
                  should_flip_negation := local_negate
                  (* Note negative special *);

                  (* pcre2_compile.c:6043-6048 — if matching is caseless,
                     upper and lower are converted to alpha. This relies
                     on the fact that the class table starts with alpha,
                     lower, upper as the first 3 entries. *)
                  if
                    (not (Int.equal (!options land Options.caseless) 0))
                    && !posix_class <= 2
                  then posix_class := 0;

                  (* pcre2_compile.c:6050-6096 — when PCRE2_UCP is set,
                     some of the POSIX classes are converted to different
                     escape sequences that use Unicode properties \p or \P
                     (done at parse time, Parse.posix_substitutes). Others
                     that are not available via \p or \P have to generate
                     XCL_PROP/XCL_NOTPROP directly, which is done here. *)
                  if
                    (not (Int.equal (!options land Options.ucp) 0))
                    && Int.equal (!xoptions land Options.extra_ascii_posix) 0
                  then
                    if
                      Int.equal !posix_class pc_graph
                      || Int.equal !posix_class pc_print
                      || Int.equal !posix_class pc_punct
                    then (
                      (* pcre2_compile.c:6061-6070 — XCL_PROP/XCL_NOTPROP
                         PT_PXGRAPH/PT_PXPRINT/PT_PXPUNCT emission.
                         DEVIATION: Unicode property classes are M7
                         (docs/ocaml-engine/08-ucp.md); fails loudly, both
                         phases. *)
                      errorcodeptr := Parse.err_deferred;
                      return_from_branch 0)
                    else if utf then
                      (* pcre2_compile.c:6072-6093 — for the other POSIX
                         classes (ex: ascii) we fall through to the
                         non-UCP case and build a bit map for characters
                         with code points less than 256. In a negated
                         POSIX class, characters with code points greater
                         than 255 must either all match or all not match;
                         setting this flag causes an explicit range to be
                         generated later when it is known that OP_XCLASS
                         is required. In the 8-bit library this is
                         relevant only in utf mode. *)
                      match_all_or_no_wide_chars :=
                        !match_all_or_no_wide_chars || local_negate;

                  (* pcre2_compile.c:6098-6109 — in the non-UCP case, or
                     when UCP makes no difference, we build the bit map
                     for the POSIX class in a chunk of local store because
                     we may be adding and subtracting from it, and we
                     don't want to subtract bits that may be in the main
                     map already. At the end we or the result into the bit
                     map that is being built. Copy in the first table
                     (always present). *)
                  let posix_class = !posix_class * 3 in
                  let pbits = Bytes.make 32 '\000' in
                  for i = 0 to 31 do
                    Bytes.set pbits i
                      (Char.chr
                         (Chartables.cbits (i + posix_class_maps.(posix_class))))
                  done;

                  (* pcre2_compile.c:6111-6122 — if there is a second
                     table, add or remove it as required. *)
                  let taboffset = posix_class_maps.(posix_class + 1) in
                  let tabopt = posix_class_maps.(posix_class + 2) in
                  if taboffset >= 0 then
                    if tabopt >= 0 then
                      for i = 0 to 31 do
                        Bytes.set pbits i
                          (Char.chr
                             (Char.code (Bytes.get pbits i)
                             lor Chartables.cbits (i + taboffset)))
                      done
                    else
                      for i = 0 to 31 do
                        Bytes.set pbits i
                          (Char.chr
                             (Char.code (Bytes.get pbits i)
                             land (lnot (Chartables.cbits (i + taboffset))
                                  land 0xff)))
                      done;

                  (* pcre2_compile.c:6124-6129 — now see if we need to
                     remove any special characters. An option value of 1
                     removes vertical space and 2 removes underscore. *)
                  let tabopt = if tabopt < 0 then -tabopt else tabopt in
                  if Int.equal tabopt 1 then
                    Bytes.set pbits 1
                      (Char.chr (Char.code (Bytes.get pbits 1) land lnot 0x3c))
                  else if Int.equal tabopt 2 then
                    Bytes.set pbits 11
                      (Char.chr (Char.code (Bytes.get pbits 11) land 0x7f));

                  (* pcre2_compile.c:6131-6141 — add the POSIX table or
                     its complement into the main table that is being
                     built and we are done. Every class contains at least
                     one < 256 character. *)
                  if local_negate then
                    for i = 0 to 31 do
                      Bytes.set classbits i
                        (Char.chr
                           (Char.code (Bytes.get classbits i)
                           lor (lnot (Char.code (Bytes.get pbits i)) land 0xff)
                           ))
                    done
                  else
                    for i = 0 to 31 do
                      Bytes.set classbits i
                        (Char.chr
                           (Char.code (Bytes.get classbits i)
                           lor Char.code (Bytes.get pbits i)))
                    done;
                  class_has_8bitchar := 1
                  (* goto CONTINUE_CLASS — end of POSIX handling *))
                else if Int.equal item Parse.meta_bigvalue then (
                  (* pcre2_compile.c:6148-6152 — other than POSIX classes,
                     the only items we should encounter are \d-type
                     escapes and literal characters (possibly as
                     ranges). *)
                  pptr := !pptr + 1;
                  (* goto CLASS_LITERAL *)
                  class_literal cb.parsed_pattern.(!pptr))
                else if item >= Parse.meta_end then (
                  (* pcre2_compile.c:6154-6166 — any other non-literal
                     must be an escape. *)
                  if not (Int.equal (Parse.meta_code item) Parse.meta_escape)
                  then (
                    errorcodeptr := Errors.err89
                    (* Internal error - unrecognized *);
                    return_from_branch 0);
                  let escape = Parse.meta_data item in

                  (* pcre2_compile.c:6169-6171 — every class contains at
                     least one < 256 character. *)
                  class_has_8bitchar := !class_has_8bitchar + 1;

                  (* switch(escape), pcre2_compile.c:6173-6257 *)
                  if Int.equal escape Parse.esc_d then
                    for i = 0 to 31 do
                      Bytes.set classbits i
                        (Char.chr
                           (Char.code (Bytes.get classbits i)
                           lor Chartables.cbits (i + Chartables.cbit_digit)))
                    done
                  else if Int.equal escape Parse.esc_big_d then (
                    should_flip_negation := true;
                    for i = 0 to 31 do
                      Bytes.set classbits i
                        (Char.chr
                           (Char.code (Bytes.get classbits i)
                           lor (lnot
                                  (Chartables.cbits (i + Chartables.cbit_digit))
                               land 0xff)))
                    done)
                  else if Int.equal escape Parse.esc_w then
                    for i = 0 to 31 do
                      Bytes.set classbits i
                        (Char.chr
                           (Char.code (Bytes.get classbits i)
                           lor Chartables.cbits (i + Chartables.cbit_word)))
                    done
                  else if Int.equal escape Parse.esc_big_w then (
                    should_flip_negation := true;
                    for i = 0 to 31 do
                      Bytes.set classbits i
                        (Char.chr
                           (Char.code (Bytes.get classbits i)
                           lor (lnot
                                  (Chartables.cbits (i + Chartables.cbit_word))
                               land 0xff)))
                    done)
                  else if Int.equal escape Parse.esc_s then
                    (* pcre2_compile.c:6195-6204 — from PCRE 8.34 we no
                       longer treat \s and \S specially (VT). *)
                    for i = 0 to 31 do
                      Bytes.set classbits i
                        (Char.chr
                           (Char.code (Bytes.get classbits i)
                           lor Chartables.cbits (i + Chartables.cbit_space)))
                    done
                  else if Int.equal escape Parse.esc_big_s then (
                    should_flip_negation := true;
                    for i = 0 to 31 do
                      Bytes.set classbits i
                        (Char.chr
                           (Char.code (Bytes.get classbits i)
                           lor (lnot
                                  (Chartables.cbits (i + Chartables.cbit_space))
                               land 0xff)))
                    done)
                  else if Int.equal escape Parse.esc_h then
                    (* pcre2_compile.c:6212-6222 — when adding the
                       horizontal or vertical space lists to a class, or
                       their complements, disable PCRE2_CASELESS, because
                       it just wastes time, and in the "not-x" UTF cases
                       can create unwanted duplicates in the XCLASS
                       list. *)
                    ignore
                      (add_list_to_class classbits class_uchardata
                         (!options land lnot Options.caseless)
                         !xoptions cb errorcodeptr Tables.hspace_list
                         Tables.notachar)
                  else if Int.equal escape Parse.esc_big_h then
                    ignore
                      (add_not_list_to_class classbits class_uchardata
                         (!options land lnot Options.caseless)
                         !xoptions cb errorcodeptr Tables.hspace_list)
                  else if Int.equal escape Parse.esc_v then
                    ignore
                      (add_list_to_class classbits class_uchardata
                         (!options land lnot Options.caseless)
                         !xoptions cb errorcodeptr Tables.vspace_list
                         Tables.notachar)
                  else if Int.equal escape Parse.esc_big_v then
                    ignore
                      (add_not_list_to_class classbits class_uchardata
                         (!options land lnot Options.caseless)
                         !xoptions cb errorcodeptr Tables.vspace_list)
                  else if
                    Int.equal escape Parse.esc_p
                    || Int.equal escape Parse.esc_big_p
                  then (
                    (* pcre2_compile.c:6243-6256 — \p and \P in a class:
                       XCL_PROP/XCL_NOTPROP extra data, xclass_has_prop,
                       and the class_has_8bitchar-- undo. DEVIATION:
                       Unicode property classes are M7
                       (docs/ocaml-engine/08-ucp.md); fails loudly, both
                       phases. *)
                    errorcodeptr := Parse.err_deferred;
                    return_from_branch 0)
                  (* no other escape reaches a class item: the C switch
                     has no default and falls through to CONTINUE_CLASS
                     with only the class_has_8bitchar increment above *))
                else
                  (* pcre2_compile.c:6267-6272 — a literal character,
                     CLASS_LITERAL. *)
                  class_literal item;

                (* pcre2_compile.c:6348-6370 — CONTINUE_CLASS. DEVIATION:
                   first propagate a deferral recorded by the add_to_class
                   family (see its DEVIATION note); the C helpers have no
                   error path. *)
                if not (Int.equal !errorcodeptr 0) then return_from_branch 0;
                (* If any wide characters or Unicode properties have been
                   encountered, set xclass = TRUE. Then, in the pre-compile
                   phase, accumulate the length of the extra data and reset
                   the pointer. Dead in M1 (nothing writes extra data), kept
                   for M6. *)
                if !class_uchardata > class_uchardata_base then (
                  xclass := true;
                  match lengthptr with
                  | Some length ->
                      length :=
                        !length + (!class_uchardata - class_uchardata_base);
                      class_uchardata := class_uchardata_base
                  | None -> ()))
            done
            (* End of main class-processing loop *);

            (* pcre2_compile.c:6373-6381 — if this class is the first thing
               in the branch, there can be no first char setting, whatever
               the repeat count. Any reqcu setting must remain unchanged
               after any kind of repeat. *)
            if Int.equal !firstcuflags req_unset then firstcuflags := req_none;
            zerofirstcu := !firstcu;
            zerofirstcuflags := !firstcuflags;
            zeroreqcu := !reqcu;
            zeroreqcuflags := !reqcuflags;

            (* pcre2_compile.c:6383-6465 — if there are characters with
               values > 255, or Unicode property settings (\p or \P), we
               have to compile an extended class (OP_XCLASS), unless there
               were no property settings and there was a negated special
               such as \S in the class, and PCRE2_UCP is not set.
               DEVIATION: OP_XCLASS emission is M6
               (docs/ocaml-engine/07-utf.md). xclass can never be true in
               M1 — every extra-data producer defers loudly above — but the
               C's entry condition is kept so both phases stay in step when
               M6 lands. *)
            if
              !xclass
              && ((not (Int.equal (!options land Options.ucp) 0))
                 || !xclass_has_prop || not !should_flip_negation)
            then (
              errorcodeptr := Parse.err_deferred;
              return_from_branch 0);

            (* pcre2_compile.c:6467-6484 — if there are no characters
               > 255, or they are all to be included or excluded, set the
               opcode to OP_CLASS or OP_NCLASS, depending on whether the
               whole class was negated and whether there were negative
               specials such as \S (non-UCP) in the class. Then copy the
               32-byte map into the code vector, negating it if
               necessary. *)
            emit_cu
              (if Bool.equal negate_class !should_flip_negation then
                 Opcodes.op_class
               else Opcodes.op_nclass);
            (match lengthptr with
            | None ->
                (* Save time in the pre-compile phase *)
                if negate_class then
                  (* Using 255 ^ instead of ~ (a note for the C, exact here
                     anyway). *)
                  for i = 0 to 31 do
                    Bytes.set classbits i
                      (Char.chr (255 lxor Char.code (Bytes.get classbits i)))
                  done;
                Bytes.blit classbits 0 cb.start_code !code 32
            | Some _ -> ());
            code := !code + 32 (* End of class processing *)))
      else if
        Int.equal meta Parse.meta_accept
        || Int.equal meta Parse.meta_prune
        || Int.equal meta Parse.meta_skip
        || Int.equal meta Parse.meta_commit
        || Int.equal meta Parse.meta_fail
        || Int.equal meta Parse.meta_then
        || Int.equal meta Parse.meta_then_arg
        || Int.equal meta Parse.meta_prune_arg
        || Int.equal meta Parse.meta_skip_arg
        || Int.equal meta Parse.meta_mark
        || Int.equal meta Parse.meta_commit_arg
      then (
        (* pcre2_compile.c:6487-6573 — ( *VERB)s:
           docs/ocaml-engine/06-verbs-k-start-opt.md. Deferred loudly. *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
      else if Int.equal meta Parse.meta_options then (
        (* pcre2_compile.c:6576-6587 — handle options change. The new
           setting must be passed back for use in subsequent branches.
           Reset the greedy defaults and the case value for firstcu and
           reqcu. META_OPTIONS is followed by TWO words (options, then
           xoptions) even though the meta_extra_lengths table says 1: the C
           consumers hardcode two ++pptr reads, as here. *)
        pptr := !pptr + 1;
        options := cb.parsed_pattern.(!pptr);
        optionsptr := !options;
        pptr := !pptr + 1;
        xoptions := cb.parsed_pattern.(!pptr);
        xoptionsptr := !xoptions;
        greedy_default :=
          if not (Int.equal (!options land Options.ungreedy) 0) then 1 else 0;
        greedy_non_default := !greedy_default lxor 1;
        req_caseopt :=
          if not (Int.equal (!options land Options.caseless) 0) then
            req_caseless
          else 0)
      else if
        Int.equal meta Parse.meta_cond_rnumber
        || Int.equal meta Parse.meta_cond_name
        || Int.equal meta Parse.meta_cond_rname
        || Int.equal meta Parse.meta_cond_define
        || Int.equal meta Parse.meta_cond_number
        || Int.equal meta Parse.meta_cond_version
        || Int.equal meta Parse.meta_cond_assert
      then (
        (* pcre2_compile.c:6590-6745 — conditional subpatterns:
           docs/ocaml-engine/05-conditionals-recursion.md. Deferred
           loudly. *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
      else if Int.equal meta Parse.meta_lookahead then (
        (* pcre2_compile.c:6748-6755 — handle all kinds of nested bracketed
           groups. The non-capturing, non-conditional cases are here;
           others come to GROUP_PROCESS via goto. *)
        cb.assert_depth <- cb.assert_depth + 1;
        group_process ~note_group_empty:false ~bravalue:Opcodes.op_assert
          ~skipunits:0)
      else if Int.equal meta Parse.meta_lookahead_na then (
        (* pcre2_compile.c:6757-6760 *)
        cb.assert_depth <- cb.assert_depth + 1;
        group_process ~note_group_empty:false ~bravalue:Opcodes.op_assert_na
          ~skipunits:0)
      else if Int.equal meta Parse.meta_lookaheadnot then
        if
          (* pcre2_compile.c:6762-6781 — optimize (?!) to ( *FAIL) unless it
             is quantified - which is a weird thing to do, but Perl allows
             all assertions to be quantified, and when they contain capturing
             parentheses there may be a potential use for this feature. Not
             that that applies to a quantified (?!) but we allow it for
             uniformity. *)
          Int.equal cb.parsed_pattern.(!pptr + 1) Parse.meta_ket
          && (cb.parsed_pattern.(!pptr + 2) < Parse.meta_asterisk
             || cb.parsed_pattern.(!pptr + 2) > Parse.meta_minmax_query)
        then (
          emit_cu Opcodes.op_fail;
          pptr := !pptr + 1)
        else (
          cb.assert_depth <- cb.assert_depth + 1;
          group_process ~note_group_empty:false ~bravalue:Opcodes.op_assert_not
            ~skipunits:0)
      else if Int.equal meta Parse.meta_lookbehind then (
        (* pcre2_compile.c:6783-6786 *)
        cb.assert_depth <- cb.assert_depth + 1;
        group_process ~note_group_empty:false ~bravalue:Opcodes.op_assertback
          ~skipunits:0)
      else if Int.equal meta Parse.meta_lookbehindnot then (
        (* pcre2_compile.c:6788-6791 *)
        cb.assert_depth <- cb.assert_depth + 1;
        group_process ~note_group_empty:false
          ~bravalue:Opcodes.op_assertback_not ~skipunits:0)
      else if Int.equal meta Parse.meta_lookbehind_na then (
        (* pcre2_compile.c:6793-6796 *)
        cb.assert_depth <- cb.assert_depth + 1;
        group_process ~note_group_empty:false ~bravalue:Opcodes.op_assertback_na
          ~skipunits:0)
      else if Int.equal meta Parse.meta_atomic then
        (* pcre2_compile.c:6798-6800 *)
        group_process ~note_group_empty:true ~bravalue:Opcodes.op_once
          ~skipunits:0
      else if Int.equal meta Parse.meta_script_run then (
        (* pcre2_compile.c:6802-6804 — ( *script_run:): OP_SCRIPT_RUN needs
           the UCP machinery (docs/ocaml-engine/08-ucp.md). Deferred loudly
           (parse_regex defers the ( *sr: forms with the same marker, so
           this is unreachable until M8). *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
      else if Int.equal meta Parse.meta_nocapture then
        (* pcre2_compile.c:6806-6808 — bravalue = OP_BRA; fall through to
           GROUP_PROCESS_NOTE_EMPTY. *)
        group_process ~note_group_empty:true ~bravalue:Opcodes.op_bra
          ~skipunits:0
      else if
        Int.equal meta Parse.meta_backref_byname
        || Int.equal meta Parse.meta_recurse_byname
      then (
        (* pcre2_compile.c:7018-7031 — handle named backreferences and
           recursions. The C's name pointer (cb->start_pattern + offset)
           becomes the pattern offset itself; GETPLUSOFFSET reads one extra
           parsed-pattern word (SIZEOFFSET 1, pcre2_compile.c:91-97). *)
        let is_dupname = ref false in
        pptr := !pptr + 1;
        let length = cb.parsed_pattern.(!pptr) in
        pptr := !pptr + 1;
        let offset = cb.parsed_pattern.(!pptr) in
        let name = offset in

        (* pcre2_compile.c:7033-7064 — in the first pass, the names
           generated in the pre-pass are available, but the main name
           table has not yet been created. Scan the list of names
           generated in the pre-pass in order to get a number and whether
           or not this name is duplicated. The PRIV(strncmp)
           (pcre2_string_utils.c:156-167) becomes a code-unit loop over
           the pattern; only its equality result is used. *)
        let groupnumber = ref 0 in
        for i = 0 to cb.names_found - 1 do
          let ng = cb.named_groups.(i) in
          let name_eq =
            Int.equal length ng.Parse.length
            &&
            let rec cmp j =
              if j >= length then true
              else if
                Char.equal cb.pattern.[name + j] cb.pattern.[ng.Parse.name + j]
              then (cmp [@tailcall]) (j + 1)
              else false
            in
            cmp 0
          in
          if name_eq then (
            is_dupname := ng.Parse.isdup;
            groupnumber := ng.Parse.number;

            (* pcre2_compile.c:7047-7055 — for a recursion, that's all
               that is needed: goto HANDLE_NUMERICAL_RECURSION, applying
               it to the first group with the given name. The target
               label lives in the META_RECURSE arm (pcre2_compile.c:8078)
               — M5, docs/ocaml-engine/05-conditionals-recursion.md —
               and is unreachable here until parse_regex stops deferring
               META_RECURSE_BYNAME emission. Deferred loudly. *)
            if Int.equal meta Parse.meta_recurse_byname then (
              errorcodeptr := Parse.err_deferred;
              return_from_branch 0);

            (* pcre2_compile.c:7057-7062 — for a back reference, update
               the back reference map and the maximum back reference. *)
            cb.backref_map <-
              (cb.backref_map
              lor if !groupnumber < 32 then 1 lsl !groupnumber else 1);
            if !groupnumber > cb.top_backref then cb.top_backref <- !groupnumber)
        done;

        (* pcre2_compile.c:7066-7073 — if the name was not found we have a
           bad reference. *)
        if Int.equal !groupnumber 0 then (
          errorcodeptr := Errors.err15;
          cb.erroroffset <- offset;
          return_from_branch 0);

        (* pcre2_compile.c:7075-7082 — if a back reference name is not
           duplicated, we can handle it as a numerical reference: goto
           HANDLE_SINGLE_REFERENCE. *)
        if not !is_dupname then handle_single_reference !groupnumber
        else
          (* pcre2_compile.c:7084-7096 — if a back reference name is
             duplicated, we generate a different opcode to a numerical
             back reference. In the second pass we must search for the
             index and count in the final name table. *)
          let count = ref 0 (* Values for first pass *) in
          let index = ref 0 in
          (match lengthptr with
          | None ->
              if
                not
                  (find_dupname_details ~name ~length index count errorcodeptr
                     cb)
              then return_from_branch 0
          | Some _ -> ());
          if Int.equal !firstcuflags req_unset then firstcuflags := req_none;
          emit_cu
            (if not (Int.equal (!options land Options.caseless) 0) then
               Opcodes.op_dnrefi
             else Opcodes.op_dnref);
          put2inc cb.start_code code !index;
          put2inc cb.start_code code !count)
      else if Int.equal meta Parse.meta_callout_number then (
        (* pcre2_compile.c:7101-7111 — handle a numerical callout. *)
        Bytes.set cb.start_code !code (Char.chr Opcodes.op_callout);
        put cb.start_code (!code + 1) cb.parsed_pattern.(!pptr + 1)
        (* Offset to next pattern item *);
        put cb.start_code
          (!code + 1 + Limits.link_size)
          cb.parsed_pattern.(!pptr + 2)
        (* Length of next pattern item *);
        Bytes.set cb.start_code
          (!code + 1 + (2 * Limits.link_size))
          (Char.chr (cb.parsed_pattern.(!pptr + 3) land 0xff));
        pptr := !pptr + 3;
        code := !code + Opcodes.op_lengths.(Opcodes.op_callout))
      else if Int.equal meta Parse.meta_callout_string then (
        (* pcre2_compile.c:7114-7175 — callout with a string argument.
           Deferred loudly (string callouts land with the verbs/callout
           chunk, docs/ocaml-engine/06-verbs-k-start-opt.md). *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
      else if
        meta >= Parse.meta_first_quantifier
        && meta <= Parse.meta_last_quantifier
      then (
        (* pcre2_compile.c:7178-8011 — handle repetition. The different
           types are all sorted out in the parsing pass. The C locals this
           arm owns (repeat_min/repeat_max, repeat_type, op_type, reqvary:
           pcre2_compile.c:5645,5647,5657; tempcode, op_previous:
           5663,5665; possessive_quantifier, mclength, mcbuffer:
           5731,5734,5741) are declared here — this arm is their only
           reader, except group_return/length_prevgroup/groupsetfirstcu,
           which the group arm writes (function-level refs above); the
           bracket-repeat case owns bralink and brazeroptr
           (pcre2_compile.c:7444-7445). *)
        let repeat_min = ref 0
        and repeat_max = ref 0 in
        if
          Int.equal meta Parse.meta_minmax_plus
          || Int.equal meta Parse.meta_minmax_query
          || Int.equal meta Parse.meta_minmax
        then (
          (* pcre2_compile.c:7182-7187 — repeat_min = *(++pptr);
             repeat_max = *(++pptr); *)
          pptr := !pptr + 1;
          repeat_min := cb.parsed_pattern.(!pptr);
          pptr := !pptr + 1;
          repeat_max := cb.parsed_pattern.(!pptr))
        else if
          Int.equal meta Parse.meta_asterisk
          || Int.equal meta Parse.meta_asterisk_plus
          || Int.equal meta Parse.meta_asterisk_query
        then (
          (* pcre2_compile.c:7189-7194 *)
          repeat_min := 0;
          repeat_max := Limits.repeat_unlimited)
        else if
          Int.equal meta Parse.meta_plus
          || Int.equal meta Parse.meta_plus_plus
          || Int.equal meta Parse.meta_plus_query
        then (
          (* pcre2_compile.c:7196-7201 *)
          repeat_min := 1;
          repeat_max := Limits.repeat_unlimited)
        else (
          (* pcre2_compile.c:7203-7207 — the META_QUERY family falls
             through to REPEAT *)
          repeat_min := 0;
          repeat_max := 1);

        (* pcre2_compile.c:7209-7210 — the REPEAT label. *)
        if !previous_matched_char && !repeat_min > 0 then matched_char := true;

        (* pcre2_compile.c:7212-7216 — remember whether this is a variable
           length repeat, and default to single-char opcodes. *)
        let reqvary =
          if Int.equal !repeat_min !repeat_max then 0 else req_vary
        in
        let op_type = ref 0 in

        (* pcre2_compile.c:7218-7226 — adjust first and required code
           units for a zero repeat. *)
        if Int.equal !repeat_min 0 then (
          firstcu := !zerofirstcu;
          firstcuflags := !zerofirstcuflags;
          reqcu := !zeroreqcu;
          reqcuflags := !zeroreqcuflags);

        (* pcre2_compile.c:7228-7252 — note the greediness and
           possessiveness. *)
        let repeat_type = ref 0 in
        let possessive_quantifier = ref false in
        if
          Int.equal meta Parse.meta_minmax_plus
          || Int.equal meta Parse.meta_asterisk_plus
          || Int.equal meta Parse.meta_plus_plus
          || Int.equal meta Parse.meta_query_plus
        then (
          repeat_type := 0 (* Force greedy *);
          possessive_quantifier := true)
        else if
          Int.equal meta Parse.meta_minmax_query
          || Int.equal meta Parse.meta_asterisk_query
          || Int.equal meta Parse.meta_plus_query
          || Int.equal meta Parse.meta_query_query
        then (
          repeat_type := !greedy_non_default;
          possessive_quantifier := false)
        else (
          repeat_type := !greedy_default;
          possessive_quantifier := false);

        (* pcre2_compile.c:7254-7258 — save start of previous item, in
           case we have to move it up in order to insert something before
           it, and remember what it was. *)
        let tempcode = ref !previous in
        let op_previous = Char.code (Bytes.get cb.start_code !previous) in

        (* pcre2_compile.c:7784-7904 — the OUTPUT_SINGLE_REPEAT label and
           the shared single-item repeat emission that follows it, factored
           as a function because both the single-character cases (goto at
           7309) and the character-type default case reach it. On entry:
           mclength = 1 with the character in mcbuffer0 (single code unit),
           or mclength = 0 for a non-property character type in
           op_previous; prop_type/prop_value >= 0 carry a property
           character type (OP_PROP/OP_NOTPROP, M7). op_type has been set by
           the caller; repeat_type does not yet include it. *)
        let output_single_repeat ~(mclength : int) ~(mcbuffer0 : int)
            ~(prop_type : int) ~(prop_value : int) : unit =
          (* pcre2_compile.c:7853-7866 and 7890-7903 — the C duplicates
             this fill; factored (port-conventions §2). mclength is 1 or 0
             in M1: multi-code-unit UTF characters were deferred by the
             caller (see the M6 DEVIATION in the OP_CHAR case). *)
          let emit_char_or_type () =
            if mclength > 0 then emit_cu mcbuffer0
            else (
              emit_cu op_previous;
              if prop_type >= 0 then (
                emit_cu prop_type;
                emit_cu prop_value))
          in
          (* pcre2_compile.c:7794-7795 *)
          let oldcode = !code (* Save where we were *) in
          code := !previous (* Usually overwrite previous item *);

          (* pcre2_compile.c:7797-7800 — if the maximum is zero then the
             minimum must also be zero; Perl allows this case, so we do
             too - by simply omitting the item altogether. *)
          if Int.equal !repeat_max 0 then raise_notrace End_repeat;

          (* pcre2_compile.c:7802-7804 — combine the op_type with the
             repeat_type. *)
          repeat_type := !repeat_type + !op_type;

          if Int.equal !repeat_min 0 then
            if
              (* pcre2_compile.c:7806-7818 — a minimum of zero is handled
                 either as the special case * or ?, or as an UPTO, with the
                 maximum given. *)
              Int.equal !repeat_max Limits.repeat_unlimited
            then emit_cu (Opcodes.op_star + !repeat_type)
            else if Int.equal !repeat_max 1 then
              emit_cu (Opcodes.op_query + !repeat_type)
            else (
              emit_cu (Opcodes.op_upto + !repeat_type);
              put2inc cb.start_code code !repeat_max)
          else if Int.equal !repeat_min 1 then
            if
              (* pcre2_compile.c:7820-7836 — a repeat minimum of 1 is
                 optimized into some special cases. If the maximum is
                 unlimited, we use OP_PLUS. Otherwise, the original item is
                 left in place and, if the maximum is greater than 1, we use
                 OP_UPTO with one less than the maximum. *)
              Int.equal !repeat_max Limits.repeat_unlimited
            then emit_cu (Opcodes.op_plus + !repeat_type)
            else (
              code := oldcode (* Leave previous item in place *);
              if Int.equal !repeat_max 1 then raise_notrace End_repeat;
              emit_cu (Opcodes.op_upto + !repeat_type);
              put2inc cb.start_code code (!repeat_max - 1))
          else (
            (* pcre2_compile.c:7838-7844 — the case {n,n} is just an
               EXACT, while the general case {n,m} is handled as an EXACT
               followed by an UPTO or STAR or QUERY. *)
            emit_cu (Opcodes.op_exact + !op_type)
            (* NB EXACT doesn't have repeat_type *);
            put2inc cb.start_code code !repeat_min;

            (* pcre2_compile.c:7846-7885 — unless repeat_max equals
               repeat_min, fill in the data for EXACT, and then generate
               the second opcode. For a repeated Unicode property match,
               there are two extra values that define the required
               property, and mclength is set zero to indicate this. *)
            if not (Int.equal !repeat_max !repeat_min) then (
              emit_char_or_type ();
              (* pcre2_compile.c:7868-7884 — now set up the following
                 opcode. *)
              if Int.equal !repeat_max Limits.repeat_unlimited then
                emit_cu (Opcodes.op_star + !repeat_type)
              else (
                repeat_max := !repeat_max - !repeat_min;
                if Int.equal !repeat_max 1 then
                  emit_cu (Opcodes.op_query + !repeat_type)
                else (
                  emit_cu (Opcodes.op_upto + !repeat_type);
                  put2inc cb.start_code code !repeat_max))));

          (* pcre2_compile.c:7888-7903 — fill in the character or
             character type for the final opcode. *)
          emit_char_or_type ()
        in

        (* pcre2_compile.c:7260-7906 — now handle repetition for the
           different types of item: switch (op_previous). If the repeat
           minimum and the repeat maximum are both 1, we can ignore the
           quantifier for non-parenthesized items, as they have only one
           alternative. For anything in parentheses, we must not ignore if
           {1} is possessive. *)
        (try
           if
             Int.equal op_previous Opcodes.op_char
             || Int.equal op_previous Opcodes.op_chari
             || Int.equal op_previous Opcodes.op_not
             || Int.equal op_previous Opcodes.op_noti
           then (
             (* pcre2_compile.c:7267-7278 — if previous was a character or
                negated character match, abolish the item and generate a
                repeat item instead. If a char item has a minimum of more
                than one, ensure that it is set in reqcu - it might not be
                if a sequence such as x{3} is the first thing in a branch
                because the x will have gone into firstcu instead. *)
             if Int.equal !repeat_max 1 && Int.equal !repeat_min 1 then
               raise_notrace End_repeat;
             op_type := chartypeoffset.(op_previous - Opcodes.op_char);

             (* pcre2_compile.c:7280-7289 — deal with UTF characters that
                take up more than one code unit (MAYBE_UTF_MULTI;
                NOT_FIRSTCU(c) = (c & 0xc0) == 0x80,
                pcre2_intmodedep.h:296). DEVIATION: multi-code-unit
                literal emission is M6 (docs/ocaml-engine/07-utf.md); no
                M1 arm can emit one, so this defers loudly instead of
                saving the character into mcbuffer. *)
             if
               utf
               && Int.equal
                    (Char.code (Bytes.get cb.start_code (!code - 1)) land 0xc0)
                    0x80
             then (
               errorcodeptr := Parse.err_deferred;
               return_from_branch 0);

             (* pcre2_compile.c:7293-7308 — handle the case of a single
                code unit - either with no UTF support, or with UTF
                disabled, or for a single-code-unit UTF character. In the
                latter case, for a repeated positive match, get the
                caseless flag for the required code unit from the previous
                character, because a class like [Aa] sets a caseless A but
                by now the req_caseopt flag has been reset. *)
             let mcbuffer0 = Char.code (Bytes.get cb.start_code (!code - 1)) in
             if op_previous <= Opcodes.op_chari && !repeat_min > 1 then (
               reqcu := mcbuffer0;
               reqcuflags := cb.req_varyopt;
               if Int.equal op_previous Opcodes.op_chari then
                 reqcuflags := !reqcuflags lor req_caseless);

             (* goto OUTPUT_SINGLE_REPEAT — code shared with single
                character types (pcre2_compile.c:7309) *)
             output_single_repeat ~mclength:1 ~mcbuffer0 ~prop_type:(-1)
               ~prop_value:(-1))
           else if
             Int.equal op_previous Opcodes.op_xclass
             || Int.equal op_previous Opcodes.op_class
             || Int.equal op_previous Opcodes.op_nclass
             || Int.equal op_previous Opcodes.op_ref
             || Int.equal op_previous Opcodes.op_refi
             || Int.equal op_previous Opcodes.op_dnref
             || Int.equal op_previous Opcodes.op_dnrefi
           then (
             (* pcre2_compile.c:7311-7344 — if previous was a character
                class or a back reference, we put the repeat stuff after
                it, but just skip the item if the repeat was {0,0}.
                (OP_REF/OP_REFI/OP_DNREF/OP_DNREFI previous items arrive
                with M2, OP_XCLASS with M6; only the class opcodes can be
                previous in M1.) *)
             if Int.equal !repeat_max 0 then (
               code := !previous;
               raise_notrace End_repeat);
             if Int.equal !repeat_max 1 && Int.equal !repeat_min 1 then
               raise_notrace End_repeat;

             (* pcre2_compile.c:7331-7343 *)
             if
               Int.equal !repeat_min 0
               && Int.equal !repeat_max Limits.repeat_unlimited
             then emit_cu (Opcodes.op_crstar + !repeat_type)
             else if
               Int.equal !repeat_min 1
               && Int.equal !repeat_max Limits.repeat_unlimited
             then emit_cu (Opcodes.op_crplus + !repeat_type)
             else if Int.equal !repeat_min 0 && Int.equal !repeat_max 1 then
               emit_cu (Opcodes.op_crquery + !repeat_type)
             else (
               emit_cu (Opcodes.op_crrange + !repeat_type);
               put2inc cb.start_code code !repeat_min;
               if Int.equal !repeat_max Limits.repeat_unlimited then
                 repeat_max := 0 (* 2-byte encoding for max *);
               put2inc cb.start_code code !repeat_max))
           else if Int.equal op_previous Opcodes.op_fail then
             (* pcre2_compile.c:7346-7352 — if previous is OP_FAIL, it was
                generated by an empty class [] (PCRE2_ALLOW_EMPTY_CLASS is
                set). The other ways in which OP_FAIL can be generated,
                that is by ( *FAIL) or (?!), disallow a quantifier at parse
                time. We can just ignore this repeat. *)
             raise_notrace End_repeat
           else if Int.equal op_previous Opcodes.op_recurse then (
             (* pcre2_compile.c:7354-7422 — repeated recursion:
                replication for a non-zero minimum, then wrapping in
                OP_BRA brackets and falling through to the repeated-
                bracket case. M5 (docs/ocaml-engine/
                05-conditionals-recursion.md); unreachable in M1 because
                META_RECURSE itself defers above. Deferred loudly. *)
             errorcodeptr := Parse.err_deferred;
             return_from_branch 0)
           else if
             Int.equal op_previous Opcodes.op_assert
             || Int.equal op_previous Opcodes.op_assert_not
             || Int.equal op_previous Opcodes.op_assert_na
             || Int.equal op_previous Opcodes.op_assertback
             || Int.equal op_previous Opcodes.op_assertback_not
             || Int.equal op_previous Opcodes.op_assertback_na
             || Int.equal op_previous Opcodes.op_once
             || Int.equal op_previous Opcodes.op_script_run
             || Int.equal op_previous Opcodes.op_bra
             || Int.equal op_previous Opcodes.op_cbra
             || Int.equal op_previous Opcodes.op_cond
           then (
             (* pcre2_compile.c:7424-7446 — if previous was a bracket
                group, we may have to replicate it in certain cases. Note
                that at this point we can encounter only the "basic"
                bracket opcodes such as BRA and CBRA, as this is the place
                where they get converted into the more special varieties
                such as BRAPOS and SBRA. Originally, PCRE did not allow
                repetition of assertions, but now it does, for Perl
                compatibility. The C's PCRE2_UCHAR *bralink / *brazeroptr
                NULL pointers become offset -1 (0 is a valid code
                offset). *)
             let len = !code - !previous in
             let bralink = ref (-1) in
             let brazeroptr = ref (-1) in

             if
               Int.equal !repeat_max 1 && Int.equal !repeat_min 1
               && not !possessive_quantifier
             then raise_notrace End_repeat;

             (* pcre2_compile.c:7450-7456 — repeating a DEFINE group (or
                any group where the condition is always FALSE and there is
                only one branch) is pointless, but Perl allows the syntax,
                so we just ignore the repeat. *)
             if
               Int.equal op_previous Opcodes.op_cond
               && Int.equal
                    (Char.code
                       (Bytes.get cb.start_code
                          (!previous + Limits.link_size + 1)))
                    Opcodes.op_false
               && not
                    (Int.equal
                       (Char.code
                          (Bytes.get cb.start_code
                             (!previous + get cb.start_code (!previous + 1))))
                       Opcodes.op_alt)
             then raise_notrace End_repeat;

             (* pcre2_compile.c:7458-7470 — Perl allows all assertions to
                be quantified, and when they contain capturing parentheses
                and/or are optional there are potential uses for this
                feature. ... General repetition is now permitted, but if
                the maximum is unlimited it is set to one more than the
                minimum. *)
             if op_previous < Opcodes.op_once (* Assertion *) then
               if Int.equal !repeat_max Limits.repeat_unlimited then
                 repeat_max := !repeat_min + 1;

             (* pcre2_compile.c:7472-7536 — the case of a zero minimum is
                special because of the need to stick OP_BRAZERO in front of
                it, and because the group appears once in the data, whereas
                in other cases it appears the minimum number of times. For
                this reason, it is simplest to treat this case separately,
                as otherwise the code gets far too messy. There are several
                special subcases when the minimum is zero. *)
             if Int.equal !repeat_min 0 then (
               (* pcre2_compile.c:7481-7510 — if the maximum is also zero,
                  we used to just omit the group from the output
                  altogether. However, that fails when a group or a
                  subgroup within it is referenced as a subroutine from
                  elsewhere in the pattern, so now we stick in OP_SKIPZERO
                  in front of it so that it is skipped on execution. As we
                  don't have a list of which groups are referenced, we
                  cannot do this selectively.

                  If the maximum is 1 or unlimited, we just have to stick
                  in the BRAZERO and do no more at this point. *)
               if
                 !repeat_max <= 1
                 || Int.equal !repeat_max Limits.repeat_unlimited
               then (
                 (* Bytes.blit = the C's memmove (overlap-safe). *)
                 Bytes.blit cb.start_code !previous cb.start_code
                   (!previous + 1) len;
                 incr code;
                 if Int.equal !repeat_max 0 then (
                   Bytes.set cb.start_code !previous
                     (Char.chr Opcodes.op_skipzero);
                   previous := !previous + 1;
                   raise_notrace End_repeat);
                 brazeroptr := !previous (* Save for possessive optimizing *);
                 Bytes.set cb.start_code !previous
                   (Char.chr (Opcodes.op_brazero + !repeat_type));
                 previous := !previous + 1)
               else (
                 (* pcre2_compile.c:7512-7533 — if the maximum is greater
                    than 1 and limited, we have to replicate in a nested
                    fashion, sticking OP_BRAZERO before each set of
                    brackets. The first one has to be handled carefully
                    because it's the original copy, which has to be moved
                    up. The remainder can be handled by code that is common
                    with the non-zero minimum case below. We have to adjust
                    the value or repeat_max, since one less copy is
                    required. *)
                 Bytes.blit cb.start_code !previous cb.start_code
                   (!previous + 2 + Limits.link_size)
                   len;
                 code := !code + 2 + Limits.link_size;
                 Bytes.set cb.start_code !previous
                   (Char.chr (Opcodes.op_brazero + !repeat_type));
                 previous := !previous + 1;
                 Bytes.set cb.start_code !previous (Char.chr Opcodes.op_bra);
                 previous := !previous + 1;

                 (* pcre2_compile.c:7527-7532 — we chain together the
                    bracket link offset fields that have to be filled in
                    later when the ends of the brackets are reached. *)
                 let linkoffset =
                   if Int.equal !bralink (-1) then 0 else !previous - !bralink
                 in
                 bralink := !previous;
                 putinc cb.start_code previous linkoffset);

               if not (Int.equal !repeat_max Limits.repeat_unlimited) then
                 repeat_max := !repeat_max - 1)
             else (
               (* pcre2_compile.c:7538-7583 — if the minimum is greater
                  than zero, replicate the group as many times as
                  necessary, and adjust the maximum to the number of
                  subsequent copies that we need. *)
               (if !repeat_min > 1 then
                  match lengthptr with
                  | Some length ->
                      (* pcre2_compile.c:7546-7561 — in the pre-compile
                         phase, we don't actually do the replication. We
                         just adjust the length as if we had. Do some
                         paranoid checks for potential integer overflow.
                         DEVIATION: PRIV(ckd_smul)'s overflow cannot occur
                         in 63-bit OCaml int arithmetic (repeat_min <=
                         65535 and length_prevgroup was bounded by the
                         OFLOW/MAX_PATTERN_SIZE checks), so only the
                         OFLOW_MAX comparison is ported. *)
                      let delta = (!repeat_min - 1) * !length_prevgroup in
                      if oflow_max - !length < delta then (
                        errorcodeptr := Errors.err20;
                        return_from_branch 0);
                      length := !length + delta
                  | None ->
                      (* pcre2_compile.c:7563-7579 — this is compiling for
                         real. If there is a set first code unit for the
                         group, and we have not yet set a "required code
                         unit", set it. *)
                      if !groupsetfirstcu && !reqcuflags >= req_none then (
                        reqcu := !firstcu;
                        reqcuflags := !firstcuflags);
                      for _i = 1 to !repeat_min - 1 do
                        Bytes.blit cb.start_code !previous cb.start_code !code
                          len;
                        code := !code + len
                      done);
               if not (Int.equal !repeat_max Limits.repeat_unlimited) then
                 repeat_max := !repeat_max - !repeat_min);

             (* pcre2_compile.c:7585-7650 — this code is common to both the
                zero and non-zero minimum cases. If the maximum is limited,
                it replicates the group in a nested fashion, remembering
                the bracket starts on a stack. In the case of a zero
                minimum, the first one was set up above. In all cases the
                repeat_max now specifies the number of additional copies
                needed. *)
             if not (Int.equal !repeat_max Limits.repeat_unlimited) then (
               (match lengthptr with
               | Some length when !repeat_max > 0 ->
                   (* pcre2_compile.c:7594-7612 — in the pre-compile phase,
                      we don't actually do the replication. We just adjust
                      the length as if we had. For each repetition we must
                      add 1 to the length for BRAZERO and for all but the
                      last repetition we must add 2 + 2*LINKSIZE to allow
                      for the nesting that occurs. DEVIATION: ckd_smul as
                      in the repeat_min block above. *)
                   let delta =
                     !repeat_max
                     * (!length_prevgroup + 1 + 2 + (2 * Limits.link_size))
                   in
                   if oflow_max + (2 + (2 * Limits.link_size)) - !length < delta
                   then (
                     errorcodeptr := Errors.err20;
                     return_from_branch 0);
                   let delta =
                     delta - (2 + (2 * Limits.link_size))
                     (* Last one doesn't nest *)
                   in
                   length := !length + delta
               | _ ->
                   (* pcre2_compile.c:7614-7634 — this is compiling for
                      real (or the pre-compile phase with repeat_max = 0,
                      when the C's loop body never runs). *)
                   for i = !repeat_max downto 1 do
                     emit_cu (Opcodes.op_brazero + !repeat_type);

                     (* All but the final copy start a new nesting,
                        maintaining the chain of brackets outstanding. *)
                     if not (Int.equal i 1) then (
                       emit_cu Opcodes.op_bra;
                       let linkoffset =
                         if Int.equal !bralink (-1) then 0 else !code - !bralink
                       in
                       bralink := !code;
                       putinc cb.start_code code linkoffset);

                     Bytes.blit cb.start_code !previous cb.start_code !code len;
                     code := !code + len
                   done);

               (* pcre2_compile.c:7636-7649 — now chain through the pending
                  brackets, and fill in their length fields (which are
                  holding the chain links pro tem). *)
               while not (Int.equal !bralink (-1)) do
                 let linkoffset = !code - !bralink + 1 in
                 let bra = !code - linkoffset in
                 let oldlinkoffset = get cb.start_code (bra + 1) in
                 bralink :=
                   if Int.equal oldlinkoffset 0 then -1
                   else !bralink - oldlinkoffset;
                 emit_cu Opcodes.op_ket;
                 putinc cb.start_code code linkoffset;
                 put cb.start_code (bra + 1) linkoffset
               done)
             else
               (* pcre2_compile.c:7652-7749 — if the maximum is unlimited,
                  set a repeater in the final copy. For SCRIPT_RUN and ONCE
                  brackets, that's all we need to do. However, possessively
                  repeated ONCE brackets can be converted into
                  non-capturing brackets, as the behaviour of (?:xx)++ is
                  the same as (?>xx)++ and this saves having to deal with
                  possessive ONCEs specially.

                  Otherwise, when we are doing the actual compile phase,
                  check to see whether this group is one that could match
                  an empty string. If so, convert the initial operator to
                  the S form (e.g. OP_BRA -> OP_SBRA) so that runtime
                  checking can be done. [This check is also applied to ONCE
                  and SCRIPT_RUN groups at runtime, but in a different
                  way.]

                  Then, if the quantifier was possessive and the bracket is
                  not a conditional, we convert the BRA code to the POS
                  form, and the KET code to KETRPOS. (It turns out to be
                  convenient at runtime to detect this kind of subpattern
                  at both the start and at the end.) The use of special
                  opcodes makes it possible to reduce greatly the stack
                  usage in pcre2_match(). If the group is preceded by
                  OP_BRAZERO, convert this to OP_BRAPOSZERO.

                  Then, if the minimum number of matches is 1 or 0, cancel
                  the possessive flag so that the default action below, of
                  wrapping everything inside atomic brackets, does not
                  happen. When the minimum is greater than 1, there will be
                  earlier copies of the group, and so we still have to wrap
                  the whole thing. *)
               let ketcode = !code - 1 - Limits.link_size in
               let bracode = ketcode - get cb.start_code (ketcode + 1) in

               (* pcre2_compile.c:7683-7685 — convert possessive ONCE
                  brackets to non-capturing. *)
               if
                 Int.equal
                   (Char.code (Bytes.get cb.start_code bracode))
                   Opcodes.op_once
                 && !possessive_quantifier
               then Bytes.set cb.start_code bracode (Char.chr Opcodes.op_bra);

               (* pcre2_compile.c:7687-7691 — for non-possessive ONCE and
                  for SCRIPT_RUN brackets, all we need to do is to set the
                  KET. *)
               if
                 Int.equal
                   (Char.code (Bytes.get cb.start_code bracode))
                   Opcodes.op_once
                 || Int.equal
                      (Char.code (Bytes.get cb.start_code bracode))
                      Opcodes.op_script_run
               then
                 Bytes.set cb.start_code ketcode
                   (Char.chr (Opcodes.op_ketrmax + !repeat_type))
               else (
                 (* pcre2_compile.c:7693-7708 — handle non-SCRIPT_RUN and
                    non-ONCE brackets and possessive ONCEs (which have been
                    converted to non-capturing above). In the compile
                    phase, adjust the opcode if the group can match an
                    empty string. For a conditional group with only one
                    branch, the value of group_return will not show "could
                    be empty", so we must check that separately. *)
                 (match lengthptr with
                 | None ->
                     if !group_return < 0 then
                       Bytes.set cb.start_code bracode
                         (Char.chr
                            (Char.code (Bytes.get cb.start_code bracode)
                            + Opcodes.op_sbra - Opcodes.op_bra));
                     if
                       Int.equal
                         (Char.code (Bytes.get cb.start_code bracode))
                         Opcodes.op_cond
                       && not
                            (Int.equal
                               (Char.code
                                  (Bytes.get cb.start_code
                                     (bracode + get cb.start_code (bracode + 1))))
                               Opcodes.op_alt)
                     then
                       Bytes.set cb.start_code bracode
                         (Char.chr Opcodes.op_scond)
                 | Some _ -> ());

                 (* pcre2_compile.c:7710-7743 — handle possessive
                    quantifiers. *)
                 if !possessive_quantifier then (
                   (* pcre2_compile.c:7714-7728 — for COND brackets, we
                      wrap the whole thing in a possessively repeated
                      non-capturing bracket, because we have not invented
                      POS versions of the COND opcodes. *)
                   if
                     Int.equal
                       (Char.code (Bytes.get cb.start_code bracode))
                       Opcodes.op_cond
                     || Int.equal
                          (Char.code (Bytes.get cb.start_code bracode))
                          Opcodes.op_scond
                   then (
                     let nlen = !code - bracode in
                     (* Bytes.blit = memmove; the cell at bracode is below
                        the destination range, so it still holds the
                        original opcode afterwards, as in the C. *)
                     Bytes.blit cb.start_code bracode cb.start_code
                       (bracode + 1 + Limits.link_size)
                       nlen;
                     code := !code + 1 + Limits.link_size;
                     let nlen = nlen + 1 + Limits.link_size in
                     Bytes.set cb.start_code bracode
                       (Char.chr
                          (if
                             Int.equal
                               (Char.code (Bytes.get cb.start_code bracode))
                               Opcodes.op_cond
                           then Opcodes.op_brapos
                           else Opcodes.op_sbrapos));
                     emit_cu Opcodes.op_ketrpos;
                     putinc cb.start_code code nlen;
                     put cb.start_code (bracode + 1) nlen)
                   else (
                     (* pcre2_compile.c:7730-7736 — for non-COND brackets,
                        we modify the BRA code and use KETRPOS. *)
                     Bytes.set cb.start_code bracode
                       (Char.chr
                          (Char.code (Bytes.get cb.start_code bracode) + 1))
                     (* Switch to xxxPOS opcodes *);
                     Bytes.set cb.start_code ketcode
                       (Char.chr Opcodes.op_ketrpos));

                   (* pcre2_compile.c:7738-7742 — if the minimum is zero,
                      mark it as possessive, then unset the possessive flag
                      when the minimum is 0 or 1. *)
                   if not (Int.equal !brazeroptr (-1)) then
                     Bytes.set cb.start_code !brazeroptr
                       (Char.chr Opcodes.op_braposzero);
                   if !repeat_min < 2 then possessive_quantifier := false)
                 else
                   (* pcre2_compile.c:7745-7747 — non-possessive
                      quantifier. *)
                   Bytes.set cb.start_code ketcode
                     (Char.chr (Opcodes.op_ketrmax + !repeat_type))))
           else if op_previous >= Opcodes.op_eodn then (
             (* pcre2_compile.c:7760-7765 — default case: not a character
                type - internal error. *)
             errorcodeptr := Errors.err10;
             return_from_branch 0)
           else (
             (* pcre2_compile.c:7753-7786 — if previous was a character
                type match (\d or similar), abolish it and create a
                suitable repeat item. The code is shared with
                single-character repeats by setting op_type to add a
                suitable offset into repeat_type. Note the the Unicode
                property types will be present only when SUPPORT_UNICODE
                is defined, but we don't wrap the little bits of code here
                because it just makes it horribly messy. *)
             if Int.equal !repeat_max 1 && Int.equal !repeat_min 1 then
               raise_notrace End_repeat;
             op_type := Opcodes.op_typestar - Opcodes.op_star
             (* Use type opcodes *);
             (* mclength = 0 — not a character *)
             let prop_type, prop_value =
               if
                 Int.equal op_previous Opcodes.op_prop
                 || Int.equal op_previous Opcodes.op_notprop
               then
                 (* pcre2_compile.c:7776-7780 — a repeated Unicode
                    property match carries its two property data units.
                    OP_PROP/OP_NOTPROP previous items arrive with M7
                    (docs/ocaml-engine/08-ucp.md); the plumbing is kept in
                    the C's shape. *)
                 ( Char.code (Bytes.get cb.start_code (!previous + 1)),
                   Char.code (Bytes.get cb.start_code (!previous + 2)) )
               else (-1, -1)
             in
             output_single_repeat ~mclength:0 ~mcbuffer0:0 ~prop_type
               ~prop_value);

           (* pcre2_compile.c:7909-7929 — if the character following a
              repeat is '+', possessive_quantifier is TRUE. For some
              opcodes, there are special alternative opcodes for this
              case. For anything else, we wrap the entire repeated item
              inside OP_ONCE brackets. Note that the repeated item starts
              at tempcode, not at previous, which might be the first part
              of a string whose (former) last char we repeated.

              Possessifying an EXACT quantifier has no effect, so we can
              ignore it. However, QUERY, STAR, or UPTO may follow (for
              quantifiers such as {5,6}, {5,}, or {5,10}). We skip over an
              EXACT item; if the length of what remains is greater than
              zero, there's a further opcode that can be handled. If not,
              do nothing, leaving the EXACT alone. *)
           if !possessive_quantifier then (
             let t = Char.code (Bytes.get cb.start_code !tempcode) in
             if Int.equal t Opcodes.op_typeexact then
               (* pcre2_compile.c:7933-7937 *)
               tempcode :=
                 !tempcode + Opcodes.op_lengths.(t)
                 +
                 let following =
                   Char.code
                     (Bytes.get cb.start_code
                        (!tempcode + 1 + Limits.imm2_size))
                 in
                 if
                   Int.equal following Opcodes.op_prop
                   || Int.equal following Opcodes.op_notprop
                 then 2
                 else 0
             else if
               (* pcre2_compile.c:7939-7949 — CHAR opcodes are used for
                  exacts whose count is 1. *)
               Int.equal t Opcodes.op_char
               || Int.equal t Opcodes.op_chari
               || Int.equal t Opcodes.op_not
               || Int.equal t Opcodes.op_noti
               || Int.equal t Opcodes.op_exact
               || Int.equal t Opcodes.op_exacti
               || Int.equal t Opcodes.op_notexact
               || Int.equal t Opcodes.op_notexacti
             then (
               tempcode := !tempcode + Opcodes.op_lengths.(t);
               (* pcre2_compile.c:7950-7953 — SUPPORT_UNICODE:
                  HAS_EXTRALEN(c) = c >= 0xc0 (pcre2_internal.h:272,
                  pcre2_intmodedep.h:286). DEVIATION: the GET_EXTRALEN
                  skip is M6 (docs/ocaml-engine/07-utf.md); unreachable in
                  M1 (no multi-code-unit literal is ever emitted), so this
                  defers loudly rather than mis-skipping. *)
               if
                 utf
                 && Char.code (Bytes.get cb.start_code (!tempcode - 1)) >= 0xc0
               then (
                 errorcodeptr := Parse.err_deferred;
                 return_from_branch 0))
             else if
               Int.equal t Opcodes.op_class || Int.equal t Opcodes.op_nclass
             then
               (* pcre2_compile.c:7956-7962 — for the class opcodes, the
                  repeat operator appears at the end; adjust tempcode to
                  point to it. 32/sizeof(PCRE2_UCHAR) = 32. *)
               tempcode := !tempcode + 1 + 32
             else if Int.equal t Opcodes.op_xclass then
               (* pcre2_compile.c:7964-7968 — unreachable in M1 (OP_XCLASS
                  emission is M6), kept in the C's shape. *)
               tempcode := !tempcode + get cb.start_code (!tempcode + 1);

             (* pcre2_compile.c:7971-7977 — if tempcode is equal to code
                (which points to the end of the repeated item), it means
                we have skipped an EXACT item but there is no following
                QUERY, STAR, or UPTO; the value of len will be 0, and we
                do nothing. In all other cases, tempcode will be pointing
                to the repeat opcode, and will be less than code, so the
                value of len will be greater than 0. *)
             let len = !code - !tempcode in
             if len > 0 then
               let repcode = Char.code (Bytes.get cb.start_code !tempcode) in

               (* pcre2_compile.c:7982-7987 — there is a table for
                  possessifying opcodes, all of which are less than
                  OP_CALLOUT. A zero entry means there is no possessified
                  version. *)
               if
                 repcode < Opcodes.op_callout && opcode_possessify.(repcode) > 0
               then
                 Bytes.set cb.start_code !tempcode
                   (Char.chr opcode_possessify.(repcode))
               else (
                 (* pcre2_compile.c:7989-8001 — for opcodes without a
                    special possessified version, wrap the item in ONCE
                    brackets. Bytes.blit = the C's memmove
                    (overlap-safe). *)
                 Bytes.blit cb.start_code !tempcode cb.start_code
                   (!tempcode + 1 + Limits.link_size)
                   len;
                 code := !code + 1 + Limits.link_size;
                 let len = len + 1 + Limits.link_size in
                 Bytes.set cb.start_code !tempcode (Char.chr Opcodes.op_once);
                 emit_cu Opcodes.op_ket;
                 putinc cb.start_code code len;
                 put cb.start_code (!tempcode + 1) len))
         with End_repeat -> ());

        (* pcre2_compile.c:8005-8010 — END_REPEAT: we set the "follows
           varying string" flag for subsequently encountered reqcus if it
           isn't already set and we have just passed a varying length
           item. *)
        cb.req_varyopt <- cb.req_varyopt lor reqvary)
      else if Int.equal meta Parse.meta_bigvalue then (
        (* pcre2_compile.c:8014-8019 — handle a 32-bit data character with
           a value greater than META_END. *)
        pptr := !pptr + 1;
        (* goto NORMAL_CHAR *)
        normal_char ())
      else if Int.equal meta Parse.meta_backref then (
        (* pcre2_compile.c:8022-8038 — handle a back reference by number,
           which is the meta argument. The pattern offsets for back
           references to group numbers less than 10 are held in a special
           vector, to avoid using more than two parsed pattern elements in
           64-bit environments. We only need the offset to the first
           occurrence, because if that doesn't fail, subsequent ones will
           also be OK. *)
        let offset =
          if meta_arg < 10 then cb.small_ref_offset.(meta_arg)
          else (
            (* GETPLUSOFFSET(offset, pptr) *)
            pptr := !pptr + 1;
            cb.parsed_pattern.(!pptr))
        in
        if meta_arg > cb.bracount then (
          cb.erroroffset <- offset;
          errorcodeptr := Errors.err15 (* Non-existent subpattern *);
          return_from_branch 0);
        (* fallthrough to HANDLE_SINGLE_REFERENCE in C *)
        handle_single_reference meta_arg)
      else if Int.equal meta Parse.meta_recurse then (
        (* pcre2_compile.c:8061-8087 — recursion:
           docs/ocaml-engine/05-conditionals-recursion.md. Deferred
           loudly. *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
      else if Int.equal meta Parse.meta_capture then (
        (* pcre2_compile.c:8090-8098 — handle capturing parentheses; the
           number is the meta argument. *)
        put2 cb.start_code (!code + 1 + Limits.link_size) meta_arg;
        cb.lastcapture <- meta_arg;
        (* goto GROUP_PROCESS_NOTE_EMPTY, with bravalue = OP_CBRA and
           skipunits = IMM2_SIZE (pcre2_compile.c:8094-8095,8098) *)
        group_process ~note_group_empty:true ~bravalue:Opcodes.op_cbra
          ~skipunits:Limits.imm2_size)
      else if Int.equal meta Parse.meta_escape then (
        (* pcre2_compile.c:8101-8206 — handle escape sequence items. For
           ones like \d, the ESC_values are arranged to be the same as the
           corresponding OP_values in the default case when PCRE2_UCP is
           not set (which is the only case in which they will appear here).

           Note: \Q and \E are never seen here, as they were dealt with in
           parse_pattern(). Neither are numerical back references or
           recursions, which were turned into META_BACKREF or META_RECURSE
           items, respectively. \k and \g, when followed by names, are
           turned into META_BACKREF_BYNAME or META_RECURSE_BYNAME. *)

        (* pcre2_compile.c:8115-8124 — we can test for escape sequences
           that consume a character because their values lie between ESC_b
           and ESC_Z. For these sequences, we disable the setting of a
           first character if it hasn't already been set. *)
        if meta_arg > Parse.esc_b && meta_arg < Parse.esc_big_z then (
          matched_char := true;
          if Int.equal !firstcuflags req_unset then firstcuflags := req_none);

        (* pcre2_compile.c:8126-8131 — set values to reset to if this is
           followed by a zero repeat. *)
        zerofirstcu := !firstcu;
        zerofirstcuflags := !firstcuflags;
        zeroreqcu := !reqcu;
        zeroreqcuflags := !reqcuflags;

        if Int.equal meta_arg Parse.esc_big_p || Int.equal meta_arg Parse.esc_p
        then (
          (* pcre2_compile.c:8136-8157 — \P and \p (OP_PROP/OP_NOTPROP and
             the \p{Any} OP_ALLANY special case) plus their extra data
             word: docs/ocaml-engine/08-ucp.md. Deferred loudly. *)
          errorcodeptr := Parse.err_deferred;
          return_from_branch 0)
        else (
          (* pcre2_compile.c:8159-8167 — \K is forbidden in lookarounds
             since 10.38 because that's what Perl has done. However,
             there's an option, in case anyone was relying on it. *)
          if
            cb.assert_depth > 0
            && Int.equal meta_arg Parse.esc_big_k
            && Int.equal (!xoptions land Options.extra_allow_lookaround_bsk) 0
          then (
            errorcodeptr := Errors.err99;
            return_from_branch 0);

          (* pcre2_compile.c:8169-8203 — for the rest (including \X when
             Unicode is supported - if not it's faulted at parse time), the
             OP value is the escape value when PCRE2_UCP is not set; if it
             is set, most of them do not show up here because they are
             converted into Unicode property tests in parse_regex().

             In non-UTF mode, and for both 32-bit modes, we turn \C into
             OP_ALLANY instead of OP_ANYBYTE so that it works in DFA mode
             and in lookbehinds. There are special UCP codes for \B and \b
             which are used in UCP mode unless "word" matching is being
             forced to ASCII.

             Note that \b and \B do a one-character lookbehind, and \A also
             behaves as if it does. *)
          let meta_arg =
            if Int.equal meta_arg Parse.esc_big_c then (
              (* pcre2_compile.c:8184-8191 *)
              cb.external_flags <- cb.external_flags lor hasbkc (* Record *);
              if not utf then Opcodes.op_allany else meta_arg)
            else if
              Int.equal meta_arg Parse.esc_big_b
              || Int.equal meta_arg Parse.esc_b
            then (
              (* pcre2_compile.c:8193-8198 *)
              let meta_arg =
                if
                  (not (Int.equal (!options land Options.ucp) 0))
                  && Int.equal (!xoptions land Options.extra_ascii_bsw) 0
                then
                  if Int.equal meta_arg Parse.esc_big_b then
                    Opcodes.op_not_ucp_word_boundary
                  else Opcodes.op_ucp_word_boundary
                else meta_arg
              in
              (* fallthrough from ESC_B/ESC_b to ESC_A in C
                 (pcre2_compile.c:8198-8201) *)
              if Int.equal cb.max_lookbehind 0 then cb.max_lookbehind <- 1;
              meta_arg)
            else if Int.equal meta_arg Parse.esc_big_a then (
              (* pcre2_compile.c:8200-8202 *)
              if Int.equal cb.max_lookbehind 0 then cb.max_lookbehind <- 1;
              meta_arg)
            else meta_arg
          in
          (* pcre2_compile.c:8205 — *code++ = meta_arg *)
          emit_cu meta_arg))
      else if meta >= Parse.meta_end then (
        (* pcre2_compile.c:8209-8221 — handle an unrecognized meta value. A
           parsed pattern value less than META_END is a literal. Otherwise
           we have a problem. *)
        errorcodeptr := Errors.err89 (* Internal error - unrecognized *);
        return_from_branch 0)
      else
        (* default, literal character: NORMAL_CHAR
           (pcre2_compile.c:8223-8336) *)
        normal_char ();

      (* End of big switch — for (;; pptr++) advances here. *)
      pptr := !pptr + 1
    done;
    (* Control never reaches here (pcre2_compile.c:8340). *)
    assert false
  with Return rc -> rc

(* pcre2_compile.c:8345-8646 — compile_regex: compile a sequence of
   alternatives. On entry, pptr is pointing past the bracket meta, but on
   return it points to the closing bracket or META_END. The code variable
   is pointing at the code unit into which the BRA operator has been
   stored. This function is used during the pre-compile phase when we are
   trying to find out the amount of memory needed, as well as during the
   real compile phase. The value of lengthptr distinguishes the two
   phases.

   Arguments (same out-parameter mapping as compile_branch; options and
   xoptions are BY VALUE in the C — updates compile_branch makes to them
   propagate across this group's branches but not to the caller):
     options           option bits, including any changes for this subpattern
     xoptions          extra option bits, ditto
     codeptr           -> the address of the current code pointer
     pptrptr           -> the address of the current parsed pattern pointer
     errorcodeptr      -> pointer to error code variable
     skipunits         skip this many code units at start (for brackets and
                       OP_COND)
     firstcuptr        place to put the first required code unit
     firstcuflagsptr   place to put the first code unit flags
     reqcuptr          place to put the last required code unit
     reqcuflagsptr     place to put the last required code unit flags
     bcptr             pointer to the chain of currently open branches
     open_caps         pointer to the chain of currently open captures
     cb                points to the data block with tables pointers etc.
     lengthptr         None during the real compile phase (C: NULL)
                       Some length-accumulator ref during pre-compile phase

   Returns:            0 There has been an error
                      +1 Success, this group must match at least one char
                      -1 Success, this group may match an empty string *)
and compile_regex (options : int) (xoptions : int) (codeptr : int ref)
    (pptrptr : int ref) (errorcodeptr : int ref) ~(skipunits : int)
    (firstcuptr : int ref) (firstcuflagsptr : int ref) (reqcuptr : int ref)
    (reqcuflagsptr : int ref) (bcptr : branch_chain option)
    (open_caps : open_capitem option) (cb : compile_block)
    (lengthptr : int ref option) : int =
  (* pcre2_compile.c:8384-8399 — locals. *)
  let code = ref !codeptr in
  let last_branch = ref !code in
  let start_bracket = !code in
  let options = ref options in
  let xoptions = ref xoptions in
  let okreturn = ref 1 in
  let pptr = ref !pptrptr in
  let firstcu = ref 0 and reqcu = ref 0 in
  let firstcuflags = ref req_unset and reqcuflags = ref req_unset in
  let branchfirstcu = ref 0 and branchreqcu = ref 0 in
  let branchfirstcuflags = ref 0 and branchreqcuflags = ref 0 in

  (* pcre2_compile.c:8401-8408 — "if set, call the external function that
     checks for stack availability" (ERR33). DEVIATION: the compile
     context's stack_guard callback was dropped with the allocator fields
     (see the compile_context record note); the guard is never set, so the
     check reduces to nothing. *)

  (* pcre2_compile.c:8410-8413 — miscellaneous initialization. *)
  let bc = { outer = bcptr; current_branch = !code } in

  (* pcre2_compile.c:8418-8425 — accumulate the length for use in the
     pre-compile phase. Start with the length of the BRA and KET and any
     extra code units that are required at the beginning. We accumulate in
     a local variable to save frequent testing of lengthptr for NULL. We
     cannot do this by looking at the value of 'code' at the start and end
     of each alternative, because compiled items are discarded during the
     pre-compile phase so that the workspace is not exceeded. *)
  let length = ref (2 + (2 * Limits.link_size) + skipunits) in

  (* pcre2_compile.c:8427-8440 — remember if this is a lookbehind
     assertion, and if it is, save its length (stored by
     set_lookbehind_lengths in the data bits of the group META word,
     pptr[-1]) and its minimum length (stored in the word that follows,
     replacing the pattern offset), then skip over that word. *)
  let op0 = Char.code (Bytes.get cb.start_code !code) in
  let lookbehind =
    Int.equal op0 Opcodes.op_assertback
    || Int.equal op0 Opcodes.op_assertback_not
    || Int.equal op0 Opcodes.op_assertback_na
  in
  let lookbehindlength = ref 0 in
  let lookbehindminlength = ref 0 in
  if lookbehind then (
    lookbehindlength := Parse.meta_data cb.parsed_pattern.(!pptr - 1);
    lookbehindminlength := cb.parsed_pattern.(!pptr);
    pptr := !pptr + Parse.sizeoffset);

  (* pcre2_compile.c:8442-8454 — if this is a capturing subpattern, add
     to the chain of open capturing items so that we can detect them if
     ( *ACCEPT) is encountered. Note that only OP_CBRA need be tested
     here; changing this opcode to one of its variants, e.g.
     OP_SCBRAPOS, happens later, after the group has been compiled. *)
  let open_caps =
    if Int.equal op0 Opcodes.op_cbra then
      Some
        {
          next = open_caps;
          number = get2 cb.start_code (!code + 1 + Limits.link_size);
          assert_depth = cb.assert_depth;
        }
    else open_caps
  in

  (* pcre2_compile.c:8456-8459 — offset is set zero to mark that this
     bracket is still open. *)
  put cb.start_code (!code + 1) 0;
  code := !code + 1 + Limits.link_size + skipunits;

  (* The C's `return okreturn` / `return 0` exits from inside the branch
     loop. Local exception, never escapes this function
     (port-conventions §2; compile phase only). *)
  let exception Return_regex of int in
  (* pcre2_compile.c:8461-8644 — loop for each alternative branch. *)
  try
    while true do
      (* pcre2_compile.c:8467-8491 — insert OP_REVERSE or OP_VREVERSE if
         this is a lookbehind assertion. There is only a single minimum
         length for the whole assertion. When the minimum length is
         LOOKBEHIND_MAX it means that all branches are of fixed length,
         though not necessarily the same length. In this case, the
         original OP_REVERSE can be used. It can also be used if a branch
         in a variable length lookbehind has the same maximum and
         minimum. Otherwise, use OP_VREVERSE, which has both maximum and
         minimum values. *)
      if lookbehind && !lookbehindlength > 0 then
        if
          Int.equal !lookbehindminlength Limits.lookbehind_max
          || Int.equal !lookbehindminlength !lookbehindlength
        then (
          Bytes.set cb.start_code !code (Char.chr Opcodes.op_reverse);
          incr code;
          put2inc cb.start_code code !lookbehindlength;
          length := !length + 1 + Limits.imm2_size)
        else (
          Bytes.set cb.start_code !code (Char.chr Opcodes.op_vreverse);
          incr code;
          put2inc cb.start_code code !lookbehindminlength;
          put2inc cb.start_code code !lookbehindlength;
          length := !length + 1 + (2 * Limits.imm2_size));

      (* pcre2_compile.c:8493-8500 — now compile the branch; in the
         pre-compile phase its length gets added into the length. *)
      let branch_return =
        compile_branch options xoptions code pptr errorcodeptr branchfirstcu
          branchfirstcuflags branchreqcu branchreqcuflags (Some bc) open_caps cb
          (match lengthptr with None -> None | Some _ -> Some length)
      in
      if Int.equal branch_return 0 then raise_notrace (Return_regex 0);

      (* pcre2_compile.c:8502-8504 — if a branch can match an empty
         string, so can the whole group. *)
      if branch_return < 0 then okreturn := -1;

      (* pcre2_compile.c:8506-8566 — in the real compile phase, there is
         some post-processing to be done. *)
      (match lengthptr with
      | None ->
          if
            not
              (Int.equal
                 (Char.code (Bytes.get cb.start_code !last_branch))
                 Opcodes.op_alt)
          then (
            (* pcre2_compile.c:8510-8519 — if this is the first branch,
               the firstcu and reqcu values for the branch become the
               values for the regex. *)
            firstcu := !branchfirstcu;
            firstcuflags := !branchfirstcuflags;
            reqcu := !branchreqcu;
            reqcuflags := !branchreqcuflags)
          else (
            (* pcre2_compile.c:8521-8543 — if this is not the first
               branch, the first char and reqcu have to match the values
               from all the previous branches, except that if the
               previous value for reqcu didn't have REQ_VARY set, it can
               still match, and we set REQ_VARY for the group from this
               branch's value.

               If we previously had a firstcu, but it doesn't match the
               new branch, we have to abandon the firstcu for the regex,
               but if there was previously no reqcu, it takes on the
               value of the old firstcu. *)
            if
              (not (Int.equal !firstcuflags !branchfirstcuflags))
              || not (Int.equal !firstcu !branchfirstcu)
            then (
              if !firstcuflags < req_none && !reqcuflags >= req_none then (
                reqcu := !firstcu;
                reqcuflags := !firstcuflags);
              firstcuflags := req_none);

            (* pcre2_compile.c:8545-8553 — if we (now or from before)
               have no firstcu, a firstcu from the branch becomes a reqcu
               if there isn't a branch reqcu. *)
            if
              !firstcuflags >= req_none
              && !branchfirstcuflags < req_none
              && !branchreqcuflags >= req_none
            then (
              branchreqcu := !branchfirstcu;
              branchreqcuflags := !branchfirstcuflags);

            (* pcre2_compile.c:8555-8564 — now ensure that the reqcus
               match. *)
            if
              (not
                 (Int.equal
                    (!reqcuflags land lnot req_vary)
                    (!branchreqcuflags land lnot req_vary)))
              || not (Int.equal !reqcu !branchreqcu)
            then reqcuflags := req_none
            else (
              reqcu := !branchreqcu;
              reqcuflags := !reqcuflags lor !branchreqcuflags
              (* To "or" REQ_VARY if present *)))
      | Some _ -> ());

      (* pcre2_compile.c:8568-8615 — handle reaching the end of the
         expression, either ')' or end of pattern. In the real compile
         phase, go back through the alternative branches and reverse the
         chain of offsets, with the field in the BRA item now becoming an
         offset to the first alternative. If there are no alternatives,
         it points to the end of the group. The length in the terminating
         ket is always the length of the whole bracketed item. Return
         leaving the pointer at the terminating char. *)
      if
        not
          (Int.equal (Parse.meta_code cb.parsed_pattern.(!pptr)) Parse.meta_alt)
      then (
        (match lengthptr with
        | None ->
            (* pcre2_compile.c:8578-8589 — do { ... } while
               (branch_length > 0). *)
            let branch_length = ref (!code - !last_branch) in
            let continue_ = ref true in
            while !continue_ do
              let prev_length = get cb.start_code (!last_branch + 1) in
              put cb.start_code (!last_branch + 1) !branch_length;
              branch_length := prev_length;
              last_branch := !last_branch - !branch_length;
              if not (!branch_length > 0) then continue_ := false
            done
        | Some _ -> ());

        (* pcre2_compile.c:8591-8595 — fill in the ket. *)
        Bytes.set cb.start_code !code (Char.chr Opcodes.op_ket);
        put cb.start_code (!code + 1) (!code - start_bracket);
        code := !code + 1 + Limits.link_size;

        (* pcre2_compile.c:8597-8614 — set values to pass back. *)
        codeptr := !code;
        pptrptr := !pptr;
        firstcuptr := !firstcu;
        firstcuflagsptr := !firstcuflags;
        reqcuptr := !reqcu;
        reqcuflagsptr := !reqcuflags;
        (match lengthptr with
        | Some lp ->
            if oflow_max - !lp < !length then (
              errorcodeptr := Errors.err20;
              raise_notrace (Return_regex 0));
            lp := !lp + !length
        | None -> ());
        raise_notrace (Return_regex !okreturn));

      (* pcre2_compile.c:8617-8637 — another branch follows. In the
         pre-compile phase, we can move the code pointer back to where it
         was for the start of the first branch. (That is, pretend that
         each branch is the only one.)

         In the real compile phase, insert an ALT node. Its length field
         points back to the previous branch while the bracket remains
         open. At the end the chain is reversed. It's done like this so
         that the start of the bracket has a zero offset until it is
         closed, making it possible to detect recursion. *)
      (match lengthptr with
      | Some _ ->
          code := !codeptr + 1 + Limits.link_size + skipunits;
          length := !length + 1 + Limits.link_size
      | None ->
          Bytes.set cb.start_code !code (Char.chr Opcodes.op_alt);
          put cb.start_code (!code + 1) (!code - !last_branch);
          last_branch := !code;
          bc.current_branch <- !code;
          code := !code + 1 + Limits.link_size);

      (* pcre2_compile.c:8639-8643 — set the maximum lookbehind length
         for the next branch (if not in a lookbehind the value will be
         zero: set_lookbehind_lengths ORs each branch length into the
         preceding META_ALT word) and then advance past the vertical
         bar. *)
      lookbehindlength := Parse.meta_data cb.parsed_pattern.(!pptr);
      pptr := !pptr + 1
    done;
    (* Control never reaches here (pcre2_compile.c:8645). *)
    assert false
  with Return_regex rc -> rc

(* ---------- Check for anchored pattern ---------- *)

(* pcre2_compile.c:8650-8768 — is_anchored. Try to find out if this is an
   anchored regular expression. Consider each alternative branch. If they
   all start with OP_SOD or OP_CIRC, or with a bracket all of whose
   alternatives start with OP_SOD or OP_CIRC (recurse ad lib), then it's
   anchored. However, if this is a multiline pattern, then only OP_SOD will
   be found, because ^ generates OP_CIRCM in that mode. We can also
   consider a regex to be anchored if OP_SOM starts all its branches (\G).
   A branch is also implicitly anchored if it starts with .* and DOTALL is
   set, unless the .* is inside capturing parentheses that are (or may be)
   back-referenced, inside an atomic group, or inside an assertion, or the
   pattern contains *PRUNE or *SKIP.

   Arguments:
     code         the compiled-code buffer
     pos          offset of the start of the group (C: the code pointer)
     bracket_map  a bitmap of which brackets we are inside while testing
     cb           the compile data block
     atomcount    atomic group level
     inassert     true if in an assertion

   Returns: true or false. Compile-time recursion, bounded like the C by
   the parenthesis nesting limit (port-conventions §6). *)
let rec is_anchored (code : Bytes.t) (pos : int) (bracket_map : int)
    (cb : compile_block) (atomcount : int) ~(inassert : bool) : bool =
  (* do { ... code += GET(code, 1); } while ( *code == OP_ALT) *)
  let rec branch pos =
    (* pcre2_compile.c:8696-8699 *)
    let scode =
      first_significant_code code
        (pos + Opcodes.op_lengths.(Char.code (Bytes.get code pos)))
        ~skipassert:false
    in
    let op = Char.code (Bytes.get code scode) in
    let ok =
      if
        (* pcre2_compile.c:8701-8708 — non-capturing brackets *)
        Int.equal op Opcodes.op_bra
        || Int.equal op Opcodes.op_brapos
        || Int.equal op Opcodes.op_sbra
        || Int.equal op Opcodes.op_sbrapos
      then is_anchored code scode bracket_map cb atomcount ~inassert
      else if
        (* pcre2_compile.c:8710-8718 — capturing brackets *)
        Int.equal op Opcodes.op_cbra
        || Int.equal op Opcodes.op_cbrapos
        || Int.equal op Opcodes.op_scbra
        || Int.equal op Opcodes.op_scbrapos
      then
        let n = get2 code (scode + 1 + Limits.link_size) in
        let new_map = bracket_map lor if n < 32 then 1 lsl n else 1 in
        is_anchored code scode new_map cb atomcount ~inassert
      else if
        (* pcre2_compile.c:8720-8725 — positive forward assertion *)
        Int.equal op Opcodes.op_assert || Int.equal op Opcodes.op_assert_na
      then is_anchored code scode bracket_map cb atomcount ~inassert:true
      else if
        (* pcre2_compile.c:8727-8734 — condition; if there is no second
           branch, it can't be anchored. *)
        Int.equal op Opcodes.op_cond || Int.equal op Opcodes.op_scond
      then
        Int.equal
          (Char.code (Bytes.get code (scode + get code (scode + 1))))
          Opcodes.op_alt
        && is_anchored code scode bracket_map cb atomcount ~inassert
      else if
        (* pcre2_compile.c:8736-8742 — atomic groups *)
        Int.equal op Opcodes.op_once
      then is_anchored code scode bracket_map cb (atomcount + 1) ~inassert
      else if
        (* pcre2_compile.c:8744-8758 — .* is not anchored unless DOTALL is
           set (which generates OP_ALLANY) and it isn't in brackets that
           are or may be referenced or inside an atomic group or an
           assertion. Also the pattern must not contain *PRUNE or *SKIP. *)
        Int.equal op Opcodes.op_typestar
        || Int.equal op Opcodes.op_typeminstar
        || Int.equal op Opcodes.op_typeposstar
      then
        not
          ((not
              (Int.equal
                 (Char.code (Bytes.get code (scode + 1)))
                 Opcodes.op_allany))
          || (not (Int.equal (bracket_map land cb.backref_map) 0))
          || atomcount > 0 || cb.had_pruneorskip || inassert
          || not
               (Int.equal
                  (cb.external_options land Options.no_dotstar_anchor)
                  0))
      else
        (* pcre2_compile.c:8760-8762 — check for explicit anchoring *)
        Int.equal op Opcodes.op_sod
        || Int.equal op Opcodes.op_som
        || Int.equal op Opcodes.op_circ
    in
    if not ok then false
    else
      (* pcre2_compile.c:8764-8766 *)
      let pos = pos + get code (pos + 1) in
      if Int.equal (Char.code (Bytes.get code pos)) Opcodes.op_alt then
        (branch [@tailcall]) pos
      else true
  in
  branch pos

(* ---------- Check for starting with ^ or .* ---------- *)

(* pcre2_compile.c:8772-8901 — is_startline. This is called to find out if
   every branch starts with ^ or .* so that "first char" processing can be
   done to speed things up in multiline matching and for non-DOTALL
   patterns that start with .* (which must start at the beginning or after
   \n). As in the case of is_anchored(), we must take account of back
   references to capturing brackets that contain .*, of .* inside atomic
   brackets or an assertion, and of *PRUNE / *SKIP.

   Arguments: as for is_anchored. Returns: true or false. *)
let rec is_startline (code : Bytes.t) (pos : int) (bracket_map : int)
    (cb : compile_block) (atomcount : int) ~(inassert : bool) : bool =
  (* The C's mid-branch `return FALSE` sites inside the OP_COND prelude.
     Local exception, never escapes this function (port-conventions §2;
     compile phase only). *)
  let exception Return_false in
  try
    (* do { ... code += GET(code, 1); } while ( *code == OP_ALT) *)
    let rec branch pos =
      (* pcre2_compile.c:8801-8804 *)
      let scode =
        ref
          (first_significant_code code
             (pos + Opcodes.op_lengths.(Char.code (Bytes.get code pos)))
             ~skipassert:false)
      in
      let op = ref (Char.code (Bytes.get code !scode)) in

      (* pcre2_compile.c:8806-8837 — if we are at the start of a
         conditional assertion group, *both* the conditional assertion
         *and* what follows the condition must satisfy the test for start
         of line. Other kinds of condition fail. Note that there may be an
         auto-callout at the start of a condition. *)
      if Int.equal !op Opcodes.op_cond then (
        scode := !scode + 1 + Limits.link_size;
        if Int.equal (Char.code (Bytes.get code !scode)) Opcodes.op_callout then
          scode := !scode + Opcodes.op_lengths.(Opcodes.op_callout)
        else if
          Int.equal (Char.code (Bytes.get code !scode)) Opcodes.op_callout_str
        then scode := !scode + get code (!scode + 1 + (2 * Limits.link_size));
        let c = Char.code (Bytes.get code !scode) in
        if
          Int.equal c Opcodes.op_cref
          || Int.equal c Opcodes.op_dncref
          || Int.equal c Opcodes.op_rref
          || Int.equal c Opcodes.op_dnrref
          || Int.equal c Opcodes.op_fail
          || Int.equal c Opcodes.op_false
          || Int.equal c Opcodes.op_true
        then raise_notrace Return_false
        else (
          (* default: assertion *)
          if
            not
              (is_startline code !scode bracket_map cb atomcount ~inassert:true)
          then raise_notrace Return_false;
          (* do scode += GET(scode, 1); while ( *scode == OP_ALT); *)
          let rec skip_alts () =
            scode := !scode + get code (!scode + 1);
            if Int.equal (Char.code (Bytes.get code !scode)) Opcodes.op_alt then
              (skip_alts [@tailcall]) ()
          in
          skip_alts ();
          scode := !scode + 1 + Limits.link_size);
        scode := first_significant_code code !scode ~skipassert:false;
        op := Char.code (Bytes.get code !scode));

      let scode = !scode in
      let op = !op in
      let ok =
        if
          (* pcre2_compile.c:8839-8846 — non-capturing brackets *)
          Int.equal op Opcodes.op_bra
          || Int.equal op Opcodes.op_brapos
          || Int.equal op Opcodes.op_sbra
          || Int.equal op Opcodes.op_sbrapos
        then is_startline code scode bracket_map cb atomcount ~inassert
        else if
          (* pcre2_compile.c:8848-8856 — capturing brackets *)
          Int.equal op Opcodes.op_cbra
          || Int.equal op Opcodes.op_cbrapos
          || Int.equal op Opcodes.op_scbra
          || Int.equal op Opcodes.op_scbrapos
        then
          let n = get2 code (scode + 1 + Limits.link_size) in
          let new_map = bracket_map lor if n < 32 then 1 lsl n else 1 in
          is_startline code scode new_map cb atomcount ~inassert
        else if
          (* pcre2_compile.c:8858-8864 — positive forward assertions *)
          Int.equal op Opcodes.op_assert || Int.equal op Opcodes.op_assert_na
        then is_startline code scode bracket_map cb atomcount ~inassert:true
        else if
          (* pcre2_compile.c:8866-8872 — atomic brackets *)
          Int.equal op Opcodes.op_once
        then is_startline code scode bracket_map cb (atomcount + 1) ~inassert
        else if
          (* pcre2_compile.c:8874-8887 — .* means "start at start or after
             \n" if it isn't in atomic brackets or brackets that may be
             referenced or an assertion, and as long as the pattern does
             not contain *PRUNE or *SKIP. *)
          Int.equal op Opcodes.op_typestar
          || Int.equal op Opcodes.op_typeminstar
          || Int.equal op Opcodes.op_typeposstar
        then
          not
            ((not
                (Int.equal
                   (Char.code (Bytes.get code (scode + 1)))
                   Opcodes.op_any))
            || (not (Int.equal (bracket_map land cb.backref_map) 0))
            || atomcount > 0 || cb.had_pruneorskip || inassert
            || not
                 (Int.equal
                    (cb.external_options land Options.no_dotstar_anchor)
                    0))
        else
          (* pcre2_compile.c:8889-8893 — check for explicit circumflex;
             anything else gives a FALSE result. *)
          Int.equal op Opcodes.op_circ || Int.equal op Opcodes.op_circm
      in
      if not ok then false
      else
        (* pcre2_compile.c:8895-8899 — move on to the next alternative *)
        let pos = pos + get code (pos + 1) in
        if Int.equal (Char.code (Bytes.get code pos)) Opcodes.op_alt then
          (branch [@tailcall]) pos
        else true
    in
    branch pos
  with Return_false -> false

(* pcre2_compile.c:8905-9049 — find_recurse: scans a compiled pattern for
   OP_RECURSE so the driver can convert recursion group numbers into
   offsets. DEFERRED (M5, docs/ocaml-engine/05-conditionals-recursion.md):
   nothing can set cb.had_recurse until the recursion parse/compile arms
   land, so the driver's fixup loop (pcre2_compile.c:10727-10784, with
   PRIV(find_bracket)) defers loudly there instead. *)

(* ---------- Check for asserted fixed first code unit ---------- *)

(* pcre2_compile.c:9053-9158 — find_firstassertedcu. During compilation,
   the "first code unit" settings from forward assertions are discarded,
   because they can cause conflicts with actual literals that follow.
   However, if we end up without a first code unit setting for an
   unanchored pattern, it is worth scanning the regex to see if there is
   an initial asserted first code unit. If all branches start with the
   same asserted code unit, or with a non-conditional bracket all of whose
   alternatives start with the same asserted code unit (recurse ad lib),
   then we return that code unit, with the flags set to zero or
   REQ_CASELESS; otherwise return zero with REQ_NONE in the flags.

   Arguments:
     code      the compiled-code buffer
     pos       offset of the start of the group (C: the code pointer)
     flagsptr  where to put the first code unit flags
     inassert  non-zero if in an assertion

   Returns: the fixed first code unit, or 0 with REQ_NONE in flags. *)
let rec find_firstassertedcu (code : Bytes.t) (pos : int) (flagsptr : int ref)
    (inassert : int) : int =
  (* pcre2_compile.c:9078-9081 *)
  let c = ref 0 in
  let cflags = ref req_none in
  flagsptr := req_none;
  (* The C's mid-loop `return 0` sites (flagsptr keeps the REQ_NONE set
     above). Local exception, never escapes this function
     (port-conventions §2; compile phase only). *)
  let exception Return0 in
  try
    (* do { ... code += GET(code, 1); } while ( *code == OP_ALT) *)
    let rec alt_loop pos =
      (* pcre2_compile.c:9085-9088 *)
      let op0 = Char.code (Bytes.get code pos) in
      let xl =
        if
          Int.equal op0 Opcodes.op_cbra
          || Int.equal op0 Opcodes.op_scbra
          || Int.equal op0 Opcodes.op_cbrapos
          || Int.equal op0 Opcodes.op_scbrapos
        then Limits.imm2_size
        else 0
      in
      let scode =
        ref
          (first_significant_code code
             (pos + 1 + Limits.link_size + xl)
             ~skipassert:true)
      in
      let op = Char.code (Bytes.get code !scode) in
      (* switch(op) *)
      if
        (* pcre2_compile.c:9095-9110 — bracket/assertion groups *)
        Int.equal op Opcodes.op_bra
        || Int.equal op Opcodes.op_brapos
        || Int.equal op Opcodes.op_cbra
        || Int.equal op Opcodes.op_scbra
        || Int.equal op Opcodes.op_cbrapos
        || Int.equal op Opcodes.op_scbrapos
        || Int.equal op Opcodes.op_assert
        || Int.equal op Opcodes.op_assert_na
        || Int.equal op Opcodes.op_once
        || Int.equal op Opcodes.op_script_run
      then (
        let dflags = ref 0 in
        let d =
          find_firstassertedcu code !scode dflags
            (inassert
            +
            if
              Int.equal op Opcodes.op_assert
              || Int.equal op Opcodes.op_assert_na
            then 1
            else 0)
        in
        if !dflags >= req_none then raise_notrace Return0;
        if !cflags >= req_none then (
          c := d;
          cflags := !dflags)
        else if (not (Int.equal !c d)) || not (Int.equal !cflags !dflags) then
          raise_notrace Return0)
      else if
        (* pcre2_compile.c:9112-9123 — OP_EXACT falls through to the
           caseful literal group *)
        Int.equal op Opcodes.op_exact
        || Int.equal op Opcodes.op_char
        || Int.equal op Opcodes.op_plus
        || Int.equal op Opcodes.op_minplus
        || Int.equal op Opcodes.op_posplus
      then (
        if Int.equal op Opcodes.op_exact then scode := !scode + Limits.imm2_size
          (* fallthrough from OP_EXACT in C *);
        if Int.equal inassert 0 then raise_notrace Return0;
        let ch = Char.code (Bytes.get code (!scode + 1)) in
        if !cflags >= req_none then (
          c := ch;
          cflags := 0)
        else if not (Int.equal !c ch) then raise_notrace Return0)
      else if
        (* pcre2_compile.c:9125-9149 — OP_EXACTI falls through to the
           caseless literal group *)
        Int.equal op Opcodes.op_exacti
        || Int.equal op Opcodes.op_chari
        || Int.equal op Opcodes.op_plusi
        || Int.equal op Opcodes.op_minplusi
        || Int.equal op Opcodes.op_posplusi
      then (
        if Int.equal op Opcodes.op_exacti then
          scode := !scode + Limits.imm2_size
          (* fallthrough from OP_EXACTI in C *);
        if Int.equal inassert 0 then raise_notrace Return0;
        let ch = Char.code (Bytes.get code (!scode + 1)) in
        (* pcre2_compile.c:9135-9145 — if the character is more than one
           code unit long, we cannot set its first code unit when matching
           caselessly (SUPPORT_UNICODE, 8-bit width arm). *)
        if ch >= 0x80 then raise_notrace Return0;
        if !cflags >= req_none then (
          c := ch;
          cflags := req_caseless)
        else if not (Int.equal !c ch) then raise_notrace Return0)
      else (* pcre2_compile.c:9092-9093 — default *)
        raise_notrace Return0;
      (* pcre2_compile.c:9152-9154 *)
      let pos = pos + get code (pos + 1) in
      if Int.equal (Char.code (Bytes.get code pos)) Opcodes.op_alt then
        (alt_loop [@tailcall]) pos
    in
    alt_loop pos;
    (* pcre2_compile.c:9156-9157 *)
    flagsptr := !cflags;
    !c
  with Return0 -> 0

(* ---------- Add an entry to the name/number table ---------- *)

(* pcre2_compile.c:9162-9219 — add_name_to_table. This function is called
   between compiling passes to add an entry to the name/number table,
   maintaining alphabetical order. Checking for permitted and forbidden
   duplicates has already been done.

   Arguments:
     cb          the compile data block
     name        pattern offset of the name to add (C: PCRE2_SPTR name)
     length      the length of the name
     groupno     the group number
     tablecount  the count of names in the table so far *)
let add_name_to_table (cb : compile_block) ~(name : int) ~(length : int)
    ~(groupno : int) ~(tablecount : int) : unit =
  let table = cb.name_table in
  (* memcmp(name, slot+IMM2_SIZE, CU2BYTES(length)): unsigned byte compare
     of the new name (in the pattern) against the slot's name. *)
  let memcmp_name slot =
    let rec cmp j =
      if j >= length then 0
      else
        let a = Char.code cb.pattern.[name + j] in
        let b = Char.code (Bytes.get table (slot + Limits.imm2_size + j)) in
        if Int.equal a b then (cmp [@tailcall]) (j + 1) else a - b
    in
    cmp 0
  in
  (* pcre2_compile.c:9187-9208 — find the insertion point. *)
  let slot = ref 0 in
  let i = ref 0 in
  let broke = ref false in
  while (not !broke) && !i < tablecount do
    let crc = memcmp_name !slot in
    let crc =
      if
        Int.equal crc 0
        && not
             (Int.equal
                (Char.code
                   (Bytes.get table (!slot + Limits.imm2_size + length)))
                0)
      then -1 (* Current name is a substring *)
      else crc
    in
    (* Make space in the table and break the loop for an earlier name. For
       a duplicate or later name, carry on. We do this for duplicates so
       that in the simple case (when ?(| is not used) they are in order of
       their numbers. In all cases they are in the order in which they
       appear in the pattern. *)
    if crc < 0 then (
      (* memmove(slot + name_entry_size, slot,
         (tablecount - i) * name_entry_size): Bytes.blit is memmove. *)
      Bytes.blit table !slot table
        (!slot + cb.name_entry_size)
        ((tablecount - !i) * cb.name_entry_size);
      broke := true)
    else (
      slot := !slot + cb.name_entry_size;
      incr i)
  done;
  (* pcre2_compile.c:9210-9218 — write the entry: group number, name, then
     a terminating zero and zero-fill (the vacated slot holds stale bytes
     after the memmove). *)
  put2 table !slot groupno;
  Bytes.blit_string cb.pattern name table (!slot + Limits.imm2_size) length;
  Bytes.fill table
    (!slot + Limits.imm2_size + length)
    (cb.name_entry_size - length - Limits.imm2_size)
    '\000'

(* ---------- Parsed-pattern lookbehind-length machinery ---------- *)

(* pcre2_intmodedep.h:686-692 — structure for checking for mutual recursion
   when scanning compiled or parsed code. The C's uint32_t *groupptr becomes
   an index into cb.parsed_pattern; the chain pointer becomes an option. *)
type parsed_recurse_check = {
  prev : parsed_recurse_check option;
  groupptr : int;
}

(* pcre2_compile.c:375 — values for parsed_skip's skiptype parameter. *)
let pskip_alt = 0
let pskip_class = 1
let pskip_ket = 2

(* pcre2_compile.c:396-402 — GI values stored in the groupinfo vector:
   "when scanning the parsed pattern, information about groups with fixed
   lengths is remembered in a dynamically created vector of the following
   flags + length values, indexed by group number". *)
let gi_set_fixed_length = 0x80000000
let gi_not_fixed_length = 0x40000000
let gi_fixed_length_mask = 0x0000ffff

(* The C's INT_MAX (32-bit int build), used by the lookbehind-length
   arithmetic overflow checks below. *)
let int_max = 0x7fffffff

(* The C's `return -1` sites inside get_branchlength and the labels it
   reaches by goto (ISNOTFIXED, PARSED_SKIP_FAILED, RECURSE_OR_BACKREF_
   LENGTH, CHECK_GROUP — the last two factored into functions below, so
   the exception crosses back into get_branchlength's handler, which turns
   it into the C's -1 return). *errcodeptr has been dealt with at every
   raise site (some C paths deliberately leave it 0). Never escapes
   get_branchlength (port-conventions §2; compile phase only). *)
exception Bl_fail

(* pcre2_compile.c:9223-9342 — parsed_skip. This function is called to skip
   parts of the parsed pattern when finding the length of a lookbehind
   branch. It is called after ( *ACCEPT) and ( *FAIL) to find the end of the
   branch, it is called to skip over an internal lookaround or (DEFINE)
   group, and it is also called to skip to the end of a class, during which
   it will never encounter nested groups (but there's no need to have
   special code for that).

   When called to find the end of a branch or group, pptr must point to the
   first meta code inside the branch, not the branch-starting code. In
   other cases it can point to the item that causes the function to be
   called.

   Arguments:
     cb        compile block (the C takes the raw pptr; the parsed pattern
               is an array here, so its owner comes too)
     pptr      current index to skip from
     skiptype  pskip_class when skipping to end of class
               pskip_alt when META_ALT ends the skip
               pskip_ket when only META_KET ends the skip

   Returns: new value of pptr, or -1 (the C's NULL) if META_END is reached
            (should never occur) or for an unknown meta value - likewise. *)
let parsed_skip (cb : compile_block) (pptr : int) ~(skiptype : int) : int =
  let nestlevel = ref 0 in
  let pptr = ref pptr in
  let exception Skip_return of int in
  try
    (* for (;; pptr++) *)
    while true do
      let word = cb.parsed_pattern.(!pptr) in
      let meta = Parse.meta_code word in
      (* The C's switch: `default: if (meta < META_END) continue;` — a
         literal skips the extra-length lookup below; every other case
         falls out of the switch (after any manual pointer bumps) and picks
         up its extra data length from the table. *)
      if word < Parse.meta_end then () (* Literal: continue *)
      else (
        if Int.equal meta Parse.meta_end then
          (* pcre2_compile.c:9264-9267 — this should never occur. *)
          raise_notrace (Skip_return (-1))
        else if Int.equal meta Parse.meta_backref then (
          if
            (* pcre2_compile.c:9269-9273 — the data for these items is
               variable in length; offset is present only if group >= 10. *)
            Parse.meta_data word >= 10
          then pptr := !pptr + Parse.sizeoffset)
        else if Int.equal meta Parse.meta_escape then (
          (* pcre2_compile.c:9275-9288 — a few escapes are followed by
             data items. *)
          let d = Parse.meta_data word in
          if Int.equal d Parse.esc_big_p || Int.equal d Parse.esc_p then
            pptr := !pptr + 1
          else if Int.equal d Parse.esc_g || Int.equal d Parse.esc_k then
            pptr := !pptr + 1 + Parse.sizeoffset)
        else if
          Int.equal meta Parse.meta_mark
          || Int.equal meta Parse.meta_commit_arg
          || Int.equal meta Parse.meta_prune_arg
          || Int.equal meta Parse.meta_skip_arg
          || Int.equal meta Parse.meta_then_arg
        then
          (* pcre2_compile.c:9290-9296 — add the length of the name (the
             table below adds the length word itself). *)
          pptr := !pptr + cb.parsed_pattern.(!pptr + 1)
        else if Int.equal meta Parse.meta_class_end then (
          if
            (* pcre2_compile.c:9300-9302 — these are the "active" items in
               this loop. *)
            Int.equal skiptype pskip_class
          then raise_notrace (Skip_return !pptr))
        else if
          Int.equal meta Parse.meta_atomic
          || Int.equal meta Parse.meta_capture
          || Int.equal meta Parse.meta_cond_assert
          || Int.equal meta Parse.meta_cond_define
          || Int.equal meta Parse.meta_cond_name
          || Int.equal meta Parse.meta_cond_number
          || Int.equal meta Parse.meta_cond_rname
          || Int.equal meta Parse.meta_cond_rnumber
          || Int.equal meta Parse.meta_cond_version
          || Int.equal meta Parse.meta_lookahead
          || Int.equal meta Parse.meta_lookaheadnot
          || Int.equal meta Parse.meta_lookahead_na
          || Int.equal meta Parse.meta_lookbehind
          || Int.equal meta Parse.meta_lookbehindnot
          || Int.equal meta Parse.meta_lookbehind_na
          || Int.equal meta Parse.meta_nocapture
          || Int.equal meta Parse.meta_script_run
        then (* pcre2_compile.c:9304-9322 *)
          incr nestlevel
        else if Int.equal meta Parse.meta_alt then (
          if
            (* pcre2_compile.c:9324-9326 *)
            Int.equal !nestlevel 0 && Int.equal skiptype pskip_alt
          then raise_notrace (Skip_return !pptr))
        else if Int.equal meta Parse.meta_ket then (
          (* pcre2_compile.c:9328-9331 *)
          if Int.equal !nestlevel 0 then raise_notrace (Skip_return !pptr);
          decr nestlevel);

        (* pcre2_compile.c:9334-9338 — the extra data item length for each
           meta is in a table. *)
        let idx = (meta lsr 16) land 0x7fff in
        if idx >= Array.length Parse.meta_extra_lengths then
          raise_notrace (Skip_return (-1));
        pptr := !pptr + Parse.meta_extra_lengths.(idx));
      incr pptr
    done;
    (* Control never reaches here (pcre2_compile.c:9340-9341). *)
    assert false
  with Skip_return v -> v

(* pcre2_compile.c:9346-9419 — get_grouplength. This is called for nested
   groups within a branch of a lookbehind whose length is being computed.
   On entry, the pointer must be at the first element after the group
   initializing code. On exit it points to OP_KET. Caching is used to
   improve processing speed when the same capturing group occurs many
   times.

   Arguments:
     pptrptr     pointer (ref) to index in the parsed pattern
     minptr      where to return the minimum length
     isinline    false if a reference or recursion; true for inline group
     errcodeptr  pointer to the errorcode
     lcptr       pointer to the loop counter
     group       number of captured group or -1 for a non-capturing group
     recurses    chain of recurse_check to catch mutual recursion
     cb          pointer to the compile data

   Returns: the maximum group length or a negative number. *)
let rec get_grouplength (pptrptr : int ref) (minptr : int ref)
    ~(isinline : bool) (errcodeptr : int ref) (lcptr : int ref) ~(group : int)
    (recurses : parsed_recurse_check option) (cb : compile_block) : int =
  (* uint32_t *gi = cb->groupinfo + 2 * group *)
  let gi = 2 * group in
  let grouplength = ref (-1) in
  let groupminlength = ref int_max in
  let exception Gl_return of int in
  try
    (* pcre2_compile.c:9377-9392 — the cache can be used only if there is
       no possibility of there being two groups with the same number. We do
       not need to set the end pointer for a group that is being processed
       as a back reference or recursion, but we must do so for an inline
       group. *)
    if group > 0 && Int.equal (cb.external_flags land Parse.dupcapused) 0 then (
      let groupinfo = cb.groupinfo.(gi) in
      if not (Int.equal (groupinfo land gi_not_fixed_length) 0) then
        raise_notrace (Gl_return (-1));
      if not (Int.equal (groupinfo land gi_set_fixed_length) 0) then (
        if isinline then pptrptr := parsed_skip cb !pptrptr ~skiptype:pskip_ket;
        minptr := cb.groupinfo.(gi + 1);
        raise_notrace (Gl_return (groupinfo land gi_fixed_length_mask))));

    (* pcre2_compile.c:9394-9405 — scan the group. In this case we find the
       end pointer of necessity. The C's `goto ISNOTFIXED` is the
       Gl_notfixed handler below. *)
    let exception Gl_notfixed in
    try
      let continue_ = ref true in
      while !continue_ do
        let branchminlength = ref 0 in
        let branchlength =
          get_branchlength pptrptr branchminlength errcodeptr lcptr recurses cb
        in
        if branchlength < 0 then raise_notrace Gl_notfixed;
        if branchlength > !grouplength then grouplength := branchlength;
        if !branchminlength < !groupminlength then
          groupminlength := !branchminlength;
        if Int.equal cb.parsed_pattern.(!pptrptr) Parse.meta_ket then
          continue_ := false
        else pptrptr := !pptrptr + 1 (* Skip META_ALT *)
      done;

      (* pcre2_compile.c:9407-9414 *)
      if group > 0 then (
        cb.groupinfo.(gi) <-
          cb.groupinfo.(gi) lor (gi_set_fixed_length lor !grouplength);
        cb.groupinfo.(gi + 1) <- !groupminlength);
      minptr := !groupminlength;
      !grouplength
    with Gl_notfixed ->
      (* ISNOTFIXED: pcre2_compile.c:9416-9418 *)
      if group > 0 then
        cb.groupinfo.(gi) <- cb.groupinfo.(gi) lor gi_not_fixed_length;
      -1
  with Gl_return v -> v

(* pcre2_compile.c:9423-9843 — get_branchlength. Return fixed maximum and
   minimum lengths for a branch in a lookbehind, giving an error if the
   length is not limited. On entry, *pptrptr points to the first element
   inside the branch. On exit it is set to point to the ALT or KET.

   Arguments:
     pptrptr     pointer (ref) to index in the parsed pattern
     minptr      where to return the minimum length
     errcodeptr  pointer to error code
     lcptr       pointer to loop counter
     recurses    chain of recurse_check to catch mutual recursion
     cb          pointer to compile block

   Returns: the maximum length, or a negative value on error (some paths -
   the C's bare `return -1`s such as \X - leave *errcodeptr at 0; the
   callers patch in ERR25). *)
and get_branchlength (pptrptr : int ref) (minptr : int ref)
    (errcodeptr : int ref) (lcptr : int ref)
    (recurses : parsed_recurse_check option) (cb : compile_block) : int =
  let branchlength = ref 0 in
  let branchminlength = ref 0 in
  let lastitemlength = ref 0 in
  let lastitemminlength = ref 0 in
  let pptr = ref !pptrptr in

  (* Local exception modelling the C's `goto EXIT` (port-conventions §2;
     compile phase only); Bl_fail (module level, above) models the
     `return -1` sites. *)
  let exception Bl_exit in
  (* The ISNOTFIXED label (pcre2_compile.c:9809-9812), reached by goto from
     several cases and by switch fallthrough from the default case. *)
  let isnotfixed () : 'a =
    errcodeptr := Errors.err25 (* Not fixed length *);
    raise_notrace Bl_fail
  in
  (* The PARSED_SKIP_FAILED label (pcre2_compile.c:9840-9842). *)
  let parsed_skip_failed () : 'a =
    errcodeptr := Errors.err90;
    raise_notrace Bl_fail
  in

  (* pcre2_compile.c:9455-9463 — a large and/or complex regex can take too
     long to process. This can happen more often when (?| groups are
     present in the pattern because their length cannot be cached. The C's
     ( *lcptr)++ compares the value before incrementing. *)
  let lc = !lcptr in
  lcptr := lc + 1;
  if lc > 2000 then (
    errcodeptr := Errors.err35 (* Lookbehind is too complicated *);
    -1)
  else
    try
      (* pcre2_compile.c:9465-9467 — scan the branch, accumulating the
         length: for (;; pptr++). *)
      while true do
        let group = ref 0 in
        let itemlength = ref 0 in
        let itemminlength = ref 0 in
        let word = cb.parsed_pattern.(!pptr) in

        (* pcre2_compile.c:9477-9480 *)
        (if word < Parse.meta_end then (
           itemlength := 1;
           itemminlength := 1)
         else
           let meta = Parse.meta_code word in
           if Int.equal meta Parse.meta_ket || Int.equal meta Parse.meta_alt
           then raise_notrace Bl_exit
           else if
             Int.equal meta Parse.meta_accept || Int.equal meta Parse.meta_fail
           then (
             (* pcre2_compile.c:9488-9495 — ( *ACCEPT) and ( *FAIL)
                terminate the branch, but we must skip to the actual
                termination. *)
             let skipped = parsed_skip cb !pptr ~skiptype:pskip_alt in
             if Int.equal skipped (-1) then parsed_skip_failed ();
             pptr := skipped;
             raise_notrace Bl_exit)
           else if
             Int.equal meta Parse.meta_mark
             || Int.equal meta Parse.meta_commit_arg
             || Int.equal meta Parse.meta_prune_arg
             || Int.equal meta Parse.meta_skip_arg
             || Int.equal meta Parse.meta_then_arg
           then
             (* pcre2_compile.c:9497-9503 *)
             pptr := !pptr + cb.parsed_pattern.(!pptr + 1) + 1
           else if
             Int.equal meta Parse.meta_circumflex
             || Int.equal meta Parse.meta_commit
             || Int.equal meta Parse.meta_dollar
             || Int.equal meta Parse.meta_prune
             || Int.equal meta Parse.meta_skip
             || Int.equal meta Parse.meta_then
           then (* pcre2_compile.c:9505-9511 *)
             ()
           else if Int.equal meta Parse.meta_options then
             (* pcre2_compile.c:9513-9515 *)
             pptr := !pptr + 2
           else if Int.equal meta Parse.meta_bigvalue then (
             (* pcre2_compile.c:9517-9520 *)
             itemlength := 1;
             itemminlength := 1;
             pptr := !pptr + 1)
           else if
             Int.equal meta Parse.meta_class
             || Int.equal meta Parse.meta_class_not
           then (
             (* pcre2_compile.c:9522-9527 *)
             itemlength := 1;
             itemminlength := 1;
             let skipped = parsed_skip cb !pptr ~skiptype:pskip_class in
             if Int.equal skipped (-1) then parsed_skip_failed ();
             pptr := skipped)
           else if
             Int.equal meta Parse.meta_class_empty_not
             || Int.equal meta Parse.meta_dot
           then (
             (* pcre2_compile.c:9529-9532 *)
             itemlength := 1;
             itemminlength := 1)
           else if Int.equal meta Parse.meta_callout_number then
             (* pcre2_compile.c:9534-9536 *)
             pptr := !pptr + 3
           else if Int.equal meta Parse.meta_callout_string then
             (* pcre2_compile.c:9538-9540 *)
             pptr := !pptr + 3 + Parse.sizeoffset
           else if Int.equal meta Parse.meta_escape then (
             (* pcre2_compile.c:9542-9566 — only some escapes consume a
                character. Of those, \R can match one or two characters,
                but \X is never allowed because it matches an unknown
                number of characters. \C is allowed only in 32-bit and
                non-UTF 8/16-bit modes. *)
             let escape = Parse.meta_data word in
             if Int.equal escape Parse.esc_big_x then
               (* return -1 with *errcodeptr unset (see the header note). *)
               raise_notrace Bl_fail
             else if Int.equal escape Parse.esc_big_r then (
               itemminlength := 1;
               itemlength := 2)
             else if escape > Parse.esc_b && escape < Parse.esc_big_z then (
               (* PCRE2_CODE_UNIT_WIDTH != 32 in this 8-bit port. *)
               if
                 (not (Int.equal (cb.external_options land Options.utf) 0))
                 && Int.equal escape Parse.esc_big_c
               then (
                 errcodeptr := Errors.err36;
                 raise_notrace Bl_fail);
               itemlength := 1;
               itemminlength := 1;
               if
                 Int.equal escape Parse.esc_p
                 || Int.equal escape Parse.esc_big_p
               then pptr := !pptr + 1 (* Skip prop data *)))
           else if
             Int.equal meta Parse.meta_lookahead
             || Int.equal meta Parse.meta_lookaheadnot
             || Int.equal meta Parse.meta_lookahead_na
           then (
             (* pcre2_compile.c:9568-9602 — lookaheads do not contribute to
                the length of this branch, but they may contain lookbehinds
                within them whose lengths need to be set. *)
             let ret = ref !pptr in
             errcodeptr :=
               check_lookbehinds cb (!pptr + 1) (Some ret) recurses lcptr;
             if not (Int.equal !errcodeptr 0) then raise_notrace Bl_fail;
             pptr := !ret;

             (* Ignore any qualifiers that follow a lookahead assertion. *)
             let next = cb.parsed_pattern.(!pptr + 1) in
             if
               Int.equal next Parse.meta_asterisk
               || Int.equal next Parse.meta_asterisk_plus
               || Int.equal next Parse.meta_asterisk_query
               || Int.equal next Parse.meta_plus
               || Int.equal next Parse.meta_plus_plus
               || Int.equal next Parse.meta_plus_query
               || Int.equal next Parse.meta_query
               || Int.equal next Parse.meta_query_plus
               || Int.equal next Parse.meta_query_query
             then pptr := !pptr + 1
             else if
               Int.equal next Parse.meta_minmax
               || Int.equal next Parse.meta_minmax_plus
               || Int.equal next Parse.meta_minmax_query
             then pptr := !pptr + 3)
           else if
             Int.equal meta Parse.meta_lookbehind
             || Int.equal meta Parse.meta_lookbehindnot
             || Int.equal meta Parse.meta_lookbehind_na
           then (
             if
               (* pcre2_compile.c:9604-9612 — a nested lookbehind does not
                  contribute any length to this lookbehind, but must itself
                  be checked and have its lengths set. *)
               not (set_lookbehind_lengths cb pptr errcodeptr lcptr recurses)
             then raise_notrace Bl_fail)
           else if
             Int.equal meta Parse.meta_backref_byname
             || Int.equal meta Parse.meta_recurse_byname
           then (
             (* pcre2_compile.c:9614-9661 — back references and recursions
                are handled by very similar code. At this stage, the names
                generated in the parsing pass are available, but the main
                name table has not yet been created. So for the named
                varieties, scan the list of names in order to get the
                number of the first one in the pattern, and whether or not
                this name is duplicated. *)
             if
               Int.equal meta Parse.meta_backref_byname
               && not
                    (Int.equal
                       (cb.external_options land Options.match_unset_backref)
                       0)
             then isnotfixed ();
             (* Fall through (META_RECURSE_BYNAME) *)
             let is_dupname = ref false in
             pptr := !pptr + 1;
             let length = cb.parsed_pattern.(!pptr) in
             (* GETPLUSOFFSET(offset, pptr) *)
             pptr := !pptr + 1;
             let offset = cb.parsed_pattern.(!pptr) in
             let name = offset in
             (* PRIV(strncmp) (pcre2_string_utils.c:156-167) becomes a
                code-unit loop over the pattern, as in compile_branch's
                named-backref arm; the C breaks at the first match. *)
             let i = ref 0 in
             let broke = ref false in
             while (not !broke) && !i < cb.names_found do
               let ng = cb.named_groups.(!i) in
               let name_eq =
                 Int.equal length ng.Parse.length
                 &&
                 let rec cmp j =
                   if j >= length then true
                   else if
                     Char.equal
                       cb.pattern.[name + j]
                       cb.pattern.[ng.Parse.name + j]
                   then (cmp [@tailcall]) (j + 1)
                   else false
                 in
                 cmp 0
               in
               if name_eq then (
                 group := ng.Parse.number;
                 is_dupname := ng.Parse.isdup;
                 broke := true)
               else incr i
             done;

             if Int.equal !group 0 then (
               errcodeptr := Errors.err15 (* Non-existent subpattern *);
               cb.erroroffset <- offset;
               raise_notrace Bl_fail);

             (* pcre2_compile.c:9653-9661 — a numerical back reference can
                be fixed length if duplicate capturing groups are not being
                used. A non-duplicate named back reference can also be
                handled. Handle as a numbered version, or fail as a
                duplicate name. *)
             if
               Int.equal meta Parse.meta_recurse_byname
               || (not !is_dupname)
                  && Int.equal (cb.external_flags land Parse.dupcapused) 0
             then
               recurse_or_backref_length ~group:!group ~offset pptr itemlength
                 itemminlength errcodeptr lcptr recurses cb
             else isnotfixed () (* Duplicate name or number *))
           else if Int.equal meta Parse.meta_backref then (
             (* pcre2_compile.c:9663-9676 — the offset values for back
                references < 10 are in a separate vector because otherwise
                they would use more than two parsed pattern elements on
                64-bit systems. *)
             if
               (not
                  (Int.equal
                     (cb.external_options land Options.match_unset_backref)
                     0))
               || not (Int.equal (cb.external_flags land Parse.dupcapused) 0)
             then isnotfixed ();
             group := Parse.meta_data word;
             if !group < 10 then
               let offset = cb.small_ref_offset.(!group) in
               recurse_or_backref_length ~group:!group ~offset pptr itemlength
                 itemminlength errcodeptr lcptr recurses cb
             else (
               (* Fall through to the META_RECURSE case: for groups >= 10 -
                  picking up group twice does no harm
                  (pcre2_compile.c:9678-9686). *)
               group := Parse.meta_data word;
               pptr := !pptr + 1;
               let offset = cb.parsed_pattern.(!pptr) in
               recurse_or_backref_length ~group:!group ~offset pptr itemlength
                 itemminlength errcodeptr lcptr recurses cb))
           else if Int.equal meta Parse.meta_recurse then (
             (* pcre2_compile.c:9681-9686 — a true recursion implies not
                fixed length, but a subroutine call may be OK. Back
                reference "recursions" are also failed. *)
             group := Parse.meta_data word;
             pptr := !pptr + 1;
             let offset = cb.parsed_pattern.(!pptr) in
             recurse_or_backref_length ~group:!group ~offset pptr itemlength
               itemminlength errcodeptr lcptr recurses cb)
           else if Int.equal meta Parse.meta_cond_define then (
             (* pcre2_compile.c:9730-9736 — a (DEFINE) group is never
                obeyed inline and so it does not contribute to the length
                of this branch. Skip from the following item to the next
                unpaired ket.
                DEVIATION: the C stores parsed_skip's result without a NULL
                check (it would dereference NULL, UB); fail with the
                parsed_skip error instead. *)
             let skipped = parsed_skip cb (!pptr + 1) ~skiptype:pskip_ket in
             if Int.equal skipped (-1) then parsed_skip_failed ();
             pptr := skipped)
           else if
             Int.equal meta Parse.meta_cond_name
             || Int.equal meta Parse.meta_cond_number
             || Int.equal meta Parse.meta_cond_rname
             || Int.equal meta Parse.meta_cond_rnumber
           then (
             (* pcre2_compile.c:9738-9746 — check other nested groups -
                advance past the initial data for each type and then seek a
                fixed length with get_grouplength(). *)
             pptr := !pptr + 2 + Parse.sizeoffset;
             check_group ~group:!group pptr itemlength itemminlength errcodeptr
               lcptr recurses cb)
           else if Int.equal meta Parse.meta_cond_assert then (
             (* pcre2_compile.c:9748-9750 *)
             pptr := !pptr + 1;
             check_group ~group:!group pptr itemlength itemminlength errcodeptr
               lcptr recurses cb)
           else if Int.equal meta Parse.meta_cond_version then (
             (* pcre2_compile.c:9752-9754 *)
             pptr := !pptr + 4;
             check_group ~group:!group pptr itemlength itemminlength errcodeptr
               lcptr recurses cb)
           else if Int.equal meta Parse.meta_capture then (
             (* pcre2_compile.c:9756-9758 + fall through *)
             group := Parse.meta_data word;
             pptr := !pptr + 1;
             check_group ~group:!group pptr itemlength itemminlength errcodeptr
               lcptr recurses cb)
           else if
             Int.equal meta Parse.meta_atomic
             || Int.equal meta Parse.meta_nocapture
             || Int.equal meta Parse.meta_script_run
           then (
             (* pcre2_compile.c:9760-9770 *)
             pptr := !pptr + 1;
             check_group ~group:!group pptr itemlength itemminlength errcodeptr
               lcptr recurses cb)
           else if
             Int.equal meta Parse.meta_query
             || Int.equal meta Parse.meta_query_plus
             || Int.equal meta Parse.meta_query_query
             || Int.equal meta Parse.meta_minmax
             || Int.equal meta Parse.meta_minmax_plus
             || Int.equal meta Parse.meta_minmax_query
           then (
             (* pcre2_compile.c:9772-9805 — the ? and {n,m} families share
                the REPETITION code. Exact repetition is OK; variable
                repetition is not. A repetition of zero must subtract the
                length that has already been added. *)
             let min = ref 0 in
             let max = ref 1 in
             if
               Int.equal meta Parse.meta_minmax
               || Int.equal meta Parse.meta_minmax_plus
               || Int.equal meta Parse.meta_minmax_query
             then (
               min := cb.parsed_pattern.(!pptr + 1);
               max := cb.parsed_pattern.(!pptr + 2);
               pptr := !pptr + 2);

             (* REPETITION *)
             if not (Int.equal !max Limits.repeat_unlimited) then (
               if
                 (not (Int.equal !lastitemlength 0))
                 (* Should not occur, but just in case *)
                 && (not (Int.equal !max 0))
                 && (int_max - !branchlength) / !lastitemlength < !max - 1
               then (
                 errcodeptr :=
                   Errors.err87 (* Integer overflow; lookbehind too big *);
                 raise_notrace Bl_fail);
               if Int.equal !min 0 then
                 branchminlength := !branchminlength - !lastitemminlength
               else itemminlength := (!min - 1) * !lastitemminlength;
               if Int.equal !max 0 then
                 branchlength := !branchlength - !lastitemlength
               else itemlength := (!max - 1) * !lastitemlength)
             else (* Fall through to the default case. *)
               isnotfixed ())
           else
             (* pcre2_compile.c:9807-9812 — any other item means this
                branch does not have a fixed length. *)
             isnotfixed ());

        (* pcre2_compile.c:9815-9825 — add the item length to the
           branchlength, checking for integer overflow and for the branch
           length exceeding the overall limit. Later, if there is at least
           one variable-length branch in the group, there is a test for
           the (smaller) variable-length branch length limit. *)
        if int_max - !branchlength < !itemlength then (
          errcodeptr := Errors.err87;
          raise_notrace Bl_fail)
        else (
          branchlength := !branchlength + !itemlength;
          if !branchlength > Limits.lookbehind_max then (
            errcodeptr := Errors.err87;
            raise_notrace Bl_fail));

        branchminlength := !branchminlength + !itemminlength;

        (* pcre2_compile.c:9829-9832 — save this item length for use if
           the next item is a quantifier. *)
        lastitemlength := !itemlength;
        lastitemminlength := !itemminlength;
        pptr := !pptr + 1
      done;
      assert false
    with
    | Bl_exit ->
        (* EXIT: pcre2_compile.c:9835-9838 *)
        pptrptr := !pptr;
        minptr := !branchminlength;
        !branchlength
    | Bl_fail -> -1

(* pcre2_compile.c:9688-9728 — the RECURSE_OR_BACKREF_LENGTH label inside
   get_branchlength, reached from the named/numbered backreference and
   recursion cases. Finds the referenced group in the parsed pattern and
   measures it with get_grouplength, guarding against local and mutual
   recursion. Writes the item lengths through itemlength/itemminlength;
   failure paths raise Bl_fail (caught by the enclosing get_branchlength),
   which is the C's `return -1` / `goto ISNOTFIXED` / `goto
   PARSED_SKIP_FAILED`. *)
and recurse_or_backref_length ~(group : int) ~(offset : int) (pptr : int ref)
    (itemlength : int ref) (itemminlength : int ref) (errcodeptr : int ref)
    (lcptr : int ref) (recurses : parsed_recurse_check option)
    (cb : compile_block) : unit =
  let isnotfixed () : 'a =
    errcodeptr := Errors.err25;
    raise_notrace Bl_fail
  in
  (* pcre2_compile.c:9689-9695 *)
  if group > cb.bracount then (
    cb.erroroffset <- offset;
    errcodeptr := Errors.err15 (* Non-existent subpattern *);
    raise_notrace Bl_fail);
  if Int.equal group 0 then isnotfixed () (* Local recursion *);
  (* pcre2_compile.c:9696-9700 — find the referenced group. (The C's scan
     cannot hit META_END because group <= cb->bracount guarantees the
     capture exists; the loop condition mirrors `*gptr != META_END` all the
     same.) *)
  let gptr = ref 0 in
  while
    (not (Int.equal cb.parsed_pattern.(!gptr) (Parse.meta_capture lor group)))
    && not (Int.equal cb.parsed_pattern.(!gptr) Parse.meta_end)
  do
    if Int.equal (Parse.meta_code cb.parsed_pattern.(!gptr)) Parse.meta_bigvalue
    then gptr := !gptr + 1;
    gptr := !gptr + 1
  done;

  (* pcre2_compile.c:9702-9711 — we must start the search for the end of
     the group at the first meta code inside the group. Otherwise it will
     be treated as an enclosed group. *)
  let gptrend = parsed_skip cb (!gptr + 1) ~skiptype:pskip_ket in
  if Int.equal gptrend (-1) then (
    (* PARSED_SKIP_FAILED: pcre2_compile.c:9840-9842 *)
    errcodeptr := Errors.err90;
    raise_notrace Bl_fail);
  if !pptr > !gptr && !pptr < gptrend then isnotfixed () (* Local recursion *);
  let rec find_mutual (r : parsed_recurse_check option) : bool =
    match r with
    | None -> false
    | Some rc ->
        if Int.equal rc.groupptr !gptr then true else (find_mutual [@tailcall]) rc.prev
  in
  if find_mutual recurses then isnotfixed () (* Mutual recursion *);
  let this_recurse = { prev = recurses; groupptr = !gptr } in

  (* pcre2_compile.c:9713-9728 — we do not need to know the position of
     the end of the group, that is, gptr is not used after the call to
     get_grouplength(). Setting isinline false stops it scanning for the
     end when the length can be found in the cache. *)
  gptr := !gptr + 1;
  let groupminlength = ref 0 in
  let grouplength =
    get_grouplength gptr groupminlength ~isinline:false errcodeptr lcptr ~group
      (Some this_recurse) cb
  in
  if grouplength < 0 then (
    if Int.equal !errcodeptr 0 then isnotfixed ();
    raise_notrace Bl_fail (* Error already set *));
  itemlength := grouplength;
  itemminlength := !groupminlength

(* pcre2_compile.c:9764-9770 — the CHECK_GROUP label inside
   get_branchlength: seek a fixed length with get_grouplength(). A
   negative result is the C's `return -1` (the errorcode may deliberately
   still be 0; the callers patch in ERR25). *)
and check_group ~(group : int) (pptr : int ref) (itemlength : int ref)
    (itemminlength : int ref) (errcodeptr : int ref) (lcptr : int ref)
    (recurses : parsed_recurse_check option) (cb : compile_block) : unit =
  let groupminlength = ref 0 in
  let grouplength =
    get_grouplength pptr groupminlength ~isinline:true errcodeptr lcptr ~group
      recurses cb
  in
  if grouplength < 0 then raise_notrace Bl_fail;
  itemlength := grouplength;
  itemminlength := !groupminlength

(* pcre2_compile.c:9847-9937 — set_lookbehind_lengths. This function is
   called for each lookbehind, to set the lengths in its branches. An error
   occurs if any branch does not have a limited maximum length that is less
   than the limit (65535). On exit, the pointer must be left on the final
   ket.

   The function also maintains the max_lookbehind value. Any lookbehind
   branch that contains a nested lookbehind may actually look further back
   than the length of the branch. The additional amount is passed back from
   get_branchlength() as an "extra" value.

   Arguments:
     cb          pointer to compile block
     pptrptr     pointer (ref) to index in the parsed pattern
     errcodeptr  pointer to error code
     lcptr       pointer to loop counter
     recurses    chain of recurse_check to catch mutual recursion

   Returns: true if all is well; false otherwise, with error code and
   offset set. *)
and set_lookbehind_lengths (cb : compile_block) (pptrptr : int ref)
    (errcodeptr : int ref) (lcptr : int ref)
    (recurses : parsed_recurse_check option) : bool =
  let bptr = ref !pptrptr in
  let gbptr = !pptrptr in
  let maxlength = ref 0 in
  let minlength = ref int_max in
  let variable = ref false in

  (* READPLUSOFFSET(offset, bptr) — offset for error messages. *)
  let offset = cb.parsed_pattern.(!bptr + 1) in
  pptrptr := !pptrptr + Parse.sizeoffset;

  let exception Slb_false in
  try
    (* pcre2_compile.c:9886-9913 — each branch can have a different
       maximum length, but we can keep only a single minimum for the whole
       group, because there's nowhere to save individual values in the
       META_ALT item: do { ... } while (META_CODE( *bptr) == META_ALT). *)
    let continue_ = ref true in
    while !continue_ do
      pptrptr := !pptrptr + 1;
      let branchminlength = ref 0 in
      let branchlength =
        get_branchlength pptrptr branchminlength errcodeptr lcptr recurses cb
      in

      if branchlength < 0 then (
        (* The errorcode and offset may already be set from a nested
           lookbehind. *)
        if Int.equal !errcodeptr 0 then errcodeptr := Errors.err25;
        if Int.equal cb.erroroffset Parse.pcre2_unset then
          cb.erroroffset <- offset;
        raise_notrace Slb_false);

      if not (Int.equal branchlength !branchminlength) then variable := true;
      if !branchminlength < !minlength then minlength := !branchminlength;
      if branchlength > !maxlength then maxlength := branchlength;
      if branchlength > cb.max_lookbehind then cb.max_lookbehind <- branchlength;
      (* branchlength never more than 65535 *)
      cb.parsed_pattern.(!bptr) <- cb.parsed_pattern.(!bptr) lor branchlength;
      bptr := !pptrptr;
      if
        not
          (Int.equal (Parse.meta_code cb.parsed_pattern.(!bptr)) Parse.meta_alt)
      then continue_ := false
    done;

    (* pcre2_compile.c:9915-9936 — if any branch is of variable length,
       the whole lookbehind is of variable length. If the maximum length
       of any branch exceeds the maximum for variable lookbehinds, give an
       error. Otherwise, the minimum length is set in the word that
       follows the original group META value. For a fixed-length
       lookbehind, this is set to LOOKBEHIND_MAX, to indicate that each
       branch is of a fixed (but possibly different) length. (The C
       assigns gbptr[1] in the if/else and then re-assigns the same value
       at 9935; one assignment suffices, done after the error check to
       match the observable order.) *)
    if !variable then (
      cb.parsed_pattern.(gbptr + 1) <- !minlength;
      if !maxlength > cb.max_varlookbehind then (
        errcodeptr := Errors.err100;
        cb.erroroffset <- offset;
        raise_notrace Slb_false))
    else cb.parsed_pattern.(gbptr + 1) <- Limits.lookbehind_max;
    true
  with Slb_false -> false

(* pcre2_compile.c:9941-10102 — check_lookbehinds. This function is called
   at the end of parsing a pattern if any lookbehinds were encountered. It
   scans the parsed pattern for them, calling set_lookbehind_lengths() for
   each one. At the start, the errorcode is zero and the error offset is
   marked unset. This enables the functions above not to override settings
   from deeper nestings.

   This function is called recursively from get_branchlength() for
   lookaheads in order to process any lookbehinds that they may contain. It
   stops when it hits a non-nested closing parenthesis in this case,
   returning the index of it through retptr.

   Arguments:
     cb        points to the compile block
     pptr      where to start (start of pattern or start of lookahead)
     retptr    if not None, return the ket index here
     recurses  chain of recurse_check to catch mutual recursion
     lcptr     points to loop counter

   Returns: 0 on success, or an errorcode (cb.erroroffset will be set). *)
and check_lookbehinds (cb : compile_block) (pptr : int)
    (retptr : int ref option) (recurses : parsed_recurse_check option)
    (lcptr : int ref) : int =
  let errorcode = ref 0 in
  let nestlevel = ref 0 in
  let pptr = ref pptr in

  cb.erroroffset <- Parse.pcre2_unset;

  let exception Cl_return of int in
  try
    (* for (; *pptr != META_END; pptr++) *)
    while not (Int.equal cb.parsed_pattern.(!pptr) Parse.meta_end) do
      let word = cb.parsed_pattern.(!pptr) in
      (if word < Parse.meta_end then () (* Literal: continue *)
       else
         let meta = Parse.meta_code word in
         if Int.equal meta Parse.meta_escape then (
           if
             (* pcre2_compile.c:9983-9986 *)
             Int.equal (word - Parse.meta_escape) Parse.esc_big_p
             || Int.equal (word - Parse.meta_escape) Parse.esc_p
           then pptr := !pptr + 1)
         else if Int.equal meta Parse.meta_ket then (
           (* pcre2_compile.c:9988-9994 *)
           decr nestlevel;
           if !nestlevel < 0 then (
             (match retptr with Some r -> r := !pptr | None -> ());
             raise_notrace (Cl_return 0)))
         else if
           Int.equal meta Parse.meta_atomic
           || Int.equal meta Parse.meta_capture
           || Int.equal meta Parse.meta_cond_assert
           || Int.equal meta Parse.meta_lookahead
           || Int.equal meta Parse.meta_lookaheadnot
           || Int.equal meta Parse.meta_lookahead_na
           || Int.equal meta Parse.meta_nocapture
           || Int.equal meta Parse.meta_script_run
         then (* pcre2_compile.c:9996-10005 *)
           incr nestlevel
         else if
           Int.equal meta Parse.meta_accept
           || Int.equal meta Parse.meta_alt
           || Int.equal meta Parse.meta_asterisk
           || Int.equal meta Parse.meta_asterisk_plus
           || Int.equal meta Parse.meta_asterisk_query
           || Int.equal meta Parse.meta_backref
           || Int.equal meta Parse.meta_circumflex
           || Int.equal meta Parse.meta_class
           || Int.equal meta Parse.meta_class_empty
           || Int.equal meta Parse.meta_class_empty_not
           || Int.equal meta Parse.meta_class_end
           || Int.equal meta Parse.meta_class_not
           || Int.equal meta Parse.meta_commit
           || Int.equal meta Parse.meta_dollar
           || Int.equal meta Parse.meta_dot
           || Int.equal meta Parse.meta_fail
           || Int.equal meta Parse.meta_plus
           || Int.equal meta Parse.meta_plus_plus
           || Int.equal meta Parse.meta_plus_query
           || Int.equal meta Parse.meta_prune
           || Int.equal meta Parse.meta_query
           || Int.equal meta Parse.meta_query_plus
           || Int.equal meta Parse.meta_query_query
           || Int.equal meta Parse.meta_range_escaped
           || Int.equal meta Parse.meta_range_literal
           || Int.equal meta Parse.meta_skip
           || Int.equal meta Parse.meta_then
         then
           (* pcre2_compile.c:10007-10034 — nothing to do (a META_BACKREF
              offset word, present for groups >= 10, is skipped by the
              literal test above, exactly as in the C). *)
           ()
         else if Int.equal meta Parse.meta_recurse then
           (* pcre2_compile.c:10036-10038 *)
           pptr := !pptr + Parse.sizeoffset
         else if
           Int.equal meta Parse.meta_backref_byname
           || Int.equal meta Parse.meta_recurse_byname
         then
           (* pcre2_compile.c:10040-10043 *)
           pptr := !pptr + 1 + Parse.sizeoffset
         else if Int.equal meta Parse.meta_cond_define then (
           (* pcre2_compile.c:10045-10048 *)
           pptr := !pptr + Parse.sizeoffset;
           incr nestlevel)
         else if
           Int.equal meta Parse.meta_cond_name
           || Int.equal meta Parse.meta_cond_number
           || Int.equal meta Parse.meta_cond_rname
           || Int.equal meta Parse.meta_cond_rnumber
         then (
           (* pcre2_compile.c:10050-10056 *)
           pptr := !pptr + 1 + Parse.sizeoffset;
           incr nestlevel)
         else if Int.equal meta Parse.meta_cond_version then (
           (* pcre2_compile.c:10058-10061 *)
           pptr := !pptr + 3;
           incr nestlevel)
         else if Int.equal meta Parse.meta_callout_string then
           (* pcre2_compile.c:10063-10065 *)
           pptr := !pptr + 3 + Parse.sizeoffset
         else if
           Int.equal meta Parse.meta_bigvalue
           || Int.equal meta Parse.meta_posix
           || Int.equal meta Parse.meta_posix_neg
         then (* pcre2_compile.c:10067-10071 *)
           pptr := !pptr + 1
         else if
           Int.equal meta Parse.meta_minmax
           || Int.equal meta Parse.meta_minmax_query
           || Int.equal meta Parse.meta_minmax_plus
           || Int.equal meta Parse.meta_options
         then (* pcre2_compile.c:10073-10078 *)
           pptr := !pptr + 2
         else if Int.equal meta Parse.meta_callout_number then
           (* pcre2_compile.c:10080-10082 *)
           pptr := !pptr + 3
         else if
           Int.equal meta Parse.meta_mark
           || Int.equal meta Parse.meta_commit_arg
           || Int.equal meta Parse.meta_prune_arg
           || Int.equal meta Parse.meta_skip_arg
           || Int.equal meta Parse.meta_then_arg
         then
           (* pcre2_compile.c:10084-10090 *)
           pptr := !pptr + 1 + cb.parsed_pattern.(!pptr + 1)
         else if
           Int.equal meta Parse.meta_lookbehind
           || Int.equal meta Parse.meta_lookbehindnot
           || Int.equal meta Parse.meta_lookbehind_na
         then (
           if
             (* pcre2_compile.c:10092-10097 *)
             not (set_lookbehind_lengths cb pptr errorcode lcptr recurses)
           then raise_notrace (Cl_return !errorcode))
         else
           (* pcre2_compile.c:9980-9981 — unrecognized meta code (the C
              switch's default case, first in the source). *)
           raise_notrace (Cl_return Errors.err70));
      pptr := !pptr + 1
    done;
    0
  with Cl_return rc -> rc

(* ---------- The compiled pattern ---------- *)

(* pcre2_intmodedep.h:620-644 — pcre2_real_code, the compiled pattern.
   The C lays the name table and the code out in one heap block after the
   struct; here they are two Bytes.t values and "codestart" is offset 0 of
   [code]. DEVIATION: memctl (allocator), tables (always the default
   Chartables in this port), executable_jit (no JIT), blocksize and
   magic_number (no serialization / paranoia check across a C ABI) are
   dropped. Mutable fields are the ones pcre2_compile() assigns after the
   record is created (pcre2_compile.c:10697-10955). *)
type re = {
  code : Bytes.t; (* the compiled bytecode; offset 0 = C's codestart *)
  name_table : Bytes.t; (* name_count entries of name_entry_size units *)
  start_bitmap : Bytes.t; (* 32 bytes; filled by PRIV(study) (M5) *)
  compile_options : int; (* options passed to pcre2_compile() *)
  mutable overall_options : int; (* options after processing the pattern *)
  extra_options : int; (* taken from compile_context *)
  mutable flags : int; (* various state flags *)
  limit_heap : int; (* limit set in the pattern *)
  limit_match : int; (* limit set in the pattern *)
  limit_depth : int; (* limit set in the pattern *)
  mutable first_codeunit : int; (* starting code unit *)
  mutable last_codeunit : int; (* this codeunit must be seen *)
  bsr_convention : int; (* what \R matches *)
  newline_convention : int; (* what is a newline? *)
  mutable max_lookbehind : int; (* longest lookbehind (characters) *)
  mutable minlength : int; (* minimum length of match *)
  mutable top_bracket : int; (* highest numbered group *)
  mutable top_backref : int; (* highest numbered back reference *)
  name_entry_size : int; (* size (code units) of table entries *)
  name_count : int; (* number of name entries in the table *)
}

(* ---------- Start-of-pattern option settings ---------- *)

(* pcre2_compile.c:819-826 — the pso types. *)
type pso_type =
  | Pso_opt (* Value is an option bit *)
  | Pso_flg (* Value is a flag bit *)
  | Pso_nl (* Value is a newline type *)
  | Pso_bsr (* Value is a \R type *)
  | Pso_limh (* Read integer value for heap limit *)
  | Pso_limm (* Read integer value for match limit *)
  | Pso_limd (* Read integer value for depth limit *)

(* pcre2_compile.c:828-859 — the table of start-of-pattern options such as
   ( *UTF) and settings such as ( *LIMIT_MATCH=nnnn) and ( *CRLF). The C
   entries carry explicit lengths; String.length supplies them here.
   STRING_UTFn_RIGHTPAR is the 8-bit "UTF8)" (pcre2_compile.c:73). *)
let pso_list : (string * pso_type * int) array =
  [|
    ("UTF8)", Pso_opt, Options.utf);
    ("UTF)", Pso_opt, Options.utf);
    ("UCP)", Pso_opt, Options.ucp);
    ("NOTEMPTY)", Pso_flg, notempty_set);
    ("NOTEMPTY_ATSTART)", Pso_flg, ne_atst_set);
    ("NO_AUTO_POSSESS)", Pso_opt, Options.no_auto_possess);
    ("NO_DOTSTAR_ANCHOR)", Pso_opt, Options.no_dotstar_anchor);
    ("NO_JIT)", Pso_flg, nojit);
    ("NO_START_OPT)", Pso_opt, Options.no_start_optimize);
    ("LIMIT_HEAP=", Pso_limh, 0);
    ("LIMIT_MATCH=", Pso_limm, 0);
    ("LIMIT_DEPTH=", Pso_limd, 0);
    ("LIMIT_RECURSION=", Pso_limd, 0);
    ("CR)", Pso_nl, Options.newline_cr);
    ("LF)", Pso_nl, Options.newline_lf);
    ("CRLF)", Pso_nl, Options.newline_crlf);
    ("ANY)", Pso_nl, Options.newline_any);
    ("NUL)", Pso_nl, Options.newline_nul);
    ("ANYCRLF)", Pso_nl, Options.newline_anycrlf);
    ("BSR_ANYCRLF)", Pso_bsr, Options.bsr_anycrlf);
    ("BSR_UNICODE)", Pso_bsr, Options.bsr_unicode);
  |]

(* ---------- Compile a Regular Expression ---------- *)

(* pcre2_compile.c:10096-10993 — pcre2_compile. This function reads a
   regular expression in the form of a string and returns a compiled [re],
   or an error code with the offset of the error in the pattern.

   Boundary notes for the OCaml seam:
   - errorptr/erroroffset NULL checks (10181-10183) are N/A: the result
     type carries both.
   - The NULL pattern / ERR16 check (10187-10194) is N/A: an OCaml string
     cannot be NULL.
   - zero_terminated / PCRE2_ZERO_TERMINATED (10225-10226) is N/A: the
     seam always passes an explicit-length string. Where the C reads
     through the terminating NUL of a zero-terminated pattern (the
     ( *LIMIT_...=ddd) scan), [byte_at] below supplies the 0 byte.
   - ERR21 (heap allocation failure, 10514, 10545, 10626) has no OCaml
     equivalent: Array.make/Bytes.make failure (Out_of_memory) is fatal.

   Arguments:
     ccontext  the compile context (default: default_compile_context)
     pattern   the regular expression
     options   option bits (already an int; Options.of_int32 at the seam)

   Returns: Ok re, or Error (errorcode, erroroffset). *)
let pcre2_compile ?(ccontext : compile_context = default_compile_context)
    (pattern : string) ~(options : int) : (re, int * int) result =
  (* The C's HAD_CB_ERROR / HAD_EARLY_ERROR epilogue (10982-10992): every
     error path carries (errorcode, erroroffset). Local exception, never
     escapes this function (port-conventions §2; compile phase only). *)
  let exception Had_error of int * int in
  try
    let patlen = String.length pattern in

    (* pcre2_compile.c:10201-10203 — PCRE2_MATCH_INVALID_UTF implies UTF.
       The modified value is also what re->compile_options records
       (10642). *)
    let options =
      if not (Int.equal (options land Options.match_invalid_utf) 0) then
        options lor Options.utf
      else options
    in

    (* pcre2_compile.c:10205-10220 — check that all undefined public
       option bits are zero (ERR17 = 117, port-conventions §5), and the
       restricted set allowed with PCRE2_LITERAL (ERR92). *)
    if
      (not (Int.equal (options land lnot Options.public_compile_options) 0))
      || not
           (Int.equal
              (ccontext.extra_options
              land lnot Options.public_compile_extra_options)
              0)
    then raise_notrace (Had_error (Errors.err17, 0));
    if
      (not (Int.equal (options land Options.literal) 0))
      && ((not
             (Int.equal
                (options land lnot Options.public_literal_compile_options)
                0))
         || not
              (Int.equal
                 (ccontext.extra_options
                 land lnot Options.public_literal_compile_extra_options)
                 0))
    then raise_notrace (Had_error (Errors.err92, 0));

    (* pcre2_compile.c:10228-10232 — check for an overlong pattern. *)
    if patlen > ccontext.max_pattern_length then
      raise_notrace (Had_error (Errors.err88, 0));

    (* pcre2_compile.c:10238-10290 — initialize the "static" compile
       data. *)
    let cb = make_block ~cx:ccontext pattern ~options in

    (* Reads through the zero terminator of a zero-terminated pattern
       exactly as the C's ptr[pp] does in the ( *LIMIT_...) scan below
       (see the boundary notes above). *)
    let byte_at p = if p >= patlen then 0 else Char.code pattern.[p] in
    (* pcre2_compile.c:408 — IS_DIGIT(x). *)
    let is_digit_code x = x >= Char.code '0' && x <= Char.code '9' in

    (* pcre2_compile.c:10150-10158 — NL/BSR set flags, unset match limits,
       newline/bsr "unset; can be set by the pattern". *)
    let setflags = ref 0 in
    let limit_heap = ref 0xffff_ffff in
    let limit_match = ref 0xffff_ffff in
    let limit_depth = ref 0xffff_ffff in
    let newline = ref 0 in
    let bsr = ref 0 in

    (* pcre2_compile.c:10305-10377 — unless PCRE2_LITERAL is set, check
       for global one-time option settings at the start of the pattern,
       and remember the offset to the actual regex. *)
    let skipatstart = ref 0 in
    (if Int.equal (options land Options.literal) 0 then
       let in_pso = ref true in
       while
         !in_pso
         && patlen - !skipatstart >= 2
         && Int.equal (byte_at !skipatstart) (Char.code '(')
         && Int.equal (byte_at (!skipatstart + 1)) (Char.code '*')
       do
         (* for (i = 0; i < sizeof(pso_list)/sizeof(pso); i++) *)
         let i = ref 0 in
         let matched = ref false in
         while (not !matched) && !i < Array.length pso_list do
           let name, ptype, value = pso_list.(!i) in
           let plen = String.length name in
           if
             patlen - !skipatstart - 2 >= plen
             && Parse.strncmp_c8_eq pattern (!skipatstart + 2) name plen
           then (
             matched := true;
             skipatstart := !skipatstart + plen + 2;
             match ptype with
             | Pso_opt ->
                 (* pcre2_compile.c:10326-10328 *)
                 cb.external_options <- cb.external_options lor value
             | Pso_flg ->
                 (* pcre2_compile.c:10330-10332 *)
                 setflags := !setflags lor value
             | Pso_nl ->
                 (* pcre2_compile.c:10334-10337 *)
                 newline := value;
                 setflags := !setflags lor nl_set
             | Pso_bsr ->
                 (* pcre2_compile.c:10339-10342 *)
                 bsr := value;
                 setflags := !setflags lor bsr_set
             | Pso_limh | Pso_limm | Pso_limd ->
                 (* pcre2_compile.c:10344-10370 *)
                 let c = ref 0 in
                 let pp = ref !skipatstart in
                 if not (is_digit_code (byte_at !pp)) then
                   raise_notrace (Had_error (Errors.err60, !pp));
                 let brk = ref false in
                 while (not !brk) && is_digit_code (byte_at !pp) do
                   (* c is uint32 in C; the pre-multiply guard
                      (UINT32_MAX/10 - 1) keeps c*10+9 < 2^32, so plain int
                      arithmetic cannot diverge from the C. *)
                   if !c > 429496728 then brk := true (* Integer overflow *)
                   else (
                     c := (!c * 10) + (byte_at !pp - Char.code '0');
                     incr pp)
                 done;
                 let ch = byte_at !pp in
                 incr pp (* ptr[pp++] *);
                 if not (Int.equal ch (Char.code ')')) then
                   raise_notrace (Had_error (Errors.err60, !pp));
                 (match ptype with
                 | Pso_limh -> limit_heap := !c
                 | Pso_limm -> limit_match := !c
                 | _ -> limit_depth := !c);
                 skipatstart := !pp (* skipatstart += pp - skipatstart *))
           else incr i
         done;
         (* pcre2_compile.c:10375 — out of the pso loop when no table
            entry matched. *)
         if not !matched then in_pso := false
       done);

    (* pcre2_compile.c:10381 — end of pattern-start options; advance to
       start of real regex. *)
    let skipatstart = !skipatstart in

    (* pcre2_compile.c:10385-10391 — ERR32 (no Unicode support) is N/A:
       this port is built with Unicode support (SUPPORT_UNICODE). *)

    (* pcre2_compile.c:10393-10417 — check UTF. We have the original
       options in 'options', with that value as modified by ( *UTF) etc in
       cb.external_options. *)
    let utf = not (Int.equal (cb.external_options land Options.utf) 0) in
    if utf then (
      if not (Int.equal (options land Options.never_utf) 0) then
        raise_notrace (Had_error (Errors.err74, skipatstart));
      (* pcre2_compile.c:10406-10408 — PRIV(valid_utf) over the pattern
         unless PCRE2_NO_UTF_CHECK. DEVIATION (M6 deferral,
         docs/ocaml-engine/07-utf8.md): the whole UTF-8 compile pipeline
         (valid_utf, GETCHARINC decoding in parse, ord2utf emission) is
         M6; until it lands, UTF compilation fails loudly here — before
         parse_regex can misread multi-byte characters bytewise — instead
         of guessing. The ERR91 surrogate-escapes check (10410-10416) is
         UTF-16 only, N/A. *)
      raise_notrace (Had_error (Parse.err_deferred, skipatstart)));

    (* pcre2_compile.c:10419-10426 — check UCP lockout. *)
    let ucp = not (Int.equal (cb.external_options land Options.ucp) 0) in
    if ucp && not (Int.equal (cb.external_options land Options.never_ucp) 0)
    then raise_notrace (Had_error (Errors.err75, skipatstart));

    (* pcre2_compile.c:10428-10430 — process the BSR setting. *)
    let bsr = if Int.equal !bsr 0 then ccontext.bsr_convention else !bsr in

    (* pcre2_compile.c:10432-10470 — process the newline setting. *)
    let newline =
      if Int.equal !newline 0 then ccontext.newline_convention else !newline
    in
    cb.nltype <- Parse.nltype_fixed;
    if Int.equal newline Options.newline_cr then (
      cb.nllen <- 1;
      cb.nl0 <- 0x0d (* CHAR_CR *))
    else if Int.equal newline Options.newline_lf then (
      cb.nllen <- 1;
      cb.nl0 <- 0x0a (* CHAR_NL *))
    else if Int.equal newline Options.newline_nul then (
      cb.nllen <- 1;
      cb.nl0 <- 0x00 (* CHAR_NUL *))
    else if Int.equal newline Options.newline_crlf then (
      cb.nllen <- 2;
      cb.nl0 <- 0x0d;
      cb.nl1 <- 0x0a)
    else if Int.equal newline Options.newline_any then
      cb.nltype <- Parse.nltype_any
    else if Int.equal newline Options.newline_anycrlf then
      cb.nltype <- Parse.nltype_anycrlf
    else raise_notrace (Had_error (Errors.err56, skipatstart));

    (* pcre2_compile.c:10472-10524 — pre-scan the pattern: size the parsed
       pattern (big32count is 32-bit-mode only), then do the parsing scan.
       parse_regex writes through the parse_context view of the compile
       block (the C shares one struct); the parse-owned fields are folded
       back into cb after the call. *)
    let pcx = Parse.make_context pattern in
    pcx.Parse.ptr <- skipatstart;
    pcx.Parse.external_options <- cb.external_options;
    pcx.Parse.extra_options <- ccontext.extra_options;
    pcx.Parse.parens_nest_limit <- ccontext.parens_nest_limit;
    pcx.Parse.nltype <- cb.nltype;
    pcx.Parse.nllen <- cb.nllen;
    pcx.Parse.nl0 <- cb.nl0;
    pcx.Parse.nl1 <- cb.nl1;
    Parse.allocate_parsed_pattern pcx ~options;

    let has_lookbehind = ref false in
    let errorcode =
      Parse.parse_regex pcx ~options:cb.external_options has_lookbehind
    in
    if not (Int.equal errorcode 0) then
      raise_notrace (Had_error (errorcode, pcx.Parse.erroroffset));

    cb.bracount <- pcx.Parse.bracount;
    cb.external_flags <- pcx.Parse.external_flags;
    cb.names_found <- pcx.Parse.names_found;
    cb.name_entry_size <- pcx.Parse.name_entry_size;
    cb.named_groups <- pcx.Parse.named_groups;
    cb.named_group_list_size <- pcx.Parse.named_group_list_size;
    cb.dupnames <- pcx.Parse.dupnames;
    cb.parsed_pattern <- pcx.Parse.parsed_pattern;
    cb.parsed_pattern_end <- pcx.Parse.parsed_pattern_end;
    Array.blit pcx.Parse.small_ref_offset 0 cb.small_ref_offset 0 10;

    (* pcre2_compile.c:10526-10553 — if there are any lookbehinds, scan
       the parsed pattern to figure out their lengths. Workspace is needed
       to remember whether numbered groups are or are not of limited
       length, and if limited, what the minimum and maximum lengths are.
       This caching saves re-computing the length of any group that is
       referenced more than once, which is particularly relevant when
       recursion is involved. Unnumbered groups do not have this exposure
       because they cannot be referenced. The vector must be initialized
       to zero.
       DEVIATION: the C keeps a 256-element default vector on the stack
       and only heap-allocates (with the ERR21 failure path) for larger
       group counts (10539-10549); here the exactly-sized zeroed vector is
       always allocated. *)
    if !has_lookbehind then (
      let loopcount = ref 0 in
      cb.groupinfo <- Array.make (2 * (cb.bracount + 1)) 0;
      let errorcode = check_lookbehinds cb 0 None None loopcount in
      if not (Int.equal errorcode 0) then
        raise_notrace (Had_error (errorcode, cb.erroroffset)));

    (* pcre2_compile.c:10575-10596 — pretend to compile the pattern while
       actually just accumulating the amount of memory required. On error,
       errorcode will be set non-zero, so we don't need to look at the
       result of the function. *)
    cb.erroroffset <- patlen (* For subsequent errors that do not set it *);
    let pptr = ref 0 in
    let code = ref 0 in
    Bytes.set cb.start_code 0 (Char.chr Opcodes.op_bra) (* 10590 *);
    let length = ref 1 (* 10142: allow for final END opcode *) in
    let errorcode = ref 0 in
    let firstcu = ref 0 and firstcuflags = ref 0 in
    let reqcu = ref 0 and reqcuflags = ref 0 in
    ignore
      (compile_regex cb.external_options ccontext.extra_options code pptr
         errorcode ~skipunits:0 firstcu firstcuflags reqcu reqcuflags None None
         cb (Some length));
    if not (Int.equal !errorcode 0) then
      raise_notrace (Had_error (!errorcode, cb.erroroffset));

    (* pcre2_compile.c:10598-10604 — this should be caught in
       compile_regex(), but just in case... *)
    if !length > Limits.max_pattern_size then
      raise_notrace (Had_error (Errors.err20, cb.erroroffset));

    (* pcre2_compile.c:10606-10619 — compute the size of the data block
       for the compiled pattern and names table (ERR101). DEVIATION:
       sizeof(pcre2_real_code) has no OCaml equivalent, so re_blocksize
       counts only the name-table and code units; the check is unreachable
       anyway because max_pattern_compiled_length is not exposed through
       this port's contexts (always PCRE2_UNSET). *)
    let re_blocksize = !length + (cb.names_found * cb.name_entry_size) in
    if re_blocksize > ccontext.max_pattern_compiled_length then
      raise_notrace (Had_error (Errors.err101, cb.erroroffset));

    (* pcre2_compile.c:10621-10658 — get and initialize the compiled
       pattern block. *)
    let re =
      {
        code = Bytes.make !length '\000';
        name_table = Bytes.make (cb.names_found * cb.name_entry_size) '\000';
        start_bitmap = Bytes.make 32 '\000' (* 10639 *);
        compile_options = options (* 10642 *);
        overall_options = cb.external_options (* 10643 *);
        extra_options = ccontext.extra_options (* 10644 *);
        (* 10645 — PCRE2_CODE_UNIT_WIDTH/8 = PCRE2_MODE8 for this 8-bit
           port. *)
        flags = mode8 lor cb.external_flags lor !setflags;
        limit_heap = !limit_heap (* 10646 *);
        limit_match = !limit_match (* 10647 *);
        limit_depth = !limit_depth (* 10648 *);
        first_codeunit = 0 (* 10649 *);
        last_codeunit = 0 (* 10650 *);
        bsr_convention = bsr (* 10651 *);
        newline_convention = newline (* 10652 *);
        max_lookbehind = 0 (* 10653 *);
        minlength = 0 (* 10654 *);
        top_bracket = 0 (* 10655 *);
        top_backref = 0 (* 10656 *);
        name_entry_size = cb.name_entry_size (* 10657 *);
        name_count = cb.names_found (* 10658 *);
      }
    in

    (* pcre2_compile.c:10666-10678 — update the compile data block for the
       actual compile: the name/number table and the code buffer now live
       in the compiled block. *)
    cb.parens_depth <- 0;
    cb.assert_depth <- 0;
    cb.lastcapture <- 0;
    cb.name_table <- re.name_table;
    cb.start_code <- re.code;
    cb.req_varyopt <- 0;
    cb.had_accept <- false;
    cb.had_pruneorskip <- false;

    (* pcre2_compile.c:10680-10688 — if any named groups were found,
       create the name/number table from the list created in the
       pre-pass. *)
    for i = 0 to cb.names_found - 1 do
      let ng = cb.named_groups.(i) in
      add_name_to_table cb ~name:ng.Parse.name ~length:ng.Parse.length
        ~groupno:ng.Parse.number ~tablecount:i
    done;

    (* pcre2_compile.c:10690-10710 — set up a starting, non-extracting
       bracket, then compile the expression. On error, errorcode will be
       set non-zero, so we don't need to look at the result of the
       function here. *)
    pptr := 0;
    code := 0;
    Bytes.set cb.start_code 0 (Char.chr Opcodes.op_bra);
    let regexrc =
      compile_regex re.overall_options ccontext.extra_options code pptr
        errorcode ~skipunits:0 firstcu firstcuflags reqcu reqcuflags None None
        cb None
    in
    if regexrc < 0 then re.flags <- re.flags lor match_empty;
    re.top_bracket <- cb.bracount;
    re.top_backref <- cb.top_backref;
    re.max_lookbehind <- cb.max_lookbehind;

    if cb.had_accept then (
      reqcu := 0 (* Must disable after ( *ACCEPT) *);
      reqcuflags := req_none;
      re.flags <- re.flags lor hasaccept (* Disables minimum length *));

    (* pcre2_compile.c:10712-10725 — fill in the final opcode and check
       for disastrous overflow (ERR23). DEVIATION: the bounds check
       precedes the OP_END write (the C writes into its oversized heap
       block and only then detects the overflow); the observable outcome —
       error 123 — is identical, and OCaml cannot write past the buffer.
       The blocksize shrink for usedlength < length is N/A: re.code keeps
       its allocated size, and the unused tail bytes are 0 = OP_END. *)
    let usedlength = !code + 1 in
    if usedlength > !length then errorcode := Errors.err23
    else Bytes.set re.code !code (Char.chr Opcodes.op_end);

    (* pcre2_compile.c:10727-10784 — scan the pattern for recursion/
       subroutine calls and convert the group numbers into offsets
       (find_recurse / PRIV(find_bracket), ERR53). DEFERRED (M5,
       docs/ocaml-engine/05-conditionals-recursion.md): parse_regex defers
       every recursion construct, so cb.had_recurse cannot be true yet;
       fail loudly rather than silently emitting unfixed offsets. *)
    if Int.equal !errorcode 0 && cb.had_recurse then
      errorcode := Parse.err_deferred;

    (* pcre2_compile.c:10794-10805 — unless PCRE2_NO_AUTO_POSSESS, check
       whether any single character iterators can be auto-possessified
       (PRIV(auto_possessify), ERR80). DEFERRED (M9,
       docs/ocaml-engine/10-performance.md) WITHOUT an error marker:
       auto-possession only rewrites quantifier opcodes into possessive
       variants when the following item cannot match the same character —
       it never changes what a pattern matches, only how fast failures are
       detected — and its only error, ERR80, means malformed bytecode
       (internal error). Compilation must succeed without it. *)

    (* pcre2_compile.c:10807-10809 — failed to compile, or error while
       post-processing. *)
    if not (Int.equal !errorcode 0) then
      raise_notrace (Had_error (!errorcode, cb.erroroffset));

    (* pcre2_compile.c:10811-10819 — successful compile. If the anchored
       option was not passed, set it if we can determine that the pattern
       is anchored by virtue of ^ characters or \A or anything else, such
       as starting with non-atomic .* when DOTALL is set. *)
    if
      Int.equal (re.overall_options land Options.anchored) 0
      && is_anchored re.code 0 0 cb 0 ~inassert:false
    then re.overall_options <- re.overall_options lor Options.anchored;

    (* pcre2_compile.c:10821-10956 — set up the first code unit or
       startline flag, the required code unit, and then study the pattern.
       This code need not be obeyed if PCRE2_NO_START_OPTIMIZE is set, as
       the data it would create will not be used. *)
    if Int.equal (re.overall_options land Options.no_start_optimize) 0 then (
      let minminlength = ref 0 in

      (* pcre2_compile.c:10832-10837 — if we do not have a first code
         unit, see if there is one that is asserted. *)
      if !firstcuflags >= req_none then
        firstcu := find_firstassertedcu re.code 0 firstcuflags 0;

      (* pcre2_compile.c:10839-10872 — save the data for a first code
         unit. The existence of one means the minimum length must be at
         least 1. *)
      if !firstcuflags < req_none then (
        re.first_codeunit <- !firstcu;
        re.flags <- re.flags lor firstset;
        incr minminlength;

        (* Handle caseless first code units. *)
        if not (Int.equal (!firstcuflags land req_caseless) 0) then
          if !firstcu < 128 || ((not utf) && (not ucp) && !firstcu < 255) then (
            if not (Int.equal (Chartables.fcc !firstcu) !firstcu) then
              re.flags <- re.flags lor firstcaseless)
          else if
            (* pcre2_compile.c:10861-10864 — SUPPORT_UNICODE, 8-bit
               width arm. *)
            ucp && (not utf)
            && not (Int.equal (Ucd.othercase !firstcu) !firstcu)
          then re.flags <- re.flags lor firstcaseless)
      else if
        (* pcre2_compile.c:10874-10882 — when there is no first code
           unit, for non-anchored patterns, see if we can set the
           PCRE2_STARTLINE flag. *)
        Int.equal (re.overall_options land Options.anchored) 0
        && is_startline re.code 0 0 cb 0 ~inassert:false
      then re.flags <- re.flags lor startline;

      (* pcre2_compile.c:10884-10935 — handle the "required code unit",
         if one is set. *)
      if !reqcuflags < req_none then (
        (* pcre2_compile.c:10896-10904 — 8-bit arm: in the UTF case we
           can increment the minimum length only if we are sure this
           really is a different character and not a non-starting code
           unit of the first character. *)
        if
          Int.equal (re.overall_options land Options.utf) 0 (* Not UTF *)
          || !firstcuflags >= req_none (* First not set *)
          || Int.equal (!firstcu land 0x80) 0 (* First is ASCII *)
          || Int.equal (!reqcu land 0x80) 0 (* Req is ASCII *)
        then incr minminlength;

        (* pcre2_compile.c:10906-10934 — in the case of an anchored
           pattern, set up the value only if it follows a variable length
           item in the pattern. *)
        if
          Int.equal (re.overall_options land Options.anchored) 0
          || not (Int.equal (!reqcuflags land req_vary) 0)
        then (
          re.last_codeunit <- !reqcu;
          re.flags <- re.flags lor lastset;

          (* Handle caseless required code units as for first code units
             (above). *)
          if not (Int.equal (!reqcuflags land req_caseless) 0) then
            if !reqcu < 128 || ((not utf) && (not ucp) && !reqcu < 255) then (
              if not (Int.equal (Chartables.fcc !reqcu) !reqcu) then
                re.flags <- re.flags lor lastcaseless)
            else if
              (* pcre2_compile.c:10924-10926 — SUPPORT_UNICODE, 8-bit
                 width arm. *)
              ucp && (not utf) && not (Int.equal (Ucd.othercase !reqcu) !reqcu)
            then re.flags <- re.flags lor lastcaseless));

      (* pcre2_compile.c:10937-10950 — study the compiled pattern:
         PRIV(study) fills re.start_bitmap (PCRE2_FIRSTMAPSET) and
         re.minlength, and its FIRSTMAPSET minminlength bump rides with
         it. DEFERRED (M5 owns the observable parts, M9 the rest;
         docs/ocaml-engine/06-verbs-k-start-opt.md) WITHOUT an error
         marker: both are start-of-match optimizations whose absence is
         the documented PCRE2_NO_START_OPTIMIZE behavior, and study's
         only error (ERR31) is "internal error: should not occur", so
         skipping cannot change compile error behavior. Until it lands,
         re.minlength is minminlength alone (lower than the C's studied
         value; unobservable through the engine boundary). *)

      (* pcre2_compile.c:10952-10955 — if the minimum length set (or not
         set) by study() is less than the minimum implied by required
         code units, override it. *)
      if re.minlength < !minminlength then re.minlength <- !minminlength);

    Ok re
  with Had_error (errorcode, erroroffset) -> Error (errorcode, erroroffset)

(* ---------- Inline sanity checks (module-initialization asserts) ---------- *)

(* PUT/GET and PUT2/GET2 roundtrips at the boundary values 0, 255, 256,
   65535, plus the exact big-endian byte layout and the INC cursor forms. *)
let () =
  let buf = Bytes.make 8 '\xaa' in
  List.iter
    (fun v ->
      put buf 0 v;
      assert (Int.equal (get buf 0) v);
      put2 buf 2 v;
      assert (Int.equal (get2 buf 2) v))
    [ 0; 255; 256; 65535 ];
  put buf 0 0x1234;
  assert (Char.equal (Bytes.get buf 0) '\x12');
  assert (Char.equal (Bytes.get buf 1) '\x34');
  let cursor = ref 0 in
  putinc buf cursor 65535;
  put2inc buf cursor 256;
  assert (Int.equal !cursor (Limits.link_size + Limits.imm2_size));
  assert (Int.equal (get buf 0) 65535);
  assert (Int.equal (get2 buf 2) 256)

(* Compile-context defaults (pcre2_context.c:133-145). *)
let () =
  assert (
    Int.equal default_compile_context.newline_convention Options.newline_lf);
  assert (Int.equal Options.newline_lf 2);
  assert (Int.equal default_compile_context.bsr_convention Options.bsr_unicode);
  assert (Int.equal Options.bsr_unicode 1);
  assert (Int.equal default_compile_context.parens_nest_limit 250);
  assert (Int.equal default_compile_context.max_varlookbehind 255);
  assert (Int.equal default_compile_context.extra_options 0)

(* make_block defaults (pcre2_compile.c:10238-10290) and the workspace
   overrun guard (pcre2_compile.c:5748-5759). *)
let () =
  let cb = make_block "abc" ~options:Options.caseless in
  assert (Int.equal cb.external_options Options.caseless);
  assert (Int.equal cb.end_pattern 3);
  assert (Int.equal compile_work_size 6000);
  assert (Int.equal cb.workspace_size compile_work_size);
  assert (Int.equal (Bytes.length cb.start_workspace) compile_work_size);
  assert (cb.start_code == cb.start_workspace) (* pre-compile aliasing *);
  assert (Int.equal cb.small_ref_offset.(9) Parse.pcre2_unset);
  assert (Int.equal cb.max_varlookbehind 255);
  assert (Int.equal cb.nltype Parse.nltype_fixed);
  assert (Int.equal (check_workspace_overflow cb 0) 0);
  assert (
    Int.equal
      (check_workspace_overflow cb
         (compile_work_size - work_size_safety_margin))
      0);
  assert (
    Int.equal
      (check_workspace_overflow cb
         (compile_work_size - work_size_safety_margin + 1))
      Errors.err86);
  assert (
    Int.equal (check_workspace_overflow cb (compile_work_size - 1)) Errors.err86);
  assert (Int.equal (check_workspace_overflow cb compile_work_size) Errors.err52);
  assert (Int.equal Errors.err52 152);
  assert (Int.equal Errors.err86 186)

(* first_significant_code on hand-built code fragments. *)
let () =
  (* OP_CREF is skipped whatever skipassert is: [CREF 2][CHAR 'x']. *)
  let buf = Bytes.make 8 '\000' in
  Bytes.set buf 0 (Char.chr Opcodes.op_cref);
  put2 buf 1 2;
  Bytes.set buf 3 (Char.chr Opcodes.op_char);
  Bytes.set buf 4 'x';
  assert (Int.equal (first_significant_code buf 0 ~skipassert:false) 3);
  assert (Int.equal (first_significant_code buf 0 ~skipassert:true) 3);
  (* \b is skipped only when skipassert is set. *)
  let buf = Bytes.make 4 '\000' in
  Bytes.set buf 0 (Char.chr Opcodes.op_word_boundary);
  Bytes.set buf 1 (Char.chr Opcodes.op_char);
  Bytes.set buf 2 'x';
  assert (Int.equal (first_significant_code buf 0 ~skipassert:false) 0);
  assert (Int.equal (first_significant_code buf 0 ~skipassert:true) 1);
  (* Two-branch negative lookahead, then 'y' — the do..while walks the
     OP_ALT chain to the final OP_KET and skips its length:
     [ASSERT_NOT L=5][CHAR x][ALT L=5][CHAR z][KET L=10][CHAR y]. *)
  let buf = Bytes.make 16 '\000' in
  Bytes.set buf 0 (Char.chr Opcodes.op_assert_not);
  put buf 1 5;
  Bytes.set buf 3 (Char.chr Opcodes.op_char);
  Bytes.set buf 4 'x';
  Bytes.set buf 5 (Char.chr Opcodes.op_alt);
  put buf 6 5;
  Bytes.set buf 8 (Char.chr Opcodes.op_char);
  Bytes.set buf 9 'z';
  Bytes.set buf 10 (Char.chr Opcodes.op_ket);
  put buf 11 10;
  Bytes.set buf 13 (Char.chr Opcodes.op_char);
  Bytes.set buf 14 'y';
  assert (Int.equal (first_significant_code buf 0 ~skipassert:true) 13);
  assert (Int.equal (first_significant_code buf 0 ~skipassert:false) 0)

(* find_dupname_details on a hand-built name table over pattern "aabbcc":
   entries "aa"->1, "aa"->3, "bb"->2 (entry size IMM2_SIZE + 2 + 1 = 5). *)
let () =
  let cb = make_block "aabbcc" ~options:0 in
  cb.name_entry_size <- Limits.imm2_size + 2 + 1;
  cb.names_found <- 3;
  cb.name_table <- Bytes.make (3 * cb.name_entry_size) '\000';
  let set_entry i group nm =
    let base = i * cb.name_entry_size in
    put2 cb.name_table base group;
    Bytes.blit_string nm 0 cb.name_table (base + Limits.imm2_size)
      (String.length nm)
  in
  set_entry 0 1 "aa";
  set_entry 1 3 "aa";
  set_entry 2 2 "bb";
  let index = ref (-1) in
  let count = ref (-1) in
  let errorcode = ref 0 in
  (* "aa" (pattern offset 0): first index 0, two duplicates; backref map
     and top_backref updated (pcre2_compile.c:5586-5596). *)
  assert (find_dupname_details ~name:0 ~length:2 index count errorcode cb);
  assert (Int.equal !index 0);
  assert (Int.equal !count 2);
  assert (Int.equal cb.backref_map ((1 lsl 1) lor (1 lsl 3)));
  assert (Int.equal cb.top_backref 3);
  (* "bb" (pattern offset 2): index 2, a single entry. *)
  assert (find_dupname_details ~name:2 ~length:2 index count errorcode cb);
  assert (Int.equal !index 2);
  assert (Int.equal !count 1);
  assert (Int.equal cb.top_backref 3);
  (* "cc" (pattern offset 4) is not in the table: internal error ERR53 with
     erroroffset at the name (pcre2_compile.c:5570-5578). *)
  assert (not (find_dupname_details ~name:4 ~length:2 index count errorcode cb));
  assert (Int.equal !errorcode Errors.err53);
  assert (Int.equal cb.erroroffset 4)

(* pcre2_compile.c:8101-8105 — "the ESC_values are arranged to be the same
   as the corresponding OP_values": META_ESCAPE emission (compile_branch)
   relies on this identity, so pin every pair that can reach it. ESC_dum
   pads the enum so the alignment holds across OP_ALLANY. *)
let () =
  assert (Int.equal Parse.esc_big_a Opcodes.op_sod);
  assert (Int.equal Parse.esc_big_g Opcodes.op_som);
  assert (Int.equal Parse.esc_big_k Opcodes.op_set_som);
  assert (Int.equal Parse.esc_big_b Opcodes.op_not_word_boundary);
  assert (Int.equal Parse.esc_b Opcodes.op_word_boundary);
  assert (Int.equal Parse.esc_big_d Opcodes.op_not_digit);
  assert (Int.equal Parse.esc_d Opcodes.op_digit);
  assert (Int.equal Parse.esc_big_s Opcodes.op_not_whitespace);
  assert (Int.equal Parse.esc_s Opcodes.op_whitespace);
  assert (Int.equal Parse.esc_big_w Opcodes.op_not_wordchar);
  assert (Int.equal Parse.esc_w Opcodes.op_wordchar);
  assert (Int.equal Parse.esc_big_n Opcodes.op_any);
  assert (Int.equal Parse.esc_dum Opcodes.op_allany);
  assert (Int.equal Parse.esc_big_c Opcodes.op_anybyte);
  assert (Int.equal Parse.esc_big_p Opcodes.op_notprop);
  assert (Int.equal Parse.esc_p Opcodes.op_prop);
  assert (Int.equal Parse.esc_big_r Opcodes.op_anynl);
  assert (Int.equal Parse.esc_big_h Opcodes.op_not_hspace);
  assert (Int.equal Parse.esc_h Opcodes.op_hspace);
  assert (Int.equal Parse.esc_big_v Opcodes.op_not_vspace);
  assert (Int.equal Parse.esc_v Opcodes.op_vspace);
  assert (Int.equal Parse.esc_big_x Opcodes.op_extuni);
  assert (Int.equal Parse.esc_big_z Opcodes.op_eodn);
  assert (Int.equal Parse.esc_z Opcodes.op_eod)

(* Repeat tables: chartypeoffset (pcre2_compile.c:687-691) and
   opcode_possessify (pcre2_compile.c:861-917) — length (OP_END..OP_CALLOUT
   inclusive) and the entries the repeat arm relies on. *)
let () =
  assert (Int.equal (Array.length chartypeoffset) 4);
  assert (Int.equal chartypeoffset.(0) 0);
  assert (Int.equal chartypeoffset.(1) 13);
  assert (Int.equal chartypeoffset.(2) 26);
  assert (Int.equal chartypeoffset.(3) 39);
  assert (Int.equal (Array.length opcode_possessify) (Opcodes.op_callout + 1));
  assert (Int.equal opcode_possessify.(Opcodes.op_star) Opcodes.op_posstar);
  assert (Int.equal opcode_possessify.(Opcodes.op_minstar) 0);
  assert (Int.equal opcode_possessify.(Opcodes.op_upto) Opcodes.op_posupto);
  assert (Int.equal opcode_possessify.(Opcodes.op_exact) 0);
  assert (Int.equal opcode_possessify.(Opcodes.op_stari) Opcodes.op_posstari);
  assert (Int.equal opcode_possessify.(Opcodes.op_notstar) Opcodes.op_notposstar);
  assert (
    Int.equal opcode_possessify.(Opcodes.op_notuptoi) Opcodes.op_notposuptoi);
  assert (
    Int.equal opcode_possessify.(Opcodes.op_typestar) Opcodes.op_typeposstar);
  assert (Int.equal opcode_possessify.(Opcodes.op_crrange) Opcodes.op_crposrange);
  assert (Int.equal opcode_possessify.(Opcodes.op_crposstar) 0);
  assert (Int.equal opcode_possessify.(Opcodes.op_class) 0);
  assert (Int.equal opcode_possessify.(Opcodes.op_ref) 0);
  assert (Int.equal opcode_possessify.(Opcodes.op_recurse) 0)

(* compile_branch on parsed streams produced by Parse.parse_regex, driven
   with pcre2_compile()'s two-phase protocol (pre-compile pass accumulating
   the length into the workspace, then the real pass into a buffer of
   exactly that size), reduced to a single branch. Every expected byte
   sequence and first/required-code-unit outcome below was traced against
   pcre2_compile.c:5636-8336. *)
let () =
  let parse ?(options = 0) ?(extra = 0) pat : Parse.parse_context =
    let cx = Parse.make_context pat in
    cx.Parse.external_options <- options;
    cx.Parse.extra_options <- extra;
    Parse.allocate_parsed_pattern cx ~options;
    let has_lookbehind = ref false in
    let prc = Parse.parse_regex cx ~options has_lookbehind in
    assert (Int.equal prc 0);
    cx
  in
  let make_cb ?(options = 0) pat (cx : Parse.parse_context) : compile_block =
    let cb = make_block pat ~options in
    cb.parsed_pattern <- cx.Parse.parsed_pattern;
    cb.parsed_pattern_end <- cx.Parse.parsed_pattern_end;
    cb
  in
  (* One compile_branch call from parsed-pattern index 0 / code offset 0.
     Returns (rc, errorcode, end code offset, end pptr, options out,
     firstcu, firstcuflags, reqcu, reqcuflags). *)
  let run_branch ?(options = 0) ?(extra = 0) (cb : compile_block) lengthptr =
    let optionsptr = ref options and xoptionsptr = ref extra in
    let codeptr = ref 0 and pptrptr = ref 0 and errorcodeptr = ref 0 in
    let firstcuptr = ref 0 and firstcuflagsptr = ref 0 in
    let reqcuptr = ref 0 and reqcuflagsptr = ref 0 in
    let rc =
      compile_branch optionsptr xoptionsptr codeptr pptrptr errorcodeptr
        firstcuptr firstcuflagsptr reqcuptr reqcuflagsptr None None cb lengthptr
    in
    ( rc,
      !errorcodeptr,
      !codeptr,
      !pptrptr,
      !optionsptr,
      !firstcuptr,
      !firstcuflagsptr,
      !reqcuptr,
      !reqcuflagsptr )
  in
  (* Two-pass driver: asserts the pre-compile length equals the real pass's
     emitted length (this catches most two-pass bugs), and that both passes
     agree on the return code. *)
  let compile2 ?(options = 0) ?(extra = 0) pat =
    let cx = parse ~options ~extra pat in
    let cb = make_cb ~options pat cx in
    let length = ref 0 in
    let rc1, err1, _, _, _, _, _, _, _ =
      run_branch ~options ~extra cb (Some length)
    in
    assert (Int.equal err1 0);
    cb.start_code <- Bytes.make !length '\000' (* real-phase buffer *);
    cb.req_varyopt <- 0 (* pcre2_compile.c:10676, between the phases *);
    let rc2, err2, endcode, endpptr, opts_out, fcu, fcuf, rcu, rcuf =
      run_branch ~options ~extra cb None
    in
    assert (Int.equal err2 0);
    assert (Int.equal rc1 rc2);
    assert (Int.equal endcode !length);
    (cb, rc2, endpptr, opts_out, fcu, fcuf, rcu, rcuf)
  in
  let assert_code (cb : compile_block) (expected : int list) =
    assert (Int.equal (Bytes.length cb.start_code) (List.length expected));
    List.iteri
      (fun i v -> assert (Int.equal (Char.code (Bytes.get cb.start_code i)) v))
      expected
  in
  (* Deferred arms fail loudly with Parse.err_deferred, identically in the
     pre-compile and real phases. *)
  let expect_deferred ?(options = 0) ?(extra = 0) pat =
    let cx = parse ~options ~extra pat in
    let cb = make_cb ~options pat cx in
    let rc, err, _, _, _, _, _, _, _ =
      run_branch ~options ~extra cb (Some (ref 0))
    in
    assert (Int.equal rc 0);
    assert (Int.equal err Parse.err_deferred);
    let cb = make_cb ~options pat cx in
    cb.start_code <- Bytes.make 64 '\000';
    let rc, err, _, _, _, _, _, _, _ = run_branch ~options ~extra cb None in
    assert (Int.equal rc 0);
    assert (Int.equal err Parse.err_deferred)
  in

  (* Literals: OP_CHAR chain, firstcu/reqcu protocol, and the
     must-match-a-character return +1 (pcre2_compile.c:5800-5804,
     8226-8336). *)
  let cb, rc, endpptr, _, fcu, fcuf, rcu, rcuf = compile2 "abc" in
  assert (Int.equal rc 1);
  assert_code cb
    [ Opcodes.op_char; 0x61; Opcodes.op_char; 0x62; Opcodes.op_char; 0x63 ];
  assert (Int.equal cb.parsed_pattern.(endpptr) Parse.meta_end);
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x63);
  assert (Int.equal rcuf 0);

  (* Empty branch: may match an empty string (return -1), zero length. *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "" in
  assert (Int.equal rc (-1));
  assert_code cb [];
  assert (Int.equal fcuf req_unset);
  assert (Int.equal rcuf req_unset);

  (* A branch stops at META_ALT, leaving pptr on it
     (pcre2_compile.c:5816-5825). *)
  let cb, rc, endpptr, _, _, _, _, _ = compile2 "a|b" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_char; 0x61 ];
  assert (Int.equal cb.parsed_pattern.(endpptr) Parse.meta_alt);

  (* Caseless: OP_CHARI and the REQ_CASELESS flag, both from the options
     argument and from an in-pattern (?i) via META_OPTIONS
     (pcre2_compile.c:5713-5719, 6581-6587, 8274). *)
  let cb, _, _, _, fcu, fcuf, rcu, rcuf =
    compile2 ~options:Options.caseless "ab"
  in
  assert_code cb [ Opcodes.op_chari; 0x61; Opcodes.op_chari; 0x62 ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf req_caseless);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf req_caseless);
  let cb, _, _, opts_out, fcu, fcuf, _, rcuf = compile2 "(?i)x" in
  assert_code cb [ Opcodes.op_chari; 0x78 ];
  assert (Int.equal opts_out Options.caseless) (* passed back to caller *);
  assert (Int.equal fcu 0x78);
  assert (Int.equal fcuf req_caseless);
  assert (Int.equal rcuf req_unset) (* single char sets no reqcu *);
  let cb, _, _, opts_out, fcu, fcuf, rcu, rcuf = compile2 "(?i)a(?-i)b" in
  assert_code cb [ Opcodes.op_chari; 0x61; Opcodes.op_char; 0x62 ];
  assert (Int.equal opts_out 0);
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf req_caseless);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0) (* req_caseopt was reset by (?-i) *);

  (* META_DOT: OP_ANY, or OP_ALLANY under PCRE2_DOTALL; '.' first means no
     first code unit (pcre2_compile.c:5846-5857). *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "." in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_any ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, _, _, _ = compile2 "(?s)." in
  assert_code cb [ Opcodes.op_allany ];
  let cb, _, _, _, _, _, _, _ = compile2 ~options:Options.dotall "." in
  assert_code cb [ Opcodes.op_allany ];

  (* Anchors ^ $ and their multiline variants; non-multiline ^ leaves
     firstcu alone, OP_CIRCM disables it (pcre2_compile.c:5828-5844). *)
  let cb, rc, _, _, fcu, fcuf, _, rcuf = compile2 "^a$" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_circ; Opcodes.op_char; 0x61; Opcodes.op_doll ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, fcuf, rcu, rcuf =
    compile2 ~options:Options.multiline "^a$"
  in
  assert_code cb [ Opcodes.op_circm; Opcodes.op_char; 0x61; Opcodes.op_dollm ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcu 0x61) (* firstcu disabled, so 'a' becomes reqcu *);
  assert (Int.equal rcuf 0);

  (* Simple escapes: ESC value = OP value; \d-class escapes consume a
     character (rc +1) and disable firstcu (pcre2_compile.c:8113-8124,
     8205). *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "\\d\\W" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_digit; Opcodes.op_not_wordchar ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, _, _, _ = compile2 "\\h\\H\\v\\V\\N\\X" in
  assert_code cb
    [
      Opcodes.op_hspace;
      Opcodes.op_not_hspace;
      Opcodes.op_vspace;
      Opcodes.op_not_vspace;
      Opcodes.op_any;
      Opcodes.op_extuni;
    ];

  (* Zero-width escapes: no matched_char (rc -1); \b/\B/\A register a
     one-character lookbehind (pcre2_compile.c:8179-8202). *)
  let cb, rc, _, _, _, fcuf, _, _ = compile2 "\\A\\G\\K\\b\\B\\Z\\z" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_sod;
      Opcodes.op_som;
      Opcodes.op_set_som;
      Opcodes.op_word_boundary;
      Opcodes.op_not_word_boundary;
      Opcodes.op_eodn;
      Opcodes.op_eod;
    ];
  assert (Int.equal fcuf req_unset);
  assert (Int.equal cb.max_lookbehind 1);

  (* \R -> OP_ANYNL; \C -> OP_ALLANY in non-UTF mode, recording
     PCRE2_HASBKC (pcre2_compile.c:8184-8191). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "\\R\\C" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_anynl; Opcodes.op_allany ];
  assert (Int.equal (cb.external_flags land hasbkc) hasbkc);

  (* \b/\B under PCRE2_UCP become the UCP word-boundary opcodes unless
     PCRE2_EXTRA_ASCII_BSW (pcre2_compile.c:8193-8198). *)
  let cb, _, _, _, _, _, _, _ = compile2 ~options:Options.ucp "\\b\\B" in
  assert_code cb
    [ Opcodes.op_ucp_word_boundary; Opcodes.op_not_ucp_word_boundary ];
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:Options.ucp ~extra:Options.extra_ascii_bsw "\\b\\B"
  in
  assert_code cb [ Opcodes.op_word_boundary; Opcodes.op_not_word_boundary ];

  (* Explicit \r / \n set PCRE2_HASCRORLF (pcre2_compile.c:8278-8281). *)
  let cb, _, _, _, _, _, _, _ = compile2 "\n" in
  assert_code cb [ Opcodes.op_char; 0x0a ];
  assert (Int.equal (cb.external_flags land hascrorlf) hascrorlf);
  let cb, _, _, _, _, _, _, _ = compile2 "\\r" in
  assert_code cb [ Opcodes.op_char; 0x0d ];
  assert (Int.equal (cb.external_flags land hascrorlf) hascrorlf);

  (* PCRE2_AUTO_CALLOUT: OP_CALLOUT items (1 + 2*LINK_SIZE + 1 units:
     pattern offset, item length, callout number 255) around each item
     (pcre2_compile.c:7101-7111). *)
  let cb, rc, _, _, _, _, _, _ = compile2 ~options:Options.auto_callout "ab" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_callout;
      0;
      0;
      0;
      1;
      255;
      Opcodes.op_char;
      0x61;
      Opcodes.op_callout;
      0;
      1;
      0;
      1;
      255;
      Opcodes.op_char;
      0x62;
      Opcodes.op_callout;
      0;
      2;
      0;
      0;
      255;
    ];

  (* \K inside an assertion: ERR99 unless PCRE2_EXTRA_ALLOW_LOOKAROUND_BSK
     (pcre2_compile.c:8159-8167). assert_depth is maintained by the
     lookaround arms (M4); simulate it directly. *)
  let cx = parse "\\K" in
  let cb = make_cb "\\K" cx in
  cb.assert_depth <- 1;
  let rc, err, _, _, _, _, _, _, _ = run_branch cb (Some (ref 0)) in
  assert (Int.equal rc 0);
  assert (Int.equal err Errors.err99);
  let cb = make_cb "\\K" cx in
  cb.assert_depth <- 1;
  let rc, err, _, _, _, _, _, _, _ =
    run_branch ~extra:Options.extra_allow_lookaround_bsk cb (Some (ref 0))
  in
  assert (Int.equal rc (-1));
  assert (Int.equal err 0);

  (* --- Character classes (compile_branch B, pcre2_compile.c:5860-6484,
     helpers 5206-5530). assert_class checks the opcode and all 256 bitmap
     bits of an emitted 33-unit bitmap class against a membership
     predicate. *)
  let assert_class (cb : compile_block) (expected_op : int)
      (member : int -> bool) =
    assert (Int.equal (Bytes.length cb.start_code) 33);
    assert (Int.equal (Char.code (Bytes.get cb.start_code 0)) expected_op);
    for c = 0 to 255 do
      let bit =
        Char.code (Bytes.get cb.start_code (1 + (c lsr 3)))
        land (1 lsl (c land 7))
      in
      assert (Bool.equal (not (Int.equal bit 0)) (member c))
    done
  in
  let is_digit c = c >= 0x30 && c <= 0x39 in
  let is_letter c = (c >= 0x41 && c <= 0x5a) || (c >= 0x61 && c <= 0x7a) in

  (* Empty classes under PCRE2_ALLOW_EMPTY_CLASS: [] always fails (OP_FAIL),
     [^] matches anything (OP_ALLANY) (pcre2_compile.c:5860-5873). *)
  let cb, rc, _, _, _, fcuf, _, rcuf =
    compile2 ~options:Options.allow_empty_class "[]"
  in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_fail ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:Options.allow_empty_class "[^]"
  in
  assert_code cb [ Opcodes.op_allany ];

  (* One-char classes: positive is a normal literal via NORMAL_CHAR_SET
     (pcre2_compile.c:5911-5915); negative is OP_NOT/OP_NOTI with no first
     code unit (5917-5946). *)
  let cb, rc, _, _, fcu, fcuf, _, _ = compile2 "[x]" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_char; 0x78 ];
  assert (Int.equal fcu 0x78);
  assert (Int.equal fcuf 0);
  let cb, _, _, _, fcu, fcuf, _, _ = compile2 "(?i)[x]" in
  assert_code cb [ Opcodes.op_chari; 0x78 ];
  assert (Int.equal fcu 0x78);
  assert (Int.equal fcuf req_caseless);
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "[^x]" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_not; 0x78 ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, _, _, _ = compile2 "(?i)[^x]" in
  assert_code cb [ Opcodes.op_noti; 0x78 ];
  (* No UCD caseset check for OP_NOTI without UTF/UCP: (?i)[^k] stays a
     plain OP_NOTI (pcre2_compile.c:5930-5931 requires utf||ucp). *)
  let cb, _, _, _, _, _, _, _ = compile2 "(?i)[^k]" in
  assert_code cb [ Opcodes.op_noti; 0x6b ];
  (* One-char class + \r/\n flag via the literal path. *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\r]" in
  assert_code cb [ Opcodes.op_char; 0x0d ];
  assert (Int.equal (cb.external_flags land hascrorlf) hascrorlf);

  (* Two-char case-partner classes become caseless single characters via
     CLASS_CASELESS_CHAR, with the temporary caseless setting reset
     afterwards (pcre2_compile.c:5949-5992, 8327-8334). *)
  let cb, rc, _, opts_out, fcu, fcuf, rcu, rcuf = compile2 "[aA]b" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_chari; 0x61; Opcodes.op_char; 0x62 ];
  assert (Int.equal opts_out 0) (* caseless was only instated temporarily *);
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf req_caseless);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0);
  let cb, _, _, _, _, _, _, _ = compile2 "[Aa]" in
  assert_code cb [ Opcodes.op_chari; 0x41 ];
  (* k/K (and s/S) have multi-character caseless sets (0x212a / 0x17f), so
     UCD_CASESET(c) != 0 blocks the optimization and a bitmap class is
     compiled (pcre2_compile.c:5962) — unless PCRE2_EXTRA_CASELESS_RESTRICT
     applies with both chars ASCII (5963-5964). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[kK]" in
  assert_class cb Opcodes.op_class (fun c ->
      Int.equal c 0x6b || Int.equal c 0x4b);
  let cb, _, _, _, _, _, _, _ =
    compile2 ~extra:Options.extra_caseless_restrict "[kK]"
  in
  assert_code cb [ Opcodes.op_chari; 0x6b ];
  let cb, _, _, _, _, _, _, _ = compile2 "[sS]" in
  assert_class cb Opcodes.op_class (fun c ->
      Int.equal c 0x73 || Int.equal c 0x53);

  (* General bitmap classes (pcre2_compile.c:6021-6371, 6467-6484):
     literals, ranges, negation (OP_NCLASS + inverted map), the caseless
     fcc closure, class escapes, and POSIX tables. *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "[abc]" in
  assert (Int.equal rc 1);
  assert_class cb Opcodes.op_class (fun c -> c >= 0x61 && c <= 0x63);
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, _, _, _ = compile2 "[^abc]" in
  assert_class cb Opcodes.op_nclass (fun c -> not (c >= 0x61 && c <= 0x63));
  let cb, _, _, _, _, _, _, _ = compile2 "[a-z]" in
  assert_class cb Opcodes.op_class (fun c -> c >= 0x61 && c <= 0x7a);
  (* Caseless closure of a range via the fcc table
     (pcre2_compile.c:5288-5294). *)
  let cb, _, _, _, _, _, _, _ = compile2 "(?i)[a-z]" in
  assert_class cb Opcodes.op_class is_letter;
  let cb, _, _, _, _, _, _, _ = compile2 "(?i)[^a-k]" in
  assert_class cb Opcodes.op_nclass (fun c ->
      not ((c >= 0x61 && c <= 0x6b) || (c >= 0x41 && c <= 0x4b)));
  (* An escaped range endpoint (META_RANGE_ESCAPED) fills the same bits. *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\x41-\\x5a]" in
  assert_class cb Opcodes.op_class (fun c -> c >= 0x41 && c <= 0x5a);
  (* Explicit \n inside a range sets PCRE2_HASCRORLF
     (pcre2_compile.c:6274-6291). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\n-\\r]" in
  assert_class cb Opcodes.op_class (fun c -> c >= 0x0a && c <= 0x0d);
  assert (Int.equal (cb.external_flags land hascrorlf) hascrorlf);

  (* Class escapes: \d ORs the digit map in; \D ORs its complement and
     flips the negation logic, so [\D] and [^\d] are both OP_NCLASS while
     [^\D] collapses back to OP_CLASS of the digits
     (pcre2_compile.c:6175-6183, 6473-6482). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\d]" in
  assert_class cb Opcodes.op_class is_digit;
  let cb, _, _, _, _, _, _, _ = compile2 "[\\D]" in
  assert_class cb Opcodes.op_nclass (fun c -> not (is_digit c));
  let cb, _, _, _, _, _, _, _ = compile2 "[^\\d]" in
  assert_class cb Opcodes.op_nclass (fun c -> not (is_digit c));
  let cb, _, _, _, _, _, _, _ = compile2 "[^\\D]" in
  assert_class cb Opcodes.op_class is_digit;
  let cb, _, _, _, _, _, _, _ = compile2 "[\\w]" in
  assert_class cb Opcodes.op_class (fun c ->
      is_letter c || is_digit c || Int.equal c 0x5f);
  let cb, _, _, _, _, _, _, _ = compile2 "[\\s]" in
  assert_class cb Opcodes.op_class (fun c ->
      (c >= 0x09 && c <= 0x0d) || Int.equal c 0x20);
  let cb, _, _, _, _, _, _, _ = compile2 "[^\\S]" in
  assert_class cb Opcodes.op_class (fun c ->
      (c >= 0x09 && c <= 0x0d) || Int.equal c 0x20);
  (* \h/\H use the [hv]space lists; wide list entries are clamped away in
     non-UTF 8-bit mode (pcre2_compile.c:6218-6238, 5297-5302). \H does NOT
     set should_flip_negation — its complement is added explicitly. *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\h]" in
  assert_class cb Opcodes.op_class (fun c ->
      Int.equal c 0x09 || Int.equal c 0x20 || Int.equal c 0xa0);
  let cb, _, _, _, _, _, _, _ = compile2 "[\\H]" in
  assert_class cb Opcodes.op_class (fun c ->
      not (Int.equal c 0x09 || Int.equal c 0x20 || Int.equal c 0xa0));
  let cb, _, _, _, _, _, _, _ = compile2 "[\\v]" in
  assert_class cb Opcodes.op_class (fun c ->
      (c >= 0x0a && c <= 0x0d) || Int.equal c 0x85);
  let cb, _, _, _, _, _, _, _ = compile2 "[^\\v]" in
  assert_class cb Opcodes.op_nclass (fun c ->
      not ((c >= 0x0a && c <= 0x0d) || Int.equal c 0x85));

  (* POSIX classes: base map +/- second map + tweaks
     (pcre2_compile.c:6098-6142, posix_class_maps 715-740). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[[:alpha:]]" in
  assert_class cb Opcodes.op_class is_letter;
  let cb, _, _, _, _, _, _, _ = compile2 "[[:^alpha:]]" in
  assert_class cb Opcodes.op_nclass (fun c -> not (is_letter c));
  let cb, _, _, _, _, _, _, _ = compile2 "[^[:^alpha:]]" in
  assert_class cb Opcodes.op_class is_letter;
  let cb, _, _, _, _, _, _, _ = compile2 "[[:alnum:]]" in
  assert_class cb Opcodes.op_class (fun c -> is_letter c || is_digit c);
  let cb, _, _, _, _, _, _, _ = compile2 "[[:word:]]" in
  assert_class cb Opcodes.op_class (fun c ->
      is_letter c || is_digit c || Int.equal c 0x5f);
  (* [:blank:] = space map minus vertical space (tweak 1). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[[:blank:]]" in
  assert_class cb Opcodes.op_class (fun c ->
      Int.equal c 0x09 || Int.equal c 0x20);
  (* Caseless [:upper:]/[:lower:] convert to alpha
     (pcre2_compile.c:6043-6048). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[[:upper:]]" in
  assert_class cb Opcodes.op_class (fun c -> c >= 0x41 && c <= 0x5a);
  let cb, _, _, _, _, _, _, _ = compile2 "(?i)[[:upper:]]" in
  assert_class cb Opcodes.op_class is_letter;
  (* [:ascii:] = print + cntrl maps. *)
  let cb, _, _, _, _, _, _, _ = compile2 "[[:ascii:]]" in
  assert_class cb Opcodes.op_class (fun c -> c <= 0x7f);
  let cb, _, _, _, _, _, _, _ = compile2 "[^[:ascii:]]" in
  assert_class cb Opcodes.op_nclass (fun c -> c > 0x7f);

  (* Mixed class content ORs together. *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\dA-Fa-f]" in
  assert_class cb Opcodes.op_class (fun c ->
      is_digit c || (c >= 0x41 && c <= 0x46) || (c >= 0x61 && c <= 0x66));

  (* --- Repeats (compile_branch C, pcre2_compile.c:7178-8011). The
     quantifier arm abolishes and rewrites a previous single-char or
     character-type item (OUTPUT_SINGLE_REPEAT, 7784-7904), appends a CR
     opcode after a class (7311-7344), and the possessive pass (7909-8003)
     then switches the repeat opcode to its POS variant via
     opcode_possessify. compile2 also checks the two-pass protocol: the
     pre-compile length must equal the real phase's emitted length. *)

  (* Hand-verified trace 1 — "a*" (pcre2_compile.c:7189-7194, 7215,
     7220-7226, 7794-7795, 7809-7811, 7888-7894): OP_CHAR 0x61 at offset 0
     is abolished (code = previous, 7795); min=0/max=unlimited emits
     OP_STAR + repeat_type(greedy_default=0) + op_type(0) = 33 at offset 0
     (7811 after 7804); the final fill re-emits 0x61 at offset 1
     (7890-7894). The zero-minimum repeat backs off to the zero* values
     (7220-7226): firstcuflags = zerofirstcuflags = REQ_NONE (set when 'a'
     was first, 8285), so the branch may match empty (rc -1); END_REPEAT
     ORs REQ_VARY into req_varyopt (8010). *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "a*" in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_star; 0x61 ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  assert (Int.equal cb.req_varyopt req_vary);

  (* Lazy quantifiers take repeat_type = greedy_non_default = 1
     (pcre2_compile.c:7240-7246): a+? = OP_MINPLUS, a?? = OP_MINQUERY.
     a+? must match at least one char (7210). *)
  let cb, rc, _, _, fcu, fcuf, _, rcuf = compile2 "a+?" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_minplus; 0x61 ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcuf req_unset);
  let cb, rc, _, _, _, _, _, _ = compile2 "a??" in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_minquery; 0x61 ];

  (* Hand-verified trace 2 — "a{2,5}" (pcre2_compile.c:7182-7187,
     7293-7308, 7838-7884, 7888-7894): min=2/max=5 from the two words
     after META_MINMAX. repeat_min > 1 sets reqcu = 0x61 with reqcuflags =
     cb->req_varyopt = 0 (7302-7305). {n,m} = EXACT then UPTO: OP_EXACT +
     op_type(0) = 41, PUT2(2) (7843-7844); fill 0x61 (7853-7857);
     repeat_max = 5-2 = 3 != 1, so OP_UPTO + repeat_type(0) = 39, PUT2(3)
     (7874-7883); final fill 0x61 (7890-7894). *)
  let cb, rc, _, _, fcu, fcuf, rcu, rcuf = compile2 "a{2,5}" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_exact; 0; 2; 0x61; Opcodes.op_upto; 0; 3; 0x61 ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x61);
  assert (Int.equal rcuf 0);
  assert (Int.equal cb.req_varyopt req_vary);

  (* "a{3,}": EXACT 3 then STAR (pcre2_compile.c:7843-7844, 7851-7857,
     7870-7871, 7890-7894). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{3,}" in
  assert_code cb [ Opcodes.op_exact; 0; 3; 0x61; Opcodes.op_star; 0x61 ];

  (* "a{4}" = {n,n}: just an EXACT (pcre2_compile.c:7838-7844 with the
     7851 middle skipped); reqvary = 0 (7215) leaves req_varyopt alone. *)
  let cb, rc, _, _, fcu, _, rcu, rcuf = compile2 "a{4}" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_exact; 0; 4; 0x61 ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal rcu 0x61);
  assert (Int.equal rcuf 0);
  assert (Int.equal cb.req_varyopt 0);

  (* "a{1,3}": min=1/max limited leaves the char item in place and appends
     OP_UPTO with max-1 (pcre2_compile.c:7825-7835), then the final fill
     (7890-7894). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{1,3}" in
  assert_code cb [ Opcodes.op_char; 0x61; Opcodes.op_upto; 0; 2; 0x61 ];

  (* "a{0,3}": zero minimum with a limited maximum is an UPTO
     (pcre2_compile.c:7813-7817). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{0,3}" in
  assert_code cb [ Opcodes.op_upto; 0; 3; 0x61 ];

  (* "a{1}" = {1,1}: the quantifier is ignored for non-parenthesized items
     (pcre2_compile.c:7277). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "a{1}" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_char; 0x61 ];
  assert (Int.equal cb.req_varyopt 0);

  (* Hand-verified trace 3 — "(?i)a*" (pcre2_compile.c:7278, 689-691):
     previous is OP_CHARI, so op_type = chartypeoffset[OP_CHARI - OP_CHAR]
     = OP_STARI - OP_STAR = 13, and 7811 emits OP_STAR + 0 + 13 = 46 =
     OP_STARI, then the fill 0x61. *)
  let cb, _, _, _, _, fcuf, _, _ = compile2 "(?i)a*" in
  assert_code cb [ Opcodes.op_stari; 0x61 ];
  assert (Int.equal fcuf req_none);

  (* (?i)x{3}: OP_EXACT + op_type(13) = OP_EXACTI (7843, "NB EXACT doesn't
     have repeat_type"); OP_CHARI previous adds REQ_CASELESS to the reqcu
     flags (7306). *)
  let cb, _, _, _, fcu, fcuf, rcu, rcuf = compile2 "(?i)x{3}" in
  assert_code cb [ Opcodes.op_exacti; 0; 3; 0x78 ];
  assert (Int.equal fcu 0x78);
  assert (Int.equal fcuf req_caseless);
  assert (Int.equal rcu 0x78);
  assert (Int.equal rcuf req_caseless);

  (* PCRE2_UNGREEDY flips the greedy default (pcre2_compile.c:5698-5699,
     7244,7249): (?U)a* = OP_MINSTAR, (?U)a*? = OP_STAR. *)
  let cb, _, _, _, _, _, _, _ = compile2 "(?U)a*" in
  assert_code cb [ Opcodes.op_minstar; 0x61 ];
  let cb, _, _, _, _, _, _, _ = compile2 "(?U)a*?" in
  assert_code cb [ Opcodes.op_star; 0x61 ];

  (* Character-type repeats use the TYPE opcodes: op_type = OP_TYPESTAR -
     OP_STAR = 52 (pcre2_compile.c:7773), and the type opcode itself is
     the final fill (7895-7897). *)
  let cb, rc, _, _, _, fcuf, _, _ = compile2 ".*" in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_typestar; Opcodes.op_any ];
  assert (Int.equal fcuf req_none);
  let cb, rc, _, _, _, _, _, _ = compile2 ".+?" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_typeminplus; Opcodes.op_any ];
  let cb, _, _, _, _, _, _, _ = compile2 "(?s).*" in
  assert_code cb [ Opcodes.op_typestar; Opcodes.op_allany ];
  let cb, rc, _, _, _, _, _, _ = compile2 "\\d+" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_typeplus; Opcodes.op_digit ];
  let cb, _, _, _, _, _, _, _ = compile2 "\\W*?" in
  assert_code cb [ Opcodes.op_typeminstar; Opcodes.op_not_wordchar ];
  let cb, _, _, _, _, _, _, _ = compile2 "\\d{2,4}" in
  assert_code cb
    [
      Opcodes.op_typeexact;
      0;
      2;
      Opcodes.op_digit;
      Opcodes.op_typeupto;
      0;
      2;
      Opcodes.op_digit;
    ];

  (* Negated one-char classes repeat through the NOT opcodes: op_type =
     chartypeoffset[OP_NOT - OP_CHAR] = 26, chartypeoffset[OP_NOTI -
     OP_CHAR] = 39 (pcre2_compile.c:689-691, 7278). *)
  let cb, rc, _, _, _, fcuf, _, _ = compile2 "[^x]*" in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_notstar; 0x78 ];
  assert (Int.equal fcuf req_none);
  let cb, rc, _, _, _, _, _, _ = compile2 "(?i)[^x]+?" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_notminplusi; 0x78 ];

  (* Class repeats: the CR opcode goes after the 33-unit bitmap item
     (pcre2_compile.c:7311-7344). Check the opcode + full bitmap + repeat
     tail. *)
  let assert_class_with_tail (cb : compile_block) (expected_op : int)
      (member : int -> bool) (tail : int list) =
    assert (Int.equal (Bytes.length cb.start_code) (33 + List.length tail));
    assert (Int.equal (Char.code (Bytes.get cb.start_code 0)) expected_op);
    for c = 0 to 255 do
      let bit =
        Char.code (Bytes.get cb.start_code (1 + (c lsr 3)))
        land (1 lsl (c land 7))
      in
      assert (Bool.equal (not (Int.equal bit 0)) (member c))
    done;
    List.iteri
      (fun i v ->
        assert (Int.equal (Char.code (Bytes.get cb.start_code (33 + i))) v))
      tail
  in
  let is_lower c = c >= 0x61 && c <= 0x7a in

  (* Hand-verified trace 4 — "[a-z]{2,4}" (pcre2_compile.c:7337-7343):
     previous is OP_CLASS (110) + the 32-byte bitmap for a-z; {2,4} is
     neither *, + nor ?, so OP_CRRANGE + repeat_type(0) = 104 is appended,
     then PUT2INC(repeat_min=2) and PUT2INC(repeat_max=4), each big-endian
     over two code units: tail = 104,0,2,0,4 at offsets 33..37. *)
  let cb, rc, _, _, _, _, _, _ = compile2 "[a-z]{2,4}" in
  assert (Int.equal rc 1);
  assert_class_with_tail cb Opcodes.op_class is_lower
    [ Opcodes.op_crrange; 0; 2; 0; 4 ];
  let cb, _, _, _, _, _, _, _ = compile2 "[a-z]*" in
  assert_class_with_tail cb Opcodes.op_class is_lower [ Opcodes.op_crstar ];
  let cb, _, _, _, _, _, _, _ = compile2 "[a-z]+?" in
  assert_class_with_tail cb Opcodes.op_class is_lower [ Opcodes.op_crminplus ];
  (* Unlimited max in a CRRANGE uses the 2-byte encoding 0
     (pcre2_compile.c:7341). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[a-z]{2,}" in
  assert_class_with_tail cb Opcodes.op_class is_lower
    [ Opcodes.op_crrange; 0; 2; 0; 0 ];
  let cb, _, _, _, _, _, _, _ = compile2 "[^ab]??" in
  assert_class_with_tail cb Opcodes.op_nclass
    (fun c -> not (Int.equal c 0x61 || Int.equal c 0x62))
    [ Opcodes.op_crminquery ];

  (* Possessive quantifiers (pcre2_compile.c:7232-7238, 7909-8003):
     repeat_type is forced greedy and the emitted repeat opcode is
     switched to its POS variant via opcode_possessify (7986-7987). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a*+" in
  assert_code cb [ Opcodes.op_posstar; 0x61 ];
  let cb, _, _, _, _, _, _, _ = compile2 "a++" in
  assert_code cb [ Opcodes.op_posplus; 0x61 ];
  let cb, _, _, _, _, _, _, _ = compile2 "a?+" in
  assert_code cb [ Opcodes.op_posquery; 0x61 ];
  let cb, _, _, _, _, _, _, _ = compile2 "[a-z]*+" in
  assert_class_with_tail cb Opcodes.op_class is_lower [ Opcodes.op_crposstar ];
  let cb, _, _, _, _, _, _, _ = compile2 "\\d++" in
  assert_code cb [ Opcodes.op_typeposplus; Opcodes.op_digit ];

  (* Hand-verified trace 5 — "a{2,3}+" (pcre2_compile.c:7838-7878,
     7931-7954, 7971-7987): the repeat compiles as EXACT 2 'a' then, since
     repeat_max - repeat_min = 1, OP_QUERY + repeat_type(0) 'a'
     (7875-7878). The possessive pass starts at tempcode = previous, skips
     the EXACT item (op_lengths[OP_EXACT] = 4, 7945-7949) to the OP_QUERY;
     len = 2 > 0, and opcode_possessify[OP_QUERY] = OP_POSQUERY replaces
     it in place (7986-7987). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{2,3}+" in
  assert_code cb [ Opcodes.op_exact; 0; 2; 0x61; Opcodes.op_posquery; 0x61 ];

  (* "a{3}+": possessifying an EXACT has no effect — after skipping the
     EXACT item, tempcode == code, len = 0, nothing is done
     (pcre2_compile.c:7925-7929, 7971-7978). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{3}+" in
  assert_code cb [ Opcodes.op_exact; 0; 3; 0x61 ];

  (* "a{1,2}+": the char item stays (7831), OP_UPTO 1 follows; the skip
     over OP_CHAR (op_lengths = 2) lands on the UPTO, which possessifies
     to OP_POSUPTO (7941-7949, 7986-7987). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{1,2}+" in
  assert_code cb [ Opcodes.op_char; 0x61; Opcodes.op_posupto; 0; 1; 0x61 ];

  (* A quantified empty-class OP_FAIL is ignored (pcre2_compile.c:
     7346-7352), but END_REPEAT still ORs the REQ_VARY in (8010). *)
  let cb, rc, _, _, _, _, _, _ =
    compile2 ~options:Options.allow_empty_class "[]*"
  in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_fail ];
  assert (Int.equal cb.req_varyopt req_vary);

  (* The zerofirstcu/zeroreqcu interplay across a zero-min repeat
     (pcre2_compile.c:7220-7226): in "a?b" the backoff resets firstcu to
     "none" and reqcu to unset, then 'b' becomes the required unit with
     the REQ_VARY flag from req_varyopt (8323). In "ab*c" the backoff
     restores firstcu = 'a' (zerofirstcu was saved at 8316-8317). *)
  let cb, rc, _, _, _, fcuf, rcu, rcuf = compile2 "a?b" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_query; 0x61; Opcodes.op_char; 0x62 ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf req_vary);
  let cb, _, _, _, fcu, fcuf, rcu, rcuf = compile2 "ab*c" in
  assert_code cb
    [ Opcodes.op_char; 0x61; Opcodes.op_star; 0x62; Opcodes.op_char; 0x63 ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x63);
  assert (Int.equal rcuf req_vary);

  (* {0} repeats make code go backwards (the item is emitted, then
     dropped): the pre-compile length keeps the dropped item
     (pcre2_compile.c:5761-5767 "don't ever reduce the length"), so the
     real phase emits no more than the estimate rather than exactly it.
     Char items pass through OUTPUT_SINGLE_REPEAT's max == 0 exit (7800);
     classes through the class arm's (7324-7328). *)
  let compile2_shrink ?(options = 0) pat expected =
    let cx = parse ~options pat in
    let cb = make_cb ~options pat cx in
    let length = ref 0 in
    let rc1, err1, _, _, _, _, _, _, _ = run_branch ~options cb (Some length) in
    assert (Int.equal err1 0);
    cb.start_code <- Bytes.make !length '\000';
    cb.req_varyopt <- 0 (* pcre2_compile.c:10676, between the phases *);
    let rc2, err2, endcode, _, _, _, _, _, _ = run_branch ~options cb None in
    assert (Int.equal err2 0);
    assert (Int.equal rc1 rc2);
    assert (endcode <= !length);
    assert (Int.equal endcode (List.length expected));
    List.iteri
      (fun i v -> assert (Int.equal (Char.code (Bytes.get cb.start_code i)) v))
      expected
  in
  compile2_shrink "a{0}b" [ Opcodes.op_char; 0x62 ];
  compile2_shrink "[ab]{0}c" [ Opcodes.op_char; 0x63 ];

  (* --- Groups (compile_branch D + compile_regex,
     pcre2_compile.c:6806-7015, 8090-8098, 8345-8646) and bracket repeats
     (pcre2_compile.c:7424-7751). Every byte sequence below was verified
     against `pcre2test -q` with the fullbincode modifier on the real
     10.44 library (offsets shifted by the driver's 3-unit outer Bra,
     which is not part of compile_branch output). *)

  (* "(a)": OP_CBRA + link + IMM2 group number, closed by compile_regex's
     OP_KET whose link equals the whole bracketed length
     (pcre2_compile.c:8093-8098, 8458, 8591-8595). The group sets firstcu
     = 'a' (6965-6971) but no reqcu: the subfirstcu-to-subreqcu conversion
     (6981-6985) applies only when firstcu was already set, and the
     subpattern itself set none (verified: pcre2test -I shows no "Last
     code unit"). *)
  let cb, rc, endpptr, _, fcu, fcuf, _, rcuf = compile2 "(a)" in
  assert (Int.equal rc 1);
  assert_code cb
    [ Opcodes.op_cbra; 0; 7; 0; 1; Opcodes.op_char; 0x61; Opcodes.op_ket; 0; 7 ];
  assert (Int.equal cb.parsed_pattern.(endpptr) Parse.meta_end);
  assert (Int.equal cb.lastcapture 1) (* pcre2_compile.c:8097 *);
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcuf req_unset);

  (* "(?:ab)": non-capturing OP_BRA, no IMM2 (pcre2_compile.c:6806-6808). *)
  let cb, rc, _, _, fcu, fcuf, rcu, rcuf = compile2 "(?:ab)" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_bra;
      0;
      7;
      Opcodes.op_char;
      0x61;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      7;
    ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0);

  (* "(a)(b)": sequential capture numbers; the second group's firstcu
     becomes the branch reqcu via the subfirstcu-to-subreqcu conversion
     (pcre2_compile.c:6977-6994). *)
  let cb, rc, _, _, fcu, fcuf, rcu, rcuf = compile2 "(a)(b)" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_cbra;
      0;
      7;
      0;
      2;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      7;
    ];
  assert (Int.equal cb.lastcapture 2);
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0);

  (* "((a))": nested groups; each KET link spans its own bracket
     (pcre2_compile.c:8594). *)
  let cb, rc, _, _, fcu, _, _, _ = compile2 "((a))" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      15;
      0;
      1;
      Opcodes.op_cbra;
      0;
      7;
      0;
      2;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      15;
    ];
  assert (Int.equal fcu 0x61);

  (* "(a|b)": OP_ALT chain. While open, each ALT's link points back to the
     previous branch (pcre2_compile.c:8633-8636); at the close the chain
     is reversed so the BRA/ALT links point forward (8578-8589). Differing
     branch firstcus abandon the group's firstcu (8528-8543) and the
     differing reqcus then abandon its reqcu (8555-8559): the group
     reports REQ_NONE for both, so the outer branch takes firstcuflags =
     REQ_NONE (6973) and leaves reqcuflags REQ_UNSET (6990 needs
     subreqcuflags < REQ_NONE). *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "(a|b)" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_alt;
      0;
      5;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      12;
    ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);

  (* "()": an empty group may match empty (group_return -1, so no
     matched_char at 6853) and leaves firstcu unset (6965 requires
     subfirstcuflags != REQ_UNSET). *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "()" in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_cbra; 0; 5; 0; 1; Opcodes.op_ket; 0; 5 ];
  assert (Int.equal fcuf req_unset);
  assert (Int.equal rcuf req_unset);

  (* Option changes inside a group do not escape it: compile_regex takes
     options by value (pcre2_compile.c:8378,8497), so (?-i) inside the
     group leaves the outer caseless setting intact. *)
  let cb, _, _, opts_out, _, _, _, _ = compile2 "(?i)((?-i)a)b" in
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_chari;
      0x62;
    ];
  assert (Int.equal opts_out Options.caseless);

  (* --- Bracket repeats (pcre2_compile.c:7424-7751). *)

  (* Hand-verified trace 6 — "(a)*" (pcre2_compile.c:7479-7510,
     7678-7691, 7745-7747): min 0/max unlimited moves the 10-unit group up
     by one (memmove, 7501), emits OP_BRAZERO + repeat_type(0) at the old
     start (7509), and the unlimited-max branch locates ketcode = code-3
     and bracode = ketcode - GET(ketcode,1) = the CBRA (7680-7681);
     group_return = +1 means no SBRA conversion (7705), non-possessive so
     *ketcode = OP_KETRMAX + 0 (7747). Zero minimum backs firstcu off to
     REQ_NONE (7220-7226 via the group's zerofirstcuflags from 6974). *)
  let cb, rc, _, _, _, fcuf, _, _ = compile2 "(a)*" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_brazero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrmax;
      0;
      7;
    ];
  assert (Int.equal fcuf req_none);

  (* "(a)*?": lazy repeat_type(1) selects OP_BRAMINZERO (7509) and
     OP_KETRMIN (7747). *)
  let cb, _, _, _, _, _, _, _ = compile2 "(a)*?" in
  assert_code cb
    [
      Opcodes.op_braminzero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrmin;
      0;
      7;
    ];

  (* "(a)+": min 1 needs no BRAZERO and no replication; just the KETRMAX
     conversion (7544 false, 7592 false, 7747). Like "(a)", firstcu only
     (min 1 does not reach the 7569 promotion, which needs min > 1). *)
  let cb, rc, _, _, fcu, fcuf, _, rcuf = compile2 "(a)+" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrmax;
      0;
      7;
    ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcuf req_unset);

  (* "(a)?" = {0,1}: BRAZERO inserted, repeat_max-- -> 0, so the common
     limited-max code adds no copies and the KET stays OP_KET
     (7499-7510, 7535, 7616 zero iterations). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a)?" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_brazero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
    ];

  (* "(?:a)?": same shape over a non-capturing bracket. *)
  let cb, _, _, _, _, _, _, _ = compile2 "(?:a)?" in
  assert_code cb
    [
      Opcodes.op_brazero;
      Opcodes.op_bra;
      0;
      5;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      5;
    ];

  (* Hand-verified trace 7 — "(a){2}" (pcre2_compile.c:7544-7583): min 2
     replicates the group once via memcpy (7574-7578); repeat_max -=
     repeat_min leaves 0, so nothing further. In the pre-compile phase the
     replication is pure length arithmetic: delta = (repeat_min-1) *
     length_prevgroup (7550-7561); compile2 checks the two phases agree. *)
  let cb, rc, _, _, fcu, _, rcu, _ = compile2 "(a){2}" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
    ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal rcu 0x61);

  (* Hand-verified trace 8 — "(a){2,4}" (pcre2_compile.c:7574-7583,
     7616-7649): two mandatory copies (offsets 0,10), then repeat_max = 2
     optional copies compiled countdown: i=2 emits BRAZERO(20) + BRA(21)
     with its link field (22-23) holding the chain link pro tem (0 =
     chain end) + copy(24); i=1 emits BRAZERO(34) + copy(35). The
     chain-through pass then walks bralink: linkoffset = code(45) -
     bralink(22) + 1 = 24, bra = 21, emits OP_KET(45) with link 24 and
     back-fills PUT(bra+1, 24) (7639-7649). Pre-compile counts delta =
     repeat_max*(length_prevgroup + 1 + 2 + 2*LINK_SIZE) - (2+2*LINK_SIZE)
     (7600-7611): 2*17 - 6 = 28 = the 28 units emitted after the two
     mandatory copies. *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a){2,4}" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_brazero;
      Opcodes.op_bra;
      0;
      24;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_brazero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      24;
    ];

  (* "(a){0,2}": the zero-minimum nested case (pcre2_compile.c:7519-7533)
     moves the original copy up by 2+LINK_SIZE, emits BRAZERO + OP_BRA
     with the chain link, and repeat_max-- = 1 more copy from the common
     code; the chain-through pass closes the nesting bracket: KET(25)
     link 24 -> BRA(1). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a){0,2}" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_brazero;
      Opcodes.op_bra;
      0;
      24;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_brazero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      24;
    ];

  (* "(a){0}b": {0,0} sticks OP_SKIPZERO in front of the group so it is
     skipped on execution (pcre2_compile.c:7481-7507); unlike the
     char/class {0} cases, code does not go backwards, so compile2's
     exact-length check applies. *)
  let cb, rc, _, _, _, _, rcu, rcuf = compile2 "(a){0}b" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_skipzero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_char;
      0x62;
    ];
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0) (* {0,0}: reqvary = 0 (pcre2_compile.c:7215) *);

  (* Hand-verified trace 9 — "(a)*+" (pcre2_compile.c:7712-7742): the
     possessive unlimited repeat switches the CBRA to OP_CBRAPOS
     ( *bracode += 1, 7734), the KET to OP_KETRPOS (7735), and the saved
     brazeroptr to OP_BRAPOSZERO (7741); repeat_min(0) < 2 then cancels
     possessive_quantifier so no ONCE wrapping happens (7742). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a)*+" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_braposzero;
      Opcodes.op_cbrapos;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrpos;
      0;
      7;
    ];

  (* "(?:a)++": OP_BRA + 1 = OP_BRAPOS, no BRAZERO for min 1. *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(?:a)++" in
  assert (Int.equal rc 1);
  assert_code cb
    [ Opcodes.op_brapos; 0; 5; Opcodes.op_char; 0x61; Opcodes.op_ketrpos; 0; 5 ];

  (* "(a|)*": the group may match empty (group_return -1), so the real
     phase converts OP_CBRA to OP_SCBRA ( *bracode += OP_SBRA - OP_BRA,
     pcre2_compile.c:7703-7705) before setting KETRMAX. *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a|)*" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_brazero;
      Opcodes.op_scbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_alt;
      0;
      3;
      Opcodes.op_ketrmax;
      0;
      10;
    ];

  (* "(a){2,}+": with repeat_min 2 the possessive flag survives (7742), so
     after the last copy becomes CBRAPOS/KETRPOS the generic possessive
     pass wraps the whole repeated item in ONCE brackets
     (pcre2_compile.c:7989-8001): [ONCE [CBRA a KET] [CBRAPOS a KETRPOS]
     KET]. *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a){2,}+" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_once;
      0;
      23;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_cbrapos;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrpos;
      0;
      7;
      Opcodes.op_ket;
      0;
      23;
    ];

  (* "(a){1}+": a possessive {1,1} is NOT ignored (7447 requires
     !possessive_quantifier); nothing in the bracket arm changes the
     group, and the generic possessive pass wraps it in ONCE
     (opcode_possessify[OP_CBRA] does not apply — OP_CBRA > OP_CALLOUT). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a){1}+" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_once;
      0;
      13;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      13;
    ];

  (* "(a){1}": a non-possessive {1,1} bracket repeat is ignored (7447). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a){1}" in
  assert (Int.equal rc 1);
  assert_code cb
    [ Opcodes.op_cbra; 0; 7; 0; 1; Opcodes.op_char; 0x61; Opcodes.op_ket; 0; 7 ];

  (* groupsetfirstcu (pcre2_compile.c:6971, 7569-7573): in "(a){2}c" the
     group set firstcu = 'a' and the min > 1 replication promotes it to
     reqcu before 'c' overrides — reqcu ends as 'c' with REQ_VARY. In
     "(ab){2}" the 7569 promotion guard is false (the group itself already
     set reqcu = 'b' via pcre2_compile.c:6990-6994); firstcu 'a', reqcu 'b'
     come from the group's own values. *)
  let cb, _, _, _, fcu, fcuf, rcu, rcuf = compile2 "(ab){2}" in
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0);
  assert (Int.equal (Bytes.length cb.start_code) 24);

  (* Deferred arms fail loudly in both phases: Unicode property classes
     (M7), and the Unicode caseless-literal path (M6/M7 DEVIATION in
     compile_branch). *)
  expect_deferred ~options:(Options.ucp lor Options.caseless) "k";
  expect_deferred ~options:Options.ucp "[\\d]"
  (* parse substitutes \p{Nd} under UCP: ESC_p in a class = XCL_PROP (M7) *);
  expect_deferred ~options:(Options.ucp lor Options.caseless) "[^k]"
(* OP_NOTPROP PT_CLIST (M7) *)

(* pcre2_compile() end-to-end: two-pass driver, pso settings, error
   propagation, name table, and the anchoring / first-and-required
   code-unit finalization. Expected bytecode verified against `pcre2test
   -q` with fullbincode / -I on the real 10.44 library. *)
let () =
  let ok pat =
    match pcre2_compile pat ~options:0 with
    | Ok re -> re
    | Error (e, o) ->
        failwith
          (Printf.sprintf "pcre2_compile %S: error %d at offset %d" pat e o)
  in
  let expect_err pat options e o =
    match pcre2_compile pat ~options with
    | Ok _ -> assert false
    | Error (e', o') ->
        assert (Int.equal e e');
        assert (Int.equal o o')
  in
  (* /abc/: [BRA 9][CHAR a][CHAR b][CHAR c][KET 9][END]; firstcu 'a',
     reqcu 'c', not anchored, no name table. *)
  let re = ok "abc" in
  assert (Int.equal (Bytes.length re.code) 13);
  List.iteri
    (fun i v -> assert (Int.equal (Char.code (Bytes.get re.code i)) v))
    [
      Opcodes.op_bra;
      0;
      9;
      Opcodes.op_char;
      0x61;
      Opcodes.op_char;
      0x62;
      Opcodes.op_char;
      0x63;
      Opcodes.op_ket;
      0;
      9;
      Opcodes.op_end;
    ];
  assert (Int.equal re.compile_options 0);
  assert (Int.equal re.overall_options 0);
  assert (Int.equal (re.flags land (firstset lor lastset)) (firstset lor lastset));
  assert (Int.equal re.first_codeunit 0x61);
  assert (Int.equal re.last_codeunit 0x63);
  assert (Int.equal (re.flags land match_empty) 0);
  assert (Int.equal re.minlength 2 (* first + required code units *));
  assert (Int.equal re.top_bracket 0);
  assert (Int.equal re.name_count 0);
  assert (Int.equal re.newline_convention Options.newline_lf);
  assert (Int.equal re.bsr_convention Options.bsr_unicode);
  assert (Int.equal re.limit_match 0xffff_ffff);
  (* /^abc|^def/ is startline-anchored via OP_CIRC in every branch;
     /(?s).*abc/ is auto-anchored via TYPESTAR+ALLANY. *)
  let re = ok "^abc" in
  assert (not (Int.equal (re.overall_options land Options.anchored) 0));
  let re = ok "(?s).*abc" in
  assert (not (Int.equal (re.overall_options land Options.anchored) 0));
  (* Multiline ^ compiles CIRCM: not anchored, but STARTLINE. *)
  let re =
    match pcre2_compile "^abc" ~options:Options.multiline with
    | Ok re -> re
    | Error _ -> assert false
  in
  assert (Int.equal (re.overall_options land Options.anchored) 0);
  assert (not (Int.equal (re.flags land startline) 0));
  (* /(|a)/ can match an empty string. *)
  let re = ok "(|a)" in
  assert (not (Int.equal (re.flags land match_empty) 0));
  assert (Int.equal re.top_bracket 1);
  (* find_firstassertedcu: /(?=abc)a../ has an asserted first cu... but
     lookaheads are M4-deferred; use the caseless-literal path instead:
     /(?i)abc/ sets FIRSTCASELESS/LASTCASELESS with argoptions =
     alloptions = 0 (a leading (?i) does NOT reach external_options —
     pcre2test shows no "Options:" line, testoutput2:484-488). *)
  let re = ok "(?i)abc" in
  assert (Int.equal re.compile_options 0);
  assert (Int.equal re.overall_options 0);
  assert (
    Int.equal
      (re.flags land (firstset lor firstcaseless lor lastset lor lastcaseless))
      (firstset lor firstcaseless lor lastset lor lastcaseless));
  assert (Int.equal re.first_codeunit 0x61);
  assert (Int.equal re.last_codeunit 0x63);
  (* pso settings: ( *CR) newline override + NL_SET flag; ( *NOTEMPTY) flag;
     ( *LIMIT_MATCH=1000) limit. *)
  let re = ok "(*CR)a" in
  assert (Int.equal re.newline_convention Options.newline_cr);
  assert (not (Int.equal (re.flags land nl_set) 0));
  let re = ok "(*BSR_ANYCRLF)a" in
  assert (Int.equal re.bsr_convention Options.bsr_anycrlf);
  assert (not (Int.equal (re.flags land bsr_set) 0));
  let re = ok "(*NOTEMPTY)a" in
  assert (not (Int.equal (re.flags land notempty_set) 0));
  let re = ok "(*LIMIT_MATCH=1000)a" in
  assert (Int.equal re.limit_match 1000);
  assert (Int.equal re.limit_heap 0xffff_ffff);
  (* ( *ANY)/( *ANYCRLF) newline types flow into the parse-time IS_NEWLINE
     (extended-mode # comment scan, pcre2_compile.c:3070-3085 +
     PRIV(is_newline) pcre2_newline.c:78-145): the comment ends at the
     LF / CR and 'a' compiles. *)
  (match pcre2_compile "(*ANY)#c\na" ~options:Options.extended with
  | Ok re ->
      assert (Int.equal re.newline_convention Options.newline_any);
      assert (Int.equal re.first_codeunit 0x61)
  | Error _ -> assert false);
  (match pcre2_compile "(*ANYCRLF)#c\ra" ~options:Options.extended with
  | Ok re ->
      assert (Int.equal re.newline_convention Options.newline_anycrlf);
      assert (Int.equal re.first_codeunit 0x61)
  | Error _ -> assert false);
  (* Malformed limit: ERR60 with the C's ptr+pp offsets
     (pcre2_compile.c:10349-10365). *)
  expect_err "(*LIMIT_MATCH=x)a" 0 Errors.err60 14;
  expect_err "(*LIMIT_MATCH=12" 0 Errors.err60 17;
  (* Unknown ( *WORD) is not a pso: falls through to parse_regex's verb
     handling (deferred, M5) — just check it is an error, not a crash. *)
  (match pcre2_compile "(*XYZZY)a" ~options:0 with
  | Ok _ -> assert false
  | Error _ -> ());
  (* Error propagation with parse offsets: "(" = ERR14 at 1; "a{2,1}" =
     ERR4 at 5 (testoutput2:128-129 shape). *)
  expect_err "(" 0 Errors.err14 1;
  expect_err "a{2,1}" 0 Errors.err4 5;
  (* Option validation: an undefined public bit (0x08000000 is unassigned
     in 10.44's compile options) = ERR17 (117) at offset 0; PCRE2_LITERAL
     restricted set = ERR92. *)
  expect_err "a" 0x08000000 Errors.err17 0;
  expect_err "a" (Options.literal lor Options.multiline) Errors.err92 0;
  (* ( *UTF) under NEVER_UTF: ERR74 at the post-pso offset. *)
  expect_err "(*UTF)a" Options.never_utf Errors.err74 6;
  expect_err "(*UCP)a" Options.never_ucp Errors.err75 6;
  (* UTF compile pipeline defers loudly (M6). *)
  expect_err "a" Options.utf Parse.err_deferred 0;
  (* Name table: /(?<xx>a)(?<yy>b)(?<ww>c)/ has 3 entries of size
     2+2+1 = 5, alphabetically ordered by add_name_to_table. *)
  let re = ok "(?<xx>a)(?<yy>b)(?<ww>c)" in
  assert (Int.equal re.name_count 3);
  assert (Int.equal re.name_entry_size 5);
  assert (Int.equal re.top_bracket 3);
  let assert_entry i expect_name expect_num =
    let base = i * re.name_entry_size in
    assert (Int.equal (get2 re.name_table base) expect_num);
    assert (
      String.equal
        (Bytes.sub_string re.name_table (base + Limits.imm2_size) 2)
        expect_name);
    assert (
      Int.equal
        (Char.code (Bytes.get re.name_table (base + Limits.imm2_size + 2)))
        0)
  in
  assert_entry 0 "ww" 3;
  assert_entry 1 "xx" 1;
  assert_entry 2 "yy" 2

(* Backreference compilation (M2 compile chunk, pcre2_compile.c:7018-7098
   and 8022-8058, docs/ocaml-engine/03-backreferences.md): bytecode,
   top_backref and error numbers/offsets pinned with `pcre2test -q` +
   fullbincode/-I on the real 10.44 library. *)
let () =
  let ok ?(options = 0) pat =
    match pcre2_compile pat ~options with
    | Ok re -> re
    | Error (e, o) ->
        failwith
          (Printf.sprintf "pcre2_compile %S: error %d at offset %d" pat e o)
  in
  let expect_err pat options e o =
    match pcre2_compile pat ~options with
    | Ok _ -> assert false
    | Error (e', o') ->
        assert (Int.equal e e');
        assert (Int.equal o o')
  in
  let assert_code (re : re) (expected : int list) =
    assert (Int.equal (Bytes.length re.code) (List.length expected));
    List.iteri
      (fun i v -> assert (Int.equal (Char.code (Bytes.get re.code i)) v))
      expected
  in
  let byte (re : re) i = Char.code (Bytes.get re.code i) in
  (* /(a)\1/: [BRA 16][CBRA 7 1][CHAR a][KET 7][REF 1][KET 16][END];
     Max back reference = 1. *)
  let re = ok "(a)\\1" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      16;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ref;
      0;
      1;
      Opcodes.op_ket;
      0;
      16;
      Opcodes.op_end;
    ];
  assert (Int.equal re.top_backref 1);
  (* Caseless: /i \1 = OP_REFI. *)
  let re = ok "(?i)(a)\\1" in
  assert (Int.equal (byte re 13) Opcodes.op_refi);
  assert (Int.equal (get2 re.code 14) 1);
  (* /(?P<n>a)\k<n>/: a non-duplicated name resolves to the numerical
     reference \1 (HANDLE_SINGLE_REFERENCE via META_BACKREF_BYNAME). *)
  let re = ok "(?P<n>a)\\k<n>" in
  assert (Int.equal (byte re 13) Opcodes.op_ref);
  assert (Int.equal (get2 re.code 14) 1);
  assert (Int.equal re.top_backref 1);
  (* /((a)|(b))\k<A>(?<A>c)/dupnames: single-entry name, forward
     reference — resolves numerically to \4. *)
  let re = ok ~options:Options.dupnames "((a)|(b))\\k<A>(?<A>c)" in
  assert (Int.equal (byte re 34) Opcodes.op_ref);
  assert (Int.equal (get2 re.code 35) 4);
  assert (Int.equal re.top_backref 4);
  (* /(?P<zz>a)(?P<zz>b)\k<zz>/dupnames: OP_DNREF with name-table index 0
     and duplicate count 2 (pcre2test: "23 \k<zz>2"); Max back
     reference = 2. Caseless flavour is OP_DNREFI. *)
  let re = ok ~options:Options.dupnames "(?P<zz>a)(?P<zz>b)\\k<zz>" in
  assert (Int.equal (Bytes.length re.code) 32);
  assert (Int.equal (byte re 23) Opcodes.op_dnref);
  assert (Int.equal (get2 re.code 24) 0 (* index *));
  assert (Int.equal (get2 re.code 26) 2 (* count *));
  assert (Int.equal (byte re 28) Opcodes.op_ket);
  assert (Int.equal re.top_backref 2);
  let re =
    ok
      ~options:(Options.dupnames lor Options.caseless)
      "(?P<zz>a)(?P<zz>b)\\k<zz>"
  in
  assert (Int.equal (byte re 23) Opcodes.op_dnrefi);
  (* /(?|(a)|(b))\1/: duplicate group numbers from (?| — \1 is numeric. *)
  let re = ok "(?|(a)|(b))\\1" in
  assert (Int.equal (byte re 32) Opcodes.op_ref);
  assert (Int.equal (get2 re.code 33) 1);
  assert (Int.equal re.top_backref 1);
  (* Group >= 10 takes the GETPLUSOFFSET parsed-pattern word
     (pcre2_compile.c:8030-8031): /(a)..(l)\12/ ends [123 REF 12][126 KET]
     [129 END]. *)
  let re = ok "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)\\12" in
  assert (Int.equal (Bytes.length re.code) 130);
  assert (Int.equal (byte re 123) Opcodes.op_ref);
  assert (Int.equal (get2 re.code 124) 12);
  assert (Int.equal (byte re 126) Opcodes.op_ket);
  assert (Int.equal (byte re 129) Opcodes.op_end);
  assert (Int.equal re.top_backref 12);
  (* Repeats after a backref (pcre2_compile.c:7311-7344): {2} = CRRANGE,
     {0,3}? = CRMINRANGE, {0} drops the item, *+ wraps in ONCE. *)
  let re = ok "(a|b)\\1{2}" in
  assert (Int.equal (byte re 18) Opcodes.op_ref);
  assert (Int.equal (byte re 21) Opcodes.op_crrange);
  assert (Int.equal (get2 re.code 22) 2);
  assert (Int.equal (get2 re.code 24) 2);
  let re = ok "(a)\\1{0,3}?" in
  assert (Int.equal (byte re 13) Opcodes.op_ref);
  assert (Int.equal (byte re 16) Opcodes.op_crminrange);
  assert (Int.equal (get2 re.code 17) 0);
  assert (Int.equal (get2 re.code 19) 3);
  (* {0} drops the backref entirely (code = previous,
     pcre2_compile.c:7324-7328): [BRA 13][CBRA 7 1][CHAR a][KET 7][KET 13]
     [END]. The pre-pass deliberately never reduces the length
     (pcre2_compile.c:5761-5767), so re.code keeps 3 units of zeroed slack
     after OP_END (usedlength < length, pcre2_compile.c:10712-10725). *)
  let re = ok "(a)\\1{0}" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      13;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      13;
      Opcodes.op_end;
      0;
      0;
      0;
    ];
  let re = ok "(a)\\1*+" in
  assert (Int.equal (byte re 13) Opcodes.op_once);
  assert (Int.equal (byte re 16) Opcodes.op_ref);
  assert (Int.equal (byte re 19) Opcodes.op_crstar);
  assert (Int.equal (byte re 20) Opcodes.op_ket);
  assert (Int.equal (get re.code 21) 7);
  (* PCRE2_MATCH_UNSET_BACKREF has no compile-time effect outside the
     lookbehind-length machinery (pcre2_compile.c:9621,9668 — M4). *)
  let re = ok ~options:Options.match_unset_backref "(a)\\1" in
  assert (Int.equal (byte re 13) Opcodes.op_ref);
  (* ERR15 (115) reference to non-existent subpattern, with the parse-
     recorded offsets: named at the name (pcre2_compile.c:7068-7073),
     numeric at small_ref_offset / GETPLUSOFFSET (8033-8038). *)
  expect_err "(?P=zz)" 0 Errors.err15 4;
  expect_err "(a)\\2" 0 Errors.err15 4;
  expect_err "\\2()" 0 Errors.err15 1;
  expect_err "abc\\1" 0 Errors.err15 4;
  expect_err "\\g{12}abc" 0 Errors.err15 5;
  expect_err "\\g{2}()" 0 Errors.err15 4

(* Lookaround assertions and atomic groups (M3 compile chunk,
   pcre2_compile.c:6748-6804, 8427-8491, 8639-8643 and the lookbehind-
   length machinery 9223-10102, docs/ocaml-engine/04-lookaround-atomic-
   possessive.md): bytecode, max_lookbehind and error numbers/offsets
   pinned with `pcre2test -q` + fullbincode/-I on the real 10.44
   library. *)
let () =
  let ok ?(options = 0) pat =
    match pcre2_compile pat ~options with
    | Ok re -> re
    | Error (e, o) ->
        failwith
          (Printf.sprintf "pcre2_compile %S: error %d at offset %d" pat e o)
  in
  let expect_err pat options e o =
    match pcre2_compile pat ~options with
    | Ok _ -> assert false
    | Error (e', o') ->
        assert (Int.equal e e');
        assert (Int.equal o o')
  in
  let assert_code (re : re) (expected : int list) =
    assert (Int.equal (Bytes.length re.code) (List.length expected));
    List.iteri
      (fun i v -> assert (Int.equal (Char.code (Bytes.get re.code i)) v))
      expected
  in
  let byte (re : re) i = Char.code (Bytes.get re.code i) in
  (* /(?=ab)/: [BRA 13][ASSERT 7][CHAR a][CHAR b][KET 7][KET 13][END]. *)
  let re = ok "(?=ab)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      13;
      Opcodes.op_assert;
      0;
      7;
      Opcodes.op_char;
      0x61;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      13;
      Opcodes.op_end;
    ];
  assert (Int.equal re.max_lookbehind 0);
  (* /(?!x)/: [BRA 11][ASSERT_NOT 5][CHAR x][KET 5][KET 11][END]. *)
  let re = ok "(?!x)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      11;
      Opcodes.op_assert_not;
      0;
      5;
      Opcodes.op_char;
      0x78;
      Opcodes.op_ket;
      0;
      5;
      Opcodes.op_ket;
      0;
      11;
      Opcodes.op_end;
    ];
  (* /(?<=ab)c/: fixed-length lookbehind — [ASSERTBACK 10][REVERSE 2]
     inserted at the head of the branch; Max lookbehind = 2. *)
  let re = ok "(?<=ab)c" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      18;
      Opcodes.op_assertback;
      0;
      10;
      Opcodes.op_reverse;
      0;
      2;
      Opcodes.op_char;
      0x61;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      10;
      Opcodes.op_char;
      0x63;
      Opcodes.op_ket;
      0;
      18;
      Opcodes.op_end;
    ];
  assert (Int.equal re.max_lookbehind 2);
  (* /(?<!a|bc)d/: per-branch fixed lengths (1 and 2) each get their own
     OP_REVERSE (the whole group's minlength word is LOOKBEHIND_MAX);
     Max lookbehind = 2. *)
  let re = ok "(?<!a|bc)d" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      26;
      Opcodes.op_assertback_not;
      0;
      8;
      Opcodes.op_reverse;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_alt;
      0;
      10;
      Opcodes.op_reverse;
      0;
      2;
      Opcodes.op_char;
      0x62;
      Opcodes.op_char;
      0x63;
      Opcodes.op_ket;
      0;
      18;
      Opcodes.op_char;
      0x64;
      Opcodes.op_ket;
      0;
      26;
      Opcodes.op_end;
    ];
  assert (Int.equal re.max_lookbehind 2);
  (* /(?>a+)b/: [BRA 13][ONCE 5][PLUS a][KET 5][CHAR b][KET 13][END].
     pcre2test shows "a++" because auto_possessify rewrites OP_PLUS to
     OP_POSPLUS — that pass is M9 (deferred without an error marker, see
     the driver note at its call site); opcode positions and lengths are
     identical. *)
  let re = ok "(?>a+)b" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      13;
      Opcodes.op_once;
      0;
      5;
      Opcodes.op_plus;
      0x61;
      Opcodes.op_ket;
      0;
      5;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      13;
      Opcodes.op_end;
    ];
  (* /(?<=a?bc|ab)d/: variable-length first branch (min 2, max 3) gets
     OP_VREVERSE; the fixed second branch (min = max = 2) keeps
     OP_REVERSE; Max lookbehind = 3. (pcre2test shows "a?+" — OP_POSQUERY
     — after the M9 auto_possessify pass; OP_QUERY here, same length.) *)
  let re = ok "(?<=a?bc|ab)d" in
  assert (Int.equal (Bytes.length re.code) 36);
  assert (Int.equal (byte re 3) Opcodes.op_assertback);
  assert (Int.equal (get re.code 4) 14);
  assert (Int.equal (byte re 6) Opcodes.op_vreverse);
  assert (Int.equal (get2 re.code 7) 2 (* min *));
  assert (Int.equal (get2 re.code 9) 3 (* max *));
  assert (Int.equal (byte re 11) Opcodes.op_query);
  assert (Int.equal (byte re 17) Opcodes.op_alt);
  assert (Int.equal (get re.code 18) 10);
  assert (Int.equal (byte re 20) Opcodes.op_reverse);
  assert (Int.equal (get2 re.code 21) 2);
  assert (Int.equal (byte re 27) Opcodes.op_ket);
  assert (Int.equal (get re.code 28) 24);
  assert (Int.equal (byte re 30) Opcodes.op_char);
  assert (Int.equal re.max_lookbehind 3);
  assert (Int.equal re.first_codeunit 0x64 (* "First code unit = 'd'" *));
  (* Max lookbehind is the longest branch, not the longest whole
     assertion: /(?<=ab|defgh)x/ -> 5. *)
  let re = ok "(?<=ab|defgh)x" in
  assert (Int.equal re.max_lookbehind 5);
  (* An unquantified (?!) is optimized to OP_FAIL
     (pcre2_compile.c:6762-6774)... *)
  let re = ok "(?!)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      4;
      Opcodes.op_fail;
      Opcodes.op_ket;
      0;
      4;
      Opcodes.op_end;
    ];
  (* ...but a quantified one is a real (repeated) assertion group. *)
  let re = ok "(?!)+" in
  assert (Int.equal (Bytes.length re.code) 20);
  assert (Int.equal (byte re 3) Opcodes.op_assert_not);
  assert (Int.equal (byte re 9) Opcodes.op_brazero);
  assert (Int.equal (byte re 10) Opcodes.op_assert_not);
  (* Non-atomic forms: (?* and (?<* (and their alpha synonyms) compile
     OP_ASSERT_NA / OP_ASSERTBACK_NA. *)
  let re = ok "(?*abc)d" in
  assert (Int.equal (byte re 3) Opcodes.op_assert_na);
  let re = ok "(?<*ab)c" in
  assert (Int.equal (byte re 3) Opcodes.op_assertback_na);
  assert (Int.equal (byte re 6) Opcodes.op_reverse);
  assert (Int.equal re.max_lookbehind 2);
  (* Alpha synonyms produce identical code to the symbolic forms. *)
  let re = ok "(*plb:ab)c" in
  assert (Int.equal (byte re 3) Opcodes.op_assertback);
  assert (Int.equal (byte re 6) Opcodes.op_reverse);
  assert (Int.equal (get2 re.code 7) 2);
  (* A lookahead nested in a lookbehind contributes no length but
     compiles inline (REVERSE 1 for the y). *)
  let re = ok "(?<=(?=x)y)z" in
  assert (Int.equal (byte re 3) Opcodes.op_assertback);
  assert (Int.equal (byte re 6) Opcodes.op_reverse);
  assert (Int.equal (get2 re.code 7) 1);
  assert (Int.equal (byte re 9) Opcodes.op_assert);
  assert (Int.equal re.max_lookbehind 1);
  (* Lookbehind length errors, offsets per pcre2test: ERR25 (125) for
     unlimited length (\X never allowed); ERR100 (200) for a variable
     branch longer than max_varlookbehind (default 255); ERR87 (187) for
     a branch over LOOKBEHIND_MAX (65535). *)
  expect_err "(?<=a+)b" 0 Errors.err25 0;
  expect_err "x(?<!a*)b" 0 Errors.err25 1;
  expect_err "(?<=\\Xa)b" 0 Errors.err25 0;
  expect_err "(?<=a{0,300})b" 0 Errors.err100 0;
  expect_err "(?<=a{40000}a{40000})b" 0 Errors.err87 0;
  (* A variable lookbehind within the default 255 limit compiles. *)
  let re = ok "(?<=a{2,5})b" in
  assert (Int.equal (byte re 3) Opcodes.op_assertback);
  assert (Int.equal (byte re 6) Opcodes.op_vreverse);
  assert (Int.equal (get2 re.code 7) 2);
  assert (Int.equal (get2 re.code 9) 5);
  assert (Int.equal re.max_lookbehind 5)
