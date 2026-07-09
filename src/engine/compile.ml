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

   Chunk boundary (M1 chunks "compile_branch A".."D"): chars/escapes,
   classes, repeats and plain capture/non-capture groups (with bracket
   repeats) are live. The arms for conditionals/lookarounds (M4/M5),
   verbs, backrefs, recursion and string callouts are deferred — they fail
   loudly with Parse.err_deferred (identically in both phases, before any
   phase-dependent work); within the repeat arm, an OP_RECURSE previous
   item likewise defers (M5). The C local `offset`
   (pcre2_compile.c:5658), owned by the still-deferred conditional arms,
   arrives with them. The class locals (negate_class, should_flip_negation,
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
      else if
        Int.equal meta Parse.meta_lookahead
        || Int.equal meta Parse.meta_lookahead_na
        || Int.equal meta Parse.meta_lookaheadnot
        || Int.equal meta Parse.meta_lookbehind
        || Int.equal meta Parse.meta_lookbehindnot
        || Int.equal meta Parse.meta_lookbehind_na
        || Int.equal meta Parse.meta_atomic
        || Int.equal meta Parse.meta_script_run
      then (
        (* pcre2_compile.c:6747-6804 — lookarounds, atomic groups
           (docs/ocaml-engine/04-lookaround-atomic-possessive.md) and
           ( *script_run:) (docs/ocaml-engine/08-ucp.md). Deferred
           loudly. *)
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
        (* pcre2_compile.c:7021-7098 — named backreferences
           (docs/ocaml-engine/03-backreferences.md) and named recursion
           (docs/ocaml-engine/05-conditionals-recursion.md). Deferred
           loudly. *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
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
               Int.equal !repeat_max 1
               && Int.equal !repeat_min 1
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
                         if Int.equal !bralink (-1) then 0
                         else !code - !bralink
                       in
                       bralink := !code;
                       putinc cb.start_code code linkoffset);

                     Bytes.blit cb.start_code !previous cb.start_code !code
                       len;
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
             else (
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
                   (if
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
                        (Char.chr Opcodes.op_ketrpos)));

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
                     (Char.chr (Opcodes.op_ketrmax + !repeat_type)))))
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
        (* pcre2_compile.c:8022-8058 — back reference by number:
           docs/ocaml-engine/03-backreferences.md. Deferred loudly. *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
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
     assertion, and if it is, save its length and skip over the pattern
     offset. DEVIATION: the lookbehind machinery — the length/min-length
     bookkeeping here (8434-8440), the OP_REVERSE/OP_VREVERSE insertion per
     branch (8467-8491) and the per-alternative lookbehindlength update
     (8639-8642) — lands with the lookaround chunks (M3/M4,
     docs/ocaml-engine/04-lookaround-atomic-possessive.md). It is
     unreachable until then, because compile_branch defers every lookbehind
     META before an ASSERTBACK bravalue can reach this function; deferred
     loudly, both phases, so those chunks cannot silently miss it. *)
  let op0 = Char.code (Bytes.get cb.start_code !code) in
  let lookbehind =
    Int.equal op0 Opcodes.op_assertback
    || Int.equal op0 Opcodes.op_assertback_not
    || Int.equal op0 Opcodes.op_assertback_na
  in
  if lookbehind then (
    errorcodeptr := Parse.err_deferred;
    0)
  else (
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
           this is a lookbehind assertion: with the lookbehind machinery
           above (M3/M4; lookbehind is always false here). *)

        (* pcre2_compile.c:8493-8500 — now compile the branch; in the
           pre-compile phase its length gets added into the length. *)
        let branch_return =
          compile_branch options xoptions code pptr errorcodeptr branchfirstcu
            branchfirstcuflags branchreqcu branchreqcuflags (Some bc) open_caps
            cb
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
            (Int.equal
               (Parse.meta_code cb.parsed_pattern.(!pptr))
               Parse.meta_alt)
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
           for the next branch (deferred with the lookbehind machinery
           above) and then advance past the vertical bar. *)
        pptr := !pptr + 1
      done;
      (* Control never reaches here (pcre2_compile.c:8645). *)
      assert false
    with Return_regex rc -> rc)

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
    [
      Opcodes.op_brapos;
      0;
      5;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrpos;
      0;
      5;
    ];

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

  (* Deferred arms fail loudly in both phases: backrefs (M2), Unicode
     property classes (M7), and the Unicode caseless-literal path (M6/M7
     DEVIATION in compile_branch). *)
  expect_deferred "\\1()" (* META_BACKREF comes first in this stream *);
  expect_deferred ~options:(Options.ucp lor Options.caseless) "k";
  expect_deferred ~options:Options.ucp "[\\d]"
  (* parse substitutes \p{Nd} under UCP: ESC_p in a class = XCL_PROP (M7) *);
  expect_deferred ~options:(Options.ucp lor Options.caseless) "[^k]"
(* OP_NOTPROP PT_CLIST (M7) *)
