(* Compile phase of the pure-OCaml PCRE2 10.44 port: META stream -> bytecode.

   Ported from the back half of vendor/pcre2/src/pcre2_compile.c. This
   module currently holds the compile-phase foundations: the workspace and
   length-overflow constants, the first/required code-unit flag values, the
   LINK_SIZE / IMM2_SIZE store-load primitives (PUT/GET, PUT2/GET2 and
   their INC forms) over the Bytes.t code buffer, the compile_context and
   compile_block records with their pcre2_compile() defaults, and the
   small helpers compile_branch and its callers need early
   (check_workspace_overflow, first_significant_code,
   find_dupname_details). compile_branch, compile_regex, and the
   pcre2_compile() driver are later M1 chunks
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

   Chunk boundary (this is M1 chunk "compile_branch A"): the arms for
   character classes (chunk B), quantifiers (chunk C), groups/conditionals/
   lookarounds (chunk D and later milestones), verbs, backrefs, recursion
   and string callouts are deferred — they fail loudly with
   Parse.err_deferred (identically in both phases, before any
   phase-dependent work). The C locals owned by those arms (bravalue,
   group_return, repeat_min/max, repeat_type, op_type, offset,
   length_prevgroup, tempcode, op_previous, groupsetfirstcu, classbits,
   class_uchardata and friends, pcre2_compile.c:5642-5694) arrive with
   their arms. *)
let compile_branch (optionsptr : int ref) (xoptionsptr : int ref)
    (codeptr : int ref) (pptrptr : int ref) (errorcodeptr : int ref)
    (firstcuptr : int ref) (firstcuflagsptr : int ref) (reqcuptr : int ref)
    (reqcuflagsptr : int ref) (_bcptr : branch_chain option)
    (_open_caps : open_capitem option) (cb : compile_block)
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

      (* pcre2_compile.c:5806-5809. note_group_empty = FALSE and
         skipunits = 0 join with the group arms (chunk D). *)
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
        || Int.equal meta Parse.meta_class_not
        || Int.equal meta Parse.meta_class
      then (
        (* pcre2_compile.c:5860-6484 — character classes: M1 chunk
           compile_branch B. Deferred loudly, identically in both phases. *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
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
      else if Int.equal meta Parse.meta_nocapture then (
        (* pcre2_compile.c:6806-7018 — non-capturing bracket and the
           GROUP_PROCESS machinery: M1 chunk compile_branch D. Deferred
           loudly. *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
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
        (* pcre2_compile.c:7178-8011 — repetition (META_ASTERISK ..
           META_MINMAX_QUERY): M1 chunk compile_branch C. Deferred
           loudly. *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
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
        (* pcre2_compile.c:8090-8098 — capturing parentheses: M1 chunk
           compile_branch D. Deferred loudly. *)
        errorcodeptr := Parse.err_deferred;
        return_from_branch 0)
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

  (* Deferred arms fail loudly in both phases: classes (chunk B),
     quantifiers (chunk C), groups (chunk D), backrefs (M2), and the
     Unicode caseless-literal path (M6/M7 DEVIATION in compile_branch). *)
  expect_deferred "[ab]";
  expect_deferred "a*";
  expect_deferred "(a)";
  expect_deferred "(?:a)";
  expect_deferred "\\1()" (* META_BACKREF comes first in this stream *);
  expect_deferred ~options:(Options.ucp lor Options.caseless) "k"
