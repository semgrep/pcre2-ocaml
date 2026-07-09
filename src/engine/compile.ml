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
