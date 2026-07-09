(* Parse phase of the pure-OCaml PCRE2 10.44 port: pattern -> META stream.

   Ported from the front half of vendor/pcre2/src/pcre2_compile.c. This
   module currently holds the parsed-pattern encoding scheme (META_* codes,
   meta_extra_lengths) and the parse-phase helper functions read_number,
   read_repeat_counts, check_posix_syntax, check_posix_name and read_name,
   plus the minimal parse_context they thread state through. parse_regex
   itself lands in later chunks.

   The pattern is a `string` of 8-bit code units (this port is the 8-bit
   library); pointers become int indices into that string. *)

(* ---------- Parsed-pattern element manipulation ---------- *)

(* pcre2_compile.c:111-115 — macros for manipulating elements of the parsed
   pattern vector (32-bit unsigned ints in C; ints holding 0..2^32-1 here). *)
let meta_code x = x land 0xffff_0000
let meta_data x = x land 0x0000_ffff
let meta_diff x y = (x - y) lsr 16

(* pcre2_compile.c:87-109 — PUTOFFSET/GETOFFSET store a PCRE2_SIZE (a
   pattern offset) in the parsed pattern; SIZEOFFSET is how many elements
   that takes.
   DEVIATION: the vendored 64-bit build uses two uint32 elements
   (pcre2_compile.c:98-108, SIZEOFFSET = 2) because size_t does not fit in
   one. An OCaml int holds any supported pattern offset in a single
   parsed-pattern element, so this port uses the C's own
   PCRE2_SIZE_MAX <= UINT32_MAX configuration (pcre2_compile.c:91-97,
   SIZEOFFSET = 1). The parsed pattern is internal only, so the layout
   difference is not observable. *)
let sizeoffset = 1

(* ---------- META codes for parsed patterns ---------- *)

(* pcre2_compile.c:202-300 — code values for parsed patterns, stored in a
   vector of 32-bit unsigned ints. Values less than META_END are literal
   data values. The coding for identifying the item is in the top 16 bits,
   leaving 16 bits for the additional data that some of them need.

   NOTE (as in the C): when these definitions are changed, the table of
   extra lengths for each code (meta_extra_lengths, just below) must be
   updated to remain in step. *)

let meta_end = 0x8000_0000 (* End of pattern *)
let meta_alt = 0x8001_0000 (* alternation *)
let meta_atomic = 0x8002_0000 (* atomic group *)
let meta_backref = 0x8003_0000 (* Back ref *)
let meta_backref_byname = 0x8004_0000 (* \k'name' *)
let meta_bigvalue = 0x8005_0000 (* Next is a literal > META_END *)
let meta_callout_number = 0x8006_0000 (* (?C with numerical argument *)
let meta_callout_string = 0x8007_0000 (* (?C with string argument *)
let meta_capture = 0x8008_0000 (* Capturing parenthesis *)
let meta_circumflex = 0x8009_0000 (* ^ metacharacter *)
let meta_class = 0x800a_0000 (* start non-empty class *)
let meta_class_empty = 0x800b_0000 (* empty class *)
let meta_class_empty_not = 0x800c_0000 (* negative empty class *)
let meta_class_end = 0x800d_0000 (* end of non-empty class *)
let meta_class_not = 0x800e_0000 (* start non-empty negative class *)
let meta_cond_assert = 0x800f_0000 (* (?(?assertion)... *)
let meta_cond_define = 0x8010_0000 (* (?(DEFINE)... *)
let meta_cond_name = 0x8011_0000 (* (?(<name>)... *)
let meta_cond_number = 0x8012_0000 (* (?(digits)... *)
let meta_cond_rname = 0x8013_0000 (* (?(R&name)... *)
let meta_cond_rnumber = 0x8014_0000 (* (?(Rdigits)... *)
let meta_cond_version = 0x8015_0000 (* (?(VERSION<op>x.y)... *)
let meta_dollar = 0x8016_0000 (* $ metacharacter *)
let meta_dot = 0x8017_0000 (* . metacharacter *)
let meta_escape = 0x8018_0000 (* \d and friends *)
let meta_ket = 0x8019_0000 (* closing parenthesis *)
let meta_nocapture = 0x801a_0000 (* no capture parens *)
let meta_options = 0x801b_0000 (* (?i) and friends *)
let meta_posix = 0x801c_0000 (* POSIX class item *)
let meta_posix_neg = 0x801d_0000 (* negative POSIX class item *)
let meta_range_escaped = 0x801e_0000 (* range with at least one escape *)
let meta_range_literal = 0x801f_0000 (* range defined literally *)
let meta_recurse = 0x8020_0000 (* Recursion *)
let meta_recurse_byname = 0x8021_0000 (* (?&name) *)
let meta_script_run = 0x8022_0000 (* ( *script_run:...) *)

(* pcre2_compile.c:248-254 — these must be kept together to make it easy to
   check that an assertion is present where expected in a conditional
   group. *)
let meta_lookahead = 0x8023_0000 (* (?= *)
let meta_lookaheadnot = 0x8024_0000 (* (?! *)
let meta_lookbehind = 0x8025_0000 (* (?<= *)
let meta_lookbehindnot = 0x8026_0000 (* (?<! *)

(* pcre2_compile.c:256-259 — these cannot be conditions. *)
let meta_lookahead_na = 0x8027_0000 (* ( *napla: *)
let meta_lookbehind_na = 0x8028_0000 (* ( *naplb: *)

(* pcre2_compile.c:261-275 — these must be kept in this order, with
   consecutive values, and the _ARG versions of COMMIT, PRUNE, SKIP, and
   THEN immediately after their non-argument versions. *)
let meta_mark = 0x8029_0000 (* ( *MARK) *)
let meta_accept = 0x802a_0000 (* ( *ACCEPT) *)
let meta_fail = 0x802b_0000 (* ( *FAIL) *)
let meta_commit = 0x802c_0000
let meta_commit_arg = 0x802d_0000
let meta_prune = 0x802e_0000
let meta_prune_arg = 0x802f_0000
let meta_skip = 0x8030_0000
let meta_skip_arg = 0x8031_0000
let meta_then = 0x8032_0000
let meta_then_arg = 0x8033_0000

(* pcre2_compile.c:277-290 — these must be kept in groups of adjacent 3
   values, and all together. *)
let meta_asterisk = 0x8034_0000 (* *  *)
let meta_asterisk_plus = 0x8035_0000 (* *+ *)
let meta_asterisk_query = 0x8036_0000 (* *? *)
let meta_plus = 0x8037_0000 (* +  *)
let meta_plus_plus = 0x8038_0000 (* ++ *)
let meta_plus_query = 0x8039_0000 (* +? *)
let meta_query = 0x803a_0000 (* ?  *)
let meta_query_plus = 0x803b_0000 (* ?+ *)
let meta_query_query = 0x803c_0000 (* ?? *)
let meta_minmax = 0x803d_0000 (* {n,m}  repeat *)
let meta_minmax_plus = 0x803e_0000 (* {n,m}+ repeat *)
let meta_minmax_query = 0x803f_0000 (* {n,m}? repeat *)

(* pcre2_compile.c:292-293 *)
let meta_first_quantifier = meta_asterisk
let meta_last_quantifier = meta_minmax_query

(* pcre2_compile.c:295-300 — a special "meta code" used only to distinguish
   ( *asr: from ( *sr: in the table of alphabetic assertions. It is never
   stored in the parsed pattern because ( *asr: is turned into
   ( *sr:( *atomic: at that stage. There is therefore no need for it to have
   a length entry, so it uses a high value. *)
let meta_atomic_script_run = 0x8fff_0000

(* pcre2_compile.c:302-371 — table of extra lengths for each of the meta
   codes, indexed by (code >> 16) & 0x7fff (see pcre2_compile.c:9336-9338).
   Must be kept in step with the definitions above. For some items these
   values are a basic length to which a variable amount has to be added. *)
let meta_extra_lengths =
  [|
    0 (* META_END *);
    0 (* META_ALT *);
    0 (* META_ATOMIC *);
    0 (* META_BACKREF - more if group is >= 10 *);
    1 + sizeoffset (* META_BACKREF_BYNAME *);
    1 (* META_BIGVALUE *);
    3 (* META_CALLOUT_NUMBER *);
    3 + sizeoffset (* META_CALLOUT_STRING *);
    0 (* META_CAPTURE *);
    0 (* META_CIRCUMFLEX *);
    0 (* META_CLASS *);
    0 (* META_CLASS_EMPTY *);
    0 (* META_CLASS_EMPTY_NOT *);
    0 (* META_CLASS_END *);
    0 (* META_CLASS_NOT *);
    0 (* META_COND_ASSERT *);
    sizeoffset (* META_COND_DEFINE *);
    1 + sizeoffset (* META_COND_NAME *);
    1 + sizeoffset (* META_COND_NUMBER *);
    1 + sizeoffset (* META_COND_RNAME *);
    1 + sizeoffset (* META_COND_RNUMBER *);
    3 (* META_COND_VERSION *);
    0 (* META_DOLLAR *);
    0 (* META_DOT *);
    0 (* META_ESCAPE - more for ESC_P, ESC_p, ESC_g, ESC_k *);
    0 (* META_KET *);
    0 (* META_NOCAPTURE *);
    1 (* META_OPTIONS *);
    1 (* META_POSIX *);
    1 (* META_POSIX_NEG *);
    0 (* META_RANGE_ESCAPED *);
    0 (* META_RANGE_LITERAL *);
    sizeoffset (* META_RECURSE *);
    1 + sizeoffset (* META_RECURSE_BYNAME *);
    0 (* META_SCRIPT_RUN *);
    0 (* META_LOOKAHEAD *);
    0 (* META_LOOKAHEADNOT *);
    sizeoffset (* META_LOOKBEHIND *);
    sizeoffset (* META_LOOKBEHINDNOT *);
    0 (* META_LOOKAHEAD_NA *);
    sizeoffset (* META_LOOKBEHIND_NA *);
    1 (* META_MARK - plus the string length *);
    0 (* META_ACCEPT *);
    0 (* META_FAIL *);
    0 (* META_COMMIT *);
    1 (* META_COMMIT_ARG - plus the string length *);
    0 (* META_PRUNE *);
    1 (* META_PRUNE_ARG - plus the string length *);
    0 (* META_SKIP *);
    1 (* META_SKIP_ARG - plus the string length *);
    0 (* META_THEN *);
    1 (* META_THEN_ARG - plus the string length *);
    0 (* META_ASTERISK *);
    0 (* META_ASTERISK_PLUS *);
    0 (* META_ASTERISK_QUERY *);
    0 (* META_PLUS *);
    0 (* META_PLUS_PLUS *);
    0 (* META_PLUS_QUERY *);
    0 (* META_QUERY *);
    0 (* META_QUERY_PLUS *);
    0 (* META_QUERY_QUERY *);
    2 (* META_MINMAX *);
    2 (* META_MINMAX_PLUS *);
    2 (* META_MINMAX_QUERY *);
  |]

(* ---------- Small character helpers ---------- *)

(* pcre2_compile.c:408 — IS_DIGIT(x). *)
let is_digit (c : char) =
  let c = Char.code c in
  c >= 0x30 && c <= 0x39

(* CHAR_SPACE / CHAR_HT test (pcre2_internal.h:696,703) used for skipping
   white space in quantifiers and braced names, e.g. pcre2_compile.c:1423. *)
let space_or_tab (c : char) = Char.equal c ' ' || Char.equal c '\t'

(* ---------- Escape codes (the C's ESC_ constants) ---------- *)

(* pcre2_internal.h:1338-1364 — escaped items that aren't just an encoding
   of a particular data value such as \n. They must have non-zero values, as
   check_escape() returns 0 for a data character. In the escapes table below
   their values are negated in order to distinguish them from data values.

   They must appear here in the same order as in the opcode definitions
   (Opcodes.op_sod onward), up to ESC_z (asserted at the end of this
   module). ESC_dum is a dummy for OP_ALLANY, which corresponds to "." in
   DOTALL mode rather than an escape sequence.

   ESC_ub is a special return from check_escape() when, in BSUX mode, \u{ is
   not followed by hex digits and }, in which case it should mean a literal
   "u" followed by a literal "{".

   Negative numbers are used to encode a backreference (\1, \2, \3, etc.) in
   check_escape().

   Naming: OCaml values must start with a lowercase letter, so the C's
   uppercase-letter names become esc_big_* (ESC_A -> esc_big_a); the
   lowercase-letter names map directly (ESC_b -> esc_b). *)
let esc_big_a = 1
let esc_big_g = 2
let esc_big_k = 3
let esc_big_b = 4
let esc_b = 5
let esc_big_d = 6
let esc_d = 7
let esc_big_s = 8
let esc_s = 9
let esc_big_w = 10
let esc_w = 11
let esc_big_n = 12
let esc_dum = 13
let esc_big_c = 14
let esc_big_p = 15
let esc_p = 16
let esc_big_r = 17
let esc_big_h = 18
let esc_h = 19
let esc_big_v = 20
let esc_v = 21
let esc_big_x = 22
let esc_big_z = 23
let esc_z = 24
let esc_big_e = 25
let esc_big_q = 26
let esc_g = 27
let esc_k = 28
let esc_ub = 29

(* ---------- Escape lookup tables ---------- *)

(* pcre2_compile.c:410-455 — table to identify hex digits (xdigitab). The
   tables in chartables are dependent on the locale, and may mark arbitrary
   characters as digits; only 0-9, a-f, and A-F are recognized as hex digits
   here. The value in the table is the binary hex digit value, or 0xff for
   non-hex digits. This is the "normal" (non-EBCDIC) table. *)
let xdigitab =
  [|
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*   0-  7 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*   8- 15 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*  16- 23 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*  24- 31 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*    - '  *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*  ( - /  *)
    0x00;
    0x01;
    0x02;
    0x03;
    0x04;
    0x05;
    0x06;
    0x07;
    (*  0 - 7  *)
    0x08;
    0x09;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*  8 - ?  *)
    0xff;
    0x0a;
    0x0b;
    0x0c;
    0x0d;
    0x0e;
    0x0f;
    0xff;
    (*  @ - G  *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*  H - O  *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*  P - W  *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*  X - _  *)
    0xff;
    0x0a;
    0x0b;
    0x0c;
    0x0d;
    0x0e;
    0x0f;
    0xff;
    (*  ` - g  *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*  h - o  *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*  p - w  *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (*  x -127 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 128-135 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 136-143 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 144-151 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 152-159 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 160-167 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 168-175 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 176-183 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 184-191 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 192-199 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 200-207 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 208-215 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 216-223 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 224-231 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 232-239 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 240-247 *)
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    0xff;
    (* 248-255 *)
  |]

(* pcre2_compile.c:74 — XDIGIT(c), 8-bit mode: a plain table lookup (every
   code unit is <= 255). *)
let xdigit (c : int) : int = xdigitab.(c)

(* pcre2_compile.c:498-550 — table for handling alphanumeric escaped
   characters. Positive returns are simple data values; negative values are
   for special things like \d and so on. Zero means further processing is
   needed (for things like \x), or the escape is invalid. This is the
   "normal" table for ASCII systems, running from '0' to 'z'
   (ESCAPES_FIRST/ESCAPES_LAST, pcre2_compile.c:507-508). *)
let escapes_first = 0x30 (* CHAR_0 *)
let escapes_last = 0x7a (* CHAR_z *)

let escapes =
  [|
    0 (* 0 *);
    0 (* 1 *);
    0 (* 2 *);
    0 (* 3 *);
    0 (* 4 *);
    0 (* 5 *);
    0 (* 6 *);
    0 (* 7 *);
    0 (* 8 *);
    0 (* 9 *);
    0x3a (* : *);
    0x3b (* ; *);
    0x3c (* < *);
    0x3d (* = *);
    0x3e (* > *);
    0x3f (* ? *);
    0x40 (* @ *);
    -esc_big_a;
    -esc_big_b;
    -esc_big_c;
    -esc_big_d;
    -esc_big_e;
    0 (* F *);
    -esc_big_g;
    -esc_big_h;
    0 (* I *);
    0 (* J *);
    -esc_big_k;
    0 (* L *);
    0 (* M *);
    -esc_big_n;
    0 (* O *);
    -esc_big_p;
    -esc_big_q;
    -esc_big_r;
    -esc_big_s;
    0 (* T *);
    0 (* U *);
    -esc_big_v;
    -esc_big_w;
    -esc_big_x;
    0 (* Y *);
    -esc_big_z;
    0x5b (* [ *);
    0x5c (* \ *);
    0x5d (* ] *);
    0x5e (* ^ *);
    0x5f (* _ *);
    0x60 (* ` *);
    0x07 (* a -> BEL *);
    -esc_b;
    0 (* c *);
    -esc_d;
    0x1b (* e -> ESC *);
    0x0c (* f -> FF *);
    0 (* g *);
    -esc_h;
    0 (* i *);
    0 (* j *);
    -esc_k;
    0 (* l *);
    0 (* m *);
    0x0a (* n -> LF *);
    0 (* o *);
    -esc_p;
    0 (* q *);
    0x0d (* r -> CR *);
    -esc_s;
    0x09 (* t -> HT *);
    0 (* u *);
    -esc_v;
    -esc_w;
    0 (* x *);
    0 (* y *);
    -esc_z;
  |]

(* pcre2_internal.h:545 — PCRE2_HASBKPORX, the compiled-pattern flag bit
   recorded in cb->external_flags when \P, \p, or \X is seen. The rest of
   the internal flag set (pcre2_internal.h:525-548) will be ported with the
   compile_block in the compile-phase chunks. *)
let hasbkporx = 0x0010_0000

(* ---------- POSIX class names ---------- *)

(* pcre2_compile.c:693-707 — tables of names of POSIX character classes and
   their lengths. The C keeps the names in a single \0-separated string;
   an array of strings is equivalent here. The list of lengths is terminated
   by a zero length entry. The first three must be alpha, lower, upper, as
   this is assumed for handling case independence. *)
let posix_names =
  [|
    "alpha";
    "lower";
    "upper";
    "alnum";
    "ascii";
    "blank";
    "cntrl";
    "digit";
    "graph";
    "print";
    "punct";
    "space";
    "word";
    "xdigit";
  |]

let posix_name_lengths = [| 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 4; 6; 0 |]

(* ---------- Parse context ---------- *)

(* Threads the state the C passes to the parse helpers: the pattern and its
   end (parse_regex's ptr/ptrend over cb->start_pattern..cb->end_pattern),
   the canonical read position (a local `ptr` in parse_regex,
   pcre2_compile.c:2773), and the errorcode/erroroffset plumbing
   (C: *errorcodeptr and cb->erroroffset). The helpers below take an explicit
   `int ref` read pointer exactly as the C passes &ptr / &p / &tempptr, so
   probing calls on temporary pointers work; parse_regex keeps the main
   position in [ptr]. *)
type parse_context = {
  pattern : string; (* cb->start_pattern; indices are pattern offsets *)
  ptrend : int; (* one past the last code unit of the pattern *)
  mutable ptr : int; (* parse_regex's main read position *)
  mutable errorcode : int; (* *errorcodeptr *)
  mutable erroroffset : int; (* cb->erroroffset *)
  (* Until the full compile_block record lands (compile-phase chunks),
     parse_context also carries the two compile_block fields check_escape
     touches (pcre2_intmodedep.h:735,743): *)
  mutable bracount : int; (* cb->bracount: capturing groups seen so far *)
  mutable external_flags : int; (* cb->external_flags: hasbkporx etc. *)
}

let make_context (pattern : string) : parse_context =
  {
    pattern;
    ptrend = String.length pattern;
    ptr = 0;
    errorcode = 0;
    erroroffset = 0;
    bracount = 0;
    external_flags = 0;
  }

(* Local control-flow exception modeling the C's forward "goto EXIT" /
   "goto FAILED" jumps to a shared function epilogue (port-conventions §2).
   Raised and caught within a single helper below; never escapes this
   module. Compile-time code only (§6 permits exceptions outside the
   interpreter frame loop). *)
exception Goto_exit

(* Local control-flow exception modeling check_escape's two mid-function
   `return 0` statements (pcre2_compile.c:1666,2130-2132), which skip the
   shared exit epilogue at pcre2_compile.c:2136-2140. Raised and caught
   within check_escape only; never escapes this module. *)
exception Return_zero

(* ---------- Read a number, possibly signed ---------- *)

(* pcre2_compile.c:1301-1384 — read_number. Reads numbers in the pattern;
   the initial pointer must be at the sign or first digit of the number.
   When relative values (introduced by + or -) are allowed, they are
   relative group numbers, and the result must be greater than zero.

   Arguments:
     cx          pattern / ptrend / errorcode (C: ptrend, errorcodeptr)
     ptrptr      the character pointer variable (C: PCRE2_SPTR *ptrptr)
     allow_sign  if < 0, sign not allowed; if >= 0, sign is relative to this
     max_value   the largest number allowed (uint32 in C)
     max_error   the error to give for an over-large number
     intptr      where to put the result

   Returns:      true  - a number was read
                 false - errorcode = 0 => no number was found
                         errorcode <> 0 => an error occurred *)
let read_number (cx : parse_context) (ptrptr : int ref) ~(allow_sign : int)
    ~(max_value : int) ~(max_error : int) (intptr : int ref) : bool =
  let sign = ref 0 in
  let n = ref 0 in
  let ptr = ref !ptrptr in
  let yield = ref false in
  let max_value = ref max_value in

  cx.errorcode <- 0;

  (* pcre2_compile.c:1335-1348 *)
  if allow_sign >= 0 && !ptr < cx.ptrend then
    if Char.equal cx.pattern.[!ptr] '+' then (
      sign := 1;
      (* uint32 subtraction (pcre2_compile.c:1340). It cannot wrap for the
         reachable call sites (allow_sign = cb->bracount <= MAX_GROUP_NUMBER
         = max_value), but mask for C parity. *)
      max_value := (!max_value - allow_sign) land 0xffff_ffff;
      incr ptr)
    else if Char.equal cx.pattern.[!ptr] '-' then (
      sign := -1;
      incr ptr);

  (* pcre2_compile.c:1350 — early return: no out-parameter writes. *)
  if !ptr >= cx.ptrend || not (is_digit cx.pattern.[!ptr]) then false
  else (
    (try
       (* pcre2_compile.c:1351-1359 *)
       while !ptr < cx.ptrend && is_digit cx.pattern.[!ptr] do
         (* n is uint32 in C; mask keeps wraparound parity. *)
         n :=
           ((!n * 10) + Char.code cx.pattern.[!ptr] - Char.code '0')
           land 0xffff_ffff;
         incr ptr;
         if !n > !max_value then (
           cx.errorcode <- max_error;
           raise_notrace Goto_exit)
       done;

       (* pcre2_compile.c:1361-1376 *)
       if allow_sign >= 0 && not (Int.equal !sign 0) then (
         if Int.equal !n 0 then (
           cx.errorcode <- Errors.err26;
           (* +0 and -0 are not allowed *)
           raise_notrace Goto_exit);
         if !sign > 0 then n := !n + allow_sign
         else if !n > allow_sign then (
           (* C compares (int)n > allow_sign; the cast is a no-op here
              because n <= max_value <= MAX_GROUP_NUMBER on this path. *)
           cx.errorcode <- Errors.err15;
           (* Non-existent subpattern *)
           raise_notrace Goto_exit)
         else n := allow_sign + 1 - !n);

       yield := true
     with Goto_exit -> ());

    (* EXIT: pcre2_compile.c:1380-1383 *)
    intptr := !n;
    ptrptr := !ptr;
    !yield)

(* ---------- Read repeat counts ---------- *)

(* pcre2_compile.c:1388-1514 — read_repeat_counts. Reads an item of the form
   {n,m} and returns the values through minp/maxp when they are not None
   (C: non-NULL pointers). Repeat counts must be less than 65536
   (MAX_REPEAT_COUNT); a larger value (REPEAT_UNLIMITED) is used for
   "unlimited". Either n or m may be absent, but not both. Spaces and tabs
   are allowed after { and before } and between the numbers and the comma.

   Arguments:
     cx        pattern / ptrend / errorcode
     ptrptr    points to the index of the character after '{'
     minp      if not None, where to put min
     maxp      if not None, where to put max

   Returns:    false if not a repeat quantifier, errorcode set zero
               false on error, with errorcode set non-zero
               true on success, with pointer updated to point after '}' *)
let read_repeat_counts (cx : parse_context) (ptrptr : int ref)
    (minp : int ref option) (maxp : int ref option) : bool =
  let p = ref !ptrptr in
  let yield = ref false in
  let min = ref 0 in
  (* This value is larger than MAX_REPEAT_COUNT (pcre2_compile.c:1420). *)
  let max = ref Limits.repeat_unlimited in

  cx.errorcode <- 0;
  (* pcre2_compile.c:1423 *)
  while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
    incr p
  done;

  (* pcre2_compile.c:1425-1455 — check the syntax before interpreting.
     Otherwise, a non-quantifier sequence such as "X{123456ABC" would
     incorrectly give a "number too big in quantifier" error. All the
     early `return FALSE`s in this block leave *ptrptr untouched. *)
  let syntax_ok =
    let pp = ref !p in
    let had_minimum = ref false in
    (* pcre2_compile.c:1430-1434 *)
    if !pp < cx.ptrend && is_digit cx.pattern.[!pp] then (
      had_minimum := true;
      incr pp;
      while !pp < cx.ptrend && is_digit cx.pattern.[!pp] do
        incr pp
      done);
    (* pcre2_compile.c:1436-1437 *)
    while !pp < cx.ptrend && space_or_tab cx.pattern.[!pp] do
      incr pp
    done;
    if !pp >= cx.ptrend then false
    else if Char.equal cx.pattern.[!pp] '}' then
      (* pcre2_compile.c:1439-1442 *)
      !had_minimum
    else if not (Char.equal cx.pattern.[!pp] ',') then false
    else (
      (* pcre2_compile.c:1445-1454 *)
      incr pp;
      (* the C's *pp++ != CHAR_COMMA consumed the comma *)
      while !pp < cx.ptrend && space_or_tab cx.pattern.[!pp] do
        incr pp
      done;
      if !pp >= cx.ptrend then false
      else
        let digits_ok =
          if is_digit cx.pattern.[!pp] then (
            incr pp;
            while !pp < cx.ptrend && is_digit cx.pattern.[!pp] do
              incr pp
            done;
            true)
          else !had_minimum
        in
        if not digits_ok then false
        else (
          while !pp < cx.ptrend && space_or_tab cx.pattern.[!pp] do
            incr pp
          done;
          !pp < cx.ptrend && Char.equal cx.pattern.[!pp] '}'))
  in
  if not syntax_ok then false
  else (
    (* pcre2_compile.c:1457-1499 — now process the quantifier for real. We
       know it must be {n} or {n,} or {,m} or {n,m}. The only error that
       read_number() can return is for a number that is too big. If
       errorcode is returned as zero it means no number was found. *)
    (try
       (* Deal with {,m} or n too big. If we successfully read m there is no
          need to check m >= n because n defaults to zero
          (pcre2_compile.c:1461-1473). *)
       if
         not
           (read_number cx p ~allow_sign:(-1) ~max_value:Limits.max_repeat_count
              ~max_error:Errors.err5 min)
       then (
         if not (Int.equal cx.errorcode 0) then raise_notrace Goto_exit;
         (* n too big *)
         incr p;
         (* Skip comma and subsequent spaces *)
         while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
           incr p
         done;
         if
           not
             (read_number cx p ~allow_sign:(-1)
                ~max_value:Limits.max_repeat_count ~max_error:Errors.err5 max)
         then
           if not (Int.equal cx.errorcode 0) then
             raise_notrace Goto_exit (* m too big *))
       else (
         (* Have read one number. Deal with {n} or {n,} or {n,m}
            (pcre2_compile.c:1477-1499). *)
         while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
           incr p
         done;
         (* safe without a ptrend test (as in C, pcre2_compile.c:1480): the
            syntax pre-check guaranteed a terminating '}' before ptrend. *)
         if Char.equal cx.pattern.[!p] '}' then max := !min
         else (
           (* Handle {n,} or {n,m} *)
           incr p;
           (* Skip comma and subsequent spaces *)
           while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
             incr p
           done;
           if
             not
               (read_number cx p ~allow_sign:(-1)
                  ~max_value:Limits.max_repeat_count ~max_error:Errors.err5 max)
           then
             if not (Int.equal cx.errorcode 0) then
               raise_notrace Goto_exit (* m too big *);
           if !max < !min then (
             cx.errorcode <- Errors.err4;
             raise_notrace Goto_exit)));

       (* Valid quantifier exists (pcre2_compile.c:1501-1507). *)
       while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
         incr p
       done;
       incr p;
       yield := true;
       (match minp with Some r -> r := !min | None -> ());
       match maxp with Some r -> r := !max | None -> ()
     with Goto_exit -> ());

    (* EXIT: pcre2_compile.c:1511-1513 — update the pattern pointer. *)
    ptrptr := !p;
    !yield)

(* ---------- Handle escapes ---------- *)

(* pcre2_compile.c:1518-1548 — check_escape. Called when a \ has been
   encountered. It either returns a positive value for a simple escape such
   as \d, or 0 for a data character, which is placed in chptr. A
   backreference to group n is returned as negative n. On entry, ptr is
   pointing at the character after \. On exit, it points after the final
   code unit of the escape sequence.

   This function is also called from pcre2_substitute() to handle escape
   sequences in replacement strings. In this case, the cb argument is None
   (C: NULL), and in the case of escapes that have further processing, only
   sequences that define a data character are recognised. The isclass
   argument is not relevant; the options argument is the final value of the
   compiled pattern's options.

   Arguments:
     cx             pattern / ptrend / errorcode (C: ptrend, errorcodeptr)
     ptrptr         the input position pointer
     chptr          where to put a returned data character
     options        the current options bits
     xoptions       the current extra options bits
     isclass        true if inside a character class
     cb             compile data block (the parse context doubling as one)
                    or None when called from pcre2_substitute()

   Returns:         zero => a data character
                    positive => a special escape sequence
                    negative => a numerical back reference
                    on error, cx.errorcode is set non-zero *)
let check_escape (cx : parse_context) (ptrptr : int ref) (chptr : int ref)
    ~(options : int) ~(xoptions : int) ~(isclass : bool)
    (cb : parse_context option) : int =
  (* pcre2_compile.c:1555-1561 *)
  let utf = not (Int.equal (options land Options.utf) 0) in
  let alt_bsux =
    ref
      (not
         (Int.equal
            (options land Options.alt_bsux
            lor (xoptions land Options.extra_alt_bsux))
            0))
  in
  let ptr = ref !ptrptr in
  let c = ref 0 in
  let escape = ref 0 in

  (* pcre2_compile.c:1995-2050 — the COME_FROM_NU label and the rest of the
     \x{} processing: scan hex digits up to the closing brace. Shared
     between Perl-style \x{ handling and the \N{U+ jump at
     pcre2_compile.c:1629 (goto COME_FROM_NU). On entry ptr points at the
     first code unit after "\x{" + optional space/tab, or after "\N{U+". *)
  let hex_brace_tail () =
    if !ptr >= cx.ptrend || Char.equal cx.pattern.[!ptr] '}' then
      cx.errorcode <- Errors.err78
    else (
      c := 0;
      let overflow = ref false in
      (* pcre2_compile.c:2005-2018 *)
      let scanning = ref true in
      while
        !scanning && !ptr < cx.ptrend
        && not (Int.equal (xdigit (Char.code cx.pattern.[!ptr])) 0xff)
      do
        let cc = xdigit (Char.code cx.pattern.[!ptr]) in
        incr ptr;
        if Int.equal !c 0 && Int.equal cc 0 then () (* Leading zeroes *)
        else (
          c := (!c lsl 4) lor cc;
          if
            (utf && !c > 0x10ffff) || ((not utf) && !c > Limits.max_non_utf_char)
          then (
            overflow := true;
            scanning := false))
      done;

      (* Perl ignores spaces and tabs before } (pcre2_compile.c:2022). *)
      while !ptr < cx.ptrend && space_or_tab cx.pattern.[!ptr] do
        incr ptr
      done;

      (* On overflow, skip remaining hex digits (pcre2_compile.c:2026-2030). *)
      if !overflow then (
        while
          !ptr < cx.ptrend
          && not (Int.equal (xdigit (Char.code cx.pattern.[!ptr])) 0xff)
        do
          incr ptr
        done;
        cx.errorcode <- Errors.err34)
      else if !ptr < cx.ptrend then (
        (* C: else if (ptr < ptrend && *ptr++ == '}') — the increment
           happens whether or not the code unit is '}'. *)
        let ch = cx.pattern.[!ptr] in
        incr ptr;
        if Char.equal ch '}' then (
          if
            (* pcre2_compile.c:2031-2039 *)
            utf && !c >= 0xd800 && !c <= 0xdfff
            && Int.equal (xoptions land Options.extra_allow_surrogate_escapes) 0
          then (
            decr ptr;
            cx.errorcode <- Errors.err73))
        else (
          (* If the sequence of hex digits (followed by optional space) does
             not end with '}', give an error (pcre2_compile.c:2041-2050). *)
          decr ptr;
          cx.errorcode <- Errors.err67))
      else (
        decr ptr;
        cx.errorcode <- Errors.err67))
  in

  (* If backslash is at the end of the string, it's an error
     (pcre2_compile.c:1563-1569). Neither ptrptr nor chptr is written. *)
  if !ptr >= cx.ptrend then (
    cx.errorcode <- Errors.err1;
    0)
  else
    try
      (* pcre2_compile.c:1571-1572 — GETCHARINCTEST: get character value,
         increment pointer. M6: in UTF mode this must decode a UTF-8
         character (pcre2_intmodedep.h:322-325); until utf.ml lands, the
         byte read below is the 8-bit non-UTF expansion
         (pcre2_intmodedep.h:264). UTF compilation is rejected before parse
         in the current engine, so the utf-true decode is unreachable. *)
      c := Char.code cx.pattern.[!ptr];
      incr ptr;
      cx.errorcode <- 0 (* Be optimistic *);

      (* Non-alphanumerics are literals, so we just leave the value in c
         (pcre2_compile.c:1574-1578). *)
      (if !c < escapes_first || !c > escapes_last then ()
         (* Definitely literal *)
       else
         (* Otherwise, do a table lookup (pcre2_compile.c:1580-1646). *)
         let i = escapes.(!c - escapes_first) in
         if not (Int.equal i 0) then
           if i > 0 then (
             (* pcre2_compile.c:1588-1593 *)
             c := i;
             if
               Int.equal !c 0x0d (* CHAR_CR *)
               && not
                    (Int.equal (xoptions land Options.extra_escaped_cr_is_lf) 0)
             then c := 0x0a (* CHAR_LF *))
           else (
             (* Negative table entry: return a special escape
                (pcre2_compile.c:1594-1598). *)
             escape := -i;
             (match cb with
             | Some cbv
               when Int.equal !escape esc_big_p
                    || Int.equal !escape esc_p
                    || Int.equal !escape esc_big_x ->
                 (* Note \P, \p, or \X *)
                 cbv.external_flags <- cbv.external_flags lor hasbkporx
             | _ -> ());

             (* Perl supports \N{name} for character names and \N{U+dddd}
                for numerical Unicode code points. PCRE does not support
                \N{name}, but it does support quantification such as
                \N{2,3}, so if \N{ is not followed by U+dddd we check for a
                quantifier (pcre2_compile.c:1600-1644). *)
             if
               Int.equal !escape esc_big_n
               && !ptr < cx.ptrend
               && Char.equal cx.pattern.[!ptr] '{'
             then (
               let p = ref (!ptr + 1) in
               (* Perl ignores spaces and tabs after { *)
               while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
                 incr p
               done;
               (* \N{U+ can be handled by the \x{ code, but in Perl \N{U+
                  forces Unicode casing semantics for the entire pattern, so
                  allow it only in UTF (i.e. Unicode) mode
                  (pcre2_compile.c:1613-1633; non-EBCDIC branch). *)
               if
                 cx.ptrend - !p > 1
                 && Char.equal cx.pattern.[!p] 'U'
                 && Char.equal cx.pattern.[!p + 1] '+'
               then
                 if utf then (
                   ptr := !p + 2;
                   escape := 0 (* Not a fancy escape after all *);
                   hex_brace_tail () (* C: goto COME_FROM_NU *))
                 else cx.errorcode <- Errors.err93
               else if
                 (* Give an error if what follows is not a quantifier, but
                    don't override an error set by the quantifier reader
                    (pcre2_compile.c:1635-1643). *)
                 (not (read_repeat_counts cx p None None))
                 && Int.equal cx.errorcode 0
               then cx.errorcode <- Errors.err37))
         else (
           (* Escapes that need further processing, including those that are
              unknown, have a zero entry in the lookup table
              (pcre2_compile.c:1648-1669). *)

           (* Filter calls from pcre2_substitute(): only \c, \o, and \x are
              recognized (\u and \U can never appear as they are used for
              case forcing). *)
           (match cb with
           | None ->
               if
                 (not (Int.equal !c (Char.code 'c')))
                 && (not (Int.equal !c (Char.code 'o')))
                 && not (Int.equal !c (Char.code 'x'))
               then (
                 cx.errorcode <- Errors.err3;
                 raise_notrace Return_zero (* pcre2_compile.c:1665-1666 *));
               alt_bsux := false (* Do not modify \x handling *)
           | Some _ -> ());

           (* pcre2_compile.c:1671 — switch (c). Safe: escapes_first <= c <=
              escapes_last was checked above, so c is a valid char code. *)
           match Char.chr !c with
           | 'F' | 'l' | 'L' ->
               (* A number of Perl escapes are not handled by PCRE
                  (pcre2_compile.c:1673-1680). *)
               cx.errorcode <- Errors.err37
           | 'u' ->
               (* \u is unrecognized when neither PCRE2_ALT_BSUX nor
                  PCRE2_EXTRA_ALT_BSUX is set. Otherwise, \u must be followed
                  by exactly four hex digits or, if PCRE2_EXTRA_ALT_BSUX is
                  set, by any number of hex digits in braces. Otherwise it is
                  a lowercase u letter. Unlike other braced items, white
                  space is NOT allowed. When \u{ is not followed by hex
                  digits, a special return is given because otherwise \u{ 12}
                  (for example) would be treated as u{12}
                  (pcre2_compile.c:1682-1750). *)
               if not !alt_bsux then cx.errorcode <- Errors.err37
               else if !ptr >= cx.ptrend then () (* break: literal u *)
               else
                 (* checked_range is false on the C's inner breaks that skip
                    the range checks at pcre2_compile.c:1740-1748. *)
                 let checked_range = ref true in
                 (if
                    Char.equal cx.pattern.[!ptr] '{'
                    && not (Int.equal (xoptions land Options.extra_alt_bsux) 0)
                  then (
                    (* pcre2_compile.c:1696-1725 *)
                    let hptr = ref (!ptr + 1) in
                    let cc = ref 0 in
                    let scanning = ref true in
                    while
                      !scanning && !hptr < cx.ptrend
                      && not
                           (Int.equal
                              (xdigit (Char.code cx.pattern.[!hptr]))
                              0xff)
                    do
                      let xc = xdigit (Char.code cx.pattern.[!hptr]) in
                      if not (Int.equal (!cc land 0xf000_0000) 0) then (
                        (* Test for 32-bit overflow (pcre2_compile.c:1704-1709) *)
                        cx.errorcode <- Errors.err77;
                        ptr := !hptr (* Show where *);
                        (* *hptr != } will cause another break below *)
                        scanning := false)
                      else (
                        cc := (!cc lsl 4) lor xc;
                        incr hptr)
                    done;

                    if
                      Int.equal !hptr (!ptr + 1) (* No hex digits *)
                      || !hptr >= cx.ptrend (* Hit end of input *)
                      || not (Char.equal cx.pattern.[!hptr] '}')
                      (* No } terminator *)
                    then (
                      escape := esc_ub (* Special return *);
                      incr ptr (* Skip { *);
                      checked_range := false (* Hex escape not recognized *))
                    else (
                      c := !cc (* Accept the code point *);
                      ptr := !hptr + 1))
                  else if
                    (* Must be exactly 4 hex digits
                       (pcre2_compile.c:1727-1738). *)
                    cx.ptrend - !ptr < 4
                  then checked_range := false (* Less than 4 chars *)
                  else
                    let cc = xdigit (Char.code cx.pattern.[!ptr]) in
                    if Int.equal cc 0xff then checked_range := false
                    else
                      let xc = xdigit (Char.code cx.pattern.[!ptr + 1]) in
                      if Int.equal xc 0xff then checked_range := false
                      else
                        let cc = (cc lsl 4) lor xc in
                        let xc = xdigit (Char.code cx.pattern.[!ptr + 2]) in
                        if Int.equal xc 0xff then checked_range := false
                        else
                          let cc = (cc lsl 4) lor xc in
                          let xc = xdigit (Char.code cx.pattern.[!ptr + 3]) in
                          if Int.equal xc 0xff then checked_range := false
                          else (
                            c := (cc lsl 4) lor xc;
                            ptr := !ptr + 4));

                 if !checked_range then
                   (* pcre2_compile.c:1740-1748 *)
                   if utf then (
                     if !c > 0x10ffff then cx.errorcode <- Errors.err77
                     else if
                       !c >= 0xd800 && !c <= 0xdfff
                       && Int.equal
                            (xoptions land Options.extra_allow_surrogate_escapes)
                            0
                     then cx.errorcode <- Errors.err73)
                   else if !c > Limits.max_non_utf_char then
                     cx.errorcode <- Errors.err77
           | 'U' ->
               (* \U is unrecognized unless PCRE2_ALT_BSUX or
                  PCRE2_EXTRA_ALT_BSUX is set, in which case it is an upper
                  case letter (pcre2_compile.c:1752-1757). *)
               if not !alt_bsux then cx.errorcode <- Errors.err37
           | 'g' ->
               (* In a character class, \g is just a literal "g". Outside,
                  \g must be followed by a plain or braced number, \g{name}
                  (synonymous with \k{name}), or an Oniguruma-style name or
                  number in angle brackets or single quotes (a subroutine
                  call, returned as ESC_g). Return a negative number for a
                  numerical back reference, ESC_k for a named back reference,
                  and ESC_g for a named or numbered subroutine call
                  (pcre2_compile.c:1759-1838). *)
               if isclass then () (* break *)
               else if !ptr >= cx.ptrend then cx.errorcode <- Errors.err57
               else if
                 Char.equal cx.pattern.[!ptr] '<'
                 || Char.equal cx.pattern.[!ptr] '\''
               then escape := esc_g
               else
                 (* cb is non-NULL here: None (substitute) calls were
                    filtered to \c, \o, \x at pcre2_compile.c:1661-1669. *)
                 let bracount =
                   match cb with Some cbv -> cbv.bracount | None -> 0
                 in
                 let s = ref 0 in
                 let broke = ref false in
                 (* If there is a brace delimiter, try to read a numerical
                    reference. If there isn't one, assume we have a name and
                    treat it as \k (pcre2_compile.c:1795-1817). *)
                 if Char.equal cx.pattern.[!ptr] '{' then (
                   let p = ref (!ptr + 1) in
                   while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
                     incr p
                   done;
                   if
                     not
                       (read_number cx p ~allow_sign:bracount
                          ~max_value:Limits.max_group_number
                          ~max_error:Errors.err61 s)
                   then (
                     if Int.equal cx.errorcode 0 then
                       escape := esc_k (* No number found *);
                     broke := true)
                   else (
                     while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
                       incr p
                     done;
                     if !p >= cx.ptrend || not (Char.equal cx.pattern.[!p] '}')
                     then (
                       cx.errorcode <- Errors.err57;
                       broke := true)
                     else ptr := !p + 1))
                 else if
                   (* Read an undelimited number
                      (pcre2_compile.c:1819-1829). *)
                   not
                     (read_number cx ptr ~allow_sign:bracount
                        ~max_value:Limits.max_group_number
                        ~max_error:Errors.err61 s)
                 then (
                   if Int.equal cx.errorcode 0 then
                     cx.errorcode <- Errors.err57 (* No number found *);
                   broke := true);
                 if not !broke then
                   (* pcre2_compile.c:1831-1837 *)
                   if !s <= 0 then cx.errorcode <- Errors.err15
                   else escape := - !s
           | '1' .. '9' ->
               (* The handling of escape sequences consisting of a string of
                  digits starting with one that is not zero is not
                  straightforward. Outside a character class, the digits are
                  read as a decimal number. If the number is less than 10, or
                  if there are that many previous extracting left brackets,
                  it is a back reference. Otherwise, up to three octal digits
                  are read to form an escaped character code. Inside a
                  character class, \ followed by a digit is always either a
                  literal 8 or 9 or an octal number
                  (pcre2_compile.c:1840-1886). *)
               let handled = ref false in
               if not isclass then (
                 let oldptr = !ptr in
                 decr ptr (* Back to the digit *);
                 (* cb is non-NULL here (see the \g case above). *)
                 let bracount =
                   match cb with Some cbv -> cbv.bracount | None -> 0
                 in
                 let s = ref 0 in
                 (* As we know we are at a digit, the only possible error
                    from read_number() is a number that is too large to be a
                    group number, in which case we fall through and handle
                    this as not a group reference. \1 to \9 are always back
                    references. \8x and \9x are too; \1x to \7x are octal
                    escapes if there are not that many previous captures
                    (pcre2_compile.c:1862-1876). *)
                 if
                   read_number cx ptr ~allow_sign:(-1)
                     ~max_value:((0x7fff_ffff / 10) - 1) (* INT_MAX/10 - 1 *)
                     ~max_error:0 s
                   && (!s < 10
                      || Char.code cx.pattern.[oldptr - 1] >= Char.code '8'
                      || !s <= bracount)
                 then (
                   if !s > Limits.max_group_number then
                     cx.errorcode <- Errors.err61
                   else escape := - !s (* Indicates a back reference *);
                   handled := true)
                 else ptr := oldptr (* Put the pointer back and fall through *));
               if not !handled then
                 if !c >= Char.code '8' then
                   (* If the first digit is 8 or 9, Perl no longer inserts a
                      binary zero; the digit stays a literal
                      (pcre2_compile.c:1881-1886). *)
                   () (* break *)
                 else (
                   (* Fall through to the octal handling below. *)
                   (* pcre2_compile.c:1896-1899 — case CHAR_0 body, reached
                      by fallthrough from the digit cases in C. Note i == 0
                      on entry: the escapes-table entry for this character
                      was 0 (pcre2_compile.c:1586), so the C's
                      `while(i++ < 2 ...)` reads at most 2 more octal
                      digits. *)
                   c := !c - Char.code '0';
                   let count = ref 0 in
                   while
                     !count < 2 && !ptr < cx.ptrend
                     && Char.code cx.pattern.[!ptr] >= Char.code '0'
                     && Char.code cx.pattern.[!ptr] <= Char.code '7'
                   do
                     c := (!c * 8) + Char.code cx.pattern.[!ptr] - Char.code '0';
                     incr ptr;
                     incr count
                   done;
                   (* PCRE2_CODE_UNIT_WIDTH == 8 (pcre2_compile.c:1900-1902) *)
                   if (not utf) && !c > 0xff then cx.errorcode <- Errors.err51)
           | '0' ->
               (* \0 always starts an octal number. No more than 3 octal
                  digits are read (pcre2_compile.c:1888-1903). The body is
                  duplicated from the fallthrough seam above (fallthrough
                  from the digit cases in C). *)
               c := !c - Char.code '0';
               let count = ref 0 in
               while
                 !count < 2 && !ptr < cx.ptrend
                 && Char.code cx.pattern.[!ptr] >= Char.code '0'
                 && Char.code cx.pattern.[!ptr] <= Char.code '7'
               do
                 c := (!c * 8) + Char.code cx.pattern.[!ptr] - Char.code '0';
                 incr ptr;
                 incr count
               done;
               (* PCRE2_CODE_UNIT_WIDTH == 8 (pcre2_compile.c:1900-1902) *)
               if (not utf) && !c > 0xff then cx.errorcode <- Errors.err51
           | 'o' ->
               (* \o is a relatively new Perl feature, supporting a more
                  general way of specifying character codes in octal. The
                  only supported form is \o{ddd}, with optional spaces or
                  tabs after { and before } (pcre2_compile.c:1905-1964). *)
               (* C: if (ptr >= ptrend || *ptr++ != '{') { ptr--; ERR55 } —
                  at end of input the increment never happens, so the net
                  effect there is ptr-1; on a non-{ code unit the ++/-- pair
                  cancels out. *)
               if !ptr >= cx.ptrend then (
                 decr ptr;
                 cx.errorcode <- Errors.err55)
               else if not (Char.equal cx.pattern.[!ptr] '{') then
                 cx.errorcode <- Errors.err55
               else (
                 incr ptr;
                 (* pcre2_compile.c:1917-1922 *)
                 while !ptr < cx.ptrend && space_or_tab cx.pattern.[!ptr] do
                   incr ptr
                 done;
                 if !ptr >= cx.ptrend || Char.equal cx.pattern.[!ptr] '}' then
                   cx.errorcode <- Errors.err78
                 else (
                   (* pcre2_compile.c:1924-1941 *)
                   c := 0;
                   let overflow = ref false in
                   let scanning = ref true in
                   while
                     !scanning && !ptr < cx.ptrend
                     && Char.code cx.pattern.[!ptr] >= Char.code '0'
                     && Char.code cx.pattern.[!ptr] <= Char.code '7'
                   do
                     let cc = Char.code cx.pattern.[!ptr] in
                     incr ptr;
                     if Int.equal !c 0 && Int.equal cc (Char.code '0') then ()
                       (* Leading zeroes *)
                     else (
                       c := (!c lsl 3) + (cc - Char.code '0');
                       (* PCRE2_CODE_UNIT_WIDTH == 8
                          (pcre2_compile.c:1934-1935) *)
                       if !c > if utf then 0x10ffff else 0xff then (
                         overflow := true;
                         scanning := false))
                   done;

                   (* pcre2_compile.c:1943 *)
                   while !ptr < cx.ptrend && space_or_tab cx.pattern.[!ptr] do
                     incr ptr
                   done;

                   if !overflow then (
                     (* pcre2_compile.c:1945-1949 *)
                     while
                       !ptr < cx.ptrend
                       && Char.code cx.pattern.[!ptr] >= Char.code '0'
                       && Char.code cx.pattern.[!ptr] <= Char.code '7'
                     do
                       incr ptr
                     done;
                     cx.errorcode <- Errors.err34)
                   else if !ptr < cx.ptrend then (
                     (* C: else if (ptr < ptrend && *ptr++ == '}') — the
                        increment happens whether or not the code unit is
                        '}' (pcre2_compile.c:1950-1963). *)
                     let ch = cx.pattern.[!ptr] in
                     incr ptr;
                     if Char.equal ch '}' then (
                       if
                         utf && !c >= 0xd800 && !c <= 0xdfff
                         && Int.equal
                              (xoptions
                             land Options.extra_allow_surrogate_escapes)
                              0
                       then (
                         decr ptr;
                         cx.errorcode <- Errors.err73))
                     else (
                       decr ptr;
                       cx.errorcode <- Errors.err64))
                   else (
                     decr ptr;
                     cx.errorcode <- Errors.err64)))
           | 'x' ->
               (* When PCRE2_ALT_BSUX or PCRE2_EXTRA_ALT_BSUX is set, \x
                  must be followed by two hexadecimal digits. Otherwise it is
                  a lowercase x letter (pcre2_compile.c:1966-1978). *)
               if !alt_bsux then
                 if cx.ptrend - !ptr < 2 then () (* Less than 2 characters *)
                 else
                   let cc = xdigit (Char.code cx.pattern.[!ptr]) in
                   if Int.equal cc 0xff then () (* Not a hex digit *)
                   else
                     let xc = xdigit (Char.code cx.pattern.[!ptr + 1]) in
                     if Int.equal xc 0xff then () (* Not a hex digit *)
                     else (
                       c := (cc lsl 4) lor xc;
                       ptr := !ptr + 2)
               else if
                 (* Handle \x in Perl's style. \x{ddd} is a character code
                    which can be greater than 0xff in UTF-8 mode, but only if
                    the ddd are hex digits. Perl gives an error otherwise, so
                    PCRE does too (pcre2_compile.c:1980-2065). *)
                 !ptr < cx.ptrend && Char.equal cx.pattern.[!ptr] '{'
               then (
                 incr ptr;
                 while !ptr < cx.ptrend && space_or_tab cx.pattern.[!ptr] do
                   incr ptr
                 done;
                 hex_brace_tail () (* COME_FROM_NU label onward *))
               else (
                 (* Read up to two hex digits after \x
                    (pcre2_compile.c:2053-2064). *)
                 c := 0;
                 if !ptr >= cx.ptrend then () (* break *)
                 else
                   let cc = xdigit (Char.code cx.pattern.[!ptr]) in
                   if Int.equal cc 0xff then () (* Not a hex digit *)
                   else (
                     incr ptr;
                     c := cc;
                     if !ptr >= cx.ptrend then () (* break *)
                     else
                       let cc = xdigit (Char.code cx.pattern.[!ptr]) in
                       if Int.equal cc 0xff then () (* Not a hex digit *)
                       else (
                         incr ptr;
                         c := (!c lsl 4) lor cc)))
           | 'c' ->
               (* In an ASCII (or Unicode) environment, an error is given if
                  the character following \c is not a printable ASCII
                  character. Otherwise, the following character is
                  upper-cased if it is a letter, and after that the 0x40 bit
                  is flipped; the result is the value of the escape
                  (pcre2_compile.c:2068-2124; ASCII branch). *)
               if !ptr >= cx.ptrend then cx.errorcode <- Errors.err2
               else (
                 c := Char.code cx.pattern.[!ptr];
                 if !c >= Char.code 'a' && !c <= Char.code 'z' then
                   c := !c - 32 (* UPPER_CASE, pcre2_compile.c:509 *);
                 if !c < 32 || !c > 126 then
                   (* Excludes all non-printable ASCII. Note the break comes
                      before the ptr++ below. *)
                   cx.errorcode <- Errors.err68
                 else (
                   c := !c lxor 0x40;
                   incr ptr))
           | _ ->
               (* Any other alphanumeric following \ is an error
                  (pcre2_compile.c:2126-2132). Early return: chptr is not
                  written, and ptrptr points at the character at fault. *)
               cx.errorcode <- Errors.err3;
               ptrptr := !ptr - 1;
               raise_notrace Return_zero));

      (* Set the pointer to the next character before returning
         (pcre2_compile.c:2136-2140). *)
      ptrptr := !ptr;
      chptr := !c;
      !escape
    with Return_zero -> 0

(* ---------- Check for POSIX class syntax ---------- *)

(* pcre2_compile.c:2331-2399 — check_posix_syntax. Called when the sequence
   "[:" or "[." or "[=" is encountered in a character class. Checks whether
   this is followed by a sequence of characters terminated by a matching
   ":]" or ".]" or "=]". If we reach an unescaped ']' without the special
   preceding character, return false. Only the escapes \\ and \] are
   interpreted (see the long comment in the C). A new group opening with the
   same terminator supersedes an apparent outer class, hence the
   '[' + terminator check.

   Arguments:
     cx        pattern / ptrend
     ptr       index of the character after the initial [ (colon/dot/equals)
     endptr    where to return the index of the terminating ':', '.' or '='

   Returns:    true or false *)
let check_posix_syntax (cx : parse_context) (ptr : int) (endptr : int ref) :
    bool =
  let terminator = cx.pattern.[ptr] in
  (* for (; ptrend - ptr >= 2; ptr++) — pcre2_compile.c:2382-2396. The
     rec-loop step is +2 when the backslash arm fired (its ptr++ plus the
     loop's) and +1 otherwise. *)
  let rec loop ptr =
    if cx.ptrend - ptr < 2 then false
    else if
      Char.equal cx.pattern.[ptr] '\\'
      && (Char.equal cx.pattern.[ptr + 1] ']'
         || Char.equal cx.pattern.[ptr + 1] '\\')
    then (loop [@tailcall]) (ptr + 2)
    else if
      Char.equal cx.pattern.[ptr] '['
      && Char.equal cx.pattern.[ptr + 1] terminator
      || Char.equal cx.pattern.[ptr] ']'
    then false
    else if
      Char.equal cx.pattern.[ptr] terminator
      && Char.equal cx.pattern.[ptr + 1] ']'
    then (
      endptr := ptr;
      true)
    else (loop [@tailcall]) (ptr + 1)
  in
  loop (ptr + 1)

(* ---------- Check POSIX class name ---------- *)

(* pcre2_string_utils.c:185-196 — PRIV(strncmp_c8), specialized to the
   == 0 (equality) test that is its only use in this module
   (pcre2_compile.c:2425). Compares len code units of the pattern starting
   at ptr with the first len bytes of name. *)
let strncmp_c8_eq (pattern : string) (ptr : int) (name : string) (len : int) :
    bool =
  let rec loop i =
    if i >= len then true
    else if Char.equal pattern.[ptr + i] name.[i] then (loop [@tailcall]) (i + 1)
    else false
  in
  loop 0

(* pcre2_compile.c:2403-2430 — check_posix_name. Checks the name given in a
   POSIX-style class entry such as [:alnum:].

   Arguments:
     cx        the pattern
     ptr       index of the first letter
     len       the length of the name

   Returns:    a value representing the name, or -1 if unknown *)
let check_posix_name (cx : parse_context) (ptr : int) (len : int) : int =
  let rec loop yield =
    if Int.equal posix_name_lengths.(yield) 0 then -1
    else if
      Int.equal len posix_name_lengths.(yield)
      && strncmp_c8_eq cx.pattern ptr posix_names.(yield) len
    then yield
    else (loop [@tailcall]) (yield + 1)
  in
  loop 0

(* ---------- Read a subpattern or VERB name ---------- *)

(* pcre2_compile.c:2434-2570 — read_name. Called from parse_regex() whenever
   it needs to read the name of a subpattern or a ( *VERB) or an
   ( *alpha_assertion). The initial pointer must be to the preceding
   character. If that character is '*' we are reading a verb or alpha
   assertion name. The pointer is updated to point after the name, for a
   VERB or alpha assertion name, or after the name's terminator for a
   subpattern name. Returning both the offset and the name index is
   redundant information, but some callers use one and some the other.
   When the name is in braces, spaces and tabs are allowed (and ignored) at
   either end.

   Arguments:
     cx          pattern / ptrend / errorcode (C also passes cb)
     ptrptr      the character pointer variable
     utf         true if the input is UTF-encoded
     terminator  the terminator of a subpattern name must be this (a code
                 unit value; 0 for verb/alpha-assertion callers)
     offsetptr   where to put the offset from the start of the pattern
     nameptr     where to put the index of the name in the pattern
     namelenptr  where to put the length of the name

   Returns:    true if a name was read
               false otherwise, with error code set *)
let read_name (cx : parse_context) (ptrptr : int ref) ~(utf : bool)
    ~(terminator : int) (offsetptr : int ref) (nameptr : int ref)
    (namelenptr : int ref) : bool =
  let ptr = ref !ptrptr in
  (* pcre2_compile.c:2469 — BOOL is_group = ( *ptr++ != CHAR_ASTERISK). *)
  let is_group = not (Char.equal cx.pattern.[!ptr] '*') in
  incr ptr;
  (* pcre2_compile.c:2470 *)
  let is_braced = Int.equal terminator (Char.code '}') in
  try
    (* pcre2_compile.c:2472-2473 *)
    if is_braced then
      while !ptr < cx.ptrend && space_or_tab cx.pattern.[!ptr] do
        incr ptr
      done;

    (* pcre2_compile.c:2475-2480 — no characters in name. *)
    if !ptr >= cx.ptrend then (
      cx.errorcode <-
        (if is_group then Errors.err62 (* Subpattern name expected *)
         else Errors.err60 (* Verb not recognized or malformed *));
      raise_notrace Goto_exit (* goto FAILED *));

    (* pcre2_compile.c:2482-2483. *ptr - cb->start_pattern: indices here are
       already offsets from the start of the pattern. *)
    nameptr := !ptr;
    offsetptr := !ptr;

    (* The SUPPORT_UNICODE group-name scan (pcre2_compile.c:2488-2512) needs
       GETCHAR/UCD_CHARTYPE from utf.ml, which lands in M6. Until then this
       function mirrors the !SUPPORT_UNICODE build (pcre2_compile.c:2514-2516),
       in which utf is ignored ((void)utf) and the byte path below is always
       taken. UTF compilation is rejected before parse in the current
       engine, so the arm is unreachable. *)
    ignore (utf : bool);

    (* pcre2_compile.c:2518-2533 — handle non-group names and group names in
       non-UTF modes. A group name must not start with a digit. If either of
       the others start with a digit it just won't be recognized. *)
    if is_group && is_digit cx.pattern.[!ptr] then (
      cx.errorcode <- Errors.err44;
      raise_notrace Goto_exit (* goto FAILED *));
    (* MAX_255 of *ptr is TRUE in the 8-bit library (pcre2_intmodedep.h:212).
       cb->ctypes: the engine currently has only the default C-locale
       tables (Chartables.ctypes); custom-table plumbing arrives with the
       compile_block. *)
    while
      !ptr < cx.ptrend
      && not
           (Int.equal
              (Chartables.ctypes (Char.code cx.pattern.[!ptr])
              land Chartables.ctype_word)
              0)
    do
      incr ptr
    done;

    (* pcre2_compile.c:2535-2542 — check name length. *)
    if !ptr > !nameptr + Limits.max_name_size then (
      cx.errorcode <- Errors.err48;
      raise_notrace Goto_exit (* goto FAILED *));
    namelenptr := !ptr - !nameptr;

    (* pcre2_compile.c:2544-2562 — subpattern names must not be empty, and
       their terminator is checked here. (What follows a verb or alpha
       assertion name is checked separately.) *)
    if is_group then (
      if Int.equal !ptr !nameptr then (
        cx.errorcode <- Errors.err62;
        (* Subpattern name expected *)
        raise_notrace Goto_exit (* goto FAILED *));
      if is_braced then
        while !ptr < cx.ptrend && space_or_tab cx.pattern.[!ptr] do
          incr ptr
        done;
      if
        !ptr >= cx.ptrend
        || not (Int.equal (Char.code cx.pattern.[!ptr]) terminator)
      then (
        cx.errorcode <- Errors.err42;
        raise_notrace Goto_exit (* goto FAILED *));
      incr ptr);

    (* pcre2_compile.c:2564-2565 *)
    ptrptr := !ptr;
    true
  with Goto_exit ->
    (* FAILED: pcre2_compile.c:2567-2569 *)
    ptrptr := !ptr;
    false

(* ---------- Inline sanity checks (module-initialization asserts) ---------- *)

(* META encoding scheme. *)
let () =
  assert (Int.equal (meta_code (meta_capture lor 12)) meta_capture);
  assert (Int.equal (meta_data (meta_capture lor 12)) 12);
  assert (Int.equal (meta_diff meta_minmax_query meta_end) 0x3f);
  (* meta_extra_lengths covers META_END..META_MINMAX_QUERY. *)
  assert (Int.equal (Array.length meta_extra_lengths) 64);
  assert (Int.equal meta_extra_lengths.(meta_diff meta_minmax meta_end) 2);
  assert (Int.equal meta_first_quantifier meta_asterisk);
  assert (Int.equal meta_last_quantifier meta_minmax_query);
  assert (Int.equal meta_atomic_script_run 0x8fff_0000)

(* read_number on "123". *)
let () =
  let cx = make_context "123" in
  let ptr = ref 0 in
  let n = ref (-1) in
  let ok =
    read_number cx ptr ~allow_sign:(-1) ~max_value:Limits.max_repeat_count
      ~max_error:Errors.err5 n
  in
  assert ok;
  assert (Int.equal !n 123);
  assert (Int.equal !ptr 3);
  assert (Int.equal cx.errorcode 0);
  assert (Int.equal cx.ptr 0);
  assert (Int.equal cx.erroroffset 0)

(* read_repeat_counts on "{2,5}", "{3,}", "{4}" (pointer starts after '{'),
   plus the ERR5 (105) and ERR4 (104) error paths. *)
let () =
  let cx = make_context "{2,5}" in
  let ptr = ref 1 in
  let minr = ref (-1) in
  let maxr = ref (-1) in
  let ok = read_repeat_counts cx ptr (Some minr) (Some maxr) in
  assert ok;
  assert (Int.equal !minr 2);
  assert (Int.equal !maxr 5);
  assert (Int.equal !ptr 5);
  assert (Int.equal cx.errorcode 0)

let () =
  let cx = make_context "{3,}" in
  let ptr = ref 1 in
  let minr = ref (-1) in
  let maxr = ref (-1) in
  let ok = read_repeat_counts cx ptr (Some minr) (Some maxr) in
  assert ok;
  assert (Int.equal !minr 3);
  assert (Int.equal !maxr Limits.repeat_unlimited);
  assert (Int.equal !ptr 4)

let () =
  let cx = make_context "{4}" in
  let ptr = ref 1 in
  let minr = ref (-1) in
  let maxr = ref (-1) in
  let ok = read_repeat_counts cx ptr (Some minr) (Some maxr) in
  assert ok;
  assert (Int.equal !minr 4);
  assert (Int.equal !maxr 4);
  assert (Int.equal !ptr 3)

let () =
  (* n too big: ERR5 = 105 ("number too big in {} quantifier"). *)
  let cx = make_context "{99999}" in
  let ptr = ref 1 in
  let ok = read_repeat_counts cx ptr None None in
  assert (not ok);
  assert (Int.equal cx.errorcode Errors.err5);
  assert (Int.equal Errors.err5 105);
  (* max < min: ERR4 = 104 ("numbers out of order in {} quantifier"). *)
  let cx = make_context "{5,2}" in
  let ptr = ref 1 in
  let ok = read_repeat_counts cx ptr None None in
  assert (not ok);
  assert (Int.equal cx.errorcode Errors.err4);
  (* not a quantifier: pointer must stay untouched, errorcode 0. *)
  let cx = make_context "{123456ABC" in
  let ptr = ref 1 in
  let ok = read_repeat_counts cx ptr None None in
  assert (not ok);
  assert (Int.equal cx.errorcode 0);
  assert (Int.equal !ptr 1)

(* POSIX class syntax and names: "alpha" is class 0, "junk" is unknown. *)
let () =
  let cx = make_context "[:alpha:]" in
  let endp = ref (-1) in
  assert (check_posix_syntax cx 1 endp);
  assert (Int.equal !endp 7);
  assert (Int.equal (check_posix_name cx 2 5) 0);
  let cx = make_context "junk" in
  assert (Int.equal (check_posix_name cx 0 4) (-1))

(* read_name on "<foo>" (terminator '>') and the ERR44 path. *)
let () =
  let cx = make_context "<foo>" in
  let ptr = ref 0 in
  let offset = ref (-1) in
  let name = ref (-1) in
  let namelen = ref (-1) in
  let ok =
    read_name cx ptr ~utf:false ~terminator:(Char.code '>') offset name namelen
  in
  assert ok;
  assert (Int.equal !name 1);
  assert (Int.equal !offset 1);
  assert (Int.equal !namelen 3);
  assert (Int.equal !ptr 5);
  let cx = make_context "<9a>" in
  let ptr = ref 0 in
  let ok =
    read_name cx ptr ~utf:false ~terminator:(Char.code '>') offset name namelen
  in
  assert (not ok);
  assert (Int.equal cx.errorcode Errors.err44);
  assert (Int.equal Errors.err44 144)

(* ESC-code encoding invariants: the escape values correspond in order to the
   opcodes OP_SOD..OP_EOD (pcre2_internal.h:1343-1344,1369-1372), and the
   escapes table covers exactly '0'..'z'. *)
let () =
  assert (Int.equal esc_big_a Opcodes.op_sod);
  assert (Int.equal esc_b Opcodes.op_word_boundary);
  assert (Int.equal esc_big_n Opcodes.op_any);
  assert (Int.equal esc_dum Opcodes.op_allany);
  assert (Int.equal esc_z Opcodes.op_eod);
  assert (Int.equal esc_ub 29);
  assert (Int.equal (Array.length escapes) (escapes_last - escapes_first + 1));
  assert (Int.equal (Array.length xdigitab) 256);
  assert (Int.equal (xdigit (Char.code 'f')) 15);
  assert (Int.equal (xdigit (Char.code 'G')) 0xff);
  assert (Int.equal escapes.(Char.code 'n' - escapes_first) 0x0a);
  assert (Int.equal escapes.(Char.code 'Q' - escapes_first) (-esc_big_q))

(* check_escape sanity checks. Each expected quadruple
   (escape, chptr, ptrptr, errorcode) below was traced against
   pcre2_compile.c:1550-2141; chptr = -1 / ptrptr = 0 mean "not written"
   (the ERR1 and mid-function return-0 paths). *)
let () =
  let run ?(options = 0) ?(xoptions = 0) ?(isclass = false) ?(sub = false)
      ?(bracount = 0) pat =
    let cx = make_context pat in
    cx.bracount <- bracount;
    let ptr = ref 0 in
    let ch = ref (-1) in
    let esc =
      check_escape cx ptr ch ~options ~xoptions ~isclass
        (if sub then None else Some cx)
    in
    (esc, !ch, !ptr, cx.errorcode)
  in
  let eq (e, c, p, ec) (e', c', p', ec') =
    Int.equal e e' && Int.equal c c' && Int.equal p p' && Int.equal ec ec'
  in

  (* Simple data escapes from the table: \n \e; \: literal; \r with and
     without PCRE2_EXTRA_ESCAPED_CR_IS_LF. *)
  assert (eq (run "n") (0, 0x0a, 1, 0));
  assert (eq (run "e") (0, 0x1b, 1, 0));
  assert (eq (run ":") (0, 0x3a, 1, 0));
  assert (eq (run "r") (0, 0x0d, 1, 0));
  assert (eq (run "r" ~xoptions:Options.extra_escaped_cr_is_lf) (0, 0x0a, 1, 0));
  (* Out-of-table code units are definitely literal. *)
  assert (eq (run "~") (0, 0x7e, 1, 0));
  assert (eq (run ".") (0, 0x2e, 1, 0));

  (* Special escapes: \Q \E \A \d \k; \Q..\E quoting itself is handled by
     parse_regex — check_escape just returns ESC_Q/ESC_E (per the C). *)
  assert (eq (run "Q") (esc_big_q, Char.code 'Q', 1, 0));
  assert (eq (run "E") (esc_big_e, Char.code 'E', 1, 0));
  assert (eq (run "A") (esc_big_a, Char.code 'A', 1, 0));
  assert (eq (run "d") (esc_d, Char.code 'd', 1, 0));
  assert (eq (run "k") (esc_k, Char.code 'k', 1, 0));

  (* \P sets the HASBKPORX external flag when cb is present. *)
  let cx = make_context "P" in
  let ptr = ref 0 in
  let ch = ref (-1) in
  let esc =
    check_escape cx ptr ch ~options:0 ~xoptions:0 ~isclass:false (Some cx)
  in
  assert (Int.equal esc esc_big_p);
  assert (Int.equal (cx.external_flags land hasbkporx) hasbkporx);

  (* \N: plain, quantified, \N{U+} rejected in non-UTF mode (ERR93),
     \N{name} unsupported (ERR37); \N{U+41} accepted in UTF mode via the
     COME_FROM_NU path. *)
  assert (eq (run "N") (esc_big_n, Char.code 'N', 1, 0));
  assert (eq (run "N{2,3}") (esc_big_n, Char.code 'N', 1, 0));
  assert (eq (run "N{U+41}") (esc_big_n, Char.code 'N', 1, Errors.err93));
  assert (eq (run "N{name}") (esc_big_n, Char.code 'N', 1, Errors.err37));
  assert (eq (run "N{U+41}" ~options:Options.utf) (0, 0x41, 7, 0));

  (* Hex escapes: \x41 \x{7f} (spaces allowed inside braces), plain \x is a
     binary zero; ERR78/ERR67/ERR34 paths. *)
  assert (eq (run "x41") (0, 0x41, 3, 0));
  assert (eq (run "x{7f}") (0, 0x7f, 5, 0));
  assert (eq (run "x{ 7f }") (0, 0x7f, 7, 0));
  assert (eq (run "x") (0, 0, 1, 0));
  assert (eq (run "xg") (0, 0, 1, 0));
  assert (eq (run "x{}") (0, Char.code 'x', 2, Errors.err78));
  assert (eq (run "x{2f") (0, 0x2f, 3, Errors.err67));
  assert (eq (run "x{zz}") (0, 0, 2, Errors.err67));
  assert (
    Int.equal
      (let _, _, _, ec = run "x{110000}" in
       ec)
      Errors.err34);

  (* Octal: \0 \07 \377 (classified as octal, = 255), \777 overflows the
     8-bit non-UTF limit (ERR51); \o{101} and its error paths. *)
  assert (eq (run "0") (0, 0, 1, 0));
  assert (eq (run "07") (0, 7, 2, 0));
  assert (eq (run "377") (0, 255, 3, 0));
  assert (eq (run "377" ~isclass:true) (0, 255, 3, 0));
  assert (eq (run "777") (0, 511, 3, Errors.err51));
  assert (eq (run "o{101}") (0, 65, 6, 0));
  assert (eq (run "o") (0, Char.code 'o', 0, Errors.err55));
  assert (eq (run "oz") (0, Char.code 'o', 1, Errors.err55));
  assert (eq (run "o{}") (0, Char.code 'o', 2, Errors.err78));
  assert (eq (run "o{12") (0, 0o12, 3, Errors.err64));
  assert (eq (run "o{400}") (0, 256, 5, Errors.err34));

  (* Backslash-digit disambiguation: \1..\9 always back references; \8x \9x
     too; \1x..\7x octal when there aren't that many captures; inside a
     class always literal-or-octal. \8 classifies as backreference -8 here;
     ERR15 for the non-existent group is diagnosed later, in parse_regex
     (M2). *)
  assert (eq (run "8") (-8, Char.code '8', 1, 0));
  assert (eq (run "9x") (-9, Char.code '9', 1, 0));
  assert (eq (run "11") (0, 0o11, 2, 0));
  assert (eq (run "12" ~bracount:12) (-12, Char.code '1', 2, 0));
  assert (eq (run "8" ~isclass:true) (0, Char.code '8', 1, 0));
  assert (eq (run "9" ~isclass:true) (0, Char.code '9', 1, 0));

  (* \g: subroutine-call forms, braced and plain numbers, relative numbers,
     name fallback to ESC_k, error paths, literal inside a class. *)
  assert (eq (run "g<name>") (esc_g, Char.code 'g', 1, 0));
  assert (eq (run "g'1'") (esc_g, Char.code 'g', 1, 0));
  assert (eq (run "g1" ~bracount:1) (-1, Char.code 'g', 2, 0));
  assert (eq (run "g{2}" ~bracount:2) (-2, Char.code 'g', 4, 0));
  assert (eq (run "g{-1}" ~bracount:3) (-3, Char.code 'g', 5, 0));
  assert (eq (run "g{name}") (esc_k, Char.code 'g', 1, 0));
  assert (eq (run "g") (0, Char.code 'g', 1, Errors.err57));
  assert (eq (run "g{0}") (0, Char.code 'g', 4, Errors.err15));
  assert (eq (run "g" ~isclass:true) (0, Char.code 'g', 1, 0));

  (* \c: value, lower-case letter upper-cased, error paths. *)
  assert (eq (run "cA") (0, 1, 2, 0));
  assert (eq (run "ca") (0, 1, 2, 0));
  assert (eq (run "c{") (0, 0x3b, 2, 0));
  assert (eq (run "c") (0, Char.code 'c', 1, Errors.err2));
  assert (eq (run "c\x01") (0, 1, 1, Errors.err68));

  (* alt_bsux: \u as 4 hex digits (PCRE2_ALT_BSUX), \u{...} braced form
     (PCRE2_EXTRA_ALT_BSUX), the ESC_ub special return for \u{ 12}, the
     out-of-range check, and \x as exactly 2 hex digits. Without either
     option, \u and \U are ERR37. *)
  assert (eq (run "u0041" ~options:Options.alt_bsux) (0, 0x41, 5, 0));
  assert (eq (run "u12" ~options:Options.alt_bsux) (0, Char.code 'u', 1, 0));
  assert (eq (run "u{2b}" ~xoptions:Options.extra_alt_bsux) (0, 0x2b, 5, 0));
  assert (
    eq
      (run "u{ 12}" ~xoptions:Options.extra_alt_bsux)
      (esc_ub, Char.code 'u', 2, 0));
  assert (
    Int.equal
      (let _, _, _, ec = run "u{110000}" ~xoptions:Options.extra_alt_bsux in
       ec)
      Errors.err77);
  assert (eq (run "x4z" ~options:Options.alt_bsux) (0, Char.code 'x', 1, 0));
  assert (eq (run "u0041") (0, Char.code 'u', 1, Errors.err37));
  assert (eq (run "U") (0, Char.code 'U', 1, Errors.err37));
  assert (eq (run "U" ~options:Options.alt_bsux) (0, Char.code 'U', 1, 0));

  (* Unsupported Perl escapes and unknown alphanumerics: ERR37 / ERR3. The
     ERR3 default case is an early return with ptrptr pointing at the
     character at fault and chptr unwritten. *)
  assert (eq (run "L") (0, Char.code 'L', 1, Errors.err37));
  assert (eq (run "F") (0, Char.code 'F', 1, Errors.err37));
  assert (eq (run "i") (0, -1, 0, Errors.err3));
  (* \ at end of pattern: ERR1, nothing written. *)
  assert (eq (run "") (0, -1, 0, Errors.err1));

  (* pcre2_substitute() filter (cb = None): the filter lives in the
     further-processing branch, so table escapes such as \d still come back
     as specials; of the zero-entry characters only \c, \o, and \x are
     recognized (even \L is ERR3 here, not ERR37), and alt_bsux is forced
     off so \x keeps Perl semantics. *)
  assert (eq (run "d" ~sub:true) (esc_d, Char.code 'd', 1, 0));
  assert (eq (run "g1" ~sub:true) (0, -1, 0, Errors.err3));
  assert (eq (run "L" ~sub:true) (0, -1, 0, Errors.err3));
  assert (eq (run "x41" ~sub:true) (0, 0x41, 3, 0));
  assert (eq (run "x4" ~sub:true ~options:Options.alt_bsux) (0, 4, 2, 0))
