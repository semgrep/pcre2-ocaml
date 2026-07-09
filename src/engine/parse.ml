(* Parse phase of the pure-OCaml PCRE2 10.44 port: pattern -> META stream.

   Ported from the front half of vendor/pcre2/src/pcre2_compile.c. This
   module currently holds the parsed-pattern encoding scheme (META_* codes,
   meta_extra_lengths), the parse-phase helpers (read_number,
   read_repeat_counts, check_posix_syntax, check_posix_name, read_name,
   check_escape, handle_escdsw, manage_callouts), parse_context, and
   parse_regex itself (conditional/recursion/verb/callout, script-run and
   \p arms still deferred per docs/ocaml-engine/02-core-compile-match.md).

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

(* pcre2_compile.c:91-97 — PUTOFFSET(s,p) with the SIZEOFFSET = 1 layout
   (see the deviation note above): *p++ = s. *)
let putoffset (buf : int array) (pp : int ref) (offset : int) : unit =
  buf.(!pp) <- offset;
  incr pp

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

(* pcre2_internal.h:535 — PCRE2_JCHANGED: the (?J) option was used in the
   pattern. *)
let jchanged = 0x0000_0400

(* pcre2_internal.h:546 — PCRE2_DUPCAPUSED: the pattern contains (?|. *)
let dupcapused = 0x0020_0000

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

(* pcre2_compile.c:709-713 — indices into the POSIX class list needed by the
   parse phase (PC_DIGIT and PC_XDIGIT; PC_GRAPH/PC_PRINT/PC_PUNCT are used
   only by the compile-phase class code and arrive with it). *)
let pc_digit = 7
let pc_xdigit = 13

(* pcre2_compile.c:742-763 — the POSIX class Unicode property substitutes
   that are used in UCP mode must be in the order of the POSIX class names,
   defined above. Two values per class: the type and value of a \p or \P
   item. The special cases are specified with a negative type: a non-zero
   value causes \h or \H to be used, and a zero value falls through to
   behave like a non-UCP POSIX class. *)
let posix_substitutes =
  [|
    Opcodes.pt_gc;
    Ucp.ucp_l;
    (* alpha *)
    Opcodes.pt_pc;
    Ucp.ucp_ll;
    (* lower *)
    Opcodes.pt_pc;
    Ucp.ucp_lu;
    (* upper *)
    Opcodes.pt_alnum;
    0;
    (* alnum *)
    -1;
    0;
    (* ascii, treat as non-UCP *)
    -1;
    1;
    (* blank, treat as \h *)
    Opcodes.pt_pc;
    Ucp.ucp_cc;
    (* cntrl *)
    Opcodes.pt_pc;
    Ucp.ucp_nd;
    (* digit *)
    Opcodes.pt_pxgraph;
    0;
    (* graph *)
    Opcodes.pt_pxprint;
    0;
    (* print *)
    Opcodes.pt_pxpunct;
    0;
    (* punct *)
    Opcodes.pt_pxspace;
    0;
    (* space *)
    (* Xps is POSIX space, but from 8.34 *)
    Opcodes.pt_word;
    0;
    (* word *)
    (* Perl and POSIX space are the same *)
    Opcodes.pt_pxxdigit;
    0;
    (* xdigit *)
    (* Perl has additional hex digits *)
  |]

(* ---------- Parse context ---------- *)

(* pcre2_intmodedep.h:709-717 — structure for building a list of named
   groups during the first pass of compiling. The C's PCRE2_SPTR name
   pointer becomes a pattern offset. *)
type named_group = {
  name : int; (* Offset of the name in the pattern *)
  number : int; (* uint32_t: group number *)
  length : int; (* uint16_t: length of the name *)
  mutable isdup : bool; (* uint16_t: TRUE if a duplicate *)
}

(* pcre2_compile.c:187 — the initial number of entries in the named-group
   list (a stack vector in the C pcre2_compile(), grown on the heap when it
   fills up; see the DEFINE_NAME code in parse_regex). *)
let named_group_list_size = 20

(* pcre2_internal.h:490-492 — newline-convention types, owned by Newline
   (the pcre2_newline.c module); re-exported here for the parse/compile
   users of cb->nltype. *)
let nltype_fixed = Newline.nltype_fixed (* Newline is a fixed length string *)
let nltype_any = Newline.nltype_any (* Newline is any Unicode line ending *)
let nltype_anycrlf = Newline.nltype_anycrlf (* Newline is CR, LF, or CRLF *)

(* pcre2.h.generic:482 — PCRE2_UNSET is ~(PCRE2_SIZE)0.
   DEVIATION: pattern offsets are OCaml ints here; max_int plays the same
   "larger than any real offset" role for both the == and < comparisons the
   C performs on PCRE2_UNSET values. *)
let pcre2_unset = max_int

(* Threads the state the C passes to the parse helpers: the pattern and its
   end (parse_regex's ptr/ptrend over cb->start_pattern..cb->end_pattern),
   the canonical read position (a local `ptr` in parse_regex,
   pcre2_compile.c:2773), and the errorcode/erroroffset plumbing
   (C: *errorcodeptr and cb->erroroffset). The helpers below take an explicit
   `int ref` read pointer exactly as the C passes &ptr / &p / &tempptr, so
   probing calls on temporary pointers work; parse_regex keeps the main
   position in a local [ptr]. *)
type parse_context = {
  pattern : string; (* cb->start_pattern; indices are pattern offsets *)
  ptrend : int; (* one past the last code unit of the pattern *)
  mutable ptr : int; (* parse_regex's start position (after skipatstart) *)
  mutable errorcode : int; (* *errorcodeptr *)
  mutable erroroffset : int; (* cb->erroroffset *)
  (* Until the full compile_block record lands (compile-phase chunks),
     parse_context also carries the compile_block / compile-context fields
     the parse phase touches (pcre2_intmodedep.h:735,743 and neighbours): *)
  mutable bracount : int; (* cb->bracount: capturing groups seen so far *)
  mutable external_flags : int; (* cb->external_flags: hasbkporx etc. *)
  (* cb->external_options (pcre2_intmodedep.h:742): the external (initial)
     options, set from the pcre2_compile() options argument
     (pcre2_compile.c:10254) and updated only by ( *UTF)-style
     start-of-pattern settings — unlike parse_regex's local [options], which
     tracks in-pattern (?i)-style changes. *)
  mutable external_options : int;
  mutable extra_options : int; (* cb->cx->extra_options *)
  mutable parens_nest_limit : int; (* cb->cx->parens_nest_limit *)
  (* cb->nltype/nllen/nl (pcre2_intmodedep.h:751-755). Defaults are the
     build-default newline convention, NEWLINE_DEFAULT = LF
     (config.h.generic:230-235); the compile-phase chunk wires the compile
     context / ( *CR)-style settings through these. *)
  mutable nltype : int;
  mutable nllen : int;
  mutable nl0 : int; (* cb->nl[0] *)
  mutable nl1 : int; (* cb->nl[1] (only read when nllen = 2) *)
  (* cb->small_ref_offset — first-occurrence pattern offsets of \1..\9
     (pcre2_compile.c:10280-10290, initialized to PCRE2_UNSET). *)
  small_ref_offset : int array;
  (* The named-group list built by parse_regex's DEFINE_NAME code and
     consumed by the compile phase: cb->names_found / name_entry_size
     (pcre2_intmodedep.h:736-737, both uint16_t), cb->named_groups /
     named_group_list_size (pcre2_intmodedep.h:740-741), and cb->dupnames
     (pcre2_intmodedep.h:762). *)
  mutable names_found : int;
  mutable name_entry_size : int;
  mutable named_groups : named_group array;
  mutable named_group_list_size : int;
  mutable dupnames : bool;
  (* cb->parsed_pattern / cb->parsed_pattern_end: the parsed-pattern vector
     and its end. C pointers become the int array plus an index limit;
     allocate_parsed_pattern (below) sizes them as pcre2_compile() does. *)
  mutable parsed_pattern : int array;
  mutable parsed_pattern_end : int; (* index one past the last element *)
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
    external_options = 0;
    extra_options = 0;
    parens_nest_limit = Limits.parens_nest_limit;
    nltype = nltype_fixed;
    nllen = 1;
    nl0 = 0x0a (* CHAR_LF *);
    nl1 = 0;
    small_ref_offset = Array.make 10 pcre2_unset (* pcre2_compile.c:10290 *);
    names_found = 0 (* pcre2_compile.c:10264 *);
    name_entry_size = 0 (* pcre2_compile.c:10260 *);
    (* pcre2_compile.c:10168,10262 — the initial NAMED_GROUP_LIST_SIZE
       entries. Sharing one dummy record across the fresh slots is safe:
       only entries below names_found are ever read or mutated, and each is
       replaced with a fresh record when it is added. *)
    named_groups =
      Array.make named_group_list_size
        { name = 0; number = 0; length = 0; isdup = false };
    named_group_list_size (* pcre2_compile.c:10263 *);
    dupnames = false (* pcre2_compile.c:10250 *);
    parsed_pattern = [||];
    parsed_pattern_end = 0;
  }

(* pcre2_internal.h:494-506 — IS_NEWLINE(p), with NLBLOCK = cb and
   PSEND = end_pattern as pcre2_compile.c sets them up. The non-FIXED arm
   is the macro's `(p) < PSEND && PRIV(is_newline)(p, nltype, PSEND,
   &nllen, utf)`, delegating to Newline.is_newline
   (pcre2_newline.c:78-145); like the C, which passes &(cb->nllen), the
   matched newline's length is written into cx.nllen only on a TRUE return
   (the caller advances by it). The macro's [utf] is parse_regex's local
   (options & PCRE2_UTF); PCRE2_UTF never changes after the driver folds
   ( *UTF) into the external options before parsing, so it is recovered
   here from cx.external_options. The FIXED arm is compared inline by the
   macro itself — PRIV(is_newline) never sees NLTYPE_FIXED — so it stays
   here, below. *)
let is_newline_at (cx : parse_context) (p : int) : bool =
  if not (Int.equal cx.nltype nltype_fixed) then (
    p < cx.ptrend
    &&
    let len = ref 0 in
    let hit =
      Newline.is_newline cx.pattern cx.nltype p cx.ptrend len
        (not (Int.equal (cx.external_options land Options.utf) 0))
    in
    if hit then cx.nllen <- !len;
    hit)
  else
    p <= cx.ptrend - cx.nllen
    && Int.equal (Char.code cx.pattern.[p]) cx.nl0
    && (Int.equal cx.nllen 1 || Int.equal (Char.code cx.pattern.[p + 1]) cx.nl1)

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
      (* pcre2_compile.c:1571-1572 — GETCHARINCTEST
         (pcre2_intmodedep.h:319-324): get character value, increment
         pointer. *)
      c := Utf.getcharinctest ~utf cx.pattern ptr;
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

(* ---------- Handle \P and \p ---------- *)

(* pcre2_compile.c:2145-2326 — get_ucp. This function is called after \P or
   \p has been encountered (Unicode support is always compiled in the
   reference configuration, so no SUPPORT_UNICODE guard). On entry ptrptr
   points after the P or p; on exit it is left pointing after the final
   code unit of the escape sequence.

   Arguments:
     cx        errorcode plumbing (the C's errorcodeptr and cb)
     ptrptr    the pattern position pointer
     negptr    set true for negation ({^...}) else false
     ptypeptr  set to the PT_* type value (C: uint16_t)
     pdataptr  set to the detailed property value (C: uint16_t)

   Returns: true if the type value was found, or false for an invalid
   type. *)
let get_ucp (cx : parse_context) (ptrptr : int ref) (negptr : bool ref)
    (ptypeptr : int ref) (pdataptr : int ref) : bool =
  let pat = cx.pattern in
  let ptr = ref !ptrptr in
  (* pcre2_compile.c:2173-2175 — PCRE2_UCHAR name[50]; PCRE2_UCHAR *vptr =
     NULL (a pointer into name[], here the index with -1 for NULL);
     uint16_t ptscript = PT_NOTSCRIPT. [i] mirrors the C's loop index: one
     past the last stored name character when the loop ends. *)
  let name = Bytes.make 50 '\000' in
  let i = ref 0 in
  let vptr = ref (-1) in
  let ptscript = ref Opcodes.pt_notscript in
  let exception Error_return in
  try
    if !ptr >= cx.ptrend then raise_notrace Error_return;
    let c = ref (Char.code pat.[!ptr]) in
    incr ptr;
    negptr := false;

    (* pcre2_compile.c:2181-2215 — \P or \p can be followed by a name in
       {}, optionally preceded by ^ for negation. *)
    if Int.equal !c (Char.code '{') then (
      if !ptr >= cx.ptrend then raise_notrace Error_return;
      if Char.equal pat.[!ptr] '^' then (
        negptr := true;
        incr ptr);
      (* for (i = 0; i < sizeof(name)/sizeof(PCRE2_UCHAR) - 1; i++) *)
      let broke = ref false in
      while (not !broke) && !i < Bytes.length name - 1 do
        if !ptr >= cx.ptrend then raise_notrace Error_return;
        c := Char.code pat.[!ptr];
        incr ptr;
        (* while (c == '_' || c == '-' || isspace(c)) — C-locale isspace()
           holds for 0x20 and 0x09-0x0d only. *)
        while
          Int.equal !c (Char.code '_')
          || Int.equal !c (Char.code '-')
          || Int.equal !c 0x20
          || (!c >= 0x09 && !c <= 0x0d)
        do
          if !ptr >= cx.ptrend then raise_notrace Error_return;
          c := Char.code pat.[!ptr];
          incr ptr
        done;
        if Int.equal !c 0 (* CHAR_NUL *) then raise_notrace Error_return;
        if Int.equal !c (Char.code '}') then broke := true
        else (
          (* name[i] = tolower(c) — C-locale tolower() folds only A-Z. *)
          Bytes.set name !i
            (Char.chr
               (if !c >= Char.code 'A' && !c <= Char.code 'Z' then !c + 32
                else !c));
          if
            (Int.equal !c (Char.code ':') || Int.equal !c (Char.code '='))
            && Int.equal !vptr (-1)
          then vptr := !i;
          incr i)
      done;
      if not (Int.equal !c (Char.code '}')) then raise_notrace Error_return
      (* name[i] = 0 — the length [i] delimits the name below. *))
    else if
      (* pcre2_compile.c:2217-2225 — if { doesn't follow \p or \P there is
         just one following character, which must be an ASCII letter.
         MAX_255(c) always holds for an 8-bit code unit. *)
      not (Int.equal (Chartables.ctypes !c land Chartables.ctype_letter) 0)
    then (
      Bytes.set name 0
        (Char.chr
           (if !c >= Char.code 'A' && !c <= Char.code 'Z' then !c + 32 else !c));
      i := 1)
    else raise_notrace Error_return;

    ptrptr := !ptr;

    (* pcre2_compile.c:2229-2277 — if the property contains ':' or '=' we
       have class name and value separately specified. Supported:
       Bidi_Class (synonym bc), for which the property names are
       "bidi<name>"; Script (synonym sc), for which the property name is
       the script name; Script_Extensions (synonym scx), ditto. For both
       script properties, a PT_xxx value is set so that (1) they can be
       distinguished and (2) invalid script names that happen to be the
       name of another property can be diagnosed. The C's in-place
       memmoves (2273-2276) become string concatenation on the value
       part. *)
    let lookup_name =
      if not (Int.equal !vptr (-1)) then
        let prop = Bytes.sub_string name 0 !vptr in
        let value = Bytes.sub_string name (!vptr + 1) (!i - !vptr - 1) in
        if String.equal prop "bidiclass" || String.equal prop "bc" then
          Some ("bidi" ^ value)
        else if String.equal prop "script" || String.equal prop "sc" then (
          ptscript := Opcodes.pt_sc;
          Some value)
        else if String.equal prop "scriptextensions" || String.equal prop "scx"
        then (
          ptscript := Opcodes.pt_scx;
          Some value)
        else None (* ERR47 (pcre2_compile.c:2267-2271) *)
      else Some (Bytes.sub_string name 0 !i)
    in
    match lookup_name with
    | None ->
        cx.errorcode <- Errors.err47;
        false
    | Some lookup_name ->
        (* pcre2_compile.c:2279-2317 — search for a recognized property
           using binary chop over Ucptables.utt (strcmp_c8 and
           String.compare agree on these ASCII names). *)
        let found = ref false in
        let searching = ref true in
        let bot = ref 0 in
        let top = ref (Array.length Ucptables.utt) in
        while !searching && !bot < !top do
          let m = (!bot + !top) lsr 1 in
          let entry_name, entry_type, entry_value = Ucptables.utt.(m) in
          let r = String.compare lookup_name entry_name in
          if Int.equal r 0 then (
            (* pcre2_compile.c:2290-2314 — when a matching property is
               found, some extra checking is needed when the \p{xx:yy}
               syntax is used and xx is either sc or scx. *)
            pdataptr := entry_value;
            if Int.equal !vptr (-1) || Int.equal !ptscript Opcodes.pt_notscript
            then (
              ptypeptr := entry_type;
              found := true;
              searching := false)
            else if Int.equal entry_type Opcodes.pt_sc then (
              ptypeptr := Opcodes.pt_sc;
              found := true;
              searching := false)
            else if Int.equal entry_type Opcodes.pt_scx then (
              ptypeptr := !ptscript;
              found := true;
              searching := false)
            else searching := false (* break: non-script found *))
          else if r > 0 then bot := m + 1
          else top := m
        done;
        if !found then true
        else (
          (* pcre2_compile.c:2319-2320 — unrecognized property. *)
          cx.errorcode <- Errors.err47;
          false)
  with Error_return ->
    (* pcre2_compile.c:2322-2325 — malformed \P or \p. *)
    cx.errorcode <- Errors.err46;
    ptrptr := !ptr;
    false

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

(* pcre2_string_utils.c:150-167 — PRIV(strncmp), specialized to the == 0
   (equality) test that is its only use in this module
   (pcre2_compile.c:4874). Compares len code units of the pattern starting
   at p1 with the len code units starting at p2. *)
let strncmp_eq (pattern : string) (p1 : int) (p2 : int) (len : int) : bool =
  let rec loop i =
    if i >= len then true
    else if Char.equal pattern.[p1 + i] pattern.[p2 + i] then
      (loop [@tailcall]) (i + 1)
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

    (* pcre2_compile.c:2485-2512 — in UTF mode, a group name may contain
       letters and decimal digits as defined by Unicode properties, and
       underscores, but must not start with a digit. *)
    if utf && is_group then (
      let c = ref (Utf.getchar cx.pattern !ptr) in
      let type_ = ref (Ucd.chartype !c) in
      if Int.equal !type_ Ucp.ucp_nd then (
        cx.errorcode <- Errors.err44;
        raise_notrace Goto_exit (* goto FAILED *));
      let brk = ref false in
      while not !brk do
        if
          (not (Int.equal !type_ Ucp.ucp_nd))
          && (not (Int.equal Tables.ucp_gentype.(!type_) Ucp.ucp_l))
          && not (Int.equal !c (Char.code '_'))
        then brk := true
        else (
          incr ptr;
          ptr := Utf.forwardchartest cx.pattern !ptr cx.ptrend;
          if !ptr >= cx.ptrend then brk := true
          else (
            c := Utf.getchar cx.pattern !ptr;
            type_ := Ucd.chartype !c))
      done)
    else (
      (* pcre2_compile.c:2518-2533 — handle non-group names and group
         names in non-UTF modes. A group name must not start with a
         digit. If either of the others start with a digit it just won't
         be recognized. *)
      if is_group && is_digit cx.pattern.[!ptr] then (
        cx.errorcode <- Errors.err44;
        raise_notrace Goto_exit (* goto FAILED *));
      (* MAX_255 of *ptr is TRUE in the 8-bit library
         (pcre2_intmodedep.h:212). cb->ctypes: the engine currently has
         only the default C-locale tables (Chartables.ctypes);
         custom-table plumbing arrives with the compile_block. *)
      while
        !ptr < cx.ptrend
        && not
             (Int.equal
                (Chartables.ctypes (Char.code cx.pattern.[!ptr])
                land Chartables.ctype_word)
                0)
      do
        incr ptr
      done);

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

(* ---------- Parsed-pattern buffer sizing ---------- *)

(* pcre2_compile.c:10478-10519 — pcre2_compile() sizes the parsed-pattern
   vector before calling parse_regex(). When PCRE2_AUTO_CALLOUT is not set,
   the number of unsigned 32-bit ints in the parsed pattern is bounded by
   the length of the pattern (from cx.ptr, i.e. after the skipped ( *...)
   start-of-pattern settings) plus one for the terminator, plus four if
   PCRE2_EXTRA_MATCH_WORD or _LINE is set. With PCRE2_AUTO_CALLOUT a
   numerical callout (4 elements) is assumed for each character plus one at
   the end. big32count (pcre2_compile.c:10485-10491) is 32-bit-mode only and
   is always 0 in this 8-bit port.
   DEVIATION: the C keeps a 1024-element stack vector for small patterns and
   allocates on the heap otherwise (pcre2_compile.c:10508-10518); this port
   always allocates the exact size, which is not observable. *)
let allocate_parsed_pattern (cx : parse_context) ~(options : int) : unit =
  let parsed_size_needed = cx.ptrend - cx.ptr in
  (* pcre2_compile.c:10501-10503 — ccontext->extra_options test. *)
  let parsed_size_needed =
    if
      not
        (Int.equal
           (cx.extra_options
           land (Options.extra_match_word lor Options.extra_match_line))
           0)
    then parsed_size_needed + 4
    else parsed_size_needed
  in
  (* pcre2_compile.c:10505-10506 *)
  let parsed_size_needed =
    if not (Int.equal (options land Options.auto_callout) 0) then
      (parsed_size_needed + 1) * 5
    else parsed_size_needed
  in
  (* pcre2_compile.c:10510-10511,10519 — the vector holds
     parsed_size_needed + 1 elements; parsed_pattern_end is one past it. *)
  cx.parsed_pattern <- Array.make (parsed_size_needed + 1) 0;
  cx.parsed_pattern_end <- parsed_size_needed + 1

(* ---------- Manage callouts at start of cycle ---------- *)

(* pcre2_compile.c:2574-2620 — manage_callouts. At the start of a new item
   in parse_regex() we are able to record the details of the previous item
   in a prior callout, and also to set up an automatic callout if enabled.
   Avoid having two adjacent automatic callouts, which would otherwise
   happen for items such as \Q that contribute nothing to the parsed
   pattern. The C's uint32_t *previous_callout pointer becomes an index into
   cx.parsed_pattern, with -1 for NULL.

   Arguments:
     ptr              current pattern pointer
     pcalloutptr      the previous-callout index variable (a C
                      uint32_t pointer-to-pointer)
     auto_callout     true if auto_callouts are enabled
     parsed_pattern   the parsed pattern position

   Returns: possibly updated parsed_pattern position. *)
let manage_callouts (cx : parse_context) (ptr : int) (pcalloutptr : int ref)
    ~(auto_callout : bool) (parsed_pattern : int) : int =
  let buf = cx.parsed_pattern in
  let parsed_pattern = ref parsed_pattern in
  let previous_callout = ref !pcalloutptr in

  (* pcre2_compile.c:2600-2601 — previous_callout[2] = ptr -
     cb->start_pattern - previous_callout[1] (the previous item's length;
     indices here are already pattern offsets). *)
  if !previous_callout >= 0 then
    buf.(!previous_callout + 2) <- ptr - buf.(!previous_callout + 1);

  (* pcre2_compile.c:2603-2616 *)
  if not auto_callout then previous_callout := -1
  else (
    if
      !previous_callout < 0
      || (not (Int.equal !previous_callout (!parsed_pattern - 4)))
      || not (Int.equal buf.(!previous_callout + 3) 255)
    then (
      previous_callout := !parsed_pattern (* Set up new automatic callout *);
      parsed_pattern := !parsed_pattern + 4;
      buf.(!previous_callout) <- meta_callout_number;
      buf.(!previous_callout + 2) <- 0;
      buf.(!previous_callout + 3) <- 255);
    buf.(!previous_callout + 1) <- ptr (* ptr - cb->start_pattern *));

  (* pcre2_compile.c:2618-2619 *)
  pcalloutptr := !previous_callout;
  !parsed_pattern

(* ---------- Handle \d, \D, \s, \S, \w, \W ---------- *)

(* pcre2_compile.c:2624-2699 — handle_escdsw. Called from parse_regex(),
   both for freestanding escapes and those within classes, to handle those
   escapes that may change when Unicode property support is requested.

   Arguments:
     escape          the ESC_... value
     parsed_pattern  where to add the code
     options         options bits
     xoptions        extra options bits

   Returns:          updated value of the parsed_pattern position *)
let handle_escdsw (cx : parse_context) (escape : int) (parsed_pattern : int)
    ~(options : int) ~(xoptions : int) : int =
  let buf = cx.parsed_pattern in
  let pp = ref parsed_pattern in
  let ascii_option = ref 0 in
  let prop = ref esc_p in

  (* pcre2_compile.c:2648-2670 — switch(escape): the ESC_D/ESC_S/ESC_W arms
     set prop = ESC_P and fall through to their lowercase partners. *)
  if Int.equal escape esc_big_d || Int.equal escape esc_d then (
    if Int.equal escape esc_big_d then prop := esc_big_p
      (* fallthrough from ESC_D in C *);
    ascii_option := Options.extra_ascii_bsd)
  else if Int.equal escape esc_big_s || Int.equal escape esc_s then (
    if Int.equal escape esc_big_s then prop := esc_big_p
      (* fallthrough from ESC_S in C *);
    ascii_option := Options.extra_ascii_bss)
  else if Int.equal escape esc_big_w || Int.equal escape esc_w then (
    if Int.equal escape esc_big_w then prop := esc_big_p
      (* fallthrough from ESC_W in C *);
    ascii_option := Options.extra_ascii_bsw);

  (* pcre2_compile.c:2672-2696 *)
  if
    Int.equal (options land Options.ucp) 0
    || not (Int.equal (xoptions land !ascii_option) 0)
  then (
    buf.(!pp) <- meta_escape + escape;
    incr pp)
  else (
    buf.(!pp) <- meta_escape + !prop;
    incr pp;
    if Int.equal escape esc_d || Int.equal escape esc_big_d then (
      buf.(!pp) <- (Opcodes.pt_pc lsl 16) lor Ucp.ucp_nd;
      incr pp)
    else if Int.equal escape esc_s || Int.equal escape esc_big_s then (
      buf.(!pp) <- Opcodes.pt_space lsl 16;
      incr pp)
    else (
      (* ESC_w / ESC_W *)
      buf.(!pp) <- Opcodes.pt_word lsl 16;
      incr pp));
  !pp

(* ---------- Parse regex and identify named groups ---------- *)

(* pcre2_compile.c:2723-2732 — a structure for dealing with nested groups.
   An array of these lives in the compile workspace (see parse_regex). The
   C's uint16_t fields hold values bounded by MAX_GROUP_NUMBER (65535); the
   nest_depth counter is uint16_t in C (pcre2_compile.c:2790) and would wrap
   at 65536 — exact as plain int ONLY while parens_nest_limit <= 65535
   (hardwired 250 today). If the compile-context chunk ever wires a
   user-settable limit > 65535, nest_depth must take `land 0xffff` to keep
   C's wrap-then-ERR22 behavior. *)
type nest_save = {
  mutable nest_depth : int; (* uint16_t *)
  mutable reset_group : int; (* uint16_t *)
  mutable max_group : int; (* uint16_t *)
  mutable flags : int; (* uint16_t *)
  mutable options : int; (* uint32_t *)
  mutable xoptions : int; (* uint32_t *)
}

(* pcre2_compile.c:2734-2736 *)
let nsf_reset = 0x0001
let nsf_condassert = 0x0002
let nsf_atomicsr = 0x0004

(* pcre2_compile.c:596-631 — table of special "verbs" like ( *PRUNE). This
   is a short table, so it is searched linearly. The C keeps the names in
   one \0-separated string (verbnames) with per-entry lengths in verbs;
   OCaml string literals carry their own lengths, so each entry is
   (name, base META code, has_arg). Order preserved. The empty name is a
   shorthand for MARK. has_arg: > 0 => must have an argument; < 0 =>
   optional argument, convert to pre-MARK; 0 => optional argument; bump
   the META code if found. *)
let verbs : (string * int * int) array =
  [|
    ("", meta_mark, 1);
    ("MARK", meta_mark, 1);
    ("ACCEPT", meta_accept, -1);
    ("F", meta_fail, -1);
    ("FAIL", meta_fail, -1);
    ("COMMIT", meta_commit, 0);
    ("PRUNE", meta_prune, 0);
    ("SKIP", meta_skip, 0);
    ("THEN", meta_then, 0);
  |]

let verbcount = Array.length verbs

(* pcre2_compile.c:639-685 — table of "alpha assertions" like ( *pla:...),
   similar to the ( *VERB) table. The C keeps the names in one \0-separated
   string (alasnames) with per-entry lengths in alasmeta; OCaml string
   literals carry their own lengths, so each entry is (name, base META
   code). Order preserved. *)
let alasmeta : (string * int) array =
  [|
    ("pla", meta_lookahead);
    ("plb", meta_lookbehind);
    ("napla", meta_lookahead_na);
    ("naplb", meta_lookbehind_na);
    ("nla", meta_lookaheadnot);
    ("nlb", meta_lookbehindnot);
    ("positive_lookahead", meta_lookahead);
    ("positive_lookbehind", meta_lookbehind);
    ("non_atomic_positive_lookahead", meta_lookahead_na);
    ("non_atomic_positive_lookbehind", meta_lookbehind_na);
    ("negative_lookahead", meta_lookaheadnot);
    ("negative_lookbehind", meta_lookbehindnot);
    ("atomic", meta_atomic);
    ("sr", meta_script_run);
    ("asr", meta_atomic_script_run);
    ("script_run", meta_script_run);
    ("atomic_script_run", meta_atomic_script_run);
  |]

let alascount = Array.length alasmeta

(* pcre2_compile.c:2751-2754 — states used for analyzing ranges in character
   classes. The two OK values must be last. *)
let range_no = 0
let range_started = 1
let range_ok_escaped = 2
let range_ok_literal = 3

(* pcre2_compile.c:2738-2749 — options that are changeable within the
   pattern must be tracked during parsing. Some (e.g. PCRE2_EXTENDED) are
   implemented entirely during parsing, but all must be tracked so that
   META_OPTIONS items set the correct values for the main compiling
   phase. *)
let parse_tracked_options =
  Options.caseless lor Options.dotall lor Options.dupnames lor Options.extended
  lor Options.extended_more lor Options.multiline lor Options.no_auto_capture
  lor Options.ungreedy

let parse_tracked_extra_options =
  Options.extra_caseless_restrict lor Options.extra_ascii_bsd
  lor Options.extra_ascii_bss lor Options.extra_ascii_bsw
  lor Options.extra_ascii_digit lor Options.extra_ascii_posix

(* DEVIATION: distinctive placeholder error code for parse_regex arms that
   were deferred to later chunks (conditionals, recursion, subroutine
   calls, verbs -> M5; script runs -> M8; callouts were the last, M8).
   PCRE2 compile errors occupy 100..201, so 299 can never collide with a
   real result; deferred constructs failed loudly instead of misparsing.
   No use site remains — kept for the invariant assert below and as the
   marker to reuse if a future chunk needs to defer again. *)
let err_deferred = 299

(* Local control-flow exceptions for parse_regex (port-conventions §2:
   compile-time code may use exceptions; the interpreter loop may not).
   Loop_continue models the C's `continue` statements in the main scan
   loop; Goto_failed models `goto FAILED` (cx.errorcode is set before the
   raise; the handler at the end of parse_regex records cx.erroroffset).
   Neither escapes parse_regex. *)
exception Loop_continue
exception Goto_failed

(* pcre2_compile.c:2703-5039 — parse_regex. This function is called first
   of all. It scans the pattern and (1) identifies capturing groups, and
   (2) writes a parsed version of the pattern with comments omitted and
   escapes processed into the parsed_pattern vector.

   Ported so far: the main-loop skeleton (literals, \Q..\E, extended-mode
   white space and # comments, (?# comments, the escape dispatch (numeric
   backrefs and the \k / \g{} named references included), inline option
   settings (?imnrsxJUa..) / (?^) / (?|, bare capturing/non-capturing
   parentheses, quantifiers * + ? {n,m} with their lazy/possessive
   modifiers, named-group definitions (?<name> (?'name' (?P<name> with the
   named-group list, (?P=name) named references, alternation, group close,
   and the end-of-pattern epilogue), character classes (parse_regex B:
   POSIX class items with their UCP substitutions, literals, ranges and
   in-class escapes), the ( *VERB)/( *VERB:NAME) arm with its
   inverbname accumulator block (pcre2_compile.c:2941-3039), and the
   (?C callout arm (numerical and string forms) — every arm of the C
   function is now live (nothing fails with err_deferred).

   Arguments:
     cx              compile block; parsing starts at cx.ptr and the parsed
                     pattern lands in cx.parsed_pattern (which the caller
                     sizes with allocate_parsed_pattern, as pcre2_compile()
                     does)
     options         compiling dynamic options (may change during the scan)
     has_lookbehind  set true if a lookbehind is found

   Returns: zero on success or a non-zero error code, with the error offset
            placed in cx.erroroffset. *)
let parse_regex (cx : parse_context) ~(options : int)
    (has_lookbehind : bool ref) : int =
  let pat = cx.pattern in
  let buf = cx.parsed_pattern in

  (* pcre2_compile.c:2776-2808 — local state (the class/verb/named-group
     locals belong to deferred arms). *)
  let previous_callout = ref (-1) in
  (* uint32_t *previous_callout = NULL *)
  (* pcre2_compile.c:2780-2781 — uint32_t *verblengthptr / *verbstartptr =
     NULL, as indices into the parsed pattern with -1 for NULL. Set by the
     ( *VERB) arm; verblengthptr is filled in by the inverbname block when
     the closing parenthesis is reached, verbstartptr is read by the
     META_ACCEPT block in CHECK_QUANTIFIER. *)
  let verblengthptr = ref (-1) in
  let verbstartptr = ref (-1) in
  (* pcre2_compile.c:2806 — PCRE2_SPTR verbnamestart = NULL, as a pattern
     index with -1 for NULL. *)
  let verbnamestart = ref (-1) in
  (* pcre2_compile.c:2788 — uint32_t add_after_mark = 0. *)
  let add_after_mark = ref 0 in
  let pp = ref 0 in
  (* parsed_pattern = cb->parsed_pattern, as an index *)
  let this_parsed_item = ref (-1) in
  (* NULL *)
  let prev_parsed_item = ref (-1) in
  (* NULL *)
  let meta_quantifier = ref 0 in
  let xoptions = ref cx.extra_options in
  (* cb->cx->extra_options *)
  let nest_depth = ref 0 in
  let after_manual_callout = ref 0 in
  let expect_cond_assert = ref 0 in
  let options = ref options in
  let inescq = ref false in
  let inverbname = ref false in
  let utf = not (Int.equal (!options land Options.utf) 0) in
  let auto_callout = not (Int.equal (!options land Options.auto_callout) 0) in
  let okquantifier = ref false in
  let ptr = ref cx.ptr in

  cx.errorcode <- 0;

  (* pcre2_intmodedep.h:319-324 — GETCHARINCTEST(c, ptr), over the
     function-scope utf and ptr. *)
  let getcharinctest () = Utf.getcharinctest ~utf pat ptr in

  (* pcre2_compile.c:2768 — PARSED_LITERAL(c, p), the 8-bit expansion
     (literal values cannot reach META_END). *)
  let parsed_literal ch =
    buf.(!pp) <- ch;
    incr pp;
    okquantifier := true
  in

  (* Shared error epilogues: UNCLOSED_PARENTHESIS (pcre2_compile.c:
     5019-5020), FAILED_BACK (pcre2_compile.c:5028-5032) and
     BAD_VERSION_CONDITION (pcre2_compile.c:5036-5038). All always raise
     (their return type is polymorphic). *)
  let unclosed_parenthesis () =
    cx.errorcode <- Errors.err14;
    raise_notrace Goto_failed
  in
  let failed_back () =
    decr ptr;
    raise_notrace Goto_failed
  in
  let bad_version_condition () =
    cx.errorcode <- Errors.err79;
    raise_notrace Goto_failed
  in

  (* pcre2_compile.c:4824-4923 — the DEFINE_NAME label: define a named
     group. A forward goto target reached from (?'name' (with the
     terminator set to the apostrophe, pcre2_compile.c:4830-4831) and from
     the (?< (pcre2_compile.c:4783-4784) and (?P< (pcre2_compile.c:
     4364-4365) disambiguations with the terminator set to '>'. On entry
     ptr points at the delimiter character that precedes the name. *)
  let define_name ~terminator =
    let offset = ref 0 in
    let name = ref 0 in
    let namelen = ref 0 in
    if not (read_name cx ptr ~utf ~terminator offset name namelen) then
      raise_notrace Goto_failed;

    (* We have a name for this capturing group. It is also assigned a
       number, which is its primary means of identification
       (pcre2_compile.c:4837-4847). *)
    if cx.bracount >= Limits.max_group_number then (
      cx.errorcode <- Errors.err97;
      raise_notrace Goto_failed);
    cx.bracount <- cx.bracount + 1;
    buf.(!pp) <- meta_capture lor cx.bracount;
    incr pp;
    nest_depth := !nest_depth + 1;

    (* Check not too many names (pcre2_compile.c:4849-4855). *)
    if cx.names_found >= Limits.max_name_count then (
      cx.errorcode <- Errors.err49;
      raise_notrace Goto_failed);

    (* Adjust the entry size to accommodate the longest name found
       (pcre2_compile.c:4857-4860). *)
    if !namelen + Limits.imm2_size + 1 > cx.name_entry_size then
      cx.name_entry_size <- !namelen + Limits.imm2_size + 1;

    (* pcre2_compile.c:4862-4890 — scan the list to check for duplicates.
       For duplicate names, if the number is the same, break the loop,
       which causes the name to be discarded; otherwise, if DUPNAMES is not
       set, give an error. If it is set, allow the name with a different
       number, but continue scanning in case this is a duplicate with the
       same number. For non-duplicate names, give an error if the number is
       duplicated. *)
    let isdupname = ref false in
    let broke = ref false in
    let i = ref 0 in
    while (not !broke) && !i < cx.names_found do
      let ng = cx.named_groups.(!i) in
      if Int.equal !namelen ng.length && strncmp_eq pat !name ng.name !namelen
      then
        if Int.equal ng.number cx.bracount then broke := true
        else if Int.equal (!options land Options.dupnames) 0 then (
          cx.errorcode <- Errors.err43;
          raise_notrace Goto_failed)
        else (
          ng.isdup <- true;
          isdupname := true (* Mark as a duplicate *);
          cx.dupnames <- true (* Duplicate names exist *))
      else if Int.equal ng.number cx.bracount then (
        cx.errorcode <- Errors.err65;
        raise_notrace Goto_failed);
      if not !broke then incr i
    done;

    (* Ignore a duplicate with the same number: the C's
       `if (i < cb->names_found) break` (pcre2_compile.c:4892). *)
    if not !broke then (
      (* Increase the list size if necessary (pcre2_compile.c:4894-4915).
         DEVIATION: the C reports malloc failure as ERR21; Array.make has no
         recoverable failure (Out_of_memory is fatal), so that error path
         has no OCaml equivalent. *)
      if cx.names_found >= cx.named_group_list_size then (
        let newsize = cx.named_group_list_size * 2 in
        let newspace =
          Array.make newsize { name = 0; number = 0; length = 0; isdup = false }
        in
        Array.blit cx.named_groups 0 newspace 0 cx.named_group_list_size;
        cx.named_groups <- newspace;
        cx.named_group_list_size <- newsize);

      (* Add this name to the list (pcre2_compile.c:4917-4923). *)
      cx.named_groups.(cx.names_found) <-
        {
          name = !name;
          number = cx.bracount;
          length = !namelen;
          isdup = !isdupname;
        };
      cx.names_found <- cx.names_found + 1)
  in

  try
    (* pcre2_compile.c:2810-2822 — insert leading items for word and line
       matching (features provided for the benefit of pcre2grep). *)
    if not (Int.equal (!xoptions land Options.extra_match_line) 0) then (
      buf.(!pp) <- meta_circumflex;
      incr pp;
      buf.(!pp) <- meta_nocapture;
      incr pp)
    else if not (Int.equal (!xoptions land Options.extra_match_word) 0) then (
      buf.(!pp) <- meta_escape + esc_b;
      incr pp;
      buf.(!pp) <- meta_nocapture;
      incr pp);

    (* pcre2_compile.c:2824-2844 — if the pattern is actually a literal
       string, process it separately to avoid cluttering up the main loop.
       The C's `goto PARSED_END` is the fall-through to the shared epilogue
       after this if/else. *)
    (if not (Int.equal (!options land Options.literal) 0) then
       while !ptr < cx.ptrend do
         if !pp >= cx.parsed_pattern_end then (
           cx.errorcode <- Errors.err63;
           (* Internal error (parsed pattern overflow) *)
           raise_notrace Goto_failed);
         let thisptr = !ptr in
         let c = getcharinctest () in
         if auto_callout then
           pp := manage_callouts cx thisptr previous_callout ~auto_callout !pp;
         parsed_literal c
       done
     else
       (* Process a real regex which may contain meta-characters. *)

       (* pcre2_compile.c:2846-2856 — the nest-save stack lives in the
          compile workspace: COMPILE_WORK_SIZE = 3000*LINK_SIZE 8-bit code
          units (pcre2_compile.c:166), rounded down so that no nest_save
          spans the end of the workspace (sizeof(nest_save) = 16 bytes).
          The C's top_nest/end_nests pointers become an index into [nests],
          with -1 for NULL. *)
       let workspace_size = 3000 * Limits.link_size in
       let sizeof_nest_save = 16 in
       let nest_slots =
         (workspace_size - (workspace_size mod sizeof_nest_save))
         / sizeof_nest_save
       in
       let nests =
         Array.init nest_slots (fun _ ->
             {
               nest_depth = 0;
               reset_group = 0;
               max_group = 0;
               flags = 0;
               options = 0;
               xoptions = 0;
             })
       in
       let top_nest = ref (-1) in

       (* pcre2_compile.c:2858-2860 — PCRE2_EXTENDED_MORE implies
          PCRE2_EXTENDED. *)
       if not (Int.equal (!options land Options.extended_more) 0) then
         options := !options lor Options.extended;

       (* Shared goto targets of the lookaround/atomic-group arms below.
          Each is reached both from its traditional symbolic form and from
          the alpha-assertion dispatch (pcre2_compile.c:4005-4028), so the
          labels become functions. prev_expect_cond_assert is a
          per-iteration value (pcre2_compile.c:3168), passed in by the call
          sites. *)

       (* pcre2_compile.c:4741-4748 — the ATOMIC_GROUP label: come here
          from ( *atomic: with ptr at the colon, or fall in from (?> with
          ptr at '>'. *)
       let atomic_group () =
         buf.(!pp) <- meta_atomic;
         incr pp;
         nest_depth := !nest_depth + 1;
         incr ptr
       in

       (* pcre2_compile.c:4428-4433 — the SET_RECURSION label: come here
          from (?R (with i = 0), from the RECURSION_BYNUMBER cases below,
          and from a numerical \g<n> / \g'n' (pcre2_compile.c:3380-3381).
          On entry ptr is at the closing delimiter. *)
       let set_recursion ~i =
         buf.(!pp) <- meta_recurse lor i;
         incr pp;
         let offset = !ptr in
         (* offset = ptr - cb->start_pattern *)
         incr ptr;
         putoffset buf pp offset;
         okquantifier := true
       in

       (* pcre2_compile.c:4413-4433 — the RECURSION_BYNUMBER label:
          recursion/subroutine calls by number, (?digits) (?+digits)
          (?-digits). Reached from the (?digits and (?+ arms and, for
          (?- followed by a digit, from the C switch's default case
          (pcre2_compile.c:4170-4171). On entry ptr is at the sign or
          first digit. *)
       let recursion_bynumber () =
         let i = ref 0 in
         if
           not
             (read_number cx ptr
                ~allow_sign:(if is_digit pat.[!ptr] then -1 else cx.bracount)
                  (* + and - are relative *)
                ~max_value:Limits.max_group_number ~max_error:Errors.err61 i)
         then raise_notrace Goto_failed;
         if !i < 0 (* NB (?0) is permitted *) then (
           cx.errorcode <- Errors.err15 (* Unknown group *);
           failed_back ());
         if !ptr >= cx.ptrend || not (Char.equal pat.[!ptr] ')') then
           unclosed_parenthesis ();
         set_recursion ~i:!i
       in

       (* pcre2_compile.c:4437-4446 — the RECURSE_BY_NAME label:
          recursion/subroutine calls by name, (?&name); (?P>name) comes
          here too (pcre2_compile.c:4371). On entry ptr is at the
          character preceding the name ('&' or '>'). *)
       let recurse_by_name () =
         let offset = ref 0 in
         let name = ref 0 in
         let namelen = ref 0 in
         if
           not
             (read_name cx ptr ~utf ~terminator:(Char.code ')') offset name
                namelen)
         then raise_notrace Goto_failed;
         buf.(!pp) <- meta_recurse_byname;
         incr pp;
         buf.(!pp) <- !namelen;
         incr pp;
         putoffset buf pp !offset;
         okquantifier := true
       in

       (* pcre2_compile.c:4797-4821 — the POST_ASSERTION label. If the
          previous item was a condition starting (?(? an assertion,
          optionally preceded by a callout, is expected. This is checked
          later on, during actual compilation. However we need to identify
          this kind of assertion in this pass because it must not be
          qualified. The value of expect_cond_assert is set to 2 after (?(?
          is processed. We decrement it for a callout - still leaving a
          positive value that identifies the assertion. Multiple callouts
          or any other items will make it zero or less, which doesn't
          matter because they will cause an error later. *)
       let post_assertion ~prev_expect_cond_assert =
         nest_depth := !nest_depth + 1;
         if prev_expect_cond_assert > 0 then (
           if Int.equal !top_nest (-1) then top_nest := 0
           else (
             incr top_nest;
             if !top_nest >= nest_slots then (
               cx.errorcode <- Errors.err84;
               raise_notrace Goto_failed));
           let tn = nests.(!top_nest) in
           tn.nest_depth <- !nest_depth;
           tn.flags <- nsf_condassert;
           tn.options <- !options land parse_tracked_options;
           tn.xoptions <- !xoptions land parse_tracked_extra_options)
       in

       (* pcre2_compile.c:4753-4757 — the POSITIVE_LOOK_AHEAD label: come
          here from ( *pla: with ptr at the colon, or fall in from (?= with
          ptr at '='. *)
       let positive_look_ahead ~prev_expect_cond_assert =
         buf.(!pp) <- meta_lookahead;
         incr pp;
         incr ptr;
         post_assertion ~prev_expect_cond_assert
       in

       (* pcre2_compile.c:4759-4763 — the POSITIVE_NONATOMIC_LOOK_AHEAD
          label: come here from ( *napla: with ptr at the colon, or fall in
          from (?* with ptr at '*'. *)
       let positive_nonatomic_look_ahead ~prev_expect_cond_assert =
         buf.(!pp) <- meta_lookahead_na;
         incr pp;
         incr ptr;
         post_assertion ~prev_expect_cond_assert
       in

       (* pcre2_compile.c:4765-4769 — the NEGATIVE_LOOK_AHEAD label: come
          here from ( *nla: with ptr at the colon, or fall in from (?! with
          ptr at '!'. *)
       let negative_look_ahead ~prev_expect_cond_assert =
         buf.(!pp) <- meta_lookaheadnot;
         incr pp;
         incr ptr;
         post_assertion ~prev_expect_cond_assert
       in

       (* pcre2_compile.c:4790-4795 — the POST_LOOKBEHIND label: come here
          from ( *plb: ( *naplb: and ( *nlb: (with ptr backed up onto the
          last name character) or fall in from (?< with ptr at '<'. The
          lookbehind META has already been stored; record the pattern
          offset of the assertion (for lookbehind-length error messages)
          and fall through to POST_ASSERTION. *)
       let post_lookbehind ~prev_expect_cond_assert =
         has_lookbehind := true;
         (* offset = ptr - cb->start_pattern - 2 (indices here are already
            pattern offsets). *)
         let offset = !ptr - 2 in
         putoffset buf pp offset;
         ptr := !ptr + 2;
         (* Fall through *)
         post_assertion ~prev_expect_cond_assert
       in

       (* pcre2_compile.c:2862-2864 — now scan the pattern. *)
       while !ptr < cx.ptrend do
         try
           (* pcre2_compile.c:2876-2880 *)
           if !pp >= cx.parsed_pattern_end then (
             cx.errorcode <- Errors.err63;
             (* Internal error (parsed pattern overflow) *)
             raise_notrace Goto_failed);

           (* pcre2_compile.c:2882-2886 *)
           if !nest_depth > cx.parens_nest_limit then (
             cx.errorcode <- Errors.err19;
             (* Parentheses too deeply nested *)
             raise_notrace Goto_failed);

           (* pcre2_compile.c:2888-2897 — if the last time round this loop
              something was added, parsed_pattern will no longer be equal to
              this_parsed_item. Remember where the previous item started and
              reset for the next item. *)
           if not (Int.equal !this_parsed_item !pp) then (
             prev_parsed_item := !this_parsed_item;
             this_parsed_item := !pp);

           (* pcre2_compile.c:2899-2902 — get next input character, save its
              position for callout handling. *)
           let thisptr = !ptr in
           let c = ref (getcharinctest ()) in

           (* pcre2_compile.c:2904-2939 — copy quoted literals until \E,
              allowing for the possibility of automatic callouts, except
              when processing a ( *VERB) "name". *)
           if !inescq then (
             if
               Int.equal !c 0x5c (* CHAR_BACKSLASH *)
               && !ptr < cx.ptrend
               && Char.equal pat.[!ptr] 'E'
             then (
               inescq := false;
               incr ptr (* Skip E *))
             else (
               if !expect_cond_assert > 0 then (
                 (* A literal is not allowed if we are expecting a
                    conditional assertion, but an empty \Q\E sequence is
                    OK. *)
                 decr ptr;
                 cx.errorcode <- Errors.err28;
                 raise_notrace Goto_failed);
               (if !inverbname then (
                  (* Don't use parsed_literal (PARSED_LITERAL) because it
                     sets okquantifier. *)
                  buf.(!pp) <- !c;
                  incr pp)
                else
                  let amc = !after_manual_callout in
                  after_manual_callout := amc - 1;
                  if amc <= 0 then
                    pp :=
                      manage_callouts cx thisptr previous_callout ~auto_callout
                        !pp;
                  parsed_literal !c);
               meta_quantifier := 0);
             raise_notrace Loop_continue (* Next character *));

           (* pcre2_compile.c:2941-3039 — if we are processing the "name"
              part of a ( *VERB:NAME) item, all characters up to the closing
              parenthesis are literals except when PCRE2_ALT_VERBNAMES is
              set. That causes backslash interpretation, but only \Q and \E
              and escaped characters are allowed (no character types such as
              \d). If PCRE2_EXTENDED is also set, we must ignore white space
              and # comments. Do this by not entering the special
              ( *VERB:NAME) processing - they are then picked up below. Note
              that c is a character, not a code unit. *)
           if
             !inverbname
             && ((* EITHER: not both options set
                    (pcre2_compile.c:2953-2955) *)
                 (not
                    (Int.equal
                       (!options
                       land (Options.extended lor Options.alt_verbnames))
                       (Options.extended lor Options.alt_verbnames)))
                (* OR: character > 255 AND not Unicode Pattern White Space
                   (pcre2_compile.c:2957-2958) *)
                || !c > 255
                   && (not (Int.equal (!c lor 1) 0x200f))
                   && not (Int.equal (!c lor 1) 0x2029)
                (* OR: not a # comment or isspace() white space, and not
                   CHAR_NEL when Unicode is supported
                   (pcre2_compile.c:2960-2966) *)
                || !c < 256
                   && (not (Int.equal !c (Char.code '#')))
                   && Int.equal
                        (Chartables.ctypes !c land Chartables.ctype_space)
                        0
                   && not (Int.equal !c 0x85 (* CHAR_NEL *)))
           then (
             (* switch(c) (pcre2_compile.c:2970-3037) *)
             if Int.equal !c (Char.code ')') then (
               (* pcre2_compile.c:2979-3001 *)
               inverbname := false;
               (* This is the length in characters *)
               let verbnamelength = !pp - !verblengthptr - 1 in
               (* But the limit on the length is in code units *)
               if !ptr - !verbnamestart - 1 > Limits.max_mark then (
                 decr ptr;
                 cx.errorcode <- Errors.err76;
                 raise_notrace Goto_failed);
               buf.(!verblengthptr) <- verbnamelength;

               (* If this name was on a verb such as ( *ACCEPT) which does
                  not continue, a ( *MARK) was generated for the name. We
                  now add the original verb as the next item
                  (pcre2_compile.c:2992-3000). *)
               if not (Int.equal !add_after_mark 0) then (
                 buf.(!pp) <- !add_after_mark;
                 incr pp;
                 add_after_mark := 0))
             else if Int.equal !c 0x5c (* CHAR_BACKSLASH *) then
               (* pcre2_compile.c:3003-3010 *)
               let escape =
                 if not (Int.equal (!options land Options.alt_verbnames) 0) then (
                   let e =
                     check_escape cx ptr c ~options:!options ~xoptions:!xoptions
                       ~isclass:false (Some cx)
                   in
                   if not (Int.equal cx.errorcode 0) then
                     raise_notrace Goto_failed;
                   e)
                 else 0 (* Treat all as literal *)
               in

               (* switch(escape) (pcre2_compile.c:3012-3036) *)
               if Int.equal escape 0 then (
                 (* Don't use parsed_literal (PARSED_LITERAL) because it
                    sets okquantifier (pcre2_compile.c:3014-3019). *)
                 buf.(!pp) <- !c;
                 incr pp)
               else if Int.equal escape esc_ub then (
                 (* pcre2_compile.c:3021-3024 *)
                 buf.(!pp) <- Char.code 'u';
                 incr pp;
                 parsed_literal (Char.code '{'))
               else if Int.equal escape esc_big_q then inescq := true
               else if Int.equal escape esc_big_e then () (* Ignore *)
               else (
                 cx.errorcode <- Errors.err40 (* Invalid in verb name *);
                 raise_notrace Goto_failed)
             else (
               (* The C switch's default case, first in the source
                  (pcre2_compile.c:2972-2977) — don't use parsed_literal
                  (PARSED_LITERAL) because it sets okquantifier. *)
               buf.(!pp) <- !c;
               incr pp);
             raise_notrace Loop_continue (* Next character in pattern *));

           (* pcre2_compile.c:3041-3054 — not a verb name character. Process
              \Q and \E here, so that an item such as A\Q\E+ is treated as
              A+, as in Perl. An isolated \E is ignored. *)
           if Int.equal !c 0x5c (* CHAR_BACKSLASH *) && !ptr < cx.ptrend then
             if Char.equal pat.[!ptr] 'Q' || Char.equal pat.[!ptr] 'E' then (
               inescq := Char.equal pat.[!ptr] 'Q';
               incr ptr;
               raise_notrace Loop_continue);

           (* pcre2_compile.c:3056-3086 — skip over whitespace and #
              comments in extended mode. The whitespace characters are the
              "Pattern White Space" set: the isspace() characters plus NEL
              (0x85, the only extra member reachable as an 8-bit code unit)
              plus U+200E, U+200F, U+2028, U+2029. *)
           if not (Int.equal (!options land Options.extended) 0) then (
             if
               !c < 256
               && not
                    (Int.equal
                       (Chartables.ctypes !c land Chartables.ctype_space)
                       0)
             then raise_notrace Loop_continue;
             (* SUPPORT_UNICODE branch (pcre2_compile.c:3067-3069). *)
             if
               Int.equal !c 0x85 (* CHAR_NEL *)
               || Int.equal (!c lor 1) 0x200f
               || Int.equal (!c lor 1) 0x2029
             then raise_notrace Loop_continue;
             if Int.equal !c (Char.code '#') then (
               (* pcre2_compile.c:3070-3085 *)
               let scanning = ref true in
               while !scanning && !ptr < cx.ptrend do
                 if is_newline_at cx !ptr then (
                   (* For non-fixed-length newline cases, IS_NEWLINE sets
                      cb->nllen. *)
                   ptr := !ptr + cx.nllen;
                   scanning := false)
                 else (
                   incr ptr;
                   (* pcre2_compile.c:3080-3082 — if utf,
                      FORWARDCHARTEST(ptr, ptrend). *)
                   if utf then ptr := Utf.forwardchartest pat !ptr cx.ptrend)
               done;
               raise_notrace Loop_continue (* Next character in pattern *)));

           (* pcre2_compile.c:3088-3101 — skip over bracketed comments. *)
           if
             Int.equal !c (Char.code '(')
             && cx.ptrend - !ptr >= 2
             && Char.equal pat.[!ptr] '?'
             && Char.equal pat.[!ptr + 1] '#'
           then (
             (* C: while (++ptr < ptrend && *ptr != ')'); *)
             incr ptr;
             while !ptr < cx.ptrend && not (Char.equal pat.[!ptr] ')') do
               incr ptr
             done;
             if !ptr >= cx.ptrend then (
               cx.errorcode <- Errors.err18;
               (* A special error for missing ) in a comment, to make it
                  easier to debug. *)
               raise_notrace Goto_failed);
             incr ptr;
             raise_notrace Loop_continue (* Next character in pattern *));

           (* pcre2_compile.c:3103-3117 — if the next item is not a
              quantifier, fill in length of any previous callout and create
              an auto callout if required. *)
           if
             (not (Int.equal !c (Char.code '*')))
             && (not (Int.equal !c (Char.code '+')))
             && (not (Int.equal !c (Char.code '?')))
             && ((not (Int.equal !c (Char.code '{')))
                ||
                let tempptr = ref !ptr in
                not (read_repeat_counts cx tempptr None None))
           then (
             let amc = !after_manual_callout in
             after_manual_callout := amc - 1;
             if amc <= 0 then (
               pp :=
                 manage_callouts cx thisptr previous_callout ~auto_callout !pp;
               this_parsed_item := !pp (* New start for current item *)));

           (* pcre2_compile.c:3119-3163 — if expect_cond_assert is 2, we
              have just passed (?( and are expecting an assertion, possibly
              preceded by a callout; if 1, we have just had the callout and
              expect an assertion. (Only the deferred conditional-group arm
              sets it (M5); the scaffolding is ported so those chunks slot
              in.) *)
           (if !expect_cond_assert > 0 then
              let ok =
                Int.equal !c (Char.code '(')
                && cx.ptrend - !ptr >= 3
                && (Char.equal pat.[!ptr] '?' || Char.equal pat.[!ptr] '*')
              in
              let ok =
                if not ok then false
                else if Char.equal pat.[!ptr] '*' then
                  (* New alpha assertion format, possibly. MAX_255(ptr[1]) is
                     always true in the 8-bit library. *)
                  not
                    (Int.equal
                       (Chartables.ctypes (Char.code pat.[!ptr + 1])
                       land Chartables.ctype_lcletter)
                       0)
                else
                  (* Traditional symbolic format. *)
                  match pat.[!ptr + 1] with
                  | 'C' -> Int.equal !expect_cond_assert 2
                  | '=' | '!' -> true
                  | '<' ->
                      Char.equal pat.[!ptr + 2] '='
                      || Char.equal pat.[!ptr + 2] '!'
                  | _ -> false
              in
              if not ok then (
                decr ptr (* Adjust error offset *);
                cx.errorcode <- Errors.err28;
                raise_notrace Goto_failed));

           (* pcre2_compile.c:3165-3177 — remember whether we are expecting
              a conditional assertion and the quantification status of the
              previous significant item, then set the defaults for this
              item. *)
           let prev_expect_cond_assert = !expect_cond_assert in
           expect_cond_assert := 0;
           let prev_okquantifier = !okquantifier in
           let prev_meta_quantifier = !meta_quantifier in
           okquantifier := false;
           meta_quantifier := 0;

           (* pcre2_compile.c:3179-3191 — if the previous significant item
              was a quantifier, adjust the parsed code if there is a
              following + or ? modifier. The base meta value is always
              followed by the PLUS and QUERY values, in that order. Done
              here rather than after reading a quantifier so that
              intervening comments and /x whitespace can be ignored without
              replicating code. *)
           if
             (not (Int.equal prev_meta_quantifier 0))
             && (Int.equal !c (Char.code '?') || Int.equal !c (Char.code '+'))
           then (
             let idx =
               !pp
               + if Int.equal prev_meta_quantifier meta_minmax then -3 else -1
             in
             buf.(idx) <-
               (prev_meta_quantifier
               +
               if Int.equal !c (Char.code '?') then 0x0002_0000 else 0x0001_0000
               );
             raise_notrace Loop_continue (* Next character in pattern *));

           (* pcre2_compile.c:3452-3490 — the CHECK_QUANTIFIER label: shared
              quantifier post-processing, a forward goto target for the
              * + ? arms and the {n,m} fall-through below. Check that a
              quantifier is allowed after the previous item. This
              guarantees that there is a previous item. *)
           let check_quantifier ~min_repeat ~max_repeat =
             if not prev_okquantifier then (
               cx.errorcode <- Errors.err9;
               failed_back () (* goto FAILED_BACK *));

             (* pcre2_compile.c:3464-3477 — most ( *VERB)s are not allowed
                to be quantified, but an ungreedy quantifier can be useful
                for ( *ACCEPT) - meaning "succeed on backtrack", a sort of
                negated ( *COMMIT). We therefore allow ( *ACCEPT) to be
                quantified by wrapping it in non-capturing brackets, but we
                have to allow for a preceding ( *MARK) for when ( *ACCEPT)
                has an argument. *)
             (* safe: prev_okquantifier guarantees a previous item
                (pcre2_compile.c:3454-3455), so prev_parsed_item >= 0. *)
             if Int.equal buf.(!prev_parsed_item) meta_accept then (
               let p = ref (!pp - 1) in
               while !p >= !verbstartptr do
                 buf.(!p + 1) <- buf.(!p);
                 decr p
               done;
               buf.(!verbstartptr) <- meta_nocapture;
               buf.(!pp + 1) <- meta_ket;
               pp := !pp + 2);

             (* pcre2_compile.c:3479-3490 — now we can put the quantifier
                into the parsed pattern vector. At this stage, we have only
                the basic quantifier. The check for a following + or ?
                modifier happens at the top of the loop, after any
                intervening comments have been removed. *)
             buf.(!pp) <- !meta_quantifier;
             incr pp;
             if Int.equal !c (Char.code '{') then (
               buf.(!pp) <- min_repeat;
               incr pp;
               buf.(!pp) <- max_repeat;
               incr pp)
           in

           (* pcre2_compile.c:3193-3195 — process the next item in the main
              part of a pattern. The C switch's default case (non-special
              character) comes first in the source and is the last arm of
              this match. In UTF mode getcharinctest can decode a character
              above 255: every switch case label is an ASCII code unit, so
              such a character always takes the default (literal) arm —
              dispatched here before Char.chr, whose byte precondition
              holds on the other side of the guard. *)
           if !c > 255 then
             (* pcre2_compile.c:3197-3199 — non-special character (the C
                switch's default case, first in the source). *)
             parsed_literal !c
           else
             match Char.chr !c with
             | '\\' ->
                 (* ---- Escape sequence ---- pcre2_compile.c:3202-3404 *)
                 let tempptr = !ptr in
                 let escape =
                   ref
                     (check_escape cx ptr c ~options:!options
                        ~xoptions:!xoptions ~isclass:false (Some cx))
                 in

                 (* pcre2_compile.c:3208-3219 — the ESCAPE_FAILED label: a bad
                    escape is fatal unless PCRE2_EXTRA_BAD_ESCAPE_IS_LITERAL
                    is set, in which case the escape sequence is re-read as a
                    literal character. The C's forward gotos to ESCAPE_FAILED
                    from the arms below are `escape_failed (); process_escape
                    ()` here: after recovery escape = 0, so the re-entry takes
                    the literal path. Note that errorcode is NOT reset,
                    exactly as in the C. *)
                 let escape_failed () =
                   if
                     Int.equal
                       (!xoptions land Options.extra_bad_escape_is_literal)
                       0
                   then raise_notrace Goto_failed;
                   ptr := tempptr;
                   if !ptr >= cx.ptrend then c := 0x5c (* CHAR_BACKSLASH *)
                   else
                     (* GETCHARINCTEST — get character value, increment
                        pointer (pcre2_compile.c:3216). *)
                     c := getcharinctest ();
                   escape := 0 (* Treat as literal character *)
                 in

                 let rec process_escape () =
                   if Int.equal !escape 0 then
                     (* pcre2_compile.c:3221-3226 — the escape was a data
                        escape or literal character. *)
                     parsed_literal !c
                   else if !escape < 0 then (
                     (* pcre2_compile.c:3228-3250 — a back (or forward)
                        reference. Keep the offset in order to give a more
                        useful diagnostic for a bad forward reference. For
                        references to groups numbered less than 10 no more
                        than two items can be used in parsed_pattern (they may
                        be just two characters in the input), so for them the
                        offset of the first occurrence is held in a special
                        vector. *)
                     let offset = !ptr - 1 in
                     (* ptr - cb->start_pattern - 1 *)
                     let escape = - !escape in
                     buf.(!pp) <- meta_backref lor escape;
                     incr pp;
                     if escape < 10 then (
                       if Int.equal cx.small_ref_offset.(escape) pcre2_unset
                       then cx.small_ref_offset.(escape) <- offset)
                     else putoffset buf pp offset;
                     okquantifier := true)
                   else if Int.equal !escape esc_big_c then
                     (* pcre2_compile.c:3270-3283 — \C. The NEVER_BACKSLASH_C
                        build-time switch is not defined in the reference
                        configuration, so only the PCRE2_NEVER_BACKSLASH_C
                        option check (ERR83) applies. *)
                     if
                       not
                         (Int.equal (!options land Options.never_backslash_c) 0)
                     then (
                       cx.errorcode <- Errors.err83;
                       escape_failed () (* goto ESCAPE_FAILED *);
                       process_escape ())
                     else (
                       okquantifier := true;
                       buf.(!pp) <- meta_escape + !escape;
                       incr pp)
                   else if Int.equal !escape esc_ub then (
                     (* pcre2_compile.c:3285-3293 — a special return that
                        happens only in EXTRA_ALT_BSUX mode, when \u{ is not
                        followed by hex digits and }. It requests two literal
                        characters, u and {. *)
                     buf.(!pp) <- Char.code 'u';
                     incr pp;
                     parsed_literal (Char.code '{'))
                   else if
                     Int.equal !escape esc_big_x
                     || Int.equal !escape esc_big_h
                     || Int.equal !escape esc_h
                     || Int.equal !escape esc_big_n
                     || Int.equal !escape esc_big_r
                     || Int.equal !escape esc_big_v
                     || Int.equal !escape esc_v
                   then (
                     (* pcre2_compile.c:3295-3308 — ESC_X (Unicode support is
                        compiled in, so no ERR45) falls through to
                        ESC_H/ESC_h/ESC_N/ESC_R/ESC_V/ESC_v in C. *)
                     okquantifier := true;
                     buf.(!pp) <- meta_escape + !escape;
                     incr pp)
                   else if
                     Int.equal !escape esc_d
                     || Int.equal !escape esc_big_d
                     || Int.equal !escape esc_s
                     || Int.equal !escape esc_big_s
                     || Int.equal !escape esc_w
                     || Int.equal !escape esc_big_w
                   then (
                     (* pcre2_compile.c:3314-3325 — escapes that may change in
                        UCP mode. *)
                     okquantifier := true;
                     pp :=
                       handle_escdsw cx !escape !pp ~options:!options
                         ~xoptions:!xoptions)
                   else if
                     Int.equal !escape esc_big_p || Int.equal !escape esc_p
                   then
                     (* pcre2_compile.c:3327-3346 — \P and \p Unicode
                        property matching (SUPPORT_UNICODE is defined in
                        the reference configuration, so the ERR45 arm does
                        not apply). *)
                     let negated = ref false in
                     let ptype = ref 0 in
                     let pdata = ref 0 in
                     if not (get_ucp cx ptr negated ptype pdata) then (
                       escape_failed () (* goto ESCAPE_FAILED *);
                       process_escape ())
                     else (
                       if !negated then
                         escape :=
                           if Int.equal !escape esc_big_p then esc_p
                           else esc_big_p;
                       buf.(!pp) <- meta_escape + !escape;
                       incr pp;
                       buf.(!pp) <- (!ptype lsl 16) lor !pdata;
                       incr pp;
                       okquantifier := true (* End \P and \p *))
                   else if Int.equal !escape esc_g || Int.equal !escape esc_k
                   then (
                     if
                       (* pcre2_compile.c:3348-3402 — when \g is used with
                          quotes or angle brackets as delimiters, it is a
                          numerical or named subroutine call; with brace
                          delimiters it is a numerical back reference and does
                          not come here because check_escape() returns it
                          directly. \k is always a named back reference.
                          Subroutine calls are deferred (M5). *)
                       !ptr >= cx.ptrend
                       || (not (Char.equal pat.[!ptr] '{'))
                          && (not (Char.equal pat.[!ptr] '<'))
                          && not (Char.equal pat.[!ptr] '\'')
                     then (
                       cx.errorcode <-
                         (if Int.equal !escape esc_g then Errors.err57
                          else Errors.err69);
                       escape_failed () (* goto ESCAPE_FAILED *);
                       process_escape ())
                     else
                       let terminator =
                         if Char.equal pat.[!ptr] '<' then Char.code '>'
                         else if Char.equal pat.[!ptr] '\'' then Char.code '\''
                         else Char.code '}'
                       in
                       (* For a non-braced \g, check for a numerical recursion
                          (pcre2_compile.c:3366-3384). *)
                       let recovered = ref false in
                       (if
                          Int.equal !escape esc_g
                          && not (Int.equal terminator (Char.code '}'))
                        then
                          let p = ref (!ptr + 1) in
                          let i = ref 0 in
                          if
                            read_number cx p ~allow_sign:cx.bracount
                              ~max_value:Limits.max_group_number
                              ~max_error:Errors.err61 i
                          then
                            if
                              !p >= cx.ptrend
                              || not (Int.equal (Char.code pat.[!p]) terminator)
                            then (
                              cx.errorcode <- Errors.err57;
                              escape_failed () (* goto ESCAPE_FAILED *);
                              process_escape ();
                              recovered := true)
                            else (
                              (* pcre2_compile.c:3380-3381 — ptr = p; goto
                                 SET_RECURSION. *)
                              ptr := !p;
                              set_recursion ~i:!i;
                              recovered := true)
                          else if not (Int.equal cx.errorcode 0) then (
                            escape_failed () (* goto ESCAPE_FAILED *);
                            process_escape ();
                            recovered := true));
                       if not !recovered then
                         (* Not a numerical recursion. Perl allows spaces and
                            tabs after { and before } but not for other
                            delimiters (pcre2_compile.c:3386-3390). *)
                         let offset = ref 0 in
                         let name = ref 0 in
                         let namelen = ref 0 in
                         if
                           not
                             (read_name cx ptr ~utf ~terminator offset name
                                namelen)
                         then (
                           escape_failed () (* goto ESCAPE_FAILED *);
                           process_escape ())
                         else if
                           (* pcre2_compile.c:3392-3402 — \k and \g when used
                              with braces are back references, whereas \g
                              used with quotes or angle brackets is a
                              recursion. *)
                           Int.equal !escape esc_k
                           || Int.equal terminator (Char.code '}')
                         then (
                           buf.(!pp) <- meta_backref_byname;
                           incr pp;
                           buf.(!pp) <- !namelen;
                           incr pp;
                           putoffset buf pp !offset;
                           okquantifier := true)
                         else (
                           (* META_RECURSE_BYNAME emission (pcre2_compile.c:
                              3395-3401 — the \g-with-quotes/angle-brackets
                              arm of the shared ternary store). *)
                           buf.(!pp) <- meta_recurse_byname;
                           incr pp;
                           buf.(!pp) <- !namelen;
                           incr pp;
                           putoffset buf pp !offset;
                           okquantifier := true))
                   else (
                     (* pcre2_compile.c:3310-3312 — the C switch's default
                        case: \A, \B, \b, \G, \K, \Z, \z cannot be
                        quantified. *)
                     buf.(!pp) <- meta_escape + !escape;
                     incr pp)
                 in
                 if not (Int.equal cx.errorcode 0) then escape_failed ();
                 process_escape ()
             (* ---- Single-character special items ---- *)
             | '^' ->
                 (* pcre2_compile.c:3409-3411 *)
                 buf.(!pp) <- meta_circumflex;
                 incr pp
             | '$' ->
                 (* pcre2_compile.c:3413-3415 *)
                 buf.(!pp) <- meta_dollar;
                 incr pp
             | '.' ->
                 (* pcre2_compile.c:3417-3420 *)
                 buf.(!pp) <- meta_dot;
                 incr pp;
                 okquantifier := true
             (* ---- Single-character quantifiers ---- *)
             | '*' ->
                 (* pcre2_compile.c:3425-3427 *)
                 meta_quantifier := meta_asterisk;
                 check_quantifier ~min_repeat:0 ~max_repeat:0
                 (* goto CHECK_QUANTIFIER *)
             | '+' ->
                 (* pcre2_compile.c:3429-3431 *)
                 meta_quantifier := meta_plus;
                 check_quantifier ~min_repeat:0 ~max_repeat:0
                 (* goto CHECK_QUANTIFIER *)
             | '?' ->
                 (* pcre2_compile.c:3433-3435 *)
                 meta_quantifier := meta_query;
                 check_quantifier ~min_repeat:0 ~max_repeat:0
                 (* goto CHECK_QUANTIFIER *)
             | '{' ->
                 (* ---- Potential {n,m} quantifier ----
                    pcre2_compile.c:3440-3449 *)
                 let min_repeat = ref 0 in
                 let max_repeat = ref 0 in
                 if
                   not
                     (read_repeat_counts cx ptr (Some min_repeat)
                        (Some max_repeat))
                 then (
                   if not (Int.equal cx.errorcode 0) then
                     raise_notrace Goto_failed (* Error in quantifier *);
                   parsed_literal !c
                   (* Not a quantifier; no more quantifier processing *))
                 else (
                   meta_quantifier := meta_minmax;
                   (* Fall through *)
                   check_quantifier ~min_repeat:!min_repeat
                     ~max_repeat:!max_repeat)
             | '[' ->
                 (* ---- Character class ---- pcre2_compile.c:3493-3496 *)
                 okquantifier := true;

                 (* pcre2_compile.c:3498-3535 — in another (POSIX) regex
                    library, the ugly syntax [[:<:]] and [[:>:]] is used for
                    "start of word" and "end of word". As these are otherwise
                    illegal sequences, we don't break anything by recognizing
                    them. They are replaced by \b(?=\w) and \b(?<=\w)
                    respectively. Sequences like [a[:<:]] are erroneous and
                    are handled by the normal code below. *)
                 if
                   cx.ptrend - !ptr >= 6
                   && (strncmp_c8_eq pat !ptr "[:<:]]" 6
                      || strncmp_c8_eq pat !ptr "[:>:]]" 6)
                 then (
                   buf.(!pp) <- meta_escape + esc_b;
                   incr pp;
                   if Char.equal pat.[!ptr + 2] '<' then (
                     buf.(!pp) <- meta_lookahead;
                     incr pp)
                   else (
                     buf.(!pp) <- meta_lookbehind;
                     incr pp;
                     has_lookbehind := true;
                     (* The offset is used only for the "non-fixed length"
                        error; this won't occur here, so just store zero. *)
                     putoffset buf pp 0);
                   if Int.equal (!options land Options.ucp) 0 then (
                     buf.(!pp) <- meta_escape + esc_w;
                     incr pp)
                   else (
                     buf.(!pp) <- meta_escape + esc_p;
                     incr pp;
                     buf.(!pp) <- Opcodes.pt_word lsl 16;
                     incr pp);
                   buf.(!pp) <- meta_ket;
                   incr pp;
                   ptr := !ptr + 6 (* C: break — end of the class item *))
                 else
                   (* pcre2_compile.c:3537-3546 — PCRE supports POSIX class
                      stuff inside a class. Perl gives an error if they are
                      encountered at the top level, so we'll do that too. *)
                   let tempptr = ref 0 in
                   if
                     !ptr < cx.ptrend
                     && (Char.equal pat.[!ptr] ':'
                        || Char.equal pat.[!ptr] '.'
                        || Char.equal pat.[!ptr] '=')
                     && check_posix_syntax cx !ptr tempptr
                   then (
                     (* C: errorcode = ( *ptr-- == CHAR_COLON)? ERR12 : ERR13
                        — the test reads the old value, then ptr backs up to
                        the '['. *)
                     cx.errorcode <-
                       (if Char.equal pat.[!ptr] ':' then Errors.err12
                        else Errors.err13);
                     decr ptr;
                     raise_notrace Goto_failed);

                   (* pcre2_compile.c:3548-3572 — process a regular character
                      class. If the first character is '^', set the negation
                      flag. If the first few characters (either before or
                      after ^) are \Q\E or \E or space or tab in extended-more
                      mode, we skip them too. This makes for compatibility
                      with Perl. *)
                   let negate_class = ref false in
                   let broke = ref false in
                   while (not !broke) && !ptr < cx.ptrend do
                     c := getcharinctest ();
                     if Int.equal !c 0x5c (* CHAR_BACKSLASH *) then
                       if !ptr < cx.ptrend && Char.equal pat.[!ptr] 'E' then
                         incr ptr
                       else if
                         cx.ptrend - !ptr >= 3
                         && strncmp_c8_eq pat !ptr "Q\\E" 3
                       then ptr := !ptr + 3
                       else broke := true
                     else if
                       (not (Int.equal (!options land Options.extended_more) 0))
                       && (Int.equal !c 0x20 || Int.equal !c 0x09)
                       (* Note: just these two *)
                     then ()
                     else if (not !negate_class) && Int.equal !c (Char.code '^')
                     then negate_class := true
                     else broke := true
                   done;

                   (* pcre2_compile.c:3574-3582 — now the real contents of the
                      class; c has the first "real" character. Empty classes
                      are permitted only if the option is set. Note the C
                      tests cb->external_options, not the (?i)-tracked local
                      options. *)
                   if
                     Int.equal !c (Char.code ']')
                     && not
                          (Int.equal
                             (cx.external_options land Options.allow_empty_class)
                             0)
                   then (
                     buf.(!pp) <-
                       (if !negate_class then meta_class_empty_not
                        else meta_class_empty);
                     incr pp (* C: break — end of class processing *))
                   else (
                     (* Process a non-empty class
                        (pcre2_compile.c:3584-3587). *)
                     buf.(!pp) <-
                       (if !negate_class then meta_class_not else meta_class);
                     incr pp;
                     let class_range_state = ref range_no in

                     (* pcre2_compile.c:3743-3766 — the CLASS_LITERAL label:
                        handle a literal character, tracking whether values
                        are literal or escaped for range handling (the
                        EBCDIC-motivated state machine described at
                        pcre2_compile.c:3589-3595). *)
                     let class_literal ~char_is_literal =
                       if Int.equal !class_range_state range_started then (
                         if Int.equal !c buf.(!pp - 2) then
                           decr pp (* Optimize one-char range *)
                         else if buf.(!pp - 2) > !c then (
                           (* Check range is in order *)
                           cx.errorcode <- Errors.err8;
                           failed_back ())
                         else (
                           if
                             (not char_is_literal)
                             && Int.equal buf.(!pp - 1) meta_range_literal
                           then buf.(!pp - 1) <- meta_range_escaped;
                           parsed_literal !c);
                         class_range_state := range_no)
                       else (
                         (* Potential start of range *)
                         class_range_state :=
                           if char_is_literal then range_ok_literal
                           else range_ok_escaped;
                         parsed_literal !c)
                     in

                     (* pcre2_compile.c:3597-3904 — loop for the contents of
                        the class. Every non-failing path through the body
                        falls through to the shared CLASS_CONTINUE tail at the
                        bottom of the loop. *)
                     let class_break = ref false in
                     while not !class_break do
                       if !inescq then
                         if
                           (* pcre2_compile.c:3601-3614 — inside \Q...\E
                              everything is literal except \E. char_is_literal
                              is TRUE here (set at the C loop head). *)
                           Int.equal !c 0x5c (* CHAR_BACKSLASH *)
                           && !ptr < cx.ptrend
                           && Char.equal pat.[!ptr] 'E'
                         then (
                           inescq := false (* Reset literal state *);
                           incr ptr (* Skip the 'E'; goto CLASS_CONTINUE *))
                         else class_literal ~char_is_literal:true
                           (* goto CLASS_LITERAL *)
                       else if
                         (* pcre2_compile.c:3616-3620 — skip over space and
                            tab (only) in extended-more mode. *)
                         (not
                            (Int.equal (!options land Options.extended_more) 0))
                         && (Int.equal !c 0x20 || Int.equal !c 0x09)
                       then () (* goto CLASS_CONTINUE *)
                       else if
                         (* pcre2_compile.c:3622-3632 — handle POSIX class
                            names. Perl allows a negation extension of the
                            form [:^name:]. A square bracket that doesn't
                            match the syntax is treated as a literal. We also
                            recognize the POSIX constructions [.ch.] and
                            [=ch=] ("collating elements") and fault them, as
                            Perl 5.6 and 5.8 do. *)
                         Int.equal !c (Char.code '[')
                         && cx.ptrend - !ptr >= 3
                         && (Char.equal pat.[!ptr] ':'
                            || Char.equal pat.[!ptr] '.'
                            || Char.equal pat.[!ptr] '=')
                         && check_posix_syntax cx !ptr tempptr
                       then (
                         let posix_negate = ref false in

                         (* pcre2_compile.c:3637-3646 — Perl treats a hyphen
                            before a POSIX class as a literal, not the start
                            of a range. However, it gives a warning in its
                            warning mode. PCRE does not have a warning mode,
                            so we give an error, because this is likely an
                            error on the user's part. *)
                         if Int.equal !class_range_state range_started then (
                           cx.errorcode <- Errors.err50;
                           raise_notrace Goto_failed);

                         (* pcre2_compile.c:3648-3652 *)
                         if not (Char.equal pat.[!ptr] ':') then (
                           cx.errorcode <- Errors.err13;
                           failed_back ());

                         (* pcre2_compile.c:3654-3658 — if ( *(++ptr) == '^').
                            Safe: check_posix_syntax proved a terminator
                            sequence at tempptr >= ptr + 1, so ptr + 1 is in
                            bounds. *)
                         incr ptr;
                         if Char.equal pat.[!ptr] '^' then (
                           posix_negate := true;
                           incr ptr);

                         (* pcre2_compile.c:3660-3666 *)
                         let posix_class =
                           check_posix_name cx !ptr (!tempptr - !ptr)
                         in
                         if posix_class < 0 then (
                           cx.errorcode <- Errors.err30;
                           raise_notrace Goto_failed);
                         ptr := !tempptr + 2;

                         (* pcre2_compile.c:3668-3679 — Perl treats a hyphen
                            after a POSIX class as a literal, not the start
                            of a range, warning unless the hyphen is the last
                            character in the class. PCRE gives an error. *)
                         if
                           !ptr < cx.ptrend - 1
                           && Char.equal pat.[!ptr] '-'
                           && not (Char.equal pat.[!ptr + 1] ']')
                         then (
                           cx.errorcode <- Errors.err50;
                           raise_notrace Goto_failed);

                         (* pcre2_compile.c:3681-3687 — set "a hyphen is not
                            the start of a range" for the -] case, and also
                            in case the POSIX class is followed by \E or \Q\E
                            (possibly repeated) and *then* a hyphen. *)
                         class_range_state := range_no;

                         (* pcre2_compile.c:3689-3722 — when PCRE2_UCP is
                            set, unless PCRE2_EXTRA_ASCII_POSIX is set, some
                            of the POSIX classes are converted to use Unicode
                            properties \p or \P or, in one case, \h or \H
                            (via posix_substitutes). A negative type with a
                            zero value falls through to behave like a non-UCP
                            POSIX class. SUPPORT_UNICODE is defined in the
                            reference configuration, so this block is always
                            compiled. *)
                         let ucp_done =
                           if
                             (not (Int.equal (!options land Options.ucp) 0))
                             && Int.equal
                                  (!xoptions land Options.extra_ascii_posix)
                                  0
                             && not
                                  ((not
                                      (Int.equal
                                         (!xoptions
                                        land Options.extra_ascii_digit)
                                         0))
                                  && (Int.equal posix_class pc_digit
                                     || Int.equal posix_class pc_xdigit))
                           then
                             let ptype = posix_substitutes.(2 * posix_class) in
                             let pvalue =
                               posix_substitutes.((2 * posix_class) + 1)
                             in
                             if ptype >= 0 then (
                               buf.(!pp) <-
                                 (meta_escape
                                 + if !posix_negate then esc_big_p else esc_p);
                               incr pp;
                               buf.(!pp) <- (ptype lsl 16) lor pvalue;
                               incr pp;
                               true (* goto CLASS_CONTINUE *))
                             else if not (Int.equal pvalue 0) then (
                               buf.(!pp) <-
                                 (meta_escape
                                 + if !posix_negate then esc_big_h else esc_h);
                               incr pp;
                               true (* goto CLASS_CONTINUE *))
                             else false (* Fall through *)
                           else false
                         in

                         (* pcre2_compile.c:3724-3727 — non-UCP POSIX
                            class. *)
                         if not ucp_done then (
                           buf.(!pp) <-
                             (if !posix_negate then meta_posix_neg
                              else meta_posix);
                           incr pp;
                           buf.(!pp) <- posix_class;
                           incr pp))
                       else if
                         (* pcre2_compile.c:3730-3737 — handle potential
                            start of range. *)
                         Int.equal !c (Char.code '-')
                         && !class_range_state >= range_ok_escaped
                       then (
                         buf.(!pp) <-
                           (if Int.equal !class_range_state range_ok_literal
                            then meta_range_literal
                            else meta_range_escaped);
                         incr pp;
                         class_range_state := range_started)
                       else if not (Int.equal !c 0x5c (* CHAR_BACKSLASH *)) then
                         (* pcre2_compile.c:3739-3741 — handle a literal
                            character. *)
                         class_literal ~char_is_literal:true
                       else (
                         (* pcre2_compile.c:3769-3787 — handle escapes in a
                            class. *)
                         tempptr := !ptr;
                         let escape =
                           ref
                             (check_escape cx ptr c ~options:!options
                                ~xoptions:!xoptions ~isclass:true (Some cx))
                         in
                         if not (Int.equal cx.errorcode 0) then (
                           if
                             Int.equal
                               (!xoptions
                              land Options.extra_bad_escape_is_literal)
                               0
                           then raise_notrace Goto_failed;
                           ptr := !tempptr;
                           if !ptr >= cx.ptrend then
                             c := 0x5c (* CHAR_BACKSLASH *)
                           else c := getcharinctest ()
                             (* Get character value, increment pointer *);
                           escape := 0 (* Treat as literal character *));

                         (* pcre2_compile.c:3789-3813 — first switch on the
                            escape value. *)
                         if Int.equal !escape 0 then
                           (* Escaped character code point is in c *)
                           class_literal ~char_is_literal:false
                           (* goto CLASS_LITERAL *)
                         else if Int.equal !escape esc_b then (
                           c := 0x08 (* CHAR_BS: \b is backspace in a class *);
                           class_literal ~char_is_literal:false)
                         else if Int.equal !escape esc_big_q then inescq := true
                           (* Enter literal mode; goto CLASS_CONTINUE *)
                         else if Int.equal !escape esc_big_e then ()
                           (* Ignore orphan \E; goto CLASS_CONTINUE *)
                         else if
                           Int.equal !escape esc_big_b
                           || Int.equal !escape esc_big_r
                           || Int.equal !escape esc_big_x
                         then (
                           (* Always an error in a class *)
                           cx.errorcode <- Errors.err7;
                           decr ptr;
                           raise_notrace Goto_failed)
                         else (
                           (* pcre2_compile.c:3815-3825 — the second part of
                              a range can be a single-character escape
                              sequence (detected above), but not any of the
                              other escapes. Perl treats a hyphen as a
                              literal in such circumstances but warns; PCRE
                              faults it. *)
                           if Int.equal !class_range_state range_started then (
                             cx.errorcode <- Errors.err50;
                             raise_notrace
                               Goto_failed (* Always an error here *));

                           (* pcre2_compile.c:3827-3881 — of the remaining
                              escapes, only those that define characters are
                              allowed in a class. None may start a range. *)
                           class_range_state := range_no;
                           if Int.equal !escape esc_big_n then (
                             cx.errorcode <- Errors.err71;
                             raise_notrace Goto_failed)
                           else if
                             Int.equal !escape esc_big_h
                             || Int.equal !escape esc_h
                             || Int.equal !escape esc_big_v
                             || Int.equal !escape esc_v
                           then (
                             buf.(!pp) <- meta_escape + !escape;
                             incr pp)
                           else if
                             Int.equal !escape esc_d
                             || Int.equal !escape esc_big_d
                             || Int.equal !escape esc_s
                             || Int.equal !escape esc_big_s
                             || Int.equal !escape esc_w
                             || Int.equal !escape esc_big_w
                           then
                             (* These escapes may be converted to Unicode
                                property tests when PCRE2_UCP is set. *)
                             pp :=
                               handle_escdsw cx !escape !pp ~options:!options
                                 ~xoptions:!xoptions
                           else if
                             Int.equal !escape esc_big_p
                             || Int.equal !escape esc_p
                           then (
                             (* pcre2_compile.c:3857-3875 — explicit
                                Unicode property matching. A get_ucp()
                                failure here is `goto FAILED` (not the
                                ESCAPE_FAILED recovery the freestanding arm
                                uses). *)
                             let negated = ref false in
                             let ptype = ref 0 in
                             let pdata = ref 0 in
                             if not (get_ucp cx ptr negated ptype pdata) then
                               raise_notrace Goto_failed;
                             if !negated then
                               escape :=
                                 if Int.equal !escape esc_big_p then esc_p
                                 else esc_big_p;
                             buf.(!pp) <- meta_escape + !escape;
                             incr pp;
                             buf.(!pp) <- (!ptype lsl 16) lor !pdata;
                             incr pp (* End \P and \p *))
                           else (
                             (* All others are not allowed in a class *)
                             cx.errorcode <- Errors.err7;
                             decr ptr;
                             raise_notrace Goto_failed);

                           (* pcre2_compile.c:3883-3891 — Perl gives a
                              warning unless a following hyphen is the last
                              character in the class. PCRE throws an
                              error. *)
                           if
                             !ptr < cx.ptrend - 1
                             && Char.equal pat.[!ptr] '-'
                             && not (Char.equal pat.[!ptr + 1] ']')
                           then (
                             cx.errorcode <- Errors.err50;
                             raise_notrace Goto_failed)));

                       (* CLASS_CONTINUE: pcre2_compile.c:3894-3903 — proceed
                          to next thing in the class. *)
                       if !ptr >= cx.ptrend then (
                         cx.errorcode <- Errors.err6;
                         (* Missing terminating ']' *)
                         raise_notrace Goto_failed);
                       c := getcharinctest ();
                       if Int.equal !c (Char.code ']') && not !inescq then
                         class_break := true
                     done;

                     (* pcre2_compile.c:3906-3915 — -] at the end of a class
                        is a literal '-'. *)
                     if Int.equal !class_range_state range_started then (
                       buf.(!pp - 1) <- Char.code '-';
                       class_range_state := range_no);
                     buf.(!pp) <- meta_class_end;
                     incr pp)
             | '(' ->
                 (* ---- Opening parenthesis ---- pcre2_compile.c:3918-3921 *)
                 if !ptr >= cx.ptrend then unclosed_parenthesis ();
                 (* If ( is not followed by ? it is either a capture or a
                    special verb or an alpha assertion or a positive
                    non-atomic lookahead (pcre2_compile.c:3923-3926). *)
                 if not (Char.equal pat.[!ptr] '?') then (
                   if not (Char.equal pat.[!ptr] '*') then (
                     (* pcre2_compile.c:3930-3947 — handle capturing brackets
                        (or non-capturing if auto-capture is turned off). *)
                     nest_depth := !nest_depth + 1;
                     if Int.equal (!options land Options.no_auto_capture) 0 then (
                       if cx.bracount >= Limits.max_group_number then (
                         cx.errorcode <- Errors.err97;
                         raise_notrace Goto_failed);
                       cx.bracount <- cx.bracount + 1;
                       buf.(!pp) <- meta_capture lor cx.bracount;
                       incr pp)
                     else (
                       buf.(!pp) <- meta_nocapture;
                       incr pp))
                   else if
                     cx.ptrend - !ptr <= 1 || Char.equal pat.[!ptr + 1] ')'
                   then
                     (* pcre2_compile.c:3949-3953 — do nothing for ( * followed
                        by end of pattern or ) so it gives a "bad quantifier"
                        error rather than "(*MARK) must have an argument".
                        (The C also assigns c = ptr[1] here; c is dead
                        afterwards.) *)
                     ()
                   else if
                     (* CHMAX_255(c) is always true for an 8-bit code unit. *)
                     not
                       (Int.equal
                          (Chartables.ctypes (Char.code pat.[!ptr + 1])
                          land Chartables.ctype_lcletter)
                          0)
                   then (
                     (* pcre2_compile.c:3955-3972 — handle "alpha assertions"
                        such as ( *pla:...). Most of these are synonyms for
                        the historical symbolic assertions, but the script
                        run and non-atomic lookaround ones are new. They are
                        distinguished by starting with a lower case letter.
                        Checking both ends of the alphabet makes this work in
                        all character codes. *)
                     let offset = ref 0 in
                     let name = ref 0 in
                     let namelen = ref 0 in
                     if
                       not
                         (read_name cx ptr ~utf ~terminator:0 offset name
                            namelen)
                     then raise_notrace Goto_failed;
                     if !ptr >= cx.ptrend || not (Char.equal pat.[!ptr] ':')
                     then (
                       cx.errorcode <- Errors.err95 (* Malformed *);
                       raise_notrace Goto_failed);

                     (* pcre2_compile.c:3974-3988 — scan the table of alpha
                        assertion names. *)
                     let i = ref 0 in
                     while
                       !i < alascount
                       && not
                            (Int.equal !namelen
                               (String.length (fst alasmeta.(!i)))
                            && strncmp_c8_eq pat !name
                                 (fst alasmeta.(!i))
                                 !namelen)
                     do
                       incr i
                     done;
                     if !i >= alascount then (
                       cx.errorcode <-
                         Errors.err95 (* Alpha assertion not recognized *);
                       raise_notrace Goto_failed);

                     (* pcre2_compile.c:3990-4000 — check for expecting an
                        assertion condition. If so, only atomic lookaround
                        assertions are valid. *)
                     let meta = snd alasmeta.(!i) in
                     if
                       prev_expect_cond_assert > 0
                       && (meta < meta_lookahead || meta > meta_lookbehindnot)
                     then (
                       cx.errorcode <-
                         (if
                            Int.equal meta meta_lookahead_na
                            || Int.equal meta meta_lookbehind_na
                          then Errors.err98
                          else Errors.err28)
                       (* (Atomic) assertion expected *);
                       raise_notrace Goto_failed);

                     (* pcre2_compile.c:4002-4060 — the lookaround alphabetic
                        synonyms can mostly be handled by jumping to the code
                        that handles the traditional symbolic forms (the
                        switch's default case — ERR89, "should never occur
                        because the meta values come from a table above" — is
                        the final else here). *)
                     if Int.equal meta meta_atomic then atomic_group ()
                     else if Int.equal meta meta_lookahead then
                       positive_look_ahead ~prev_expect_cond_assert
                     else if Int.equal meta meta_lookahead_na then
                       positive_nonatomic_look_ahead ~prev_expect_cond_assert
                     else if Int.equal meta meta_lookaheadnot then
                       negative_look_ahead ~prev_expect_cond_assert
                     else if
                       Int.equal meta meta_lookbehind
                       || Int.equal meta meta_lookbehindnot
                       || Int.equal meta meta_lookbehind_na
                     then (
                       buf.(!pp) <- meta;
                       incr pp;
                       decr ptr;
                       post_lookbehind ~prev_expect_cond_assert)
                     else if
                       Int.equal meta meta_script_run
                       || Int.equal meta meta_atomic_script_run
                     then (
                       (* pcre2_compile.c:4030-4055 — the script run
                          facilities are handled here. Unicode support is
                          required (the C's no-Unicode ERR96 arm at
                          4056-4059 is unreachable: this port always has
                          Unicode support). Always record a
                          META_SCRIPT_RUN item. Then, for the atomic
                          version, insert META_ATOMIC and remember
                          (NSF_ATOMICSR) that we need two META_KETs at
                          the end. *)
                       buf.(!pp) <- meta_script_run;
                       incr pp;
                       nest_depth := !nest_depth + 1;
                       incr ptr;
                       if Int.equal meta meta_atomic_script_run then (
                         buf.(!pp) <- meta_atomic;
                         incr pp;
                         if Int.equal !top_nest (-1) then top_nest := 0
                         else (
                           incr top_nest;
                           if !top_nest >= nest_slots then (
                             cx.errorcode <- Errors.err84;
                             raise_notrace Goto_failed));
                         let tn = nests.(!top_nest) in
                         tn.nest_depth <- !nest_depth;
                         tn.flags <- nsf_atomicsr;
                         tn.options <- !options land parse_tracked_options;
                         tn.xoptions <-
                           !xoptions land parse_tracked_extra_options))
                     else (
                       (* pcre2_compile.c:4007-4009 *)
                       cx.errorcode <- Errors.err89;
                       raise_notrace Goto_failed))
                   else
                     (* ---- Handle ( *VERB) and ( *VERB:NAME) ----
                        pcre2_compile.c:4064-4076 *)
                     let offset = ref 0 in
                     let name = ref 0 in
                     let namelen = ref 0 in
                     if
                       not
                         (read_name cx ptr ~utf ~terminator:0 offset name
                            namelen)
                     then raise_notrace Goto_failed;
                     if
                       !ptr >= cx.ptrend
                       || (not (Char.equal pat.[!ptr] ':'))
                          && not (Char.equal pat.[!ptr] ')')
                     then (
                       cx.errorcode <- Errors.err60 (* Malformed *);
                       raise_notrace Goto_failed);

                     (* pcre2_compile.c:4078-4092 — scan the table of verb
                        names. *)
                     let i = ref 0 in
                     while
                       !i < verbcount
                       && not
                            (let vname, _, _ = verbs.(!i) in
                             Int.equal !namelen (String.length vname)
                             && strncmp_c8_eq pat !name vname !namelen)
                     do
                       incr i
                     done;
                     if !i >= verbcount then (
                       cx.errorcode <- Errors.err60 (* Verb not recognized *);
                       raise_notrace Goto_failed);
                     let _, verb_meta, verb_has_arg = verbs.(!i) in

                     (* pcre2_compile.c:4094-4098 — an empty argument is
                        treated as no argument. *)
                     if
                       Char.equal pat.[!ptr] ':'
                       && !ptr + 1 < cx.ptrend
                       && Char.equal pat.[!ptr + 1] ')'
                     then incr ptr (* Advance to the closing parens *);

                     (* pcre2_compile.c:4100-4106 — check for mandatory
                        non-empty argument; this is ( *MARK). *)
                     if verb_has_arg > 0 && not (Char.equal pat.[!ptr] ':') then (
                       cx.errorcode <- Errors.err66;
                       raise_notrace Goto_failed);

                     (* pcre2_compile.c:4108-4112 — remember where this verb,
                        possibly with a preceding ( *MARK), starts, for
                        handling quantified ( *ACCEPT). *)
                     verbstartptr := !pp;
                     okquantifier := Int.equal verb_meta meta_accept;

                     (* pcre2_compile.c:4114-4151 — it appears that Perl
                        allows any characters whatsoever, other than a closing
                        parenthesis, to appear in arguments ("names"), so we
                        no longer insist on letters, digits, and underscores.
                        Perl does not, however, do any interpretation within
                        arguments, and has no means of including a closing
                        parenthesis. PCRE supports escape processing but only
                        when it is requested by an option. We set inverbname
                        true here, and let the main loop take care of this so
                        that escape and \x processing is done by the main code
                        above. The C's `if ( *ptr++ == CHAR_COLON)` reads the
                        delimiter, then skips past it. *)
                     let was_colon = Char.equal pat.[!ptr] ':' in
                     incr ptr (* Skip past : or ) *);
                     if was_colon then (
                       (* Some optional arguments can be treated as a
                          preceding ( *MARK) (pcre2_compile.c:4125-4131) *)
                       if verb_has_arg < 0 then (
                         add_after_mark := verb_meta;
                         buf.(!pp) <- meta_mark;
                         incr pp)
                       else (
                         (* The remaining verbs with arguments (except *MARK)
                            need a different opcode
                            (pcre2_compile.c:4133-4140). *)
                         buf.(!pp) <-
                           (verb_meta
                           +
                           if not (Int.equal verb_meta meta_mark) then
                             0x0001_0000
                           else 0);
                         incr pp);

                       (* Set up for reading the name in the main loop
                          (pcre2_compile.c:4142-4147). *)
                       verblengthptr := !pp;
                       incr pp;
                       verbnamestart := !ptr;
                       inverbname := true)
                     else (
                       (* No verb "name" argument
                          (pcre2_compile.c:4148-4151) *)
                       buf.(!pp) <- verb_meta;
                       incr pp) (* End of ( *VERB) handling *))
                 else (
                   (* ---- Items starting (? ---- pcre2_compile.c:4157-4167.
                      The type of item is determined by what follows (?.
                      Handle (?| and option changes under "default" (the last
                      arm here) because both need a new block on the nest
                      stack. Comments starting with (?# were handled above.
                      Note the ambiguity of (?-: a digit after it means a
                      relative recursion or subroutine call, otherwise it is
                      an option unsetting. *)
                   incr ptr;
                   if !ptr >= cx.ptrend then unclosed_parenthesis ();
                   match pat.[!ptr] with
                   | 'P' ->
                       (* ---- Python syntax support ----
                          pcre2_compile.c:4355-4387 *)
                       incr ptr;
                       if !ptr >= cx.ptrend then unclosed_parenthesis ();
                       (* (?P<name> is the same as (?<name>, which defines a
                          named group (pcre2_compile.c:4360-4366). *)
                       if Char.equal pat.[!ptr] '<' then
                         define_name ~terminator:(Char.code '>')
                         (* goto DEFINE_NAME *)
                       else if Char.equal pat.[!ptr] '>' then
                         (* (?P>name) is the same as (?&name), which is a
                            recursion or subroutine call: goto RECURSE_BY_NAME
                            (pcre2_compile.c:4368-4371). *)
                         recurse_by_name ()
                       else if not (Char.equal pat.[!ptr] '=') then (
                         (* (?P=name) is the same as \k<name>, a back
                            reference by name. Anything else after (?P is an
                            error (pcre2_compile.c:4373-4380). *)
                         cx.errorcode <- Errors.err41;
                         raise_notrace Goto_failed)
                       else
                         (* pcre2_compile.c:4381-4387 *)
                         let offset = ref 0 in
                         let name = ref 0 in
                         let namelen = ref 0 in
                         if
                           not
                             (read_name cx ptr ~utf ~terminator:(Char.code ')')
                                offset name namelen)
                         then raise_notrace Goto_failed;
                         buf.(!pp) <- meta_backref_byname;
                         incr pp;
                         buf.(!pp) <- !namelen;
                         incr pp;
                         putoffset buf pp !offset;
                         okquantifier := true (* End of (?P processing *)
                   | 'R' ->
                       (* ---- Recursion/subroutine calls by number ----
                          pcre2_compile.c:4390-4400 — (?R) == (?R0). *)
                       incr ptr;
                       if !ptr >= cx.ptrend || not (Char.equal pat.[!ptr] ')')
                       then (
                         cx.errorcode <- Errors.err58;
                         raise_notrace Goto_failed);
                       set_recursion ~i:0
                   | '+' ->
                       (* pcre2_compile.c:4402-4411 — an item starting (?-
                          followed by a digit comes here via the "default"
                          case because (?- followed by a non-digit is an
                          options setting. *)
                       if cx.ptrend - !ptr < 2 || not (is_digit pat.[!ptr + 1])
                       then (
                         cx.errorcode <- Errors.err29 (* Missing number *);
                         raise_notrace Goto_failed);
                       (* Fall through *)
                       recursion_bynumber ()
                   | '0' .. '9' ->
                       (* pcre2_compile.c:4413-4434 *)
                       recursion_bynumber ()
                   | '&' ->
                       (* ---- Recursion/subroutine calls by name ----
                          pcre2_compile.c:4437-4446 *)
                       recurse_by_name ()
                   | 'C' ->
                       (* ---- Callout with numerical or string argument ----
                          pcre2_compile.c:4449-4563 *)
                       incr ptr;
                       if !ptr >= cx.ptrend then unclosed_parenthesis ();

                       (* pcre2_compile.c:4454-4463 — if the previous item
                          was a condition starting (?(? an assertion,
                          optionally preceded by a callout, is expected. This
                          is checked later on, during actual compilation.
                          However we need to identify this kind of assertion
                          in this pass because it must not be qualified. The
                          value of expect_cond_assert is set to 2 after (?(?
                          is processed. We decrement it for a callout - still
                          leaving a positive value that identifies the
                          assertion. Multiple callouts or any other items
                          will make it zero or less, which doesn't matter
                          because they will cause an error later. *)
                       expect_cond_assert := prev_expect_cond_assert - 1;

                       (* pcre2_compile.c:4465-4473 — if previous_callout is
                          not NULL, it means this follows a previous callout.
                          If it was a manual callout, do nothing; this means
                          its "length of next pattern item" field will remain
                          zero. If it was an automatic callout, abolish it. *)
                       if
                         !previous_callout >= 0
                         && (not
                               (Int.equal
                                  (!options land Options.auto_callout)
                                  0))
                         && Int.equal !previous_callout (!pp - 4)
                         && Int.equal buf.(!pp - 1) 255
                       then pp := !previous_callout;

                       (* pcre2_compile.c:4475-4479 — save for updating next
                          pattern item length, and skip one item before
                          completing. *)
                       previous_callout := !pp;
                       after_manual_callout := 1;

                       (* pcre2_compile.c:4481-4527 — handle a string
                          argument; specific delimiter is required. *)
                       (if
                          (not (Char.equal pat.[!ptr] ')'))
                          && not (is_digit pat.[!ptr])
                        then (
                          let startptr = !ptr in
                          (* pcre2_compile.c:4488-4501 — look up the ending
                             delimiter paired with *ptr
                             (pcre2_tables.c:73-81). *)
                          let delimiter = ref 0 in
                          let i = ref 0 in
                          while
                            Int.equal !delimiter 0
                            && not
                                 (Int.equal Tables.callout_start_delims.(!i) 0)
                          do
                            if
                              Int.equal (Char.code pat.[!ptr])
                                Tables.callout_start_delims.(!i)
                            then delimiter := Tables.callout_end_delims.(!i)
                            else incr i
                          done;
                          if Int.equal !delimiter 0 then (
                            cx.errorcode <- Errors.err82;
                            raise_notrace Goto_failed);

                          buf.(!pp) <- meta_callout_string;
                          pp := !pp + 3 (* Skip pattern info *);

                          (* pcre2_compile.c:4506-4516 — scan to the ending
                             delimiter; a doubled delimiter is a literal
                             occurrence and does not end the string. *)
                          let broke = ref false in
                          while not !broke do
                            incr ptr;
                            if !ptr >= cx.ptrend then (
                              cx.errorcode <- Errors.err81;
                              ptr := startptr
                              (* To give a more useful message *);
                              raise_notrace Goto_failed);
                            if
                              Int.equal (Char.code pat.[!ptr]) !delimiter
                            then (
                              incr ptr;
                              if
                                !ptr >= cx.ptrend
                                || not
                                     (Int.equal (Char.code pat.[!ptr])
                                        !delimiter)
                              then broke := true)
                          done;

                          (* pcre2_compile.c:4518-4526 — store the length
                             (including both delimiters) and the pattern
                             offset of the string. The C's
                             calloutlength > UINT32_MAX test (ERR72) is kept
                             even though a pattern (an OCaml string) can
                             never be that long here. *)
                          let calloutlength = !ptr - startptr in
                          if calloutlength > 0xFFFF_FFFF then (
                            cx.errorcode <- Errors.err72;
                            raise_notrace Goto_failed);
                          buf.(!pp) <- calloutlength;
                          incr pp;
                          let offset = startptr in
                          (* offset = startptr - cb->start_pattern *)
                          putoffset buf pp offset)
                        else (
                          (* pcre2_compile.c:4529-4547 — handle a callout
                             with an optional numerical argument, which must
                             be less than or equal to 255. A missing argument
                             gives 0. *)
                          let n = ref 0 in
                          buf.(!pp) <- meta_callout_number
                          (* Numerical callout *);
                          pp := !pp + 3 (* Skip pattern info *);
                          while !ptr < cx.ptrend && is_digit pat.[!ptr] do
                            (* n = n * 10 + *ptr++ - CHAR_0 *)
                            n := (!n * 10) + Char.code pat.[!ptr] - 0x30;
                            incr ptr;
                            if !n > 255 then (
                              cx.errorcode <- Errors.err38;
                              raise_notrace Goto_failed)
                          done;
                          buf.(!pp) <- !n;
                          incr pp));

                       (* pcre2_compile.c:4549-4556 — both formats must have
                          a closing parenthesis. *)
                       if
                         !ptr >= cx.ptrend
                         || not (Char.equal pat.[!ptr] ')')
                       then (
                         cx.errorcode <- Errors.err39;
                         raise_notrace Goto_failed);
                       incr ptr;

                       (* pcre2_compile.c:4558-4562 — remember the offset to
                          the next item in the pattern, and set a default
                          length. This should get updated after the next item
                          is read. *)
                       buf.(!previous_callout + 1) <- !ptr;
                       buf.(!previous_callout + 2) <- 0
                       (* End callout *)
                   | '(' ->
                       (* ---- Conditional group ----
                          pcre2_compile.c:4566-4738 — a condition can be an
                          assertion, a number (referring to a numbered group's
                          having been set), a name (referring to a named
                          group), or 'R', referring to overall recursion.
                          R<digits> and R&name are also permitted for
                          recursion state tests. Numbers may be preceded by +
                          or - to specify a relative group number.

                          There are several syntaxes for testing a named
                          group: (?(name)) is used by Python; Perl 5.10
                          onwards uses (?(<name>) or (?('name')).

                          There are two unfortunate ambiguities. 'R' can be
                          the recursive thing or the name 'R' (and similarly
                          for 'R' followed by digits). 'DEFINE' can be the
                          Perl DEFINE feature or the Python named test. We
                          look for a name first; if not found, we try the
                          other case.

                          For compatibility with auto-callouts, we allow a
                          callout to be specified before a condition that is
                          an assertion. *)
                       incr ptr;
                       if !ptr >= cx.ptrend then unclosed_parenthesis ();
                       nest_depth := !nest_depth + 1;

                       (* pcre2_compile.c:4589-4603 — if the next character
                          is ? or * there must be an assertion next
                          (optionally preceded by a callout). We do not check
                          this here, but instead we set expect_cond_assert to
                          2. If this is still greater than zero (callouts
                          decrement it) when the next assertion is read, it
                          will be marked as a condition that must not be
                          repeated. A value greater than zero also causes
                          checking that an assertion (possibly with callout)
                          follows. *)
                       if Char.equal pat.[!ptr] '?' || Char.equal pat.[!ptr] '*'
                       then (
                         buf.(!pp) <- meta_cond_assert;
                         incr pp;
                         decr ptr
                         (* Pull pointer back to the opening parenthesis. *);
                         expect_cond_assert := 2 (* break: end of conditional *))
                       else
                         (* pcre2_compile.c:4605-4619 — handle
                            (?([+-]number)... *)
                         let i = ref 0 in
                         (if
                            read_number cx ptr ~allow_sign:cx.bracount
                              ~max_value:Limits.max_group_number
                              ~max_error:Errors.err61 i
                          then (
                            if !i <= 0 then (
                              cx.errorcode <- Errors.err15;
                              raise_notrace Goto_failed);
                            buf.(!pp) <- meta_cond_number;
                            incr pp;
                            let offset = !ptr - 2 in
                            (* ptr - cb->start_pattern - 2 *)
                            putoffset buf pp offset;
                            buf.(!pp) <- !i;
                            incr pp)
                          else if not (Int.equal cx.errorcode 0) then
                            raise_notrace Goto_failed (* Number too big *)
                          else if
                            (* pcre2_compile.c:4622-4663 — no number found.
                               Handle the special case
                               (?(VERSION[>]=n.m)... *)
                            cx.ptrend - !ptr >= 10
                            && strncmp_c8_eq pat !ptr "VERSION" 7
                            && not (Char.equal pat.[!ptr + 7] ')')
                          then (
                            let ge = ref 0 in
                            let major = ref 0 in
                            let minor = ref 0 in
                            ptr := !ptr + 7;
                            if Char.equal pat.[!ptr] '>' then (
                              ge := 1;
                              incr ptr);

                            (* pcre2_compile.c:4639-4643 — NOTE: cannot write
                               IS_DIGIT( *(++ptr)) here because IS_DIGIT
                               references its argument twice. *)
                            if not (Char.equal pat.[!ptr] '=') then
                              bad_version_condition ()
                            else (
                              incr ptr;
                              if not (is_digit pat.[!ptr]) then
                                bad_version_condition ());

                            if
                              not
                                (read_number cx ptr ~allow_sign:(-1)
                                   ~max_value:1000 ~max_error:Errors.err79 major)
                            then raise_notrace Goto_failed;

                            if !ptr >= cx.ptrend then bad_version_condition ();
                            if Char.equal pat.[!ptr] '.' then (
                              incr ptr;
                              if !ptr >= cx.ptrend || not (is_digit pat.[!ptr])
                              then bad_version_condition ();
                              minor :=
                                (Char.code pat.[!ptr] - Char.code '0') * 10;
                              incr ptr;
                              if !ptr >= cx.ptrend then bad_version_condition ();
                              if is_digit pat.[!ptr] then (
                                minor :=
                                  !minor + Char.code pat.[!ptr] - Char.code '0';
                                incr ptr);
                              if
                                !ptr >= cx.ptrend
                                || not (Char.equal pat.[!ptr] ')')
                              then bad_version_condition ());

                            buf.(!pp) <- meta_cond_version;
                            incr pp;
                            buf.(!pp) <- !ge;
                            incr pp;
                            buf.(!pp) <- !major;
                            incr pp;
                            buf.(!pp) <- !minor;
                            incr pp)
                          else
                            (* pcre2_compile.c:4665-4728 — all the remaining
                               cases now require us to read a name. We cannot
                               at this stage distinguish ambiguous cases such
                               as (?(R12) which might be a recursion test by
                               number or a name, because the named groups
                               have not yet all been identified. Those cases
                               are treated as names, but given a different
                               META code. *)
                            let was_r_ampersand = ref false in
                            let terminator =
                              if
                                Char.equal pat.[!ptr] 'R'
                                && cx.ptrend - !ptr > 1
                                && Char.equal pat.[!ptr + 1] '&'
                              then (
                                was_r_ampersand := true;
                                incr ptr;
                                Char.code ')')
                              else if Char.equal pat.[!ptr] '<' then
                                Char.code '>'
                              else if Char.equal pat.[!ptr] '\'' then
                                Char.code '\''
                              else (
                                decr ptr (* Point to char before name *);
                                Char.code ')')
                            in
                            let offset = ref 0 in
                            let name = ref 0 in
                            let namelen = ref 0 in
                            if
                              not
                                (read_name cx ptr ~utf ~terminator offset name
                                   namelen)
                            then raise_notrace Goto_failed;

                            (* pcre2_compile.c:4693-4699 — handle
                               (?(R&name) *)
                            if !was_r_ampersand then (
                              buf.(!pp) <- meta_cond_rname;
                              decr ptr (* Back to closing parens *))
                            else if Int.equal terminator (Char.code ')') then (
                              (* pcre2_compile.c:4701-4717 — handle (?(name).
                                 If the name is "DEFINE" we identify it with
                                 a special code. Likewise if the name
                                 consists of R followed only by digits.
                                 Otherwise, handle it like a quoted name. *)
                              (if
                                 Int.equal !namelen 6
                                 && strncmp_c8_eq pat !name "DEFINE" 6
                               then buf.(!pp) <- meta_cond_define
                               else
                                 let i = ref 1 in
                                 while
                                   !i < !namelen && is_digit pat.[!name + !i]
                                 do
                                   incr i
                                 done;
                                 buf.(!pp) <-
                                   (if
                                      Char.equal pat.[!name] 'R'
                                      && !i >= !namelen
                                    then meta_cond_rnumber
                                    else meta_cond_name));
                              decr ptr (* Back to closing parens *))
                            else
                              (* pcre2_compile.c:4719-4721 — handle
                                 (?('name') or (?(<name>) *)
                              buf.(!pp) <- meta_cond_name;

                            (* pcre2_compile.c:4723-4727 — all these cases
                               except DEFINE end with the name length and
                               offset; DEFINE just has an offset (for the
                               "too many branches" error). *)
                            let stored = buf.(!pp) in
                            incr pp;
                            if not (Int.equal stored meta_cond_define) then (
                              buf.(!pp) <- !namelen;
                              incr pp);
                            putoffset buf pp !offset
                            (* End cases that read a name *));

                         (* pcre2_compile.c:4730-4737 — check the closing
                            parenthesis of the condition. *)
                         if !ptr >= cx.ptrend || not (Char.equal pat.[!ptr] ')')
                         then (
                           cx.errorcode <- Errors.err24;
                           raise_notrace Goto_failed);
                         incr ptr (* End of condition processing *)
                   | '>' ->
                       (* ---- Atomic group ---- pcre2_compile.c:4741-4748 *)
                       atomic_group ()
                   | '=' ->
                       (* ---- Lookahead assertions ----
                          pcre2_compile.c:4753-4757 *)
                       positive_look_ahead ~prev_expect_cond_assert
                   | '*' ->
                       (* pcre2_compile.c:4759-4763 — the (?* non-atomic
                          form. *)
                       positive_nonatomic_look_ahead ~prev_expect_cond_assert
                   | '!' ->
                       (* pcre2_compile.c:4765-4769 *)
                       negative_look_ahead ~prev_expect_cond_assert
                   | '<' ->
                       (* ---- Lookbehind assertions ----
                          pcre2_compile.c:4774-4785 — (?< followed by = or !
                          or * is a lookbehind assertion. Otherwise (?< is the
                          start of the name of a capturing group. *)
                       if
                         cx.ptrend - !ptr <= 1
                         || (not (Char.equal pat.[!ptr + 1] '='))
                            && (not (Char.equal pat.[!ptr + 1] '!'))
                            && not (Char.equal pat.[!ptr + 1] '*')
                       then define_name ~terminator:(Char.code '>')
                         (* goto DEFINE_NAME *)
                       else (
                         (* pcre2_compile.c:4786-4795 *)
                         buf.(!pp) <-
                           (if Char.equal pat.[!ptr + 1] '=' then
                              meta_lookbehind
                            else if Char.equal pat.[!ptr + 1] '!' then
                              meta_lookbehindnot
                            else meta_lookbehind_na);
                         incr pp;
                         post_lookbehind ~prev_expect_cond_assert)
                   | '\'' ->
                       (* ---- Define a named group ----
                          pcre2_compile.c:4824-4831 — a named group may be
                          defined as (?'name') or (?<name>); DEFINE_NAME with
                          the terminator set to the apostrophe. *)
                       define_name ~terminator:(Char.code '\'')
                   | _ ->
                       (* pcre2_compile.c:4169-4352 — the C switch's default
                          case (first in the source): (?-digit relative
                          recursion, else (?| or a (possibly empty) option
                          setting, optionally followed by a non-capturing
                          group. *)
                       if
                         Char.equal pat.[!ptr] '-'
                         && cx.ptrend - !ptr > 1
                         && is_digit pat.[!ptr + 1]
                       then
                         (* goto RECURSION_BYNUMBER (the + case is handled by
                            CHAR_PLUS above, pcre2_compile.c:4170-4171). *)
                         recursion_bynumber ()
                       else (
                         (* pcre2_compile.c:4176-4186 *)
                         nest_depth := !nest_depth + 1;
                         if Int.equal !top_nest (-1) then top_nest := 0
                         else (
                           incr top_nest;
                           if !top_nest >= nest_slots then (
                             cx.errorcode <- Errors.err84;
                             raise_notrace Goto_failed));
                         let tn = nests.(!top_nest) in
                         tn.nest_depth <- !nest_depth;
                         tn.flags <- 0;
                         tn.options <- !options land parse_tracked_options;
                         tn.xoptions <-
                           !xoptions land parse_tracked_extra_options;

                         if Char.equal pat.[!ptr] '|' then (
                           (* pcre2_compile.c:4188-4199 — start of a
                              non-capturing group that resets the capture count
                              for each branch. *)
                           tn.reset_group <- cx.bracount;
                           tn.max_group <- cx.bracount;
                           tn.flags <- tn.flags lor nsf_reset;
                           cx.external_flags <- cx.external_flags lor dupcapused;
                           buf.(!pp) <- meta_nocapture;
                           incr pp;
                           incr ptr)
                         else
                           (* pcre2_compile.c:4201-4351 — scan for options
                              imnrsxJU (and the two-character a.. sequences) to
                              be set or unset. The C's optset/xoptset
                              accumulator pointers become the [setting] selector
                              consulted by add_opt/add_xopt. *)
                           let hyphenok = ref true in
                           let oldoptions = !options in
                           let oldxoptions = !xoptions in
                           tn.reset_group <- 0;
                           tn.max_group <- 0;
                           let set = ref 0 in
                           let unset = ref 0 in
                           let xset = ref 0 in
                           let xunset = ref 0 in
                           let setting = ref true in
                           (* optset = &set; xoptset = &xset *)
                           let add_opt v =
                             if !setting then set := !set lor v
                             else unset := !unset lor v
                           in
                           let add_xopt v =
                             if !setting then xset := !xset lor v
                             else xunset := !xunset lor v
                           in

                           (* pcre2_compile.c:4216-4225 — ^ at the start unsets
                              irmnsx and disables the subsequent use of -. *)
                           if !ptr < cx.ptrend && Char.equal pat.[!ptr] '^' then (
                             options :=
                               !options
                               land lnot
                                      (Options.caseless lor Options.multiline
                                     lor Options.no_auto_capture
                                     lor Options.dotall lor Options.extended
                                     lor Options.extended_more);
                             xoptions :=
                               !xoptions
                               land lnot Options.extra_caseless_restrict;
                             hyphenok := false;
                             incr ptr);

                           (* pcre2_compile.c:4227-4313 *)
                           while
                             !ptr < cx.ptrend
                             && (not (Char.equal pat.[!ptr] ')'))
                             && not (Char.equal pat.[!ptr] ':')
                           do
                             (* switch ( *ptr++ ) *)
                             let ch = pat.[!ptr] in
                             incr ptr;
                             match ch with
                             | '-' ->
                                 (* pcre2_compile.c:4232-4242 *)
                                 if not !hyphenok then (
                                   cx.errorcode <- Errors.err94;
                                   decr ptr (* Correct the offset *);
                                   raise_notrace Goto_failed);
                                 setting := false
                                 (* optset = &unset; xoptset = &xunset *);
                                 hyphenok := false
                             | 'a' ->
                                 (* pcre2_compile.c:4244-4283 — there are some
                                    two-character sequences that start with 'a';
                                    a bare 'a' sets all the ASCII options
                                    together. *)
                                 let matched2 =
                                   !ptr < cx.ptrend
                                   &&
                                   match pat.[!ptr] with
                                   | 'D' ->
                                       add_xopt Options.extra_ascii_bsd;
                                       incr ptr;
                                       true
                                   | 'P' ->
                                       add_xopt
                                         (Options.extra_ascii_posix
                                        lor Options.extra_ascii_digit);
                                       incr ptr;
                                       true
                                   | 'S' ->
                                       add_xopt Options.extra_ascii_bss;
                                       incr ptr;
                                       true
                                   | 'T' ->
                                       add_xopt Options.extra_ascii_digit;
                                       incr ptr;
                                       true
                                   | 'W' ->
                                       add_xopt Options.extra_ascii_bsw;
                                       incr ptr;
                                       true
                                   | _ -> false
                                 in
                                 if not matched2 then
                                   add_xopt
                                     (Options.extra_ascii_bsd
                                    lor Options.extra_ascii_bss
                                    lor Options.extra_ascii_bsw
                                    lor Options.extra_ascii_digit
                                    lor Options.extra_ascii_posix)
                             | 'J' ->
                                 (* pcre2_compile.c:4285-4288 — record that it
                                    changed in the external options. *)
                                 add_opt Options.dupnames;
                                 cx.external_flags <-
                                   cx.external_flags lor jchanged
                             | 'i' -> add_opt Options.caseless
                             | 'm' -> add_opt Options.multiline
                             | 'n' -> add_opt Options.no_auto_capture
                             | 'r' -> add_xopt Options.extra_caseless_restrict
                             | 's' -> add_opt Options.dotall
                             | 'U' -> add_opt Options.ungreedy
                             | 'x' ->
                                 (* pcre2_compile.c:4297-4306 — if x appears
                                    twice it sets the extended extended
                                    option. *)
                                 add_opt Options.extended;
                                 if
                                   !ptr < cx.ptrend && Char.equal pat.[!ptr] 'x'
                                 then (
                                   add_opt Options.extended_more;
                                   incr ptr)
                             | _ ->
                                 (* pcre2_compile.c:4308-4311 *)
                                 cx.errorcode <- Errors.err11;
                                 decr ptr (* Correct the offset *);
                                 raise_notrace Goto_failed
                           done;

                           (* pcre2_compile.c:4315-4324 — if we are setting
                              extended without extended-more, ensure that any
                              existing extended-more gets unset. Also, unsetting
                              extended must also unset extended-more. *)
                           if
                             Int.equal
                               (!set
                               land (Options.extended lor Options.extended_more)
                               )
                               Options.extended
                             || not (Int.equal (!unset land Options.extended) 0)
                           then unset := !unset lor Options.extended_more;

                           options := !options lor !set land lnot !unset;
                           xoptions := !xoptions lor !xset land lnot !xunset;

                           (* pcre2_compile.c:4326-4341 — if the options ended
                              with ')' this is not the start of a nested group
                              with option changes, so the options change at this
                              level: if the previous level set up a nest block,
                              discard the one just created, otherwise adjust it
                              for the previous level. If the options ended with
                              ':' we are starting a non-capturing group,
                              possibly with an options setting. *)
                           if !ptr >= cx.ptrend then unclosed_parenthesis ();
                           let term = pat.[!ptr] in
                           incr ptr (* *ptr++ *);
                           if Char.equal term ')' then (
                             nest_depth := !nest_depth - 1;
                             (* This is not a nested group after all. *)
                             if
                               !top_nest > 0
                               && Int.equal nests.(!top_nest - 1).nest_depth
                                    !nest_depth
                             then decr top_nest
                             else nests.(!top_nest).nest_depth <- !nest_depth)
                           else (
                             buf.(!pp) <- meta_nocapture;
                             incr pp);

                           (* pcre2_compile.c:4343-4350 — if nothing changed, no
                              need to record. *)
                           if
                             (not (Int.equal !options oldoptions))
                             || not (Int.equal !xoptions oldxoptions)
                           then (
                             buf.(!pp) <- meta_options;
                             incr pp;
                             buf.(!pp) <- !options;
                             incr pp;
                             buf.(!pp) <- !xoptions;
                             incr pp)))
             (* ---- Branch terminators ---- *)
             | '|' ->
                 (* pcre2_compile.c:4929-4942 — alternation: reset the capture
                    count if we are in a (?| group. *)
                 if
                   !top_nest >= 0
                   && Int.equal nests.(!top_nest).nest_depth !nest_depth
                   && not (Int.equal (nests.(!top_nest).flags land nsf_reset) 0)
                 then (
                   if cx.bracount > nests.(!top_nest).max_group then
                     nests.(!top_nest).max_group <- cx.bracount;
                   cx.bracount <- nests.(!top_nest).reset_group);
                 buf.(!pp) <- meta_alt;
                 incr pp
             | ')' ->
                 (* pcre2_compile.c:4944-4975 — end of group; reset the
                    capture count to the maximum if we are in a (?| group
                    and/or reset the options that are tracked during parsing.
                    Disallow quantifier for a condition that is an
                    assertion. *)
                 okquantifier := true;
                 if
                   !top_nest >= 0
                   && Int.equal nests.(!top_nest).nest_depth !nest_depth
                 then (
                   let tn = nests.(!top_nest) in
                   options :=
                     !options land lnot parse_tracked_options lor tn.options;
                   xoptions :=
                     !xoptions
                     land lnot parse_tracked_extra_options
                     lor tn.xoptions;
                   if
                     (not (Int.equal (tn.flags land nsf_reset) 0))
                     && tn.max_group > cx.bracount
                   then cx.bracount <- tn.max_group;
                   if not (Int.equal (tn.flags land nsf_condassert) 0) then
                     okquantifier := false;
                   if not (Int.equal (tn.flags land nsf_atomicsr) 0) then (
                     buf.(!pp) <- meta_ket;
                     incr pp);
                   if Int.equal !top_nest 0 then top_nest := -1
                   else decr top_nest);
                 if Int.equal !nest_depth 0 then (
                   (* Unmatched closing parenthesis *)
                   cx.errorcode <- Errors.err22;
                   failed_back ());
                 nest_depth := !nest_depth - 1;
                 buf.(!pp) <- meta_ket;
                 incr pp
             | _ ->
                 (* pcre2_compile.c:3197-3199 — non-special character (the C
                    switch's default case, first in the source). *)
                 parsed_literal !c
         with Loop_continue -> ()
       done;

       (* pcre2_compile.c:4979-4985 — end of pattern reached. Check for
          missing ) at the end of a verb name. *)
       if !inverbname && !ptr >= cx.ptrend then (
         cx.errorcode <- Errors.err60;
         raise_notrace Goto_failed));

    (* PARSED_END: pcre2_compile.c:4987-5017 — manage callout for the final
       item, insert trailing items for word and line matching, terminate
       the parsed pattern, then return success if all groups are closed. *)
    pp := manage_callouts cx !ptr previous_callout ~auto_callout !pp;

    if not (Int.equal (!xoptions land Options.extra_match_line) 0) then (
      buf.(!pp) <- meta_ket;
      incr pp;
      buf.(!pp) <- meta_dollar;
      incr pp)
    else if not (Int.equal (!xoptions land Options.extra_match_word) 0) then (
      buf.(!pp) <- meta_ket;
      incr pp;
      buf.(!pp) <- meta_escape + esc_b;
      incr pp);

    if !pp >= cx.parsed_pattern_end then (
      cx.errorcode <- Errors.err63;
      (* Internal error (parsed pattern overflow) *)
      raise_notrace Goto_failed);

    buf.(!pp) <- meta_end (* C: *parsed_pattern = META_END, no increment *);
    if Int.equal !nest_depth 0 then 0 else unclosed_parenthesis ()
  with Goto_failed ->
    (* FAILED: pcre2_compile.c:5022-5026 — come here for all failures. *)
    cx.erroroffset <- !ptr;
    cx.errorcode
