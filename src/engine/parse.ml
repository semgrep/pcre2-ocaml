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

(* ---------- POSIX class names ---------- *)

(* pcre2_compile.c:693-707 — tables of names of POSIX character classes and
   their lengths. The C keeps the names in a single \0-separated string;
   an array of strings is equivalent here. The list of lengths is terminated
   by a zero length entry. The first three must be alpha, lower, upper, as
   this is assumed for handling case independence. *)
let posix_names =
  [|
    "alpha"; "lower"; "upper"; "alnum"; "ascii"; "blank"; "cntrl";
    "digit"; "graph"; "print"; "punct"; "space"; "word"; "xdigit";
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
}

let make_context (pattern : string) : parse_context =
  {
    pattern;
    ptrend = String.length pattern;
    ptr = 0;
    errorcode = 0;
    erroroffset = 0;
  }

(* Local control-flow exception modeling the C's forward "goto EXIT" /
   "goto FAILED" jumps to a shared function epilogue (port-conventions §2).
   Raised and caught within a single helper below; never escapes this
   module. Compile-time code only (§6 permits exceptions outside the
   interpreter frame loop). *)
exception Goto_exit

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
  if allow_sign >= 0 && !ptr < cx.ptrend then begin
    if Char.equal cx.pattern.[!ptr] '+' then begin
      sign := 1;
      (* uint32 subtraction (pcre2_compile.c:1340). It cannot wrap for the
         reachable call sites (allow_sign = cb->bracount <= MAX_GROUP_NUMBER
         = max_value), but mask for C parity. *)
      max_value := (!max_value - allow_sign) land 0xffff_ffff;
      incr ptr
    end
    else if Char.equal cx.pattern.[!ptr] '-' then begin
      sign := -1;
      incr ptr
    end
  end;

  (* pcre2_compile.c:1350 — early return: no out-parameter writes. *)
  if !ptr >= cx.ptrend || not (is_digit cx.pattern.[!ptr]) then false
  else begin
    (try
       (* pcre2_compile.c:1351-1359 *)
       while !ptr < cx.ptrend && is_digit cx.pattern.[!ptr] do
         (* n is uint32 in C; mask keeps wraparound parity. *)
         n :=
           (!n * 10 + Char.code cx.pattern.[!ptr] - Char.code '0')
           land 0xffff_ffff;
         incr ptr;
         if !n > !max_value then begin
           cx.errorcode <- max_error;
           raise_notrace Goto_exit
         end
       done;

       (* pcre2_compile.c:1361-1376 *)
       if allow_sign >= 0 && not (Int.equal !sign 0) then begin
         if Int.equal !n 0 then begin
           cx.errorcode <- Errors.err26; (* +0 and -0 are not allowed *)
           raise_notrace Goto_exit
         end;
         if !sign > 0 then n := !n + allow_sign
         else if !n > allow_sign then begin
           (* C compares (int)n > allow_sign; the cast is a no-op here
              because n <= max_value <= MAX_GROUP_NUMBER on this path. *)
           cx.errorcode <- Errors.err15; (* Non-existent subpattern *)
           raise_notrace Goto_exit
         end
         else n := allow_sign + 1 - !n
       end;

       yield := true
     with Goto_exit -> ());

    (* EXIT: pcre2_compile.c:1380-1383 *)
    intptr := !n;
    ptrptr := !ptr;
    !yield
  end

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
    if !pp < cx.ptrend && is_digit cx.pattern.[!pp] then begin
      had_minimum := true;
      incr pp;
      while !pp < cx.ptrend && is_digit cx.pattern.[!pp] do
        incr pp
      done
    end;
    (* pcre2_compile.c:1436-1437 *)
    while !pp < cx.ptrend && space_or_tab cx.pattern.[!pp] do
      incr pp
    done;
    if !pp >= cx.ptrend then false
    else if Char.equal cx.pattern.[!pp] '}' then
      (* pcre2_compile.c:1439-1442 *)
      !had_minimum
    else if not (Char.equal cx.pattern.[!pp] ',') then false
    else begin
      (* pcre2_compile.c:1445-1454 *)
      incr pp; (* the C's *pp++ != CHAR_COMMA consumed the comma *)
      while !pp < cx.ptrend && space_or_tab cx.pattern.[!pp] do
        incr pp
      done;
      if !pp >= cx.ptrend then false
      else begin
        let digits_ok =
          if is_digit cx.pattern.[!pp] then begin
            incr pp;
            while !pp < cx.ptrend && is_digit cx.pattern.[!pp] do
              incr pp
            done;
            true
          end
          else !had_minimum
        in
        if not digits_ok then false
        else begin
          while !pp < cx.ptrend && space_or_tab cx.pattern.[!pp] do
            incr pp
          done;
          !pp < cx.ptrend && Char.equal cx.pattern.[!pp] '}'
        end
      end
    end
  in
  if not syntax_ok then false
  else begin
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
           (read_number cx p ~allow_sign:(-1)
              ~max_value:Limits.max_repeat_count ~max_error:Errors.err5 min)
       then begin
         if not (Int.equal cx.errorcode 0) then raise_notrace Goto_exit; (* n too big *)
         incr p; (* Skip comma and subsequent spaces *)
         while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
           incr p
         done;
         if
           not
             (read_number cx p ~allow_sign:(-1)
                ~max_value:Limits.max_repeat_count ~max_error:Errors.err5 max)
         then begin
           if not (Int.equal cx.errorcode 0) then raise_notrace Goto_exit (* m too big *)
         end
       end
       else begin
         (* Have read one number. Deal with {n} or {n,} or {n,m}
            (pcre2_compile.c:1477-1499). *)
         while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
           incr p
         done;
         (* safe without a ptrend test (as in C, pcre2_compile.c:1480): the
            syntax pre-check guaranteed a terminating '}' before ptrend. *)
         if Char.equal cx.pattern.[!p] '}' then max := !min
         else begin
           (* Handle {n,} or {n,m} *)
           incr p; (* Skip comma and subsequent spaces *)
           while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
             incr p
           done;
           if
             not
               (read_number cx p ~allow_sign:(-1)
                  ~max_value:Limits.max_repeat_count ~max_error:Errors.err5
                  max)
           then begin
             if not (Int.equal cx.errorcode 0) then raise_notrace Goto_exit (* m too big *)
           end;
           if !max < !min then begin
             cx.errorcode <- Errors.err4;
             raise_notrace Goto_exit
           end
         end
       end;

       (* Valid quantifier exists (pcre2_compile.c:1501-1507). *)
       while !p < cx.ptrend && space_or_tab cx.pattern.[!p] do
         incr p
       done;
       incr p;
       yield := true;
       (match minp with Some r -> r := !min | None -> ());
       (match maxp with Some r -> r := !max | None -> ())
     with Goto_exit -> ());

    (* EXIT: pcre2_compile.c:1511-1513 — update the pattern pointer. *)
    ptrptr := !p;
    !yield
  end

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
      (Char.equal cx.pattern.[ptr] '['
      && Char.equal cx.pattern.[ptr + 1] terminator)
      || Char.equal cx.pattern.[ptr] ']'
    then false
    else if
      Char.equal cx.pattern.[ptr] terminator
      && Char.equal cx.pattern.[ptr + 1] ']'
    then begin
      endptr := ptr;
      true
    end
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
    else if Char.equal pattern.[ptr + i] name.[i] then
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
    if !ptr >= cx.ptrend then begin
      cx.errorcode <-
        (if is_group then Errors.err62 (* Subpattern name expected *)
         else Errors.err60 (* Verb not recognized or malformed *));
      raise_notrace Goto_exit (* goto FAILED *)
    end;

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
    if is_group && is_digit cx.pattern.[!ptr] then begin
      cx.errorcode <- Errors.err44;
      raise_notrace Goto_exit (* goto FAILED *)
    end;
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
    if !ptr > !nameptr + Limits.max_name_size then begin
      cx.errorcode <- Errors.err48;
      raise_notrace Goto_exit (* goto FAILED *)
    end;
    namelenptr := !ptr - !nameptr;

    (* pcre2_compile.c:2544-2562 — subpattern names must not be empty, and
       their terminator is checked here. (What follows a verb or alpha
       assertion name is checked separately.) *)
    if is_group then begin
      if Int.equal !ptr !nameptr then begin
        cx.errorcode <- Errors.err62; (* Subpattern name expected *)
        raise_notrace Goto_exit (* goto FAILED *)
      end;
      if is_braced then
        while !ptr < cx.ptrend && space_or_tab cx.pattern.[!ptr] do
          incr ptr
        done;
      if
        !ptr >= cx.ptrend
        || not (Int.equal (Char.code cx.pattern.[!ptr]) terminator)
      then begin
        cx.errorcode <- Errors.err42;
        raise_notrace Goto_exit (* goto FAILED *)
      end;
      incr ptr
    end;

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
    read_name cx ptr ~utf:false
      ~terminator:(Char.code '>')
      offset name namelen
  in
  assert ok;
  assert (Int.equal !name 1);
  assert (Int.equal !offset 1);
  assert (Int.equal !namelen 3);
  assert (Int.equal !ptr 5);
  let cx = make_context "<9a>" in
  let ptr = ref 0 in
  let ok =
    read_name cx ptr ~utf:false
      ~terminator:(Char.code '>')
      offset name namelen
  in
  assert (not ok);
  assert (Int.equal cx.errorcode Errors.err44);
  assert (Int.equal Errors.err44 144)
