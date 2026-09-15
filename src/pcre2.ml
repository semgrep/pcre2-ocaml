[@@@warning "-32"]

(* Registers exceptions with the C runtime and caches polymorphic variants *)
let () = Bindings.pcre2_ocaml_init ()
let ( >+= ) x f = Option.map f x
let ( let* ) = Result.bind

(* Provides common types and functions for representation of matches, capture
   groups and ranges. This allows us to avoid duplicating these definitions
   for various PCRE2 matching flavours, since they all share the same offset
   vector (ovector) representation. *)
module Match = struct
  type match_ = string * int * int (* need only ovec? *) [@@deriving show, eq]
  type range = { start : int; end_ : int } [@@deriving show, eq]

  type captures = string * (int * int) array * (string * int) array
  [@@deriving show, eq]

  let range_of_match (_, start, end_) = { start; end_ }

  let substring_of_match (subject, start, end_) =
    String.sub subject start (end_ - start)

  let range_of_captures (_, matches, _) =
    (* Array should always be at least length 1 *)
    let start, end_ = matches.(0) in
    { start; end_ }

  let captures_length ((_, matches, _) : captures) : int = Array.length matches

  let get_match i matches =
    if 0 <= i && i < Array.length matches then
      let ((start, end_) as match_) = matches.(i) in
      if match_ = Bindings.unset then None else Some (start, end_)
    else None

  let match_of_captures ((subject, matches, _) : captures) (i : int) :
      match_ option =
    get_match i matches >+= fun (start, end_) -> (subject, start, end_)

  let named_match_of_captures ((subject, matches, names) : captures)
      (group_name : string) : match_ option =
    Array.find_map
      (fun (s, i) ->
        if String.equal group_name s then get_match i matches else None)
      names
    >+= fun (start, end_) -> (subject, start, end_)
end

include Match

(* Iteration must not revisit a match position, or an empty (zero-width) match
   would repeat forever: an empty match at the end position of the previous
   match is instead skipped by resuming the search one character further on. *)

(* The next offset at which to resume a search when skipping past an empty
   match. For a UTF regex this must be the next character boundary: PCRE2
   rejects offsets inside a UTF-8 sequence. *)
let next_search_offset ~is_utf (subject : string) (offset : int) : int =
  (* In UTF-8, bytes of the form 0b10xxxxxx occur only in the middle of a
     character; every other byte starts one. *)
  let is_utf8_continuation_byte c = Char.code c land 0xc0 = 0x80 in
  let length = String.length subject in
  let rec next_boundary o =
    if o < length && is_utf8_continuation_byte subject.[o] then
      next_boundary (o + 1)
    else o
  in
  if is_utf then next_boundary (offset + 1) else offset + 1

(* Whether a match spanning [start, end_) is an empty match overlapping the
   end of the previous match (if any). *)
let is_overlapping_empty_match ~(last_match_end : int option) (start : int)
    (end_ : int) : bool =
  Int.equal start end_
  &&
  match last_match_end with
  | Some last -> Int.equal last end_
  | None -> false

module Error = struct
  type compile_error_code =
    | END_BACKSLASH [@value 101]
    | END_BACKSLASH_C [@value 102]
    | UNKNOWN_ESCAPE [@value 103]
    | QUANTIFIER_OUT_OF_ORDER [@value 104]
    | QUANTIFIER_TOO_BIG [@value 105]
    | MISSING_SQUARE_BRACKET [@value 106]
    | ESCAPE_INVALID_IN_CLASS [@value 107]
    | CLASS_RANGE_ORDER [@value 108]
    | QUANTIFIER_INVALID [@value 109]
    | INTERNAL_UNEXPECTED_REPEAT [@value 110]
    | INVALID_AFTER_PARENS_QUERY [@value 111]
    | POSIX_CLASS_NOT_IN_CLASS [@value 112]
    | POSIX_NO_SUPPORT_COLLATING [@value 113]
    | MISSING_CLOSING_PARENTHESIS [@value 114]
    | BAD_SUBPATTERN_REFERENCE [@value 115]
    | NULL_PATTERN [@value 116]
    | BAD_OPTIONS [@value 117]
      (* TODO: can we make this not possible with the API we expose? *)
    | MISSING_COMMENT_CLOSING [@value 118]
    | PARENTHESES_NEST_TOO_DEEP [@value 119]
    | PATTERN_TOO_LARGE [@value 120]
    | HEAP_FAILED [@value 121]
    | UNMATCHED_CLOSING_PARENTHESIS [@value 122]
    | INTERNAL_CODE_OVERFLOW [@value 123]
    | MISSING_CONDITION_CLOSING [@value 124]
    | LOOKBEHIND_NOT_FIXED_LENGTH [@value 125]
    | ZERO_RELATIVE_REFERENCE [@value 126]
    | TOO_MANY_CONDITION_BRANCHES [@value 127]
    | CONDITION_ASSERTION_EXPECTED [@value 128]
    | BAD_RELATIVE_REFERENCE [@value 129]
    | UNKNOWN_POSIX_CLASS [@value 130]
    | INTERNAL_STUDY_ERROR [@value 131]
    | UNICODE_NOT_SUPPORTED [@value 132]
    | PARENTHESES_STACK_CHECK [@value 133]
    | CODE_POINT_TOO_BIG [@value 134]
    | LOOKBEHIND_TOO_COMPLICATED [@value 135]
    | LOOKBEHIND_INVALID_BACKSLASH_C [@value 136]
    | UNSUPPORTED_ESCAPE_SEQUENCE [@value 137]
    | CALLOUT_NUMBER_TOO_BIG [@value 138]
    | MISSING_CALLOUT_CLOSING [@value 139]
    | ESCAPE_INVALID_IN_VERB [@value 140]
    | UNRECOGNIZED_AFTER_QUERY_P [@value 141]
    | MISSING_NAME_TERMINATOR [@value 142]
    | DUPLICATE_SUBPATTERN_NAME [@value 143]
    | INVALID_SUBPATTERN_NAME [@value 144]
    | UNICODE_PROPERTIES_UNAVAILABLE [@value 145]
    | MALFORMED_UNICODE_PROPERTY [@value 146]
    | UNKNOWN_UNICODE_PROPERTY [@value 147]
    | SUBPATTERN_NAME_TOO_LONG [@value 148]
    | TOO_MANY_NAMED_SUBPATTERNS [@value 149]
    | CLASS_INVALID_RANGE [@value 150]
    | OCTAL_BYTE_TOO_BIG [@value 151]
    | INTERNAL_OVERRAN_WORKSPACE [@value 152]
    | INTERNAL_MISSING_SUBPATTERN [@value 153]
    | DEFINE_TOO_MANY_BRANCHES [@value 154]
    | BACKSLASH_O_MISSING_BRACE [@value 155]
    | INTERNAL_UNKNOWN_NEWLINE [@value 156]
    | BACKSLASH_G_SYNTAX [@value 157]
    | PARENS_QUERY_R_MISSING_CLOSING [@value 158]
    | VERB_ARGUMENT_NOT_ALLOWED [@value 159]
        (** NOTE: Obsolete; should not occur - since when? *)
    | VERB_UNKNOWN [@value 160]
    | SUBPATTERN_NUMBER_TOO_BIG [@value 161]
    | SUBPATTERN_NAME_EXPECTED [@value 162]
    | INTERNAL_PARSED_OVERFLOW [@value 163]
    | INVALID_OCTAL [@value 164]
    | SUBPATTERN_NAMES_MISMATCH [@value 165]
    | MARK_MISSING_ARGUMENT [@value 166]
    | INVALID_HEXADECIMAL [@value 167]
    | BACKSLASH_C_SYNTAX [@value 168]
    | BACKSLASH_K_SYNTAX [@value 169]
    | INTERNAL_BAD_CODE_LOOKBEHINDS [@value 170]
    | BACKSLASH_N_IN_CLASS [@value 171]
    | CALLOUT_STRING_TOO_LONG [@value 172]
    | UNICODE_DISALLOWED_CODE_POINT [@value 173]
    | UTF_IS_DISABLED [@value 174]
    | UCP_IS_DISABLED [@value 175]
    | VERB_NAME_TOO_LONG [@value 176]
    | BACKSLASH_U_CODE_POINT_TOO_BIG [@value 177]
    | MISSING_OCTAL_OR_HEX_DIGITS [@value 178]
    | VERSION_CONDITION_SYNTAX [@value 179]
    | INTERNAL_BAD_CODE_AUTO_POSSESS [@value 180]
    | CALLOUT_NO_STRING_DELIMITER [@value 181]
    | CALLOUT_BAD_STRING_DELIMITER [@value 182]
    | BACKSLASH_C_CALLER_DISABLED [@value 183]
    | QUERY_BARJX_NEST_TOO_DEEP [@value 184]
    | BACKSLASH_C_LIBRARY_DISABLED [@value 185]
    | PATTERN_TOO_COMPLICATED [@value 186]
    | LOOKBEHIND_TOO_LONG [@value 187]
    | PATTERN_STRING_TOO_LONG [@value 188]
    | INTERNAL_BAD_CODE [@value 189]
    | INTERNAL_BAD_CODE_IN_SKIP [@value 190]
    | NO_SURROGATES_IN_UTF16 [@value 191]
    | BAD_LITERAL_OPTIONS [@value 192]
    | SUPPORTED_ONLY_IN_UNICODE [@value 193]
    | INVALID_HYPHEN_IN_OPTIONS [@value 194]
    | ALPHA_ASSERTION_UNKNOWN [@value 195]
    | SCRIPT_RUN_NOT_AVAILABLE [@value 196]
    | TOO_MANY_CAPTURES [@value 197]
    | CONDITION_ATOMIC_ASSERTION_EXPECTED [@value 198]
    | BACKSLASH_K_IN_LOOKAROUND [@value 199]
  [@@deriving eq, enum]

  (* [compile_error_code_to_enum]/[compile_error_code_of_enum] are generated
     by [@@deriving enum] from the [@value] annotation above, so the mapping
     between a code and its PCRE2 integer lives in exactly one place: the
     type declaration itself. *)
  let int_of_compile_error_code : compile_error_code -> int =
    compile_error_code_to_enum

  let compile_error_code_of_int (n : int) : compile_error_code =
    match compile_error_code_of_enum n with
    | Some c -> c
    | None ->
        invalid_arg (Printf.sprintf "%d is not a valid PCRE2 compile error" n)

  let pp_compile_error_code (fmt : Format.formatter)
      (code : compile_error_code) : unit =
    Format.pp_print_string fmt
      (Bindings.pcre2_get_error_message (int_of_compile_error_code code))

  let show_compile_error_code (code : compile_error_code) : string =
    Bindings.pcre2_get_error_message (int_of_compile_error_code code)

  (** An error encountered while compiling a pattern, alongside the offset (in
      code units) into the pattern at which it occurred. Not all errors are
      associated with a meaningful offset, in which case it is given as 0. See
      `pcre2_compile(3)` for details. *)
  type compile_error = { code : compile_error_code; offset : int }
  [@@deriving eq]

  let compile_error_of_int_pair ((code, offset) : int * int) : compile_error =
    { code = compile_error_code_of_int code; offset }

  let pp_compile_error (fmt : Format.formatter)
      ({ code; offset } : compile_error) : unit =
    Format.fprintf fmt "%s (at offset %d)"
      (Bindings.pcre2_get_error_message (int_of_compile_error_code code))
      offset

  let show_compile_error (e : compile_error) : string =
    Format.asprintf "%a" pp_compile_error e

  type match_error =
    (* Error codes for UTF-8 validity checks. See pcre2unicode(3). *)
    | UTF8_ERR1 [@value -3]
    | UTF8_ERR2 [@value -4]
    | UTF8_ERR3 [@value -5]
    | UTF8_ERR4 [@value -6]
    | UTF8_ERR5 [@value -7]
    | UTF8_ERR6 [@value -8]
    | UTF8_ERR7 [@value -9]
    | UTF8_ERR8 [@value -10]
    | UTF8_ERR9 [@value -11]
    | UTF8_ERR10 [@value -12]
    | UTF8_ERR11 [@value -13]
    | UTF8_ERR12 [@value -14]
    | UTF8_ERR13 [@value -15]
    | UTF8_ERR14 [@value -16]
    | UTF8_ERR15 [@value -17]
    | UTF8_ERR16 [@value -18]
    | UTF8_ERR17 [@value -19]
    | UTF8_ERR18 [@value -20]
    | UTF8_ERR19 [@value -21]
    | UTF8_ERR20 [@value -22]
    | UTF8_ERR21 [@value -23]
    (* TODO(* (non-8 support) *):
       | UTF16_ERR1
       | UTF16_ERR2
       | UTF16_ERR3
       | UTF32_ERR1
       | UTF32_ERR2
    *)
    (* Miscellaneous error codes for pcre2[_dfa]_match, substring extraction
       functions, context functions, and serializing functions. They are in numerical
       order. Originally they were in alphabetical order too, but now that PCRE2 is
       released, the numbers must not be changed. *)
    | BADDATA [@value -29]
    | MIXEDTABLES [@value -30]
    (* Name was changed *)
    | BADMAGIC [@value -31]
    | BADMODE [@value -32]
    | BADOFFSET [@value -33]
    | BADOPTION [@value -34] (* TODO: shouldn't be possible? *)
    | BADREPLACEMENT [@value -35]
    | BADUTFOFFSET [@value -36]
    | CALLOUT [@value -37]
    | DFA_BADRESTART [@value -38]
    | DFA_RECURSE [@value -39]
    | DFA_UCOND [@value -40]
    | DFA_UFUNC [@value -41]
    | DFA_UITEM [@value -42]
    | DFA_WSSIZE [@value -43]
    | INTERNAL [@value -44]
    | JIT_BADOPTION [@value -45]
    | JIT_STACKLIMIT [@value -46]
    | MATCHLIMIT [@value -47]
    | NOMEMORY [@value -48]
    | NOSUBSTRING [@value -49]
    | NOUNIQUESUBSTRING [@value -50]
    | NULL [@value -51] (* TODO: shouldn't be possible? *)
    | RECURSELOOP [@value -52]
    | DEPTHLIMIT [@value -53]
    | UNAVAILABLE [@value -54]
    | UNSET [@value -55]
    | BADOFFSETLIMIT [@value -56]
    | BADREPESCAPE [@value -57]
    | REPMISSINGBRACE [@value -58]
    | BADSUBSTITUTION [@value -59]
    | BADSUBSPATTERN [@value -60]
    | TOOMANYREPLACE [@value -61]
    | BADSERIALIZEDDATA [@value -62]
    | HEAPLIMIT [@value -63]
    | CONVERT_SYNTAX [@value -64]
    | INTERNAL_DUPMATCH [@value -65]
    | DFA_UINVALID_UTF [@value -66]
    | INVALIDOFFSET [@value -67]
  [@@deriving eq, enum]

  (* [match_error_to_enum]/[match_error_of_enum] are generated by
     [@@deriving enum] from the [@value] annotations above, so the mapping
     between a code and its PCRE2 integer lives in exactly one place: the
     type declaration itself. Codes without a corresponding variant---NOMATCH,
     PARTIAL, and the UTF-16/UTF-32 errors, since only the 8-bit library is
     supported---are handled separately below. *)
  let int_of_match_error : match_error -> int = match_error_to_enum

  let match_error_of_int (n : int) : match_error =
    match n with
    | -1 ->
        invalid_arg "NOMATCH has no corresponding match_error---None is used."
    | -2 ->
        invalid_arg
          "PARTIAL has no corresponding match_error---the partial match is \
           returned directly."
    | -24 | -25 | -26 | -27 | -28 ->
        invalid_arg
          (Printf.sprintf
             "%d is a UTF16 or UTF32 error, but we only support UTF8" n)
    | n -> (
        match match_error_of_enum n with
        | Some e -> e
        | None ->
            invalid_arg
              (Printf.sprintf "%d is not a valid PCRE2 match error" n))

  let pp_match_error (fmt : Format.formatter) (e : match_error) : unit =
    Format.pp_print_string fmt
      (Bindings.pcre2_get_error_message (int_of_match_error e))

  let show_match_error (e : match_error) : string =
    Bindings.pcre2_get_error_message (int_of_match_error e)
end

include Error

module Options = struct
  module Jit = struct
    type matching_mode = JIT_COMPLETE | JIT_PARTIAL_SOFT | JIT_PARTIAL_HARD
    [@@deriving show, eq]

    let int32_of_matching_mode : matching_mode -> int32 = function
      | JIT_COMPLETE     -> 0x00000001l
      | JIT_PARTIAL_SOFT -> 0x00000002l
      | JIT_PARTIAL_HARD -> 0x00000004l
    [@@ocamlformat "disable"]

    type jit_only_compile_option = [ `JIT_INVALID_UTF ]
    (* [@deprecated "MATCH_INVALID_UTF should be used instead"] *)
    (* deprecated (not sure when v10.34ish?) - use MATCH_INVALID_UTF *)
    [@@deriving show, eq]

    let int32_of_compile_option : jit_only_compile_option -> int32 = function
      | (`JIT_INVALID_UTF [@alert "-deprecated"]) -> 0x00000100l

    let bitvector_of_compile_options (opts : jit_only_compile_option list) :
        int32 =
      opts |> List.map int32_of_compile_option |> List.fold_left Int32.logor 0l

    type match_option =
      [ `NOTBOL
      | `NOTEOL
      | `NOTEMPTY
      | `NOTEMPTY_ATSTART
      | `PARTIAL_SOFT
      | `PARTIAL_HARD ]
    [@@deriving show, eq]

    let int32_of_match_option : match_option -> int32 = function
      | `NOTBOL           -> 0x00000001l
      | `NOTEOL           -> 0x00000002l
      | `NOTEMPTY         -> 0x00000004l
      | `NOTEMPTY_ATSTART -> 0x00000008l
      | `PARTIAL_HARD     -> 0x00000010l
      | `PARTIAL_SOFT     -> 0x00000020l
    [@@ocamlformat "disable"]

    let bitvector_of_match_options (opts : match_option list) : int32 =
      opts |> List.map int32_of_match_option |> List.fold_left Int32.logor 0l
  end

  module Interp = struct
    type match_option =
      (* shared *)
      [ Jit.match_option
      | `COPY_MATCHED_SUBJECT
      | `DISABLE_RECURSELOOP_CHECK
      | `NO_JIT ]
    [@@deriving show, eq]

    let int32_of_match_option : match_option -> int32 = function
      | #Jit.match_option as jit_opt -> Jit.int32_of_match_option jit_opt
      | `COPY_MATCHED_SUBJECT -> 0x00004000l
      | `DISABLE_RECURSELOOP_CHECK -> 0x00040000l
      | `NO_JIT -> 0x00002000l

    let bitvector_of_match_options (opts : match_option list) : int32 =
      opts |> List.map int32_of_match_option |> List.fold_left Int32.logor 0l

    type compile_match_options = [ `ANCHORED | `NO_UTF_CHECK | `ENDANCHORED ]
    [@@deriving show, eq]

    let int32_of_compile_match_option : compile_match_options -> int32 = function
      | `ANCHORED     -> 0x80000000l
      | `NO_UTF_CHECK -> 0x40000000l
      | `ENDANCHORED  -> 0x20000000l
    [@@ocamlformat "disable"]

    type compile_option =
      [ compile_match_options
      | `ALLOW_EMPTY_CLASS
      | `ALT_BSUX
      | `AUTO_CALLOUT
      | `CASELESS
      | `DOLLAR_ENDONLY
      | `DOTALL
      | `DUPNAMES
      | `EXTENDED
      | `FIRSTLINE
      | `MATCH_UNSET_BACKREF
      | `MULTILINE
      | `NEVER_UCP
      | `NEVER_UTF
      | `NO_AUTO_CAPTURE
      | `NO_AUTO_POSSESS
      | `NO_DOTSTAR_ANCHOR
      | `NO_START_OPTIMIZE
      | `UCP
      | `UNGREEDY
      | `UTF
      | `NEVER_BACKSLASH_C
      | `ALT_CIRCUMFLEX
      | `ALT_VERBNAMES
      | `USE_OFFSET_LIMIT
      | `EXTENDED_MORE
      | `LITERAL
      | `MATCH_INVALID_UTF ]
    [@@deriving show, eq]

    let int32_of_compile_option : compile_option -> int32 = function
      | #compile_match_options as opt -> int32_of_compile_match_option opt
      | `ALLOW_EMPTY_CLASS            -> 0x00000001l
      | `ALT_BSUX                     -> 0x00000002l
      | `AUTO_CALLOUT                 -> 0x00000004l
      | `CASELESS                     -> 0x00000008l
      | `DOLLAR_ENDONLY               -> 0x00000010l
      | `DOTALL                       -> 0x00000020l
      | `DUPNAMES                     -> 0x00000040l
      | `EXTENDED                     -> 0x00000080l
      | `FIRSTLINE                    -> 0x00000100l
      | `MATCH_UNSET_BACKREF          -> 0x00000200l
      | `MULTILINE                    -> 0x00000400l
      | `NEVER_UCP                    -> 0x00000800l
      | `NEVER_UTF                    -> 0x00001000l
      | `NO_AUTO_CAPTURE              -> 0x00002000l
      | `NO_AUTO_POSSESS              -> 0x00004000l
      | `NO_DOTSTAR_ANCHOR            -> 0x00008000l
      | `NO_START_OPTIMIZE            -> 0x00010000l
      | `UCP                          -> 0x00020000l
      | `UNGREEDY                     -> 0x00040000l
      | `UTF                          -> 0x00080000l
      | `NEVER_BACKSLASH_C            -> 0x00100000l
      | `ALT_CIRCUMFLEX               -> 0x00200000l
      | `ALT_VERBNAMES                -> 0x00400000l
      | `USE_OFFSET_LIMIT             -> 0x00800000l
      | `EXTENDED_MORE                -> 0x01000000l
      | `LITERAL                      -> 0x02000000l
      | `MATCH_INVALID_UTF            -> 0x04000000l
    [@@ocamlformat "disable"]

    let bitvector_of_compile_options (opts : compile_option list) : int32 =
      opts |> List.map int32_of_compile_option |> List.fold_left Int32.logor 0l

    (* for compile ctx - can combine and just split back as needed in bindings? *)
    type compile_ctx =
      [ `EXTRA_ALLOW_SURROGATE_ESCAPES
      | `EXTRA_BAD_ESCAPE_IS_LITERAL
      | `EXTRA_MATCH_WORD
      | `EXTRA_MATCH_LINE
      | `EXTRA_ESCAPED_CR_IS_LF
      | `EXTRA_ALT_BSUX
      | `EXTRA_ALLOW_LOOKAROUND_BSK
      | (* These since 10.43 *)
        (* TODO: verify? what should we do about versioning?? *)
        `EXTRA_CASELESS_RESTRICT
      | `EXTRA_ASCII_BSD
      | `EXTRA_ASCII_BSS
      | `EXTRA_ASCII_BSW
      | `EXTRA_ASCII_POSIX
      | `EXTRA_ASCII_DIGIT ]
      (* TODO: impl these for compile *)
    [@@deriving show, eq]

    let int32_of_compile_ctx_option : compile_ctx -> int32 = function
      | `EXTRA_ALLOW_SURROGATE_ESCAPES -> 0x00000001l
      | `EXTRA_BAD_ESCAPE_IS_LITERAL   -> 0x00000002l
      | `EXTRA_MATCH_WORD              -> 0x00000004l
      | `EXTRA_MATCH_LINE              -> 0x00000008l
      | `EXTRA_ESCAPED_CR_IS_LF        -> 0x00000010l
      | `EXTRA_ALT_BSUX                -> 0x00000020l
      | `EXTRA_ALLOW_LOOKAROUND_BSK    -> 0x00000040l
      (* Assumed values. TODO: verify *)
      | `EXTRA_CASELESS_RESTRICT       -> 0x00000080l
      | `EXTRA_ASCII_BSD               -> 0x00000100l
      | `EXTRA_ASCII_BSS               -> 0x00000200l
      | `EXTRA_ASCII_BSW               -> 0x00000400l
      | `EXTRA_ASCII_POSIX             -> 0x00000800l
      | `EXTRA_ASCII_DIGIT             -> 0x00001000l
    [@@ocamlformat "disable"]

    type subst_options =
      (* shared *)
      [ Jit.match_option
      | compile_match_options
      | `NO_JIT
      | (* exclusive *)
        `SUBSTITUTE_GLOBAL
      | `SUBSTITUTE_EXTENDED
      | `SUBSTITUTE_UNSET_EMPTY
      | `SUBSTITUTE_UNKNOWN_UNSET
      | `SUBSTITUTE_OVERFLOW_LENGTH
      | `SUBSTITUTE_LITERAL
      | `SUBSTITUTE_MATCHED
      | `SUBSTITUTE_REPLACEMENT_ONLY ]
    [@@deriving show, eq]

    type newline_compile_ctx_option =
      | NEWLINE_CR
      | NEWLINE_LF
      | NEWLINE_CRLF
      | NEWLINE_ANY
      | NEWLINE_ANYCRLF
      | NEWLINE_NUL
    [@@deriving show, eq]

    type bsr = BSR_UNICODE | ANYCRLF [@@deriving show, eq]
  end
end

type dfa_match_option =
  (* shared *)
  [ Options.Jit.match_option
  | Options.Interp.compile_match_options
  | `COPY_MATCHED_SUBJECT
  | `DISABLE_RECURSELOOP_CHECK
  | (* exclusive *)
    `DFA_RESTART
  | `DFA_SHORTEST ]
[@@deriving show, eq]

let version : int * int = Bindings.get_version ()
(* FIXME?: depends on the header, instead of what is actually dynamically loaded. *)

let config_unicode : bool = true

(** Default limit for calls to internal matching function *)
let config_match_limit : int = -1

(** Default limit for depth of nested backtracking *)
let config_depth_limit : int = -1

(** Indicates use of stack recursion in matching function *)
let config_stackrecurse : bool = true

module type Matcher = Intf.Matcher

module Interp = struct
  include Options.Interp
  include Match
  include Error

  type t = Bindings.interp Bindings.regex

  let compile ?(options : compile_option list = []) (pattern : string) :
      (t, compile_error) Result.t =
    let options = bitvector_of_compile_options options in
    Bindings.pcre2_compile pattern options
    |> Result.map_error compile_error_of_int_pair

  let capture_groups (r : t) = Bindings.get_capture_groups r |> Array.to_list

  let find ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : t) (subject : string) : (match_ option, match_error) Result.t =
    let options = bitvector_of_match_options options in
    match Bindings.pcre2_match re subject subject_offset options with
    | Ok (Some (start, end_)) -> Ok (Some (subject, start, end_))
    | Ok None -> Ok None
    | Error n -> Error (match_error_of_int n)

  let find_iter ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : t) (subject : string) : (match_, match_error) Result.t Seq.t =
    let options = bitvector_of_match_options options in
    let is_utf = Bindings.regex_is_utf re in
    let subject_length = String.length subject in
    (* Copy the subject out of the OCaml heap once, rather than on every
       iteration (see [Bindings.pin_subject]). *)
    let pinned = Bindings.pin_subject subject in
    let rec next (offset, last_match_end) =
      match Bindings.pcre2_match_pinned re pinned offset options with
      | Ok (Some (start, end_))
        when is_overlapping_empty_match ~last_match_end start end_ ->
          if offset >= subject_length then None
          else next (next_search_offset ~is_utf subject offset, last_match_end)
      | Ok (Some (start, end_)) ->
          Some (Ok (subject, start, end_), (end_, Some end_))
      | Ok None -> None
      | Error n ->
          Some (Error (match_error_of_int n), (subject_length, last_match_end))
    in
    Seq.unfold next (subject_offset, None)

  let captures ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : t) (subject : string) : (captures option, match_error) Result.t =
    let options = bitvector_of_match_options options in
    match Bindings.pcre2_capture re subject subject_offset options with
    | Ok (Some (arr, names)) -> Ok (Some (subject, arr, names))
    | Ok None -> Ok None
    | Error n -> Error (match_error_of_int n)

  let captures_iter ?(options : match_option list = [])
      ?(subject_offset : int = 0) (re : t) (subject : string) :
      (captures, match_error) Result.t Seq.t =
    let options = bitvector_of_match_options options in
    let is_utf = Bindings.regex_is_utf re in
    let subject_length = String.length subject in
    (* Copy the subject out of the OCaml heap once, rather than on every
       iteration (see [Bindings.pin_subject]). *)
    let pinned = Bindings.pin_subject subject in
    let rec next (offset, last_match_end) =
      match Bindings.pcre2_capture_pinned re pinned offset options with
      | Ok (Some (arr, _))
        when (let start, end_ = arr.(0) in
              is_overlapping_empty_match ~last_match_end start end_) ->
          if offset >= subject_length then None
          else next (next_search_offset ~is_utf subject offset, last_match_end)
      | Ok (Some (arr, names)) ->
          let c : captures = (subject, arr, names) in
          let end_ = (range_of_captures c).end_ in
          Some (Ok c, (end_, Some end_))
      | Ok None -> None
      | Error n ->
          Some (Error (match_error_of_int n), (subject_length, last_match_end))
    in
    Seq.unfold next (subject_offset, None)

  let split ?(options : match_option list = []) ?(subject_offset : int = 0)
      ?(limit : int option) (re : t) (subject : string) :
      (string list, match_error) Result.t =
    let delims = find_iter ~options ~subject_offset re subject in
    let delims =
      match limit with
      | Some n when n > 0 -> Seq.take (n - 1) delims
      | None -> delims
      | _ -> invalid_arg "todo: decide how to handle 0 or negative limit"
    in
    let* end_offset, substrings =
      Seq.fold_left
        (fun x m ->
          match (x, m) with
          | Ok (start, acc), Ok m ->
              let { start = delim_start; end_ = delim_end } =
                range_of_match m
              in
              let sub = String.sub subject start (delim_start - start) in
              Ok (delim_end, sub :: acc)
          | Ok _, Error e -> Error e
          | (Error _ as e), _ -> e)
        (Ok (0, []))
        delims
    in
    Ok
      ((* We still have one more substring to add: the one after the last
          delimiter. *)
       String.(sub subject end_offset (length subject - end_offset))
       :: substrings
      (* ... and we built this in reverse to be fast---but let's return it in
         the right order. *)
      |> List.rev)

  let is_match ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : t) (subject : string) : (bool, match_error) Result.t =
    find ~options ~subject_offset re subject |> Result.map Option.is_some
end

(* Fastpath to JIT match for perf *)
module Jit = struct
  include Options.Jit

  type compile_option =
    [ jit_only_compile_option | Options.Interp.compile_option ]
  [@@deriving show, eq]

  include Match
  include Error

  type t = Bindings.jit Bindings.regex

  let of_interp ?(options : jit_only_compile_option list = [])
      ?(mode : matching_mode = JIT_COMPLETE) (interp : Interp.t) :
      (t, compile_error) Result.t =
    let mode = int32_of_matching_mode mode in
    let options = Int32.logor mode (bitvector_of_compile_options options) in
    (* [pcre2_jit_compile] has no notion of an offset into the pattern, unlike
       [pcre2_compile], so 0 is given here (see [compile_error]). *)
    Bindings.pcre2_jit_compile interp options
    |> Result.map_error (fun code ->
           { code = compile_error_code_of_int code; offset = 0 })

  let compile ?(options : compile_option list = []) (pattern : string) :
      (t, compile_error) Result.t =
    let interp_options, jit_options =
      List.partition_map
        (function
          | #jit_only_compile_option as x -> Right x
          | #Options.Interp.compile_option as x -> Left x
          | _ -> .)
        options
    in
    let* interp = Interp.compile ~options:interp_options pattern in
    of_interp ~options:jit_options ~mode:JIT_COMPLETE interp
  (* TODO: determine best way to support matching mode with uniform interface.
     Probably make options more abstract in the shared interface *)

  let capture_groups (r : t) = Bindings.get_capture_groups r |> Array.to_list

  let find ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : t) (subject : string) : (match_ option, match_error) Result.t =
    let options = bitvector_of_match_options options in
    match Bindings.pcre2_jit_match re subject subject_offset options with
    | Ok (Some (start, end_)) -> Ok (Some (subject, start, end_))
    | Ok None -> Ok None
    | Error n -> Error (match_error_of_int n)

  (* TODO(cooper): dedup impl with a functor? - entirely derived from find *)
  let find_iter ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : t) (subject : string) : (match_, match_error) Result.t Seq.t =
    let options = bitvector_of_match_options options in
    let is_utf = Bindings.regex_is_utf re in
    let subject_length = String.length subject in
    (* Copy the subject out of the OCaml heap once, rather than on every
       iteration (see [Bindings.pin_subject]). *)
    let pinned = Bindings.pin_subject subject in
    let rec next (offset, last_match_end) =
      match Bindings.pcre2_jit_match_pinned re pinned offset options with
      | Ok (Some (start, end_))
        when is_overlapping_empty_match ~last_match_end start end_ ->
          if offset >= subject_length then None
          else next (next_search_offset ~is_utf subject offset, last_match_end)
      | Ok (Some (start, end_)) ->
          Some (Ok (subject, start, end_), (end_, Some end_))
      | Ok None -> None
      | Error n ->
          Some (Error (match_error_of_int n), (subject_length, last_match_end))
    in
    Seq.unfold next (subject_offset, None)

  let captures ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : t) (subject : string) : (captures option, match_error) Result.t =
    let options = bitvector_of_match_options options in
    match Bindings.pcre2_jit_capture re subject subject_offset options with
    | Ok (Some (arr, names)) -> Ok (Some (subject, arr, names))
    | Ok None -> Ok None
    | Error n -> Error (match_error_of_int n)

  (* TODO(cooper): dedup impl with a functor? - entirely derived from
     captures *)
  let captures_iter ?(options : match_option list = [])
      ?(subject_offset : int = 0) (re : t) (subject : string) :
      (captures, match_error) Result.t Seq.t =
    let options = bitvector_of_match_options options in
    let is_utf = Bindings.regex_is_utf re in
    let subject_length = String.length subject in
    (* Copy the subject out of the OCaml heap once, rather than on every
       iteration (see [Bindings.pin_subject]). *)
    let pinned = Bindings.pin_subject subject in
    let rec next (offset, last_match_end) =
      match Bindings.pcre2_jit_capture_pinned re pinned offset options with
      | Ok (Some (arr, _))
        when (let start, end_ = arr.(0) in
              is_overlapping_empty_match ~last_match_end start end_) ->
          if offset >= subject_length then None
          else next (next_search_offset ~is_utf subject offset, last_match_end)
      | Ok (Some (arr, names)) ->
          let c : captures = (subject, arr, names) in
          let end_ = (range_of_captures c).end_ in
          Some (Ok c, (end_, Some end_))
      | Ok None -> None
      | Error n ->
          Some (Error (match_error_of_int n), (subject_length, last_match_end))
    in
    Seq.unfold next (subject_offset, None)

  let split ?(options : match_option list = []) ?(subject_offset : int = 0)
      ?(limit : int option) (re : t) (subject : string) :
      (string list, match_error) Result.t =
    let delims = find_iter ~options ~subject_offset re subject in
    let delims =
      match limit with
      | Some n when n > 0 -> Seq.take (n - 1) delims
      | None -> delims
      | _ -> invalid_arg "todo: decide how to handle 0 or negative limit"
    in
    let* end_offset, substrings =
      Seq.fold_left
        (fun x m ->
          match (x, m) with
          | Ok (start, acc), Ok m ->
              let { start = delim_start; end_ = delim_end } =
                range_of_match m
              in
              let sub = String.sub subject start (delim_start - start) in
              Ok (delim_end, sub :: acc)
          | Ok _, Error e -> Error e
          | (Error _ as e), _ -> e)
        (Ok (0, []))
        delims
    in
    Ok
      ((* We still have one more substring to add: the one after the last
          delimiter. *)
       String.(sub subject end_offset (length subject - end_offset))
       :: substrings
      (* ... and we built this in reverse to be fast---but let's return it in
         the right order. *)
      |> List.rev)

  (* TODO(cooper): dedup impl with a functor? *)
  let is_match ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : t) (subject : string) : (bool, match_error) Result.t =
    find ~options ~subject_offset re subject |> Result.map Option.is_some
end
