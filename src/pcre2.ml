[@@@warning "-32"]

(* Registers exceptions with the C runtime and caches polymorphic variants *)
let () = Bindings.pcre2_ocaml_init ()
let ( >+= ) x f = Option.map f x
let ( let* ) = Result.bind

(* Provides common types and functions for representation of matches, capture
   groups and ranges. This allows us to avoid duplicating these definitions
   for various PCRE2 matching flavours, since they all share the same offset
   vector (ovector) representation. *)
(* Moved to src/matcher/ (library pcre2.matcher) in the Matcher-API
   factoring (fast-engine milestone chunk A); aliased so the frozen public
   surface -- src/pcre2.mli, zero diff -- is unchanged. Same for Error,
   Options and MakeConvenience below. *)
module Match = Pcre2_matcher.Match
include Match

module Error = struct
  type compile_error_code =
    | END_BACKSLASH
    | END_BACKSLASH_C
    | UNKNOWN_ESCAPE
    | QUANTIFIER_OUT_OF_ORDER
    | QUANTIFIER_TOO_BIG
    | MISSING_SQUARE_BRACKET
    | ESCAPE_INVALID_IN_CLASS
    | CLASS_RANGE_ORDER
    | QUANTIFIER_INVALID
    | INTERNAL_UNEXPECTED_REPEAT
    | INVALID_AFTER_PARENS_QUERY
    | POSIX_CLASS_NOT_IN_CLASS
    | POSIX_NO_SUPPORT_COLLATING
    | MISSING_CLOSING_PARENTHESIS
    | BAD_SUBPATTERN_REFERENCE
    | NULL_PATTERN
    | BAD_OPTIONS
      (* TODO: can we make this not possible with the API we expose? *)
    | MISSING_COMMENT_CLOSING
    | PARENTHESES_NEST_TOO_DEEP
    | PATTERN_TOO_LARGE
    | HEAP_FAILED
    | UNMATCHED_CLOSING_PARENTHESIS
    | INTERNAL_CODE_OVERFLOW
    | MISSING_CONDITION_CLOSING
    | LOOKBEHIND_NOT_FIXED_LENGTH
    | ZERO_RELATIVE_REFERENCE
    | TOO_MANY_CONDITION_BRANCHES
    | CONDITION_ASSERTION_EXPECTED
    | BAD_RELATIVE_REFERENCE
    | UNKNOWN_POSIX_CLASS
    | INTERNAL_STUDY_ERROR
    | UNICODE_NOT_SUPPORTED
    | PARENTHESES_STACK_CHECK
    | CODE_POINT_TOO_BIG
    | LOOKBEHIND_TOO_COMPLICATED
    | LOOKBEHIND_INVALID_BACKSLASH_C
    | UNSUPPORTED_ESCAPE_SEQUENCE
    | CALLOUT_NUMBER_TOO_BIG
    | MISSING_CALLOUT_CLOSING
    | ESCAPE_INVALID_IN_VERB
    | UNRECOGNIZED_AFTER_QUERY_P
    | MISSING_NAME_TERMINATOR
    | DUPLICATE_SUBPATTERN_NAME
    | INVALID_SUBPATTERN_NAME
    | UNICODE_PROPERTIES_UNAVAILABLE
    | MALFORMED_UNICODE_PROPERTY
    | UNKNOWN_UNICODE_PROPERTY
    | SUBPATTERN_NAME_TOO_LONG
    | TOO_MANY_NAMED_SUBPATTERNS
    | CLASS_INVALID_RANGE
    | OCTAL_BYTE_TOO_BIG
    | INTERNAL_OVERRAN_WORKSPACE
    | INTERNAL_MISSING_SUBPATTERN
    | DEFINE_TOO_MANY_BRANCHES
    | BACKSLASH_O_MISSING_BRACE
    | INTERNAL_UNKNOWN_NEWLINE
    | BACKSLASH_G_SYNTAX
    | PARENS_QUERY_R_MISSING_CLOSING
    | VERB_ARGUMENT_NOT_ALLOWED
        (** NOTE: Obsolete; should not occur - since when? *)
    | VERB_UNKNOWN
    | SUBPATTERN_NUMBER_TOO_BIG
    | SUBPATTERN_NAME_EXPECTED
    | INTERNAL_PARSED_OVERFLOW
    | INVALID_OCTAL
    | SUBPATTERN_NAMES_MISMATCH
    | MARK_MISSING_ARGUMENT
    | INVALID_HEXADECIMAL
    | BACKSLASH_C_SYNTAX
    | BACKSLASH_K_SYNTAX
    | INTERNAL_BAD_CODE_LOOKBEHINDS
    | BACKSLASH_N_IN_CLASS
    | CALLOUT_STRING_TOO_LONG
    | UNICODE_DISALLOWED_CODE_POINT
    | UTF_IS_DISABLED
    | UCP_IS_DISABLED
    | VERB_NAME_TOO_LONG
    | BACKSLASH_U_CODE_POINT_TOO_BIG
    | MISSING_OCTAL_OR_HEX_DIGITS
    | VERSION_CONDITION_SYNTAX
    | INTERNAL_BAD_CODE_AUTO_POSSESS
    | CALLOUT_NO_STRING_DELIMITER
    | CALLOUT_BAD_STRING_DELIMITER
    | BACKSLASH_C_CALLER_DISABLED
    | QUERY_BARJX_NEST_TOO_DEEP
    | BACKSLASH_C_LIBRARY_DISABLED
    | PATTERN_TOO_COMPLICATED
    | LOOKBEHIND_TOO_LONG
    | PATTERN_STRING_TOO_LONG
    | INTERNAL_BAD_CODE
    | INTERNAL_BAD_CODE_IN_SKIP
    | NO_SURROGATES_IN_UTF16
    | BAD_LITERAL_OPTIONS
    | SUPPORTED_ONLY_IN_UNICODE
    | INVALID_HYPHEN_IN_OPTIONS
    | ALPHA_ASSERTION_UNKNOWN
    | SCRIPT_RUN_NOT_AVAILABLE
    | TOO_MANY_CAPTURES
    | CONDITION_ATOMIC_ASSERTION_EXPECTED
    | BACKSLASH_K_IN_LOOKAROUND
  [@@deriving show, eq]

  (* A compilation failure: the PCRE2 error [code], its human-readable
     [message] (pcre2_get_error_message), and the byte [offset] in the pattern
     where compilation failed ([-1] when unknown, e.g. from JIT re-tagging). *)
  type compile_error = {
    code : compile_error_code;
    message : string;
    offset : int;
  }
  [@@deriving show, eq]

  let compile_error_code_of_int : int -> compile_error_code = function
    | 101 -> END_BACKSLASH
    | 102 -> END_BACKSLASH_C
    | 103 -> UNKNOWN_ESCAPE
    | 104 -> QUANTIFIER_OUT_OF_ORDER
    | 105 -> QUANTIFIER_TOO_BIG
    | 106 -> MISSING_SQUARE_BRACKET
    | 107 -> ESCAPE_INVALID_IN_CLASS
    | 108 -> CLASS_RANGE_ORDER
    | 109 -> QUANTIFIER_INVALID
    | 110 -> INTERNAL_UNEXPECTED_REPEAT
    | 111 -> INVALID_AFTER_PARENS_QUERY
    | 112 -> POSIX_CLASS_NOT_IN_CLASS
    | 113 -> POSIX_NO_SUPPORT_COLLATING
    | 114 -> MISSING_CLOSING_PARENTHESIS
    | 115 -> BAD_SUBPATTERN_REFERENCE
    | 116 -> NULL_PATTERN
    | 117 -> BAD_OPTIONS
    | 118 -> MISSING_COMMENT_CLOSING
    | 119 -> PARENTHESES_NEST_TOO_DEEP
    | 120 -> PATTERN_TOO_LARGE
    | 121 -> HEAP_FAILED
    | 122 -> UNMATCHED_CLOSING_PARENTHESIS
    | 123 -> INTERNAL_CODE_OVERFLOW
    | 124 -> MISSING_CONDITION_CLOSING
    | 125 -> LOOKBEHIND_NOT_FIXED_LENGTH
    | 126 -> ZERO_RELATIVE_REFERENCE
    | 127 -> TOO_MANY_CONDITION_BRANCHES
    | 128 -> CONDITION_ASSERTION_EXPECTED
    | 129 -> BAD_RELATIVE_REFERENCE
    | 130 -> UNKNOWN_POSIX_CLASS
    | 131 -> INTERNAL_STUDY_ERROR
    | 132 -> UNICODE_NOT_SUPPORTED
    | 133 -> PARENTHESES_STACK_CHECK
    | 134 -> CODE_POINT_TOO_BIG
    | 135 -> LOOKBEHIND_TOO_COMPLICATED
    | 136 -> LOOKBEHIND_INVALID_BACKSLASH_C
    | 137 -> UNSUPPORTED_ESCAPE_SEQUENCE
    | 138 -> CALLOUT_NUMBER_TOO_BIG
    | 139 -> MISSING_CALLOUT_CLOSING
    | 140 -> ESCAPE_INVALID_IN_VERB
    | 141 -> UNRECOGNIZED_AFTER_QUERY_P
    | 142 -> MISSING_NAME_TERMINATOR
    | 143 -> DUPLICATE_SUBPATTERN_NAME
    | 144 -> INVALID_SUBPATTERN_NAME
    | 145 -> UNICODE_PROPERTIES_UNAVAILABLE
    | 146 -> MALFORMED_UNICODE_PROPERTY
    | 147 -> UNKNOWN_UNICODE_PROPERTY
    | 148 -> SUBPATTERN_NAME_TOO_LONG
    | 149 -> TOO_MANY_NAMED_SUBPATTERNS
    | 150 -> CLASS_INVALID_RANGE
    | 151 -> OCTAL_BYTE_TOO_BIG
    | 152 -> INTERNAL_OVERRAN_WORKSPACE
    | 153 -> INTERNAL_MISSING_SUBPATTERN
    | 154 -> DEFINE_TOO_MANY_BRANCHES
    | 155 -> BACKSLASH_O_MISSING_BRACE
    | 156 -> INTERNAL_UNKNOWN_NEWLINE
    | 157 -> BACKSLASH_G_SYNTAX
    | 158 -> PARENS_QUERY_R_MISSING_CLOSING
    | 159 -> VERB_ARGUMENT_NOT_ALLOWED
    | 160 -> VERB_UNKNOWN
    | 161 -> SUBPATTERN_NUMBER_TOO_BIG
    | 162 -> SUBPATTERN_NAME_EXPECTED
    | 163 -> INTERNAL_PARSED_OVERFLOW
    | 164 -> INVALID_OCTAL
    | 165 -> SUBPATTERN_NAMES_MISMATCH
    | 166 -> MARK_MISSING_ARGUMENT
    | 167 -> INVALID_HEXADECIMAL
    | 168 -> BACKSLASH_C_SYNTAX
    | 169 -> BACKSLASH_K_SYNTAX
    | 170 -> INTERNAL_BAD_CODE_LOOKBEHINDS
    | 171 -> BACKSLASH_N_IN_CLASS
    | 172 -> CALLOUT_STRING_TOO_LONG
    | 173 -> UNICODE_DISALLOWED_CODE_POINT
    | 174 -> UTF_IS_DISABLED
    | 175 -> UCP_IS_DISABLED
    | 176 -> VERB_NAME_TOO_LONG
    | 177 -> BACKSLASH_U_CODE_POINT_TOO_BIG
    | 178 -> MISSING_OCTAL_OR_HEX_DIGITS
    | 179 -> VERSION_CONDITION_SYNTAX
    | 180 -> INTERNAL_BAD_CODE_AUTO_POSSESS
    | 181 -> CALLOUT_NO_STRING_DELIMITER
    | 182 -> CALLOUT_BAD_STRING_DELIMITER
    | 183 -> BACKSLASH_C_CALLER_DISABLED
    | 184 -> QUERY_BARJX_NEST_TOO_DEEP
    | 185 -> BACKSLASH_C_LIBRARY_DISABLED
    | 186 -> PATTERN_TOO_COMPLICATED
    | 187 -> LOOKBEHIND_TOO_LONG
    | 188 -> PATTERN_STRING_TOO_LONG
    | 189 -> INTERNAL_BAD_CODE
    | 190 -> INTERNAL_BAD_CODE_IN_SKIP
    | 191 -> NO_SURROGATES_IN_UTF16
    | 192 -> BAD_LITERAL_OPTIONS
    | 193 -> SUPPORTED_ONLY_IN_UNICODE
    | 194 -> INVALID_HYPHEN_IN_OPTIONS
    | 195 -> ALPHA_ASSERTION_UNKNOWN
    | 196 -> SCRIPT_RUN_NOT_AVAILABLE
    | 197 -> TOO_MANY_CAPTURES
    | 198 -> CONDITION_ATOMIC_ASSERTION_EXPECTED
    | 199 -> BACKSLASH_K_IN_LOOKAROUND
    | n ->
        invalid_arg (Printf.sprintf "%d is not a valid PCRE2 compile error" n)

  (* Build the compile error carrying the PCRE2 [errcode], its message text,
     and the failing pattern [offset]. *)
  let compile_error_of_ints ((errcode, offset) : int * int) : compile_error =
    {
      code = compile_error_code_of_int errcode;
      message = Pcre2_engine.Engine.error_message errcode;
      offset;
    }

  type match_error =
    (* Error codes for UTF-8 validity checks. See pcre2unicode(3). *)
    | UTF8_ERR1
    | UTF8_ERR2
    | UTF8_ERR3
    | UTF8_ERR4
    | UTF8_ERR5
    | UTF8_ERR6
    | UTF8_ERR7
    | UTF8_ERR8
    | UTF8_ERR9
    | UTF8_ERR10
    | UTF8_ERR11
    | UTF8_ERR12
    | UTF8_ERR13
    | UTF8_ERR14
    | UTF8_ERR15
    | UTF8_ERR16
    | UTF8_ERR17
    | UTF8_ERR18
    | UTF8_ERR19
    | UTF8_ERR20
    | UTF8_ERR21
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
    | BADDATA
    | MIXEDTABLES
    (* Name was changed *)
    | BADMAGIC
    | BADMODE
    | BADOFFSET
    | BADOPTION (* TODO: shouldn't be possible? *)
    | BADREPLACEMENT
    | BADUTFOFFSET
    | CALLOUT
    | DFA_BADRESTART
    | DFA_RECURSE
    | DFA_UCOND
    | DFA_UFUNC
    | DFA_UITEM
    | DFA_WSSIZE
    | INTERNAL
    | JIT_BADOPTION
    | JIT_STACKLIMIT
    | MATCHLIMIT
    | NOMEMORY
    | NOSUBSTRING
    | NOUNIQUESUBSTRING
    | NULL (* TODO: shouldn't be possible? *)
    | RECURSELOOP
    | DEPTHLIMIT
    | UNAVAILABLE
    | UNSET
    | BADOFFSETLIMIT
    | BADREPESCAPE
    | REPMISSINGBRACE
    | BADSUBSTITUTION
    | BADSUBSPATTERN
    | TOOMANYREPLACE
    | BADSERIALIZEDDATA
    | HEAPLIMIT
    | CONVERT_SYNTAX
    | INTERNAL_DUPMATCH
    | DFA_UINVALID_UTF
    | INVALIDOFFSET
  [@@deriving show, eq]

  let match_error_of_int : int -> match_error = function
    | -1 ->
        invalid_arg "NOMATCH has no corresponding match_error---None is used."
    | -2 ->
        invalid_arg
          "PARTIAL has no corresponding match_error---the partial match is \
           returned directly."
    | -3 -> UTF8_ERR1
    | -4 -> UTF8_ERR2
    | -5 -> UTF8_ERR3
    | -6 -> UTF8_ERR4
    | -7 -> UTF8_ERR5
    | -8 -> UTF8_ERR6
    | -9 -> UTF8_ERR7
    | -10 -> UTF8_ERR8
    | -11 -> UTF8_ERR9
    | -12 -> UTF8_ERR10
    | -13 -> UTF8_ERR11
    | -14 -> UTF8_ERR12
    | -15 -> UTF8_ERR13
    | -16 -> UTF8_ERR14
    | -17 -> UTF8_ERR15
    | -18 -> UTF8_ERR16
    | -19 -> UTF8_ERR17
    | -20 -> UTF8_ERR18
    | -21 -> UTF8_ERR19
    | -22 -> UTF8_ERR20
    | -23 -> UTF8_ERR21
    | (-24 | -25 | -26 | -27 | -28) as n ->
        invalid_arg
          (Printf.sprintf
             "%d is a UTF16 or UTF32 error, but we only support UTF8" n)
    | -29 -> BADDATA
    | -30 -> MIXEDTABLES
    | -31 -> BADMAGIC
    | -32 -> BADMODE
    | -33 -> BADOFFSET
    | -34 -> BADOPTION
    | -35 -> BADREPLACEMENT
    | -36 -> BADUTFOFFSET
    | -37 -> CALLOUT
    | -38 -> DFA_BADRESTART
    | -39 -> DFA_RECURSE
    | -40 -> DFA_UCOND
    | -41 -> DFA_UFUNC
    | -42 -> DFA_UITEM
    | -43 -> DFA_WSSIZE
    | -44 -> INTERNAL
    | -45 -> JIT_BADOPTION
    | -46 -> JIT_STACKLIMIT
    | -47 -> MATCHLIMIT
    | -48 -> NOMEMORY
    | -49 -> NOSUBSTRING
    | -50 -> NOUNIQUESUBSTRING
    | -51 -> NULL
    | -52 -> RECURSELOOP
    | -53 -> DEPTHLIMIT
    | -54 -> UNAVAILABLE
    | -55 -> UNSET
    | -56 -> BADOFFSETLIMIT
    | -57 -> BADREPESCAPE
    | -58 -> REPMISSINGBRACE
    | -59 -> BADSUBSTITUTION
    | -60 -> BADSUBSPATTERN
    | -61 -> TOOMANYREPLACE
    | -62 -> BADSERIALIZEDDATA
    | -63 -> HEAPLIMIT
    | -64 -> CONVERT_SYNTAX
    | -65 -> INTERNAL_DUPMATCH
    | -66 -> DFA_UINVALID_UTF
    | -67 -> INVALIDOFFSET
    | n -> invalid_arg (Printf.sprintf "%d is not a valid PCRE2 match error" n)
end

include Error
module Options = Pcre2_matcher.Options

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

(* Convenience layer re-added from tag 7.5.3, adapted to the Result API. The
   shared types and the $-template machinery live in Intf (see the comment
   there); re-export them at the top level. *)
type split_result = Intf.split_result =
  | Text of string
  | Delim of string
  | Group of int * string
  | NoGroup
[@@deriving show, eq]

type substitution = Intf.substitution

let quote = Intf.quote
let subst = Intf.subst

(* One shared implementation of the 7.5.3 convenience functions, included by
   both Interp and Jit. Built directly on the raw capture function (NOT on
   find_iter/captures_iter, whose empty-match semantics are frozen and wrong
   for these loops) so PCRE2 option bits outside the public match_option
   lists can be OR'd in for the empty-match retries. *)
module MakeConvenience = Pcre2_matcher.Convenience.MakeConvenience

module Interp = struct
  include Options.Interp
  include Match
  include Error

  type t = Bindings.interp Bindings.regex

  let compile ?(options : compile_option list = []) (pattern : string) :
      (t, compile_error) Result.t =
    let options = bitvector_of_compile_options options in
    Bindings.pcre2_compile pattern options
    |> Result.map_error compile_error_of_ints

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
    Seq.unfold
      (fun offset ->
        match find ~options ~subject_offset:offset re subject with
        | Ok (Some (m : match_)) -> Some (Ok m, (range_of_match m).end_)
        | Ok None -> None
        | Error e -> Some (Error e, String.length subject))
      subject_offset

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
    Seq.unfold
      (fun offset ->
        match captures ~options ~subject_offset:offset re subject with
        | Ok (Some (c : captures)) -> Some (Ok c, (range_of_captures c).end_)
        | Ok None -> None
        | Error e -> Some (Error e, String.length subject))
      subject_offset

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
          | e, _ -> e)
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

  include MakeConvenience (struct
    type nonrec t = t
    type nonrec match_option = match_option

    let bitvector_of_match_options = bitvector_of_match_options
    let capture_raw = Bindings.pcre2_capture
  end)
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
    (* JIT re-tagging never fails (it aliases the interpreter), so this maps a
       bare error code with no pattern offset. *)
    Bindings.pcre2_jit_compile interp options
    |> Result.map_error (fun errcode -> compile_error_of_ints (errcode, -1))

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
    Seq.unfold
      (fun offset ->
        match find ~options ~subject_offset:offset re subject with
        | Ok (Some (m : match_)) -> Some (Ok m, (range_of_match m).end_)
        | Ok None -> None
        | Error e -> Some (Error e, String.length subject))
      subject_offset

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
    Seq.unfold
      (fun offset ->
        match captures ~options ~subject_offset:offset re subject with
        | Ok (Some (c : captures)) -> Some (Ok c, (range_of_captures c).end_)
        | Ok None -> None
        | Error e -> Some (Error e, String.length subject))
      subject_offset

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
          | e, _ -> e)
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

  include MakeConvenience (struct
    type nonrec t = t
    type nonrec match_option = match_option

    let bitvector_of_match_options = bitvector_of_match_options
    let capture_raw = Bindings.pcre2_jit_capture
  end)
end
