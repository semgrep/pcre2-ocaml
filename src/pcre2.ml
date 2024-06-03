[@@@warning "-32"]

let ( >+= ) x f = Option.map f x

type interp = Bindings.interp
type jit = Bindings.jit
type 'a regex = 'a Bindings.regex
type match_ = string * int * int (* need only ovec? *) [@@deriving show]
type range = { start : int; end_ : int } [@@deriving show]

type captures = string * (int * int) array * (string * int) array
[@@deriving show]

(* needs match_data *)
type substitution

let range_of_match (_, start, end_) = { start; end_ }

let substring_of_match (subject, start, end_) =
  String.sub subject start (end_ - start)

let range_of_captures (_, matches, _) =
  (* Array should always be at least length 1 *)
  let start, end_ = matches.(0) in
  { start; end_ }

let match_of_captures ((subject, matches, _) : captures) (i : int) :
    match_ option =
  if 0 <= i && i < Array.length matches then
    let start, end_ = matches.(i) in
    Some (subject, start, end_)
  else None

let named_match_of_captures ((subject, matches, names) : captures)
    (group_name : string) : match_ option =
  Array.find_map
    (fun (s, i) -> if String.equal group_name s then Some i else None)
    names
  >+= fun i ->
  let start, end_ = matches.(i) in
  (subject, start, end_)

(* Registers exceptions with the C runtime and caches polymorphic variants *)
let () = Bindings.pcre2_ocaml_init ()

(* Public exceptions and their registration with the C runtime *)

type compile_error =
  | END_BACKSLASH (* 101 *)
  | END_BACKSLASH_C (* 102 *)
  | UNKNOWN_ESCAPE (* 103 *)
  | QUANTIFIER_OUT_OF_ORDER (* 104 *)
  | QUANTIFIER_TOO_BIG (* 105 *)
  | MISSING_SQUARE_BRACKET (* 106 *)
  | ESCAPE_INVALID_IN_CLASS (* 107 *)
  | CLASS_RANGE_ORDER (* 108 *)
  | QUANTIFIER_INVALID (* 109 *)
  | INTERNAL_UNEXPECTED_REPEAT (* 110 *)
  | INVALID_AFTER_PARENS_QUERY (* 111 *)
  | POSIX_CLASS_NOT_IN_CLASS (* 112 *)
  | POSIX_NO_SUPPORT_COLLATING (* 113 *)
  | MISSING_CLOSING_PARENTHESIS (* 114 *)
  | BAD_SUBPATTERN_REFERENCE (* 115 *)
  | NULL_PATTERN (* 116 *)
  | BAD_OPTIONS (* 117 *)
  | MISSING_COMMENT_CLOSING (* 118 *)
  | PARENTHESES_NEST_TOO_DEEP (* 119 *)
  | PATTERN_TOO_LARGE (* 120 *)
  | HEAP_FAILED (* 121 *)
  | UNMATCHED_CLOSING_PARENTHESIS (* 122 *)
  | INTERNAL_CODE_OVERFLOW (* 123 *)
  | MISSING_CONDITION_CLOSING (* 124 *)
  | LOOKBEHIND_NOT_FIXED_LENGTH (* 125 *)
  | ZERO_RELATIVE_REFERENCE (* 126 *)
  | TOO_MANY_CONDITION_BRANCHES (* 127 *)
  | CONDITION_ASSERTION_EXPECTED (* 128 *)
  | BAD_RELATIVE_REFERENCE (* 129 *)
  | UNKNOWN_POSIX_CLASS (* 130 *)
  | INTERNAL_STUDY_ERROR (* 131 *)
  | UNICODE_NOT_SUPPORTED (* 132 *)
  | PARENTHESES_STACK_CHECK (* 133 *)
  | CODE_POINT_TOO_BIG (* 134 *)
  | LOOKBEHIND_TOO_COMPLICATED (* 135 *)
  | LOOKBEHIND_INVALID_BACKSLASH_C (* 136 *)
  | UNSUPPORTED_ESCAPE_SEQUENCE (* 137 *)
  | CALLOUT_NUMBER_TOO_BIG (* 138 *)
  | MISSING_CALLOUT_CLOSING (* 139 *)
  | ESCAPE_INVALID_IN_VERB (* 140 *)
  | UNRECOGNIZED_AFTER_QUERY_P (* 141 *)
  | MISSING_NAME_TERMINATOR (* 142 *)
  | DUPLICATE_SUBPATTERN_NAME (* 143 *)
  | INVALID_SUBPATTERN_NAME (* 144 *)
  | UNICODE_PROPERTIES_UNAVAILABLE (* 145 *)
  | MALFORMED_UNICODE_PROPERTY (* 146 *)
  | UNKNOWN_UNICODE_PROPERTY (* 147 *)
  | SUBPATTERN_NAME_TOO_LONG (* 148 *)
  | TOO_MANY_NAMED_SUBPATTERNS (* 149 *)
  | CLASS_INVALID_RANGE (* 150 *)
  | OCTAL_BYTE_TOO_BIG (* 151 *)
  | INTERNAL_OVERRAN_WORKSPACE (* 152 *)
  | INTERNAL_MISSING_SUBPATTERN (* 153 *)
  | DEFINE_TOO_MANY_BRANCHES (* 154 *)
  | BACKSLASH_O_MISSING_BRACE (* 155 *)
  | INTERNAL_UNKNOWN_NEWLINE (* 156 *)
  | BACKSLASH_G_SYNTAX (* 157 *)
  | PARENS_QUERY_R_MISSING_CLOSING (* 158 *)
  | VERB_ARGUMENT_NOT_ALLOWED
    (* 159 - OBSOLETE; Should not occur - since when? *)
  | VERB_UNKNOWN (* 160 *)
  | SUBPATTERN_NUMBER_TOO_BIG (* 161 *)
  | SUBPATTERN_NAME_EXPECTED (* 162 *)
  | INTERNAL_PARSED_OVERFLOW (* 163 *)
  | INVALID_OCTAL (* 164 *)
  | SUBPATTERN_NAMES_MISMATCH (* 165 *)
  | MARK_MISSING_ARGUMENT (* 166 *)
  | INVALID_HEXADECIMAL (* 167 *)
  | BACKSLASH_C_SYNTAX (* 168 *)
  | BACKSLASH_K_SYNTAX (* 169 *)
  | INTERNAL_BAD_CODE_LOOKBEHINDS (* 170 *)
  | BACKSLASH_N_IN_CLASS (* 171 *)
  | CALLOUT_STRING_TOO_LONG (* 172 *)
  | UNICODE_DISALLOWED_CODE_POINT (* 173 *)
  | UTF_IS_DISABLED (* 174 *)
  | UCP_IS_DISABLED (* 175 *)
  | VERB_NAME_TOO_LONG (* 176 *)
  | BACKSLASH_U_CODE_POINT_TOO_BIG (* 177 *)
  | MISSING_OCTAL_OR_HEX_DIGITS (* 178 *)
  | VERSION_CONDITION_SYNTAX (* 179 *)
  | INTERNAL_BAD_CODE_AUTO_POSSESS (* 180 *)
  | CALLOUT_NO_STRING_DELIMITER (* 181 *)
  | CALLOUT_BAD_STRING_DELIMITER (* 182 *)
  | BACKSLASH_C_CALLER_DISABLED (* 183 *)
  | QUERY_BARJX_NEST_TOO_DEEP (* 184 *)
  | BACKSLASH_C_LIBRARY_DISABLED (* 185 *)
  | PATTERN_TOO_COMPLICATED (* 186 *)
  | LOOKBEHIND_TOO_LONG (* 187 *)
  | PATTERN_STRING_TOO_LONG (* 188 *)
  | INTERNAL_BAD_CODE (* 189 *)
  | INTERNAL_BAD_CODE_IN_SKIP (* 190 *)
  | NO_SURROGATES_IN_UTF16 (* 191 *)
  | BAD_LITERAL_OPTIONS (* 192 *)
  | SUPPORTED_ONLY_IN_UNICODE (* 193 *)
  | INVALID_HYPHEN_IN_OPTIONS (* 194 *)
  | ALPHA_ASSERTION_UNKNOWN (* 195 *)
  | SCRIPT_RUN_NOT_AVAILABLE (* 196 *)
  | TOO_MANY_CAPTURES (* 197 *)
  | CONDITION_ATOMIC_ASSERTION_EXPECTED (* 198 *)
  | BACKSLASH_K_IN_LOOKAROUND (* 199 *)
[@@deriving show]

let compile_error_of_int : int -> compile_error = function
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
  | n -> invalid_arg (Printf.sprintf "%d is not a valid PCRE2 compile error" n)

type match_error =
  (* "Expected" matching error codes: no match and partial match. *)
  | NOMATCH (* (-1) *)
  | PARTIAL (* (-2) *)
  (* Error codes for UTF-8 validity checks. See man 3 pcre2unicode. *)
  | UTF8_ERR1 (* (-3) *)
  | UTF8_ERR2 (* (-4) *)
  | UTF8_ERR3 (* (-5) *)
  | UTF8_ERR4 (* (-6) *)
  | UTF8_ERR5 (* (-7) *)
  | UTF8_ERR6 (* (-8) *)
  | UTF8_ERR7 (* (-9) *)
  | UTF8_ERR8 (* (-10) *)
  | UTF8_ERR9 (* (-11) *)
  | UTF8_ERR10 (* (-12) *)
  | UTF8_ERR11 (* (-13) *)
  | UTF8_ERR12 (* (-14) *)
  | UTF8_ERR13 (* (-15) *)
  | UTF8_ERR14 (* (-16) *)
  | UTF8_ERR15 (* (-17) *)
  | UTF8_ERR16 (* (-18) *)
  | UTF8_ERR17 (* (-19) *)
  | UTF8_ERR18 (* (-20) *)
  | UTF8_ERR19 (* (-21) *)
  | UTF8_ERR20 (* (-22) *)
  | UTF8_ERR21 (* (-23) *)
  (* TODO(* (non-8 support) *):
     (* Error codes for UTF-16 validity checks *)
     | UTF16_ERR1      (* (-24) *)
     | UTF16_ERR2      (* (-25) *)
     | UTF16_ERR3      (* (-26) *)
     | (* Error codes for UTF-32 validity checks *)
     | UTF32_ERR1      (* (-27) *)
     | UTF32_ERR2      (* (-28) *)
  *)
  (* Miscellaneous error codes for pcre2[_dfa]_match, substring extraction
     functions, context functions, and serializing functions. They are in numerical
     order. Originally they were in alphabetical order too, but now that PCRE2 is
     released, the numbers must not be changed. *)
  | BADDATA (* (-29) *)
  | MIXEDTABLES
  (* (-30) *)
  (* Name was changed *)
  | BADMAGIC (* (-31) *)
  | BADMODE (* (-32) *)
  | BADOFFSET (* (-33) *)
  | BADOPTION (* (-34) *)
  | BADREPLACEMENT (* (-35) *)
  | BADUTFOFFSET (* (-36) *)
  | CALLOUT (* (-37) *)
  (* Never used by PCRE2 itself *)
  | DFA_BADRESTART (* (-38) *)
  | DFA_RECURSE (* (-39) *)
  | DFA_UCOND (* (-40) *)
  | DFA_UFUNC (* (-41) *)
  | DFA_UITEM (* (-42) *)
  | DFA_WSSIZE (* (-43) *)
  | INTERNAL (* (-44) *)
  | JIT_BADOPTION (* (-45) *)
  | JIT_STACKLIMIT (* (-46) *)
  | MATCHLIMIT (* (-47) *)
  | NOMEMORY (* (-48) *)
  | NOSUBSTRING (* (-49) *)
  | NOUNIQUESUBSTRING (* (-50) *)
  | NULL (* (-51) *)
  | RECURSELOOP (* (-52) *)
  | DEPTHLIMIT (* (-53) *)
  | UNAVAILABLE (* (-54) *)
  | UNSET (* (-55) *)
  | BADOFFSETLIMIT (* (-56) *)
  | BADREPESCAPE (* (-57) *)
  | REPMISSINGBRACE (* (-58) *)
  | BADSUBSTITUTION (* (-59) *)
  | BADSUBSPATTERN (* (-60) *)
  | TOOMANYREPLACE (* (-61) *)
  | BADSERIALIZEDDATA (* (-62) *)
  | HEAPLIMIT (* (-63) *)
  | CONVERT_SYNTAX (* (-64) *)
  | INTERNAL_DUPMATCH (* (-65) *)
  | DFA_UINVALID_UTF (* (-66) *)
  | INVALIDOFFSET (* (-67) *)

type compile_match_options = [ `ANCHORED | `NO_UTF_CHECK | `ENDANCHORED ]

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

(* Fastpath to JIT match for perf *)
module Jit = struct
  type matching_mode = JIT_COMPLETE | JIT_PARTIAL_SOFT | JIT_PARTIAL_HARD

  let int32_of_matching_mode : matching_mode -> int32 = function
    | JIT_COMPLETE     -> 0x00000001l
    | JIT_PARTIAL_SOFT -> 0x00000002l
    | JIT_PARTIAL_HARD -> 0x00000004l
  [@@ocamlformat "disable"]

  type compile_option =
    (* one of these three is required. Refactor appropriately? *)
    | JIT_INVALID_UTF (* [@deprecated "MATCH_INVALID_UTF should be used instead"] *)
  (* deprecated (not sure when v10.34ish?) - use MATCH_INVALID_UTF *)

  let int32_of_compile_option : compile_option -> int32 = function
    | (JIT_INVALID_UTF [@alert "-deprecated"]) -> 0x00000100l

  let bitvector_of_compile_options (opts : compile_option list) : int32 =
    opts |> List.map int32_of_compile_option |> List.fold_left Int32.logor 0l

  type match_option =
    [ `NOTBOL
    | `NOTEOL
    | `NOTEMPTY
    | `NOTEMPTY_ATSTART
    | `PARTIAL_SOFT
    | `PARTIAL_HARD ]

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

  let compile ?(options : compile_option list = [])
      ?(mode : matching_mode = JIT_COMPLETE) (interp : interp regex) :
      (jit regex, compile_error) Result.t =
    let mode = int32_of_matching_mode mode in
    let options = Int32.logor mode (bitvector_of_compile_options options) in
    (* TODO: error location? *)
    Bindings.pcre2_jit_compile interp options
    |> Result.map_error compile_error_of_int

  let find ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : jit regex) (subject : string) : match_ option =
    let options = bitvector_of_match_options options in
    match Bindings.pcre2_jit_match re subject subject_offset options with
    | Ok (i, j) -> Some (subject, i, j)
    | Error _ -> None

  (* TODO(cooper): dedup impl with a functor? *)
  let find_iter ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : jit regex) (subject : string) : match_ Seq.t =
    Seq.unfold
      (fun offset ->
        find ~options ~subject_offset:offset re subject >+= fun (m : match_) ->
        (m, (range_of_match m).end_))
      subject_offset

  let captures :
      ?options:match_option list ->
      ?subject_offset:int ->
      jit regex ->
      string ->
      captures option =
   fun ?options:_ ?subject_offset:_ _ _ -> failwith "todo"

  let captures_iter :
      ?options:match_option list ->
      ?subject_offset:int ->
      jit regex ->
      string ->
      captures Seq.t =
   fun ?options:_ ?subject_offset:_ _ _ -> failwith "todo"

  let split :
      ?options:match_option list ->
      ?subject_offset:int ->
      ?limit:int ->
      jit regex ->
      string ->
      string list =
   fun ?options:_ ?subject_offset:_ ?limit:_ _ _ -> failwith "todo"

  (* TODO(cooper): dedup impl with a functor? *)
  let is_match ?(options : match_option list = []) ?(subject_offset : int = 0)
      (re : jit regex) (subject : string) : bool =
    find ~options ~subject_offset re subject |> Option.is_some

  let replace :
      ?options:match_option list ->
      ?subject_offset:int ->
      jit regex ->
      substitution ->
      string ->
      string =
   fun ?options:_ ?subject_offset:_ _ _ _ -> failwith "todo"
end

type dfa_match_option =
  (* shared *)
  [ Jit.match_option
  | compile_match_options
  | `COPY_MATCHED_SUBJECT
  | `DISABLE_RECURSELOOP_CHECK
  | (* exclusive *)
    `DFA_RESTART
  | `DFA_SHORTEST ]

type match_option =
  (* shared *)
  [ Jit.match_option
  | `COPY_MATCHED_SUBJECT
  | `DISABLE_RECURSELOOP_CHECK
  | `NO_JIT ]

let int32_of_match_option : match_option -> int32 = function
  | #Jit.match_option as jit_opt -> Jit.int32_of_match_option jit_opt
  | `COPY_MATCHED_SUBJECT -> 0x00004000l
  | `DISABLE_RECURSELOOP_CHECK -> 0x00040000l
  | `NO_JIT -> 0x00002000l

let bitvector_of_match_options (opts : match_option list) : int32 =
  opts |> List.map int32_of_match_option |> List.fold_left Int32.logor 0l

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

type newline_compile_ctx_option =
  | NEWLINE_CR
  | NEWLINE_LF
  | NEWLINE_CRLF
  | NEWLINE_ANY
  | NEWLINE_ANYCRLF
  | NEWLINE_NUL

type bsr = BSR_UNICODE | ANYCRLF

let version : int * int = Bindings.get_version ()
let config_unicode : bool = true

(** Default limit for calls to internal matching function *)
let config_match_limit : int = -1

(** Default limit for depth of nested backtracking *)
let config_depth_limit : int = -1

(** Indicates use of stack recursion in matching function *)
let config_stackrecurse : bool = true

(** Compiled regular expressions *)

(* Regex pattern kind *)

let compile ?(options : compile_option list = []) (pattern : string) :
    (interp regex, compile_error) Result.t =
  let options = bitvector_of_compile_options options in
  (* TODO: error location? *)
  Bindings.pcre2_compile pattern options
  |> Result.map_error compile_error_of_int

let capture_groups (r : _ regex) = Bindings.get_capture_groups r

let find ?(options : match_option list = []) ?(subject_offset : int = 0)
    (re : _ regex) (subject : string) : match_ option =
  let options = bitvector_of_match_options options in
  match Bindings.pcre2_match re subject subject_offset options with
  | Ok (start, end_) -> Some (subject, start, end_)
  | Error _ -> None

let find_iter ?(options : match_option list = []) ?(subject_offset : int = 0)
    (re : _ regex) (subject : string) : match_ Seq.t =
  Seq.unfold
    (fun offset ->
      find ~options ~subject_offset:offset re subject >+= fun (m : match_) ->
      (m, (range_of_match m).end_))
    subject_offset

let captures ?(options : match_option list = []) ?(subject_offset : int = 0)
    (re : _ regex) (subject : string) : captures option =
  let options = bitvector_of_match_options options in
  match Bindings.pcre2_capture re subject subject_offset options with
  | Ok (arr, names) -> Some (subject, arr, names)
  | Error _ -> None

let captures_iter ?(options : match_option list = [])
    ?(subject_offset : int = 0) (re : _ regex) (subject : string) :
    captures Seq.t =
  Seq.unfold
    (fun offset ->
      captures ~options ~subject_offset:offset re subject
      >+= fun (c : captures) -> (c, (range_of_captures c).end_))
    subject_offset

let split :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?limit:int ->
    _ regex ->
    string ->
    string list =
 fun ?options:_ ?subject_offset:_ ?limit:_ _ _ -> failwith "todo"

let is_match ?(options : match_option list = []) ?(subject_offset : int = 0)
    (re : _ regex) (subject : string) : bool =
  find ~options ~subject_offset re subject |> Option.is_some

let subst : string -> substitution = fun _ -> failwith "todo"

let replace :
    ?options:match_option list ->
    ?subject_offset:int ->
    _ regex ->
    substitution ->
    string ->
    string =
 fun ?options:_ ?subject_offset:_ _ _ _ -> failwith "todo"
