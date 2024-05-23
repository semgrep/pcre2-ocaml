[@@@warning "-32"]

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

type compile_option =
  [ `ALLOW_EMPTY_CLASS
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

(* for compile ctx - can combine and just split back as needed in bindings? *)
type compile_ctx =
  [ `EXTRA_ALLOW_SURROGATE_ESCAPES
  | `EXTRA_BAD_ESCAPE_IS_LITERAL
  | `EXTRA_MATCH_WORD
  | `EXTRA_MATCH_LINE
  | `EXTRA_ESCAPED_CR_IS_LF
  | `EXTRA_ALT_BSUX
  | `EXTRA_ALLOW_LOOKAROUND_BSK
  | `EXTRA_CASELESS_RESTRICT
  | `EXTRA_ASCII_BSD
  | `EXTRA_ASCII_BSS
  | `EXTRA_ASCII_BSW
  | `EXTRA_ASCII_POSIX
  | `EXTRA_ASCII_DIGIT ]

type jit_compile_flag =
  | JIT_COMPLETE
  | JIT_PARTIAL_SOFT
  | JIT_PARTIAL_HARD
  | JIT_INVALID_UTF

type match_option =
  [ `NOTBOL
  | `NOTEOL
  | `NOTEMPTY
  | `NOTEMPTY_ATSTART
  | `PARTIAL_SOFT
  | `PARTIAL_HARD
  | `DFA_RESTART (* pcre2_dfa_match() only *)
  | `DFA_SHORTEST (* pcre2_dfa_match() only *)
  | `SUBSTITUTE_GLOBAL (* pcre2_substitute() only *)
  | `SUBSTITUTE_EXTENDED (* pcre2_substitute() only *)
  | `SUBSTITUTE_UNSET_EMPTY (* pcre2_substitute() only *)
  | `SUBSTITUTE_UNKNOWN_UNSET (* pcre2_substitute() only *)
  | `SUBSTITUTE_OVERFLOW_LENGTH (* pcre2_substitute() only *)
  | `NO_JIT (* not for pcre2_dfa_match() *)
  | `COPY_MATCHED_SUBJECT
  | `SUBSTITUTE_LITERAL (* pcre2_substitute() only *)
  | `SUBSTITUTE_MATCHED (* pcre2_substitute() only *)
  | `SUBSTITUTE_REPLACEMENT_ONLY (* pcre2_substitute() only *)
  | `DISABLE_RECURSELOOP_CHECK
    (* not for pcre2_dfa_match() or pcre2_jit_match() *) ]
(* TODO: split to enforce restrictions (maybe except `NO_JIT) *)

type newline_compile_ctx_option =
  | NEWLINE_CR
  | NEWLINE_LF
  | NEWLINE_CRLF
  | NEWLINE_ANY
  | NEWLINE_ANYCRLF
  | NEWLINE_NUL

type bsr = BSR_UNICODE | ANYCRLF

let version : int * int = (10, 43)
let config_unicode : bool = true

(** Default limit for calls to internal matching function *)
let config_match_limit : int = -1

(** Default limit for depth of nested backtracking *)
let config_depth_limit : int = -1

(** Indicates use of stack recursion in matching function *)
let config_stackrecurse : bool = true

(** Compiled regular expressions *)

type interp = Bindings.interp
type jit = Bindings.jit
type 'a regex = 'a Bindings.regex
type match_ (* need only ovec? *)
type captures (* needs match_data *)

(* Regex pattern kind *)

let compile :
    ?options:compile_option list ->
    string ->
    (interp regex, compile_error) Result.t =
 fun ?options:_ _ -> failwith "todo"

let find :
    ?options:match_option list ->
    ?subject_offset:int ->
    _ regex ->
    string ->
    match_ option =
 fun ?options:_ ?subject_offset:_ _ _ -> failwith "todo"

let find_iter :
    ?options:match_option list ->
    ?subject_offset:int ->
    _ regex ->
    string ->
    match_ Seq.t =
 fun ?options:_ ?subject_offset:_ _ _ -> failwith "todo"

let captures :
    ?options:match_option list ->
    ?subject_offset:int ->
    _ regex ->
    string ->
    captures option =
 fun ?options:_ ?subject_offset:_ _ _ -> failwith "todo"

let captures_iter :
    ?options:match_option list ->
    ?subject_offset:int ->
    _ regex ->
    string ->
    captures Seq.t =
 fun ?options:_ ?subject_offset:_ _ _ -> failwith "todo"

let split :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?limit:int ->
    _ regex ->
    string ->
    string list =
 fun ?options:_ ?subject_offset:_ ?limit:_ _ _ -> failwith "todo"

let is_match :
    ?options:match_option list ->
    ?subject_offset:int ->
    _ regex ->
    string ->
    bool =
 fun ?options:_ ?subject_offset:_ _ _ -> failwith "todo"

type substitution

let subst : string -> substitution = fun _ -> failwith "todo"

let replace :
    ?options:match_option list ->
    ?subject_offset:int ->
    _ regex ->
    substitution ->
    string ->
    string =
 fun ?options:_ ?subject_offset:_ _ _ _ -> failwith "todo"

(* Fastpath to JIT match for perf *)
module Jit = struct
  let compile :
      ?options:compile_option list ->
      string ->
      (interp regex, compile_error) Result.t =
   fun ?options:_ _ -> failwith "todo"

  let find :
      ?options:match_option list ->
      ?subject_offset:int ->
      jit regex ->
      string ->
      match_ option =
   fun ?options:_ ?subject_offset:_ _ _ -> failwith "todo"

  let find_iter :
      ?options:match_option list ->
      ?subject_offset:int ->
      jit regex ->
      string ->
      match_ Seq.t =
   fun ?options:_ ?subject_offset:_ _ _ -> failwith "todo"

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

  let is_match :
      ?options:match_option list ->
      ?subject_offset:int ->
      jit regex ->
      string ->
      bool =
   fun ?options:_ ?subject_offset:_ _ _ -> failwith "todo"

  let replace :
      ?options:match_option list ->
      ?subject_offset:int ->
      jit regex ->
      substitution ->
      string ->
      string =
   fun ?options:_ ?subject_offset:_ _ _ _ -> failwith "todo"
end
