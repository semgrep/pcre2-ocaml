type match_ [@@deriving show, eq]
type captures [@@deriving show, eq]
type substitution

type compile_error =
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
  | BAD_OPTIONS (* probably should not be possible *)
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
  | VERB_ARGUMENT_NOT_ALLOWED (* OBSOLETE; Should not occur - since when? *)
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

type match_error =
  (* "Expected" matching error codes: no match and partial match. *)
  (* | NOMATCH (* (-1) *)
     | PARTIAL (* (-2) *) *)
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
[@@deriving show, eq]

(* Fastpath to JIT match for perf *)

module Options : sig
  module Jit : sig
    type matching_mode = JIT_COMPLETE | JIT_PARTIAL_SOFT | JIT_PARTIAL_HARD
    type jit_only_compile_option = [ `JIT_INVALID_UTF ]

    type match_option =
      [ `NOTBOL
      | `NOTEOL
      | `NOTEMPTY
      | `NOTEMPTY_ATSTART
      | `PARTIAL_SOFT
      | `PARTIAL_HARD ]
  end

  module Interp : sig
    type compile_match_options = [ `ANCHORED | `NO_UTF_CHECK | `ENDANCHORED ]

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

    type match_option =
      (* shared *)
      [ Jit.match_option
      | `COPY_MATCHED_SUBJECT
      | `DISABLE_RECURSELOOP_CHECK
      | `NO_JIT
        (* not for pcre2_dfa_match() *)
        (* not for pcre2_dfa_match() or pcre2_jit_match() *) ]
    (* TODO: split to enforce restrictions (maybe except `NO_JIT) *)
    (* TODO: add match_context options (depth, heap, match) limits here or separately? *)

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
  end
end
(* TODO: hide, need since you can't do with type t = A | B or have
   type t = A | B
   include FOO with type t = t
*)

module Interp : sig
  include module type of Options.Interp

  include
    Intf.Matcher
      with type match_ = match_
       and type captures = captures
       and type substitution = substitution
       and type compile_option = Options.Interp.compile_option
       and type compile_error = compile_error
       and type match_option = Options.Interp.match_option
       and type match_error = match_error
end

module Jit : sig
  include module type of Options.Jit

  include
    Intf.Matcher
      with type match_ = match_
       and type captures = captures
       and type substitution = substitution
       and type compile_option =
        [ Options.Jit.jit_only_compile_option | Options.Interp.compile_option ]
       and type compile_error = compile_error
       and type match_option = Options.Jit.match_option
       and type match_error = match_error

  val of_interp :
    ?options:jit_only_compile_option list ->
    ?mode:matching_mode ->
    Interp.t ->
    (t, compile_error) Result.t
end

(** Version information *)
val version : int * int
(** Version of the PCRE2-C-library (major, minor) *)

val config_unicode : bool
(** Indicates whether unicode support is enabled *)

val config_match_limit : int
(** Default limit for calls to internal matching function *)

val config_depth_limit : int
(** Default limit for depth of nested backtracking *)

val config_stackrecurse : bool
(** Indicates use of stack recursion in matching function *)

type dfa_match_option =
  (* shared *)
  [ Options.Jit.match_option
  | Options.Interp.compile_match_options
  | `COPY_MATCHED_SUBJECT
  | `DISABLE_RECURSELOOP_CHECK
  | (* exclusive *)
    `DFA_RESTART
  | `DFA_SHORTEST ]
