type match_ [@@deriving show, eq]
type captures [@@deriving show, eq]

type compile_error =
  | END_BACKSLASH  (** A pattern string ends in a backslash *)
  | END_BACKSLASH_C
      (** A pattern string ends in \c, which should require an additional
          character following it. *)
  | UNKNOWN_ESCAPE
      (** An unrecognized character, not suitable for starting an escape
          sequence, followed a backslash. *)
  | QUANTIFIER_OUT_OF_ORDER
      (** The numbers were out of order in a {} quantifier (i.e., {n,m} where
          n > m). *)
  | QUANTIFIER_TOO_BIG  (** A limit was too big in a {} quantifier. *)
  | MISSING_SQUARE_BRACKET  (** Missing terminating ] for a character class *)
  | ESCAPE_INVALID_IN_CLASS
      (** Invalid escape sequence in a characters class *)
  | CLASS_RANGE_ORDER  (** Range out of order in character class *)
  | QUANTIFIER_INVALID  (** Quantifier does not follow a repeatable item *)
  | INTERNAL_UNEXPECTED_REPEAT  (** Internal error *)
  | INVALID_AFTER_PARENS_QUERY  (** Unrecognized character after (? or (?- *)
  | POSIX_CLASS_NOT_IN_CLASS
      (** POSIX named classes are supported only within a class *)
  | POSIX_NO_SUPPORT_COLLATING
      (** POSIX collating elements are not supported *)
  | MISSING_CLOSING_PARENTHESIS  (** Missing closing parenthesis. *)
  | BAD_SUBPATTERN_REFERENCE  (** Reference to non-existent subpattern *)
  | NULL_PATTERN
      (** Pattern passed as a null pointer but with non-zero length *)
  | BAD_OPTIONS  (** Unrecognized option bits *)
  | MISSING_COMMENT_CLOSING  (** Missing ) after (?# comment *)
  | PARENTHESES_NEST_TOO_DEEP  (** Parentheses are nested too deeply *)
  | PATTERN_TOO_LARGE  (** Regular expression is too large *)
  | HEAP_FAILED  (** Failed to allocate heap memory *)
  | UNMATCHED_CLOSING_PARENTHESIS  (** Unmatched closing parenthesis *)
  | INTERNAL_CODE_OVERFLOW  (** Internal *)
  | MISSING_CONDITION_CLOSING  (** Missing closing parenthesis for condition *)
  | LOOKBEHIND_NOT_FIXED_LENGTH
      (** Length of a lookbehind assertion is not limited *)
  | ZERO_RELATIVE_REFERENCE  (** A relative value of zero is not permitted *)
  | TOO_MANY_CONDITION_BRANCHES
      (** Conditional subpattern contains more than two branches *)
  | CONDITION_ASSERTION_EXPECTED  (** Assertion expected after (?( or (?(?C) *)
  | BAD_RELATIVE_REFERENCE  (** Digit expected ater (?+ or (?- *)
  | UNKNOWN_POSIX_CLASS  (** Unknown POSIX class name *)
  | INTERNAL_STUDY_ERROR  (** Internal *)
  | UNICODE_NOT_SUPPORTED  (** No Unicode support *)
  | PARENTHESES_STACK_CHECK
      (** Parentheses are too deeply nested (stack check) *)
  | CODE_POINT_TOO_BIG
      (** The character code point value in \x{} or \o{} is too large *)
  | LOOKBEHIND_TOO_COMPLICATED  (** Lookbehind is too complicated *)
  | LOOKBEHIND_INVALID_BACKSLASH_C
      (** \C is not allowed in a lookbehind assertion in the current
          UTF-{8,16,32} mode *)
  | UNSUPPORTED_ESCAPE_SEQUENCE
      (** PCRE2 does not suport \F, \L, \l, \N{name}, \U or \u *)
  | CALLOUT_NUMBER_TOO_BIG  (** Number after (?C is greater than 255 *)
  | MISSING_CALLOUT_CLOSING  (** Closing parenthesis for (?C missing *)
  | ESCAPE_INVALID_IN_VERB  (** invalid escape sequence in ( *VERB) name *)
  | UNRECOGNIZED_AFTER_QUERY_P  (** Unrecognized character after (?P *)
  | MISSING_NAME_TERMINATOR
      (** Syntax error in subpattern name (missing terminator?) *)
  | DUPLICATE_SUBPATTERN_NAME
      (** two named subpatterns have  the same name (PCRE2_DUPNAMES not set) *)
  | INVALID_SUBPATTERN_NAME  (** subpattern naem must start with a non-digit *)
  | UNICODE_PROPERTIES_UNAVAILABLE
      (** This version of PCRE2 does not support \P, \p or \X *)
  | MALFORMED_UNICODE_PROPERTY  (** Malformed \P or \p sequence *)
  | UNKNOWN_UNICODE_PROPERTY  (** Unknown property after \P or \p *)
  | SUBPATTERN_NAME_TOO_LONG
      (** Subpattern name is too long (max MAX_NAME_SIZE) code units *)
  | TOO_MANY_NAMED_SUBPATTERNS
      (** Too many named subpatterns (max MAX_NAME_SIZE) *)
  | CLASS_INVALID_RANGE  (** Invalid range in character class *)
  | OCTAL_BYTE_TOO_BIG
      (** Octal value is greater than \377 in 8-bit non-UTF-8 *)
  | INTERNAL_OVERRAN_WORKSPACE  (** Internal *)
  | INTERNAL_MISSING_SUBPATTERN  (** Internal *)
  | DEFINE_TOO_MANY_BRANCHES
      (** DEFINE subpattern contains more than one branch *)
  | BACKSLASH_O_MISSING_BRACE  (** Missing opening brace after \o *)
  | INTERNAL_UNKNOWN_NEWLINE  (** Internal *)
  | BACKSLASH_G_SYNTAX
      (** \g is not followed by a braced, angle-bracketed, or quoted
          name/number or by a plain number *)
  | PARENS_QUERY_R_MISSING_CLOSING
      (** (?R (recursive pattern call) must be followed by a closing
          parenthesis *)
  | VERB_ARGUMENT_NOT_ALLOWED
      (** obsolete - an argument is not allowed for ACCEPT, FAIL or COMMIT *)
  | VERB_UNKNOWN  (** ( *VERB) not recognized or malformed *)
  | SUBPATTERN_NUMBER_TOO_BIG  (** Subpattern number is too big *)
  | SUBPATTERN_NAME_EXPECTED  (** Subpattern name expected *)
  | INTERNAL_PARSED_OVERFLOW  (** Internal *)
  | INVALID_OCTAL  (** Non-octal character in \o{} (closing brace missing?) *)
  | SUBPATTERN_NAMES_MISMATCH
      (** Different names for subpatterns of the same number are not allowed *)
  | MARK_MISSING_ARGUMENT  (** ( *MARK) must have an argument *)
  | INVALID_HEXADECIMAL
      (** Non-hex character in \x{} (closing brace missing?) *)
  | BACKSLASH_C_SYNTAX  (** \c must be followed by a letter or one of [\]^_? *)
  | BACKSLASH_K_SYNTAX
      (** \k is not followed by a braced, angle-bracketed, or quoted name *)
  | INTERNAL_BAD_CODE_LOOKBEHINDS  (** Internal *)
  | BACKSLASH_N_IN_CLASS  (** \N is not supported in a class *)
  | CALLOUT_STRING_TOO_LONG  (** Callout srtring is too long *)
  | UNICODE_DISALLOWED_CODE_POINT
      (** Disallowed unicode code point (>= \0xd800 && <= 0xdfff) *)
  | UTF_IS_DISABLED  (** Using UTF is disabled by the application *)
  | UCP_IS_DISABLED  (** Using UCP is disabled by the application *)
  | VERB_NAME_TOO_LONG  (** name is too long in MARK, PRUNE, SKIP or THEN *)
  | BACKSLASH_U_CODE_POINT_TOO_BIG
      (** Character code point value in \u sequence is too large *)
  | MISSING_OCTAL_OR_HEX_DIGITS  (** Digits missing in \x{} or \o{} or \N{U+} *)
  | VERSION_CONDITION_SYNTAX
      (** Syntax error or number too big in (?(VERSION condition *)
  | INTERNAL_BAD_CODE_AUTO_POSSESS  (** Internal *)
  | CALLOUT_NO_STRING_DELIMITER
      (** Missing terminating delimiter for callout with string argument *)
  | CALLOUT_BAD_STRING_DELIMITER
      (** Unrecognized string delimiter follows (?C *)
  | BACKSLASH_C_CALLER_DISABLED  (** Using \C is disabled by the application *)
  | QUERY_BARJX_NEST_TOO_DEEP
      (** (?| and/or (?J: or (?x: parentheses are too deeply nested *)
  | BACKSLASH_C_LIBRARY_DISABLED
      (** Using \C is disabled in this PCRE2 library *)
  | PATTERN_TOO_COMPLICATED  (** Regular expression is too complicated *)
  | LOOKBEHIND_TOO_LONG  (** Lookbehind assertion is too long *)
  | PATTERN_STRING_TOO_LONG
      (** Pattern string is longer than the limit set by the application *)
  | INTERNAL_BAD_CODE  (** Internal *)
  | INTERNAL_BAD_CODE_IN_SKIP  (** Internal *)
  | NO_SURROGATES_IN_UTF16
      (** EXTRA_ALLOW_SURROGRATE_ESCAPES is not allowed in UTF-16 mode *)
  | BAD_LITERAL_OPTIONS  (** Invalid options bits with PCRE2_LITERAL *)
  | SUPPORTED_ONLY_IN_UNICODE
      (** \N{U+dddd} is supported only in Unicode mode *)
  | INVALID_HYPHEN_IN_OPTIONS  (** Invalid hyphen in option setting *)
  | ALPHA_ASSERTION_UNKNOWN  (** alpha_assertion not recognized *)
  | SCRIPT_RUN_NOT_AVAILABLE
      (** Script runs require Unicode support, which this version of PCRE2 does
          not have *)
  | TOO_MANY_CAPTURES  (** Too many capturing groups (max 65535) *)
  | CONDITION_ATOMIC_ASSERTION_EXPECTED
      (** Atomic assertion expected after (?( or (?(?C) *)
  | BACKSLASH_K_IN_LOOKAROUND
      (** \K is not allowed in lookarounds (cf.  EXTRA_ALLOW_LOOKAROUND_BSK) *)
[@@deriving show, eq]

type match_error =
  (* Error codes for UTF-8 validity checks. See man 3 pcre2unicode. *)
  | UTF8_ERR1
      (** The string ends with a truncated UTF-8 character; 1 byte is missing.
       *)
  | UTF8_ERR2
      (** The string ends with a truncated UTF-8 character; 2 bytes are
          missing. *)
  | UTF8_ERR3
      (** The string ends with a truncated UTF-8 character; 3 bytes are
          missing. *)
  | UTF8_ERR4
      (** The string ends with a truncated UTF-8 character; 4 bytes are
          missing. Note that this is possible since although RFC 3629 restricts
          UTF-8 characters to be no longer than 4 bytes, the encoding scheme
          (originally defined by RFC 2279) allows for up to 6 bytes, and this
          is checked first. *)
  | UTF8_ERR5
      (** The string ends with a truncated UTF-8 character; 5 bytes are
          missing. Note that this is possible since although RFC 3629 restricts
          UTF-8 characters to be no longer than 4 bytes, the encoding scheme
          (originally defined by RFC 2279) allows for up to 6 bytes, and this
          is checked first. *)
  | UTF8_ERR6
      (** The two most significant bits of the 2nd byte of the character do not
          have the binary value 0b10. *)
  | UTF8_ERR7
      (** The two most significant bits of the 3rd byte of the character do not
          have the binary value 0b10. *)
  | UTF8_ERR8
      (** The two most significant bits of the 4th byte of the character do not
          have the binary value 0b10. *)
  | UTF8_ERR9
      (** The two most significant bits of the 5th byte of the character do not
          have the binary value 0b10. Note that this is possible since although
          RFC 3629 restricts UTF-8 characters to be no longer than 4 bytes, the
          encoding scheme (originally defined by RFC 2279) allows for up to 6
          bytes, and this is checked first.*)
  | UTF8_ERR10
      (** The two most significant bits of the 6th byte of the character do not
          have the binary value 0b10. Note that this is possible since although
          RFC 3629 restricts UTF-8 characters to be no longer than 4 bytes, the
          encoding scheme (originally defined by RFC 2279) allows for up to 6
          bytes, and this is checked first.*)
  | UTF8_ERR11
      (** A character that is valid by the RFC 2279 rules is 5 bytes long;
          these code points are excluded by RFC 3629. *)
  | UTF8_ERR12
      (** A character that is valid by the RFC 2279 rules is 6 bytes long;
          these code points are excluded by RFC 3629. *)
  | UTF8_ERR13
      (** A 4 byte character has a value greater than 0x10ffff; these code
          points are excluded by RFC 3629. *)
  | UTF8_ERR14
      (** A 3 byte character has a value in the range 0xd800 to 0xdfff; this
        range of code points are reserved by RFC 3629 for use with UTF-16, and
        so are excluded from UTF-8. *)
  | UTF8_ERR15
      (** A 2 byte character is "overlong", that is, it codes for a value that
          can be represented by fewer bytes, which is invalid. For example, the
          two bytes 0xc0, 0xae give the value 0x2e, whose correct coding uses
          just one byte. *)
  | UTF8_ERR16
      (** A 3 byte character is "overlong", that is, it codes for a value that
          can be represented by fewer bytes, which is invalid. *)
  | UTF8_ERR17
      (** A 4 byte character is "overlong", that is, it codes for a value that
          can be represented by fewer bytes, which is invalid. *)
  | UTF8_ERR18
      (** A 5 byte character is "overlong", that is, it codes for a value that
          can be represented by fewer bytes, which is invalid. Note that this
          is possible since although RFC 3629 restricts
          UTF-8 characters to be no longer than 4 bytes, the encoding scheme
          (originally defined by RFC 2279) allows for up to 6 bytes, and this
          is checked first. *)
  | UTF8_ERR19
      (** A 6 byte character is "overlong", that is, it codes for a value that
          can be represented by fewer bytes, which is invalid. Note that this
          is possible since although RFC 3629 restricts
          UTF-8 characters to be no longer than 4 bytes, the encoding scheme
          (originally defined by RFC 2279) allows for up to 6 bytes, and this
          is checked first.*)
  | UTF8_ERR20
      (** The two most significant bits of the first byte of a character have
          the binary value 0b10 (that is, the most significant bit is 1 and the
          second is 0). Such a byte can only validly occur as the second or
          subsequent byte of a multi-byte character. *)
  | UTF8_ERR21
      (** The first byte of a character has the value 0xfe or 0xff. These
          values can never occur in a valid UTF-8 string. *)
  (* Miscellaneous error codes for pcre2[_dfa]_match, substring extraction
     functions, context functions, and serializing functions. *)
  | BADDATA  (** Invalid data was provided to the function *)
  | MIXEDTABLES
      (** During serialization, the patterns do not all use the same tables *)
  | BADMAGIC
      (** PCRE2 stores a 4-byte "magic number" at the start of the compiled
          code, to catch the case when it is passed a junk pointer. This is the
          error that is returned when the magic number is not present. *)
  | BADMODE
      (** This error is given when a compiled pattern is passed to a function
          in a library of a different code unit width, for example, a pattern
          compiled by the 8-bit library is passed to a 16-bit or 32-bit library
          function. *)
  | BADOFFSET
      (** The value of the subject offset was negative or greater than the
          length of the subject. *)
  | BADOPTION  (** An unrecognized bit was set in the options argument. *)
  | BADREPLACEMENT
      (** used for miscellaneous syntax errors in the replacement string with
          pcre2_substitute *)
  | BADUTFOFFSET
      (** The UTF code unit sequence that was passed as a subject was checked
    and found to be valid (the PCRE2_NO_UTF_CHECK option was not set), but the
    value of startoffset did not point to the beginning of a UTF character or
    the end of the subject. *)
  | CALLOUT
      (**  This error is never generated by pcre2_match() itself. It is
           provided for use by callout functions that want to cause
           pcre2_match() or pcre2_callout_enumerate() to return a distinctive
           error code. *)
  | DFA_BADRESTART
      (**  When pcre2_dfa_match() is called with the PCRE2_DFA_RESTART option,
           some plausibility checks are made on the contents of the workspace,
           which should contain data about the previous partial match. If any
           of these checks fail, this error is given. *)
  | DFA_RECURSE
      (**  When a recursion or subroutine call is processed, the matching
           function calls itself recursively, using private memory for the
           ovector and workspace. This error is given if the internal ovector
           is not large enough. This should be extremely rare, as a vector of
           size 1000 is used. *)
  | DFA_UCOND
      (** This return is given if pcre2_dfa_match() encounters a condition item
          that uses a backreference for the condition, or a test for recursion
          in a specific capture group. These are not supported. *)
  | DFA_UFUNC
      (** A convenience function unsupported by the DFA was used on the result
          of a DFA match (e.g., extraction by substring name) *)
  | DFA_UITEM
      (** This return is given if pcre2_dfa_match() encounters an item in the
          pattern that it does not support, for instance, the use of \C in a
          UTF mode or a backreference. *)
  | DFA_WSSIZE
      (** This return is given if pcre2_dfa_match() runs out of space in the
          workspace vector. *)
  | INTERNAL
      (** An unexpected internal error has occurred. This error could be caused
          by a bug in PCRE2 or by overwriting of the compiled pattern. *)
  | JIT_BADOPTION
      (** A matching mode which was not compiled was requested, or an unknown
          bit was set in jit compilation options. *)
  | JIT_STACKLIMIT
      (** This error is returned when a pattern that was successfully studied
          using JIT is being matched, but the memory available for the
          just-in-time processing stack is not large enough. See the pcre2jit
          documentation for more details *)
  | MATCHLIMIT  (** The backtracking match limit was reached. *)
  | NOMEMORY
      (** Heap memory is used to remember backtracking points. This error is
          given when the memory allocation function (default or custom) fails.
          Note that a different error, PCRE2_ERROR_HEAPLIMIT, is given if the
          amount of memory needed exceeds the heap limit. PCRE2_ERROR_NOMEMORY
          is also returned if PCRE2_COPY_MATCHED_SUBJECT is set and memory
          allocation fails. *)
  | NOSUBSTRING
      (** - in substrings: There is no substring with that number in the
          pattern, that is, the number is greater than the number of capturing
          parentheses.

          - in substitution: returned for a non-existent substring insertion,
          unless PCRE2_SUBSTITUTE_UNKNOWN_UNSET is set.
        *)
  | NOUNIQUESUBSTRING
      (** Returned when there is more than one capture group with a given name
          and this would result in ambiguity.
        *)
  | NULL
      (** Either the code, subject, or match_data argument was passed as NULL.
       *)
  | RECURSELOOP
      (** This error is returned when pcre2_match() detects a recursion loop
          within the pattern. Specifically, it means that either the whole
          pattern or a capture group has been called recursively for the second
          time at the same position in the subject string. Some simple patterns
          that might do this are detected and faulted at compile time, but more
          complicated cases, in particular mutual recursions between two
          different groups, cannot be detected until matching is attempted. *)
  | DEPTHLIMIT  (** The nested backtracking depth limit was reached. *)
  | UNAVAILABLE
      (** The substring number, though not greater than the number of captures
          in the pattern, is greater than the number of slots in the ovector,
          so the substring could not be captured. *)
  | UNSET
      (** - in substrings: The substring did not participate in the match. For
          example, if the pattern is (abc)|(def) and the subject is "def", and
          the ovector contains at least two capturing slots, substring number 1
          is unset.

          - in substitution: returned for an unset substring insertion
          (including an unknown substring when PCRE2_SUBSTITUTE_UNKNOWN_UNSET
          is set) when the simple (non-extended) syntax is used and
          PCRE2_SUBSTITUTE_UNSET_EMPTY is not set
        *)
  | BADOFFSETLIMIT
      (** An offset limit was set but the flag was not set at compile time *)
  | BADREPESCAPE
      (** invalid escape sequence in a replacement string used with
          pcre2_substitute *)
  | REPMISSINGBRACE
      (** missing closing curly bracket in a replacement string used with
          pcre2_substitute *)
  | BADSUBSTITUTION
      (** syntax error in extended group substitution in pcre2_substitute *)
  | BADSUBSPATTERN
      (** in pcre2_substitute, the pattern match ended before it started or the
          match started earlier than the current position in the subject, which
          can happen if \K is used in an assertion *)
  | TOOMANYREPLACE
      (** The number of total substitutions would exceed the maximum integer
          and cause overflow. *)
  | BADSERIALIZEDDATA  (** Invalid arguments to pcre2_serialize *)
  | HEAPLIMIT  (** The heap limit was reached. *)
  | CONVERT_SYNTAX  (** Syntax error in pcre2_convert *)
  | INTERNAL_DUPMATCH
      (** An internal error in substitution led to a duplicate match *)
  | DFA_UINVALID_UTF
      (** This return is given if pcre2_dfa_match() is called for a pattern
          that was compiled with PCRE2_MATCH_INVALID_UTF. This is not supported
          for DFA matching. *)
  | INVALIDOFFSET  (** internal error, should not occur *)
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

module Interp : sig
  include module type of Options.Interp

  include
    Intf.Matcher
      with type match_ = match_
       and type captures = captures
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
