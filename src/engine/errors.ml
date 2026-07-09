(* Error codes and error message texts for the pure-OCaml PCRE2 10.44 port.

   Ported from vendor/pcre2/src/pcre2_error.c (message texts and lookup) and
   vendor/pcre2/src/pcre2.h.generic (public error code numbers). The message
   strings appear verbatim in conformance expected output as
   "Failed: error NNN at offset N: <text>" lines. Module kept fully for
   fidelity. *)

(* pcre2_internal.h:239 — compile-time positive error numbers (all except
   UTF errors, which are negative) start at this value. *)
let compile_error_base = 100

(* ---------- Named error codes used often by the engine ---------- *)

(* Compile error codes: pcre2.h.generic:223-322. *)
let error_bad_options = 117 (* pcre2.h.generic:239 *)
let error_parentheses_nest_too_deep = 119 (* pcre2.h.generic:241 *)
let error_backslash_k_in_lookaround = 199 (* pcre2.h.generic:322 *)

(* pcre2_compile.c:794-812 — compile error code numbers: ERRn has the value
   COMPILE_ERROR_BASE + n (their public pcre2.h values are exactly 100
   greater than the enum offsets, i.e. errn = 100 + n). Constants are added
   here as ported code needs them; the message texts are in
   compile_error_texts below. *)
let err1 = compile_error_base + 1 (* \ at end of pattern *)
let err2 = compile_error_base + 2 (* \c at end of pattern *)
let err3 = compile_error_base + 3 (* unrecognized character follows \ *)
let err4 = compile_error_base + 4 (* numbers out of order in {} quantifier *)
let err5 = compile_error_base + 5 (* number too big in {} quantifier *)
let err15 = compile_error_base + 15 (* reference to non-existent subpattern *)
let err26 = compile_error_base + 26 (* a relative value of zero not allowed *)
let err34 = compile_error_base + 34 (* code point in \x{} or \o{} too large *)

let err37 =
  compile_error_base + 37 (* \F, \L, \l, \N{name}, \U, \u unsupported *)

let err42 = compile_error_base + 42 (* syntax error in subpattern name *)
let err44 = compile_error_base + 44 (* name must start with a non-digit *)
let err48 = compile_error_base + 48 (* subpattern name is too long *)

let err51 =
  compile_error_base + 51 (* octal value > \377 in 8-bit non-UTF mode *)

let err55 = compile_error_base + 55 (* missing opening brace after \o *)
let err57 = compile_error_base + 57 (* \g not followed by name/number *)
let err60 = compile_error_base + 60 (* ( *VERB) not recognized or malformed *)
let err61 = compile_error_base + 61 (* subpattern number is too big *)
let err62 = compile_error_base + 62 (* subpattern name expected *)
let err64 = compile_error_base + 64 (* non-octal character in \o{} *)
let err67 = compile_error_base + 67 (* non-hex character in \x{} *)
let err68 = compile_error_base + 68 (* \c must be followed by printable ASCII *)

let err73 =
  compile_error_base + 73 (* disallowed Unicode code point (surrogate) *)

let err77 =
  compile_error_base + 77 (* code point in \u.... sequence too large *)

let err78 =
  compile_error_base + 78 (* digits missing in \x{} or \o{} or \N{U+} *)

let err93 = compile_error_base + 93 (* \N{U+dddd} only in Unicode (UTF) mode *)

(* "Expected" matching error codes: pcre2.h.generic:327-328. *)
let error_nomatch = -1
let error_partial = -2

(* Miscellaneous match-time error codes: pcre2.h.generic:370-409. *)
let error_baddata = -29 (* pcre2.h.generic:370 *)
let error_badmagic = -31 (* pcre2.h.generic:372 *)
let error_badmode = -32 (* pcre2.h.generic:373 *)
let error_badoffset = -33 (* pcre2.h.generic:374 *)
let error_badoption = -34 (* pcre2.h.generic:375 *)
let error_badutfoffset = -36 (* pcre2.h.generic:377 *)
let error_internal = -44 (* pcre2.h.generic:385 *)
let error_matchlimit = -47 (* pcre2.h.generic:388 *)
let error_null = -51 (* pcre2.h.generic:392 *)
let error_recurseloop = -52 (* pcre2.h.generic:393 *)
let error_depthlimit = -53 (* pcre2.h.generic:394 *)
let error_heaplimit = -63 (* pcre2.h.generic:405 *)

(* ---------- Message texts ---------- *)

(* pcre2_error.c:65-193 — the texts of compile-time error messages. In C
   this is one long \0-separated string counted through by
   pcre2_get_error_message(); here index n holds the text for error number
   compile_error_base + n (codes 100..201; ERR0..ERR101 per
   pcre2_compile.c:795-812). Build-time inserts expanded for the 8-bit
   library: XSTRING(PCRE2_CODE_UNIT_WIDTH) = 8 (index 36),
   XSTRING(MAX_NAME_SIZE) = 128 (config.h.generic:218, index 48),
   XSTRING(MAX_NAME_COUNT) = 10000 (config.h.generic:211, index 49);
   index 68 is the non-EBCDIC variant. *)
let compile_error_texts =
  [|
    "no error";
    "\\ at end of pattern";
    "\\c at end of pattern";
    "unrecognized character follows \\";
    "numbers out of order in {} quantifier";
    (* 5 *)
    "number too big in {} quantifier";
    "missing terminating ] for character class";
    "escape sequence is invalid in character class";
    "range out of order in character class";
    "quantifier does not follow a repeatable item";
    (* 10 *)
    "internal error: unexpected repeat";
    "unrecognized character after (? or (?-";
    "POSIX named classes are supported only within a class";
    "POSIX collating elements are not supported";
    "missing closing parenthesis";
    (* 15 *)
    "reference to non-existent subpattern";
    "pattern passed as NULL with non-zero length";
    "unrecognised compile-time option bit(s)";
    "missing ) after (?# comment";
    "parentheses are too deeply nested";
    (* 20 *)
    "regular expression is too large";
    "failed to allocate heap memory";
    "unmatched closing parenthesis";
    "internal error: code overflow";
    "missing closing parenthesis for condition";
    (* 25 *)
    "length of lookbehind assertion is not limited";
    "a relative value of zero is not allowed";
    "conditional subpattern contains more than two branches";
    "assertion expected after (?( or (?(?C)";
    "digit expected after (?+ or (?-";
    (* 30 *)
    "unknown POSIX class name";
    "internal error in pcre2_study(): should not occur";
    "this version of PCRE2 does not have Unicode support";
    "parentheses are too deeply nested (stack check)";
    "character code point value in \\x{} or \\o{} is too large";
    (* 35 *)
    "lookbehind is too complicated";
    "\\C is not allowed in a lookbehind assertion in UTF-8 mode";
    "PCRE2 does not support \\F, \\L, \\l, \\N{name}, \\U, or \\u";
    "number after (?C is greater than 255";
    "closing parenthesis for (?C expected";
    (* 40 *)
    "invalid escape sequence in (*VERB) name";
    "unrecognized character after (?P";
    "syntax error in subpattern name (missing terminator?)";
    "two named subpatterns have the same name (PCRE2_DUPNAMES not set)";
    "subpattern name must start with a non-digit";
    (* 45 *)
    "this version of PCRE2 does not have support for \\P, \\p, or \\X";
    "malformed \\P or \\p sequence";
    "unknown property after \\P or \\p";
    "subpattern name is too long (maximum 128 code units)";
    "too many named subpatterns (maximum 10000)";
    (* 50 *)
    "invalid range in character class";
    "octal value is greater than \\377 in 8-bit non-UTF-8 mode";
    "internal error: overran compiling workspace";
    "internal error: previously-checked referenced subpattern not found";
    "DEFINE subpattern contains more than one branch";
    (* 55 *)
    "missing opening brace after \\o";
    "internal error: unknown newline setting";
    "\\g is not followed by a braced, angle-bracketed, or quoted name/number \
     or by a plain number";
    "(?R (recursive pattern call) must be followed by a closing parenthesis";
    "obsolete error (should not occur)";
    (* ^ was: "an argument is not allowed for (*ACCEPT), (*FAIL), or
       (*COMMIT)" — pcre2_error.c:136-137 *)
    (* 60 *)
    "(*VERB) not recognized or malformed";
    "subpattern number is too big";
    "subpattern name expected";
    "internal error: parsed pattern overflow";
    "non-octal character in \\o{} (closing brace missing?)";
    (* 65 *)
    "different names for subpatterns of the same number are not allowed";
    "(*MARK) must have an argument";
    "non-hex character in \\x{} (closing brace missing?)";
    "\\c must be followed by a printable ASCII character";
    "\\k is not followed by a braced, angle-bracketed, or quoted name";
    (* 70 *)
    "internal error: unknown meta code in check_lookbehinds()";
    "\\N is not supported in a class";
    "callout string is too long";
    "disallowed Unicode code point (>= 0xd800 && <= 0xdfff)";
    "using UTF is disabled by the application";
    (* 75 *)
    "using UCP is disabled by the application";
    "name is too long in (*MARK), (*PRUNE), (*SKIP), or (*THEN)";
    "character code point value in \\u.... sequence is too large";
    "digits missing in \\x{} or \\o{} or \\N{U+}";
    "syntax error or number too big in (?(VERSION condition";
    (* 80 *)
    "internal error: unknown opcode in auto_possessify()";
    "missing terminating delimiter for callout with string argument";
    "unrecognized string delimiter follows (?C";
    "using \\C is disabled by the application";
    "(?| and/or (?J: or (?x: parentheses are too deeply nested";
    (* 85 *)
    "using \\C is disabled in this PCRE2 library";
    "regular expression is too complicated";
    "lookbehind assertion is too long";
    "pattern string is longer than the limit set by the application";
    "internal error: unknown code in parsed pattern";
    (* 90 *)
    "internal error: bad code value in parsed_skip()";
    "PCRE2_EXTRA_ALLOW_SURROGATE_ESCAPES is not allowed in UTF-16 mode";
    "invalid option bits with PCRE2_LITERAL";
    "\\N{U+dddd} is supported only in Unicode (UTF) mode";
    "invalid hyphen in option setting";
    (* 95 *)
    "(*alpha_assertion) not recognized";
    "script runs require Unicode support, which this version of PCRE2 does not \
     have";
    "too many capturing groups (maximum 65535)";
    "atomic assertion expected after (?( or (?(?C)";
    "\\K is not allowed in lookarounds (but see \
     PCRE2_EXTRA_ALLOW_LOOKAROUND_BSK)";
    (* 100 *)
    "branch too long in variable-length lookbehind assertion";
    "compiled pattern would be longer than the limit set by the application";
  |]

(* pcre2_error.c:197-279 — match-time and UTF error texts, in the same
   format. Index n holds the text for error number -n (codes 0..-67). *)
let match_error_texts =
  [|
    "no error";
    "no match";
    "partial match";
    "UTF-8 error: 1 byte missing at end";
    "UTF-8 error: 2 bytes missing at end";
    (* 5 *)
    "UTF-8 error: 3 bytes missing at end";
    "UTF-8 error: 4 bytes missing at end";
    "UTF-8 error: 5 bytes missing at end";
    "UTF-8 error: byte 2 top bits not 0x80";
    "UTF-8 error: byte 3 top bits not 0x80";
    (* 10 *)
    "UTF-8 error: byte 4 top bits not 0x80";
    "UTF-8 error: byte 5 top bits not 0x80";
    "UTF-8 error: byte 6 top bits not 0x80";
    "UTF-8 error: 5-byte character is not allowed (RFC 3629)";
    "UTF-8 error: 6-byte character is not allowed (RFC 3629)";
    (* 15 *)
    "UTF-8 error: code points greater than 0x10ffff are not defined";
    "UTF-8 error: code points 0xd800-0xdfff are not defined";
    "UTF-8 error: overlong 2-byte sequence";
    "UTF-8 error: overlong 3-byte sequence";
    "UTF-8 error: overlong 4-byte sequence";
    (* 20 *)
    "UTF-8 error: overlong 5-byte sequence";
    "UTF-8 error: overlong 6-byte sequence";
    "UTF-8 error: isolated byte with 0x80 bit set";
    "UTF-8 error: illegal byte (0xfe or 0xff)";
    "UTF-16 error: missing low surrogate at end";
    (* 25 *)
    "UTF-16 error: invalid low surrogate";
    "UTF-16 error: isolated low surrogate";
    "UTF-32 error: code points 0xd800-0xdfff are not defined";
    "UTF-32 error: code points greater than 0x10ffff are not defined";
    "bad data value";
    (* 30 *)
    "patterns do not all use the same character tables";
    "magic number missing";
    "pattern compiled in wrong mode: 8/16/32-bit error";
    "bad offset value";
    "bad option value";
    (* 35 *)
    "invalid replacement string";
    "bad offset into UTF string";
    "callout error code";
    (* Never returned by PCRE2 itself *)
    "invalid data in workspace for DFA restart";
    "too much recursion for DFA matching";
    (* 40 *)
    "backreference condition or recursion test is not supported for DFA \
     matching";
    "function is not supported for DFA matching";
    "pattern contains an item that is not supported for DFA matching";
    "workspace size exceeded in DFA matching";
    "internal error - pattern overwritten?";
    (* 45 *)
    "bad JIT option";
    "JIT stack limit reached";
    "match limit exceeded";
    "no more memory";
    "unknown substring";
    (* 50 *)
    "non-unique substring name";
    "NULL argument passed with non-zero length";
    "nested recursion at the same subject position";
    "matching depth limit exceeded";
    "requested value is not available";
    (* 55 *)
    "requested value is not set";
    "offset limit set without PCRE2_USE_OFFSET_LIMIT";
    "bad escape sequence in replacement string";
    "expected closing curly bracket in replacement string";
    "bad substitution in replacement string";
    (* 60 *)
    "match with end before start or start moved backwards is not supported";
    "too many replacements (more than INT_MAX)";
    "bad serialized data";
    "heap limit exceeded";
    "invalid syntax";
    (* 65 *)
    "internal error - duplicate substitution match";
    "PCRE2_MATCH_INVALID_UTF is not supported for DFA matching";
    "INTERNAL ERROR: invalid substring offset";
  |]

(* Table-length checks: compile texts cover codes 100..201 (ERR0..ERR101),
   match texts cover codes 0..-67 (through PCRE2_ERROR_INVALIDOFFSET,
   pcre2.h.generic:409). *)
let () = assert (Int.equal (Array.length compile_error_texts) 102)
let () = assert (Int.equal (Array.length match_error_texts) 68)

(* pcre2_error.c:300-343 — pcre2_get_error_message. Error numbers are
   positive for compile-time errors (>= compile_error_base) and negative for
   match-time/UTF errors; the numbers are all distinct. The C function walks
   the \0-separated string n times and returns PCRE2_ERROR_BADDATA (-29)
   when it runs off the end (unknown code) or when the number is in the
   invalid range 0..99; we mirror those BADDATA cases by raising
   Invalid_argument (error paths only — never crosses the match loop). *)
let message (enumber : int) : string =
  if enumber >= compile_error_base then
    (* Compile error: n = enumber - COMPILE_ERROR_BASE. *)
    let n = enumber - compile_error_base in
    if n < Array.length compile_error_texts then compile_error_texts.(n)
    else invalid_arg "Errors.message: bad error code" (* C: BADDATA *)
  else if enumber < 0 then
    (* Match or UTF error: n = -enumber. *)
    let n = -enumber in
    if n < Array.length match_error_texts then match_error_texts.(n)
    else invalid_arg "Errors.message: bad error code" (* C: BADDATA *)
  else
    (* Invalid error number (0..99): C uses an empty message list and
       returns BADDATA. *)
    invalid_arg "Errors.message: bad error code"
