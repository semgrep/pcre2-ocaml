(* PCRE2 numeric constants used by the pcre2test-compatible harness.

   Dev-only, self-contained: values are copied from the vendored
   vendor/pcre2/src/pcre2.h.generic (10.44) with line citations so the
   harness needs no dependency on the oracle bindings' constant tables. *)

(* pcre2.h.generic:105-107 *)
let anchored = 0x80000000
let no_utf_check = 0x40000000
let endanchored = 0x20000000

(* pcre2.h.generic:119-145 — compile-only / compile+match options *)
let allow_empty_class = 0x00000001
let alt_bsux = 0x00000002
let auto_callout = 0x00000004
let caseless = 0x00000008
let dollar_endonly = 0x00000010
let dotall = 0x00000020
let dupnames = 0x00000040
let extended = 0x00000080
let firstline = 0x00000100
let match_unset_backref = 0x00000200
let multiline = 0x00000400
let never_ucp = 0x00000800
let never_utf = 0x00001000
let no_auto_capture = 0x00002000
let no_auto_possess = 0x00004000
let no_dotstar_anchor = 0x00008000
let no_start_optimize = 0x00010000
let ucp = 0x00020000
let ungreedy = 0x00040000
let utf = 0x00080000
let never_backslash_c = 0x00100000
let alt_circumflex = 0x00200000
let alt_verbnames = 0x00400000
let use_offset_limit = 0x00800000
let extended_more = 0x01000000
let literal = 0x02000000
let match_invalid_utf = 0x04000000

(* pcre2.h.generic:149-161 — compile-context extra options *)
let extra_allow_surrogate_escapes = 0x00000001
let extra_bad_escape_is_literal = 0x00000002
let extra_match_word = 0x00000004
let extra_match_line = 0x00000008
let extra_escaped_cr_is_lf = 0x00000010
let extra_alt_bsux = 0x00000020
let extra_allow_lookaround_bsk = 0x00000040
let extra_caseless_restrict = 0x00000080
let extra_ascii_bsd = 0x00000100
let extra_ascii_bss = 0x00000200
let extra_ascii_bsw = 0x00000400
let extra_ascii_posix = 0x00000800
let extra_ascii_digit = 0x00001000

(* pcre2test.c:640-641 *)
let extra_ascii_all =
  extra_ascii_bsd lor extra_ascii_bss lor extra_ascii_bsw lor extra_ascii_posix

(* pcre2.h.generic:176-194 — match-time options *)
let notbol = 0x00000001
let noteol = 0x00000002
let notempty = 0x00000004
let notempty_atstart = 0x00000008
let partial_soft = 0x00000010
let partial_hard = 0x00000020
let dfa_restart = 0x00000040
let dfa_shortest = 0x00000080
let no_jit = 0x00002000
let copy_matched_subject = 0x00004000
let disable_recurseloop_check = 0x00040000

(* pcre2.h.generic:210-215 *)
let newline_cr = 1
let newline_lf = 2
let newline_crlf = 3
let newline_any = 4
let newline_anycrlf = 5
let newline_nul = 6

(* pcre2.h.generic:217-218 *)
let bsr_unicode = 1
let bsr_anycrlf = 2

(* pcre2.h.generic:327-398 — match error codes *)
let error_nomatch = -1
let error_partial = -2
let error_utf8_err1 = -3
let error_utf32_err2 = -28
let error_badutfoffset = -36
let error_nomemory = -48
let error_nosubstring = -49
let error_nouniquesubstring = -50
let error_unavailable = -54
let error_unset = -55

(* pcre2test.c:428-429 — names, index = PCRE2_NEWLINE_* value ("DEFAULT" = 0) *)
let newline_names = [| "DEFAULT"; "CR"; "LF"; "CRLF"; "ANY"; "ANYCRLF"; "NUL" |]
