(* Public option bits for the pure-OCaml PCRE2 10.44 port.

   Ported from vendor/pcre2/src/pcre2.h.generic (public option bits) and the
   PUBLIC_* validity masks from vendor/pcre2/src/pcre2_compile.c and
   vendor/pcre2/src/pcre2_match.c. All values are plain OCaml ints; the
   63-bit native int holds the full unsigned 32-bit range. Names drop the
   PCRE2_ prefix and are snake_case. Module kept fully for fidelity. *)

(* pcre2.h.generic:105-107 — option bits that can be passed to
   pcre2_compile(), pcre2_match(), or pcre2_dfa_match(). *)
let anchored = 0x80000000
let no_utf_check = 0x40000000
let endanchored = 0x20000000

(* pcre2.h.generic:119-145 — option bits passed only to pcre2_compile()
   (some also affect JIT compilation and/or execution). *)
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

(* pcre2.h.generic:149-161 — extra compile options (an additional options
   word in the compile context). *)
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

(* pcre2.h.generic:165-168 — options for pcre2_jit_compile(). *)
let jit_complete = 0x00000001
let jit_partial_soft = 0x00000002
let jit_partial_hard = 0x00000004
let jit_invalid_utf = 0x00000100

(* pcre2.h.generic:176-194 — options for pcre2_match(), pcre2_dfa_match(),
   pcre2_jit_match(), and pcre2_substitute(). *)
let notbol = 0x00000001
let noteol = 0x00000002
let notempty = 0x00000004
let notempty_atstart = 0x00000008
let partial_soft = 0x00000010
let partial_hard = 0x00000020
let dfa_restart = 0x00000040 (* pcre2_dfa_match() only *)
let dfa_shortest = 0x00000080 (* pcre2_dfa_match() only *)
let substitute_global = 0x00000100 (* pcre2_substitute() only *)
let substitute_extended = 0x00000200 (* pcre2_substitute() only *)
let substitute_unset_empty = 0x00000400 (* pcre2_substitute() only *)
let substitute_unknown_unset = 0x00000800 (* pcre2_substitute() only *)
let substitute_overflow_length = 0x00001000 (* pcre2_substitute() only *)
let no_jit = 0x00002000 (* not for pcre2_dfa_match() *)
let copy_matched_subject = 0x00004000
let substitute_literal = 0x00008000 (* pcre2_substitute() only *)
let substitute_matched = 0x00010000 (* pcre2_substitute() only *)
let substitute_replacement_only = 0x00020000 (* pcre2_substitute() only *)
let disable_recurseloop_check = 0x00040000 (* not for dfa or jit match *)

(* pcre2_compile.c:769-772 — public options permitted with PCRE2_LITERAL. *)
let public_literal_compile_options =
  anchored lor auto_callout lor caseless lor endanchored lor firstline
  lor literal lor match_invalid_utf lor no_start_optimize lor no_utf_check
  lor use_offset_limit lor utf

(* pcre2_compile.c:774-781 — all public options for pcre2_compile();
   anything outside this mask is error 117 (BAD_OPTIONS). *)
let public_compile_options =
  public_literal_compile_options lor allow_empty_class lor alt_bsux
  lor alt_circumflex lor alt_verbnames lor dollar_endonly lor dotall
  lor dupnames lor extended lor extended_more lor match_unset_backref
  lor multiline lor never_backslash_c lor never_ucp lor never_utf
  lor no_auto_capture lor no_auto_possess lor no_dotstar_anchor lor ucp
  lor ungreedy

(* pcre2_compile.c:783-784 — extra options permitted with PCRE2_LITERAL. *)
let public_literal_compile_extra_options =
  extra_match_line lor extra_match_word lor extra_caseless_restrict

(* pcre2_compile.c:786-792 — all public extra options for pcre2_compile(). *)
let public_compile_extra_options =
  public_literal_compile_extra_options lor extra_allow_surrogate_escapes
  lor extra_bad_escape_is_literal lor extra_escaped_cr_is_lf
  lor extra_alt_bsux lor extra_allow_lookaround_bsk lor extra_ascii_bsd
  lor extra_ascii_bss lor extra_ascii_bsw lor extra_ascii_posix
  lor extra_ascii_digit

(* pcre2_match.c:73-77 — public options permitted at match time; anything
   outside this mask is PCRE2_ERROR_BADOPTION (-34). *)
let public_match_options =
  anchored lor endanchored lor notbol lor noteol lor notempty
  lor notempty_atstart lor no_utf_check lor partial_hard lor partial_soft
  lor no_jit lor copy_matched_subject lor disable_recurseloop_check

(* pcre2_match.c:79-82 — public options permitted for the JIT fast path. *)
let public_jit_match_options =
  no_utf_check lor notbol lor noteol lor notempty lor notempty_atstart
  lor partial_soft lor partial_hard lor copy_matched_subject

(* Bindings boundary: option words arrive as int32; convert once here to the
   unsigned 32-bit range in a native int (port-conventions.md section 3). *)
let of_int32 (o : int32) : int = Int32.to_int o land 0xffffffff
