(* Limits and size constants for the pure-OCaml PCRE2 10.44 port.

   Ported from vendor/pcre2/src/{pcre2_internal.h, pcre2_intmodedep.h,
   pcre2_compile.c} and vendor/pcre2/src/config.h.generic. This port is the
   8-bit library with LINK_SIZE = 2, so all mode-dependent values below are
   the PCRE2_CODE_UNIT_WIDTH == 8 / LINK_SIZE == 2 instantiations. *)

(* config.h.generic:173 — internal link size (bytes used by PUT/GET offsets).
   The 8-bit library with the default configuration uses 2. *)
let link_size = 2

(* pcre2_intmodedep.h:193 — code units used to hold a 16-bit count/offset
   (GET2/PUT2) in 8-bit mode. *)
let imm2_size = 2

(* pcre2_intmodedep.h:110 — maximum size of a compiled pattern, 8-bit mode
   with LINK_SIZE == 2: (1 << 16). *)
let max_pattern_size = 1 lsl 16

(* config.h.generic:218 — maximum length of a subpattern name (code units). *)
let max_name_size = 128

(* config.h.generic:211 — maximum number of named subpatterns. *)
let max_name_count = 10000

(* pcre2_compile.c:147 — maximum number of a capturing group. *)
let max_group_number = 65535

(* pcre2_compile.c:148 — maximum value in a {n,m} repeat quantifier. *)
let max_repeat_count = 65535

(* pcre2_compile.c:149 — REPEAT_UNLIMITED = MAX_REPEAT_COUNT + 1, the
   internal marker for an unbounded repeat. *)
let repeat_unlimited = max_repeat_count + 1

(* config.h.generic:263 — default maximum depth of nested parentheses. *)
let parens_nest_limit = 250

(* config.h.generic:224 — default maximum length, in characters, of the
   branches of a variable-length lookbehind assertion. *)
let max_varlookbehind = 255

(* config.h.generic:190 — default match limit (backtracking ticks). *)
let match_limit = 10_000_000

(* config.h.generic:204 — default match depth limit (a.k.a. DEPTH_LIMIT);
   MATCH_LIMIT_DEPTH defaults to MATCH_LIMIT. *)
let match_limit_depth = match_limit

(* config.h.generic:164 — default heap limit in kibibytes. *)
let heap_limit = 20_000_000

(* pcre2_internal.h:223 — an unsigned 32-bit value that is not a valid
   character; used as a "no character" marker. *)
let notachar = 0xffffffff

(* pcre2_internal.h:227 — the largest valid UTF/Unicode code point. *)
let max_utf_code_point = 0x10ffff

(* pcre2_internal.h:1927 — the largest code point value in non-UTF mode:
   0xffffffffU >> (32 - PCRE2_CODE_UNIT_WIDTH), i.e. 0xff for the 8-bit
   library. *)
let max_non_utf_char = 0xff

(* pcre2_intmodedep.h:213 — maximum length of a "(*MARK)" name, 8-bit mode:
   (1 << 8) - 1. *)
let max_mark = 255

(* pcre2_internal.h:248 — initial size (bytes) of the backtracking frames
   vector allocated for pcre2_match(). *)
let start_frames_size = 20480
