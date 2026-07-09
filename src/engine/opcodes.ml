(* Compiled-pattern opcodes for the pure-OCaml PCRE2 10.44 port.

   Ported from vendor/pcre2/src/pcre2_internal.h. The C enum is sequential
   from 0; the numeric values here must match it exactly, since compiled
   patterns index tables by opcode. This port is the 8-bit library, so the
   OP_LENGTHS entries below are the PCRE2_CODE_UNIT_WIDTH == 8 expansions
   (IMM2_SIZE = 2, LINK_SIZE = 2, 32/sizeof(PCRE2_UCHAR) = 32). *)

(* pcre2_internal.h:1392-1661 — the opcode enum. Values in comments are the
   C enum values; starting from 1 (after OP_END) the values up to OP_EOD
   correspond in order to the ESC_* escape list (pcre2_internal.h:1361-1364). *)

let op_end = 0 (* End of pattern *)

(* Values corresponding to backslashed metacharacters *)
let op_sod = 1 (* Start of data: \A *)
let op_som = 2 (* Start of match (subject + offset): \G *)
let op_set_som = 3 (* Set start of match (\K) *)
let op_not_word_boundary = 4 (* \B -- see also op_not_ucp_word_boundary *)
let op_word_boundary = 5 (* \b -- see also op_ucp_word_boundary *)
let op_not_digit = 6 (* \D *)
let op_digit = 7 (* \d *)
let op_not_whitespace = 8 (* \S *)
let op_whitespace = 9 (* \s *)
let op_not_wordchar = 10 (* \W *)
let op_wordchar = 11 (* \w *)
let op_any = 12 (* Match any character except newline (\N) *)
let op_allany = 13 (* Match any character *)
let op_anybyte = 14 (* Match any byte (\C); different to op_any for UTF-8 *)
let op_notprop = 15 (* \P (not Unicode property) *)
let op_prop = 16 (* \p (Unicode property) *)
let op_anynl = 17 (* \R (any newline sequence) *)
let op_not_hspace = 18 (* \H (not horizontal whitespace) *)
let op_hspace = 19 (* \h (horizontal whitespace) *)
let op_not_vspace = 20 (* \V (not vertical whitespace) *)
let op_vspace = 21 (* \v (vertical whitespace) *)
let op_extuni = 22 (* \X (extended Unicode sequence) *)
let op_eodn = 23 (* End of data or \n at end of data (\Z) *)
let op_eod = 24 (* End of data (\z) *)

(* Line end assertions *)
let op_doll = 25 (* End of line - not multiline *)
let op_dollm = 26 (* End of line - multiline *)
let op_circ = 27 (* Start of line - not multiline *)
let op_circm = 28 (* Start of line - multiline *)

(* Single characters; caseful must precede the caseless ones *)
let op_char = 29 (* Match one character, casefully *)
let op_chari = 30 (* Match one character, caselessly *)
let op_not = 31 (* Match one character, not the given one, casefully *)
let op_noti = 32 (* Match one character, not the given one, caselessly *)

(* Repeated characters; caseful must precede the caseless ones. Each set of
   13 opcodes below must stay in step: the offset from the first one is used
   to generate the others (pcre2_internal.h:1438-1439). *)
let op_star = 33
let op_minstar = 34
let op_plus = 35
let op_minplus = 36
let op_query = 37
let op_minquery = 38
let op_upto = 39 (* From 0 to n matches of one character, caseful *)
let op_minupto = 40
let op_exact = 41 (* Exactly n matches *)
let op_posstar = 42 (* Possessified star, caseful *)
let op_posplus = 43 (* Possessified plus, caseful *)
let op_posquery = 44 (* Possessified query, caseful *)
let op_posupto = 45 (* Possessified upto, caseful *)

(* Repeated characters; caseless must follow the caseful ones *)
let op_stari = 46
let op_minstari = 47
let op_plusi = 48
let op_minplusi = 49
let op_queryi = 50
let op_minqueryi = 51
let op_uptoi = 52 (* From 0 to n matches of one character, caseless *)
let op_minuptoi = 53
let op_exacti = 54
let op_posstari = 55 (* Possessified star, caseless *)
let op_posplusi = 56 (* Possessified plus, caseless *)
let op_posqueryi = 57 (* Possessified query, caseless *)
let op_posuptoi = 58 (* Possessified upto, caseless *)

(* Negated repeated character, caseful; must precede the caseless ones *)
let op_notstar = 59
let op_notminstar = 60
let op_notplus = 61
let op_notminplus = 62
let op_notquery = 63
let op_notminquery = 64
let op_notupto = 65 (* From 0 to n matches, caseful *)
let op_notminupto = 66
let op_notexact = 67 (* Exactly n matches *)
let op_notposstar = 68 (* Possessified versions, caseful *)
let op_notposplus = 69
let op_notposquery = 70
let op_notposupto = 71

(* Negated repeated character, caseless; must follow the caseful ones *)
let op_notstari = 72
let op_notminstari = 73
let op_notplusi = 74
let op_notminplusi = 75
let op_notqueryi = 76
let op_notminqueryi = 77
let op_notuptoi = 78 (* From 0 to n matches, caseless *)
let op_notminuptoi = 79
let op_notexacti = 80 (* Exactly n matches *)
let op_notposstari = 81 (* Possessified versions, caseless *)
let op_notposplusi = 82
let op_notposqueryi = 83
let op_notposuptoi = 84

(* Character types *)
let op_typestar = 85
let op_typeminstar = 86
let op_typeplus = 87
let op_typeminplus = 88
let op_typequery = 89
let op_typeminquery = 90
let op_typeupto = 91 (* From 0 to n matches *)
let op_typeminupto = 92
let op_typeexact = 93 (* Exactly n matches *)
let op_typeposstar = 94 (* Possessified versions *)
let op_typeposplus = 95
let op_typeposquery = 96
let op_typeposupto = 97

(* These are used for character classes and back references; only the first
   six are the same as the sets above. *)
let op_crstar = 98
let op_crminstar = 99
let op_crplus = 100
let op_crminplus = 101
let op_crquery = 102
let op_crminquery = 103
let op_crrange = 104 (* These are different to the three sets above *)
let op_crminrange = 105
let op_crposstar = 106 (* Possessified versions *)
let op_crposplus = 107
let op_crposquery = 108
let op_crposrange = 109

(* End of quantifier opcodes *)
let op_class = 110 (* Match a character class, chars < 256 only *)
let op_nclass = 111 (* Same, but bitmap built from a negative class *)
let op_xclass = 112 (* Extended class for handling > 255 chars *)
let op_ref = 113 (* Match a back reference, casefully *)
let op_refi = 114 (* Match a back reference, caselessly *)
let op_dnref = 115 (* Match a duplicate name backref, casefully *)
let op_dnrefi = 116 (* Match a duplicate name backref, caselessly *)
let op_recurse = 117 (* Match a numbered subpattern (possibly recursive) *)
let op_callout = 118 (* Call out to external function if provided *)
let op_callout_str = 119 (* Call out with string argument *)
let op_alt = 120 (* Start of alternation *)
let op_ket = 121 (* End of group that doesn't have an unbounded repeat *)
let op_ketrmax = 122 (* These two must remain together and in this order; *)
let op_ketrmin = 123 (* they are for groups that repeat for ever. *)
let op_ketrpos = 124 (* Possessive unlimited repeat *)

(* The assertions must come before BRA, CBRA, ONCE, and COND *)
let op_reverse = 125 (* Move pointer back - used in lookbehind assertions *)
let op_vreverse = 126 (* Move pointer back - variable *)
let op_assert = 127 (* Positive lookahead *)
let op_assert_not = 128 (* Negative lookahead *)
let op_assertback = 129 (* Positive lookbehind *)
let op_assertback_not = 130 (* Negative lookbehind *)
let op_assert_na = 131 (* Positive non-atomic lookahead *)
let op_assertback_na = 132 (* Positive non-atomic lookbehind *)

(* ONCE, SCRIPT_RUN, BRA, BRAPOS, CBRA, CBRAPOS, and COND must come
   immediately after the assertions, with ONCE first (>= ONCE tests for a
   subpattern that isn't an assertion); POS versions immediately follow the
   non-POS versions. *)
let op_once = 133 (* Atomic group, contains captures *)
let op_script_run = 134 (* Non-capture, but check characters' scripts *)
let op_bra = 135 (* Start of non-capturing bracket *)
let op_brapos = 136 (* Ditto, with unlimited, possessive repeat *)
let op_cbra = 137 (* Start of capturing bracket *)
let op_cbrapos = 138 (* Ditto, with unlimited, possessive repeat *)
let op_cond = 139 (* Conditional group *)

(* These five must follow the previous five, in the same order (>= SBRA
   distinguishes the two sets). *)
let op_sbra = 140 (* Start of non-capturing bracket, check empty *)
let op_sbrapos = 141 (* Ditto, with unlimited, possessive repeat *)
let op_scbra = 142 (* Start of capturing bracket, check empty *)
let op_scbrapos = 143 (* Ditto, with unlimited, possessive repeat *)
let op_scond = 144 (* Conditional group, check empty *)

(* The next two pairs must (respectively) be kept together *)
let op_cref = 145 (* Used to hold a capture number as condition *)
let op_dncref = 146 (* Used to point to duplicate names as a condition *)
let op_rref = 147 (* Used to hold a recursion number as condition *)
let op_dnrref = 148 (* Used to point to duplicate names as a condition *)
let op_false = 149 (* Always false (used by DEFINE and VERSION) *)
let op_true = 150 (* Always true (used by VERSION) *)
let op_brazero = 151 (* These two must remain together and in this order *)
let op_braminzero = 152
let op_braposzero = 153

(* Backtracking control verbs *)
let op_mark = 154 (* always has an argument *)
let op_prune = 155
let op_prune_arg = 156 (* same, but with argument *)
let op_skip = 157
let op_skip_arg = 158 (* same, but with argument *)
let op_then = 159
let op_then_arg = 160 (* same, but with argument *)
let op_commit = 161
let op_commit_arg = 162 (* same, but with argument *)

(* Forced failure and success verbs *)
let op_fail = 163
let op_accept = 164
let op_assert_accept = 165 (* Used inside assertions *)
let op_close = 166 (* Used before op_accept to close open captures *)

(* This is used to skip a subpattern with a {0} quantifier *)
let op_skipzero = 167

(* Identifies a DEFINE group during compilation; changed to op_false before
   compilation finishes. *)
let op_define = 168

(* These opcodes replace their normal counterparts in UCP mode when
   PCRE2_EXTRA_ASCII_BSW is not set. *)
let op_not_ucp_word_boundary = 169
let op_ucp_word_boundary = 170

(* pcre2_internal.h:1659 — not an opcode; used to check that tables indexed
   by opcode are the correct length. *)
let op_table_length = 171

(* pcre2_internal.h:1384-1390 — the values between first_autotab_op and
   last_autotab_right_op, inclusive, are used in a table for deciding whether
   a repeated character type can be auto-possessified. *)
let first_autotab_op = op_not_digit
let last_autotab_left_op = op_extuni
let last_autotab_right_op = op_dollm

(* pcre2_internal.h:1816 — a magic value for op_rref to indicate the "any
   recursion" condition. *)
let rref_any = 0xffff

(* pcre2_internal.h:1289-1307 — codes for the different types of Unicode
   property (stored with OP_PROP/OP_NOTPROP operands and in META_ESCAPE
   \p/\P data words). If these definitions are changed, the
   autopossessifying table in pcre2_auto_possess.c must be updated to
   match. *)
let pt_any = 0 (* Any property - matches all chars *)
let pt_lamp = 1 (* L& - the union of Lu, Ll, Lt *)
let pt_gc = 2 (* Specified general characteristic (e.g. L) *)
let pt_pc = 3 (* Specified particular characteristic (e.g. Lu) *)
let pt_sc = 4 (* Script only (e.g. Han) *)
let pt_scx = 5 (* Script extensions (includes SC) *)
let pt_alnum = 6 (* Alphanumeric - the union of L and N *)
let pt_space = 7 (* Perl space - general category Z plus 9,10,12,13 *)
let pt_pxspace = 8 (* POSIX space - Z plus 9,10,11,12,13 *)
let pt_word = 9 (* Word - L, N, Mn, or Pc *)
let pt_clist = 10 (* Pseudo-property: match character list *)
let pt_ucnc = 11 (* Universal Character nameable character *)
let pt_bidicl = 12 (* Specified bidi class *)
let pt_bool = 13 (* Boolean property *)
let pt_tabsize = 14 (* Size of square table for autopossessify tests *)

(* pcre2_internal.h:1309-1320 — special properties used only in XCLASS
   items when POSIX classes are specified and PCRE2_UCP is set. They are not
   available via \p or \P. *)
let pt_pxgraph = 14 (* [:graph:] - characters that mark the paper *)
let pt_pxprint = 15 (* [:print:] - [:graph:] plus non-control spaces *)
let pt_pxpunct = 16 (* [:punct:] - punctuation characters *)
let pt_pxxdigit = 17 (* [:xdigit:] - hex digits *)

(* pcre2_internal.h:1674-1714 — OP_NAME_LIST: textual names for all the
   opcodes, used only for debugging (pcre2_printint.c fills out the full
   names in many cases). Indexed by opcode. *)
let op_names =
  [|
    "End";
    "\\A";
    "\\G";
    "\\K";
    "\\B";
    "\\b";
    "\\D";
    "\\d";
    "\\S";
    "\\s";
    "\\W";
    "\\w";
    "Any";
    "AllAny";
    "Anybyte";
    "notprop";
    "prop";
    "\\R";
    "\\H";
    "\\h";
    "\\V";
    "\\v";
    "extuni";
    "\\Z";
    "\\z";
    "$";
    "$";
    "^";
    "^";
    "char";
    "chari";
    "not";
    "noti";
    "*";
    "*?";
    "+";
    "+?";
    "?";
    "??";
    "{";
    "{";
    "{";
    "*+";
    "++";
    "?+";
    "{";
    "*";
    "*?";
    "+";
    "+?";
    "?";
    "??";
    "{";
    "{";
    "{";
    "*+";
    "++";
    "?+";
    "{";
    "*";
    "*?";
    "+";
    "+?";
    "?";
    "??";
    "{";
    "{";
    "{";
    "*+";
    "++";
    "?+";
    "{";
    "*";
    "*?";
    "+";
    "+?";
    "?";
    "??";
    "{";
    "{";
    "{";
    "*+";
    "++";
    "?+";
    "{";
    "*";
    "*?";
    "+";
    "+?";
    "?";
    "??";
    "{";
    "{";
    "{";
    "*+";
    "++";
    "?+";
    "{";
    "*";
    "*?";
    "+";
    "+?";
    "?";
    "??";
    "{";
    "{";
    "*+";
    "++";
    "?+";
    "{";
    "class";
    "nclass";
    "xclass";
    "Ref";
    "Refi";
    "DnRef";
    "DnRefi";
    "Recurse";
    "Callout";
    "CalloutStr";
    "Alt";
    "Ket";
    "KetRmax";
    "KetRmin";
    "KetRpos";
    "Reverse";
    "VReverse";
    "Assert";
    "Assert not";
    "Assert back";
    "Assert back not";
    "Non-atomic assert";
    "Non-atomic assert back";
    "Once";
    "Script run";
    "Bra";
    "BraPos";
    "CBra";
    "CBraPos";
    "Cond";
    "SBra";
    "SBraPos";
    "SCBra";
    "SCBraPos";
    "SCond";
    "Cond ref";
    "Cond dnref";
    "Cond rec";
    "Cond dnrec";
    "Cond false";
    "Cond true";
    "Brazero";
    "Braminzero";
    "Braposzero";
    "*MARK";
    "*PRUNE";
    "*PRUNE";
    "*SKIP";
    "*SKIP";
    "*THEN";
    "*THEN";
    "*COMMIT";
    "*COMMIT";
    "*FAIL";
    "*ACCEPT";
    "*ASSERT_ACCEPT";
    "Close";
    "Skip zero";
    "Define";
    "\\B (ucp)";
    "\\b (ucp)";
  |]

(* pcre2_internal.h:1726-1812 — OP_LENGTHS: the length of fixed-length
   operations in the compiled pattern, indexed by opcode; 0 for the
   variable-length XCLASS and CALLOUT_STR, and minima for opcodes whose
   length can grow in UTF-8 mode. Values are the 8-bit-mode expansions:
   IMM2_SIZE = 2 (pcre2_intmodedep.h:193), LINK_SIZE = 2
   (config.h.generic:173), 32/sizeof(PCRE2_UCHAR) = 32. *)
let op_lengths =
  [|
    1;
    (* End                                    *)
    1;
    1;
    1;
    1;
    1;
    (* \A, \G, \K, \B, \b                     *)
    1;
    1;
    1;
    1;
    1;
    1;
    (* \D, \d, \S, \s, \W, \w                 *)
    1;
    1;
    1;
    (* Any, AllAny, Anybyte                   *)
    3;
    3;
    (* \P, \p                                 *)
    1;
    1;
    1;
    1;
    1;
    (* \R, \H, \h, \V, \v                     *)
    1;
    (* \X                                     *)
    1;
    1;
    1;
    1;
    1;
    1;
    (* \Z, \z, $, $M ^, ^M                    *)
    2;
    (* Char  - the minimum length             *)
    2;
    (* Chari - the minimum length             *)
    2;
    (* not                                    *)
    2;
    (* noti                                   *)
    (* Positive single-char repeats; minima in UTF-8 mode *)
    2;
    2;
    2;
    2;
    2;
    2;
    (* *, *?, +, +?, ?, ??                    *)
    4;
    4;
    (* upto, minupto            2+IMM2_SIZE   *)
    4;
    (* exact                    2+IMM2_SIZE   *)
    2;
    2;
    2;
    4;
    (* *+, ++, ?+, upto+                      *)
    2;
    2;
    2;
    2;
    2;
    2;
    (* *I, *?I, +I, +?I, ?I, ??I              *)
    4;
    4;
    (* upto I, minupto I                      *)
    4;
    (* exact I                                *)
    2;
    2;
    2;
    4;
    (* *+I, ++I, ?+I, upto+I                  *)
    (* Negative single-char repeats - only for chars < 256 *)
    2;
    2;
    2;
    2;
    2;
    2;
    (* NOT *, *?, +, +?, ?, ??                *)
    4;
    4;
    (* NOT upto, minupto                      *)
    4;
    (* NOT exact                              *)
    2;
    2;
    2;
    4;
    (* Possessive NOT *, +, ?, upto           *)
    2;
    2;
    2;
    2;
    2;
    2;
    (* NOT *I, *?I, +I, +?I, ?I, ??I          *)
    4;
    4;
    (* NOT upto I, minupto I                  *)
    4;
    (* NOT exact I                            *)
    2;
    2;
    2;
    4;
    (* Possessive NOT *I, +I, ?I, upto I      *)
    (* Positive type repeats *)
    2;
    2;
    2;
    2;
    2;
    2;
    (* Type *, *?, +, +?, ?, ??               *)
    4;
    4;
    (* Type upto, minupto                     *)
    4;
    (* Type exact                             *)
    2;
    2;
    2;
    4;
    (* Possessive *+, ++, ?+, upto+           *)
    (* Character class & ref repeats *)
    1;
    1;
    1;
    1;
    1;
    1;
    (* *, *?, +, +?, ?, ??                    *)
    5;
    5;
    (* CRRANGE, CRMINRANGE      1+2*IMM2_SIZE *)
    1;
    1;
    1;
    5;
    (* Possessive *+, ++, ?+, CRPOSRANGE      *)
    33;
    (* CLASS                    1+(32/1)      *)
    33;
    (* NCLASS                                 *)
    0;
    (* XCLASS - variable length               *)
    3;
    (* REF                      1+IMM2_SIZE   *)
    3;
    (* REFI                                   *)
    5;
    (* DNREF                    1+2*IMM2_SIZE *)
    5;
    (* DNREFI                                 *)
    3;
    (* RECURSE                  1+LINK_SIZE   *)
    6;
    (* CALLOUT                  1+2*LINK_SIZE+1 *)
    0;
    (* CALLOUT_STR - variable length          *)
    3;
    (* Alt                      1+LINK_SIZE   *)
    3;
    (* Ket                                    *)
    3;
    (* KetRmax                                *)
    3;
    (* KetRmin                                *)
    3;
    (* KetRpos                                *)
    3;
    (* Reverse                  1+IMM2_SIZE   *)
    5;
    (* VReverse                 1+2*IMM2_SIZE *)
    3;
    (* Assert                   1+LINK_SIZE   *)
    3;
    (* Assert not                             *)
    3;
    (* Assert behind                          *)
    3;
    (* Assert behind not                      *)
    3;
    (* NA Assert                              *)
    3;
    (* NA Assert behind                       *)
    3;
    (* ONCE                                   *)
    3;
    (* SCRIPT_RUN                             *)
    3;
    (* BRA                                    *)
    3;
    (* BRAPOS                                 *)
    5;
    (* CBRA                     1+LINK_SIZE+IMM2_SIZE *)
    5;
    (* CBRAPOS                                *)
    3;
    (* COND                                   *)
    3;
    (* SBRA                                   *)
    3;
    (* SBRAPOS                                *)
    5;
    (* SCBRA                                  *)
    5;
    (* SCBRAPOS                               *)
    3;
    (* SCOND                                  *)
    3;
    5;
    (* CREF, DNCREF                           *)
    3;
    5;
    (* RREF, DNRREF                           *)
    1;
    1;
    (* FALSE, TRUE                            *)
    1;
    1;
    1;
    (* BRAZERO, BRAMINZERO, BRAPOSZERO        *)
    3;
    1;
    3;
    (* MARK, PRUNE, PRUNE_ARG                 *)
    1;
    3;
    (* SKIP, SKIP_ARG                         *)
    1;
    3;
    (* THEN, THEN_ARG                         *)
    1;
    3;
    (* COMMIT, COMMIT_ARG                     *)
    1;
    1;
    1;
    (* FAIL, ACCEPT, ASSERT_ACCEPT            *)
    3;
    1;
    (* CLOSE, SKIPZERO                        *)
    1;
    (* DEFINE                                 *)
    1;
    1;
    (* \B and \b in UCP mode                  *)
  |]

(* Consistency checks mirroring the C's use of OP_TABLE_LENGTH to catch
   updating errors in tables indexed by opcode. *)
let () = assert (Int.equal op_ucp_word_boundary (op_table_length - 1))
let () = assert (Int.equal (Array.length op_names) op_table_length)
let () = assert (Int.equal (Array.length op_lengths) op_table_length)
