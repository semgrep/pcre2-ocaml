(* Engine module-initialization asserts for [Pcre2_engine.Interpreter], migrated
   verbatim from src/engine/interpreter.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Interpreter

(* Pin the dispatch-arm int literals that guard live behavior against the
   Opcodes constants (the stub groups are attribution-only: every stub
   returns the same marker). *)
let test_0 () =
  assert (Int.equal Opcodes.op_end 0);
  assert (Int.equal Opcodes.op_char 29);
  assert (Int.equal Opcodes.op_accept 164);
  assert (Int.equal Opcodes.op_close 166);
  (* Char-family opcodes now guarding live behavior (chars + char repeats
     chunk). *)
  assert (Int.equal Opcodes.op_chari 30);
  assert (Int.equal Opcodes.op_not 31);
  assert (Int.equal Opcodes.op_noti 32);
  assert (Int.equal Opcodes.op_star 33);
  assert (Int.equal Opcodes.op_minstar 34);
  assert (Int.equal Opcodes.op_plus 35);
  assert (Int.equal Opcodes.op_minplus 36);
  assert (Int.equal Opcodes.op_query 37);
  assert (Int.equal Opcodes.op_minquery 38);
  assert (Int.equal Opcodes.op_upto 39);
  assert (Int.equal Opcodes.op_minupto 40);
  assert (Int.equal Opcodes.op_exact 41);
  assert (Int.equal Opcodes.op_posstar 42);
  assert (Int.equal Opcodes.op_posplus 43);
  assert (Int.equal Opcodes.op_posquery 44);
  assert (Int.equal Opcodes.op_posupto 45);
  assert (Int.equal Opcodes.op_stari 46);
  assert (Int.equal Opcodes.op_minstari 47);
  assert (Int.equal Opcodes.op_plusi 48);
  assert (Int.equal Opcodes.op_minplusi 49);
  assert (Int.equal Opcodes.op_queryi 50);
  assert (Int.equal Opcodes.op_minqueryi 51);
  assert (Int.equal Opcodes.op_uptoi 52);
  assert (Int.equal Opcodes.op_minuptoi 53);
  assert (Int.equal Opcodes.op_exacti 54);
  assert (Int.equal Opcodes.op_posstari 55);
  assert (Int.equal Opcodes.op_posplusi 56);
  assert (Int.equal Opcodes.op_posqueryi 57);
  assert (Int.equal Opcodes.op_posuptoi 58);
  assert (Int.equal Opcodes.op_notstar 59);
  assert (Int.equal Opcodes.op_notminstar 60);
  assert (Int.equal Opcodes.op_notplus 61);
  assert (Int.equal Opcodes.op_notminplus 62);
  assert (Int.equal Opcodes.op_notquery 63);
  assert (Int.equal Opcodes.op_notminquery 64);
  assert (Int.equal Opcodes.op_notupto 65);
  assert (Int.equal Opcodes.op_notminupto 66);
  assert (Int.equal Opcodes.op_notexact 67);
  assert (Int.equal Opcodes.op_notposstar 68);
  assert (Int.equal Opcodes.op_notposplus 69);
  assert (Int.equal Opcodes.op_notposquery 70);
  assert (Int.equal Opcodes.op_notposupto 71);
  assert (Int.equal Opcodes.op_notstari 72);
  assert (Int.equal Opcodes.op_notminstari 73);
  assert (Int.equal Opcodes.op_notplusi 74);
  assert (Int.equal Opcodes.op_notminplusi 75);
  assert (Int.equal Opcodes.op_notqueryi 76);
  assert (Int.equal Opcodes.op_notminqueryi 77);
  assert (Int.equal Opcodes.op_notuptoi 78);
  assert (Int.equal Opcodes.op_notminuptoi 79);
  assert (Int.equal Opcodes.op_notexacti 80);
  assert (Int.equal Opcodes.op_notposstari 81);
  assert (Int.equal Opcodes.op_notposplusi 82);
  assert (Int.equal Opcodes.op_notposqueryi 83);
  assert (Int.equal Opcodes.op_notposuptoi 84);
  (* Class / typed-repeat / anchor opcodes now guarding live behavior
     (classes + typed repeats and anchors chunks). *)
  assert (Int.equal Opcodes.op_sod 1);
  assert (Int.equal Opcodes.op_som 2);
  assert (Int.equal Opcodes.op_set_som 3);
  assert (Int.equal Opcodes.op_not_word_boundary 4);
  assert (Int.equal Opcodes.op_word_boundary 5);
  assert (Int.equal Opcodes.op_not_digit 6);
  assert (Int.equal Opcodes.op_digit 7);
  assert (Int.equal Opcodes.op_not_whitespace 8);
  assert (Int.equal Opcodes.op_whitespace 9);
  assert (Int.equal Opcodes.op_not_wordchar 10);
  assert (Int.equal Opcodes.op_wordchar 11);
  assert (Int.equal Opcodes.op_any 12);
  assert (Int.equal Opcodes.op_allany 13);
  assert (Int.equal Opcodes.op_anybyte 14);
  assert (Int.equal Opcodes.op_notprop 15);
  assert (Int.equal Opcodes.op_prop 16);
  assert (Int.equal Opcodes.op_anynl 17);
  assert (Int.equal Opcodes.op_not_hspace 18);
  assert (Int.equal Opcodes.op_hspace 19);
  assert (Int.equal Opcodes.op_not_vspace 20);
  assert (Int.equal Opcodes.op_vspace 21);
  assert (Int.equal Opcodes.op_extuni 22);
  assert (Int.equal Opcodes.op_eodn 23);
  assert (Int.equal Opcodes.op_eod 24);
  assert (Int.equal Opcodes.op_doll 25);
  assert (Int.equal Opcodes.op_dollm 26);
  assert (Int.equal Opcodes.op_circ 27);
  assert (Int.equal Opcodes.op_circm 28);
  assert (Int.equal Opcodes.op_typestar 85);
  assert (Int.equal Opcodes.op_typeminstar 86);
  assert (Int.equal Opcodes.op_typeplus 87);
  assert (Int.equal Opcodes.op_typeminplus 88);
  assert (Int.equal Opcodes.op_typequery 89);
  assert (Int.equal Opcodes.op_typeminquery 90);
  assert (Int.equal Opcodes.op_typeupto 91);
  assert (Int.equal Opcodes.op_typeminupto 92);
  assert (Int.equal Opcodes.op_typeexact 93);
  assert (Int.equal Opcodes.op_typeposstar 94);
  assert (Int.equal Opcodes.op_typeposplus 95);
  assert (Int.equal Opcodes.op_typeposquery 96);
  assert (Int.equal Opcodes.op_typeposupto 97);
  assert (Int.equal Opcodes.op_crminquery 103);
  assert (Int.equal Opcodes.op_crrange 104);
  assert (Int.equal Opcodes.op_crminrange 105);
  assert (Int.equal Opcodes.op_crposstar 106);
  assert (Int.equal Opcodes.op_crposquery 108);
  assert (Int.equal Opcodes.op_class 110);
  assert (Int.equal Opcodes.op_nclass 111);
  (* Backreference opcodes now guarding live behavior (backreferences
     chunk). *)
  assert (Int.equal Opcodes.op_ref 113);
  assert (Int.equal Opcodes.op_refi 114);
  assert (Int.equal Opcodes.op_dnref 115);
  assert (Int.equal Opcodes.op_dnrefi 116);
  assert (Int.equal Opcodes.op_not_ucp_word_boundary 169);
  assert (Int.equal Opcodes.op_ucp_word_boundary 170);
  (* Bracket / alternation / ket opcodes now guarding live behavior
     (brackets chunk), including the *bracode switch literals inside the
     OP_KET arm. *)
  assert (Int.equal Opcodes.op_alt 120);
  assert (Int.equal Opcodes.op_ket 121);
  assert (Int.equal Opcodes.op_ketrmax 122);
  assert (Int.equal Opcodes.op_ketrmin 123);
  assert (Int.equal Opcodes.op_ketrpos 124);
  assert (Int.equal Opcodes.op_assert 127);
  assert (Int.equal Opcodes.op_assert_not 128);
  assert (Int.equal Opcodes.op_assertback 129);
  assert (Int.equal Opcodes.op_assertback_not 130);
  assert (Int.equal Opcodes.op_assert_na 131);
  assert (Int.equal Opcodes.op_assertback_na 132);
  assert (Int.equal Opcodes.op_once 133);
  assert (Int.equal Opcodes.op_script_run 134);
  assert (Int.equal Opcodes.op_bra 135);
  assert (Int.equal Opcodes.op_brapos 136);
  assert (Int.equal Opcodes.op_cbra 137);
  assert (Int.equal Opcodes.op_cbrapos 138);
  assert (Int.equal Opcodes.op_cond 139);
  assert (Int.equal Opcodes.op_sbra 140);
  assert (Int.equal Opcodes.op_sbrapos 141);
  assert (Int.equal Opcodes.op_scbra 142);
  assert (Int.equal Opcodes.op_scbrapos 143);
  assert (Int.equal Opcodes.op_scond 144);
  assert (Int.equal Opcodes.op_brazero 151);
  assert (Int.equal Opcodes.op_braminzero 152);
  assert (Int.equal Opcodes.op_skipzero 167);
  (* Lookaround / atomic / possessive opcodes now guarding live behavior
     (lookaround-atomic-possessive chunk). *)
  assert (Int.equal Opcodes.op_reverse 125);
  assert (Int.equal Opcodes.op_vreverse 126);
  assert (Int.equal Opcodes.op_braposzero 153);
  assert (Int.equal Opcodes.op_assert_accept 165);
  (* OP_lengths entries the bracket arms step by (pcre2_tables.c OP_lengths
     via Opcodes.op_lengths): 1+LINK_SIZE for BRA/ALT-class items,
     1+LINK_SIZE+IMM2_SIZE for the capturing brackets. *)
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_bra) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_alt) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_sbra) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_once) (1 + Limits.link_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_script_run) (1 + Limits.link_size));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_cbra)
      (1 + Limits.link_size + Limits.imm2_size));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_cbra)
      Opcodes.op_lengths.(Opcodes.op_cbrapos));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_scbra)
      (1 + Limits.link_size + Limits.imm2_size));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_scbrapos)
      (1 + Limits.link_size + Limits.imm2_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_brapos) (1 + Limits.link_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_sbrapos) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_assert) (1 + Limits.link_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_assert_not) (1 + Limits.link_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_assertback) (1 + Limits.link_size));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_assertback_not)
      (1 + Limits.link_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_assert_na) (1 + Limits.link_size));
  assert (
    Int.equal
      Opcodes.op_lengths.(Opcodes.op_assertback_na)
      (1 + Limits.link_size));
  (* Newline character constants consumed by the ANY/ANYNL/EODN arms
     (pcre2_internal.h:678-699). *)
  assert (Int.equal Newline.char_lf 0x0a);
  assert (Int.equal Newline.char_vt 0x0b);
  assert (Int.equal Newline.char_ff 0x0c);
  assert (Int.equal Newline.char_cr 0x0d);
  assert (Int.equal Newline.char_nel 0x85);
  (* hspace_byte/vspace_byte agree with HSPACE_LIST/VSPACE_LIST
     (pcre2_tables.c:66-67 via Tables) restricted to code units < 256. *)
  for c = 0 to 255 do
    assert (
      Bool.equal (hspace_byte c)
        (Array.exists (fun v -> Int.equal v c) Tables.hspace_list));
    assert (
      Bool.equal (vspace_byte c)
        (Array.exists (fun v -> Int.equal v c) Tables.vspace_list))
  done;
  (* hspace_char/vspace_char (the UTF loops' full HSPACE_CASES/
     VSPACE_CASES) agree with the complete lists over every value the
     decoders can produce in that range (the lists' NOTACHAR terminator
     never equals a code point). *)
  for c = 0 to 0x3100 do
    assert (
      Bool.equal (hspace_char c)
        (Array.exists (fun v -> Int.equal v c) Tables.hspace_list));
    assert (
      Bool.equal (vspace_char c)
        (Array.exists (fun v -> Int.equal v c) Tables.vspace_list))
  done;
  (* The rep tables (pcre2_match.c:128-150) and the op-offset indexing
     they are addressed with (pcre2_match.c:1254-1257, 1608-1611): each
     STAR..MINQUERY block is 6 consecutive opcodes. *)
  assert (Int.equal (Array.length rep_min) 11);
  assert (Int.equal (Array.length rep_max) 11);
  assert (Int.equal (Array.length rep_typ) 12);
  assert (Int.equal (Opcodes.op_minquery - Opcodes.op_star) 5);
  assert (Int.equal (Opcodes.op_minqueryi - Opcodes.op_stari) 5);
  assert (Int.equal (Opcodes.op_notminquery - Opcodes.op_notstar) 5);
  assert (Int.equal (Opcodes.op_notminqueryi - Opcodes.op_notstari) 5);
  assert (
    Int.equal reptype_min 0 && Int.equal reptype_max 1
    && Int.equal reptype_pos 2);
  (* The "no top-level case" default group boundaries. *)
  assert (Int.equal Opcodes.op_crstar 98);
  assert (Int.equal Opcodes.op_crposrange 109);
  assert (Int.equal Opcodes.op_cref 145);
  assert (Int.equal Opcodes.op_true 150);
  assert (Int.equal Opcodes.op_define 168);
  assert (Int.equal Opcodes.op_table_length 171);
  (* Conditional / recursion opcodes now guarding live behavior
     (conditionals/recursion match chunk): the condition opcodes read
     inside OP_COND (pcre2_internal.h:1607-1612), OP_RECURSE, OP_FAIL,
     RREF_ANY (pcre2_internal.h:1816), and the OP_lengths entries the
     condition dispatch steps by (pcre2_internal.h:1786-1803). *)
  assert (Int.equal Opcodes.op_recurse 117);
  assert (Int.equal Opcodes.op_dncref 146);
  assert (Int.equal Opcodes.op_rref 147);
  assert (Int.equal Opcodes.op_dnrref 148);
  assert (Int.equal Opcodes.op_false 149);
  assert (Int.equal Opcodes.op_fail 163);
  assert (Int.equal Opcodes.rref_any 0xffff);
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_recurse) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_cond) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_scond) (1 + Limits.link_size));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_cref) (1 + Limits.imm2_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_dncref) (1 + (2 * Limits.imm2_size)));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_rref) (1 + Limits.imm2_size));
  assert (
    Int.equal Opcodes.op_lengths.(Opcodes.op_dnrref) (1 + (2 * Limits.imm2_size)));
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_false) 1);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_true) 1);
  (* Backtracking-verb opcodes now guarding live behavior (verbs match
     chunk), and the OP_lengths entries the verb dispatch and OP_CLOSE
     step by (pcre2_internal.h:1805-1810). *)
  assert (Int.equal Opcodes.op_mark 154);
  assert (Int.equal Opcodes.op_prune 155);
  assert (Int.equal Opcodes.op_prune_arg 156);
  assert (Int.equal Opcodes.op_skip 157);
  assert (Int.equal Opcodes.op_skip_arg 158);
  assert (Int.equal Opcodes.op_then 159);
  assert (Int.equal Opcodes.op_then_arg 160);
  assert (Int.equal Opcodes.op_commit 161);
  assert (Int.equal Opcodes.op_commit_arg 162);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_mark) 3);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_prune) 1);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_prune_arg) 3);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_skip) 1);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_skip_arg) 3);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_then) 1);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_then_arg) 3);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_commit) 1);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_commit_arg) 3);
  assert (Int.equal Opcodes.op_lengths.(Opcodes.op_close) (1 + Limits.imm2_size));
  (* RM label constants match the C enum (pcre2_match.c:155-169). *)
  assert (Int.equal rm1 1);
  assert (Int.equal rm37 37);
  assert (Int.equal rm100 100);
  assert (Int.equal rm101 101);
  assert (Int.equal rm200 200);
  assert (Int.equal rm225 225);
  (* MATCH_xxx internals (pcre2_match.c:87-103). *)
  assert (Int.equal match_match 1);
  assert (Int.equal match_nomatch 0);
  assert (Int.equal match_accept (-999));
  assert (Int.equal match_ketrpos (-998));
  assert (Int.equal match_backtrack_min match_commit);
  assert (Int.equal match_backtrack_max match_then);
  assert (match_backtrack_min < match_prune && match_prune < match_skip);
  assert (match_skip < match_skip_arg && match_skip_arg < match_then);
  (* The unported marker collides with nothing it can meet. *)
  assert (error_unported < -66 && error_unported > match_then)

(* Hand-assembled programs: these predate the brackets chunk and exercise
   OP_CHAR / OP_END directly, without the OP_BRA..OP_KET wrapper the
   compiler always emits (pcre2_compile.c:10570-10604) — still valid
   programs, kept as the minimal drivers of those arms. *)
let mk_code (units : int list) : Bytes.t =
  let b = Bytes.create (List.length units) in
  List.iteri (fun i u -> Bytes.set b i (Char.chr u)) units;
  b

(* OP_CHAR chain + OP_END: the equivalent of /abc/ without the outer
   bracket. *)
let test_1 () =
  let abc =
    mk_code
      [
        Opcodes.op_char;
        Char.code 'a';
        Opcodes.op_char;
        Char.code 'b';
        Opcodes.op_char;
        Char.code 'c';
        Opcodes.op_end;
      ]
  in
  (* Anchored attempt at offset 1 of "xabc": MATCH_MATCH with ovector
     (1, 4). *)
  (match match_internal ~code:abc ~top_bracket:0 "xabc" 1 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 1);
      assert (Int.equal ov.(1) 4));
  (* "abd": the third OP_CHAR consults 'd', fails, backtracks to frame 0
     -> MATCH_NOMATCH; the ovector is untouched. *)
  (match match_internal ~code:abc ~top_bracket:0 "abd" 0 with
  | rc, ov ->
      assert (Int.equal rc match_nomatch);
      assert (Int.equal ov.(0) Frames.unset);
      assert (Int.equal ov.(1) Frames.unset));
  (* Attempt at an offset where the subject runs out: plain NOMATCH
     without partial flags... *)
  (match match_internal ~code:abc ~top_bracket:0 "xab" 1 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* ...soft partial: SCHECK_PARTIAL sets hitend and falls through to
     NOMATCH... *)
  (match
     match_internal ~moptions:Options.partial_soft ~code:abc ~top_bracket:0
       "xab" 1
   with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* ...hard partial: SCHECK_PARTIAL returns PCRE2_ERROR_PARTIAL from the
     match (pcre2_match.c:542). *)
  (match
     match_internal ~moptions:Options.partial_hard ~code:abc ~top_bracket:0
       "xab" 1
   with
  | rc, _ -> assert (Int.equal rc Errors.error_partial));
  (* ENDANCHORED (pattern option): /abc/ matches "abcx" up to eptr 3 <
     end_subject 4, so OP_END backtracks -> NOMATCH; exact-length subject
     still matches. *)
  (match
     match_internal ~poptions:Options.endanchored ~code:abc ~top_bracket:0
       "abcx" 0
   with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  match
    match_internal ~moptions:Options.endanchored ~code:abc ~top_bracket:0 "abc"
      0
  with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(1) 3)

(* Bare OP_END: the empty pattern, and the empty-match protocol
   (pcre2_match.c:881-895). *)
let test_2 () =
  let empty = mk_code [ Opcodes.op_end ] in
  (* Empty match at the start: ovector (0, 0). *)
  (match match_internal ~code:empty ~top_bracket:0 "abc" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 0));
  (* NOTEMPTY rejects any empty match. *)
  (match
     match_internal ~moptions:Options.notempty ~code:empty ~top_bracket:0 "abc"
       0
   with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* NOTEMPTY_ATSTART rejects the empty match only at
     start_subject + start_offset... *)
  (match
     match_internal ~moptions:Options.notempty_atstart ~code:empty
       ~top_bracket:0 "abc" 0
   with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* ...a bumped-along attempt (start 1, original start_offset 0) is an
     acceptable empty match, ovector (1, 1). *)
  match
    match_internal ~moptions:Options.notempty_atstart ~start_offset:0
      ~code:empty ~top_bracket:0 "abc" 1
  with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 1);
      assert (Int.equal ov.(1) 1)

(* Limit trips at the C's exact sites: match_call_count is bumped then
   compared >= match_limit, and Frdepth (= 0 for the first frame) is
   compared >= match_limit_depth, both at NEW_FRAME fallthrough
   (pcre2_match.c:782-783); the heap limit trips in Frames.create
   (pcre2_match.c:7055-7060). *)
let test_3 () =
  let abc = mk_code [ Opcodes.op_char; Char.code 'a'; Opcodes.op_end ] in
  (match match_internal ~match_limit:0 ~code:abc ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_matchlimit) (* -47 *));
  (match match_internal ~match_limit_depth:0 ~code:abc ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_depthlimit) (* -53 *));
  (match match_internal ~heap_limit:0 ~code:abc ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_heaplimit) (* -63 *));
  (* One frame per attempt while no opcode RMATCHes: limit 1 is enough. *)
  match match_internal ~match_limit:1 ~code:abc ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc match_match)

(* BADOFFSET at the seam (pcre2_match.c:6610; port-conventions §5): a
   negative start (unsigned PCRE2_SIZE in the C, hence > length) and
   start > length both give -33; start = length is a legal attempt
   position (the C check is strict >). *)
let test_4 () =
  let empty = mk_code [ Opcodes.op_end ] in
  (match match_internal ~code:empty ~top_bracket:0 "a" (-3) with
  | rc, _ -> assert (Int.equal rc Errors.error_badoffset));
  (match match_internal ~code:empty ~top_bracket:0 "a" 2 with
  | rc, _ -> assert (Int.equal rc Errors.error_badoffset));
  match match_internal ~code:empty ~top_bracket:0 "a" 1 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 1);
      assert (Int.equal ov.(1) 1)

(* Unset-capture fill on success (pcre2_match.c:934-939): with
   top_bracket = 2 and no OP_CBRA yet, offset_top stays 0 and both pairs
   read PCRE2_UNSET; an oveccount smaller than top_bracket + 1 clips the
   copy. *)
let test_5 () =
  let a_end = mk_code [ Opcodes.op_char; Char.code 'a'; Opcodes.op_end ] in
  (match match_internal ~oveccount:3 ~code:a_end ~top_bracket:2 "abc" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal (Array.length ov) 6);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 1);
      for i = 2 to 5 do
        assert (Int.equal ov.(i) Frames.unset)
      done);
  match match_internal ~oveccount:1 ~code:a_end ~top_bracket:2 "abc" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal (Array.length ov) 2);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 1)

(* Malformed programs: OP_DEFINE has no case in the C switch ->
   PCRE2_ERROR_INTERNAL. *)
let test_6 () =
  match
    match_internal ~code:(mk_code [ Opcodes.op_define ]) ~top_bracket:0 "a" 0
  with
  | rc, _ -> assert (Int.equal rc Errors.error_internal)

(* Direct match_block block: observability the (rc, ovector) surface
   hides — hitend, mark, last_used_ptr, counters — asserted against the
   C sites; also documents the direct [match_] call shape for the driver
   chunk. *)
let test_7 () =
  let abc =
    mk_code
      [
        Opcodes.op_char;
        Char.code 'a';
        Opcodes.op_char;
        Char.code 'b';
        Opcodes.op_char;
        Char.code 'c';
        Opcodes.op_end;
      ]
  in
  let mk_mb ?(moptions = 0) subject start =
    {
      match_limit = Limits.match_limit;
      match_limit_depth = Limits.match_limit_depth;
      match_call_count = 0;
      hitend = false;
      hasthen = false;
      allowemptypartial = false;
      subject;
      start_offset = start;
      end_offset_top = 0;
      partial =
        (if not (Int.equal (moptions land Options.partial_hard) 0) then 2
         else if not (Int.equal (moptions land Options.partial_soft) 0) then 1
         else 0);
      bsr_convention = Options.bsr_unicode;
      name_count = 0;
      name_entry_size = 0;
      name_table = Bytes.empty;
      start_code = abc;
      start_subject = 0;
      check_subject = 0;
      end_subject = String.length subject;
      true_end_subject = String.length subject;
      end_match_ptr = 0;
      start_used_ptr = start;
      last_used_ptr = start;
      mark = Frames.unset;
      nomatch_mark = Frames.unset;
      verb_ecode_ptr = Frames.unset;
      verb_skip_ptr = Frames.unset;
      verb_current_recurse = Frames.recurse_unset;
      moptions;
      poptions = 0;
      skip_arg_count = 0;
      ignore_skip_arg = 0;
      nltype = Newline.nltype_fixed;
      nllen = 1;
      nl0 = Newline.char_lf;
      nl1 = 0;
    }
  in
  let run mb =
    match
      (* fresh arena (use_scratch:false): module-initialization assert. *)
      Frames.create ~use_scratch:false ~top_bracket:0
        ~heap_limit:Limits.heap_limit
    with
    | Error _ -> assert false
    | Ok a ->
        (* The per-exec [match_state]; utf/ucp from mb.poptions = 0 here
           (pcre2_match.c:630-637). *)
        let st =
          {
            mb;
            arena = a;
            match_data =
              {
                ovector = Array.make 2 Frames.unset;
                oveccount = 1;
                rc = 0;
                startchar = 0;
                leftchar = 0;
                rightchar = 0;
                mark = Frames.unset;
              };
            top_bracket = 0;
            utf = false;
            ucp = false;
            nl_scratch = ref 0;
            ref_length = ref 0;
            branch_end = -1;
            assert_accept_frame = -1;
          }
        in
        match_ st ~start_eptr:mb.start_used_ptr ~start_ecode:0
  in
  (* Soft partial on "ab": rc NOMATCH but hitend set (SCHECK_PARTIAL,
     540-541: eptr 2 > start_used_ptr 0); last_used_ptr updated to the
     consulted end (RETURN_SWITCH, 6470); exactly one frame was created;
     mark never set. *)
  (let mb = mk_mb ~moptions:Options.partial_soft "ab" 0 in
   let rc = run mb in
   assert (Int.equal rc match_nomatch);
   assert mb.hitend;
   assert (Int.equal mb.last_used_ptr 2);
   assert (Int.equal mb.match_call_count 1);
   assert (Int.equal mb.mark Frames.unset));
  (* Success on "abc": end_match_ptr / end_offset_top / mark / last_used
     recorded at 926-929. *)
  let mb = mk_mb "abc" 0 in
  let rc = run mb in
  assert (Int.equal rc match_match);
  assert (Int.equal mb.end_match_ptr 3);
  assert (Int.equal mb.end_offset_top 0);
  assert (Int.equal mb.mark Frames.unset);
  assert (Int.equal mb.last_used_ptr 3);
  assert (not mb.hitend)

(* OP_CHARI: caseless single characters through the lcc table
   (pcre2_match.c:1097-1103); SCHECK_PARTIAL at the subject end
   (1035-1039). *)
let test_8 () =
  let ax =
    mk_code
      [
        Opcodes.op_chari;
        Char.code 'a';
        Opcodes.op_chari;
        Char.code 'X';
        Opcodes.op_end;
      ]
  in
  List.iter
    (fun s ->
      match match_internal ~code:ax ~top_bracket:0 s 0 with
      | rc, ov ->
          assert (Int.equal rc match_match);
          assert (Int.equal ov.(0) 0);
          assert (Int.equal ov.(1) 2))
    [ "ax"; "aX"; "Ax"; "AX" ];
  (match match_internal ~code:ax ~top_bracket:0 "ay" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* Subject runs out before the second CHARI: NOMATCH normally, -2 under
     hard partial. *)
  (match match_internal ~code:ax ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  match
    match_internal ~moptions:Options.partial_hard ~code:ax ~top_bracket:0 "a" 0
  with
  | rc, _ -> assert (Int.equal rc Errors.error_partial)

(* OP_NOT / OP_NOTI: negated single characters (pcre2_match.c:1165-1173);
   the caseless form also rejects the fcc other case. *)
let test_9 () =
  let not_a = mk_code [ Opcodes.op_not; Char.code 'a'; Opcodes.op_end ] in
  (match match_internal ~code:not_a ~top_bracket:0 "b" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 1));
  (match match_internal ~code:not_a ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* Caseful NOT: the other case is NOT excluded. *)
  (match match_internal ~code:not_a ~top_bracket:0 "A" 0 with
  | rc, _ -> assert (Int.equal rc match_match));
  let noti_a = mk_code [ Opcodes.op_noti; Char.code 'a'; Opcodes.op_end ] in
  (match match_internal ~code:noti_a ~top_bracket:0 "b" 0 with
  | rc, _ -> assert (Int.equal rc match_match));
  (match match_internal ~code:noti_a ~top_bracket:0 "a" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (match match_internal ~code:noti_a ~top_bracket:0 "A" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  (* Empty subject: eptr = start_used_ptr and no allowemptypartial, so
     SCHECK_PARTIAL does not fire even under hard partial (537-543). *)
  match
    match_internal ~moptions:Options.partial_hard ~code:noti_a ~top_bracket:0 ""
      0
  with
  | rc, _ -> assert (Int.equal rc match_nomatch)

(* TEST-ONLY: real compiled programs for the repeat arms, predating the
   brackets chunk. The compiler always wraps the pattern in OP_BRA ..
   OP_KET (pcre2_compile.c:10570-10604); these asserts strip the wrapper —
   asserting its exact shape — and terminate the body with OP_END, so
   they exercise the repeat arms without any bracket opcode in the
   program. Returns (body code, top_bracket). *)
let compile_body (pattern : string) : Bytes.t * int =
  match Compile.pcre2_compile pattern ~options:0 with
  | Error _ -> assert false
  | Ok re ->
      let code = re.Compile.code in
      assert (Int.equal (Char.code (Bytes.get code 0)) Opcodes.op_bra);
      let ket = Compile.get code 1 in
      assert (Int.equal (Char.code (Bytes.get code ket)) Opcodes.op_ket);
      assert (
        Int.equal
          (Char.code (Bytes.get code (ket + 1 + Limits.link_size)))
          Opcodes.op_end);
      let body_len = ket - (1 + Limits.link_size) in
      let body = Bytes.create (body_len + 1) in
      Bytes.blit code (1 + Limits.link_size) body 0 body_len;
      Bytes.set body body_len (Char.chr Opcodes.op_end);
      (body, re.Compile.top_bracket)

(* Single-character repeats over real compiled bodies: all three match
   strategies (exact/min-only, minimize, maximize) in both cases, plus
   the possessive forms and SCHECK_PARTIAL placement inside the loops.
   Each attempt is anchored at offset 0 (match_internal has no bump-along
   loop), so a NOMATCH here is a statement about backtracking behavior at
   that fixed start. *)
let test_10 () =
  let run ?moptions pat subj =
    let code, top_bracket = compile_body pat in
    match_internal ?moptions ~code ~top_bracket subj 0
  in
  let expect_match pat subj e =
    match run pat subj with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal ov.(0) 0);
        assert (Int.equal ov.(1) e)
  in
  let expect_nomatch pat subj =
    match run pat subj with rc, _ -> assert (Int.equal rc match_nomatch)
  in
  let expect_hard_partial pat subj =
    match run ~moptions:Options.partial_hard pat subj with
    | rc, _ -> assert (Int.equal rc Errors.error_partial)
  in
  (* a{2,4} = OP_EXACT 2 + OP_UPTO 2 (greedy): consume the maximum, then
     back off in place (RM28; the `<=` break tries Lstart_eptr without a
     new frame). *)
  expect_match "a{2,4}b" "aaab" 4;
  expect_match "a{2,4}b" "aab" 3;
  expect_nomatch "a{2,4}b" "ab";
  expect_match "a{2,4}b" "aaaab" 5;
  (* The maximum run ends at 4; backing off to the minimum at 2 never
     finds 'b'. *)
  expect_nomatch "a{2,4}b" "aaaaab";
  (* RM28 resume then the break-at-Lstart in-place tail. *)
  expect_match "a{2,4}ab" "aaab" 4;
  (* OP_MINUPTO: RM27 iterations up to the bound... *)
  expect_match "a{2,4}?b" "aaab" 4;
  (* ...and the Lmin++ >= Lmax refusal at the bound. *)
  expect_nomatch "a{2,4}?b" "aaaaab";
  (* Lazy star (OP_MINSTAR, RM27). *)
  expect_match "a*?b" "aaab" 4;
  (* Plus: minimum of one. *)
  expect_nomatch "a+b" "b";
  expect_match "a+b" "ab" 2;
  (* Query: 0 or 1, greedy. *)
  expect_match "ab?c" "abc" 3;
  expect_match "ab?c" "ac" 2;
  (* Caseless repeats: OP_STARI maximize (RM26 resume at "ab*bc"),
     OP_MINSTARI minimize (RM25), and the full descend-to-Lstart NOMATCH. *)
  expect_match "(?i)ab*c" "aBBBc" 5;
  expect_match "(?i)ab*bc" "aBBc" 4;
  expect_match "(?i)ab*?c" "aBBc" 4;
  expect_nomatch "(?i)ab*d" "aBBc";
  (* Caseful analogues (RM28 resume / descend). *)
  expect_match "ab*bc" "abbc" 4;
  expect_nomatch "ab*d" "abbc";
  (* Possessive: no backing up once the run is consumed. *)
  expect_match "a*+b" "aaab" 4;
  expect_nomatch "a*+ab" "aaab";
  expect_match "a++b" "aaab" 4;
  expect_nomatch "a++ab" "aaab";
  expect_match "a{2,4}+b" "aaab" 4;
  expect_nomatch "a{2,4}+ab" "aaab";
  (* SCHECK_PARTIAL inside the loops: the EXACT min loop, the minimize
     resume (RM27), and the greedy scan all hit the subject end. *)
  expect_hard_partial "a{3}" "aa";
  expect_hard_partial "a*?b" "aaa";
  expect_hard_partial "a*b" "aaa"

(* Negated single-character repeats (REPEATNOTCHAR) over compiled
   bodies: [^b] compiles to OP_NOT via the one-char negated class
   optimization, and its quantifiers to the NOT repeat opcodes. *)
let test_11 () =
  let run ?moptions pat subj =
    let code, top_bracket = compile_body pat in
    match_internal ?moptions ~code ~top_bracket subj 0
  in
  let expect_match pat subj e =
    match run pat subj with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal ov.(0) 0);
        assert (Int.equal ov.(1) e)
  in
  let expect_nomatch pat subj =
    match run pat subj with rc, _ -> assert (Int.equal rc match_nomatch)
  in
  (* OP_NOTSTAR maximize (scan stops at 'b'); RM32 resume. *)
  expect_match "[^b]*b" "aaab" 4;
  expect_match "[^b]*ab" "aaab" 4;
  (* OP_NOTMINSTAR minimize (RM31). *)
  expect_match "[^b]*?b" "aab" 3;
  (* Caseless: OP_NOTSTARI maximize (RM30) and minimize (RM29); Loc
     rejection of the other case. *)
  expect_match "(?i)[^b]*ab" "aAab" 4;
  expect_match "(?i)[^b]*?c" "aAc" 3;
  expect_match "(?i)[^b]{2}c" "aac" 3;
  expect_nomatch "(?i)[^b]{2}c" "aBc";
  (* Possessive NOT. *)
  expect_match "[^b]*+b" "aab" 3;
  expect_nomatch "[^b]*+ab" "aab";
  (* SCHECK_PARTIAL in the NOTEXACT min loop. *)
  match
    let code, top_bracket = compile_body "[^b]{3}" in
    match_internal ~moptions:Options.partial_hard ~code ~top_bracket "aa" 0
  with
  | rc, _ -> assert (Int.equal rc Errors.error_partial)

(* Bit-mapped classes (OP_CLASS/OP_NCLASS) over compiled bodies: the CR*
   repeat-info decode, all three match strategies (exact/min-only,
   minimize RM23, maximize RM24), the possessive forms, and the
   SCHECK_PARTIAL sites inside the loops. Anchored attempts at fixed
   offsets, as above. *)
let test_12 () =
  let run ?moptions pat subj start =
    let code, top_bracket = compile_body pat in
    match_internal ?moptions ~code ~top_bracket subj start
  in
  let expect_match ?moptions pat subj start e =
    match run ?moptions pat subj start with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal ov.(0) start);
        assert (Int.equal ov.(1) e)
  in
  let expect_nomatch ?moptions pat subj start =
    match run ?moptions pat subj start with
    | rc, _ -> assert (Int.equal rc match_nomatch)
  in
  let expect_hard_partial pat subj start =
    match run ~moptions:Options.partial_hard pat subj start with
    | rc, _ -> assert (Int.equal rc Errors.error_partial)
  in
  (* OP_CRPLUS greedy: min 1 then scan, backtrack in the RM24 loop. *)
  expect_match "[a-c]+d" "abcd" 0 4;
  expect_nomatch "[a-c]+d" "d" 0;
  (* No repeat info: Lmin = Lmax = 1. *)
  expect_match "[a-c]x" "bx" 0 2;
  expect_nomatch "[a-c]x" "dx" 0;
  (* OP_CRRANGE greedy: {2,4}. *)
  expect_match "[a-c]{2,4}d" "abd" 0 3;
  expect_match "[a-c]{2,4}d" "abcad" 0 5;
  expect_nomatch "[a-c]{2,4}d" "ad" 0;
  expect_match "[a-c]{2,4}d" "aabd" 0 4 (* RM24 back-off *);
  expect_nomatch "[a-c]{2,4}d" "abcabd" 0 (* RM24 descends past Lstart *);
  (* OP_CRMINRANGE (RM23) incl. the Lmin++ >= Lmax refusal. *)
  expect_match "[a-c]{2,4}?d" "abcd" 0 4;
  expect_nomatch "[a-c]{2,4}?d" "abcabd" 0;
  (* OP_CRSTAR / OP_CRMINSTAR / OP_CRQUERY. *)
  expect_match "[a-c]*d" "d" 0 1;
  expect_match "[a-c]*?d" "abcd" 0 4;
  expect_match "[a-c]?d" "ad" 0 2;
  expect_match "[a-c]?d" "d" 0 1;
  (* OP_CRPOSSTAR / OP_CRPOSRANGE: no backing up. *)
  expect_match "[a-c]*+d" "abcd" 0 4;
  expect_nomatch "[a-c]*+cd" "abcd" 0;
  expect_match "[a-c]{2,4}+d" "abcd" 0 4;
  expect_nomatch "[a-c]{2,4}+cd" "abcd" 0;
  (* OP_NCLASS (multi-character negated class). *)
  expect_match "[^ab]+b" "xyzb" 0 4;
  expect_nomatch "[^ab]+b" "ab" 0;
  expect_match "[^ab]{2}" "xy" 0 2;
  (* One-char negated class repeats ride OP_NOT (chars chunk); keep them
     covered from this chunk's dispatch list. *)
  expect_match "[^a]*a" "xxa" 0 3;
  expect_match "[^a]*?a" "xxa" 0 3;
  (* SCHECK_PARTIAL: the min loop, the greedy scan, and the RM23 resume. *)
  expect_hard_partial "[a-c]{3}" "ab" 0;
  expect_hard_partial "[a-c]*d" "abc" 0;
  expect_hard_partial "[a-c]+?d" "ab" 0

(* Character types: OP_ANY/OP_ALLANY singles and the REPEATTYPE machinery
   (min loops, minimize RM33, maximize RM34, possessive), plus the \d \w
   \s \h \v \R single-match arms. *)
let test_13 () =
  let run ?moptions ?poptions pat subj start =
    let code, top_bracket = compile_body pat in
    match_internal ?moptions ?poptions ~code ~top_bracket subj start
  in
  let expect_match ?moptions ?poptions pat subj start e =
    match run ?moptions ?poptions pat subj start with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal ov.(0) start);
        assert (Int.equal ov.(1) e)
  in
  let expect_nomatch ?moptions ?poptions pat subj start =
    match run ?moptions ?poptions pat subj start with
    | rc, _ -> assert (Int.equal rc match_nomatch)
  in
  let expect_hard_partial pat subj start =
    match run ~moptions:Options.partial_hard pat subj start with
    | rc, _ -> assert (Int.equal rc Errors.error_partial)
  in
  (* OP_ANY refuses the (fixed-LF) newline; OP_ALLANY takes it. *)
  expect_match ".b" "ab" 0 2;
  expect_nomatch ".b" "\nb" 0;
  expect_match "(?s).b" "\nb" 0 2;
  (* Typed repeat strategies over \d: TYPEEXACT + TYPESTAR/TYPEUPTO. *)
  expect_match "\\d{2,}x" "12x" 0 3;
  expect_match "\\d{2,}x" "12345x" 0 6;
  expect_nomatch "\\d{2,}x" "1x" 0;
  expect_match "\\d{2,4}y" "123y" 0 4 (* TYPEUPTO scan + RM34 back-off *);
  expect_match "\\d*0" "100" 0 3 (* RM34 greedy back-off *);
  expect_match "\\d*?x" "12x" 0 3 (* TYPEMINSTAR, RM33 *);
  expect_nomatch "\\d{2,3}?x" "1234x" 0 (* RM33 Lmin++ >= Lmax refusal *);
  expect_match "\\d*+x" "12x" 0 3 (* TYPEPOSSTAR *);
  expect_nomatch "\\d*+1" "11" 0;
  (* OP_ANY typed repeats: the min-loop newline stop and the maximize
     newline stop. *)
  expect_match ".*b" "aaab" 0 4;
  expect_nomatch ".+b" "\nab" 0;
  expect_match ".*" "ab\ncd" 0 2;
  expect_match "(?s).*" "ab\ncd" 0 5 (* ALLANY maximize: take all *);
  expect_match "(?s).{2}x" "\n\nx" 0 3 (* ALLANY min: Feptr += Lmin *);
  (* The ALLANY min phase is ONE up-front bound check: with nothing yet
     consumed (Feptr = start_used_ptr) SCHECK_PARTIAL does not fire and
     the result is a plain NOMATCH even under hard partial — oracle
     confirmed. After a consumed character it is a hard partial. *)
  (match run ~moptions:Options.partial_hard "(?s).{3}" "ab" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch));
  expect_hard_partial "a(?s).{2}" "ab" 0;
  expect_hard_partial ".*" "ab" 0;
  (* Single-match type arms. *)
  expect_match "\\w" "_" 0 1;
  expect_nomatch "\\W" "_" 0;
  expect_match "\\W" "-" 0 1;
  expect_match "\\s" " " 0 1;
  expect_nomatch "\\S" " " 0;
  expect_match "\\S" "x" 0 1;
  expect_match "\\d" "5" 0 1;
  expect_match "\\D" "a" 0 1;
  expect_nomatch "\\D" "5" 0;
  expect_match "\\h" "\t" 0 1;
  expect_match "\\h" "\xa0" 0 1 (* NBSP byte *);
  expect_nomatch "\\h" "\n" 0;
  expect_match "\\H" "\n" 0 1;
  expect_nomatch "\\H" " " 0;
  expect_match "\\v" "\n" 0 1;
  expect_match "\\v" "\x85" 0 1 (* NEL byte *);
  expect_nomatch "\\v" "\t" 0;
  expect_match "\\V" "\t" 0 1;
  expect_nomatch "\\V" "\x0c" 0;
  (* Typed h/v/s/w repeats: min loops, maximize scans, minimize. *)
  expect_match "\\h{2}x" " \tx" 0 3;
  expect_match "\\h*x" "  x" 0 3;
  expect_match "\\v+x" "\n\x0bx" 0 3;
  expect_match "\\H*?x" "abx" 0 3;
  expect_match "\\V{2}" "ab" 0 2;
  expect_match "\\s*x" " \tx" 0 3;
  expect_match "\\w+?-" "ab-" 0 3;
  expect_hard_partial "\\d{3}" "12" 0;
  expect_hard_partial "\\d+" "12" 0;
  (* \R (OP_ANYNL): BSR_UNICODE convention in match_internal. *)
  expect_match "\\R" "\r\n" 0 2;
  expect_match "\\R" "\rx" 0 1;
  expect_match "\\R" "\x0b" 0 1;
  expect_match "\\R" "\x85" 0 1;
  expect_nomatch "\\R" "a" 0;
  expect_hard_partial "\\R" "\r" 0 (* SCHECK_PARTIAL in the CR arm *);
  (* \R typed repeats: the min-loop CRLF absorb, the maximize scan, the
     lazy RM33 CR sub-case, and the RM34 CRLF double step (without it the
     greedy back-off would land between CR and LF and match at 2). *)
  expect_match "\\R{2}x" "\r\n\nx" 0 4;
  expect_match "\\R+" "\n\r\n\x0b" 0 4;
  expect_match "\\R*?z" "\r\nz" 0 3;
  expect_match "\\R{0,9}\\n" "\n\r\n" 0 1

(* Anchors and simple assertions: OP_CIRC/OP_CIRCM, OP_DOLL/OP_DOLLM,
   OP_SOD, OP_SOM, OP_SET_SOM, OP_EOD, OP_EODN, and the \b/\B word
   boundaries. *)
let test_14 () =
  let run ?moptions ?poptions ?start_offset pat subj start =
    let code, top_bracket = compile_body pat in
    match_internal ?moptions ?poptions ?start_offset ~code ~top_bracket subj
      start
  in
  let expect_match ?moptions ?poptions ?start_offset pat subj start e =
    match run ?moptions ?poptions ?start_offset pat subj start with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal ov.(0) start);
        assert (Int.equal ov.(1) e)
  in
  let expect_nomatch ?moptions ?poptions ?start_offset pat subj start =
    match run ?moptions ?poptions ?start_offset pat subj start with
    | rc, _ -> assert (Int.equal rc match_nomatch)
  in
  let expect_hard_partial pat subj start =
    match run ~moptions:Options.partial_hard pat subj start with
    | rc, _ -> assert (Int.equal rc Errors.error_partial)
  in
  (* OP_CIRC: start of subject only, killed by NOTBOL. *)
  expect_match "^a" "a" 0 1;
  expect_nomatch ~moptions:Options.notbol "^a" "a" 0;
  expect_nomatch "^a" "aa" 1;
  (* OP_CIRCM: after any newline; NOTBOL only kills the subject start;
     not after a final newline unless ALT_CIRCUMFLEX. *)
  expect_match "(?m)^b" "a\nb" 2 3;
  expect_nomatch "(?m)^b" "ab" 1;
  expect_nomatch ~moptions:Options.notbol "(?m)^a" "a" 0;
  expect_match ~moptions:Options.notbol "(?m)^b" "a\nb" 2 3;
  expect_nomatch "(?m)^" "a\n" 2;
  expect_match ~poptions:Options.alt_circumflex "(?m)^" "a\n" 2 2;
  (* OP_DOLL -> ASSERT_NL_OR_EOS: end of subject or the newline that ends
     it; NOTEOL kills it; DOLLAR_ENDONLY reroutes to the OP_EOD body. *)
  expect_match "a$" "a" 0 1;
  expect_nomatch ~moptions:Options.noteol "a$" "a" 0;
  expect_match "a$" "a\n" 0 1;
  expect_nomatch "a$" "a\nb" 0;
  expect_nomatch ~poptions:Options.dollar_endonly "a$" "a\n" 0;
  expect_match ~poptions:Options.dollar_endonly "a$" "a" 0 1;
  (* OP_DOLLM: before any newline, or at the end unless NOTEOL; the end
     position hits SCHECK_PARTIAL. *)
  expect_match "(?m)a$" "a\nb" 0 1;
  expect_nomatch "(?m)a$" "ab" 0;
  expect_match "(?m)a$" "a" 0 1;
  expect_nomatch ~moptions:Options.noteol "(?m)a$" "a" 0;
  expect_hard_partial "(?m)a$" "a" 0;
  (* OP_SOD (\A). *)
  expect_match "\\Aa" "aa" 0 1;
  expect_nomatch "\\Aa" "aa" 1;
  (* OP_SOM (\G): subject + start_offset. *)
  expect_match "\\Ga" "aa" 1 2;
  expect_nomatch ~start_offset:0 "\\Ga" "aa" 1;
  (* OP_SET_SOM (\K): resets ovector.(0). *)
  (match run "a\\Kb" "ab" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 1);
      assert (Int.equal ov.(1) 2));
  (* OP_EOD (\z): true end only; hard partial at the end. *)
  expect_match "a\\z" "a" 0 1;
  expect_nomatch "a\\z" "a\n" 0;
  expect_hard_partial "a\\z" "a" 0;
  (* OP_EODN (\Z): end or the final newline. *)
  expect_match "a\\Z" "a" 0 1;
  expect_match "a\\Z" "a\n" 0 1;
  expect_nomatch "a\\Z" "a\nb" 0;
  expect_hard_partial "a\\Z" "a" 0;
  (* Word boundaries at start / middle / end; \B; the end-of-subject peek
     hits SCHECK_PARTIAL. *)
  expect_match "\\bword\\b" "word" 0 4;
  expect_match "\\bword\\b" "a word z" 2 6;
  expect_nomatch "\\bword\\b" "words" 0;
  expect_nomatch "\\bord" "word" 1;
  expect_match "\\Bord" "word" 1 4;
  expect_nomatch "\\Bword" "word" 0;
  expect_match "x\\b" "x" 0 1;
  expect_hard_partial "x\\b" "x" 0

(* OP_ANYBYTE: unreachable from the non-UTF compiler (\C compiles to
   OP_ALLANY, pcre2_compile.c:8174-8176), exercised hand-assembled. *)
let test_15 () =
  let anyb = mk_code [ Opcodes.op_anybyte; Opcodes.op_end ] in
  (match match_internal ~code:anyb ~top_bracket:0 "\n" 0 with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 1));
  match match_internal ~code:anyb ~top_bracket:0 "" 0 with
  | rc, _ -> assert (Int.equal rc match_nomatch)

(* TEST-ONLY (brackets chunk): whole compiled programs — the OP_BRA ..
   OP_KET wrapper and every inner bracket now run for real. Returns
   (code, top_bracket). *)
let compile_whole (pattern : string) : Bytes.t * int =
  match Compile.pcre2_compile pattern ~options:0 with
  | Error _ -> assert false
  | Ok re -> (re.Compile.code, re.Compile.top_bracket)

(* The backtracking gauntlet: brackets, alternation, kets and the
   zero-repeat wrappers over whole compiled programs. EVERY expected
   ovector below is pinned against the C oracle (pcre2test_ml with the
   allcaptures/allaftertext modifiers); anchored attempts at offset 0
   (match_internal has no bump-along loop). *)
let test_16 () =
  let run ?moptions ?oveccount pat subj =
    let code, top_bracket = compile_whole pat in
    let oveccount =
      match oveccount with Some c -> c | None -> top_bracket + 1
    in
    match_internal ?moptions ~oveccount ~code ~top_bracket subj 0
  in
  (* Every pair of the (top_bracket + 1)-pair ovector must equal the
     oracle's, including unset (-1, -1) pairs. *)
  let expect_ov pat subj expected =
    match run pat subj with
    | rc, ov ->
        assert (Int.equal rc match_match);
        assert (Int.equal (Array.length ov) (Array.length expected));
        Array.iteri (fun i e -> assert (Int.equal ov.(i) e)) expected
  in
  let expect_nomatch pat subj =
    match run pat subj with rc, _ -> assert (Int.equal rc match_nomatch)
  in
  (* Alternative walk + capture — oracle: 0: bc, 1: b. *)
  expect_ov "(a|b)c" "bc" [| 0; 2; 0; 1 |];
  (* Backtrack across a group boundary — oracle: 0: aaab, 1: aa, 2: a
     (the first greedy a+ backs off from 3 to 2; the capture is rewritten
     at each ket pass). *)
  expect_ov "(a+)(a+)b" "aaab" [| 0; 4; 0; 2; 2; 3 |];
  (* Empty-loop protection at KETRMAX — oracle: 0: aaab, 1: "" before b
     (i.e. (3,3)): the second iteration matches empty at 3, its ket
     writes the capture, and the Feptr == P->eptr test then carries on at
     this level instead of looping forever. *)
  expect_ov "(a*)*b" "aaab" [| 0; 4; 3; 3 |];
  (* Same protection through a bounded inner repeat — oracle: 1: ""
     before b. *)
  expect_ov "(a{0,2})*b" "aaab" [| 0; 4; 3; 3 |];
  (* Leftmost-first alternation semantics — oracle: 1: a, 2: bcd. *)
  expect_ov "(a|ab)(c|bcd)" "abcd" [| 0; 4; 0; 1; 1; 4 |];
  (* Unset optional group — oracle: 0: y, 1: <unset>. *)
  expect_ov "(x)?y" "y" [| 0; 1; -1; -1 |];
  (* Bounded group repeat — oracle: 0: aa, 1: a with nothing after
     (the last taken iteration, (1,2)). *)
  expect_ov "(a){0,3}" "aa" [| 0; 2; 1; 2 |];
  (* Zero iterations of the same — oracle: empty match, 1: <unset>. *)
  expect_ov "(a){0,3}" "b" [| 0; 0; -1; -1 |];
  (* Per-iteration capture semantics — oracle: 0: ab, 1: b, 2: a: the
     second iteration overwrites group 1, while group 2 survives from the
     first iteration (frame ovector copies carry it forward). *)
  expect_ov "((a)|b)+" "ab" [| 0; 2; 1; 2; 0; 1 |];
  (* Nested alternated captures — oracle: 1: a, 2: b. *)
  expect_ov "(?:(a)|(b))+" "ab" [| 0; 2; 0; 1; 1; 2 |];
  (* KETRMIN lazy group repeat with a capture — oracle: 0: aab, 1: a
     with only b after (i.e. (1,2)). *)
  expect_ov "(a+?)+?b" "aab" [| 0; 3; 1; 2 |];
  (* KETRMIN empty-match protection — oracle: 0: b, 1: "" at 0: the lazy
     group matches empty once and the Feptr == P->eptr test carries on at
     this level. *)
  expect_ov "(a*?)+?b" "b" [| 0; 1; 0; 0 |];
  (* BRAZERO: with and without the group — oracle: 0: d / 0: abcd. *)
  expect_ov "(?:abc)?d" "d" [| 0; 1 |];
  expect_ov "(?:abc)?d" "abcd" [| 0; 4 |];
  (* BRAMINZERO: the skip-the-group try succeeds on "d"; on "abcd" the
     RM10 resume steps into the group — oracle: 0: d / 0: abcd. *)
  expect_ov "(?:abc)??d" "d" [| 0; 1 |];
  expect_ov "(?:abc)??d" "abcd" [| 0; 4 |];
  (* SKIPZERO: a {0} group is skipped entirely — oracle: 0: b,
     1: <unset>. *)
  expect_ov "(a){0}b" "b" [| 0; 1; -1; -1 |];
  (* KETRMIN re-iteration (RM6: back to the bracket in the same frame) —
     oracle: 0: ababc. *)
  expect_ov "(?:ab)+?c" "ababc" [| 0; 5 |];
  (* KETRMAX releasing an iteration (RM7 NOMATCH resume): three
     iterations fail the tail, two match — oracle: 0: aaaaab. *)
  expect_ov "(?:aa)+ab" "aaaaab" [| 0; 6 |];
  (* Inner non-capturing alternation at depth > 0: the OP_BRA optimized
     branch walk (RM1) twice, then the final branch running in place —
     oracle: 0: xcd. *)
  expect_ov "x(?:a|b|c)d" "xcd" [| 0; 3 |];
  (* GROUPLOOP: no alternative left fails the group
     (pcre2_match.c:5410). *)
  expect_nomatch "(?:a|b)" "c";
  expect_nomatch "(a|b)c" "bd";
  (* Top-level alternation with captures on distinct branches — oracle:
     1: <unset>, 2: b: a dynamic "gap" below offset_top stays unset. *)
  expect_ov "(a)|(b)" "b" [| 0; 1; -1; -1; 0; 1 |];
  (* Nested captures — oracle: 0: abc, 1: ab, 2: a. *)
  expect_ov "((a)b)c" "abc" [| 0; 3; 0; 2; 0; 1 |];
  (* Adjacent captures — oracle: 1: a, 2: b. *)
  expect_ov "(a)(b)" "ab" [| 0; 2; 0; 1; 1; 2 |];
  (* Oveccount clipping with real captures (pcre2_match.c:934-939):
     oveccount 2 keeps group 1 and clips group 2. *)
  match run ~oveccount:2 "(a)(b)" "ab" with
  | rc, ov ->
      assert (Int.equal rc match_match);
      assert (Int.equal (Array.length ov) 4);
      assert (Int.equal ov.(0) 0);
      assert (Int.equal ov.(1) 2);
      assert (Int.equal ov.(2) 0);
      assert (Int.equal ov.(3) 1)

(* Driver checks (pcre2_match, this chunk): the bump-along protocol, rc
   conventions, option/offset validation, partial bookkeeping and the
   start-of-match optimizations over whole compiled patterns. Expected
   values pinned against the C oracle (pcre2test on the real 10.44
   library). *)
let test_17 () =
  let compile pat =
    match Compile.pcre2_compile pat ~options:0 with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run ?(options = 0) ?oveccount (re : Compile.re) subj start =
    let oveccount =
      match oveccount with Some c -> c | None -> re.Compile.top_bracket + 1
    in
    let m =
      {
        ovector = Array.make (2 * oveccount) Frames.unset;
        oveccount;
        rc = 0;
        startchar = 0;
        leftchar = 0;
        rightchar = 0;
        mark = Frames.unset;
      }
    in
    let rc = pcre2_match re ~subject:subj ~start_offset:start ~options m in
    (rc, m)
  in
  let abc = compile "abc" in
  (* Bump-along success: /abc/ finds the match away from the start; rc is
     the pair count (1), startchar the attempt start (pcre2_match.c:7716-
     7719). first_cu 'a' is set, so this also runs the caseful memchr
     advance (7286-7298). *)
  (match run abc "xxabcy" 0 with
  | rc, m ->
      assert (Int.equal rc 1);
      assert (Int.equal m.ovector.(0) 2);
      assert (Int.equal m.ovector.(1) 5);
      assert (Int.equal m.startchar 2);
      assert (Int.equal m.leftchar 2);
      assert (Int.equal m.rightchar 5));
  (* Classic nomatch (7764-7766). *)
  (match run abc "abd" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
  (* Undefined match option bits -> -34 (6595-6597); PCRE2_SUBSTITUTE_GLOBAL
     is not a public match option. *)
  (match run ~options:0x00000100 abc "abc" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_badoption));
  (* Partial + ENDANCHORED -> -34 (6661-6666). *)
  (match
     run ~options:(Options.partial_hard lor Options.endanchored) abc "abc" 0
   with
  | rc, _ -> assert (Int.equal rc Errors.error_badoption));
  (* BADOFFSET, both unsigned shapes (6610). *)
  (match run abc "abc" 4 with
  | rc, _ -> assert (Int.equal rc Errors.error_badoffset));
  (match run abc "abc" (-1) with
  | rc, _ -> assert (Int.equal rc Errors.error_badoffset));
  (* PCRE2_ANCHORED as a match option: one attempt only (7594-7597). *)
  (match run ~options:Options.anchored abc "xabc" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
  (match run ~options:Options.anchored abc "abcx" 0 with
  | rc, m ->
      assert (Int.equal rc 1);
      assert (Int.equal m.ovector.(1) 3));
  (* Soft partial: the bump-along continues after hitend; ENDLOOP promotes
     the remembered first partial position (7521-7525, 7747-7762): oracle
     "Partial match: ab" at 1..3. *)
  (match run ~options:Options.partial_soft abc "xab" 0 with
  | rc, m ->
      assert (Int.equal rc Errors.error_partial);
      assert (Int.equal m.ovector.(0) 1);
      assert (Int.equal m.ovector.(1) 3);
      assert (Int.equal m.startchar 1);
      assert (Int.equal m.leftchar 1);
      assert (Int.equal m.rightchar 3));
  (* Hard partial: match_ returns PCRE2_ERROR_PARTIAL immediately; same
     result surface. *)
  (match run ~options:Options.partial_hard abc "xab" 0 with
  | rc, m ->
      assert (Int.equal rc Errors.error_partial);
      assert (Int.equal m.ovector.(0) 1);
      assert (Int.equal m.ovector.(1) 3));
  (* A partial position found mid-bump is remembered and promoted at
     ENDLOOP after the remaining attempts fail (7521-7525, 7747-7762):
     oracle /abcd/ partial_soft on "xyz ab" -> Partial match: ab (4..6). *)
  (match run ~options:Options.partial_soft (compile "abcd") "xyz ab" 0 with
  | rc, m ->
      assert (Int.equal rc Errors.error_partial);
      assert (Int.equal m.ovector.(0) 4);
      assert (Int.equal m.ovector.(1) 6);
      assert (Int.equal m.startchar 4));
  (* rc is end_offset_top/2 + 1, counting unset middle groups below the
     high-water mark (7716-7717): oracle /(a)|(b)/ on "b" prints 0..2 with
     1: <unset>. *)
  (match run (compile "(a)|(b)") "b" 0 with
  | rc, m ->
      assert (Int.equal rc 3);
      assert (Int.equal m.ovector.(0) 0);
      assert (Int.equal m.ovector.(1) 1);
      assert (Int.equal m.ovector.(2) Frames.unset);
      assert (Int.equal m.ovector.(3) Frames.unset);
      assert (Int.equal m.ovector.(4) 0);
      assert (Int.equal m.ovector.(5) 1));
  (* ...and stops at the highest CLOSED group: /(a)(b)?/ on "a" -> rc 2,
     the group-2 pair unset-filled by the OP_END copy-out (934-939). *)
  (match run (compile "(a)(b)?") "a" 0 with
  | rc, m ->
      assert (Int.equal rc 2);
      assert (Int.equal m.ovector.(2) 0);
      assert (Int.equal m.ovector.(3) 1);
      assert (Int.equal m.ovector.(4) Frames.unset);
      assert (Int.equal m.ovector.(5) Frames.unset));
  (* Ovector too small: rc = 0 (7716-7717). *)
  (match run ~oveccount:1 (compile "(a)(b)") "ab" 0 with
  | rc, m ->
      assert (Int.equal rc 0);
      assert (Int.equal m.rc 0);
      assert (Int.equal m.ovector.(0) 0);
      assert (Int.equal m.ovector.(1) 2));
  (* startchar is the attempt start, NOT ovector[0] (\K moves the latter
     only): oracle /a\Kbc/ on "xabc" -> 0: bc with startchar 1. *)
  (match run (compile "a\\Kbc") "xabc" 0 with
  | rc, m ->
      assert (Int.equal rc 1);
      assert (Int.equal m.ovector.(0) 2);
      assert (Int.equal m.ovector.(1) 4);
      assert (Int.equal m.startchar 1));
  (* ( *NOTEMPTY) flag transfer into the match options (6621-6635): a*
     never returns the empty match, so "b" is a nomatch but "ab" matches
     the "a". *)
  (let ne = compile "(*NOTEMPTY)a*" in
   (match run ne "b" 0 with
   | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
   match run ne "ab" 0 with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (Int.equal m.ovector.(0) 0);
       assert (Int.equal m.ovector.(1) 1));
  (* PCRE2_FIRSTLINE: the match must start at or before the first newline
     (7164-7186 clamp + 7584-7588 break): oracle nomatch on "ab\nabc",
     match on "abc\nx". *)
  (let fl =
     match Compile.pcre2_compile "abc" ~options:Options.firstline with
     | Error _ -> assert false
     | Ok re -> re
   in
   (match run fl "ab\nabc" 0 with
   | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
   match run fl "abc\nx" 0 with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (Int.equal m.ovector.(1) 3));
  (* PCRE2_STARTLINE bump (7318-7349): /^abc/m has no first_cu; the
     startline scan advances straight past the newline. *)
  (let ml =
     match Compile.pcre2_compile "^abc" ~options:Options.multiline with
     | Error _ -> assert false
     | Ok re -> re
   in
   assert (not (Int.equal (ml.Compile.flags land Compile.startline) 0));
   match run ml "xyz\nabc" 0 with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (Int.equal m.ovector.(0) 4);
       assert (Int.equal m.ovector.(1) 7));
  (* ( *LIMIT_MATCH=n): per-attempt limit -> PCRE2_ERROR_MATCHLIMIT
     surfaces through the default switch arm (7575-7576). NO_START_OPT is
     needed: with it off, the req_cu search for the absent 'z' would
     break the loop with NOMATCH before any attempt (oracle confirmed
     both behaviors). *)
  (match
     run (compile "(*LIMIT_MATCH=10)(*NO_START_OPT)(a+)+z") "aaaaaaaaaaaa" 0
   with
  | rc, _ -> assert (Int.equal rc Errors.error_matchlimit));
  (match run (compile "(*LIMIT_MATCH=10)(a+)+z") "aaaaaaaaaaaa" 0 with
  | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
  (* NOTEMPTY_ATSTART: only the start-of-match empty match is rejected
     (881-895 read mb.start_offset): oracle /a*/ on "bc" with
     notempty_atstart matches "" at 1. *)
  match run ~options:Options.notempty_atstart (compile "a*") "bc" 0 with
  | rc, m ->
      assert (Int.equal rc 1);
      assert (Int.equal m.ovector.(0) 1);
      assert (Int.equal m.ovector.(1) 1)

(* Backreferences (this chunk): match_ref + the OP_REF/OP_REFI/OP_DNREF/
   OP_DNREFI arms with their repeat strategies (min-only, minimize RM20,
   maximize RM21/RM22), the unset-reference rules with and without
   PCRE2_MATCH_UNSET_BACKREF, the zero-length-reference loop break, and
   the partial sites. Whole compiled patterns through the [pcre2_match]
   driver; EVERY expected value below is pinned against the C oracle
   (pcre2test on the real 10.44 library). *)
let test_18 () =
  let compile ?(options = 0) pat =
    match Compile.pcre2_compile pat ~options with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run ?(options = 0) (re : Compile.re) subj =
    let oveccount = re.Compile.top_bracket + 1 in
    let m =
      {
        ovector = Array.make (2 * oveccount) Frames.unset;
        oveccount;
        rc = 0;
        startchar = 0;
        leftchar = 0;
        rightchar = 0;
        mark = Frames.unset;
      }
    in
    let rc = pcre2_match re ~subject:subj ~start_offset:0 ~options m in
    (rc, m)
  in
  let expect_ov ?options re subj expected =
    match run ?options re subj with
    | rc, m ->
        assert (rc > 0);
        assert (Int.equal (Array.length m.ovector) (Array.length expected));
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch ?options re subj =
    match run ?options re subj with
    | rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let expect_partial ?options re subj s e =
    match run ?options re subj with
    | rc, m ->
        assert (Int.equal rc Errors.error_partial);
        assert (Int.equal m.ovector.(0) s);
        assert (Int.equal m.ovector.(1) e)
  in
  (* Single copy, no repeat — oracle: 0: aa, 1: a / no match. *)
  (let re = compile "(a)\\1" in
   expect_ov re "aa" [| 0; 2; 0; 1 |];
   expect_nomatch re "ab");
  (* Greedy range OP_CRRANGE (RM21 back-off): {2,4} takes the maximum
     available then backs off in reference-length steps — oracle: aaa /
     aaaa / aaaaa (of "aaaaaa": 1 + 4 copies) / no match on aa. *)
  (let re = compile "(a)\\1{2,4}" in
   expect_ov re "aaa" [| 0; 3; 0; 1 |];
   expect_ov re "aaaa" [| 0; 4; 0; 1 |];
   expect_ov re "aaaaaa" [| 0; 5; 0; 1 |];
   expect_nomatch re "aa");
  (* Minimize OP_CRMINRANGE (RM20): grows one copy at a time up to the
     bound — oracle: aaab -> 0: aaab; aaaaab -> 0: aaaaab. *)
  (let re = compile "(a)\\1{2,4}?b" in
   expect_ov re "aaab" [| 0; 4; 0; 1 |];
   expect_ov re "aaaaab" [| 0; 6; 0; 1 |]);
  (* Caseless reference (OP_REFI, the lcc fold in match_ref) — oracle:
     0: abAB, 1: ab. *)
  (let re = compile "(?i)(ab)\\1" in
   expect_ov re "abAB" [| 0; 4; 0; 2 |];
   expect_ov re "abab" [| 0; 4; 0; 2 |]);
  (* Unset reference, single copy: default no match; with
     PCRE2_MATCH_UNSET_BACKREF (a compile option, read from poptions) it
     matches empty — oracle: no match / 0: "" with 1: <unset>. *)
  expect_nomatch (compile "(a)?\\1") "b";
  expect_ov
    (compile ~options:Options.match_unset_backref "(a)?\\1")
    "b" [| 0; 0; -1; -1 |];
  (* Zero-length reference under * must break the repeat loop instead of
     looping forever (the set-group length-0 continue, 5068) — oracle:
     0: x, 1: "". *)
  expect_ov (compile "()\\1*x") "x" [| 0; 1; 0; 0 |];
  (* Greedy star over a set reference with RM21 back-off releasing one
     copy — oracle: 0: aaa, 1: a. *)
  expect_ov (compile "(a)\\1*a") "aaa" [| 0; 3; 0; 1 |];
  (* OP_CRRANGE max 0 => infinity ({2,}) — oracle: 0: aaab / no match. *)
  (let re = compile "(a)\\1{2,}b" in
   expect_ov re "aaab" [| 0; 4; 0; 1 |];
   expect_nomatch re "aab");
  (* OP_CRPLUS / OP_CRQUERY forms — oracle: aab/ab both match for ?,
     + needs one copy. *)
  (let re = compile "(a)\\1?b" in
   expect_ov re "aab" [| 0; 3; 0; 1 |];
   expect_ov re "ab" [| 0; 2; 0; 1 |]);
  (let re = compile "(a)\\1+b" in
   expect_ov re "aab" [| 0; 3; 0; 1 |];
   expect_nomatch re "ab");
  (* Unset reference under a repeat: Lmin = 0 continues (5072-5073) —
     oracle: 0: b; Lmin > 0 fails in the min loop — oracle: no match —
     unless MATCH_UNSET_BACKREF also continues — oracle: 0: b. *)
  expect_ov (compile "(a)?\\1{0,3}b") "b" [| 0; 1; -1; -1 |];
  expect_nomatch (compile "(a)?\\1{2,3}b") "b";
  expect_ov
    (compile ~options:Options.match_unset_backref "(a)?\\1{2,3}b")
    "b" [| 0; 1; -1; -1 |];
  (* Duplicate names (OP_DNREF): the group-list scan uses the first SET
     group — oracle: aa -> 1: a (group 2 unset); bb -> 1: <unset>, 2: b;
     ab -> no match. *)
  (let re = compile "(?J)(?:(?<n>a)|(?<n>b))\\k<n>" in
   expect_ov re "aa" [| 0; 2; 0; 1; -1; -1 |];
   expect_ov re "bb" [| 0; 2; -1; -1; 0; 1 |];
   expect_nomatch re "ab");
  (* Partial sites. Single copy (CHECK_PARTIAL after Feptr = end_subject,
     5050-5051) — oracle: Partial match: aba (hard and soft) and ab. *)
  (let re = compile "(ab)\\1" in
   expect_partial ~options:Options.partial_hard re "aba" 0 3;
   expect_partial ~options:Options.partial_soft re "aba" 0 3;
   expect_partial ~options:Options.partial_hard re "ab" 0 2);
  (* The min loop's CHECK_PARTIAL (5084-5085) — oracle: Partial match:
     aaa. *)
  expect_partial ~options:Options.partial_hard (compile "(a)\\1{3}") "aaa" 0 3;
  (* Caseless repeat, fixed count: {2} = CRRANGE(2,2), so Lmin = Lmax and
     the arm exits at the Lmin == Lmax continue (5093); the partial fires
     in the min loop's CHECK_PARTIAL (5084-5085) — oracle: 0: abABab then
     Partial match: abABa. COVERAGE TODO: the maximize scan's hard-partial
     branch (5135-5139) needs Lmin < Lmax hitting subject end, e.g.
     (?i)(ab)\\1{2,4} on "abABa"-class input — pin via oracle when the M4
     possessive-ref work touches this region. *)
  let re = compile "(?i)(ab)\\1{2}" in
  expect_ov re "abABab" [| 0; 6; 0; 2 |];
  expect_partial ~options:Options.partial_hard re "abABa" 0 5

(* Lookaround, atomic groups and possessive brackets (this chunk): the
   OP_ASSERT*/OP_ASSERT*_NOT dispatch arms with RM3/RM4, OP_ASSERT_ACCEPT
   plumbing, OP_REVERSE/OP_VREVERSE (RM37), the assertion/ONCE ket actions
   (eptr restore + atomic backtrack discard), and the BRAPOS possessive
   protocol (RM8 + OP_KETRPOS). Whole compiled patterns through the
   [pcre2_match] driver; EVERY expected value below is pinned against the
   C oracle (pcre2test on the real 10.44 library). *)
let test_19 () =
  let compile pat =
    match Compile.pcre2_compile pat ~options:0 with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run ?(options = 0) (re : Compile.re) subj =
    let oveccount = re.Compile.top_bracket + 1 in
    let m =
      {
        ovector = Array.make (2 * oveccount) Frames.unset;
        oveccount;
        rc = 0;
        startchar = 0;
        leftchar = 0;
        rightchar = 0;
        mark = Frames.unset;
      }
    in
    let rc = pcre2_match re ~subject:subj ~start_offset:0 ~options m in
    (rc, m)
  in
  let expect_ov ?options re subj expected =
    match run ?options re subj with
    | rc, m ->
        assert (rc > 0);
        assert (Int.equal (Array.length m.ovector) (Array.length expected));
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch ?options re subj =
    match run ?options re subj with
    | rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let expect_partial ?options re subj s e =
    match run ?options re subj with
    | rc, m ->
        assert (Int.equal rc Errors.error_partial);
        assert (Int.equal m.ovector.(0) s);
        assert (Int.equal m.ovector.(1) e)
  in
  (* Positive lookahead with captures inside AND after: the frontier unit
     testinput1:28 — oracle: 0: abde, 1: de, 2: abd, 3: e (captures made
     inside a matched positive assertion persist: execution continues
     forward in the deeper frames). *)
  expect_ov (compile "^(?=ab(de))(abd)(e)") "abde" [| 0; 4; 2; 4; 0; 3; 3; 4 |];
  expect_ov (compile "(?=(a))a") "a" [| 0; 1; 0; 1 |];
  (* Negative lookahead — oracle: /(?!x)a/ on "a" -> 0: a. *)
  expect_ov (compile "(?!x)a") "a" [| 0; 1 |];
  (* Captures made inside a FAILED branch of a negative assertion roll
     back (they live in discarded deeper frames) — oracle: 0: ac,
     group 1 unset. *)
  expect_ov (compile "(?!(a)b)ac") "ac" [| 0; 2; -1; -1 |];
  (* Positive lookbehind (OP_REVERSE fixed step) — oracle: 0: c at 2. *)
  expect_ov (compile "(?<=ab)c") "abc" [| 2; 3 |];
  (* Negative lookbehind, incl. the "not enough characters before the
     start" NOMATCH inside the branch making the assertion succeed —
     oracle: 0: b / no match. *)
  (let re = compile "(?<!a)b" in
   expect_ov re "cb" [| 1; 2 |];
   expect_ov re "b" [| 0; 1 |];
   expect_nomatch re "ab");
  (* Lookahead and partial matching: SCHECK_PARTIAL fires inside the
     assertion — oracle: Partial match: x / 0: x. *)
  (let re = compile "x(?=y)" in
   expect_partial ~options:Options.partial_hard re "x" 0 1;
   expect_ov re "xy" [| 0; 1 |]);
  expect_partial ~options:Options.partial_hard (compile "(?=abc)") "ab" 0 2;
  (* Atomic group: no backing into the consumed run — oracle: no match /
     0: aab, and the captured run survives the ket — oracle: 1: aa. *)
  expect_nomatch (compile "(?>a+)ab") "aaab";
  expect_ov (compile "(?>a+)b") "aab" [| 0; 3 |];
  expect_ov (compile "(?>(a+))b") "aab" [| 0; 3; 0; 2 |];
  (* The ONCE ket's P->ecode adjustment: remaining branches within the
     atomic group are not tried — oracle: no match. *)
  expect_nomatch (compile "(?>a|ab)c") "abc";
  (* Possessive brackets (BRAPOS protocol): one committed iteration at a
     time — oracle: 0: aaab, 1: aaa / no match. *)
  (let re = compile "(a+)*+b" in
   expect_ov re "aaab" [| 0; 4; 0; 3 |];
   expect_nomatch re "aaac");
  (* CBRAPOS per-iteration capture, carried by the OP_KETRPOS frame copy —
     oracle: 0: aab, 1: a (the last iteration). *)
  expect_ov (compile "(a)*+b") "aab" [| 0; 3; 1; 2 |];
  (* Captures from distinct branches both survive across iterations —
     oracle: 0: abc, 1: a, 2: b. *)
  expect_ov (compile "(?:(a)|(b))*+c") "abc" [| 0; 3; 0; 1; 1; 2 |];
  (* The empty-iteration break (Feptr == Lstart_eptr skips to the end) —
     oracle: 0: b, 1: "" / 0: aab, 1: "" (after the aa iteration, the
     empty one). *)
  (let re = compile "(a*)*+b" in
   expect_ov re "b" [| 0; 1; 0; 0 |];
   expect_ov re "aab" [| 0; 3; 2; 2 |]);
  (* BRAPOSZERO zero-repeat: the group of /(?:a|ab)*+c/ matches zero times
     at offset 2 after the committed 'a' iteration kills offset 0 —
     oracle: 0: c. *)
  expect_ov (compile "(?:a|ab)*+c") "abc" [| 2; 3 |];
  (* Variable lookbehind (OP_VREVERSE + RM37): maximum length first —
     oracle: 0: x / no match (min 2 > 1 available). *)
  (let re = compile "(?<=a{2,4})x" in
   expect_ov re "aaax" [| 3; 4 |];
   expect_nomatch re "ax");
  (* Perl-compatible maximum-length rule for captures in a variable
     lookbehind — oracle: 0: x, 1: aaaa. *)
  expect_ov (compile "(?<=(a{2,4}))x") "aaaaax" [| 5; 6; 1; 5 |];
  (* Lookbehind captures survive; hard partial does not fire once the
     match completes — oracle: 0: d, 1: abc. *)
  expect_ov ~options:Options.partial_hard (compile "(?<=(abc))d") "abcd"
    [| 3; 4; 0; 3 |]

(* Conditionals and recursion (this chunk): the OP_COND/OP_SCOND
   condition opcodes (OP_CREF/OP_DNCREF/OP_RREF/OP_DNRREF/OP_FALSE/
   OP_TRUE), the assertion-condition protocol (RM5 + the GF_CONDASSERT
   ket return), OP_RECURSE with the RM11 branch loop and the recursion
   ket actions (whole-pattern end + recursed-group capture reinstate,
   i.e. Perl's captures-discard-on-exit semantics), RECURSELOOP (-52)
   detection with its PCRE2_DISABLE_RECURSELOOP_CHECK gate, OP_FAIL, and
   the quantified-recursion compile case. Whole compiled patterns through
   the [pcre2_match] driver (limit knobs via [match_internal]); EVERY
   expected value below is pinned against the C oracle (pcre2test on the
   real 10.44 library). *)
let test_20 () =
  let compile pat =
    match Compile.pcre2_compile pat ~options:0 with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run (re : Compile.re) subj =
    let oveccount = re.Compile.top_bracket + 1 in
    let m =
      {
        ovector = Array.make (2 * oveccount) Frames.unset;
        oveccount;
        rc = 0;
        startchar = 0;
        leftchar = 0;
        rightchar = 0;
        mark = Frames.unset;
      }
    in
    let rc = pcre2_match re ~subject:subj ~start_offset:0 ~options:0 m in
    (rc, m)
  in
  let expect_ov re subj expected =
    match run re subj with
    | rc, m ->
        assert (rc > 0);
        assert (Int.equal (Array.length m.ovector) (Array.length expected));
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch re subj =
    match run re subj with rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let expect_rc re subj code =
    match run re subj with rc, _ -> assert (Int.equal rc code)
  in
  (* Group-set condition (OP_CREF), both paths — oracle: aA -> 0: aA,
     1: a; bB -> 0: bB, 1: <unset>; aB and bA -> no match. *)
  (let re = compile "(?:(a)|b)(?(1)A|B)" in
   expect_ov re "aA" [| 0; 2; 0; 1 |];
   expect_ov re "bB" [| 0; 2; -1; -1 |];
   expect_nomatch re "aB";
   expect_nomatch re "bA");
  (* Unset optional group takes the false branch — oracle: 0: c,
     1: <unset>. *)
  (let re = compile "(a)?(?(1)b|c)" in
   expect_ov re "ab" [| 0; 2; 0; 1 |];
   expect_ov re "c" [| 0; 1; -1; -1 |]);
  (* Named-group condition (a non-duplicated name compiles to OP_CREF) —
     oracle: 0: ab, 1: a. *)
  expect_ov (compile "(?<n>a)(?(<n>)b|c)") "ab" [| 0; 2; 0; 1 |];
  (* Duplicate-name condition (the OP_DNCREF group-list scan) — oracle:
     aX -> 1: a, 2: <unset>; bX -> 1: <unset>, 2: b. *)
  (let re = compile "(?J)(?:(?<n>a)|(?<n>b))(?(<n>)X|Y)" in
   expect_ov re "aX" [| 0; 2; 0; 1; -1; -1 |];
   expect_ov re "bX" [| 0; 2; -1; -1; 0; 1 |]);
  (* Recursion condition (OP_RREF): false outside a recursion — oracle:
     0: x — and true inside one — oracle: 0: ba. *)
  expect_ov (compile "(?(R)r|x)") "x" [| 0; 1 |];
  expect_ov (compile "(?(R)a|b(?R))") "ba" [| 0; 2 |];
  (* Assertion conditions (GF_CONDASSERT frames + RM5; the positive kind
     rewrites Fecode past the assertion when true) — oracle: ab / x
     match, a fails; the negative kind picks the first branch on x. *)
  (let re = compile "(?(?=ab)ab|x)" in
   expect_ov re "ab" [| 0; 2 |];
   expect_ov re "x" [| 0; 1 |];
   expect_nomatch re "a");
  (let re = compile "(?(?!a)x|ab)" in
   expect_ov re "x" [| 0; 1 |];
   expect_ov re "ab" [| 0; 2 |]);
  (* DEFINE (OP_FALSE, single-branch skip) + named recursion — oracle:
     0: abab, 1: <unset>. *)
  expect_ov (compile "(?(DEFINE)(?<f>ab))(?&f)+") "abab" [| 0; 4; -1; -1 |];
  (* VERSION conditions compile to OP_TRUE — oracle: 0: yes. *)
  expect_ov (compile "(?(VERSION>=10)yes|no)") "yes" [| 0; 3 |];
  (* OP_SCOND: a repeated conditional group that might match an empty
     string descends a level (RM35) — oracle: 0: abc, 1: a / 0: ac,
     1: a. *)
  (let re = compile "(a)(?(1)b|)*c" in
   expect_ov re "abc" [| 0; 3; 0; 1 |];
   expect_ov re "ac" [| 0; 2; 0; 1 |]);
  (* Numbered recursion, nested calls — oracle: 0: aabb, 1: aabb; the
     anchored variant fails on an unbalanced subject. *)
  (let re = compile "^(a(?1)?b)$" in
   expect_ov re "aabb" [| 0; 4; 0; 4 |];
   expect_nomatch re "aab");
  (* Whole-pattern recursion: the OP_BRA ket action reinstates and
     carries on after the (?R) call — oracle: 0: abcabc. *)
  expect_ov (compile "abc(?R)?") "abcabc" [| 0; 6 |];
  (* Captures made inside a recursion are discarded on exit (Perl
     semantics, the recursed-group ket reinstate): group 2 is set only
     inside the (?&f) call, so \2 is unset outside and the reference
     fails — oracle: no match on both. *)
  (let re = compile "(?(DEFINE)(?<f>(a)))(?&f)\\2" in
   expect_nomatch re "a";
   expect_nomatch re "aa");
  (* The reinstate also restores the PREVIOUS captures: after (?1)
     returns, group 1 is unset again and is then captured by the
     top-level (a) — oracle: 0: aa, 1: a (the second 'a'). *)
  expect_ov (compile "(?1)(a)") "aa" [| 0; 2; 1; 2 |];
  (* Recursion in an alternation branch — oracle: 0: a, 1: a, 2: a. *)
  expect_ov (compile "((a)|(?1)b)") "ab" [| 0; 1; 0; 1; 0; 1 |];
  (* PCRE2_ERROR_RECURSELOOP (-52), a direct return: repeating the same
     group's recursion at the same subject position with the same last
     consulted character — oracle: error -52 for all three (the third
     recurses group 1 at position 0 twice via the second branch). *)
  expect_rc (compile "(?R)") "a" Errors.error_recurseloop;
  expect_rc (compile "((?1))") "a" Errors.error_recurseloop;
  expect_rc (compile "((a)|(?1)b)") "b" Errors.error_recurseloop;
  (* PCRE2_DISABLE_RECURSELOOP_CHECK: the -52 check is skipped and the
     runaway recursion is caught by the limits instead — oracle
     (pcre2test disable_recurseloop_check with depth_limit=100 /
     heap_limit=1): -53 / -63. *)
  (let code, top_bracket = compile_whole "(?R)" in
   (match
      match_internal ~moptions:Options.disable_recurseloop_check
        ~match_limit_depth:100 ~code ~top_bracket "a" 0
    with
   | rc, _ -> assert (Int.equal rc Errors.error_depthlimit));
   match
     match_internal ~moptions:Options.disable_recurseloop_check ~heap_limit:1
       ~code ~top_bracket "a" 0
   with
   | rc, _ -> assert (Int.equal rc Errors.error_heaplimit));
  (* OP_FAIL: the empty negative lookahead (?!) compiles to it — oracle:
     no match. *)
  expect_nomatch (compile "Z(?!)") "Z";
  (* Quantified recursion (compile-side replication + OP_BRA wrap +
     bracket-repeat fallthrough): fixed, lazy, ranged, possessive,
     query and star forms — oracle values from pcre2test. *)
  expect_ov (compile "(x)(?1){2}") "xxx" [| 0; 3; 0; 1 |];
  expect_ov (compile "(x)(?1){2}?y") "xxxy" [| 0; 4; 0; 1 |];
  expect_ov (compile "(x)(?1){2,4}y") "xxxxxy" [| 0; 6; 0; 1 |];
  expect_ov (compile "(x)(?1)++y") "xxxy" [| 0; 4; 0; 1 |];
  expect_ov (compile "(x)(?1)?y") "xy" [| 0; 2; 0; 1 |];
  expect_ov (compile "(x)(?1)*y") "xy" [| 0; 2; 0; 1 |]

(* Backtracking verbs (this chunk): the verb dispatch arms OP_MARK/
   OP_COMMIT(_ARG)/OP_PRUNE(_ARG)/OP_SKIP(_ARG)/OP_THEN(_ARG) with the
   RM12-RM19/RM36 resumes, the MATCH_THEN branch-scope checks (RM2/RM8/
   RM11), OP_CLOSE/OP_ACCEPT (incl. the in-recursion ACCEPT walk), and
   the driver's verb rc switch (MATCH_SKIP new-start, MATCH_SKIP_ARG
   ignore/re-run, PRUNE/THEN-as-NOMATCH, COMMIT bump suppression). EVERY
   expected value below is pinned against the C oracle (pcre2test on the
   real 10.44 library, via test/pcre2test/pcre2test_ml.exe
   --driver=oracle). *)
let test_21 () =
  let compile pat =
    match Compile.pcre2_compile pat ~options:0 with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run (re : Compile.re) subj =
    let oveccount = re.Compile.top_bracket + 1 in
    let m =
      {
        ovector = Array.make (2 * oveccount) Frames.unset;
        oveccount;
        rc = 0;
        startchar = 0;
        leftchar = 0;
        rightchar = 0;
        mark = Frames.unset;
      }
    in
    let rc = pcre2_match re ~subject:subj ~start_offset:0 ~options:0 m in
    (rc, m)
  in
  let expect_ov re subj expected =
    match run re subj with
    | rc, m ->
        assert (rc > 0);
        assert (Int.equal (Array.length m.ovector) (Array.length expected));
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch re subj =
    match run re subj with rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  (* The match-data mark decoded to its name: m.mark points past the
     length code unit of the verb-name item in the compiled code
     (pcre2_match.c:6341, Fecode + 2), so the length is mark[-1] — the
     protocol pcre2test's PCHARSV(mark, -1, ...) uses; "" = NULL (never
     set). *)
  let mark_name (re : Compile.re) (m : match_data) : string =
    if m.mark < 0 then ""
    else
      Bytes.sub_string re.Compile.code m.mark
        (Char.code (Bytes.get re.Compile.code (m.mark - 1)))
  in
  (* The frontier units (testinput1:833-835 region): verbs backtracked
     into via ( *FAIL) — oracle: no match for all three. *)
  expect_nomatch (compile "a+b?(*PRUNE)c+(*FAIL)") "aaabccc";
  expect_nomatch (compile "a+b?(*COMMIT)c+(*FAIL)") "aaabccc";
  expect_nomatch (compile "a+b?(*SKIP)c+(*FAIL)") "aaabcccaaabccc";
  (* COMMIT suppresses the bump-along (the driver's MATCH_COMMIT arm,
     7567-7571): /a+( *COMMIT)b/ fails outright on "aacaab" where /a+b/
     bumps along and matches at 3 — oracle: no match / 0: aab. COMMIT is
     never reached on starts that fail before it — oracle: 0: aab. *)
  expect_nomatch (compile "a+(*COMMIT)b") "aacaab";
  expect_ov (compile "a+b") "aacaab" [| 3; 6 |];
  expect_ov (compile "a+(*COMMIT)b") "xxaab" [| 2; 5 |];
  (* COMMIT inside a group: branch 2 is still tried when branch 1 fails
     BEFORE the COMMIT — oracle: 0: Cx, 1: C — but a backtrack through
     COMMIT kills everything — oracle: no match. *)
  (let re = compile "(A(*COMMIT)B|C)x" in
   expect_ov re "Cx" [| 0; 2; 0; 1 |];
   expect_nomatch re "ABC");
  (* MARK: both branch paths set the mark (OP_MARK + RM12), and a failed
     match passes back mb->nomatch_mark (driver 7741) — oracle: 0: a with
     MK: A / 0: b with MK: B / No match, mark = B. *)
  (let re = compile "(*MARK:A)a|(*MARK:B)b" in
   (match run re "a" with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (String.equal (mark_name re m) "A"));
   (match run re "b" with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (String.equal (mark_name re m) "B"));
   match run re "c" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) "B"));
  (* COMMIT_ARG sets the mark on the success path — oracle: 0: a with
     MK: X; on "b" the first-code-unit optimization means no attempt ever
     runs, so no mark — oracle: No match (no mark). *)
  (let re = compile "(*COMMIT:X)a" in
   (match run re "a" with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (String.equal (mark_name re m) "X"));
   match run re "b" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) ""));
  (* The classic PRUNE/THEN distinction: PRUNE abandons the whole start
     position (no second branch, no bump-along success), THEN only the
     current alternative — oracle: no match / 0: ac. *)
  expect_nomatch (compile "a(*PRUNE)b|ac") "ac";
  expect_ov (compile "a(*THEN)b|ac") "ac" [| 0; 2 |];
  (* PRUNE_ARG sets the nomatch mark — oracle: No match, mark = A on
     "ACB"; success mark on "ACAB" — oracle: 0: AB with MK: A. On "AC"
     the req-cu ('B') optimization means no attempt ever runs, so no
     mark at all — oracle: No match (no mark). *)
  (let re = compile "A(*PRUNE:A)B" in
   (match run re "ACB" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) "A"));
   (match run re "ACAB" with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (String.equal (mark_name re m) "A"));
   match run re "AC" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) ""));
  (* SKIP passes back the new start point (driver 7544-7549): the
     bump-along jumps to the SKIP position, so the a+c branch is never
     tried at a start where it could succeed — oracle: no match on both
     subjects. *)
  (let re = compile "aaaaa(*SKIP)b|a+c" in
   expect_nomatch re "aaaaac";
   expect_nomatch re "aaaaacaaaab");
  (* SKIP:name matched by a MARK (the RM12 interception turns
     MATCH_SKIP_ARG into MATCH_SKIP at the mark's subject position, which
     is not past this start, so it acts like NOMATCH — but the SKIP_ARG
     return means the second branch is never tried) — oracle: No match,
     mark = x. *)
  (let re = compile "(*MARK:x)a(*SKIP:x)b|a+c" in
   match run re "aaaac" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) "x"));
  (* SKIP:name with NO matching mark: MATCH_SKIP_ARG reaches the top and
     the driver re-runs the same start with mb->ignore_skip_arg set
     (7529-7539), so the SKIP is a no-op and the second branch matches —
     oracle: 0: aaaac. *)
  expect_ov (compile "a(*SKIP:x)b|a+c") "aaaac" [| 0; 5 |];
  (* THEN is bounded by its branch (the RM2 verb_ecode_ptr check): in a
     multi-branch group a THEN in the last branch just fails the group —
     oracle: no match on both the anchored and unanchored forms. *)
  expect_nomatch (compile "^(A(*THEN)B|C(*THEN)D)") "CB";
  expect_nomatch (compile "(?:A(*THEN)B|C(*THEN)D)") "CB";
  (* SKIP may not escape a recursion (the RM11 verb-range check turns it
     into NOMATCH for the entire recursion) — oracle: no match. *)
  expect_nomatch (compile "(?(DEFINE)(?<t>a|b(*SKIP)c))x(?&t)") "xb";
  (* ACCEPT in a recursion ends the recursion, not the whole match (the
     OP_ACCEPT walk over the GF_RECURSE frames), and its captures are
     discarded on exit like any recursion — oracle: 0: ax, group 1 unset
     (both shapes). *)
  expect_ov
    (compile "(?(DEFINE)(?<f>a(*ACCEPT)z))(?&f)x")
    "ax" [| 0; 2; -1; -1 |];
  expect_ov (compile "(?1)x(?:(a(*ACCEPT)zz)){0}") "ax" [| 0; 2; -1; -1 |];
  (* OP_CLOSE before a top-level ACCEPT writes the still-open captures
     from the chained group frames (P->eptr .. Feptr) — oracle: 0: AB,
     1: AB, 2: B, 3: <unset>. *)
  expect_ov
    (compile "(A(A|B(*ACCEPT)|C)D)(E)")
    "AB"
    [| 0; 2; 0; 2; 1; 2; -1; -1 |];
  (* Study's minlength feeds the driver's no-attempt break
     (pcre2_match.c:7382-7397): a subject shorter than the studied
     minimum NOMATCHes with no attempt at all, observable as MARK
     absence. /( *MARK:m)abcd/ has lower bound 4 (find_minlength), so on
     "ab" no attempt runs — oracle: No match (no mark) — while a real
     attempt sets the mark — oracle: 0: abcd with MK: m on "xabcd". *)
  (let re = compile "(*MARK:m)abcd" in
   (match run re "ab" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) ""));
   match run re "xabcd" with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (String.equal (mark_name re m) "m"));
  (* Backreference minlength (pcre2_study.c:548-596 expansion): lower
     bound of /( *MARK:r)(ab)\1/ is 4, so "aba" (3) gets no attempt —
     oracle: No match (no mark) / 0: abab with MK: r. *)
  (let re = compile "(*MARK:r)(ab)\\1" in
   (match run re "aba" with
   | rc, m ->
       assert (Int.equal rc Errors.error_nomatch);
       assert (String.equal (mark_name re m) ""));
   match run re "abab" with
   | rc, m ->
       assert (Int.equal rc 2 (* 0: abab, 1: ab *));
       assert (String.equal (mark_name re m) "r"));
  (* OP_MARK is in set_start_bits' SSB_FAIL list (pcre2_study.c:1023):
     /( *MARK:z)[ab]c/ gets NO start bitmap, so attempts DO run on "xc"
     and the nomatch mark is passed back — oracle: No match, mark = z.
     (Without the verb the [ab] bitmap would suppress the attempts; see
     the compile.ml study asserts.) *)
  let re = compile "(*MARK:z)[ab]c" in
  match run re "xc" with
  | rc, m ->
      assert (Int.equal rc Errors.error_nomatch);
      assert (String.equal (mark_name re m) "z")

(* UTF-8 match arms (this chunk): whole compiled UTF patterns through the
   [pcre2_match] driver — literal/CHARI/NOT decodes, wide char repeats
   (with the occu other-case buffer), class/NCLASS wide-char rules, the
   XCLASS runtime, type escapes over decoded characters, lookbehind
   character stepping, caseless backreferences via UCD, the driver's
   subject validity check, and the bump-along ACROSSCHAR. EVERY expected
   value below is pinned against the C oracle (pcre2test on the real
   10.44 library). *)
let test_22 () =
  let compile ?(options = 0) pat =
    match Compile.pcre2_compile pat ~options with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run ?(options = 0) (re : Compile.re) subj start =
    let oveccount = re.Compile.top_bracket + 1 in
    let m =
      {
        ovector = Array.make (2 * oveccount) Frames.unset;
        oveccount;
        rc = 0;
        startchar = 0;
        leftchar = 0;
        rightchar = 0;
        mark = Frames.unset;
      }
    in
    let rc = pcre2_match re ~subject:subj ~start_offset:start ~options m in
    (rc, m)
  in
  let expect_ov ?options re subj expected =
    match run ?options re subj 0 with
    | rc, m ->
        assert (rc > 0);
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch ?options re subj =
    match run ?options re subj 0 with
    | rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let u = Options.utf in
  (* OP_CHAR utf unit-compare + first-cu bump-along — oracle /é/utf on
     "café": 0: \x{e9} (bytes 3..5). *)
  expect_ov (compile ~options:u "\xc3\xa9") "caf\xc3\xa9" [| 3; 5 |];
  (* OP_CHARI utf, fc >= 128: other case via UCD — oracle /(?i)É/utf on
     "é": 0: \x{e9}. *)
  expect_ov (compile ~options:u "(?i)\xc3\x89") "\xc3\xa9" [| 0; 2 |];
  (* OP_ANY utf steps over the whole character — oracle /./utf on "é":
     0: \x{e9}. *)
  expect_ov (compile ~options:u ".") "\xc3\xa9" [| 0; 2 |];
  (* OP_NCLASS: a wide character matches a negated bitmap class — oracle
     /[^x]/utf on "é": 0: \x{e9} — and the class utf repeat — oracle
     /[^x]+/utf on "éàz": 0: \x{e9}\x{e0}z. *)
  expect_ov (compile ~options:u "[^x]") "\xc3\xa9" [| 0; 2 |];
  expect_ov (compile ~options:u "[^x]+") "\xc3\xa9\xc3\xa0z" [| 0; 5 |];
  (* OP_XCLASS runtime, hit and miss — oracle /[\x{100}-\x{200}]/utf:
     \x{150} matches, A and \x{201} do not. *)
  (let re = compile ~options:u "[\\x{100}-\\x{200}]" in
   expect_ov re "\xc5\x90" [| 0; 2 |];
   expect_nomatch re "A";
   expect_nomatch re "\xc8\x81");
  (* \d over a wide digit: ASCII semantics without UCP — oracle /\d/utf
     on U+0660 ARABIC-INDIC DIGIT ZERO: no match. *)
  expect_nomatch (compile ~options:u "\\d") "\xd9\xa0";
  (* \H over a decoded NBSP (multibyte HSPACE case) — oracle /\H\h/utf on
     "\x{a0}\x{a0}": no match. *)
  expect_nomatch (compile ~options:u "\\H\\h") "\xc2\xa0\xc2\xa0";
  (* \R matches the wide newline U+2028 under the default BSR — oracle
     /\R/utf on "\x{2028}z": 0: \x{2028}. *)
  expect_ov (compile ~options:u "\\R") "\xe2\x80\xa8z" [| 0; 3 |];
  (* Wide-char repeats: caseful — oracle /é+x/utf on "éééx": 0: whole —
     and caseless with the occu other-case encoding — oracle /(?i)é+x/utf
     on "éÉÉx": 0: whole. *)
  expect_ov
    (compile ~options:u "\xc3\xa9+x")
    "\xc3\xa9\xc3\xa9\xc3\xa9x" [| 0; 7 |];
  expect_ov
    (compile ~options:u "(?i)\xc3\xa9+x")
    "\xc3\xa9\xc3\x89\xc3\x89x" [| 0; 7 |];
  (* Fixed lookbehind steps CHARACTERS (OP_REVERSE utf) — oracle
     /(?<=é)x/utf on "éx": 0: x — and through a multi-byte '.' — oracle
     /(?<=a.b)x/utf on "aébx": 0: x. *)
  expect_ov (compile ~options:u "(?<=\xc3\xa9)x") "\xc3\xa9x" [| 2; 3 |];
  expect_ov (compile ~options:u "(?<=a.b)x") "a\xc3\xa9bx" [| 4; 5 |];
  (* Variable lookbehind (OP_VREVERSE utf + the RM37 FORWARDCHARTEST) —
     oracle /(?<=a\x{100}{2,4})x/utf on "a\x{100}\x{100}\x{100}x":
     0: x. *)
  expect_ov
    (compile ~options:u "(?<=a\\x{100}{2,4})x")
    "a\xc4\x80\xc4\x80\xc4\x80x" [| 7; 8 |];
  (* Caseless backreference via the UCD fold — oracle /(?i)(é)\1/utf on
     "éÉ": 0: \x{e9}\x{c9}, 1: \x{e9} — and a repeated wide backref —
     oracle /X(\x{e1})\1+Y/utf: XáááY matches, "Xááá áY" does not. *)
  expect_ov
    (compile ~options:u "(?i)(\xc3\xa9)\\1")
    "\xc3\xa9\xc3\x89" [| 0; 4; 0; 2 |];
  (let re = compile ~options:u "X(\\x{e1})\\1+Y" in
   expect_ov re "X\xc3\xa1\xc3\xa1\xc3\xa1Y" [| 0; 8; 1; 3 |];
   expect_nomatch re "X\xc3\xa1\xc3\xa1\xc3\xa1 \xc3\xa1Y");
  (* Subject UTF validity check (pcre2_match.c:6807-6929) — oracle:
     /x/utf on "\xc3" -> error -3 (1 byte missing at end) at offset 0;
     with offset=1 into "é" (a continuation byte, start_offset > 0) ->
     -36 BADUTFOFFSET; NO_UTF_CHECK skips the check entirely. *)
  (let re = compile ~options:u "x" in
   (match run re "\xc3" 0 with
   | rc, m ->
       assert (Int.equal rc Errors.error_utf8_err1);
       assert (Int.equal m.startchar 0));
   (match run re "\xc3\xa9x" 1 with
   | rc, _ -> assert (Int.equal rc Errors.error_badutfoffset));
   (match run ~options:Options.no_utf_check re "\xc3\xa9x" 1 with
   | rc, m ->
       assert (Int.equal rc 1);
       assert (Int.equal m.ovector.(0) 2));
   (* An isolated continuation byte at offset 0 is UTF8_ERR20, not
      BADUTFOFFSET (pcre2_match.c:6843-6845). *)
   match run re "\x80x" 0 with
   | rc, _ -> assert (Int.equal rc Errors.error_utf8_err20));
  (* The bump-along advances by whole characters (ACROSSCHAR,
     pcre2_match.c:7561-7563) — oracle /x/utf,no_start_optimize on "ééx":
     0: x at (4,5). *)
  expect_ov
    (compile ~options:(u lor Options.no_start_optimize) "x")
    "\xc3\xa9\xc3\xa9x" [| 4; 5 |]

(* PCRE2_MATCH_INVALID_UTF (this chunk): the driver's invalid-UTF
   fragment machinery — skipped_bad_start (pcre2_match.c:6828-6836), the
   fragment validation loop (6889-6928), FRAGMENT_RESTART (7139-7148) and
   the ENDLOOP fragment carry-on (7646-7701) — through whole compiled
   patterns. EVERY expected value below is pinned against the C oracle
   (pcre2test on the real 10.44 library, via
   test/pcre2test/pcre2test_ml.exe --driver=oracle). *)
let test_23 () =
  let compile ?(options = 0) pat =
    match Compile.pcre2_compile pat ~options with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run ?(options = 0) (re : Compile.re) subj start =
    let oveccount = re.Compile.top_bracket + 1 in
    let m =
      {
        ovector = Array.make (2 * oveccount) Frames.unset;
        oveccount;
        rc = 0;
        startchar = 0;
        leftchar = 0;
        rightchar = 0;
        mark = Frames.unset;
      }
    in
    let rc = pcre2_match re ~subject:subj ~start_offset:start ~options m in
    (rc, m)
  in
  let expect_ov_at ?options re subj start expected =
    match run ?options re subj start with
    | rc, m ->
        assert (rc > 0);
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch_at ?options re subj start =
    match run ?options re subj start with
    | rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let miu = Options.utf lor Options.match_invalid_utf in
  (* Match in a later fragment (the ENDLOOP carry-on advances past the
     bad code unit and restarts) — oracle /abc/utf,match_invalid_utf on
     "ab\x80abc": 0: abc at (3,6). MATCH_INVALID_UTF implies UTF at
     compile (pcre2_compile.c:10201-10203), so the bare-option pattern
     behaves identically. NOMATCH when the subject is only bad code
     units (skipped_bad_start consumes everything) — oracle: no match. *)
  (let re = compile ~options:miu "abc" in
   expect_ov_at re "ab\x80abc" 0 [| 3; 6 |];
   expect_nomatch_at re "\x80\x80\x80" 0);
  (let re = compile ~options:Options.match_invalid_utf "abc" in
   expect_ov_at re "ab\x80abc" 0 [| 3; 6 |]);
  (* A start offset inside a bad fragment is NOT BADUTFOFFSET under
     MATCH_INVALID_UTF: the skipped_bad_start loop (6828-6836) just
     advances past bad starting code units — oracle: 0: abc for both
     offset=2 into "ab\x80abc" (on the bad byte) and offset=1 into
     "\x80\x80abc" (inside a leading bad run). *)
  (let re = compile ~options:miu "abc" in
   expect_ov_at re "ab\x80abc" 2 [| 3; 6 |];
   expect_ov_at re "\x80\x80abc" 1 [| 2; 5 |]);
  (* ^ does not match at later fragment starts: OP_CIRC demands the true
     subject start (and NOTBOL is set for every fragment after the
     first, 7683/7695) — oracle /^X/utf,match_invalid_utf: no match on
     "\x80X" (skipped-bad start) and on "A\x80X" (carry-on fragment);
     the FIRST fragment does start the subject — oracle
     /^A/utf,match_invalid_utf on "A\x80X": 0: A. *)
  (let re = compile ~options:miu "^X" in
   expect_nomatch_at re "\x80X" 0;
   expect_nomatch_at re "A\x80X" 0);
  expect_ov_at (compile ~options:miu "^A") "A\x80X" 0 [| 0; 1 |];
  (* Partial interplay: a hard partial in the TERMINAL fragment is
     returned (ovector = the fragment tail) — oracle
     /.a/utf,match_invalid_utf on "b\xf0\x91\x88b" ph: Partial match: b
     at (4,5) — but a partial in a NON-terminal fragment is discarded by
     the carry-on (match_partial = NULL, 7665-7670) — oracle
     /.a$/utf,match_invalid_utf on "b\xf0\x91\x98" ph: no match. *)
  (match
     run ~options:Options.partial_hard
       (compile ~options:miu ".a")
       "b\xf0\x91\x88b" 0
   with
  | rc, m ->
      assert (Int.equal rc Errors.error_partial);
      assert (Int.equal m.ovector.(0) 4);
      assert (Int.equal m.ovector.(1) 5));
  (match
     run ~options:Options.partial_hard
       (compile ~options:miu ".a$")
       "b\xf0\x91\x98" 0
   with
  | rc, _ -> assert (Int.equal rc Errors.error_nomatch));
  (* Per-fragment NOTEOL: $ cannot match at a non-terminal fragment's
     end (fragment_options = NOTEOL, 6924 / NOTBOL|NOTEOL, 7695) but can
     at the true subject end — oracle /ab$/utf,match_invalid_utf:
     "ab\x80cdeab" -> 0: ab at (6,8); "ab\x80cde" -> no match. *)
  (let re = compile ~options:miu "ab$" in
   expect_ov_at re "ab\x80cdeab" 0 [| 6; 8 |];
   expect_nomatch_at re "ab\x80cde" 0);
  (* Empty-fragment advance in the carry-on loop (0xff is a character
     STARTING code unit for NOT_FIRSTCU but its fragment is empty, so
     the for(;;) advances again, 7687-7698) — oracle
     /X/utf,match_invalid_utf on "AB\xfe\xffXY": 0: X at (4,5). *)
  expect_ov_at (compile ~options:miu "X") "AB\xfe\xffXY" 0 [| 4; 5 |]

(* Unicode property matching (this chunk): OP_PROP/OP_NOTPROP + the
   property repeat strategies (min/minimize/maximize + RM208-RM217/
   RM222-RM225), XCL_PROP/XCL_NOTPROP items, and the UCP word
   boundaries, through whole compiled patterns. EVERY expected value
   below is pinned against the C oracle (pcre2test on the real 10.44
   library, via test/pcre2test/pcre2test_ml.exe --driver=oracle). *)
let test_24 () =
  let compile ?(options = 0) pat =
    match Compile.pcre2_compile pat ~options with
    | Error _ -> assert false
    | Ok re -> re
  in
  let run ?(options = 0) (re : Compile.re) subj =
    let oveccount = re.Compile.top_bracket + 1 in
    let m =
      {
        ovector = Array.make (2 * oveccount) Frames.unset;
        oveccount;
        rc = 0;
        startchar = 0;
        leftchar = 0;
        rightchar = 0;
        mark = Frames.unset;
      }
    in
    let rc = pcre2_match re ~subject:subj ~start_offset:0 ~options m in
    (rc, m)
  in
  let expect_ov ?options re subj expected =
    match run ?options re subj with
    | rc, m ->
        assert (rc > 0);
        Array.iteri (fun i e -> assert (Int.equal m.ovector.(i) e)) expected
  in
  let expect_nomatch ?options re subj =
    match run ?options re subj with
    | rc, _ -> assert (Int.equal rc Errors.error_nomatch)
  in
  let u = Options.utf in
  let uu = Options.utf lor Options.ucp in
  (* PT_GC greedy repeat over mixed scripts — oracle /\p{L}+/utf on
     "abcαβ123": 0: abc\x{3b1}\x{3b2}. *)
  expect_ov (compile ~options:u "\\p{L}+") "abc\xce\xb1\xce\xb2123" [| 0; 7 |];
  (* PT_SC single, hit and miss — oracle /\p{Greek}/utf: "XαY" -> 0:
     \x{3b1}; "XY" -> no match. *)
  (let re = compile ~options:u "\\p{Greek}" in
   expect_ov re "X\xce\xb1Y" [| 1; 3 |];
   expect_nomatch re "XY");
  (* PT_PC (particular category Sc = currency) — oracle /\p{Sc}/utf on
     "x€y": 0: \x{20ac}. *)
  expect_ov (compile ~options:u "\\p{Sc}") "x\xe2\x82\xacy" [| 1; 4 |];
  (* PT_SCX: U+0964 DEVANAGARI DANDA is script Common but Devanagari is
     in its Script Extensions — oracle /\p{scx:Deva}/utf on "\x{964}":
     0: \x{964}. *)
  expect_ov (compile ~options:u "\\p{scx:Deva}") "\xe0\xa5\xa4" [| 0; 3 |];
  (* PT_ALNUM (\p{Xan}) takes ARABIC-INDIC DIGIT ZERO but not '!' —
     oracle /\p{Xan}+/utf on "ab\x{660}12!": 0: ab\x{660}12. *)
  expect_ov (compile ~options:u "\\p{Xan}+") "ab\xd9\xa012!" [| 0; 6 |];
  (* PT_WORD (\p{Xwd}) takes '_' (Pc) and COMBINING GRAVE (Mn) — oracle
     /\p{Xwd}+/utf on "a_\x{300}b c": 0: a_\x{300}b. *)
  expect_ov (compile ~options:u "\\p{Xwd}+") "a_\xcc\x80b c" [| 0; 5 |];
  (* PT_CLIST: MICRO SIGN caselessly matches GREEK SMALL MU through its
     caseless set (the compiler turns multi-case (?i) singles into
     OP_PROP PT_CLIST) — oracle /(?i)µ/utf on "\x{3bc}": 0: \x{3bc} —
     and KELVIN SIGN is in k's set — oracle /(?i)k+/utf on "\x{212a}k":
     0: \x{212a}k. *)
  expect_ov (compile ~options:u "(?i)\xc2\xb5") "\xce\xbc" [| 0; 2 |];
  expect_ov (compile ~options:u "(?i)k+") "\xe2\x84\xaak" [| 0; 4 |];
  (* ...and LATIN SMALL LETTER LONG S in s's set (a PT_CLIST repeat) —
     oracle /(?i)s+/utf on "\x{17f}s": 0: \x{17f}s. *)
  expect_ov (compile ~options:u "(?i)s+") "\xc5\xbfs" [| 0; 3 |];
  (* OP_UCP_WORD_BOUNDARY: é is a word character under UCP, so no
     boundary before 'w' — oracle /\bword\b/ucp,utf: "\x{e9}word\x{e9}"
     -> no match; ".word." -> 0: word — and OP_NOT_UCP_WORD_BOUNDARY —
     oracle /\Bword/ucp,utf on "\x{e9}word": 0: word. *)
  (let re = compile ~options:uu "\\bword\\b" in
   expect_nomatch re "\xc3\xa9word\xc3\xa9";
   expect_ov re ".word." [| 1; 5 |]);
  expect_ov (compile ~options:uu "\\Bword") "\xc3\xa9word" [| 2; 6 |];
  (* PT_BIDICL — oracle /\p{bidi_class:R}/utf on HEBREW ALEF: 0:
     \x{5d0}. *)
  expect_ov (compile ~options:u "\\p{bidi_class:R}") "\xd7\x90" [| 0; 2 |];
  (* PT_BOOL — oracle /\p{Cased}+/utf on "aAα0": 0: aA\x{3b1}. *)
  expect_ov (compile ~options:u "\\p{Cased}+") "aA\xce\xb10" [| 0; 4 |];
  (* PT_PC exact + upto (the {2,} decomposition exercises the property
     min loop then the maximize scan) — oracle /\p{Lu}{2,}/utf on
     "xABCy": 0: ABC. *)
  expect_ov (compile ~options:u "\\p{Lu}{2,}") "xABCy" [| 1; 4 |];
  (* OP_NOTPROP repeat — oracle /\P{L}+/utf on "ab12!;cd": 0: 12!;. *)
  expect_ov (compile ~options:u "\\P{L}+") "ab12!;cd" [| 2; 6 |];
  (* Property minimize resumes (the RM-label loops) — oracle
     /\p{Han}*?X/utf on "\x{2e80}\x{3105}X": 0: X (bump-along past the
     non-Han) and /\p{Greek}+?X/utf on "ααX": 0: whole. *)
  expect_ov
    (compile ~options:u "\\p{Han}*?X")
    "\xe2\xba\x80\xe3\x84\x85X" [| 6; 7 |];
  expect_ov (compile ~options:u "\\p{Greek}+?X") "\xce\xb1\xce\xb1X" [| 0; 5 |];
  (* Property maximize backtrack (RM222): \p{L}+ eats β, then backs up
     one character — oracle /\p{L}+\x{3b2}/utf on "ααβ": 0: whole. *)
  expect_ov
    (compile ~options:u "\\p{L}+\xce\xb2")
    "\xce\xb1\xce\xb1\xce\xb2" [| 0; 6 |];
  (* PT_PC minimize with a bounded range — oracle /\p{Nd}{2,3}?\./utf
     on "\x{660}\x{661}\x{662}.": 0: whole. *)
  expect_ov
    (compile ~options:u "\\p{Nd}{2,3}?\\.")
    "\xd9\xa0\xd9\xa1\xd9\xa2." [| 0; 7 |];
  (* Property min loop count — oracle /\p{L}{3}/utf: "abαc" -> 0:
     ab\x{3b1}; "ab1c" -> no match. *)
  (let re = compile ~options:u "\\p{L}{3}" in
   expect_ov re "ab\xce\xb1c" [| 0; 4 |];
   expect_nomatch re "ab1c");
  (* XCL_PROP inside a class (bitmap + property item) — oracle
     /[\p{L}0]+/utf on "9aα0b8": 0: a\x{3b1}0b. *)
  expect_ov (compile ~options:u "[\\p{L}0]+") "9a\xce\xb10b8" [| 1; 6 |];
  (* Negated class with a property item — oracle /[^\p{L}]+/utf on
     "ab€0\x{300}cd": 0: \x{20ac}0\x{300}. *)
  expect_ov
    (compile ~options:u "[^\\p{L}]+")
    "ab\xe2\x82\xac0\xcc\x80cd" [| 2; 8 |];
  (* The XCLASS-only PT_PXPUNCT (UCP [[:punct:]]) takes FULLWIDTH
     EXCLAMATION MARK — oracle /[[:punct:]]+/ucp,utf on "a\x{ff01}!b":
     0: \x{ff01}!. *)
  expect_ov (compile ~options:uu "[[:punct:]]+") "a\xef\xbc\x81!b" [| 1; 5 |]

let tests =
  [
    Alcotest.test_case "interpreter 0" `Quick test_0;
    Alcotest.test_case "interpreter 1" `Quick test_1;
    Alcotest.test_case "interpreter 2" `Quick test_2;
    Alcotest.test_case "interpreter 3" `Quick test_3;
    Alcotest.test_case "interpreter 4" `Quick test_4;
    Alcotest.test_case "interpreter 5" `Quick test_5;
    Alcotest.test_case "interpreter 6" `Quick test_6;
    Alcotest.test_case "interpreter 7" `Quick test_7;
    Alcotest.test_case "interpreter 8" `Quick test_8;
    Alcotest.test_case "interpreter 9" `Quick test_9;
    Alcotest.test_case "interpreter 10" `Quick test_10;
    Alcotest.test_case "interpreter 11" `Quick test_11;
    Alcotest.test_case "interpreter 12" `Quick test_12;
    Alcotest.test_case "interpreter 13" `Quick test_13;
    Alcotest.test_case "interpreter 14" `Quick test_14;
    Alcotest.test_case "interpreter 15" `Quick test_15;
    Alcotest.test_case "interpreter 16" `Quick test_16;
    Alcotest.test_case "interpreter 17" `Quick test_17;
    Alcotest.test_case "interpreter 18" `Quick test_18;
    Alcotest.test_case "interpreter 19" `Quick test_19;
    Alcotest.test_case "interpreter 20" `Quick test_20;
    Alcotest.test_case "interpreter 21" `Quick test_21;
    Alcotest.test_case "interpreter 22" `Quick test_22;
    Alcotest.test_case "interpreter 23" `Quick test_23;
    Alcotest.test_case "interpreter 24" `Quick test_24;
  ]
