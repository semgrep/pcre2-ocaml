(* Engine module-initialization asserts for [Pcre2_engine.Compile], migrated
   verbatim from src/engine/compile.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Compile

(* PUT/GET and PUT2/GET2 roundtrips at the boundary values 0, 255, 256,
   65535, plus the exact big-endian byte layout and the INC cursor forms. *)
let test_0 () =
  let buf = Bytes.make 8 '\xaa' in
  List.iter
    (fun v ->
      put buf 0 v;
      assert (Int.equal (get buf 0) v);
      put2 buf 2 v;
      assert (Int.equal (get2 buf 2) v))
    [ 0; 255; 256; 65535 ];
  put buf 0 0x1234;
  assert (Char.equal (Bytes.get buf 0) '\x12');
  assert (Char.equal (Bytes.get buf 1) '\x34');
  let cursor = ref 0 in
  putinc buf cursor 65535;
  put2inc buf cursor 256;
  assert (Int.equal !cursor (Limits.link_size + Limits.imm2_size));
  assert (Int.equal (get buf 0) 65535);
  assert (Int.equal (get2 buf 2) 256)

(* Compile-context defaults (pcre2_context.c:133-145). *)
let test_1 () =
  assert (
    Int.equal default_compile_context.newline_convention Options.newline_lf);
  assert (Int.equal Options.newline_lf 2);
  assert (Int.equal default_compile_context.bsr_convention Options.bsr_unicode);
  assert (Int.equal Options.bsr_unicode 1);
  assert (Int.equal default_compile_context.parens_nest_limit 250);
  assert (Int.equal default_compile_context.max_varlookbehind 255);
  assert (Int.equal default_compile_context.extra_options 0)

(* make_block defaults (pcre2_compile.c:10238-10290) and the workspace
   overrun guard (pcre2_compile.c:5748-5759). *)
let test_2 () =
  let cb = make_block "abc" ~options:Options.caseless in
  assert (Int.equal cb.external_options Options.caseless);
  assert (Int.equal cb.end_pattern 3);
  assert (Int.equal compile_work_size 6000);
  assert (Int.equal cb.workspace_size compile_work_size);
  assert (Int.equal (Bytes.length cb.start_workspace) compile_work_size);
  assert (cb.start_code == cb.start_workspace) (* pre-compile aliasing *);
  assert (Int.equal cb.small_ref_offset.(9) Parse.pcre2_unset);
  assert (Int.equal cb.max_varlookbehind 255);
  assert (Int.equal cb.nltype Parse.nltype_fixed);
  assert (Int.equal (check_workspace_overflow cb 0) 0);
  assert (
    Int.equal
      (check_workspace_overflow cb
         (compile_work_size - work_size_safety_margin))
      0);
  assert (
    Int.equal
      (check_workspace_overflow cb
         (compile_work_size - work_size_safety_margin + 1))
      Errors.err86);
  assert (
    Int.equal (check_workspace_overflow cb (compile_work_size - 1)) Errors.err86);
  assert (Int.equal (check_workspace_overflow cb compile_work_size) Errors.err52);
  assert (Int.equal Errors.err52 152);
  assert (Int.equal Errors.err86 186)

(* first_significant_code on hand-built code fragments. *)
let test_3 () =
  (* OP_CREF is skipped whatever skipassert is: [CREF 2][CHAR 'x']. *)
  let buf = Bytes.make 8 '\000' in
  Bytes.set buf 0 (Char.chr Opcodes.op_cref);
  put2 buf 1 2;
  Bytes.set buf 3 (Char.chr Opcodes.op_char);
  Bytes.set buf 4 'x';
  assert (Int.equal (first_significant_code buf 0 ~skipassert:false) 3);
  assert (Int.equal (first_significant_code buf 0 ~skipassert:true) 3);
  (* \b is skipped only when skipassert is set. *)
  let buf = Bytes.make 4 '\000' in
  Bytes.set buf 0 (Char.chr Opcodes.op_word_boundary);
  Bytes.set buf 1 (Char.chr Opcodes.op_char);
  Bytes.set buf 2 'x';
  assert (Int.equal (first_significant_code buf 0 ~skipassert:false) 0);
  assert (Int.equal (first_significant_code buf 0 ~skipassert:true) 1);
  (* Two-branch negative lookahead, then 'y' — the do..while walks the
     OP_ALT chain to the final OP_KET and skips its length:
     [ASSERT_NOT L=5][CHAR x][ALT L=5][CHAR z][KET L=10][CHAR y]. *)
  let buf = Bytes.make 16 '\000' in
  Bytes.set buf 0 (Char.chr Opcodes.op_assert_not);
  put buf 1 5;
  Bytes.set buf 3 (Char.chr Opcodes.op_char);
  Bytes.set buf 4 'x';
  Bytes.set buf 5 (Char.chr Opcodes.op_alt);
  put buf 6 5;
  Bytes.set buf 8 (Char.chr Opcodes.op_char);
  Bytes.set buf 9 'z';
  Bytes.set buf 10 (Char.chr Opcodes.op_ket);
  put buf 11 10;
  Bytes.set buf 13 (Char.chr Opcodes.op_char);
  Bytes.set buf 14 'y';
  assert (Int.equal (first_significant_code buf 0 ~skipassert:true) 13);
  assert (Int.equal (first_significant_code buf 0 ~skipassert:false) 0)

(* find_dupname_details on a hand-built name table over pattern "aabbcc":
   entries "aa"->1, "aa"->3, "bb"->2 (entry size IMM2_SIZE + 2 + 1 = 5). *)
let test_4 () =
  let cb = make_block "aabbcc" ~options:0 in
  cb.name_entry_size <- Limits.imm2_size + 2 + 1;
  cb.names_found <- 3;
  cb.name_table <- Bytes.make (3 * cb.name_entry_size) '\000';
  let set_entry i group nm =
    let base = i * cb.name_entry_size in
    put2 cb.name_table base group;
    Bytes.blit_string nm 0 cb.name_table (base + Limits.imm2_size)
      (String.length nm)
  in
  set_entry 0 1 "aa";
  set_entry 1 3 "aa";
  set_entry 2 2 "bb";
  let index = ref (-1) in
  let count = ref (-1) in
  let errorcode = ref 0 in
  (* "aa" (pattern offset 0): first index 0, two duplicates; backref map
     and top_backref updated (pcre2_compile.c:5586-5596). *)
  assert (find_dupname_details ~name:0 ~length:2 index count errorcode cb);
  assert (Int.equal !index 0);
  assert (Int.equal !count 2);
  assert (Int.equal cb.backref_map ((1 lsl 1) lor (1 lsl 3)));
  assert (Int.equal cb.top_backref 3);
  (* "bb" (pattern offset 2): index 2, a single entry. *)
  assert (find_dupname_details ~name:2 ~length:2 index count errorcode cb);
  assert (Int.equal !index 2);
  assert (Int.equal !count 1);
  assert (Int.equal cb.top_backref 3);
  (* "cc" (pattern offset 4) is not in the table: internal error ERR53 with
     erroroffset at the name (pcre2_compile.c:5570-5578). *)
  assert (not (find_dupname_details ~name:4 ~length:2 index count errorcode cb));
  assert (Int.equal !errorcode Errors.err53);
  assert (Int.equal cb.erroroffset 4)

(* pcre2_compile.c:8101-8105 — "the ESC_values are arranged to be the same
   as the corresponding OP_values": META_ESCAPE emission (compile_branch)
   relies on this identity, so pin every pair that can reach it. ESC_dum
   pads the enum so the alignment holds across OP_ALLANY. *)
let test_5 () =
  assert (Int.equal Parse.esc_big_a Opcodes.op_sod);
  assert (Int.equal Parse.esc_big_g Opcodes.op_som);
  assert (Int.equal Parse.esc_big_k Opcodes.op_set_som);
  assert (Int.equal Parse.esc_big_b Opcodes.op_not_word_boundary);
  assert (Int.equal Parse.esc_b Opcodes.op_word_boundary);
  assert (Int.equal Parse.esc_big_d Opcodes.op_not_digit);
  assert (Int.equal Parse.esc_d Opcodes.op_digit);
  assert (Int.equal Parse.esc_big_s Opcodes.op_not_whitespace);
  assert (Int.equal Parse.esc_s Opcodes.op_whitespace);
  assert (Int.equal Parse.esc_big_w Opcodes.op_not_wordchar);
  assert (Int.equal Parse.esc_w Opcodes.op_wordchar);
  assert (Int.equal Parse.esc_big_n Opcodes.op_any);
  assert (Int.equal Parse.esc_dum Opcodes.op_allany);
  assert (Int.equal Parse.esc_big_c Opcodes.op_anybyte);
  assert (Int.equal Parse.esc_big_p Opcodes.op_notprop);
  assert (Int.equal Parse.esc_p Opcodes.op_prop);
  assert (Int.equal Parse.esc_big_r Opcodes.op_anynl);
  assert (Int.equal Parse.esc_big_h Opcodes.op_not_hspace);
  assert (Int.equal Parse.esc_h Opcodes.op_hspace);
  assert (Int.equal Parse.esc_big_v Opcodes.op_not_vspace);
  assert (Int.equal Parse.esc_v Opcodes.op_vspace);
  assert (Int.equal Parse.esc_big_x Opcodes.op_extuni);
  assert (Int.equal Parse.esc_big_z Opcodes.op_eodn);
  assert (Int.equal Parse.esc_z Opcodes.op_eod)

(* Repeat tables: chartypeoffset (pcre2_compile.c:687-691) and
   opcode_possessify (pcre2_compile.c:861-917) — length (OP_END..OP_CALLOUT
   inclusive) and the entries the repeat arm relies on. *)
let test_6 () =
  assert (Int.equal (Array.length chartypeoffset) 4);
  assert (Int.equal chartypeoffset.(0) 0);
  assert (Int.equal chartypeoffset.(1) 13);
  assert (Int.equal chartypeoffset.(2) 26);
  assert (Int.equal chartypeoffset.(3) 39);
  assert (Int.equal (Array.length opcode_possessify) (Opcodes.op_callout + 1));
  assert (Int.equal opcode_possessify.(Opcodes.op_star) Opcodes.op_posstar);
  assert (Int.equal opcode_possessify.(Opcodes.op_minstar) 0);
  assert (Int.equal opcode_possessify.(Opcodes.op_upto) Opcodes.op_posupto);
  assert (Int.equal opcode_possessify.(Opcodes.op_exact) 0);
  assert (Int.equal opcode_possessify.(Opcodes.op_stari) Opcodes.op_posstari);
  assert (Int.equal opcode_possessify.(Opcodes.op_notstar) Opcodes.op_notposstar);
  assert (
    Int.equal opcode_possessify.(Opcodes.op_notuptoi) Opcodes.op_notposuptoi);
  assert (
    Int.equal opcode_possessify.(Opcodes.op_typestar) Opcodes.op_typeposstar);
  assert (Int.equal opcode_possessify.(Opcodes.op_crrange) Opcodes.op_crposrange);
  assert (Int.equal opcode_possessify.(Opcodes.op_crposstar) 0);
  assert (Int.equal opcode_possessify.(Opcodes.op_class) 0);
  assert (Int.equal opcode_possessify.(Opcodes.op_ref) 0);
  assert (Int.equal opcode_possessify.(Opcodes.op_recurse) 0)

(* compile_branch on parsed streams produced by Parse.parse_regex, driven
   with pcre2_compile()'s two-phase protocol (pre-compile pass accumulating
   the length into the workspace, then the real pass into a buffer of
   exactly that size), reduced to a single branch. Every expected byte
   sequence and first/required-code-unit outcome below was traced against
   pcre2_compile.c:5636-8336. *)
let test_7 () =
  let parse ?(options = 0) ?(extra = 0) pat : Parse.parse_context =
    let cx = Parse.make_context pat in
    cx.Parse.external_options <- options;
    cx.Parse.extra_options <- extra;
    Parse.allocate_parsed_pattern cx ~options;
    let has_lookbehind = ref false in
    let prc = Parse.parse_regex cx ~options has_lookbehind in
    assert (Int.equal prc 0);
    cx
  in
  let make_cb ?(options = 0) pat (cx : Parse.parse_context) : compile_block =
    let cb = make_block pat ~options in
    cb.parsed_pattern <- cx.Parse.parsed_pattern;
    cb.parsed_pattern_end <- cx.Parse.parsed_pattern_end;
    cb
  in
  (* One compile_branch call from parsed-pattern index 0 / code offset 0.
     Returns (rc, errorcode, end code offset, end pptr, options out,
     firstcu, firstcuflags, reqcu, reqcuflags). *)
  let run_branch ?(options = 0) ?(extra = 0) (cb : compile_block) lengthptr =
    let optionsptr = ref options and xoptionsptr = ref extra in
    let codeptr = ref 0 and pptrptr = ref 0 and errorcodeptr = ref 0 in
    let firstcuptr = ref 0 and firstcuflagsptr = ref 0 in
    let reqcuptr = ref 0 and reqcuflagsptr = ref 0 in
    let rc =
      compile_branch optionsptr xoptionsptr codeptr pptrptr errorcodeptr
        firstcuptr firstcuflagsptr reqcuptr reqcuflagsptr None None cb lengthptr
    in
    ( rc,
      !errorcodeptr,
      !codeptr,
      !pptrptr,
      !optionsptr,
      !firstcuptr,
      !firstcuflagsptr,
      !reqcuptr,
      !reqcuflagsptr )
  in
  (* Two-pass driver: asserts the pre-compile length equals the real pass's
     emitted length (this catches most two-pass bugs), and that both passes
     agree on the return code. *)
  let compile2 ?(options = 0) ?(extra = 0) pat =
    let cx = parse ~options ~extra pat in
    let cb = make_cb ~options pat cx in
    let length = ref 0 in
    let rc1, err1, _, _, _, _, _, _, _ =
      run_branch ~options ~extra cb (Some length)
    in
    assert (Int.equal err1 0);
    cb.start_code <- Bytes.make !length '\000' (* real-phase buffer *);
    cb.req_varyopt <- 0 (* pcre2_compile.c:10676, between the phases *);
    let rc2, err2, endcode, endpptr, opts_out, fcu, fcuf, rcu, rcuf =
      run_branch ~options ~extra cb None
    in
    assert (Int.equal err2 0);
    assert (Int.equal rc1 rc2);
    assert (Int.equal endcode !length);
    (cb, rc2, endpptr, opts_out, fcu, fcuf, rcu, rcuf)
  in
  let assert_code (cb : compile_block) (expected : int list) =
    assert (Int.equal (Bytes.length cb.start_code) (List.length expected));
    List.iteri
      (fun i v -> assert (Int.equal (Char.code (Bytes.get cb.start_code i)) v))
      expected
  in
  (* Literals: OP_CHAR chain, firstcu/reqcu protocol, and the
     must-match-a-character return +1 (pcre2_compile.c:5800-5804,
     8226-8336). *)
  let cb, rc, endpptr, _, fcu, fcuf, rcu, rcuf = compile2 "abc" in
  assert (Int.equal rc 1);
  assert_code cb
    [ Opcodes.op_char; 0x61; Opcodes.op_char; 0x62; Opcodes.op_char; 0x63 ];
  assert (Int.equal cb.parsed_pattern.(endpptr) Parse.meta_end);
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x63);
  assert (Int.equal rcuf 0);

  (* Empty branch: may match an empty string (return -1), zero length. *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "" in
  assert (Int.equal rc (-1));
  assert_code cb [];
  assert (Int.equal fcuf req_unset);
  assert (Int.equal rcuf req_unset);

  (* A branch stops at META_ALT, leaving pptr on it
     (pcre2_compile.c:5816-5825). *)
  let cb, rc, endpptr, _, _, _, _, _ = compile2 "a|b" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_char; 0x61 ];
  assert (Int.equal cb.parsed_pattern.(endpptr) Parse.meta_alt);

  (* Caseless: OP_CHARI and the REQ_CASELESS flag, both from the options
     argument and from an in-pattern (?i) via META_OPTIONS
     (pcre2_compile.c:5713-5719, 6581-6587, 8274). *)
  let cb, _, _, _, fcu, fcuf, rcu, rcuf =
    compile2 ~options:Options.caseless "ab"
  in
  assert_code cb [ Opcodes.op_chari; 0x61; Opcodes.op_chari; 0x62 ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf req_caseless);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf req_caseless);
  let cb, _, _, opts_out, fcu, fcuf, _, rcuf = compile2 "(?i)x" in
  assert_code cb [ Opcodes.op_chari; 0x78 ];
  assert (Int.equal opts_out Options.caseless) (* passed back to caller *);
  assert (Int.equal fcu 0x78);
  assert (Int.equal fcuf req_caseless);
  assert (Int.equal rcuf req_unset) (* single char sets no reqcu *);
  let cb, _, _, opts_out, fcu, fcuf, rcu, rcuf = compile2 "(?i)a(?-i)b" in
  assert_code cb [ Opcodes.op_chari; 0x61; Opcodes.op_char; 0x62 ];
  assert (Int.equal opts_out 0);
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf req_caseless);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0) (* req_caseopt was reset by (?-i) *);

  (* META_DOT: OP_ANY, or OP_ALLANY under PCRE2_DOTALL; '.' first means no
     first code unit (pcre2_compile.c:5846-5857). *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "." in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_any ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, _, _, _ = compile2 "(?s)." in
  assert_code cb [ Opcodes.op_allany ];
  let cb, _, _, _, _, _, _, _ = compile2 ~options:Options.dotall "." in
  assert_code cb [ Opcodes.op_allany ];

  (* Anchors ^ $ and their multiline variants; non-multiline ^ leaves
     firstcu alone, OP_CIRCM disables it (pcre2_compile.c:5828-5844). *)
  let cb, rc, _, _, fcu, fcuf, _, rcuf = compile2 "^a$" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_circ; Opcodes.op_char; 0x61; Opcodes.op_doll ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, fcuf, rcu, rcuf =
    compile2 ~options:Options.multiline "^a$"
  in
  assert_code cb [ Opcodes.op_circm; Opcodes.op_char; 0x61; Opcodes.op_dollm ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcu 0x61) (* firstcu disabled, so 'a' becomes reqcu *);
  assert (Int.equal rcuf 0);

  (* Simple escapes: ESC value = OP value; \d-class escapes consume a
     character (rc +1) and disable firstcu (pcre2_compile.c:8113-8124,
     8205). *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "\\d\\W" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_digit; Opcodes.op_not_wordchar ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, _, _, _ = compile2 "\\h\\H\\v\\V\\N\\X" in
  assert_code cb
    [
      Opcodes.op_hspace;
      Opcodes.op_not_hspace;
      Opcodes.op_vspace;
      Opcodes.op_not_vspace;
      Opcodes.op_any;
      Opcodes.op_extuni;
    ];

  (* Zero-width escapes: no matched_char (rc -1); \b/\B/\A register a
     one-character lookbehind (pcre2_compile.c:8179-8202). *)
  let cb, rc, _, _, _, fcuf, _, _ = compile2 "\\A\\G\\K\\b\\B\\Z\\z" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_sod;
      Opcodes.op_som;
      Opcodes.op_set_som;
      Opcodes.op_word_boundary;
      Opcodes.op_not_word_boundary;
      Opcodes.op_eodn;
      Opcodes.op_eod;
    ];
  assert (Int.equal fcuf req_unset);
  assert (Int.equal cb.max_lookbehind 1);

  (* \R -> OP_ANYNL; \C -> OP_ALLANY in non-UTF mode, recording
     PCRE2_HASBKC (pcre2_compile.c:8184-8191). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "\\R\\C" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_anynl; Opcodes.op_allany ];
  assert (Int.equal (cb.external_flags land hasbkc) hasbkc);

  (* \b/\B under PCRE2_UCP become the UCP word-boundary opcodes unless
     PCRE2_EXTRA_ASCII_BSW (pcre2_compile.c:8193-8198). *)
  let cb, _, _, _, _, _, _, _ = compile2 ~options:Options.ucp "\\b\\B" in
  assert_code cb
    [ Opcodes.op_ucp_word_boundary; Opcodes.op_not_ucp_word_boundary ];
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:Options.ucp ~extra:Options.extra_ascii_bsw "\\b\\B"
  in
  assert_code cb [ Opcodes.op_word_boundary; Opcodes.op_not_word_boundary ];

  (* Explicit \r / \n set PCRE2_HASCRORLF (pcre2_compile.c:8278-8281). *)
  let cb, _, _, _, _, _, _, _ = compile2 "\n" in
  assert_code cb [ Opcodes.op_char; 0x0a ];
  assert (Int.equal (cb.external_flags land hascrorlf) hascrorlf);
  let cb, _, _, _, _, _, _, _ = compile2 "\\r" in
  assert_code cb [ Opcodes.op_char; 0x0d ];
  assert (Int.equal (cb.external_flags land hascrorlf) hascrorlf);

  (* PCRE2_AUTO_CALLOUT: OP_CALLOUT items (1 + 2*LINK_SIZE + 1 units:
     pattern offset, item length, callout number 255) around each item
     (pcre2_compile.c:7101-7111). *)
  let cb, rc, _, _, _, _, _, _ = compile2 ~options:Options.auto_callout "ab" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_callout;
      0;
      0;
      0;
      1;
      255;
      Opcodes.op_char;
      0x61;
      Opcodes.op_callout;
      0;
      1;
      0;
      1;
      255;
      Opcodes.op_char;
      0x62;
      Opcodes.op_callout;
      0;
      2;
      0;
      0;
      255;
    ];

  (* Explicit callouts: OP_CALLOUT for (?Cn) (pcre2_compile.c:7101-7111)
     and OP_CALLOUT_STR for (?C"text") (pcre2_compile.c:7114-7175) — the
     item carries the offset to the next pattern item, its length, the
     total item length, the offset one past the starting delimiter, then
     the copied string (starting delimiter included) and a terminating
     zero. String pinned with pcre2test 10.44 (Callout (4): "text"). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(?C1)abc" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_callout;
      0;
      5;
      0;
      1;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_char;
      0x62;
      Opcodes.op_char;
      0x63;
    ];
  let cb, rc, _, _, _, _, _, _ = compile2 "(?C\"text\")x" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_callout_str;
      0;
      10;
      0;
      1;
      0;
      15;
      0;
      4;
      0x22;
      0x74;
      0x65;
      0x78;
      0x74;
      0;
      Opcodes.op_char;
      0x78;
    ];

  (* \K inside an assertion: ERR99 unless PCRE2_EXTRA_ALLOW_LOOKAROUND_BSK
     (pcre2_compile.c:8159-8167). assert_depth is maintained by the
     lookaround arms (M4); simulate it directly. *)
  let cx = parse "\\K" in
  let cb = make_cb "\\K" cx in
  cb.assert_depth <- 1;
  let rc, err, _, _, _, _, _, _, _ = run_branch cb (Some (ref 0)) in
  assert (Int.equal rc 0);
  assert (Int.equal err Errors.err99);
  let cb = make_cb "\\K" cx in
  cb.assert_depth <- 1;
  let rc, err, _, _, _, _, _, _, _ =
    run_branch ~extra:Options.extra_allow_lookaround_bsk cb (Some (ref 0))
  in
  assert (Int.equal rc (-1));
  assert (Int.equal err 0);

  (* --- Character classes (compile_branch B, pcre2_compile.c:5860-6484,
     helpers 5206-5530). assert_class checks the opcode and all 256 bitmap
     bits of an emitted 33-unit bitmap class against a membership
     predicate. *)
  let assert_class (cb : compile_block) (expected_op : int)
      (member : int -> bool) =
    assert (Int.equal (Bytes.length cb.start_code) 33);
    assert (Int.equal (Char.code (Bytes.get cb.start_code 0)) expected_op);
    for c = 0 to 255 do
      let bit =
        Char.code (Bytes.get cb.start_code (1 + (c lsr 3)))
        land (1 lsl (c land 7))
      in
      assert (Bool.equal (not (Int.equal bit 0)) (member c))
    done
  in
  let is_digit c = c >= 0x30 && c <= 0x39 in
  let is_letter c = (c >= 0x41 && c <= 0x5a) || (c >= 0x61 && c <= 0x7a) in

  (* Empty classes under PCRE2_ALLOW_EMPTY_CLASS: [] always fails (OP_FAIL),
     [^] matches anything (OP_ALLANY) (pcre2_compile.c:5860-5873). *)
  let cb, rc, _, _, _, fcuf, _, rcuf =
    compile2 ~options:Options.allow_empty_class "[]"
  in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_fail ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:Options.allow_empty_class "[^]"
  in
  assert_code cb [ Opcodes.op_allany ];

  (* One-char classes: positive is a normal literal via NORMAL_CHAR_SET
     (pcre2_compile.c:5911-5915); negative is OP_NOT/OP_NOTI with no first
     code unit (5917-5946). *)
  let cb, rc, _, _, fcu, fcuf, _, _ = compile2 "[x]" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_char; 0x78 ];
  assert (Int.equal fcu 0x78);
  assert (Int.equal fcuf 0);
  let cb, _, _, _, fcu, fcuf, _, _ = compile2 "(?i)[x]" in
  assert_code cb [ Opcodes.op_chari; 0x78 ];
  assert (Int.equal fcu 0x78);
  assert (Int.equal fcuf req_caseless);
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "[^x]" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_not; 0x78 ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, _, _, _ = compile2 "(?i)[^x]" in
  assert_code cb [ Opcodes.op_noti; 0x78 ];
  (* No UCD caseset check for OP_NOTI without UTF/UCP: (?i)[^k] stays a
     plain OP_NOTI (pcre2_compile.c:5930-5931 requires utf||ucp). *)
  let cb, _, _, _, _, _, _, _ = compile2 "(?i)[^k]" in
  assert_code cb [ Opcodes.op_noti; 0x6b ];
  (* One-char class + \r/\n flag via the literal path. *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\r]" in
  assert_code cb [ Opcodes.op_char; 0x0d ];
  assert (Int.equal (cb.external_flags land hascrorlf) hascrorlf);

  (* Two-char case-partner classes become caseless single characters via
     CLASS_CASELESS_CHAR, with the temporary caseless setting reset
     afterwards (pcre2_compile.c:5949-5992, 8327-8334). *)
  let cb, rc, _, opts_out, fcu, fcuf, rcu, rcuf = compile2 "[aA]b" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_chari; 0x61; Opcodes.op_char; 0x62 ];
  assert (Int.equal opts_out 0) (* caseless was only instated temporarily *);
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf req_caseless);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0);
  let cb, _, _, _, _, _, _, _ = compile2 "[Aa]" in
  assert_code cb [ Opcodes.op_chari; 0x41 ];
  (* k/K (and s/S) have multi-character caseless sets (0x212a / 0x17f), so
     UCD_CASESET(c) != 0 blocks the optimization and a bitmap class is
     compiled (pcre2_compile.c:5962) — unless PCRE2_EXTRA_CASELESS_RESTRICT
     applies with both chars ASCII (5963-5964). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[kK]" in
  assert_class cb Opcodes.op_class (fun c ->
      Int.equal c 0x6b || Int.equal c 0x4b);
  let cb, _, _, _, _, _, _, _ =
    compile2 ~extra:Options.extra_caseless_restrict "[kK]"
  in
  assert_code cb [ Opcodes.op_chari; 0x6b ];
  let cb, _, _, _, _, _, _, _ = compile2 "[sS]" in
  assert_class cb Opcodes.op_class (fun c ->
      Int.equal c 0x73 || Int.equal c 0x53);

  (* General bitmap classes (pcre2_compile.c:6021-6371, 6467-6484):
     literals, ranges, negation (OP_NCLASS + inverted map), the caseless
     fcc closure, class escapes, and POSIX tables. *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "[abc]" in
  assert (Int.equal rc 1);
  assert_class cb Opcodes.op_class (fun c -> c >= 0x61 && c <= 0x63);
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  let cb, _, _, _, _, _, _, _ = compile2 "[^abc]" in
  assert_class cb Opcodes.op_nclass (fun c -> not (c >= 0x61 && c <= 0x63));
  let cb, _, _, _, _, _, _, _ = compile2 "[a-z]" in
  assert_class cb Opcodes.op_class (fun c -> c >= 0x61 && c <= 0x7a);
  (* Caseless closure of a range via the fcc table
     (pcre2_compile.c:5288-5294). *)
  let cb, _, _, _, _, _, _, _ = compile2 "(?i)[a-z]" in
  assert_class cb Opcodes.op_class is_letter;
  let cb, _, _, _, _, _, _, _ = compile2 "(?i)[^a-k]" in
  assert_class cb Opcodes.op_nclass (fun c ->
      not ((c >= 0x61 && c <= 0x6b) || (c >= 0x41 && c <= 0x4b)));
  (* An escaped range endpoint (META_RANGE_ESCAPED) fills the same bits. *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\x41-\\x5a]" in
  assert_class cb Opcodes.op_class (fun c -> c >= 0x41 && c <= 0x5a);
  (* Explicit \n inside a range sets PCRE2_HASCRORLF
     (pcre2_compile.c:6274-6291). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\n-\\r]" in
  assert_class cb Opcodes.op_class (fun c -> c >= 0x0a && c <= 0x0d);
  assert (Int.equal (cb.external_flags land hascrorlf) hascrorlf);

  (* Class escapes: \d ORs the digit map in; \D ORs its complement and
     flips the negation logic, so [\D] and [^\d] are both OP_NCLASS while
     [^\D] collapses back to OP_CLASS of the digits
     (pcre2_compile.c:6175-6183, 6473-6482). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\d]" in
  assert_class cb Opcodes.op_class is_digit;
  let cb, _, _, _, _, _, _, _ = compile2 "[\\D]" in
  assert_class cb Opcodes.op_nclass (fun c -> not (is_digit c));
  let cb, _, _, _, _, _, _, _ = compile2 "[^\\d]" in
  assert_class cb Opcodes.op_nclass (fun c -> not (is_digit c));
  let cb, _, _, _, _, _, _, _ = compile2 "[^\\D]" in
  assert_class cb Opcodes.op_class is_digit;
  let cb, _, _, _, _, _, _, _ = compile2 "[\\w]" in
  assert_class cb Opcodes.op_class (fun c ->
      is_letter c || is_digit c || Int.equal c 0x5f);
  let cb, _, _, _, _, _, _, _ = compile2 "[\\s]" in
  assert_class cb Opcodes.op_class (fun c ->
      (c >= 0x09 && c <= 0x0d) || Int.equal c 0x20);
  let cb, _, _, _, _, _, _, _ = compile2 "[^\\S]" in
  assert_class cb Opcodes.op_class (fun c ->
      (c >= 0x09 && c <= 0x0d) || Int.equal c 0x20);
  (* \h/\H use the [hv]space lists; wide list entries are clamped away in
     non-UTF 8-bit mode (pcre2_compile.c:6218-6238, 5297-5302). \H does NOT
     set should_flip_negation — its complement is added explicitly. *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\h]" in
  assert_class cb Opcodes.op_class (fun c ->
      Int.equal c 0x09 || Int.equal c 0x20 || Int.equal c 0xa0);
  let cb, _, _, _, _, _, _, _ = compile2 "[\\H]" in
  assert_class cb Opcodes.op_class (fun c ->
      not (Int.equal c 0x09 || Int.equal c 0x20 || Int.equal c 0xa0));
  let cb, _, _, _, _, _, _, _ = compile2 "[\\v]" in
  assert_class cb Opcodes.op_class (fun c ->
      (c >= 0x0a && c <= 0x0d) || Int.equal c 0x85);
  let cb, _, _, _, _, _, _, _ = compile2 "[^\\v]" in
  assert_class cb Opcodes.op_nclass (fun c ->
      not ((c >= 0x0a && c <= 0x0d) || Int.equal c 0x85));

  (* POSIX classes: base map +/- second map + tweaks
     (pcre2_compile.c:6098-6142, posix_class_maps 715-740). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[[:alpha:]]" in
  assert_class cb Opcodes.op_class is_letter;
  let cb, _, _, _, _, _, _, _ = compile2 "[[:^alpha:]]" in
  assert_class cb Opcodes.op_nclass (fun c -> not (is_letter c));
  let cb, _, _, _, _, _, _, _ = compile2 "[^[:^alpha:]]" in
  assert_class cb Opcodes.op_class is_letter;
  let cb, _, _, _, _, _, _, _ = compile2 "[[:alnum:]]" in
  assert_class cb Opcodes.op_class (fun c -> is_letter c || is_digit c);
  let cb, _, _, _, _, _, _, _ = compile2 "[[:word:]]" in
  assert_class cb Opcodes.op_class (fun c ->
      is_letter c || is_digit c || Int.equal c 0x5f);
  (* [:blank:] = space map minus vertical space (tweak 1). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[[:blank:]]" in
  assert_class cb Opcodes.op_class (fun c ->
      Int.equal c 0x09 || Int.equal c 0x20);
  (* Caseless [:upper:]/[:lower:] convert to alpha
     (pcre2_compile.c:6043-6048). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[[:upper:]]" in
  assert_class cb Opcodes.op_class (fun c -> c >= 0x41 && c <= 0x5a);
  let cb, _, _, _, _, _, _, _ = compile2 "(?i)[[:upper:]]" in
  assert_class cb Opcodes.op_class is_letter;
  (* [:ascii:] = print + cntrl maps. *)
  let cb, _, _, _, _, _, _, _ = compile2 "[[:ascii:]]" in
  assert_class cb Opcodes.op_class (fun c -> c <= 0x7f);
  let cb, _, _, _, _, _, _, _ = compile2 "[^[:ascii:]]" in
  assert_class cb Opcodes.op_nclass (fun c -> c > 0x7f);

  (* Mixed class content ORs together. *)
  let cb, _, _, _, _, _, _, _ = compile2 "[\\dA-Fa-f]" in
  assert_class cb Opcodes.op_class (fun c ->
      is_digit c || (c >= 0x41 && c <= 0x46) || (c >= 0x61 && c <= 0x66));

  (* --- Repeats (compile_branch C, pcre2_compile.c:7178-8011). The
     quantifier arm abolishes and rewrites a previous single-char or
     character-type item (OUTPUT_SINGLE_REPEAT, 7784-7904), appends a CR
     opcode after a class (7311-7344), and the possessive pass (7909-8003)
     then switches the repeat opcode to its POS variant via
     opcode_possessify. compile2 also checks the two-pass protocol: the
     pre-compile length must equal the real phase's emitted length. *)

  (* Hand-verified trace 1 — "a*" (pcre2_compile.c:7189-7194, 7215,
     7220-7226, 7794-7795, 7809-7811, 7888-7894): OP_CHAR 0x61 at offset 0
     is abolished (code = previous, 7795); min=0/max=unlimited emits
     OP_STAR + repeat_type(greedy_default=0) + op_type(0) = 33 at offset 0
     (7811 after 7804); the final fill re-emits 0x61 at offset 1
     (7890-7894). The zero-minimum repeat backs off to the zero* values
     (7220-7226): firstcuflags = zerofirstcuflags = REQ_NONE (set when 'a'
     was first, 8285), so the branch may match empty (rc -1); END_REPEAT
     ORs REQ_VARY into req_varyopt (8010). *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "a*" in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_star; 0x61 ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);
  assert (Int.equal cb.req_varyopt req_vary);

  (* Lazy quantifiers take repeat_type = greedy_non_default = 1
     (pcre2_compile.c:7240-7246): a+? = OP_MINPLUS, a?? = OP_MINQUERY.
     a+? must match at least one char (7210). *)
  let cb, rc, _, _, fcu, fcuf, _, rcuf = compile2 "a+?" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_minplus; 0x61 ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcuf req_unset);
  let cb, rc, _, _, _, _, _, _ = compile2 "a??" in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_minquery; 0x61 ];

  (* Hand-verified trace 2 — "a{2,5}" (pcre2_compile.c:7182-7187,
     7293-7308, 7838-7884, 7888-7894): min=2/max=5 from the two words
     after META_MINMAX. repeat_min > 1 sets reqcu = 0x61 with reqcuflags =
     cb->req_varyopt = 0 (7302-7305). {n,m} = EXACT then UPTO: OP_EXACT +
     op_type(0) = 41, PUT2(2) (7843-7844); fill 0x61 (7853-7857);
     repeat_max = 5-2 = 3 != 1, so OP_UPTO + repeat_type(0) = 39, PUT2(3)
     (7874-7883); final fill 0x61 (7890-7894). *)
  let cb, rc, _, _, fcu, fcuf, rcu, rcuf = compile2 "a{2,5}" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_exact; 0; 2; 0x61; Opcodes.op_upto; 0; 3; 0x61 ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x61);
  assert (Int.equal rcuf 0);
  assert (Int.equal cb.req_varyopt req_vary);

  (* "a{3,}": EXACT 3 then STAR (pcre2_compile.c:7843-7844, 7851-7857,
     7870-7871, 7890-7894). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{3,}" in
  assert_code cb [ Opcodes.op_exact; 0; 3; 0x61; Opcodes.op_star; 0x61 ];

  (* "a{4}" = {n,n}: just an EXACT (pcre2_compile.c:7838-7844 with the
     7851 middle skipped); reqvary = 0 (7215) leaves req_varyopt alone. *)
  let cb, rc, _, _, fcu, _, rcu, rcuf = compile2 "a{4}" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_exact; 0; 4; 0x61 ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal rcu 0x61);
  assert (Int.equal rcuf 0);
  assert (Int.equal cb.req_varyopt 0);

  (* "a{1,3}": min=1/max limited leaves the char item in place and appends
     OP_UPTO with max-1 (pcre2_compile.c:7825-7835), then the final fill
     (7890-7894). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{1,3}" in
  assert_code cb [ Opcodes.op_char; 0x61; Opcodes.op_upto; 0; 2; 0x61 ];

  (* "a{0,3}": zero minimum with a limited maximum is an UPTO
     (pcre2_compile.c:7813-7817). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{0,3}" in
  assert_code cb [ Opcodes.op_upto; 0; 3; 0x61 ];

  (* "a{1}" = {1,1}: the quantifier is ignored for non-parenthesized items
     (pcre2_compile.c:7277). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "a{1}" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_char; 0x61 ];
  assert (Int.equal cb.req_varyopt 0);

  (* Hand-verified trace 3 — "(?i)a*" (pcre2_compile.c:7278, 689-691):
     previous is OP_CHARI, so op_type = chartypeoffset[OP_CHARI - OP_CHAR]
     = OP_STARI - OP_STAR = 13, and 7811 emits OP_STAR + 0 + 13 = 46 =
     OP_STARI, then the fill 0x61. *)
  let cb, _, _, _, _, fcuf, _, _ = compile2 "(?i)a*" in
  assert_code cb [ Opcodes.op_stari; 0x61 ];
  assert (Int.equal fcuf req_none);

  (* (?i)x{3}: OP_EXACT + op_type(13) = OP_EXACTI (7843, "NB EXACT doesn't
     have repeat_type"); OP_CHARI previous adds REQ_CASELESS to the reqcu
     flags (7306). *)
  let cb, _, _, _, fcu, fcuf, rcu, rcuf = compile2 "(?i)x{3}" in
  assert_code cb [ Opcodes.op_exacti; 0; 3; 0x78 ];
  assert (Int.equal fcu 0x78);
  assert (Int.equal fcuf req_caseless);
  assert (Int.equal rcu 0x78);
  assert (Int.equal rcuf req_caseless);

  (* PCRE2_UNGREEDY flips the greedy default (pcre2_compile.c:5698-5699,
     7244,7249): (?U)a* = OP_MINSTAR, (?U)a*? = OP_STAR. *)
  let cb, _, _, _, _, _, _, _ = compile2 "(?U)a*" in
  assert_code cb [ Opcodes.op_minstar; 0x61 ];
  let cb, _, _, _, _, _, _, _ = compile2 "(?U)a*?" in
  assert_code cb [ Opcodes.op_star; 0x61 ];

  (* Character-type repeats use the TYPE opcodes: op_type = OP_TYPESTAR -
     OP_STAR = 52 (pcre2_compile.c:7773), and the type opcode itself is
     the final fill (7895-7897). *)
  let cb, rc, _, _, _, fcuf, _, _ = compile2 ".*" in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_typestar; Opcodes.op_any ];
  assert (Int.equal fcuf req_none);
  let cb, rc, _, _, _, _, _, _ = compile2 ".+?" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_typeminplus; Opcodes.op_any ];
  let cb, _, _, _, _, _, _, _ = compile2 "(?s).*" in
  assert_code cb [ Opcodes.op_typestar; Opcodes.op_allany ];
  let cb, rc, _, _, _, _, _, _ = compile2 "\\d+" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_typeplus; Opcodes.op_digit ];
  let cb, _, _, _, _, _, _, _ = compile2 "\\W*?" in
  assert_code cb [ Opcodes.op_typeminstar; Opcodes.op_not_wordchar ];
  let cb, _, _, _, _, _, _, _ = compile2 "\\d{2,4}" in
  assert_code cb
    [
      Opcodes.op_typeexact;
      0;
      2;
      Opcodes.op_digit;
      Opcodes.op_typeupto;
      0;
      2;
      Opcodes.op_digit;
    ];

  (* Negated one-char classes repeat through the NOT opcodes: op_type =
     chartypeoffset[OP_NOT - OP_CHAR] = 26, chartypeoffset[OP_NOTI -
     OP_CHAR] = 39 (pcre2_compile.c:689-691, 7278). *)
  let cb, rc, _, _, _, fcuf, _, _ = compile2 "[^x]*" in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_notstar; 0x78 ];
  assert (Int.equal fcuf req_none);
  let cb, rc, _, _, _, _, _, _ = compile2 "(?i)[^x]+?" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_notminplusi; 0x78 ];

  (* Class repeats: the CR opcode goes after the 33-unit bitmap item
     (pcre2_compile.c:7311-7344). Check the opcode + full bitmap + repeat
     tail. *)
  let assert_class_with_tail (cb : compile_block) (expected_op : int)
      (member : int -> bool) (tail : int list) =
    assert (Int.equal (Bytes.length cb.start_code) (33 + List.length tail));
    assert (Int.equal (Char.code (Bytes.get cb.start_code 0)) expected_op);
    for c = 0 to 255 do
      let bit =
        Char.code (Bytes.get cb.start_code (1 + (c lsr 3)))
        land (1 lsl (c land 7))
      in
      assert (Bool.equal (not (Int.equal bit 0)) (member c))
    done;
    List.iteri
      (fun i v ->
        assert (Int.equal (Char.code (Bytes.get cb.start_code (33 + i))) v))
      tail
  in
  let is_lower c = c >= 0x61 && c <= 0x7a in

  (* Hand-verified trace 4 — "[a-z]{2,4}" (pcre2_compile.c:7337-7343):
     previous is OP_CLASS (110) + the 32-byte bitmap for a-z; {2,4} is
     neither *, + nor ?, so OP_CRRANGE + repeat_type(0) = 104 is appended,
     then PUT2INC(repeat_min=2) and PUT2INC(repeat_max=4), each big-endian
     over two code units: tail = 104,0,2,0,4 at offsets 33..37. *)
  let cb, rc, _, _, _, _, _, _ = compile2 "[a-z]{2,4}" in
  assert (Int.equal rc 1);
  assert_class_with_tail cb Opcodes.op_class is_lower
    [ Opcodes.op_crrange; 0; 2; 0; 4 ];
  let cb, _, _, _, _, _, _, _ = compile2 "[a-z]*" in
  assert_class_with_tail cb Opcodes.op_class is_lower [ Opcodes.op_crstar ];
  let cb, _, _, _, _, _, _, _ = compile2 "[a-z]+?" in
  assert_class_with_tail cb Opcodes.op_class is_lower [ Opcodes.op_crminplus ];
  (* Unlimited max in a CRRANGE uses the 2-byte encoding 0
     (pcre2_compile.c:7341). *)
  let cb, _, _, _, _, _, _, _ = compile2 "[a-z]{2,}" in
  assert_class_with_tail cb Opcodes.op_class is_lower
    [ Opcodes.op_crrange; 0; 2; 0; 0 ];
  let cb, _, _, _, _, _, _, _ = compile2 "[^ab]??" in
  assert_class_with_tail cb Opcodes.op_nclass
    (fun c -> not (Int.equal c 0x61 || Int.equal c 0x62))
    [ Opcodes.op_crminquery ];

  (* Possessive quantifiers (pcre2_compile.c:7232-7238, 7909-8003):
     repeat_type is forced greedy and the emitted repeat opcode is
     switched to its POS variant via opcode_possessify (7986-7987). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a*+" in
  assert_code cb [ Opcodes.op_posstar; 0x61 ];
  let cb, _, _, _, _, _, _, _ = compile2 "a++" in
  assert_code cb [ Opcodes.op_posplus; 0x61 ];
  let cb, _, _, _, _, _, _, _ = compile2 "a?+" in
  assert_code cb [ Opcodes.op_posquery; 0x61 ];
  let cb, _, _, _, _, _, _, _ = compile2 "[a-z]*+" in
  assert_class_with_tail cb Opcodes.op_class is_lower [ Opcodes.op_crposstar ];
  let cb, _, _, _, _, _, _, _ = compile2 "\\d++" in
  assert_code cb [ Opcodes.op_typeposplus; Opcodes.op_digit ];

  (* Hand-verified trace 5 — "a{2,3}+" (pcre2_compile.c:7838-7878,
     7931-7954, 7971-7987): the repeat compiles as EXACT 2 'a' then, since
     repeat_max - repeat_min = 1, OP_QUERY + repeat_type(0) 'a'
     (7875-7878). The possessive pass starts at tempcode = previous, skips
     the EXACT item (op_lengths[OP_EXACT] = 4, 7945-7949) to the OP_QUERY;
     len = 2 > 0, and opcode_possessify[OP_QUERY] = OP_POSQUERY replaces
     it in place (7986-7987). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{2,3}+" in
  assert_code cb [ Opcodes.op_exact; 0; 2; 0x61; Opcodes.op_posquery; 0x61 ];

  (* "a{3}+": possessifying an EXACT has no effect — after skipping the
     EXACT item, tempcode == code, len = 0, nothing is done
     (pcre2_compile.c:7925-7929, 7971-7978). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{3}+" in
  assert_code cb [ Opcodes.op_exact; 0; 3; 0x61 ];

  (* "a{1,2}+": the char item stays (7831), OP_UPTO 1 follows; the skip
     over OP_CHAR (op_lengths = 2) lands on the UPTO, which possessifies
     to OP_POSUPTO (7941-7949, 7986-7987). *)
  let cb, _, _, _, _, _, _, _ = compile2 "a{1,2}+" in
  assert_code cb [ Opcodes.op_char; 0x61; Opcodes.op_posupto; 0; 1; 0x61 ];

  (* A quantified empty-class OP_FAIL is ignored (pcre2_compile.c:
     7346-7352), but END_REPEAT still ORs the REQ_VARY in (8010). *)
  let cb, rc, _, _, _, _, _, _ =
    compile2 ~options:Options.allow_empty_class "[]*"
  in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_fail ];
  assert (Int.equal cb.req_varyopt req_vary);

  (* The zerofirstcu/zeroreqcu interplay across a zero-min repeat
     (pcre2_compile.c:7220-7226): in "a?b" the backoff resets firstcu to
     "none" and reqcu to unset, then 'b' becomes the required unit with
     the REQ_VARY flag from req_varyopt (8323). In "ab*c" the backoff
     restores firstcu = 'a' (zerofirstcu was saved at 8316-8317). *)
  let cb, rc, _, _, _, fcuf, rcu, rcuf = compile2 "a?b" in
  assert (Int.equal rc 1);
  assert_code cb [ Opcodes.op_query; 0x61; Opcodes.op_char; 0x62 ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf req_vary);
  let cb, _, _, _, fcu, fcuf, rcu, rcuf = compile2 "ab*c" in
  assert_code cb
    [ Opcodes.op_char; 0x61; Opcodes.op_star; 0x62; Opcodes.op_char; 0x63 ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x63);
  assert (Int.equal rcuf req_vary);

  (* {0} repeats make code go backwards (the item is emitted, then
     dropped): the pre-compile length keeps the dropped item
     (pcre2_compile.c:5761-5767 "don't ever reduce the length"), so the
     real phase emits no more than the estimate rather than exactly it.
     Char items pass through OUTPUT_SINGLE_REPEAT's max == 0 exit (7800);
     classes through the class arm's (7324-7328). *)
  let compile2_shrink ?(options = 0) pat expected =
    let cx = parse ~options pat in
    let cb = make_cb ~options pat cx in
    let length = ref 0 in
    let rc1, err1, _, _, _, _, _, _, _ = run_branch ~options cb (Some length) in
    assert (Int.equal err1 0);
    cb.start_code <- Bytes.make !length '\000';
    cb.req_varyopt <- 0 (* pcre2_compile.c:10676, between the phases *);
    let rc2, err2, endcode, _, _, _, _, _, _ = run_branch ~options cb None in
    assert (Int.equal err2 0);
    assert (Int.equal rc1 rc2);
    assert (endcode <= !length);
    assert (Int.equal endcode (List.length expected));
    List.iteri
      (fun i v -> assert (Int.equal (Char.code (Bytes.get cb.start_code i)) v))
      expected
  in
  compile2_shrink "a{0}b" [ Opcodes.op_char; 0x62 ];
  compile2_shrink "[ab]{0}c" [ Opcodes.op_char; 0x63 ];

  (* --- Groups (compile_branch D + compile_regex,
     pcre2_compile.c:6806-7015, 8090-8098, 8345-8646) and bracket repeats
     (pcre2_compile.c:7424-7751). Every byte sequence below was verified
     against `pcre2test -q` with the fullbincode modifier on the real
     10.44 library (offsets shifted by the driver's 3-unit outer Bra,
     which is not part of compile_branch output). *)

  (* "(a)": OP_CBRA + link + IMM2 group number, closed by compile_regex's
     OP_KET whose link equals the whole bracketed length
     (pcre2_compile.c:8093-8098, 8458, 8591-8595). The group sets firstcu
     = 'a' (6965-6971) but no reqcu: the subfirstcu-to-subreqcu conversion
     (6981-6985) applies only when firstcu was already set, and the
     subpattern itself set none (verified: pcre2test -I shows no "Last
     code unit"). *)
  let cb, rc, endpptr, _, fcu, fcuf, _, rcuf = compile2 "(a)" in
  assert (Int.equal rc 1);
  assert_code cb
    [ Opcodes.op_cbra; 0; 7; 0; 1; Opcodes.op_char; 0x61; Opcodes.op_ket; 0; 7 ];
  assert (Int.equal cb.parsed_pattern.(endpptr) Parse.meta_end);
  assert (Int.equal cb.lastcapture 1) (* pcre2_compile.c:8097 *);
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcuf req_unset);

  (* "(?:ab)": non-capturing OP_BRA, no IMM2 (pcre2_compile.c:6806-6808). *)
  let cb, rc, _, _, fcu, fcuf, rcu, rcuf = compile2 "(?:ab)" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_bra;
      0;
      7;
      Opcodes.op_char;
      0x61;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      7;
    ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0);

  (* "(a)(b)": sequential capture numbers; the second group's firstcu
     becomes the branch reqcu via the subfirstcu-to-subreqcu conversion
     (pcre2_compile.c:6977-6994). *)
  let cb, rc, _, _, fcu, fcuf, rcu, rcuf = compile2 "(a)(b)" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_cbra;
      0;
      7;
      0;
      2;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      7;
    ];
  assert (Int.equal cb.lastcapture 2);
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0);

  (* "((a))": nested groups; each KET link spans its own bracket
     (pcre2_compile.c:8594). *)
  let cb, rc, _, _, fcu, _, _, _ = compile2 "((a))" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      15;
      0;
      1;
      Opcodes.op_cbra;
      0;
      7;
      0;
      2;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      15;
    ];
  assert (Int.equal fcu 0x61);

  (* "(a|b)": OP_ALT chain. While open, each ALT's link points back to the
     previous branch (pcre2_compile.c:8633-8636); at the close the chain
     is reversed so the BRA/ALT links point forward (8578-8589). Differing
     branch firstcus abandon the group's firstcu (8528-8543) and the
     differing reqcus then abandon its reqcu (8555-8559): the group
     reports REQ_NONE for both, so the outer branch takes firstcuflags =
     REQ_NONE (6973) and leaves reqcuflags REQ_UNSET (6990 needs
     subreqcuflags < REQ_NONE). *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "(a|b)" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_alt;
      0;
      5;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      12;
    ];
  assert (Int.equal fcuf req_none);
  assert (Int.equal rcuf req_unset);

  (* "()": an empty group may match empty (group_return -1, so no
     matched_char at 6853) and leaves firstcu unset (6965 requires
     subfirstcuflags != REQ_UNSET). *)
  let cb, rc, _, _, _, fcuf, _, rcuf = compile2 "()" in
  assert (Int.equal rc (-1));
  assert_code cb [ Opcodes.op_cbra; 0; 5; 0; 1; Opcodes.op_ket; 0; 5 ];
  assert (Int.equal fcuf req_unset);
  assert (Int.equal rcuf req_unset);

  (* Option changes inside a group do not escape it: compile_regex takes
     options by value (pcre2_compile.c:8378,8497), so (?-i) inside the
     group leaves the outer caseless setting intact. *)
  let cb, _, _, opts_out, _, _, _, _ = compile2 "(?i)((?-i)a)b" in
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_chari;
      0x62;
    ];
  assert (Int.equal opts_out Options.caseless);

  (* --- Bracket repeats (pcre2_compile.c:7424-7751). *)

  (* Hand-verified trace 6 — "(a)*" (pcre2_compile.c:7479-7510,
     7678-7691, 7745-7747): min 0/max unlimited moves the 10-unit group up
     by one (memmove, 7501), emits OP_BRAZERO + repeat_type(0) at the old
     start (7509), and the unlimited-max branch locates ketcode = code-3
     and bracode = ketcode - GET(ketcode,1) = the CBRA (7680-7681);
     group_return = +1 means no SBRA conversion (7705), non-possessive so
     *ketcode = OP_KETRMAX + 0 (7747). Zero minimum backs firstcu off to
     REQ_NONE (7220-7226 via the group's zerofirstcuflags from 6974). *)
  let cb, rc, _, _, _, fcuf, _, _ = compile2 "(a)*" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_brazero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrmax;
      0;
      7;
    ];
  assert (Int.equal fcuf req_none);

  (* "(a)*?": lazy repeat_type(1) selects OP_BRAMINZERO (7509) and
     OP_KETRMIN (7747). *)
  let cb, _, _, _, _, _, _, _ = compile2 "(a)*?" in
  assert_code cb
    [
      Opcodes.op_braminzero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrmin;
      0;
      7;
    ];

  (* "(a)+": min 1 needs no BRAZERO and no replication; just the KETRMAX
     conversion (7544 false, 7592 false, 7747). Like "(a)", firstcu only
     (min 1 does not reach the 7569 promotion, which needs min > 1). *)
  let cb, rc, _, _, fcu, fcuf, _, rcuf = compile2 "(a)+" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrmax;
      0;
      7;
    ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcuf req_unset);

  (* "(a)?" = {0,1}: BRAZERO inserted, repeat_max-- -> 0, so the common
     limited-max code adds no copies and the KET stays OP_KET
     (7499-7510, 7535, 7616 zero iterations). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a)?" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_brazero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
    ];

  (* "(?:a)?": same shape over a non-capturing bracket. *)
  let cb, _, _, _, _, _, _, _ = compile2 "(?:a)?" in
  assert_code cb
    [
      Opcodes.op_brazero;
      Opcodes.op_bra;
      0;
      5;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      5;
    ];

  (* Hand-verified trace 7 — "(a){2}" (pcre2_compile.c:7544-7583): min 2
     replicates the group once via memcpy (7574-7578); repeat_max -=
     repeat_min leaves 0, so nothing further. In the pre-compile phase the
     replication is pure length arithmetic: delta = (repeat_min-1) *
     length_prevgroup (7550-7561); compile2 checks the two phases agree. *)
  let cb, rc, _, _, fcu, _, rcu, _ = compile2 "(a){2}" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
    ];
  assert (Int.equal fcu 0x61);
  assert (Int.equal rcu 0x61);

  (* Hand-verified trace 8 — "(a){2,4}" (pcre2_compile.c:7574-7583,
     7616-7649): two mandatory copies (offsets 0,10), then repeat_max = 2
     optional copies compiled countdown: i=2 emits BRAZERO(20) + BRA(21)
     with its link field (22-23) holding the chain link pro tem (0 =
     chain end) + copy(24); i=1 emits BRAZERO(34) + copy(35). The
     chain-through pass then walks bralink: linkoffset = code(45) -
     bralink(22) + 1 = 24, bra = 21, emits OP_KET(45) with link 24 and
     back-fills PUT(bra+1, 24) (7639-7649). Pre-compile counts delta =
     repeat_max*(length_prevgroup + 1 + 2 + 2*LINK_SIZE) - (2+2*LINK_SIZE)
     (7600-7611): 2*17 - 6 = 28 = the 28 units emitted after the two
     mandatory copies. *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a){2,4}" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_brazero;
      Opcodes.op_bra;
      0;
      24;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_brazero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      24;
    ];

  (* "(a){0,2}": the zero-minimum nested case (pcre2_compile.c:7519-7533)
     moves the original copy up by 2+LINK_SIZE, emits BRAZERO + OP_BRA
     with the chain link, and repeat_max-- = 1 more copy from the common
     code; the chain-through pass closes the nesting bracket: KET(25)
     link 24 -> BRA(1). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a){0,2}" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_brazero;
      Opcodes.op_bra;
      0;
      24;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_brazero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      24;
    ];

  (* "(a){0}b": {0,0} sticks OP_SKIPZERO in front of the group so it is
     skipped on execution (pcre2_compile.c:7481-7507); unlike the
     char/class {0} cases, code does not go backwards, so compile2's
     exact-length check applies. *)
  let cb, rc, _, _, _, _, rcu, rcuf = compile2 "(a){0}b" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_skipzero;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_char;
      0x62;
    ];
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0) (* {0,0}: reqvary = 0 (pcre2_compile.c:7215) *);

  (* Hand-verified trace 9 — "(a)*+" (pcre2_compile.c:7712-7742): the
     possessive unlimited repeat switches the CBRA to OP_CBRAPOS
     ( *bracode += 1, 7734), the KET to OP_KETRPOS (7735), and the saved
     brazeroptr to OP_BRAPOSZERO (7741); repeat_min(0) < 2 then cancels
     possessive_quantifier so no ONCE wrapping happens (7742). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a)*+" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_braposzero;
      Opcodes.op_cbrapos;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrpos;
      0;
      7;
    ];

  (* "(?:a)++": OP_BRA + 1 = OP_BRAPOS, no BRAZERO for min 1. *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(?:a)++" in
  assert (Int.equal rc 1);
  assert_code cb
    [ Opcodes.op_brapos; 0; 5; Opcodes.op_char; 0x61; Opcodes.op_ketrpos; 0; 5 ];

  (* "(a|)*": the group may match empty (group_return -1), so the real
     phase converts OP_CBRA to OP_SCBRA ( *bracode += OP_SBRA - OP_BRA,
     pcre2_compile.c:7703-7705) before setting KETRMAX. *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a|)*" in
  assert (Int.equal rc (-1));
  assert_code cb
    [
      Opcodes.op_brazero;
      Opcodes.op_scbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_alt;
      0;
      3;
      Opcodes.op_ketrmax;
      0;
      10;
    ];

  (* "(a){2,}+": with repeat_min 2 the possessive flag survives (7742), so
     after the last copy becomes CBRAPOS/KETRPOS the generic possessive
     pass wraps the whole repeated item in ONCE brackets
     (pcre2_compile.c:7989-8001): [ONCE [CBRA a KET] [CBRAPOS a KETRPOS]
     KET]. *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a){2,}+" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_once;
      0;
      23;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_cbrapos;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ketrpos;
      0;
      7;
      Opcodes.op_ket;
      0;
      23;
    ];

  (* "(a){1}+": a possessive {1,1} is NOT ignored (7447 requires
     !possessive_quantifier); nothing in the bracket arm changes the
     group, and the generic possessive pass wraps it in ONCE
     (opcode_possessify[OP_CBRA] does not apply — OP_CBRA > OP_CALLOUT). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a){1}+" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_once;
      0;
      13;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      13;
    ];

  (* "(a){1}": a non-possessive {1,1} bracket repeat is ignored (7447). *)
  let cb, rc, _, _, _, _, _, _ = compile2 "(a){1}" in
  assert (Int.equal rc 1);
  assert_code cb
    [ Opcodes.op_cbra; 0; 7; 0; 1; Opcodes.op_char; 0x61; Opcodes.op_ket; 0; 7 ];

  (* groupsetfirstcu (pcre2_compile.c:6971, 7569-7573): in "(a){2}c" the
     group set firstcu = 'a' and the min > 1 replication promotes it to
     reqcu before 'c' overrides — reqcu ends as 'c' with REQ_VARY. In
     "(ab){2}" the 7569 promotion guard is false (the group itself already
     set reqcu = 'b' via pcre2_compile.c:6990-6994); firstcu 'a', reqcu 'b'
     come from the group's own values. *)
  let cb, _, _, _, fcu, fcuf, rcu, rcuf = compile2 "(ab){2}" in
  assert (Int.equal fcu 0x61);
  assert (Int.equal fcuf 0);
  assert (Int.equal rcu 0x62);
  assert (Int.equal rcuf 0);
  assert (Int.equal (Bytes.length cb.start_code) 24);

  (* \p and \P in a class (pcre2_compile.c:6243-6255): parse substitutes
     \p{Nd} for \d under UCP, so /[\d]/ucp = OP_XCLASS with no bitmap
     (class_has_8bitchar undone), flags = XCL_HASPROP, one XCL_PROP item.
     Oracle dump `[\p{Nd}]`, item spanning offsets 3-11 (length 8). *)
  let cb, rc, _, _, _, _, _, _ = compile2 ~options:Options.ucp "[\\d]" in
  assert (Int.equal rc 1);
  assert_code cb
    [
      Opcodes.op_xclass;
      0;
      8;
      Opcodes.xcl_hasprop;
      Opcodes.xcl_prop;
      Opcodes.pt_pc;
      Ucp.ucp_nd;
      Opcodes.xcl_end;
    ];
  (* /[\p{L}\d]/utf: bitmap (0-9) + XCL_PROP; flags = XCL_MAP lor
     XCL_HASPROP. Oracle dump `[0-9\p{L}]`, item spanning offsets 3-43
     (length 40). *)
  let cb, _, _, _, _, _, _, _ = compile2 ~options:Options.utf "[\\p{L}\\d]" in
  assert (Int.equal (Bytes.length cb.start_code) 40);
  assert (Int.equal (Char.code (Bytes.get cb.start_code 0)) Opcodes.op_xclass);
  assert (Int.equal (get cb.start_code 1) 40);
  assert (
    Int.equal
      (Char.code (Bytes.get cb.start_code 3))
      (Opcodes.xcl_map lor Opcodes.xcl_hasprop));
  (* The 32-byte map covers 0x30-0x39 ('0' bit set, 'a' clear)... *)
  assert (Int.equal (Char.code (Bytes.get cb.start_code (4 + (0x30 / 8)))) 0xff);
  assert (Int.equal (Char.code (Bytes.get cb.start_code (4 + (0x61 / 8)))) 0);
  (* ...followed by the property item and XCL_END. *)
  List.iteri
    (fun i v ->
      assert (Int.equal (Char.code (Bytes.get cb.start_code (36 + i))) v))
    [ Opcodes.xcl_prop; Opcodes.pt_gc; Ucp.ucp_l; Opcodes.xcl_end ];
  (* UCP POSIX graph/print/punct classes (pcre2_compile.c:6061-6070):
     /[[:graph:]]/ucp = XCLASS with XCL_PROP PT_PXGRAPH 0; the negated
     /[^[:^print:]]/ucp = XCL_NOT lor XCL_HASPROP with XCL_NOTPROP
     PT_PXPRINT 0. Oracle items span offsets 3-11 (length 8). *)
  let cb, _, _, _, _, _, _, _ = compile2 ~options:Options.ucp "[[:graph:]]" in
  assert_code cb
    [
      Opcodes.op_xclass;
      0;
      8;
      Opcodes.xcl_hasprop;
      Opcodes.xcl_prop;
      Opcodes.pt_pxgraph;
      0;
      Opcodes.xcl_end;
    ];
  let cb, _, _, _, _, _, _, _ = compile2 ~options:Options.ucp "[^[:^print:]]" in
  assert_code cb
    [
      Opcodes.op_xclass;
      0;
      8;
      Opcodes.xcl_not lor Opcodes.xcl_hasprop;
      Opcodes.xcl_notprop;
      Opcodes.pt_pxprint;
      0;
      Opcodes.xcl_end;
    ];
  (* Freestanding \p/\P (pcre2_compile.c:8136-8156): OP_PROP/OP_NOTPROP
     with type and value; \p{Any} = OP_ALLANY (auto-anchoring benefit);
     repeats go through the property branch of OUTPUT_SINGLE_REPEAT
     (7776-7780: /\p{Nd}{2,}/ = TYPEEXACT{2} PROP Nd + TYPESTAR PROP Nd —
     compile_branch output; the driver's auto_possessify pass then turns
     the TYPESTAR into TYPEPOSSTAR, the oracle dump's `prop Nd *+`). *)
  let cb, rc, _, _, _, fcuf, _, _ = compile2 "\\p{Greek}" in
  assert (Int.equal rc 1);
  assert (Int.equal fcuf req_none);
  assert_code cb [ Opcodes.op_prop; Opcodes.pt_scx; Ucp.ucp_greek ];
  let cb, _, _, _, _, _, _, _ = compile2 "\\p{^L}" in
  assert_code cb [ Opcodes.op_notprop; Opcodes.pt_gc; Ucp.ucp_l ];
  let cb, _, _, _, _, _, _, _ = compile2 "\\P{Nd}" in
  assert_code cb [ Opcodes.op_notprop; Opcodes.pt_pc; Ucp.ucp_nd ];
  let cb, _, _, _, _, _, _, _ = compile2 "\\p{Any}" in
  assert_code cb [ Opcodes.op_allany ];
  let cb, _, _, _, _, _, _, _ = compile2 "\\p{Nd}{2,}" in
  assert_code cb
    [
      Opcodes.op_typeexact;
      0;
      2;
      Opcodes.op_prop;
      Opcodes.pt_pc;
      Ucp.ucp_nd;
      Opcodes.op_typestar;
      Opcodes.op_prop;
      Opcodes.pt_pc;
      Ucp.ucp_nd;
    ];

  (* UTF / Unicode-caseless compile_branch arms, pinned against `pcre2test
     -q` + fullbincode on the real 10.44 library (testoutput-style dumps
     probed for each pattern below). *)
  (* Caseless multicase literal (pcre2_compile.c:8237-8251): /(?i)k/ under
     UCP (or UTF) = [PROP clist <caseset(k)>]; oracle dump
     `clist 004b 006b 212a`. *)
  let kset = Ucd.caseset (Char.code 'k') in
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:(Options.ucp lor Options.caseless) "k"
  in
  assert_code cb [ Opcodes.op_prop; Opcodes.pt_clist; kset ];
  (* Negated one-char class of a multicase character
     (pcre2_compile.c:5930-5940): /(?i)[^k]/ = [NOTPROP clist ...]. *)
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:(Options.ucp lor Options.caseless) "[^k]"
  in
  assert_code cb [ Opcodes.op_notprop; Opcodes.pt_clist; kset ];
  (* Multi-byte literal via ord2utf (pcre2_compile.c:8264-8270): /é/utf =
     [CHAR c3 a9]; caseless single-other-case /(?i)é/utf = [CHARI c3 a9]
     (caseset(0xe9) = 0 falls through to CLASS_CASELESS_CHAR). *)
  let cb, _, _, _, fcu, _, rcu, _ = compile2 ~options:Options.utf "\xc3\xa9" in
  assert_code cb [ Opcodes.op_char; 0xc3; 0xa9 ];
  (* firstcu = mcbuffer[0], reqcu = code[-1] for a multi-unit char
     (pcre2_compile.c:8299-8307). *)
  assert (Int.equal fcu 0xc3);
  assert (Int.equal rcu 0xa9);
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:(Options.utf lor Options.caseless) "\xc3\xa9"
  in
  assert_code cb [ Opcodes.op_chari; 0xc3; 0xa9 ];
  (* Negated one-char class of a wide char (pcre2_compile.c:5942-5945
     PUTCHAR): /[^\x{100}]/utf = [NOT c4 80] (no OP_XCLASS). *)
  let cb, _, _, _, _, _, _, _ = compile2 ~options:Options.utf "[^\\x{100}]" in
  assert_code cb [ Opcodes.op_not; 0xc4; 0x80 ];
  (* OP_XCLASS without a bitmap (pcre2_compile.c:5321-5336, 6436-6462):
     /[\x{100}-\x{200}]/utf = [XCLASS len=10 flags=0 XCL_RANGE c4 80 c8 80
     XCL_END]; oracle shows the item spanning offsets 3-13. *)
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:Options.utf "[\\x{100}-\\x{200}]"
  in
  assert_code cb
    [
      Opcodes.op_xclass;
      0;
      10;
      0;
      Opcodes.xcl_range;
      0xc4;
      0x80;
      0xc8;
      0x80;
      Opcodes.xcl_end;
    ];
  (* OP_XCLASS with the XCL_MAP bitmap (pcre2_compile.c:6442-6457):
     /[\x{ff}-\x{100}]/utf = 40 units, map bit 0xff + XCL_SINGLE 0x100;
     oracle shows the item spanning offsets 3-43. *)
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:Options.utf "[\\x{ff}-\\x{100}]"
  in
  let xclass_map_expected map tail =
    [ Opcodes.op_xclass; 0; 4 + 32 + List.length tail; Opcodes.xcl_map ]
    @ map @ tail
  in
  let map_ff = List.init 32 (fun i -> if Int.equal i 31 then 0x80 else 0) in
  assert_code cb
    (xclass_map_expected map_ff
       [ Opcodes.xcl_single; 0xc4; 0x80; Opcodes.xcl_end ]);
  (* Caseless closure (pcre2_compile.c:5239-5282 with get_othercase_range
     and add_list_to_class_internal): /(?i)[à-ÿ]/utf compiles to map bits
     c0-d6, d8-de, e0-ff plus XCL_SINGLE 0x212b (from å's caseless set,
     emitted mid-closure) and XCL_SINGLE 0x178 (ÿ's other case) — oracle
     dump `[\xc0-\xd6\xd8-\xde\xe0-\xff\x{212b}\x{178}]`, 44 units. *)
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:(Options.utf lor Options.caseless) "[\xc3\xa0-\xc3\xbf]"
  in
  let map_folded =
    List.init 32 (fun i ->
        match i with
        | 24 | 25 -> 0xff (* c0-cf *)
        | 26 | 27 -> 0x7f (* d0-d6, d8-de *)
        | 28 | 29 | 30 | 31 -> 0xff (* e0-ff *)
        | _ -> 0)
  in
  assert_code cb
    (xclass_map_expected map_folded
       [
         Opcodes.xcl_single;
         0xe2;
         0x84;
         0xab;
         Opcodes.xcl_single;
         0xc5;
         0xb8;
         Opcodes.xcl_end;
       ]);
  (* Repeated multi-byte character (pcre2_compile.c:7280-7289 BACKCHAR +
     mcbuffer, OUTPUT_SINGLE_REPEAT): /é{2,4}/utf = [EXACT 2 c3 a9]
     [UPTO 2 c3 a9] from compile_branch; the driver's auto_possessify
     pass then rewrites the UPTO to POSUPTO — the oracle's {0,2}+
     (pinned in the debug_printer dumps). *)
  let cb, _, _, _, _, _, _, _ = compile2 ~options:Options.utf "\xc3\xa9{2,4}" in
  assert_code cb
    [ Opcodes.op_exact; 0; 2; 0xc3; 0xa9; Opcodes.op_upto; 0; 2; 0xc3; 0xa9 ];
  (* Explicitly possessive multi-byte repeat: the possessive post-pass
     skips the character with GET_EXTRALEN (pcre2_compile.c:7950-7953) and
     possessifies OP_PLUS: /é++/utf = [POSPLUS c3 a9]. *)
  let cb, _, _, _, _, _, _, _ = compile2 ~options:Options.utf "\xc3\xa9++" in
  assert_code cb [ Opcodes.op_posplus; 0xc3; 0xa9 ];
  (* VERB_ARG with a wide character (pcre2_compile.c:6556-6571): the name
     is stored as UTF-8 code units with the code-unit length in front:
     ( *MARK:é) = [MARK 2 c3 a9 0]; oracle dump `*MARK \x{c3}\x{a9}`. *)
  let cb, _, _, _, _, _, _, _ =
    compile2 ~options:Options.utf "(*MARK:\xc3\xa9)a"
  in
  assert_code cb [ Opcodes.op_mark; 2; 0xc3; 0xa9; 0; Opcodes.op_char; 0x61 ]

(* pcre2_compile() end-to-end: two-pass driver, pso settings, error
   propagation, name table, and the anchoring / first-and-required
   code-unit finalization. Expected bytecode verified against `pcre2test
   -q` with fullbincode / -I on the real 10.44 library. *)
let test_8 () =
  let ok pat =
    match pcre2_compile pat ~options:0 with
    | Ok re -> re
    | Error (e, o) ->
        failwith
          (Printf.sprintf "pcre2_compile %S: error %d at offset %d" pat e o)
  in
  let expect_err pat options e o =
    match pcre2_compile pat ~options with
    | Ok _ -> assert false
    | Error (e', o') ->
        assert (Int.equal e e');
        assert (Int.equal o o')
  in
  (* /abc/: [BRA 9][CHAR a][CHAR b][CHAR c][KET 9][END]; firstcu 'a',
     reqcu 'c', not anchored, no name table. *)
  let re = ok "abc" in
  assert (Int.equal (Bytes.length re.code) 13);
  List.iteri
    (fun i v -> assert (Int.equal (Char.code (Bytes.get re.code i)) v))
    [
      Opcodes.op_bra;
      0;
      9;
      Opcodes.op_char;
      0x61;
      Opcodes.op_char;
      0x62;
      Opcodes.op_char;
      0x63;
      Opcodes.op_ket;
      0;
      9;
      Opcodes.op_end;
    ];
  assert (Int.equal re.compile_options 0);
  assert (Int.equal re.overall_options 0);
  assert (Int.equal (re.flags land (firstset lor lastset)) (firstset lor lastset));
  assert (Int.equal re.first_codeunit 0x61);
  assert (Int.equal re.last_codeunit 0x63);
  assert (Int.equal (re.flags land match_empty) 0);
  assert (
    Int.equal re.minlength
      3 (* find_minlength's studied value (oracle: lower bound = 3) *));
  assert (Int.equal re.top_bracket 0);
  assert (Int.equal re.name_count 0);
  assert (Int.equal re.newline_convention Options.newline_lf);
  assert (Int.equal re.bsr_convention Options.bsr_unicode);
  assert (Int.equal re.limit_match 0xffff_ffff);
  (* /^abc|^def/ is startline-anchored via OP_CIRC in every branch;
     /(?s).*abc/ is auto-anchored via TYPESTAR+ALLANY. *)
  let re = ok "^abc" in
  assert (not (Int.equal (re.overall_options land Options.anchored) 0));
  let re = ok "(?s).*abc" in
  assert (not (Int.equal (re.overall_options land Options.anchored) 0));
  (* Multiline ^ compiles CIRCM: not anchored, but STARTLINE. *)
  let re =
    match pcre2_compile "^abc" ~options:Options.multiline with
    | Ok re -> re
    | Error _ -> assert false
  in
  assert (Int.equal (re.overall_options land Options.anchored) 0);
  assert (not (Int.equal (re.flags land startline) 0));
  (* /(|a)/ can match an empty string. *)
  let re = ok "(|a)" in
  assert (not (Int.equal (re.flags land match_empty) 0));
  assert (Int.equal re.top_bracket 1);
  (* find_firstassertedcu: /(?=abc)a../ has an asserted first cu... but
     lookaheads are M4-deferred; use the caseless-literal path instead:
     /(?i)abc/ sets FIRSTCASELESS/LASTCASELESS with argoptions =
     alloptions = 0 (a leading (?i) does NOT reach external_options —
     pcre2test shows no "Options:" line, testoutput2:484-488). *)
  let re = ok "(?i)abc" in
  assert (Int.equal re.compile_options 0);
  assert (Int.equal re.overall_options 0);
  assert (
    Int.equal
      (re.flags land (firstset lor firstcaseless lor lastset lor lastcaseless))
      (firstset lor firstcaseless lor lastset lor lastcaseless));
  assert (Int.equal re.first_codeunit 0x61);
  assert (Int.equal re.last_codeunit 0x63);
  (* pso settings: ( *CR) newline override + NL_SET flag; ( *NOTEMPTY) flag;
     ( *LIMIT_MATCH=1000) limit. *)
  let re = ok "(*CR)a" in
  assert (Int.equal re.newline_convention Options.newline_cr);
  assert (not (Int.equal (re.flags land nl_set) 0));
  let re = ok "(*BSR_ANYCRLF)a" in
  assert (Int.equal re.bsr_convention Options.bsr_anycrlf);
  assert (not (Int.equal (re.flags land bsr_set) 0));
  let re = ok "(*NOTEMPTY)a" in
  assert (not (Int.equal (re.flags land notempty_set) 0));
  let re = ok "(*LIMIT_MATCH=1000)a" in
  assert (Int.equal re.limit_match 1000);
  assert (Int.equal re.limit_heap 0xffff_ffff);
  (* ( *ANY)/( *ANYCRLF) newline types flow into the parse-time IS_NEWLINE
     (extended-mode # comment scan, pcre2_compile.c:3070-3085 +
     PRIV(is_newline) pcre2_newline.c:78-145): the comment ends at the
     LF / CR and 'a' compiles. *)
  (match pcre2_compile "(*ANY)#c\na" ~options:Options.extended with
  | Ok re ->
      assert (Int.equal re.newline_convention Options.newline_any);
      assert (Int.equal re.first_codeunit 0x61)
  | Error _ -> assert false);
  (match pcre2_compile "(*ANYCRLF)#c\ra" ~options:Options.extended with
  | Ok re ->
      assert (Int.equal re.newline_convention Options.newline_anycrlf);
      assert (Int.equal re.first_codeunit 0x61)
  | Error _ -> assert false);
  (* Malformed limit: ERR60 with the C's ptr+pp offsets
     (pcre2_compile.c:10349-10365). *)
  expect_err "(*LIMIT_MATCH=x)a" 0 Errors.err60 14;
  expect_err "(*LIMIT_MATCH=12" 0 Errors.err60 17;
  (* Unknown ( *WORD) is not a pso: falls through to parse_regex's verb
     handling — ERR60 with ptr at the closing parenthesis. *)
  expect_err "(*XYZZY)a" 0 Errors.err60 7;
  (* Error propagation with parse offsets: "(" = ERR14 at 1; "a{2,1}" =
     ERR4 at 5 (testoutput2:128-129 shape). *)
  expect_err "(" 0 Errors.err14 1;
  expect_err "a{2,1}" 0 Errors.err4 5;
  (* Option validation: an undefined public bit (0x08000000 is unassigned
     in 10.44's compile options) = ERR17 (117) at offset 0; PCRE2_LITERAL
     restricted set = ERR92. *)
  expect_err "a" 0x08000000 Errors.err17 0;
  expect_err "a" (Options.literal lor Options.multiline) Errors.err92 0;
  (* ( *UTF) under NEVER_UTF: ERR74 at the post-pso offset. *)
  expect_err "(*UTF)a" Options.never_utf Errors.err74 6;
  expect_err "(*UCP)a" Options.never_ucp Errors.err75 6;
  (* UTF pattern validity gate (pcre2_compile.c:10406-10408): the negative
     valid_utf code, with the offset set by valid_utf itself. Oracle:
     /\xc3(/utf gives "Failed: error -8 at offset 0"; testoutput10:8-19
     pins the bracketed and truncated shapes. *)
  expect_err "\xc3(" Options.utf Errors.error_utf8_err6 0;
  expect_err "[\xc3(]" Options.utf Errors.error_utf8_err6 1;
  expect_err "\xc3" Options.utf Errors.error_utf8_err1 0;
  (* PCRE2_NO_UTF_CHECK skips the gate (10406). *)
  (match
     pcre2_compile "\xc3\xa9" ~options:(Options.utf lor Options.no_utf_check)
   with
  | Ok _ -> ()
  | Error _ -> assert false);
  (* /(?i)é/utf end-to-end; oracle fullbincode:
       0   6 Bra / 3  /i \x{e9} / 6   6 Ket / 9     End *)
  (match pcre2_compile "(?i)\xc3\xa9" ~options:Options.utf with
  | Error _ -> assert false
  | Ok re ->
      List.iteri
        (fun i v -> assert (Int.equal (Char.code (Bytes.get re.code i)) v))
        [
          Opcodes.op_bra;
          0;
          6;
          Opcodes.op_chari;
          0xc3;
          0xa9;
          Opcodes.op_ket;
          0;
          6;
          Opcodes.op_end;
        ]);
  (* /[\S\x{100}]/utf: the explicitly-given wide char is discarded — the
     xclass entry condition (pcre2_compile.c:6383-6407) fails for a
     negated special without UCP or properties, leaving OP_NCLASS (oracle
     dump: `[\x00-\x08\x0e-\x1f!-\xff] (neg)`); the pre-compile phase
     over-estimated the length by the discarded extra data, which the
     driver absorbs (usedlength < length, 10717-10725). *)
  (match pcre2_compile "[\\S\\x{100}]" ~options:Options.utf with
  | Error _ -> assert false
  | Ok re ->
      assert (Int.equal (Char.code (Bytes.get re.code 3)) Opcodes.op_nclass));
  (* UTF group names (read_name, pcre2_compile.c:2488-2512): Unicode
     letters are valid name characters; a leading Nd digit is ERR44 at the
     name start (oracle: /(?<1é>a)/utf = error 144 at offset 3). *)
  (match pcre2_compile "(?<\xc3\xa9x>a)\\k<\xc3\xa9x>" ~options:Options.utf with
  | Ok _ -> ()
  | Error _ -> assert false);
  expect_err "(?<1\xc3\xa9>a)" Options.utf Errors.err44 3;
  (* Name table: /(?<xx>a)(?<yy>b)(?<ww>c)/ has 3 entries of size
     2+2+1 = 5, alphabetically ordered by add_name_to_table. *)
  let re = ok "(?<xx>a)(?<yy>b)(?<ww>c)" in
  assert (Int.equal re.name_count 3);
  assert (Int.equal re.name_entry_size 5);
  assert (Int.equal re.top_bracket 3);
  let assert_entry i expect_name expect_num =
    let base = i * re.name_entry_size in
    assert (Int.equal (get2 re.name_table base) expect_num);
    assert (
      String.equal
        (Bytes.sub_string re.name_table (base + Limits.imm2_size) 2)
        expect_name);
    assert (
      Int.equal
        (Char.code (Bytes.get re.name_table (base + Limits.imm2_size + 2)))
        0)
  in
  assert_entry 0 "ww" 3;
  assert_entry 1 "xx" 1;
  assert_entry 2 "yy" 2

(* Backtracking-verb compilation (M5 verbs chunk, pcre2_compile.c:596-637,
   2941-3039, 4064-4152 and 6487-6573,
   docs/ocaml-engine/06-verbs-k-start-opt.md): bytecode and flags pinned
   with `pcre2test` + fullbincode on the real 10.44 library. *)
let test_9 () =
  let ok ?(options = 0) pat =
    match pcre2_compile pat ~options with
    | Ok re -> re
    | Error (e, o) ->
        failwith
          (Printf.sprintf "pcre2_compile %S: error %d at offset %d" pat e o)
  in
  let assert_code (re : re) (expected : int list) =
    assert (Int.equal (Bytes.length re.code) (List.length expected));
    List.iteri
      (fun i v -> assert (Int.equal (Char.code (Bytes.get re.code i)) v))
      expected
  in
  let bra = Opcodes.op_bra and ket = Opcodes.op_ket in

  (* /( *FAIL)/ *)
  let re = ok "(*FAIL)" in
  assert_code re [ bra; 0; 4; Opcodes.op_fail; ket; 0; 4; Opcodes.op_end ];

  (* /a( *COMMIT)b/ *)
  let re = ok "a(*COMMIT)b" in
  assert_code re
    [
      bra;
      0;
      8;
      Opcodes.op_char;
      0x61;
      Opcodes.op_commit;
      Opcodes.op_char;
      0x62;
      ket;
      0;
      8;
      Opcodes.op_end;
    ];

  (* /( *MARK:x)a/ — OP_MARK carries a length code unit, the name and a
     terminating zero (pcre2_compile.c:6547-6572). *)
  let re = ok "(*MARK:x)a" in
  assert_code re
    [
      bra;
      0;
      9;
      Opcodes.op_mark;
      1;
      0x78;
      0;
      Opcodes.op_char;
      0x61;
      ket;
      0;
      9;
      Opcodes.op_end;
    ];

  (* /( *PRUNE:n)a/ *)
  let re = ok "(*PRUNE:n)a" in
  assert_code re
    [
      bra;
      0;
      9;
      Opcodes.op_prune_arg;
      1;
      0x6e;
      0;
      Opcodes.op_char;
      0x61;
      ket;
      0;
      9;
      Opcodes.op_end;
    ];

  (* /( *SKIP)a/ *)
  let re = ok "(*SKIP)a" in
  assert_code re
    [
      bra;
      0;
      6;
      Opcodes.op_skip;
      Opcodes.op_char;
      0x61;
      ket;
      0;
      6;
      Opcodes.op_end;
    ];

  (* /( *THEN)a|b/ — sets PCRE2_HASTHEN (pcre2_compile.c:6525-6528). *)
  let re = ok "(*THEN)a|b" in
  assert_code re
    [
      bra;
      0;
      6;
      Opcodes.op_then;
      Opcodes.op_char;
      0x61;
      Opcodes.op_alt;
      0;
      5;
      Opcodes.op_char;
      0x62;
      ket;
      0;
      11;
      Opcodes.op_end;
    ];
  assert (Int.equal (re.flags land hasthen) hasthen);
  let re = ok "(*THEN:x)a" in
  assert (Int.equal (re.flags land hasthen) hasthen);

  (* /a( *ACCEPT)b/ — sets PCRE2_HASACCEPT and disables reqcu
     (pcre2_compile.c:6496-6513, 10705-10710). *)
  let re = ok "a(*ACCEPT)b" in
  assert_code re
    [
      bra;
      0;
      8;
      Opcodes.op_char;
      0x61;
      Opcodes.op_accept;
      Opcodes.op_char;
      0x62;
      ket;
      0;
      8;
      Opcodes.op_end;
    ];
  assert (Int.equal (re.flags land hasaccept) hasaccept);
  assert (Int.equal (re.flags land lastset) 0);

  (* /(?:a( *ACCEPT))+/ *)
  let re = ok "(?:a(*ACCEPT))+" in
  assert_code re
    [
      bra;
      0;
      12;
      bra;
      0;
      6;
      Opcodes.op_char;
      0x61;
      Opcodes.op_accept;
      Opcodes.op_ketrmax;
      0;
      6;
      ket;
      0;
      12;
      Opcodes.op_end;
    ];

  (* /a( *ACCEPT)?/ — parse_regex wraps the quantified ( *ACCEPT) in
     non-capturing brackets (pcre2_compile.c:3464-3477). *)
  let re = ok "a(*ACCEPT)?" in
  assert_code re
    [
      bra;
      0;
      13;
      Opcodes.op_char;
      0x61;
      Opcodes.op_brazero;
      bra;
      0;
      4;
      Opcodes.op_accept;
      ket;
      0;
      4;
      ket;
      0;
      13;
      Opcodes.op_end;
    ];

  (* /( *ACCEPT:x)+/ — the argument becomes a preceding ( *MARK) inside the
     wrapping brackets. *)
  let re = ok "(*ACCEPT:x)+" in
  assert_code re
    [
      bra;
      0;
      14;
      Opcodes.op_sbra;
      0;
      8;
      Opcodes.op_mark;
      1;
      0x78;
      0;
      Opcodes.op_accept;
      Opcodes.op_ketrmax;
      0;
      8;
      ket;
      0;
      14;
      Opcodes.op_end;
    ];

  (* /((a)(b)( *ACCEPT)c)/ — open captures at the same assertion level are
     closed with OP_CLOSE before OP_ACCEPT (pcre2_compile.c:6498-6511):
     only group 1 is still open at the ( *ACCEPT). *)
  let re = ok "((a)(b)(*ACCEPT)c)" in
  assert_code re
    [
      bra;
      0;
      37;
      Opcodes.op_cbra;
      0;
      31;
      0;
      1;
      Opcodes.op_cbra;
      0;
      7;
      0;
      2;
      Opcodes.op_char;
      0x61;
      ket;
      0;
      7;
      Opcodes.op_cbra;
      0;
      7;
      0;
      3;
      Opcodes.op_char;
      0x62;
      ket;
      0;
      7;
      Opcodes.op_close;
      0;
      1;
      Opcodes.op_accept;
      Opcodes.op_char;
      0x63;
      ket;
      0;
      31;
      ket;
      0;
      37;
      Opcodes.op_end;
    ];

  (* /(?=a( *ACCEPT))b/ — ACCEPT inside an assertion becomes
     OP_ASSERT_ACCEPT (pcre2_compile.c:6512). *)
  let re = ok "(?=a(*ACCEPT))b" in
  assert_code re
    [
      bra;
      0;
      14;
      Opcodes.op_assert;
      0;
      6;
      Opcodes.op_char;
      0x61;
      Opcodes.op_assert_accept;
      ket;
      0;
      6;
      Opcodes.op_char;
      0x62;
      ket;
      0;
      14;
      Opcodes.op_end;
    ];

  (* /( *COMMIT:abc)/ *)
  let re = ok "(*COMMIT:abc)" in
  assert_code re
    [
      bra;
      0;
      9;
      Opcodes.op_commit_arg;
      3;
      0x61;
      0x62;
      0x63;
      0;
      ket;
      0;
      9;
      Opcodes.op_end;
    ];

  (* /(?C{ab}}c})z/ — string callout, full compile (pcre2_compile.c:
     7114-7175): a doubled ending delimiter is copied once, so the
     pre-pass length (which includes both source delimiters,
     pcre2_compile.c:7122-7124) over-estimates by one and re.code keeps a
     zeroed slack unit after OP_END (usedlength < length,
     pcre2_compile.c:10712-10725). String pinned with pcre2test 10.44
     (Callout (4): {ab}c}). *)
  let re = ok "(?C{ab}}c})z" in
  assert_code re
    [
      bra;
      0;
      20;
      Opcodes.op_callout_str;
      0;
      11;
      0;
      1;
      0;
      15;
      0;
      4;
      0x7b;
      0x61;
      0x62;
      0x7d;
      0x63;
      0;
      Opcodes.op_char;
      0x7a;
      ket;
      0;
      20;
      Opcodes.op_end;
      0;
    ]

(* Backreference compilation (M2 compile chunk, pcre2_compile.c:7018-7098
   and 8022-8058, docs/ocaml-engine/03-backreferences.md): bytecode,
   top_backref and error numbers/offsets pinned with `pcre2test -q` +
   fullbincode/-I on the real 10.44 library. *)
let test_10 () =
  let ok ?(options = 0) pat =
    match pcre2_compile pat ~options with
    | Ok re -> re
    | Error (e, o) ->
        failwith
          (Printf.sprintf "pcre2_compile %S: error %d at offset %d" pat e o)
  in
  let expect_err pat options e o =
    match pcre2_compile pat ~options with
    | Ok _ -> assert false
    | Error (e', o') ->
        assert (Int.equal e e');
        assert (Int.equal o o')
  in
  let assert_code (re : re) (expected : int list) =
    assert (Int.equal (Bytes.length re.code) (List.length expected));
    List.iteri
      (fun i v -> assert (Int.equal (Char.code (Bytes.get re.code i)) v))
      expected
  in
  let byte (re : re) i = Char.code (Bytes.get re.code i) in
  (* /(a)\1/: [BRA 16][CBRA 7 1][CHAR a][KET 7][REF 1][KET 16][END];
     Max back reference = 1. *)
  let re = ok "(a)\\1" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      16;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ref;
      0;
      1;
      Opcodes.op_ket;
      0;
      16;
      Opcodes.op_end;
    ];
  assert (Int.equal re.top_backref 1);
  (* Caseless: /i \1 = OP_REFI. *)
  let re = ok "(?i)(a)\\1" in
  assert (Int.equal (byte re 13) Opcodes.op_refi);
  assert (Int.equal (get2 re.code 14) 1);
  (* /(?P<n>a)\k<n>/: a non-duplicated name resolves to the numerical
     reference \1 (HANDLE_SINGLE_REFERENCE via META_BACKREF_BYNAME). *)
  let re = ok "(?P<n>a)\\k<n>" in
  assert (Int.equal (byte re 13) Opcodes.op_ref);
  assert (Int.equal (get2 re.code 14) 1);
  assert (Int.equal re.top_backref 1);
  (* /((a)|(b))\k<A>(?<A>c)/dupnames: single-entry name, forward
     reference — resolves numerically to \4. *)
  let re = ok ~options:Options.dupnames "((a)|(b))\\k<A>(?<A>c)" in
  assert (Int.equal (byte re 34) Opcodes.op_ref);
  assert (Int.equal (get2 re.code 35) 4);
  assert (Int.equal re.top_backref 4);
  (* /(?P<zz>a)(?P<zz>b)\k<zz>/dupnames: OP_DNREF with name-table index 0
     and duplicate count 2 (pcre2test: "23 \k<zz>2"); Max back
     reference = 2. Caseless flavour is OP_DNREFI. *)
  let re = ok ~options:Options.dupnames "(?P<zz>a)(?P<zz>b)\\k<zz>" in
  assert (Int.equal (Bytes.length re.code) 32);
  assert (Int.equal (byte re 23) Opcodes.op_dnref);
  assert (Int.equal (get2 re.code 24) 0 (* index *));
  assert (Int.equal (get2 re.code 26) 2 (* count *));
  assert (Int.equal (byte re 28) Opcodes.op_ket);
  assert (Int.equal re.top_backref 2);
  let re =
    ok
      ~options:(Options.dupnames lor Options.caseless)
      "(?P<zz>a)(?P<zz>b)\\k<zz>"
  in
  assert (Int.equal (byte re 23) Opcodes.op_dnrefi);
  (* /(?|(a)|(b))\1/: duplicate group numbers from (?| — \1 is numeric. *)
  let re = ok "(?|(a)|(b))\\1" in
  assert (Int.equal (byte re 32) Opcodes.op_ref);
  assert (Int.equal (get2 re.code 33) 1);
  assert (Int.equal re.top_backref 1);
  (* Group >= 10 takes the GETPLUSOFFSET parsed-pattern word
     (pcre2_compile.c:8030-8031): /(a)..(l)\12/ ends [123 REF 12][126 KET]
     [129 END]. *)
  let re = ok "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)\\12" in
  assert (Int.equal (Bytes.length re.code) 130);
  assert (Int.equal (byte re 123) Opcodes.op_ref);
  assert (Int.equal (get2 re.code 124) 12);
  assert (Int.equal (byte re 126) Opcodes.op_ket);
  assert (Int.equal (byte re 129) Opcodes.op_end);
  assert (Int.equal re.top_backref 12);
  (* Repeats after a backref (pcre2_compile.c:7311-7344): {2} = CRRANGE,
     {0,3}? = CRMINRANGE, {0} drops the item, *+ wraps in ONCE. *)
  let re = ok "(a|b)\\1{2}" in
  assert (Int.equal (byte re 18) Opcodes.op_ref);
  assert (Int.equal (byte re 21) Opcodes.op_crrange);
  assert (Int.equal (get2 re.code 22) 2);
  assert (Int.equal (get2 re.code 24) 2);
  let re = ok "(a)\\1{0,3}?" in
  assert (Int.equal (byte re 13) Opcodes.op_ref);
  assert (Int.equal (byte re 16) Opcodes.op_crminrange);
  assert (Int.equal (get2 re.code 17) 0);
  assert (Int.equal (get2 re.code 19) 3);
  (* {0} drops the backref entirely (code = previous,
     pcre2_compile.c:7324-7328): [BRA 13][CBRA 7 1][CHAR a][KET 7][KET 13]
     [END]. The pre-pass deliberately never reduces the length
     (pcre2_compile.c:5761-5767), so re.code keeps 3 units of zeroed slack
     after OP_END (usedlength < length, pcre2_compile.c:10712-10725). *)
  let re = ok "(a)\\1{0}" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      13;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      13;
      Opcodes.op_end;
      0;
      0;
      0;
    ];
  let re = ok "(a)\\1*+" in
  assert (Int.equal (byte re 13) Opcodes.op_once);
  assert (Int.equal (byte re 16) Opcodes.op_ref);
  assert (Int.equal (byte re 19) Opcodes.op_crstar);
  assert (Int.equal (byte re 20) Opcodes.op_ket);
  assert (Int.equal (get re.code 21) 7);
  (* PCRE2_MATCH_UNSET_BACKREF has no compile-time effect outside the
     lookbehind-length machinery (pcre2_compile.c:9621,9668 — M4). *)
  let re = ok ~options:Options.match_unset_backref "(a)\\1" in
  assert (Int.equal (byte re 13) Opcodes.op_ref);
  (* ERR15 (115) reference to non-existent subpattern, with the parse-
     recorded offsets: named at the name (pcre2_compile.c:7068-7073),
     numeric at small_ref_offset / GETPLUSOFFSET (8033-8038). *)
  expect_err "(?P=zz)" 0 Errors.err15 4;
  expect_err "(a)\\2" 0 Errors.err15 4;
  expect_err "\\2()" 0 Errors.err15 1;
  expect_err "abc\\1" 0 Errors.err15 4;
  expect_err "\\g{12}abc" 0 Errors.err15 5;
  expect_err "\\g{2}()" 0 Errors.err15 4

(* Lookaround assertions and atomic groups (M3 compile chunk,
   pcre2_compile.c:6748-6804, 8427-8491, 8639-8643 and the lookbehind-
   length machinery 9223-10102, docs/ocaml-engine/04-lookaround-atomic-
   possessive.md): bytecode, max_lookbehind and error numbers/offsets
   pinned with `pcre2test -q` + fullbincode/-I on the real 10.44
   library. *)
let test_11 () =
  let ok ?(options = 0) pat =
    match pcre2_compile pat ~options with
    | Ok re -> re
    | Error (e, o) ->
        failwith
          (Printf.sprintf "pcre2_compile %S: error %d at offset %d" pat e o)
  in
  let expect_err pat options e o =
    match pcre2_compile pat ~options with
    | Ok _ -> assert false
    | Error (e', o') ->
        assert (Int.equal e e');
        assert (Int.equal o o')
  in
  let assert_code (re : re) (expected : int list) =
    assert (Int.equal (Bytes.length re.code) (List.length expected));
    List.iteri
      (fun i v -> assert (Int.equal (Char.code (Bytes.get re.code i)) v))
      expected
  in
  let byte (re : re) i = Char.code (Bytes.get re.code i) in
  (* /(?=ab)/: [BRA 13][ASSERT 7][CHAR a][CHAR b][KET 7][KET 13][END]. *)
  let re = ok "(?=ab)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      13;
      Opcodes.op_assert;
      0;
      7;
      Opcodes.op_char;
      0x61;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      13;
      Opcodes.op_end;
    ];
  assert (Int.equal re.max_lookbehind 0);
  (* /(?!x)/: [BRA 11][ASSERT_NOT 5][CHAR x][KET 5][KET 11][END]. *)
  let re = ok "(?!x)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      11;
      Opcodes.op_assert_not;
      0;
      5;
      Opcodes.op_char;
      0x78;
      Opcodes.op_ket;
      0;
      5;
      Opcodes.op_ket;
      0;
      11;
      Opcodes.op_end;
    ];
  (* /(?<=ab)c/: fixed-length lookbehind — [ASSERTBACK 10][REVERSE 2]
     inserted at the head of the branch; Max lookbehind = 2. *)
  let re = ok "(?<=ab)c" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      18;
      Opcodes.op_assertback;
      0;
      10;
      Opcodes.op_reverse;
      0;
      2;
      Opcodes.op_char;
      0x61;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      10;
      Opcodes.op_char;
      0x63;
      Opcodes.op_ket;
      0;
      18;
      Opcodes.op_end;
    ];
  assert (Int.equal re.max_lookbehind 2);
  (* /(?<!a|bc)d/: per-branch fixed lengths (1 and 2) each get their own
     OP_REVERSE (the whole group's minlength word is LOOKBEHIND_MAX);
     Max lookbehind = 2. *)
  let re = ok "(?<!a|bc)d" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      26;
      Opcodes.op_assertback_not;
      0;
      8;
      Opcodes.op_reverse;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_alt;
      0;
      10;
      Opcodes.op_reverse;
      0;
      2;
      Opcodes.op_char;
      0x62;
      Opcodes.op_char;
      0x63;
      Opcodes.op_ket;
      0;
      18;
      Opcodes.op_char;
      0x64;
      Opcodes.op_ket;
      0;
      26;
      Opcodes.op_end;
    ];
  assert (Int.equal re.max_lookbehind 2);
  (* /(?>a+)b/: [BRA 13][ONCE 5][POSPLUS a][KET 5][CHAR b][KET 13][END].
     pcre2test shows "a++": the driver's auto_possessify pass (M9)
     rewrites OP_PLUS to OP_POSPLUS at the end of the atomic group
     (pcre2_auto_possess.c:646-649, 1174-1175). *)
  let re = ok "(?>a+)b" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      13;
      Opcodes.op_once;
      0;
      5;
      Opcodes.op_posplus;
      0x61;
      Opcodes.op_ket;
      0;
      5;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      13;
      Opcodes.op_end;
    ];
  (* /(?<=a?bc|ab)d/: variable-length first branch (min 2, max 3) gets
     OP_VREVERSE; the fixed second branch (min = max = 2) keeps
     OP_REVERSE; Max lookbehind = 3. (pcre2test shows "a?+" — the M9
     auto_possessify pass rewrites OP_QUERY to OP_POSQUERY before the
     disjoint 'b', pcre2_auto_possess.c:1182-1183; same length.) *)
  let re = ok "(?<=a?bc|ab)d" in
  assert (Int.equal (Bytes.length re.code) 36);
  assert (Int.equal (byte re 3) Opcodes.op_assertback);
  assert (Int.equal (get re.code 4) 14);
  assert (Int.equal (byte re 6) Opcodes.op_vreverse);
  assert (Int.equal (get2 re.code 7) 2 (* min *));
  assert (Int.equal (get2 re.code 9) 3 (* max *));
  assert (Int.equal (byte re 11) Opcodes.op_posquery);
  assert (Int.equal (byte re 17) Opcodes.op_alt);
  assert (Int.equal (get re.code 18) 10);
  assert (Int.equal (byte re 20) Opcodes.op_reverse);
  assert (Int.equal (get2 re.code 21) 2);
  assert (Int.equal (byte re 27) Opcodes.op_ket);
  assert (Int.equal (get re.code 28) 24);
  assert (Int.equal (byte re 30) Opcodes.op_char);
  assert (Int.equal re.max_lookbehind 3);
  assert (Int.equal re.first_codeunit 0x64 (* "First code unit = 'd'" *));
  (* Max lookbehind is the longest branch, not the longest whole
     assertion: /(?<=ab|defgh)x/ -> 5. *)
  let re = ok "(?<=ab|defgh)x" in
  assert (Int.equal re.max_lookbehind 5);
  (* An unquantified (?!) is optimized to OP_FAIL
     (pcre2_compile.c:6762-6774)... *)
  let re = ok "(?!)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      4;
      Opcodes.op_fail;
      Opcodes.op_ket;
      0;
      4;
      Opcodes.op_end;
    ];
  (* ...but a quantified one is a real (repeated) assertion group. *)
  let re = ok "(?!)+" in
  assert (Int.equal (Bytes.length re.code) 20);
  assert (Int.equal (byte re 3) Opcodes.op_assert_not);
  assert (Int.equal (byte re 9) Opcodes.op_brazero);
  assert (Int.equal (byte re 10) Opcodes.op_assert_not);
  (* Non-atomic forms: (?* and (?<* (and their alpha synonyms) compile
     OP_ASSERT_NA / OP_ASSERTBACK_NA. *)
  let re = ok "(?*abc)d" in
  assert (Int.equal (byte re 3) Opcodes.op_assert_na);
  let re = ok "(?<*ab)c" in
  assert (Int.equal (byte re 3) Opcodes.op_assertback_na);
  assert (Int.equal (byte re 6) Opcodes.op_reverse);
  assert (Int.equal re.max_lookbehind 2);
  (* Alpha synonyms produce identical code to the symbolic forms. *)
  let re = ok "(*plb:ab)c" in
  assert (Int.equal (byte re 3) Opcodes.op_assertback);
  assert (Int.equal (byte re 6) Opcodes.op_reverse);
  assert (Int.equal (get2 re.code 7) 2);
  (* A lookahead nested in a lookbehind contributes no length but
     compiles inline (REVERSE 1 for the y). *)
  let re = ok "(?<=(?=x)y)z" in
  assert (Int.equal (byte re 3) Opcodes.op_assertback);
  assert (Int.equal (byte re 6) Opcodes.op_reverse);
  assert (Int.equal (get2 re.code 7) 1);
  assert (Int.equal (byte re 9) Opcodes.op_assert);
  assert (Int.equal re.max_lookbehind 1);
  (* Lookbehind length errors, offsets per pcre2test: ERR25 (125) for
     unlimited length (\X never allowed); ERR100 (200) for a variable
     branch longer than max_varlookbehind (default 255); ERR87 (187) for
     a branch over LOOKBEHIND_MAX (65535). *)
  expect_err "(?<=a+)b" 0 Errors.err25 0;
  expect_err "x(?<!a*)b" 0 Errors.err25 1;
  expect_err "(?<=\\Xa)b" 0 Errors.err25 0;
  expect_err "(?<=a{0,300})b" 0 Errors.err100 0;
  expect_err "(?<=a{40000}a{40000})b" 0 Errors.err87 0;
  (* A variable lookbehind within the default 255 limit compiles. *)
  let re = ok "(?<=a{2,5})b" in
  assert (Int.equal (byte re 3) Opcodes.op_assertback);
  assert (Int.equal (byte re 6) Opcodes.op_vreverse);
  assert (Int.equal (get2 re.code 7) 2);
  assert (Int.equal (get2 re.code 9) 5);
  assert (Int.equal re.max_lookbehind 5)

(* Conditional groups and recursion (M4 compile chunks,
   pcre2_compile.c:6590-6745, 6861-6913, 8061-8087, 8905-9049,
   10727-10784 and pcre2_find_bracket.c,
   docs/ocaml-engine/05-conditionals-recursion.md): bytecode and error
   numbers/offsets pinned with `pcre2test -q` + fullbincode/-I on the real
   10.44 library. *)
let test_12 () =
  let ok ?(options = 0) pat =
    match pcre2_compile pat ~options with
    | Ok re -> re
    | Error (e, o) ->
        failwith
          (Printf.sprintf "pcre2_compile %S: error %d at offset %d" pat e o)
  in
  let expect_err pat options e o =
    match pcre2_compile pat ~options with
    | Ok _ -> assert false
    | Error (e', o') ->
        assert (Int.equal e e');
        assert (Int.equal o o')
  in
  let assert_code (re : re) (expected : int list) =
    assert (Int.equal (Bytes.length re.code) (List.length expected));
    List.iteri
      (fun i v -> assert (Int.equal (Char.code (Bytes.get re.code i)) v))
      expected
  in
  let byte (re : re) i = Char.code (Bytes.get re.code i) in
  (* /(?(1)a|b)(x)/: [COND 8][CREF 1] a [ALT 5] b [KET 13][CBRA 1] x. *)
  let re = ok "(?(1)a|b)(x)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      29;
      Opcodes.op_cond;
      0;
      8;
      Opcodes.op_cref;
      0;
      1;
      Opcodes.op_char;
      0x61;
      Opcodes.op_alt;
      0;
      5;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      13;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x78;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      29;
      Opcodes.op_end;
    ];
  assert (Int.equal re.top_backref 1);
  (* A named condition on a non-duplicated name compiles identically
     (OP_CREF with the group number). *)
  let re = ok "(?(<n>)a|b)(?<n>x)" in
  assert (Int.equal (byte re 3) Opcodes.op_cond);
  assert (Int.equal (byte re 6) Opcodes.op_cref);
  assert (Int.equal (get2 re.code 7) 1);
  assert (Int.equal re.top_backref 1);
  (* (?(+1) and (?(-1) relative forms resolve at parse time. *)
  let re = ok "(?(+1)a|b)(x)" in
  assert (Int.equal (byte re 6) Opcodes.op_cref);
  assert (Int.equal (get2 re.code 7) 1);
  (* /(?(R)a|b)/: overall recursion test = OP_RREF with RREF_ANY. *)
  let re = ok "(?(R)a|b)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      19;
      Opcodes.op_cond;
      0;
      8;
      Opcodes.op_rref;
      0xff;
      0xff;
      Opcodes.op_char;
      0x61;
      Opcodes.op_alt;
      0;
      5;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      13;
      Opcodes.op_ket;
      0;
      19;
      Opcodes.op_end;
    ];
  (* (?(R2) with group 2 defined: OP_RREF 2; (?(R&name): OP_RREF with the
     named group's number, which also updates Max back reference. *)
  let re = ok "(?(R2)a|b)(x)(y)" in
  assert (Int.equal (byte re 6) Opcodes.op_rref);
  assert (Int.equal (get2 re.code 7) 2);
  let re = ok "(?(R&f)a|b)(?<f>x)" in
  assert (Int.equal (byte re 6) Opcodes.op_rref);
  assert (Int.equal (get2 re.code 7) 1);
  assert (Int.equal re.top_backref 1);
  (* Duplicate names: (?(d) = OP_DNCREF, (?(R&d) = OP_DNRREF, both with
     name-table index 0 and count 2 (pcre2test: "Cond ref <d>2" /
     "Cond recurse <d>2"). *)
  let re = ok "(?J)(?<d>a)(?<d>b)(?(d)c)" in
  assert (Int.equal (byte re 23) Opcodes.op_cond);
  assert (Int.equal (byte re 26) Opcodes.op_dncref);
  assert (Int.equal (get2 re.code 27) 0 (* index *));
  assert (Int.equal (get2 re.code 29) 2 (* count *));
  let re = ok "(?J)(?<d>a)(?<d>b)(?(R&d)c)" in
  assert (Int.equal (byte re 26) Opcodes.op_dnrref);
  assert (Int.equal (get2 re.code 27) 0);
  assert (Int.equal (get2 re.code 29) 2);
  (* /(?(DEFINE)(?<f>x))(?&f)/: OP_DEFINE is rewritten to OP_FALSE; the
     named subroutine call becomes OP_RECURSE with the offset of CBra 1
     (7) after the driver fixup pass. *)
  let re = ok "(?(DEFINE)(?<f>x))(?&f)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      23;
      Opcodes.op_cond;
      0;
      14;
      Opcodes.op_false;
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_char;
      0x78;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_ket;
      0;
      14;
      Opcodes.op_recurse;
      0;
      7;
      Opcodes.op_ket;
      0;
      23;
      Opcodes.op_end;
    ];
  (* Repeating a DEFINE group is pointless but allowed; the repeat is
     ignored (pcre2_compile.c:7450-7456 reads the OP_FALSE written
     above). *)
  let re = ok "(?(DEFINE)a)*b" in
  assert (Int.equal (byte re 3) Opcodes.op_cond);
  assert (Int.equal (byte re 6) Opcodes.op_false);
  assert (Int.equal (byte re 12) Opcodes.op_char);
  assert (Int.equal (byte re 17) Opcodes.op_end);
  (* VERSION conditions compile to OP_TRUE / OP_FALSE against 10.44. *)
  let re = ok "(?(VERSION>=10.4)yes|no)" in
  assert (Int.equal (byte re 6) Opcodes.op_true);
  let re = ok "(?(VERSION=10.44)yes|no)" in
  assert (Int.equal (byte re 6) Opcodes.op_true);
  let re = ok "(?(VERSION>=10.45)yes|no)" in
  assert (Int.equal (byte re 6) Opcodes.op_false);
  let re = ok "(?(VERSION=10.4)yes|no)" in
  assert (Int.equal (byte re 6) Opcodes.op_false);
  (* /(?(?=x)a|b)/: assertion condition — no condition opcode
     (skipunits = 0); the assertion is the first item in the group. *)
  let re = ok "(?(?=x)a|b)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      24;
      Opcodes.op_cond;
      0;
      13;
      Opcodes.op_assert;
      0;
      5;
      Opcodes.op_char;
      0x78;
      Opcodes.op_ket;
      0;
      5;
      Opcodes.op_char;
      0x61;
      Opcodes.op_alt;
      0;
      5;
      Opcodes.op_char;
      0x62;
      Opcodes.op_ket;
      0;
      18;
      Opcodes.op_ket;
      0;
      24;
      Opcodes.op_end;
    ];
  (* /(?R)/: whole-pattern recursion — OP_RECURSE with offset 0. *)
  let re = ok "(?R)" in
  assert_code re
    [
      Opcodes.op_bra;
      0;
      6;
      Opcodes.op_recurse;
      0;
      0;
      Opcodes.op_ket;
      0;
      6;
      Opcodes.op_end;
    ];
  (* Backward and forward numerical subroutine calls: the fixup pass
     replaces group numbers with offsets. *)
  let re = ok "a(x)(?1)b" in
  assert (Int.equal (byte re 15) Opcodes.op_recurse);
  assert (Int.equal (get re.code 16) 5 (* offset of CBra 1 *));
  let re = ok "(?+1)(x)" in
  assert (Int.equal (byte re 3) Opcodes.op_recurse);
  assert (Int.equal (get re.code 4) 6 (* forward reference to CBra 1 *));
  (* Repeated recursions to the same group exercise the fixup cache. *)
  let re = ok "(?2)(?2)(x)(y)" in
  assert (Int.equal (byte re 3) Opcodes.op_recurse);
  assert (Int.equal (get re.code 4) 19);
  assert (Int.equal (byte re 6) Opcodes.op_recurse);
  assert (Int.equal (get re.code 7) 19);
  (* Named recursion resolves to the first group with the name. *)
  let re = ok "(?&f)(?<f>x)" in
  assert (Int.equal (byte re 3) Opcodes.op_recurse);
  assert (Int.equal (get re.code 4) 6);
  (* Error numbers and offsets. ERR27 for an assertion condition reports
     the offset stored by the last GETPLUSOFFSET (the C's function-level
     `offset`, stale for META_COND_ASSERT). *)
  expect_err "(?(1)a|b|c)(x)" 0 Errors.err27 0;
  expect_err "(?<n>x)\\k<n>(?(?=y)a|b|c)" 0 Errors.err27 10;
  expect_err "(?(DEFINE)a|b)" 0 Errors.err54 3;
  expect_err "(?(x)a|b)" 0 Errors.err15 3;
  expect_err "(?(R&x)a|b)" 0 Errors.err15 5;
  expect_err "(?(R12)a|b)" 0 Errors.err15 3;
  expect_err "(?(R999999999999)a|b)" 0 Errors.err61 8;
  expect_err "(?(1)a|b)" 0 Errors.err15 2;
  expect_err "(?(1 )a|b)" 0 Errors.err24 4;
  expect_err "(?(0)a|b)" 0 Errors.err15 4;
  expect_err "a(?1)b" 0 Errors.err15 4;
  expect_err "(?-1)(x)" 0 Errors.err15 4;
  expect_err "(?R" 0 Errors.err58 3;
  expect_err "(?+)" 0 Errors.err29 2;
  expect_err "(?(?z)a)" 0 Errors.err28 2

(* Study integration (pcre2_compile.c:10937-10955 + pcre2_study.c): the
   start-of-match data PRIV(study) records in the compiled pattern.
   EVERY expected value below is pinned against the C oracle (pcre2test
   'info' on the real 10.44 library: "Starting code units", "First code
   unit" and "Subject length lower bound" lines). *)
let test_13 () =
  let ok pat =
    match pcre2_compile pat ~options:0 with
    | Ok re -> re
    | Error (e, o) ->
        failwith
          (Printf.sprintf "pcre2_compile %S: error %d at offset %d" pat e o)
  in
  let bit_set (re : re) c =
    not
      (Int.equal
         (Char.code (Bytes.get re.start_bitmap (c lsr 3))
         land (1 lsl (c land 7)))
         0)
  in
  let bitmap_is (re : re) (expected : int list) =
    assert (not (Int.equal (re.flags land firstmapset) 0));
    for c = 0 to 255 do
      assert (
        Bool.equal (bit_set re c)
          (List.exists (fun x -> Int.equal x c) expected))
    done
  in
  (* /[ab]c/: Starting code units: a b; lower bound 2. Two starting units
     that are not a caseless pair, so no FIRSTSET. *)
  let re = ok "[ab]c" in
  bitmap_is re [ 0x61; 0x62 ];
  assert (Int.equal (re.flags land firstset) 0);
  assert (Int.equal re.minlength 2);
  (* /(a|b)x/: the bitmap is built across the group's branches
     (set_start_bits recursion, pcre2_study.c:1204-1225). *)
  let re = ok "(a|b)x" in
  bitmap_is re [ 0x61; 0x62 ];
  assert (Int.equal re.minlength 2);
  (* /\d+/: OP_TYPEPOSPLUS fudges the pointer onto OP_DIGIT
     (pcre2_study.c:1456-1460, 1428-1431): the ten digits. *)
  let re = ok "\\d+" in
  bitmap_is re (List.init 10 (fun i -> 0x30 + i));
  assert (Int.equal re.minlength 1);
  (* /^x/: anchored with a first code unit — study skips set_start_bits
     entirely (pcre2_study.c:1778); no bitmap. *)
  let re = ok "^x" in
  assert (not (Int.equal (re.overall_options land Options.anchored) 0));
  assert (not (Int.equal (re.flags land firstset) 0));
  assert (Int.equal (re.flags land firstmapset) 0);
  assert (Int.equal re.minlength 1);
  (* /( *COMMIT)[ab]c/: OP_COMMIT is in set_start_bits' SSB_FAIL list
     (pcre2_study.c:1006), so no bitmap — but find_minlength skips the
     verb (742-752) and still yields 2. Oracle: no "Starting code units"
     line, lower bound 2. *)
  let re = ok "(*COMMIT)[ab]c" in
  assert (Int.equal (re.flags land (firstset lor firstmapset)) 0);
  assert (Int.equal re.minlength 2);
  (* /[Ww]ord/: exactly two starting units that ARE a caseless pair — the
     bitmap is replaced by a caseless first code unit
     (pcre2_study.c:1784-1877). Oracle: First code unit = 'W' (caseless);
     lower bound 4. *)
  let re = ok "[Ww]ord" in
  assert (
    Int.equal
      (re.flags land (firstset lor firstcaseless))
      (firstset lor firstcaseless));
  assert (Int.equal (re.flags land firstmapset) 0);
  assert (Int.equal re.first_codeunit (Char.code 'W'));
  assert (Int.equal re.minlength 4);
  (* /a*a/: the single-unit bitmap must NOT be promoted to a first code
     unit equal to the required code unit (pcre2_study.c:1855-1873).
     Oracle: Starting code units: a; Last code unit = 'a'; bound 1. *)
  let re = ok "a*a" in
  bitmap_is re [ 0x61 ];
  assert (Int.equal (re.flags land firstset) 0);
  assert (Int.equal re.minlength 1);
  (* find_minlength hand-pins (oracle "Subject length lower bound"):
     exact+upto repeats, alternation minimum, backreference expansion and
     group recursion. *)
  assert (Int.equal (ok "abc{2,4}").minlength 4);
  assert (Int.equal (ok "(a|bc)d").minlength 2);
  assert (Int.equal (ok "(ab)\\1").minlength 4);
  assert (Int.equal (ok "(a(?1)?b)").minlength 2)

let tests =
  [
    Alcotest.test_case "compile 0" `Quick test_0;
    Alcotest.test_case "compile 1" `Quick test_1;
    Alcotest.test_case "compile 2" `Quick test_2;
    Alcotest.test_case "compile 3" `Quick test_3;
    Alcotest.test_case "compile 4" `Quick test_4;
    Alcotest.test_case "compile 5" `Quick test_5;
    Alcotest.test_case "compile 6" `Quick test_6;
    Alcotest.test_case "compile 7" `Quick test_7;
    Alcotest.test_case "compile 8" `Quick test_8;
    Alcotest.test_case "compile 9" `Quick test_9;
    Alcotest.test_case "compile 10" `Quick test_10;
    Alcotest.test_case "compile 11" `Quick test_11;
    Alcotest.test_case "compile 12" `Quick test_12;
    Alcotest.test_case "compile 13" `Quick test_13;
  ]
