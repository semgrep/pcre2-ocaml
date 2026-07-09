(* Engine module-initialization asserts for [Pcre2_engine.Parse], migrated
   verbatim from src/engine/parse.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Parse

(* META encoding scheme. *)
let test_0 () =
  assert (Int.equal (meta_code (meta_capture lor 12)) meta_capture);
  assert (Int.equal (meta_data (meta_capture lor 12)) 12);
  assert (Int.equal (meta_diff meta_minmax_query meta_end) 0x3f);
  (* meta_extra_lengths covers META_END..META_MINMAX_QUERY. *)
  assert (Int.equal (Array.length meta_extra_lengths) 64);
  assert (Int.equal meta_extra_lengths.(meta_diff meta_minmax meta_end) 2);
  assert (Int.equal meta_first_quantifier meta_asterisk);
  assert (Int.equal meta_last_quantifier meta_minmax_query);
  assert (Int.equal meta_atomic_script_run 0x8fff_0000)

(* read_number on "123". *)
let test_1 () =
  let cx = make_context "123" in
  let ptr = ref 0 in
  let n = ref (-1) in
  let ok =
    read_number cx ptr ~allow_sign:(-1) ~max_value:Limits.max_repeat_count
      ~max_error:Errors.err5 n
  in
  assert ok;
  assert (Int.equal !n 123);
  assert (Int.equal !ptr 3);
  assert (Int.equal cx.errorcode 0);
  assert (Int.equal cx.ptr 0);
  assert (Int.equal cx.erroroffset 0)

(* read_repeat_counts on "{2,5}", "{3,}", "{4}" (pointer starts after '{'),
   plus the ERR5 (105) and ERR4 (104) error paths. *)
let test_2 () =
  let cx = make_context "{2,5}" in
  let ptr = ref 1 in
  let minr = ref (-1) in
  let maxr = ref (-1) in
  let ok = read_repeat_counts cx ptr (Some minr) (Some maxr) in
  assert ok;
  assert (Int.equal !minr 2);
  assert (Int.equal !maxr 5);
  assert (Int.equal !ptr 5);
  assert (Int.equal cx.errorcode 0)

let test_3 () =
  let cx = make_context "{3,}" in
  let ptr = ref 1 in
  let minr = ref (-1) in
  let maxr = ref (-1) in
  let ok = read_repeat_counts cx ptr (Some minr) (Some maxr) in
  assert ok;
  assert (Int.equal !minr 3);
  assert (Int.equal !maxr Limits.repeat_unlimited);
  assert (Int.equal !ptr 4)

let test_4 () =
  let cx = make_context "{4}" in
  let ptr = ref 1 in
  let minr = ref (-1) in
  let maxr = ref (-1) in
  let ok = read_repeat_counts cx ptr (Some minr) (Some maxr) in
  assert ok;
  assert (Int.equal !minr 4);
  assert (Int.equal !maxr 4);
  assert (Int.equal !ptr 3)

let test_5 () =
  (* n too big: ERR5 = 105 ("number too big in {} quantifier"). *)
  let cx = make_context "{99999}" in
  let ptr = ref 1 in
  let ok = read_repeat_counts cx ptr None None in
  assert (not ok);
  assert (Int.equal cx.errorcode Errors.err5);
  assert (Int.equal Errors.err5 105);
  (* max < min: ERR4 = 104 ("numbers out of order in {} quantifier"). *)
  let cx = make_context "{5,2}" in
  let ptr = ref 1 in
  let ok = read_repeat_counts cx ptr None None in
  assert (not ok);
  assert (Int.equal cx.errorcode Errors.err4);
  (* not a quantifier: pointer must stay untouched, errorcode 0. *)
  let cx = make_context "{123456ABC" in
  let ptr = ref 1 in
  let ok = read_repeat_counts cx ptr None None in
  assert (not ok);
  assert (Int.equal cx.errorcode 0);
  assert (Int.equal !ptr 1)

(* POSIX class syntax and names: "alpha" is class 0, "junk" is unknown. *)
let test_6 () =
  let cx = make_context "[:alpha:]" in
  let endp = ref (-1) in
  assert (check_posix_syntax cx 1 endp);
  assert (Int.equal !endp 7);
  assert (Int.equal (check_posix_name cx 2 5) 0);
  let cx = make_context "junk" in
  assert (Int.equal (check_posix_name cx 0 4) (-1))

(* read_name on "<foo>" (terminator '>') and the ERR44 path. *)
let test_7 () =
  let cx = make_context "<foo>" in
  let ptr = ref 0 in
  let offset = ref (-1) in
  let name = ref (-1) in
  let namelen = ref (-1) in
  let ok =
    read_name cx ptr ~utf:false ~terminator:(Char.code '>') offset name namelen
  in
  assert ok;
  assert (Int.equal !name 1);
  assert (Int.equal !offset 1);
  assert (Int.equal !namelen 3);
  assert (Int.equal !ptr 5);
  let cx = make_context "<9a>" in
  let ptr = ref 0 in
  let ok =
    read_name cx ptr ~utf:false ~terminator:(Char.code '>') offset name namelen
  in
  assert (not ok);
  assert (Int.equal cx.errorcode Errors.err44);
  assert (Int.equal Errors.err44 144)

(* ESC-code encoding invariants: the escape values correspond in order to the
   opcodes OP_SOD..OP_EOD (pcre2_internal.h:1343-1344,1369-1372), and the
   escapes table covers exactly '0'..'z'. *)
let test_8 () =
  assert (Int.equal esc_big_a Opcodes.op_sod);
  assert (Int.equal esc_b Opcodes.op_word_boundary);
  assert (Int.equal esc_big_n Opcodes.op_any);
  assert (Int.equal esc_dum Opcodes.op_allany);
  assert (Int.equal esc_z Opcodes.op_eod);
  assert (Int.equal esc_ub 29);
  assert (Int.equal (Array.length escapes) (escapes_last - escapes_first + 1));
  assert (Int.equal (Array.length xdigitab) 256);
  assert (Int.equal (xdigit (Char.code 'f')) 15);
  assert (Int.equal (xdigit (Char.code 'G')) 0xff);
  assert (Int.equal escapes.(Char.code 'n' - escapes_first) 0x0a);
  assert (Int.equal escapes.(Char.code 'Q' - escapes_first) (-esc_big_q))

(* check_escape sanity checks. Each expected quadruple
   (escape, chptr, ptrptr, errorcode) below was traced against
   pcre2_compile.c:1550-2141; chptr = -1 / ptrptr = 0 mean "not written"
   (the ERR1 and mid-function return-0 paths). *)
let test_9 () =
  let run ?(options = 0) ?(xoptions = 0) ?(isclass = false) ?(sub = false)
      ?(bracount = 0) pat =
    let cx = make_context pat in
    cx.bracount <- bracount;
    let ptr = ref 0 in
    let ch = ref (-1) in
    let esc =
      check_escape cx ptr ch ~options ~xoptions ~isclass
        (if sub then None else Some cx)
    in
    (esc, !ch, !ptr, cx.errorcode)
  in
  let eq (e, c, p, ec) (e', c', p', ec') =
    Int.equal e e' && Int.equal c c' && Int.equal p p' && Int.equal ec ec'
  in

  (* Simple data escapes from the table: \n \e; \: literal; \r with and
     without PCRE2_EXTRA_ESCAPED_CR_IS_LF. *)
  assert (eq (run "n") (0, 0x0a, 1, 0));
  assert (eq (run "e") (0, 0x1b, 1, 0));
  assert (eq (run ":") (0, 0x3a, 1, 0));
  assert (eq (run "r") (0, 0x0d, 1, 0));
  assert (eq (run "r" ~xoptions:Options.extra_escaped_cr_is_lf) (0, 0x0a, 1, 0));
  (* Out-of-table code units are definitely literal. *)
  assert (eq (run "~") (0, 0x7e, 1, 0));
  assert (eq (run ".") (0, 0x2e, 1, 0));

  (* Special escapes: \Q \E \A \d \k; \Q..\E quoting itself is handled by
     parse_regex — check_escape just returns ESC_Q/ESC_E (per the C). *)
  assert (eq (run "Q") (esc_big_q, Char.code 'Q', 1, 0));
  assert (eq (run "E") (esc_big_e, Char.code 'E', 1, 0));
  assert (eq (run "A") (esc_big_a, Char.code 'A', 1, 0));
  assert (eq (run "d") (esc_d, Char.code 'd', 1, 0));
  assert (eq (run "k") (esc_k, Char.code 'k', 1, 0));

  (* \P sets the HASBKPORX external flag when cb is present. *)
  let cx = make_context "P" in
  let ptr = ref 0 in
  let ch = ref (-1) in
  let esc =
    check_escape cx ptr ch ~options:0 ~xoptions:0 ~isclass:false (Some cx)
  in
  assert (Int.equal esc esc_big_p);
  assert (Int.equal (cx.external_flags land hasbkporx) hasbkporx);

  (* \N: plain, quantified, \N{U+} rejected in non-UTF mode (ERR93),
     \N{name} unsupported (ERR37); \N{U+41} accepted in UTF mode via the
     COME_FROM_NU path. *)
  assert (eq (run "N") (esc_big_n, Char.code 'N', 1, 0));
  assert (eq (run "N{2,3}") (esc_big_n, Char.code 'N', 1, 0));
  assert (eq (run "N{U+41}") (esc_big_n, Char.code 'N', 1, Errors.err93));
  assert (eq (run "N{name}") (esc_big_n, Char.code 'N', 1, Errors.err37));
  assert (eq (run "N{U+41}" ~options:Options.utf) (0, 0x41, 7, 0));

  (* Hex escapes: \x41 \x{7f} (spaces allowed inside braces), plain \x is a
     binary zero; ERR78/ERR67/ERR34 paths. *)
  assert (eq (run "x41") (0, 0x41, 3, 0));
  assert (eq (run "x{7f}") (0, 0x7f, 5, 0));
  assert (eq (run "x{ 7f }") (0, 0x7f, 7, 0));
  assert (eq (run "x") (0, 0, 1, 0));
  assert (eq (run "xg") (0, 0, 1, 0));
  assert (eq (run "x{}") (0, Char.code 'x', 2, Errors.err78));
  assert (eq (run "x{2f") (0, 0x2f, 3, Errors.err67));
  assert (eq (run "x{zz}") (0, 0, 2, Errors.err67));
  assert (
    Int.equal
      (let _, _, _, ec = run "x{110000}" in
       ec)
      Errors.err34);

  (* Octal: \0 \07 \377 (classified as octal, = 255), \777 overflows the
     8-bit non-UTF limit (ERR51); \o{101} and its error paths. *)
  assert (eq (run "0") (0, 0, 1, 0));
  assert (eq (run "07") (0, 7, 2, 0));
  assert (eq (run "377") (0, 255, 3, 0));
  assert (eq (run "377" ~isclass:true) (0, 255, 3, 0));
  assert (eq (run "777") (0, 511, 3, Errors.err51));
  assert (eq (run "o{101}") (0, 65, 6, 0));
  assert (eq (run "o") (0, Char.code 'o', 0, Errors.err55));
  assert (eq (run "oz") (0, Char.code 'o', 1, Errors.err55));
  assert (eq (run "o{}") (0, Char.code 'o', 2, Errors.err78));
  assert (eq (run "o{12") (0, 0o12, 3, Errors.err64));
  assert (eq (run "o{400}") (0, 256, 5, Errors.err34));

  (* Backslash-digit disambiguation: \1..\9 always back references; \8x \9x
     too; \1x..\7x octal when there aren't that many captures; inside a
     class always literal-or-octal. \8 classifies as backreference -8 here;
     ERR15 for the non-existent group is diagnosed later, in parse_regex
     (M2). *)
  assert (eq (run "8") (-8, Char.code '8', 1, 0));
  assert (eq (run "9x") (-9, Char.code '9', 1, 0));
  assert (eq (run "11") (0, 0o11, 2, 0));
  assert (eq (run "12" ~bracount:12) (-12, Char.code '1', 2, 0));
  assert (eq (run "8" ~isclass:true) (0, Char.code '8', 1, 0));
  assert (eq (run "9" ~isclass:true) (0, Char.code '9', 1, 0));

  (* \g: subroutine-call forms, braced and plain numbers, relative numbers,
     name fallback to ESC_k, error paths, literal inside a class. *)
  assert (eq (run "g<name>") (esc_g, Char.code 'g', 1, 0));
  assert (eq (run "g'1'") (esc_g, Char.code 'g', 1, 0));
  assert (eq (run "g1" ~bracount:1) (-1, Char.code 'g', 2, 0));
  assert (eq (run "g{2}" ~bracount:2) (-2, Char.code 'g', 4, 0));
  assert (eq (run "g{-1}" ~bracount:3) (-3, Char.code 'g', 5, 0));
  assert (eq (run "g{name}") (esc_k, Char.code 'g', 1, 0));
  assert (eq (run "g") (0, Char.code 'g', 1, Errors.err57));
  assert (eq (run "g{0}") (0, Char.code 'g', 4, Errors.err15));
  assert (eq (run "g" ~isclass:true) (0, Char.code 'g', 1, 0));

  (* \c: value, lower-case letter upper-cased, error paths. *)
  assert (eq (run "cA") (0, 1, 2, 0));
  assert (eq (run "ca") (0, 1, 2, 0));
  assert (eq (run "c{") (0, 0x3b, 2, 0));
  assert (eq (run "c") (0, Char.code 'c', 1, Errors.err2));
  assert (eq (run "c\x01") (0, 1, 1, Errors.err68));

  (* alt_bsux: \u as 4 hex digits (PCRE2_ALT_BSUX), \u{...} braced form
     (PCRE2_EXTRA_ALT_BSUX), the ESC_ub special return for \u{ 12}, the
     out-of-range check, and \x as exactly 2 hex digits. Without either
     option, \u and \U are ERR37. *)
  assert (eq (run "u0041" ~options:Options.alt_bsux) (0, 0x41, 5, 0));
  assert (eq (run "u12" ~options:Options.alt_bsux) (0, Char.code 'u', 1, 0));
  assert (eq (run "u{2b}" ~xoptions:Options.extra_alt_bsux) (0, 0x2b, 5, 0));
  assert (
    eq
      (run "u{ 12}" ~xoptions:Options.extra_alt_bsux)
      (esc_ub, Char.code 'u', 2, 0));
  assert (
    Int.equal
      (let _, _, _, ec = run "u{110000}" ~xoptions:Options.extra_alt_bsux in
       ec)
      Errors.err77);
  assert (eq (run "x4z" ~options:Options.alt_bsux) (0, Char.code 'x', 1, 0));
  assert (eq (run "u0041") (0, Char.code 'u', 1, Errors.err37));
  assert (eq (run "U") (0, Char.code 'U', 1, Errors.err37));
  assert (eq (run "U" ~options:Options.alt_bsux) (0, Char.code 'U', 1, 0));

  (* Unsupported Perl escapes and unknown alphanumerics: ERR37 / ERR3. The
     ERR3 default case is an early return with ptrptr pointing at the
     character at fault and chptr unwritten. *)
  assert (eq (run "L") (0, Char.code 'L', 1, Errors.err37));
  assert (eq (run "F") (0, Char.code 'F', 1, Errors.err37));
  assert (eq (run "i") (0, -1, 0, Errors.err3));
  (* \ at end of pattern: ERR1, nothing written. *)
  assert (eq (run "") (0, -1, 0, Errors.err1));

  (* pcre2_substitute() filter (cb = None): the filter lives in the
     further-processing branch, so table escapes such as \d still come back
     as specials; of the zero-entry characters only \c, \o, and \x are
     recognized (even \L is ERR3 here, not ERR37), and alt_bsux is forced
     off so \x keeps Perl semantics. *)
  assert (eq (run "d" ~sub:true) (esc_d, Char.code 'd', 1, 0));
  assert (eq (run "g1" ~sub:true) (0, -1, 0, Errors.err3));
  assert (eq (run "L" ~sub:true) (0, -1, 0, Errors.err3));
  assert (eq (run "x41" ~sub:true) (0, 0x41, 3, 0));
  assert (eq (run "x4" ~sub:true ~options:Options.alt_bsux) (0, 4, 2, 0))

(* parse_regex sanity checks. Every expected parsed-pattern stream and
   (errorcode, erroroffset) pair below was traced against
   pcre2_compile.c:2773-5039. run_parse mirrors pcre2_compile()'s driver
   steps: size the vector, then parse from offset 0. *)
let test_10 () =
  let run_parse ?(options = 0) ?(extra = 0) pat =
    let cx = make_context pat in
    cx.external_options <- options (* pcre2_compile.c:10254 *);
    cx.extra_options <- extra;
    allocate_parsed_pattern cx ~options;
    let hlb = ref false in
    let rc = parse_regex cx ~options hlb in
    (cx, rc)
  in
  let expect ?options ?extra pat expected =
    let cx, rc = run_parse ?options ?extra pat in
    assert (Int.equal rc 0);
    Array.iteri (fun i v -> assert (Int.equal cx.parsed_pattern.(i) v)) expected
  in
  let expect_err ?options ?extra pat code offset =
    let cx, rc = run_parse ?options ?extra pat in
    assert (Int.equal rc code);
    assert (Int.equal cx.erroroffset offset)
  in

  (* Literals and META_END. *)
  expect "abc" [| 0x61; 0x62; 0x63; meta_end |];
  expect "" [| meta_end |];

  (* Alternation. *)
  expect "a|b" [| 0x61; meta_alt; 0x62; meta_end |];

  (* \Q..\E quoting; an isolated \E is ignored. *)
  expect "\\Qa.b\\E." [| 0x61; 0x2e; 0x62; meta_dot; meta_end |];
  expect "a\\Eb" [| 0x61; 0x62; meta_end |];

  (* Comments: (?#...) always; # only in extended mode. *)
  expect "a(?#c)b" [| 0x61; 0x62; meta_end |];
  expect_err "a(?#b" Errors.err18 5;
  expect ~options:Options.extended "a b # c\n d"
    [| 0x61; 0x62; 0x64; meta_end |];
  (* PCRE2_EXTENDED_MORE implies PCRE2_EXTENDED (pcre2_compile.c:2858-2860). *)
  expect ~options:Options.extended_more "a\tb" [| 0x61; 0x62; meta_end |];

  (* PCRE2_LITERAL mode: metacharacters are data. *)
  expect ~options:Options.literal "a*b" [| 0x61; 0x2a; 0x62; meta_end |];

  (* Anchors and dot. *)
  expect "^a$." [| meta_circumflex; 0x61; meta_dollar; meta_dot; meta_end |];

  (* Escape dispatch: type escapes, non-quantifiable escapes, data
     escapes. *)
  expect "\\d\\s\\w"
    [|
      meta_escape + esc_d; meta_escape + esc_s; meta_escape + esc_w; meta_end;
    |];
  expect "\\A\\b\\R\\C"
    [|
      meta_escape + esc_big_a;
      meta_escape + esc_b;
      meta_escape + esc_big_r;
      meta_escape + esc_big_c;
      meta_end;
    |];
  expect "\\x41\\n" [| 0x41; 0x0a; meta_end |];
  (* UCP mode rewrites \d/\S via handle_escdsw; an ASCII extra option keeps
     the non-property form. *)
  expect ~options:Options.ucp "\\d"
    [| meta_escape + esc_p; (Opcodes.pt_pc lsl 16) lor Ucp.ucp_nd; meta_end |];
  expect ~options:Options.ucp "\\S"
    [| meta_escape + esc_big_p; Opcodes.pt_space lsl 16; meta_end |];
  expect ~options:Options.ucp ~extra:Options.extra_ascii_bsw "\\w"
    [| meta_escape + esc_w; meta_end |];
  (* \C under PCRE2_NEVER_BACKSLASH_C. *)
  expect_err ~options:Options.never_backslash_c "\\C" Errors.err83 2;
  (* Bad escapes: fatal, or a literal under
     PCRE2_EXTRA_BAD_ESCAPE_IS_LITERAL (ESCAPE_FAILED recovery). *)
  expect_err "\\j" Errors.err3 1;
  expect ~extra:Options.extra_bad_escape_is_literal "\\j" [| 0x6a; meta_end |];

  (* Numeric back references: \1..\9 record their first offset in
     small_ref_offset; \g{12} stores the offset in the parsed pattern. *)
  let cx, rc = run_parse "()\\1" in
  assert (Int.equal rc 0);
  assert (Int.equal cx.parsed_pattern.(0) (meta_capture lor 1));
  assert (Int.equal cx.parsed_pattern.(1) meta_ket);
  assert (Int.equal cx.parsed_pattern.(2) (meta_backref lor 1));
  assert (Int.equal cx.parsed_pattern.(3) meta_end);
  assert (Int.equal cx.small_ref_offset.(1) 3);
  assert (Int.equal cx.small_ref_offset.(2) pcre2_unset);
  expect "\\g{12}" [| meta_backref lor 12; 5; meta_end |];

  (* Groups: capturing, non-capturing via option, (?:, and (?|. *)
  expect "(a)" [| meta_capture lor 1; 0x61; meta_ket; meta_end |];
  expect ~options:Options.no_auto_capture "(a)"
    [| meta_nocapture; 0x61; meta_ket; meta_end |];
  expect "(?:a)" [| meta_nocapture; 0x61; meta_ket; meta_end |];
  expect "(?|a|b)"
    [| meta_nocapture; 0x61; meta_alt; 0x62; meta_ket; meta_end |];
  let cx, rc = run_parse "(?|(a)|(b)(c))(d)" in
  assert (Int.equal rc 0);
  assert (Int.equal cx.bracount 3);
  assert (Int.equal (cx.external_flags land dupcapused) dupcapused);

  (* Inline option settings and their scope. *)
  expect "(?i)x" [| meta_options; Options.caseless; 0; 0x78; meta_end |];
  expect "(?i)a(?-i)b"
    [|
      meta_options;
      Options.caseless;
      0;
      0x61;
      meta_options;
      0;
      0;
      0x62;
      meta_end;
    |];
  expect "(?-i)a" [| 0x61; meta_end |];
  (* (?i: emits META_NOCAPTURE before META_OPTIONS; ) restores the tracked
     options from the nest stack. *)
  expect "(?i:a)b"
    [|
      meta_nocapture;
      meta_options;
      Options.caseless;
      0;
      0x61;
      meta_ket;
      0x62;
      meta_end;
    |];
  expect "((?i)a)"
    [|
      meta_capture lor 1;
      meta_options;
      Options.caseless;
      0;
      0x61;
      meta_ket;
      meta_end;
    |];
  (* (?xx) inside the pattern turns on extended-more whitespace skipping. *)
  expect "(?xx)a b"
    [|
      meta_options;
      Options.extended lor Options.extended_more;
      0;
      0x61;
      0x62;
      meta_end;
    |];
  (* (?^) unsets imnsx; from multiline it is a change worth recording. *)
  expect ~options:Options.multiline "(?^)a"
    [| meta_options; 0; 0; 0x61; meta_end |];
  expect_err "(?^-i)" Errors.err94 3;
  expect_err "(?z)" Errors.err11 2;

  (* Parenthesis bookkeeping errors. *)
  expect_err "(" Errors.err14 1;
  expect_err "(a" Errors.err14 2;
  expect_err "(?i" Errors.err14 3;
  expect_err ")" Errors.err22 0;
  expect_err (String.make 252 '(') Errors.err19 251;

  (* Quantifiers (parse_regex C): * + ? {n,m} plus the lazy/possessive
     modifier adjustment at the top of the loop. Each stream was traced
     against pcre2_compile.c:3425-3449, 3452-3490 and 3179-3191. *)
  expect "a*" [| 0x61; meta_asterisk; meta_end |];
  expect "a+" [| 0x61; meta_plus; meta_end |];
  expect "a?" [| 0x61; meta_query; meta_end |];
  expect "a*?" [| 0x61; meta_asterisk_query; meta_end |];
  expect "a+?" [| 0x61; meta_plus_query; meta_end |];
  expect "a*+" [| 0x61; meta_asterisk_plus; meta_end |];
  expect "a?+" [| 0x61; meta_query_plus; meta_end |];
  expect "a{2,5}" [| 0x61; meta_minmax; 2; 5; meta_end |];
  expect "a{2}?" [| 0x61; meta_minmax_query; 2; 2; meta_end |];
  expect "a{2,}+"
    [| 0x61; meta_minmax_plus; 2; Limits.repeat_unlimited; meta_end |];
  (* {,m} is a quantifier meaning {0,m} (pcre2_compile.c:1461-1465). *)
  expect "a{,5}" [| 0x61; meta_minmax; 0; 5; meta_end |];
  (* Comments and /x white space between a quantifier and its + or ?
     modifier are ignored (pcre2_compile.c:3179-3191). *)
  expect "a*(?#c)?" [| 0x61; meta_asterisk_query; meta_end |];
  expect "ab|c*" [| 0x61; 0x62; meta_alt; 0x63; meta_asterisk; meta_end |];
  expect "(a)*"
    [| meta_capture lor 1; 0x61; meta_ket; meta_asterisk; meta_end |];
  expect "[ab]+"
    [| meta_class; 0x61; 0x62; meta_class_end; meta_plus; meta_end |];
  (* ERR9 "quantifier does not follow a repeatable item"; FAILED_BACK makes
     the offset point at the quantifier character (at its final code unit,
     for {n,m}). *)
  expect_err "*" Errors.err9 0;
  expect_err "+a" Errors.err9 0;
  expect_err "a**" Errors.err9 2;
  expect_err "(?i)*" Errors.err9 4;
  expect_err "\\b*" Errors.err9 2;
  expect_err "a{2}{3}" Errors.err9 6;
  (* Quantifier errors from the {n,m} syntax; a non-quantifier brace is a
     literal. *)
  expect_err "a{2,1}" Errors.err4 5;
  expect "a{,}b" [| 0x61; 0x7b; 0x2c; 0x7d; 0x62; meta_end |];

  (* PCRE2_EXTRA_MATCH_LINE / _WORD leading and trailing items. *)
  expect ~extra:Options.extra_match_line "a"
    [| meta_circumflex; meta_nocapture; 0x61; meta_ket; meta_dollar; meta_end |];
  expect ~extra:Options.extra_match_word "a"
    [|
      meta_escape + esc_b;
      meta_nocapture;
      0x61;
      meta_ket;
      meta_escape + esc_b;
      meta_end;
    |];

  (* PCRE2_AUTO_CALLOUT via manage_callouts: a numerical callout (255)
     before every item and at the end, with [1] = pattern offset and
     [2] = length of the preceding item. *)
  expect ~options:Options.auto_callout "ab"
    [|
      meta_callout_number;
      0;
      1;
      255;
      0x61;
      meta_callout_number;
      1;
      1;
      255;
      0x62;
      meta_callout_number;
      2;
      0;
      255;
      meta_end;
    |];

  (* Explicit callouts (pcre2_compile.c:4449-4563): numerical (?Cn) and
     string (?C"text") forms. Element [1] is the pattern offset just past
     the callout's closing parenthesis, [2] the length of the next item
     (filled in by a later manage_callouts cycle — for a manual callout
     the item immediately after it is skipped via after_manual_callout).
     Error numbers and offsets pinned against pcre2test 10.44 (and
     conformance testinput2:1299-1302). *)
  expect "(?C1)abc"
    [| meta_callout_number; 5; 1; 1; 0x61; 0x62; 0x63; meta_end |];
  (* A missing numerical argument gives 0 (pcre2_compile.c:4529-4530). *)
  expect "(?C)a" [| meta_callout_number; 4; 1; 0; 0x61; meta_end |];
  (* String form: [3] = length including both delimiters, [4] = pattern
     offset of the starting delimiter. *)
  expect "(?C\"text\")x" [| meta_callout_string; 10; 1; 6; 3; 0x78; meta_end |];
  (* Bracket-like {..} delimiters; a doubled ending delimiter is a literal
     occurrence and does not end the string (pcre2_compile.c:4506-4516). *)
  expect "(?C{ab}}c})z" [| meta_callout_string; 11; 1; 7; 3; 0x7a; meta_end |];
  (* An automatic callout immediately preceding a manual one is abolished
     (pcre2_compile.c:4465-4473). *)
  expect ~options:Options.auto_callout "a(?C1)b"
    [|
      meta_callout_number;
      0;
      1;
      255;
      0x61;
      meta_callout_number;
      6;
      1;
      1;
      0x62;
      meta_callout_number;
      7;
      0;
      255;
      meta_end;
    |];
  (* ERR81 missing terminating delimiter (ptr reset to the starting
     delimiter for the message, pcre2_compile.c:4508-4513); ERR39 closing
     parenthesis expected (4549-4555); ERR82 unrecognized delimiter
     (4497-4501); ERR38 number > 255 (4540-4544); ERR14 for (?C at end of
     pattern (4452). *)
  expect_err "a(?C\"" Errors.err81 4;
  expect_err "a(?C\"a" Errors.err81 4;
  expect_err "a(?C\"a\"" Errors.err39 7;
  expect_err "a(?C\"a\"bcde(?C\"b\")xyz" Errors.err39 7;
  expect_err "a(?Cx)" Errors.err82 4;
  expect_err "(?C256)" Errors.err38 6;
  expect_err "(?C" Errors.err14 3;

  (* Character classes (parse_regex B). Each expected stream / error
     offset below was traced against pcre2_compile.c:3493-3915. *)
  expect "[abc]" [| meta_class; 0x61; 0x62; 0x63; meta_class_end; meta_end |];
  expect "[^a-z]"
    [|
      meta_class_not; 0x61; meta_range_literal; 0x7a; meta_class_end; meta_end;
    |];
  (* ]-as-first-char literal rule vs PCRE2_ALLOW_EMPTY_CLASS (the check is
     on cb->external_options). *)
  expect "[]a]" [| meta_class; 0x5d; 0x61; meta_class_end; meta_end |];
  expect ~options:Options.allow_empty_class "[]a]"
    [| meta_class_empty; 0x61; 0x5d; meta_end |];
  expect ~options:Options.allow_empty_class "[^]"
    [| meta_class_empty_not; meta_end |];
  (* -] at the end of a class is a literal '-'; [a-a] optimizes to a single
     character; extended-more skips spaces inside classes. *)
  expect "[a-]" [| meta_class; 0x61; 0x2d; meta_class_end; meta_end |];
  expect "[a-a]" [| meta_class; 0x61; meta_class_end; meta_end |];
  expect ~options:Options.extended_more "[ a b]"
    [| meta_class; 0x61; 0x62; meta_class_end; meta_end |];
  (* \Q..\E inside a class; \b is backspace in a class. *)
  expect "[\\Qa]\\E]" [| meta_class; 0x61; 0x5d; meta_class_end; meta_end |];
  expect "[\\b]" [| meta_class; 0x08; meta_class_end; meta_end |];
  (* Escaped range endpoints use META_RANGE_ESCAPED, whether the escape is
     the start or (converting META_RANGE_LITERAL) the end. *)
  expect "[\\x41-\\x5a]"
    [| meta_class; 0x41; meta_range_escaped; 0x5a; meta_class_end; meta_end |];
  expect "[A-\\x5a]"
    [| meta_class; 0x41; meta_range_escaped; 0x5a; meta_class_end; meta_end |];
  (* Class-specific escapes: \d via handle_escdsw; \h emitted directly. *)
  expect "[\\d\\h]"
    [|
      meta_class;
      meta_escape + esc_d;
      meta_escape + esc_h;
      meta_class_end;
      meta_end;
    |];
  (* POSIX class items, plain and negated; the UCP substitutions from
     posix_substitutes ([:alpha:] -> \p{L}, [:blank:] -> \h, [:ascii:]
     falls through) and the ASCII-forcing extra options. *)
  expect "[[:alpha:]]" [| meta_class; meta_posix; 0; meta_class_end; meta_end |];
  expect "[[:^digit:]]"
    [| meta_class; meta_posix_neg; pc_digit; meta_class_end; meta_end |];
  expect ~options:Options.ucp "[[:alpha:]]"
    [|
      meta_class;
      meta_escape + esc_p;
      (Opcodes.pt_gc lsl 16) lor Ucp.ucp_l;
      meta_class_end;
      meta_end;
    |];
  expect ~options:Options.ucp "[[:^alpha:]]"
    [|
      meta_class;
      meta_escape + esc_big_p;
      (Opcodes.pt_gc lsl 16) lor Ucp.ucp_l;
      meta_class_end;
      meta_end;
    |];
  expect ~options:Options.ucp "[[:blank:]]"
    [| meta_class; meta_escape + esc_h; meta_class_end; meta_end |];
  expect ~options:Options.ucp "[[:ascii:]]"
    [| meta_class; meta_posix; 4; meta_class_end; meta_end |];
  expect ~options:Options.ucp ~extra:Options.extra_ascii_digit "[[:digit:]]"
    [| meta_class; meta_posix; pc_digit; meta_class_end; meta_end |];
  (* [[:<:]] and [[:>:]] become \b(?=\w) and \b(?<=\w). *)
  expect "[[:<:]]"
    [|
      meta_escape + esc_b;
      meta_lookahead;
      meta_escape + esc_w;
      meta_ket;
      meta_end;
    |];
  (* Class error sites: ERR6 missing ], ERR8 range out of order, ERR7 bad
     escape in class, ERR71 \N, ERR12/ERR13 top-level POSIX items, ERR13
     collating elements, ERR30 unknown POSIX name, ERR50 invalid ranges
     around POSIX items and type escapes. *)
  expect_err "[" Errors.err6 1;
  expect_err "[abc" Errors.err6 4;
  expect_err "[z-a]" Errors.err8 3;
  expect_err "[\\B]" Errors.err7 2;
  expect_err "[\\A]" Errors.err7 2;
  expect_err "[\\N]" Errors.err71 3;
  expect_err "[:alpha:]" Errors.err12 0;
  expect_err "[=ch=]" Errors.err13 0;
  expect_err "[a[.ch.]]" Errors.err13 2;
  expect_err "[[:foo:]]" Errors.err30 3;
  expect_err "[a-[:alpha:]]" Errors.err50 4;
  expect_err "[[:alpha:]-a]" Errors.err50 10;
  expect_err "[\\d-x]" Errors.err50 3;
  expect_err "[a-\\d]" Errors.err50 5;
  (* \p and \P inside a class (pcre2_compile.c:3857-3875): META_ESCAPE +
     ESC_p/ESC_P followed by the (ptype << 16) | pdata word. In-class
     get_ucp failures are `goto FAILED` — no BAD_ESCAPE_IS_LITERAL
     recovery (oracle: /[\p{Zz}]/ fails even with bad_escape_is_literal). *)
  expect "[\\p{L}]"
    [|
      meta_class;
      meta_escape + esc_p;
      (Opcodes.pt_gc lsl 16) lor Ucp.ucp_l;
      meta_class_end;
      meta_end;
    |];
  expect "[\\P{Nd}]"
    [|
      meta_class;
      meta_escape + esc_big_p;
      (Opcodes.pt_pc lsl 16) lor Ucp.ucp_nd;
      meta_class_end;
      meta_end;
    |];
  expect_err "[\\p{Zz}]" Errors.err47 7;
  expect_err ~extra:Options.extra_bad_escape_is_literal "[\\p{Zz}]" Errors.err47
    7;

  (* [[:>:]] also sets the has_lookbehind flag and stores a zero offset. *)
  (let cx = make_context "[[:>:]]" in
   allocate_parsed_pattern cx ~options:0;
   let hlb = ref false in
   let rc = parse_regex cx ~options:0 hlb in
   assert (Int.equal rc 0);
   assert !hlb;
   Array.iteri
     (fun i v -> assert (Int.equal cx.parsed_pattern.(i) v))
     [|
       meta_escape + esc_b;
       meta_lookbehind;
       0;
       meta_escape + esc_w;
       meta_ket;
       meta_end;
     |]);

  (* Named-group definitions (DEFINE_NAME, pcre2_compile.c:4824-4923) via
     (?<name>, (?'name', and (?P<name>. *)
  let check_named pat_str =
    let cx, rc = run_parse pat_str in
    assert (Int.equal rc 0);
    Array.iteri
      (fun i v -> assert (Int.equal cx.parsed_pattern.(i) v))
      [| meta_capture lor 1; 0x78; meta_ket; meta_end |];
    assert (Int.equal cx.bracount 1);
    assert (Int.equal cx.names_found 1);
    assert (Int.equal cx.name_entry_size (1 + Limits.imm2_size + 1));
    let ng = cx.named_groups.(0) in
    assert (Char.equal cx.pattern.[ng.name] 'n');
    assert (Int.equal ng.number 1);
    assert (Int.equal ng.length 1);
    assert (not ng.isdup)
  in
  check_named "(?<n>x)";
  check_named "(?'n'x)";
  check_named "(?P<n>x)";
  (* Duplicate names: ERR43 without PCRE2_DUPNAMES; with it, both entries
     are marked isdup and cb->dupnames is set. *)
  expect_err "(?<a>x)(?<a>y)" Errors.err43 12;
  (let cx, rc = run_parse ~options:Options.dupnames "(?<a>x)(?<a>y)" in
   assert (Int.equal rc 0);
   assert (Int.equal cx.names_found 2);
   assert cx.named_groups.(0).isdup;
   assert cx.named_groups.(1).isdup;
   assert (Int.equal cx.named_groups.(1).number 2);
   assert cx.dupnames);
  (* In a (?| group, a duplicate name with the same number is discarded
     (pcre2_compile.c:4878,4892); a different name for the same number is
     ERR65. *)
  (let cx, rc = run_parse "(?|(?<a>x)|(?<a>y))" in
   assert (Int.equal rc 0);
   assert (Int.equal cx.names_found 1);
   assert (not cx.named_groups.(0).isdup));
  expect_err "(?|(?<a>x)|(?<b>y))" Errors.err65 16;
  (* Malformed names and (?P errors. *)
  expect_err "(?<>x)" Errors.err62 3;
  expect_err "(?<9>x)" Errors.err44 3;
  expect_err "(?Pz)" Errors.err41 3;
  expect_err "(?P" Errors.err14 3;
  expect_err "(?<n>x" Errors.err14 6;

  (* Named back references: \k<n> \k'n' \k{n} \g{n} (pcre2_compile.c:
     3392-3402) and (?P=n) (pcre2_compile.c:4381-4387), ported with this
     chunk because the emission is name length + offset words only. *)
  expect "\\k<n>" [| meta_backref_byname; 1; 3; meta_end |];
  expect "\\k'n'" [| meta_backref_byname; 1; 3; meta_end |];
  expect "\\k{n}" [| meta_backref_byname; 1; 3; meta_end |];
  expect "\\g{n}" [| meta_backref_byname; 1; 3; meta_end |];
  expect "(?P=n)" [| meta_backref_byname; 1; 4; meta_end |];
  (* Named references are quantifiable. *)
  expect "(?P=n)?" [| meta_backref_byname; 1; 4; meta_query; meta_end |];

  (* Lookaround assertions and atomic groups (pcre2_compile.c:3955-4061,
     4741-4821): symbolic and alpha-assertion forms. The lookbehind forms
     store the offset of the assertion start (for length-error messages)
     and set has_lookbehind. *)
  expect "(?=a)" [| meta_lookahead; 0x61; meta_ket; meta_end |];
  expect "(?!a)" [| meta_lookaheadnot; 0x61; meta_ket; meta_end |];
  expect "(?*a)" [| meta_lookahead_na; 0x61; meta_ket; meta_end |];
  expect "(?>a)" [| meta_atomic; 0x61; meta_ket; meta_end |];
  expect "(?<=a)" [| meta_lookbehind; 0; 0x61; meta_ket; meta_end |];
  expect "(?<!a)" [| meta_lookbehindnot; 0; 0x61; meta_ket; meta_end |];
  expect "(?<*a)" [| meta_lookbehind_na; 0; 0x61; meta_ket; meta_end |];
  expect "x(?<=a)" [| 0x78; meta_lookbehind; 1; 0x61; meta_ket; meta_end |];
  expect "(*pla:a)" [| meta_lookahead; 0x61; meta_ket; meta_end |];
  expect "(*napla:a)" [| meta_lookahead_na; 0x61; meta_ket; meta_end |];
  expect "(*negative_lookahead:a)"
    [| meta_lookaheadnot; 0x61; meta_ket; meta_end |];
  expect "(*atomic:a)" [| meta_atomic; 0x61; meta_ket; meta_end |];
  (* Alpha lookbehinds: ptr is backed onto the last name character before
     POST_LOOKBEHIND, so the stored offset is (colon index - 3). *)
  expect "(*plb:a)" [| meta_lookbehind; 2; 0x61; meta_ket; meta_end |];
  expect "(*nlb:a)" [| meta_lookbehindnot; 2; 0x61; meta_ket; meta_end |];
  expect "(*naplb:a)" [| meta_lookbehind_na; 4; 0x61; meta_ket; meta_end |];
  (let cx = make_context "(?<=a)" in
   allocate_parsed_pattern cx ~options:0;
   let hlb = ref false in
   assert (Int.equal (parse_regex cx ~options:0 hlb) 0);
   assert !hlb);
  (let cx = make_context "(?=a)" in
   allocate_parsed_pattern cx ~options:0;
   let hlb = ref false in
   assert (Int.equal (parse_regex cx ~options:0 hlb) 0);
   assert (not !hlb));
  (* Alpha-assertion error paths: ERR95 for a malformed or unrecognized
     name (offsets verified against pcre2test 10.44: error 195 at offset
     5 for both). *)
  expect_err "(*plx:ab)" Errors.err95 5;
  expect_err "(*pla|ab)" Errors.err95 5;
  (* Unclosed assertion openers still fall out of the main loop as ERR14
     via the nest_depth check. *)
  expect_err "(?=a" Errors.err14 4;

  (* Conditional groups (pcre2_compile.c:4566-4738). Streams and error
     offsets pinned against pcre2test 10.44 (parse-time errors surface
     verbatim through pcre2_compile). *)
  expect "(?(1)a)" [| meta_cond_number; 2; 1; 0x61; meta_ket; meta_end |];
  expect "(?(+1)a)" [| meta_cond_number; 3; 1; 0x61; meta_ket; meta_end |];
  (* (?(-1) after one group: relative back to group 1. *)
  expect "(x)(?(-1)a)"
    [|
      meta_capture lor 1;
      0x78;
      meta_ket;
      meta_cond_number;
      6;
      1;
      0x61;
      meta_ket;
      meta_end;
    |];
  expect_err "(?(0)a)" Errors.err15 4 (* (?(0) is ERR15, not FAILED_BACK *);
  expect_err "(?(-1)a)(x)" Errors.err15 5;
  expect_err "(?(1 )a)" Errors.err24 4;
  (* Named conditions: Perl (?(<n>) (?('n') and Python (?(n) forms. *)
  expect "(?(<n>)a)(?<n>x)"
    [|
      meta_cond_name;
      1;
      4;
      0x61;
      meta_ket;
      meta_capture lor 1;
      0x78;
      meta_ket;
      meta_end;
    |];
  expect "(?('n')a)(?'n'x)"
    [|
      meta_cond_name;
      1;
      4;
      0x61;
      meta_ket;
      meta_capture lor 1;
      0x78;
      meta_ket;
      meta_end;
    |];
  expect "(?(n)a)(?<n>x)"
    [|
      meta_cond_name;
      1;
      3;
      0x61;
      meta_ket;
      meta_capture lor 1;
      0x78;
      meta_ket;
      meta_end;
    |];
  (* R / Rdigits are META_COND_RNUMBER (the compile phase disambiguates
     names); R followed by non-digits is a plain name; R&name is
     META_COND_RNAME. *)
  expect "(?(R)a)" [| meta_cond_rnumber; 1; 3; 0x61; meta_ket; meta_end |];
  expect "(?(R2)a)(x)(y)"
    [|
      meta_cond_rnumber;
      2;
      3;
      0x61;
      meta_ket;
      meta_capture lor 1;
      0x78;
      meta_ket;
      meta_capture lor 2;
      0x79;
      meta_ket;
      meta_end;
    |];
  expect "(?(Rx)a)" [| meta_cond_name; 2; 3; 0x61; meta_ket; meta_end |];
  expect "(?(R&n)a)(?<n>x)"
    [|
      meta_cond_rname;
      1;
      5;
      0x61;
      meta_ket;
      meta_capture lor 1;
      0x78;
      meta_ket;
      meta_end;
    |];
  (* DEFINE stores just the offset; a name that merely starts with DEFINE
     is an ordinary name. *)
  expect "(?(DEFINE)x)" [| meta_cond_define; 3; 0x78; meta_ket; meta_end |];
  expect "(?(DEFINEZ)x)" [| meta_cond_name; 7; 3; 0x78; meta_ket; meta_end |];
  (* VERSION conditions: ge flag, major, minor (tens-scaled single
     digit). *)
  expect "(?(VERSION>=10.4)y)"
    [| meta_cond_version; 1; 10; 40; 0x79; meta_ket; meta_end |];
  expect "(?(VERSION=10.44)y)"
    [| meta_cond_version; 0; 10; 44; 0x79; meta_ket; meta_end |];
  expect_err "(?(VERSION>=x)a)" Errors.err79 12;
  expect_err "(?(VERSION>=10.4x)a)" Errors.err79 16;
  expect_err "(?(VERSION>=1001)a)" Errors.err79 16;
  (* (?(VERSION) with ')' right after the name is the name "VERSION". *)
  expect "(?(VERSION)a|b)(?<VERSION>x)"
    [|
      meta_cond_name;
      7;
      3;
      0x61;
      meta_alt;
      0x62;
      meta_ket;
      meta_capture lor 1;
      0x78;
      meta_ket;
      meta_end;
    |];
  (* Assertion conditions: META_COND_ASSERT then the re-scanned assertion
     (the expect_cond_assert protocol, pcre2_compile.c:4589-4603 and
     3119-3163). *)
  expect "(?(?=x)a|b)"
    [|
      meta_cond_assert;
      meta_lookahead;
      0x78;
      meta_ket;
      0x61;
      meta_alt;
      0x62;
      meta_ket;
      meta_end;
    |];
  expect_err "(?(?z)a)" Errors.err28 2;
  expect_err "(?(?<ab))" Errors.err28 2;

  (* Recursion/subroutine calls: (?R) (?n) (?+n) (?-n) (?&name) (?P>name)
     \g<n> \g'n' \g<name> \g'name' (pcre2_compile.c:4390-4446,
     3348-3402). *)
  expect "(?R)" [| meta_recurse; 3; meta_end |];
  expect "a(?1)b" [| 0x61; meta_recurse lor 1; 4; 0x62; meta_end |];
  expect "(?+1)(x)"
    [| meta_recurse lor 1; 4; meta_capture lor 1; 0x78; meta_ket; meta_end |];
  expect "(x)(?-1)"
    [| meta_capture lor 1; 0x78; meta_ket; meta_recurse lor 1; 7; meta_end |];
  expect "(?0)" [| meta_recurse; 3; meta_end |];
  expect_err "(?R" Errors.err58 3;
  expect_err "(?R8)" Errors.err58 3;
  expect_err "(?+)" Errors.err29 2;
  expect_err "(?-1)(x)" Errors.err15 4;
  expect "(?&n)(?<n>x)"
    [|
      meta_recurse_byname; 1; 3; meta_capture lor 1; 0x78; meta_ket; meta_end;
    |];
  expect "(?P>n)" [| meta_recurse_byname; 1; 4; meta_end |];
  expect "\\g<n>" [| meta_recurse_byname; 1; 3; meta_end |];
  expect "\\g'n'" [| meta_recurse_byname; 1; 3; meta_end |];
  expect "\\g<1>" [| meta_recurse lor 1; 4; meta_end |];
  expect "\\g'1'" [| meta_recurse lor 1; 4; meta_end |];
  expect_err "\\g<-1>(x)" Errors.err15 2;
  (* Recursions are quantifiable (okquantifier = TRUE). *)
  expect "(?R)?" [| meta_recurse; 3; meta_query; meta_end |];

  (* ( *VERB) and ( *VERB:NAME) (pcre2_compile.c:596-631, 2941-3039,
     4064-4152). *)
  expect "(*FAIL)" [| meta_fail; meta_end |];
  expect "(*F)" [| meta_fail; meta_end |];
  expect "a(*COMMIT)b" [| 0x61; meta_commit; 0x62; meta_end |];
  expect "(*SKIP)a" [| meta_skip; 0x61; meta_end |];
  expect "(*THEN)a|b" [| meta_then; 0x61; meta_alt; 0x62; meta_end |];
  expect "a(*ACCEPT)b" [| 0x61; meta_accept; 0x62; meta_end |];
  (* MARK and the arg-taking forms: META word, length word, name chars. *)
  expect "(*MARK:x)a" [| meta_mark; 1; 0x78; 0x61; meta_end |];
  expect "(*:ab)" [| meta_mark; 2; 0x61; 0x62; meta_end |];
  expect "(*PRUNE:n)a" [| meta_prune_arg; 1; 0x6e; 0x61; meta_end |];
  expect "(*SKIP:n)a" [| meta_skip_arg; 1; 0x6e; 0x61; meta_end |];
  expect "(*THEN:n)a" [| meta_then_arg; 1; 0x6e; 0x61; meta_end |];
  expect "(*COMMIT:n)a" [| meta_commit_arg; 1; 0x6e; 0x61; meta_end |];
  (* An empty argument is treated as no argument
     (pcre2_compile.c:4094-4098). *)
  expect "(*COMMIT:)a" [| meta_commit; 0x61; meta_end |];
  (* ( *ACCEPT:x) converts the argument to a preceding ( *MARK)
     (pcre2_compile.c:4125-4131, 2992-3000). *)
  expect "(*ACCEPT:x)y" [| meta_mark; 1; 0x78; meta_accept; 0x79; meta_end |];
  (* Quantified ( *ACCEPT) is wrapped in non-capturing brackets, allowing
     for a preceding ( *MARK) (pcre2_compile.c:3464-3477). *)
  expect "a(*ACCEPT)?"
    [| 0x61; meta_nocapture; meta_accept; meta_ket; meta_query; meta_end |];
  expect "(*ACCEPT:x)+"
    [|
      meta_nocapture;
      meta_mark;
      1;
      0x78;
      meta_accept;
      meta_ket;
      meta_plus;
      meta_end;
    |];
  (* Verb-name escape rules: backslash is literal unless
     PCRE2_ALT_VERBNAMES is set; then only \Q\E and escaped data
     characters are allowed (pcre2_compile.c:3003-3036). *)
  (* Without ALT_VERBNAMES the backslash is a literal name character: the
     name is "a\d", three characters. *)
  expect "(*MARK:a\\d)b" [| meta_mark; 3; 0x61; 0x5c; 0x64; 0x62; meta_end |];
  expect ~options:Options.alt_verbnames "(*MARK:a\\n)b"
    [| meta_mark; 2; 0x61; 0x0a; 0x62; meta_end |];
  expect ~options:Options.alt_verbnames "(*MARK:a\\Qb)c\\Ed)e"
    [| meta_mark; 5; 0x61; 0x62; 0x29; 0x63; 0x64; 0x65; meta_end |];
  expect_err ~options:Options.alt_verbnames "(*MARK:a\\d)" Errors.err40 10;
  (* Error sites: ERR66 (mandatory argument), ERR60 (malformed /
     unrecognized), ERR76 (name too long, > MAX_MARK code units). *)
  expect_err "(*MARK)" Errors.err66 6;
  expect_err "(*MARK:)" Errors.err66 7;
  expect_err "(*JUNK)" Errors.err60 6;
  expect_err "(*MARK:ab" Errors.err60 9;
  expect_err ("(*MARK:" ^ String.make 256 'a' ^ ")") Errors.err76 263;

  (* Deferred arms fail loudly with the placeholder code (never a real
     PCRE2 error number). *)
  assert (err_deferred > 201);

  (* Script runs (pcre2_compile.c:4030-4055 + 4960-4963): META_SCRIPT_RUN
     ... META_KET; the atomic form inserts META_ATOMIC and closes with two
     META_KETs (NSF_ATOMICSR). *)
  expect "(*script_run:a)" [| meta_script_run; 0x61; meta_ket; meta_end |];
  expect "(*sr:a)" [| meta_script_run; 0x61; meta_ket; meta_end |];
  expect "(*atomic_script_run:a)"
    [| meta_script_run; meta_atomic; 0x61; meta_ket; meta_ket; meta_end |];
  expect "(*asr:a|b)"
    [|
      meta_script_run;
      meta_atomic;
      0x61;
      meta_alt;
      0x62;
      meta_ket;
      meta_ket;
      meta_end;
    |];
  expect_err "(*sr:a" Errors.err14 6 (* missing closing parenthesis *);

  (* Freestanding \p and \P (pcre2_compile.c:3327-3346): META_ESCAPE +
     ESC_p/ESC_P followed by (ptype << 16) | pdata; {^...} flips the
     escape (3337). Error codes and offsets pinned against pcre2test
     10.44. *)
  expect "\\p{L}"
    [| meta_escape + esc_p; (Opcodes.pt_gc lsl 16) lor Ucp.ucp_l; meta_end |];
  expect "\\pL"
    [| meta_escape + esc_p; (Opcodes.pt_gc lsl 16) lor Ucp.ucp_l; meta_end |];
  expect "\\PL"
    [|
      meta_escape + esc_big_p; (Opcodes.pt_gc lsl 16) lor Ucp.ucp_l; meta_end;
    |];
  expect "\\p{^L}"
    [|
      meta_escape + esc_big_p; (Opcodes.pt_gc lsl 16) lor Ucp.ucp_l; meta_end;
    |];
  expect "\\P{^L}"
    [| meta_escape + esc_p; (Opcodes.pt_gc lsl 16) lor Ucp.ucp_l; meta_end |];
  expect "\\p{Any}" [| meta_escape + esc_p; Opcodes.pt_any lsl 16; meta_end |];
  (* A quantifier is allowed after \p (okquantifier, 3340). *)
  expect "\\p{Nd}+"
    [|
      meta_escape + esc_p;
      (Opcodes.pt_pc lsl 16) lor Ucp.ucp_nd;
      meta_plus;
      meta_end;
    |];
  (* Errors: ERR46 malformed, ERR47 unknown property; offsets after the
     failing escape (oracle: 146 at 2 for /\p/, 146 at 3 for /\p1/, 147 at
     6 for /\p{Zz}/, 147 at 4 for /\p{}/, 147 at 9 for /\p{sc:Zz}/, 147 at
     11 for /\p{foo=bar}/). *)
  expect_err "\\p" Errors.err46 2;
  expect_err "\\p1" Errors.err46 3;
  expect_err "\\p{Zz}" Errors.err47 6;
  expect_err "\\p{}" Errors.err47 4;
  expect_err "\\p{sc:Zz}" Errors.err47 9;
  expect_err "\\p{foo=bar}" Errors.err47 11;
  (* The freestanding arm recovers under BAD_ESCAPE_IS_LITERAL
     (ESCAPE_FAILED; oracle matches "p{Zz}" literally). *)
  expect ~extra:Options.extra_bad_escape_is_literal "\\p{Zz}"
    [| 0x70; 0x7b; 0x5a; 0x7a; 0x7d; meta_end |]

(* get_ucp (pcre2_compile.c:2145-2326) over the utt table: loose matching
   (case folding and _ - space stripping), the sc=/scx: forms, bidi
   classes, boolean properties, and the error paths. Expected PT_*/ucp_*
   pairs pinned against the 10.44 tables (ucptables.ml). *)
let test_11 () =
  let probe s =
    let cx = make_context s in
    let ptr = ref 0 in
    let neg = ref false in
    let ptype = ref (-1) in
    let pdata = ref (-1) in
    let ok = get_ucp cx ptr neg ptype pdata in
    (ok, !neg, !ptype, !pdata, cx.errorcode, !ptr)
  in
  let expect_ok s neg ptype pdata =
    match probe s with
    | true, n, t, d, err, _ ->
        assert (Bool.equal n neg);
        assert (Int.equal t ptype);
        assert (Int.equal d pdata);
        assert (Int.equal err 0)
    | false, _, _, _, _, _ -> assert false
  in
  let expect_fail s err endptr =
    match probe s with
    | false, _, _, _, e, p ->
        assert (Int.equal e err);
        assert (Int.equal p endptr)
    | true, _, _, _, _, _ -> assert false
  in
  (* General categories, particular categories, L&. *)
  expect_ok "{L}" false Opcodes.pt_gc Ucp.ucp_l;
  expect_ok "N" false Opcodes.pt_gc Ucp.ucp_n;
  expect_ok "{Nd}" false Opcodes.pt_pc Ucp.ucp_nd;
  expect_ok "{Lu}" false Opcodes.pt_pc Ucp.ucp_lu;
  expect_ok "{L&}" false Opcodes.pt_lamp 0;
  expect_ok "{Lc}" false Opcodes.pt_lamp 0;
  (* Scripts: bare names are PT_SCX for scripts with extensions, PT_SC
     otherwise; loose matching folds case and strips _ - and space. *)
  expect_ok "{Greek}" false Opcodes.pt_scx Ucp.ucp_greek;
  expect_ok "{greek}" false Opcodes.pt_scx Ucp.ucp_greek;
  expect_ok "{GREEK}" false Opcodes.pt_scx Ucp.ucp_greek;
  expect_ok "{ G-r_e e k }" false Opcodes.pt_scx Ucp.ucp_greek;
  expect_ok "{Deseret}" false Opcodes.pt_sc Ucp.ucp_deseret;
  (* sc= forces script-only; scx=/script_extensions= forces extensions. *)
  expect_ok "{sc=Greek}" false Opcodes.pt_sc Ucp.ucp_greek;
  expect_ok "{script:Greek}" false Opcodes.pt_sc Ucp.ucp_greek;
  expect_ok "{scx:Greek}" false Opcodes.pt_scx Ucp.ucp_greek;
  expect_ok "{Script_Extensions=Han}" false Opcodes.pt_scx Ucp.ucp_han;
  (* Bidi classes ("bidi" + value) and boolean properties. *)
  expect_ok "{bc=AN}" false Opcodes.pt_bidicl Ucp.ucp_bidi_an;
  expect_ok "{Bidi_Class:AL}" false Opcodes.pt_bidicl Ucp.ucp_bidi_al;
  expect_ok "{Cased}" false Opcodes.pt_bool Ucp.ucp_cased;
  expect_ok "{White_Space}" false Opcodes.pt_bool Ucp.ucp_white_space;
  (* The Perl-extension pseudo-properties and Any. *)
  expect_ok "{Xan}" false Opcodes.pt_alnum 0;
  expect_ok "{Xwd}" false Opcodes.pt_word 0;
  expect_ok "{Xsp}" false Opcodes.pt_space 0;
  expect_ok "{Xps}" false Opcodes.pt_pxspace 0;
  expect_ok "{Xuc}" false Opcodes.pt_ucnc 0;
  expect_ok "{Any}" false Opcodes.pt_any 0;
  (* ^ negation inside the braces. *)
  expect_ok "{^L}" true Opcodes.pt_gc Ucp.ucp_l;
  expect_ok "{^Greek}" true Opcodes.pt_scx Ucp.ucp_greek;
  (* ERR46 (malformed): end of pattern, non-letter, unterminated name, NUL
     in the name; ptr points past the consumed characters. *)
  expect_fail "" Errors.err46 0;
  expect_fail "1" Errors.err46 1;
  expect_fail "{L" Errors.err46 2;
  expect_fail "{L\000}" Errors.err46 3;
  expect_fail ("{" ^ String.make 60 'a' ^ "}") Errors.err46 50;
  (* ERR47 (unknown property): after the binary chop, or an unknown
     xx in \p{xx:yy}; ptr is already past the closing brace. *)
  expect_fail "{Zz}" Errors.err47 4;
  expect_fail "{}" Errors.err47 2;
  expect_fail "{sc=Zz}" Errors.err47 7;
  expect_fail "{foo=bar}" Errors.err47 9;
  (* A script name that is a non-script property is diagnosed under sc=
     (the switch's break at pcre2_compile.c:2313). *)
  expect_fail "{sc=Cased}" Errors.err47 10

(* is_newline_at over the non-fixed newline types (PRIV(is_newline),
   pcre2_newline.c:78-145, non-UTF 8-bit arm): NLTYPE_ANY matches LF, VT,
   FF, CR (length 2 before LF, else 1) and NEL 0x85; NLTYPE_ANYCRLF
   matches only CR, LF, CRLF — in particular NOT NEL. *)
let test_12 () =
  let probe nltype pat p =
    let cx = make_context pat in
    cx.nltype <- nltype;
    let hit = is_newline_at cx p in
    (hit, cx.nllen)
  in
  (* ANY: NEL (0x85) is a newline of length 1. *)
  (match probe nltype_any "a\x85b" 1 with true, 1 -> () | _ -> assert false);
  (* ANYCRLF: NEL is NOT a newline. *)
  (match probe nltype_anycrlf "a\x85b" 1 with
  | false, _ -> ()
  | _ -> assert false);
  (* ANY: VT / FF length 1; CRLF length 2; lone CR at end length 1. *)
  (match probe nltype_any "\x0b" 0 with true, 1 -> () | _ -> assert false);
  (match probe nltype_any "\x0c" 0 with true, 1 -> () | _ -> assert false);
  (match probe nltype_any "\r\na" 0 with true, 2 -> () | _ -> assert false);
  (match probe nltype_any "a\r" 1 with true, 1 -> () | _ -> assert false);
  (* ANYCRLF: LF 1, CRLF 2; VT is not a newline. *)
  (match probe nltype_anycrlf "\n" 0 with true, 1 -> () | _ -> assert false);
  (match probe nltype_anycrlf "\r\n" 0 with true, 2 -> () | _ -> assert false);
  (match probe nltype_anycrlf "\x0b" 0 with
  | false, _ -> ()
  | _ -> assert false);
  (* p at/after ptrend: the IS_NEWLINE macro's (p) < PSEND guard. *)
  match probe nltype_any "a" 1 with false, _ -> () | _ -> assert false

let tests =
  [
    Alcotest.test_case "parse 0" `Quick test_0;
    Alcotest.test_case "parse 1" `Quick test_1;
    Alcotest.test_case "parse 2" `Quick test_2;
    Alcotest.test_case "parse 3" `Quick test_3;
    Alcotest.test_case "parse 4" `Quick test_4;
    Alcotest.test_case "parse 5" `Quick test_5;
    Alcotest.test_case "parse 6" `Quick test_6;
    Alcotest.test_case "parse 7" `Quick test_7;
    Alcotest.test_case "parse 8" `Quick test_8;
    Alcotest.test_case "parse 9" `Quick test_9;
    Alcotest.test_case "parse 10" `Quick test_10;
    Alcotest.test_case "parse 11" `Quick test_11;
    Alcotest.test_case "parse 12" `Quick test_12;
  ]
