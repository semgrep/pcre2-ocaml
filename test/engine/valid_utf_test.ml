(* Engine module-initialization asserts for [Pcre2_engine.Valid_utf], migrated
   verbatim from src/engine/valid_utf.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Valid_utf

(* Every UTF8_ERRn code with its exact reported offset, pinned against
   pcre2test 10.44 (pcre2test -error -3,...,-23 message meanings and the
   testoutput10 error-section shapes) and hand-checked against
   pcre2_valid_utf.c:134-315. Each probe validates the whole string from
   0; the "XX" prefixes shift the reported offset. *)
let test_0 () =
  let check str expect_rc expect_off =
    let off = ref (-1) in
    let rc = valid_utf str ~start:0 ~length:(String.length str) off in
    assert (Int.equal rc expect_rc);
    assert (Int.equal !off expect_off)
  in
  let ok str =
    let off = ref (-1) in
    let rc = valid_utf str ~start:0 ~length:(String.length str) off in
    assert (Int.equal rc 0);
    assert (Int.equal !off (-1) (* untouched on success *))
  in
  (* Valid strings: ASCII, 2/3/4-byte characters, boundary code points. *)
  ok "";
  ok "abc";
  ok "a\xc3\xa9z" (* U+00E9 *);
  ok "\xc2\x80" (* U+0080 *);
  ok "\xdf\xbf" (* U+07FF *);
  ok "\xe0\xa0\x80" (* U+0800 *);
  ok "\xed\x9f\xbf" (* U+D7FF: last before the surrogates *);
  ok "\xee\x80\x80" (* U+E000: first after the surrogates *);
  ok "\xef\xbf\xbf" (* U+FFFF *);
  ok "\xf0\x90\x80\x80" (* U+10000 *);
  ok "\xf4\x8f\xbf\xbf" (* U+10FFFF *);
  (* ERR1-ERR5: missing bytes at the end (pcre2_valid_utf.c:156-167);
     offset = position of the truncated character's first byte. *)
  check "X\xdf" Errors.error_utf8_err1 1;
  check "XX\xef" Errors.error_utf8_err2 2;
  check "XXX\xef\x80" Errors.error_utf8_err1 3;
  check "X\xf7" Errors.error_utf8_err3 1;
  check "\xf7\x80" Errors.error_utf8_err2 0;
  check "\xf7\x80\x80" Errors.error_utf8_err1 0;
  check "\xfb" Errors.error_utf8_err4 0;
  check "\xfb\x80\x80\x80" Errors.error_utf8_err1 0;
  check "\xfd" Errors.error_utf8_err5 0;
  check "\xfd\x80\x80\x80\x80" Errors.error_utf8_err1 0;
  (* ERR6-ERR10: continuation-byte top bits not 0x80
     (pcre2_valid_utf.c:172-176, 199-203, 221-230, 252-266, 278-297);
     offset = the character's first byte. *)
  check "X\xdf\x7f" Errors.error_utf8_err6 1;
  check "\xef\x7f\x80" Errors.error_utf8_err6 0;
  check "X\xef\x80\x7f" Errors.error_utf8_err7 1;
  check "\xf7\x80\x7f\x80" Errors.error_utf8_err7 0;
  check "X\xf7\x80\x80\x7f" Errors.error_utf8_err8 1;
  check "\xfb\x80\x80\x7f\x80" Errors.error_utf8_err8 0;
  check "X\xfb\x80\x80\x80\x7f" Errors.error_utf8_err9 1;
  check "\xfd\x80\x80\x80\x7f\x80" Errors.error_utf8_err9 0;
  check "X\xfd\x80\x80\x80\x80\x7f" Errors.error_utf8_err10 1;
  (* ERR11/ERR12: well-formed 5-byte and 6-byte characters, rejected by
     RFC 3629 (pcre2_valid_utf.c:310-314). *)
  check "X\xfb\x80\x80\x80\x80" Errors.error_utf8_err11 1;
  check "XX\xfd\x80\x80\x80\x80\x80" Errors.error_utf8_err12 2;
  (* ERR13: 4-byte character above 0x10ffff (pcre2_valid_utf.c:236-240):
     f4 90 and f5 both exceed it. *)
  check "X\xf4\x90\x80\x80" Errors.error_utf8_err13 1;
  check "\xf5\x80\x80\x80" Errors.error_utf8_err13 0;
  (* ERR14: 3-byte character in the surrogate range 0xd800-0xdfff
     (pcre2_valid_utf.c:209-213): ed a0 80 = U+D800, ed bf bf =
     U+DFFF. *)
  check "X\xed\xa0\x80" Errors.error_utf8_err14 1;
  check "\xed\xbf\xbf" Errors.error_utf8_err14 0;
  (* ERR15-ERR19: overlong sequences (pcre2_valid_utf.c:187-191, 204-208,
     231-235, 267-271, 298-302). *)
  check "X\xc0\x80" Errors.error_utf8_err15 1;
  check "\xc1\xbf" Errors.error_utf8_err15 0;
  check "X\xe0\x9f\x80" Errors.error_utf8_err16 1;
  check "X\xf0\x8f\x80\x80" Errors.error_utf8_err17 1;
  check "X\xf8\x87\x80\x80\x80" Errors.error_utf8_err18 1;
  check "X\xfc\x83\x80\x80\x80\x80" Errors.error_utf8_err19 1;
  (* ERR20: isolated continuation byte (pcre2_valid_utf.c:143-147). *)
  check "XX\x80" Errors.error_utf8_err20 2;
  check "\xbfz" Errors.error_utf8_err20 0;
  (* ERR21: illegal 0xfe / 0xff (pcre2_valid_utf.c:149-153). *)
  check "X\xfe" Errors.error_utf8_err21 1;
  check "\xff\x80" Errors.error_utf8_err21 0;
  (* The testinput10:8-11 pattern shapes (oracle: testoutput10:8-19). *)
  check "[\xc3(]" Errors.error_utf8_err6 1;
  check "\xc3" Errors.error_utf8_err1 0;
  check "\xc3(xxx" Errors.error_utf8_err6 0;
  check "\xc3\x82\x80\x80\x80\x80\x80\x80\x80\x80" Errors.error_utf8_err20 2;
  (* [start]/[length] window semantics: offsets are relative to start, and
     bytes outside the window are ignored (the match driver validates from
     mb->check_subject, pcre2_match.c:6912-6928). *)
  let off = ref (-1) in
  assert (
    Int.equal
      (valid_utf "\xff\xc3\xa9\xdf" ~start:1 ~length:3 off)
      Errors.error_utf8_err1);
  assert (Int.equal !off 2);
  assert (Int.equal (valid_utf "\xff\xc3\xa9\xdf" ~start:1 ~length:2 off) 0)

let tests = [ Alcotest.test_case "valid_utf 0" `Quick test_0 ]
