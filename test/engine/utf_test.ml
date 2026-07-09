(* Engine module-initialization asserts for [Pcre2_engine.Utf], migrated
   verbatim from src/engine/utf.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Utf

(* ord2utf encode / getchar decode roundtrips at the encoding-length
   boundary code points (utf8_table1 breakpoints 0x7f/0x7ff/0xffff and
   their successors, plus the top code point 0x10ffff), pinned against the
   UTF-8 definition (RFC 3629) and pcre2_ord2utf.c:86-95. Surrogates have
   no roundtrip contract here: ord2utf encodes any value mechanically and
   rejection is the callers' job (Valid_utf for subjects/patterns,
   check_escape ERR91 for escapes). *)
let test_0 () =
  let buf = Bytes.make 8 '\000' in
  let roundtrip c =
    let len = ord2utf c buf 0 in
    let s = Bytes.sub_string buf 0 len in
    assert (Int.equal (getchar s 0) c);
    (* GETCHARINC decodes the same bytes and lands just past them. *)
    let pos = ref 0 in
    assert (Int.equal (getcharinc s pos) c);
    assert (Int.equal !pos len);
    (* GETCHARLEN adds the extra length. *)
    let l = ref 1 in
    assert (Int.equal (getcharlen s 0 l) c);
    assert (Int.equal !l len);
    len
  in
  assert (Int.equal (roundtrip 0x00) 1);
  assert (Int.equal (roundtrip 0x7f) 1);
  assert (Int.equal (roundtrip 0x80) 2);
  assert (Int.equal (roundtrip 0x7ff) 2);
  assert (Int.equal (roundtrip 0x800) 3);
  assert (Int.equal (roundtrip 0xffff) 3);
  assert (Int.equal (roundtrip 0x10000) 4);
  assert (Int.equal (roundtrip 0x10ffff) 4);
  (* Exact bytes for known encodings: U+00E9 = C3 A9, U+20AC = E2 82 AC,
     U+10348 = F0 90 8D 88. *)
  assert (Int.equal (ord2utf 0xe9 buf 0) 2);
  assert (Char.equal (Bytes.get buf 0) '\xc3');
  assert (Char.equal (Bytes.get buf 1) '\xa9');
  assert (Int.equal (ord2utf 0x20ac buf 0) 3);
  assert (Char.equal (Bytes.get buf 0) '\xe2');
  assert (Char.equal (Bytes.get buf 1) '\x82');
  assert (Char.equal (Bytes.get buf 2) '\xac');
  assert (Int.equal (ord2utf 0x10348 buf 0) 4);
  assert (Char.equal (Bytes.get buf 0) '\xf0');
  assert (Char.equal (Bytes.get buf 1) '\x90');
  assert (Char.equal (Bytes.get buf 2) '\x8d');
  assert (Char.equal (Bytes.get buf 3) '\x88');
  (* ord2utf writes at the given position, not position 0. *)
  Bytes.set buf 0 'x';
  assert (Int.equal (ord2utf 0xe9 buf 1) 2);
  assert (Char.equal (Bytes.get buf 0) 'x');
  assert (Char.equal (Bytes.get buf 1) '\xc3');
  assert (Char.equal (Bytes.get buf 2) '\xa9')

(* The test variants dispatch on utf; the step/scan helpers move by whole
   characters. *)
let test_1 () =
  let s = "a\xc3\xa9\xe2\x82\xacz" in
  (* GETCHARINCTEST: utf=false reads single code units; utf=true
     decodes. *)
  let pos = ref 1 in
  assert (Int.equal (getcharinctest ~utf:false s pos) 0xc3);
  assert (Int.equal !pos 2);
  let pos = ref 1 in
  assert (Int.equal (getcharinctest ~utf:true s pos) 0xe9);
  assert (Int.equal !pos 3);
  assert (Int.equal (getcharinctest ~utf:true s pos) 0x20ac);
  assert (Int.equal !pos 6);
  assert (Int.equal (getcharinctest ~utf:true s pos) (Char.code 'z'));
  assert (Int.equal !pos 7);
  (* GETCHARLENTEST mirrors GETCHARLEN under utf and the plain read
     otherwise. *)
  let l = ref 1 in
  assert (Int.equal (getcharlentest ~utf:true s 3 l) 0x20ac);
  assert (Int.equal !l 3);
  let l = ref 1 in
  assert (Int.equal (getcharlentest ~utf:false s 3 l) 0xe2);
  assert (Int.equal !l 1);
  (* BACKCHAR from mid-character positions; FORWARDCHARTEST bounded by the
     end. *)
  assert (Int.equal (backchar s 5) 3);
  assert (Int.equal (backchar s 2) 1);
  assert (Int.equal (backchar s 6) 6);
  assert (Int.equal (forwardchartest s 4 7) 6);
  assert (Int.equal (forwardchartest s 6 7) 6);
  assert (Int.equal (forwardchartest s 4 5) 5);
  (* HAS_EXTRALEN / GET_EXTRALEN / NOT_FIRSTCU over the lead/continuation
     byte split. *)
  assert (not (has_extralen 0x7f));
  assert (not (has_extralen 0xbf));
  assert (has_extralen 0xc0);
  assert (Int.equal (get_extralen 0xc3) 1);
  assert (Int.equal (get_extralen 0xe2) 2);
  assert (Int.equal (get_extralen 0xf0) 3);
  assert (Int.equal (get_extralen 0xf8) 4);
  assert (Int.equal (get_extralen 0xfc) 5);
  assert (not_firstcu 0x80);
  assert (not_firstcu 0xbf);
  assert (not (not_firstcu 0x7f));
  assert (not (not_firstcu 0xc0));
  (* PUTCHAR: single unit when not utf or c <= 127, ord2utf otherwise. *)
  let buf = Bytes.make 8 '\000' in
  assert (Int.equal (putchar ~utf:false 0x1e9 buf 0) 1);
  assert (Char.equal (Bytes.get buf 0) '\xe9' (* truncated like the C *));
  assert (Int.equal (putchar ~utf:true 0x61 buf 0) 1);
  assert (Char.equal (Bytes.get buf 0) 'a');
  assert (Int.equal (putchar ~utf:true 0xe9 buf 0) 2);
  assert (Char.equal (Bytes.get buf 0) '\xc3');
  assert (Char.equal (Bytes.get buf 1) '\xa9');
  (* Truncated sequences at the end of the string (the NO_UTF_CHECK case):
     [peek]'s clamped reads supply 0 for the missing continuation bytes —
     the value the C reads from a zero-terminated pattern's NUL (see the
     DEVIATION note at [peek]); the position still advances by the lead
     byte's full GET_EXTRALEN. *)
  assert (Int.equal (getchar "\xc3" 0) 0xc0);
  assert (Int.equal (getchar "a\xe2\x82" 1) 0x2080);
  (let pos = ref 0 in
   assert (Int.equal (getcharinc "\xc3" pos) 0xc0);
   assert (Int.equal !pos 2));
  (let pos = ref 0 in
   assert (Int.equal (getcharinctest ~utf:true "\xf8\x88" pos) 0x200000);
   assert (Int.equal !pos 5));
  let l = ref 1 in
  assert (Int.equal (getcharlen "\xc3" 0 l) 0xc0);
  assert (Int.equal !l 2);
  (* getutf8_bytes decodes the same encodings from a Bytes program. *)
  let buf = Bytes.make 8 '\000' in
  List.iter
    (fun c ->
      let len = ord2utf c buf 0 in
      assert (len >= 2 (* every case here is multi-byte: lead >= 0xc0 *));
      assert (Int.equal (getutf8_bytes (Char.code (Bytes.get buf 0)) buf 0) c))
    [ 0x80; 0xe9; 0x7ff; 0x800; 0x20ac; 0xffff; 0x10000; 0x10ffff; 0x200000 ]

let tests =
  [
    Alcotest.test_case "utf 0" `Quick test_0;
    Alcotest.test_case "utf 1" `Quick test_1;
  ]
