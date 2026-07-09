(* UTF string validation for the pure-OCaml PCRE2 10.44 port (8-bit
   library).

   Ported from vendor/pcre2/src/pcre2_valid_utf.c:93-317 (the
   PCRE2_CODE_UNIT_WIDTH == 8 arm of PRIV(valid_utf); the UTF-16/UTF-32
   arms, 319-394, do not apply). This function is called (optionally) at
   the start of compile or match, to check that a supposed UTF string is
   actually valid. The early check means that subsequent code can assume
   it is dealing with a valid string. The check can be turned off for
   maximum performance (PCRE2_NO_UTF_CHECK), but the consequences of
   supplying an invalid string are then undefined.

   Pointer mapping: the C receives a PCRE2_SPTR [string] that may point
   into the middle of a subject (the match driver passes
   mb->check_subject); here that is the (s, start) pair. As in the C,
   [erroroffset] is set relative to that start (p - string).

   Returns 0 if the string is a valid UTF string, and one of the negative
   Errors.error_utf8_err1..21 codes otherwise, setting the offset of the
   bad character through [erroroffset] (untouched on success, like the
   C's out-parameter). *)

(* The C's mid-loop `return PCRE2_ERROR_UTF8_ERRn` exits: a forward goto
   to the function exit with erroroffset already stored (port-conventions
   §2). Raised and caught within valid_utf only; compile/driver-entry code,
   never inside the interpreter frame loop (§6). *)
exception Bad_utf of int

(* pcre2_valid_utf.c:93-317 — PRIV(valid_utf), 8-bit. The RFC 3629
   restrictions apply: values are limited to 0x0..0x10ffff, at most
   4 bytes long, excluding the surrogate range 0xd800-0xdfff; the format
   of 5-byte and 6-byte characters is still checked. Error returns are
   listed at pcre2_valid_utf.c:109-131 (= Errors.error_utf8_err1..21). *)
let valid_utf (s : string) ~(start : int) ~(length : int)
    (erroroffset : int ref) : int =
  try
    (* for (p = string; length > 0; p++) — [p] and [length] both also
       move inside the body, so the C for-loop becomes a while with the
       p++ at the bottom (the C's continue for ASCII characters also runs
       the p++). *)
    let p = ref start in
    let length = ref length in
    while !length > 0 do
      let c = Char.code s.[!p] in
      decr length;
      (if c < 128 then () (* ASCII character (pcre2_valid_utf.c:141) *)
       else if c < 0xc0 then (
         (* pcre2_valid_utf.c:143-147 — isolated 10xx xxxx byte. *)
         erroroffset := !p - start;
         raise_notrace (Bad_utf Errors.error_utf8_err20))
       else if c >= 0xfe then (
         (* pcre2_valid_utf.c:149-153 — invalid 0xfe or 0xff bytes. *)
         erroroffset := !p - start;
         raise_notrace (Bad_utf Errors.error_utf8_err21))
       else
         (* pcre2_valid_utf.c:155-167 — number of additional bytes (1-5),
            then check for missing bytes at the end of the string. *)
         let ab = Tables.utf8_table4.(c land 0x3f) in
         if !length < ab then (
           erroroffset := !p - start;
           raise_notrace
             (Bad_utf
                (match ab - !length with
                | 1 -> Errors.error_utf8_err1
                | 2 -> Errors.error_utf8_err2
                | 3 -> Errors.error_utf8_err3
                | 4 -> Errors.error_utf8_err4
                | _ -> Errors.error_utf8_err5 (* 5: ab <= 5, length >= 0 *))));
         length := !length - ab (* Length remaining *);

         (* pcre2_valid_utf.c:170-176 — check top bits in the second
            byte. *)
         incr p;
         let d = Char.code s.[!p] in
         if not (Int.equal (d land 0xc0) 0x80) then (
           erroroffset := !p - start - 1;
           raise_notrace (Bad_utf Errors.error_utf8_err6));

         (* pcre2_valid_utf.c:178-304 — for each length, check that the
            remaining bytes start with the 0x80 bit set and not the 0x40
            bit. Then check for an overlong sequence, and for the excluded
            range 0xd800 to 0xdfff. *)
         (match ab with
         | 1 ->
             (* pcre2_valid_utf.c:184-192 — 2-byte character. No further
                bytes to check for 0x80. Check first byte for xx00 000x
                (overlong sequence). *)
             if Int.equal (c land 0x3e) 0 then (
               erroroffset := !p - start - 1;
               raise_notrace (Bad_utf Errors.error_utf8_err15))
         | 2 ->
             (* pcre2_valid_utf.c:194-214 — 3-byte character. Check third
                byte for 0x80. Then check first 2 bytes for
                1110 0000, xx0x xxxx (overlong sequence) or
                1110 1101, 1010 xxxx (0xd800 - 0xdfff). *)
             incr p;
             if not (Int.equal (Char.code s.[!p] land 0xc0) 0x80) then (
               (* Third byte *)
               erroroffset := !p - start - 2;
               raise_notrace (Bad_utf Errors.error_utf8_err7));
             if Int.equal c 0xe0 && Int.equal (d land 0x20) 0 then (
               erroroffset := !p - start - 2;
               raise_notrace (Bad_utf Errors.error_utf8_err16));
             if Int.equal c 0xed && d >= 0xa0 then (
               erroroffset := !p - start - 2;
               raise_notrace (Bad_utf Errors.error_utf8_err14))
         | 3 ->
             (* pcre2_valid_utf.c:216-241 — 4-byte character. Check 3rd and
                4th bytes for 0x80. Then check first 2 bytes for
                1111 0000, xx00 xxxx (overlong sequence), then check for a
                character greater than 0x0010ffff (f4 8f bf bf). *)
             incr p;
             if not (Int.equal (Char.code s.[!p] land 0xc0) 0x80) then (
               (* Third byte *)
               erroroffset := !p - start - 2;
               raise_notrace (Bad_utf Errors.error_utf8_err7));
             incr p;
             if not (Int.equal (Char.code s.[!p] land 0xc0) 0x80) then (
               (* Fourth byte *)
               erroroffset := !p - start - 3;
               raise_notrace (Bad_utf Errors.error_utf8_err8));
             if Int.equal c 0xf0 && Int.equal (d land 0x30) 0 then (
               erroroffset := !p - start - 3;
               raise_notrace (Bad_utf Errors.error_utf8_err17));
             if c > 0xf4 || (Int.equal c 0xf4 && d > 0x8f) then (
               erroroffset := !p - start - 3;
               raise_notrace (Bad_utf Errors.error_utf8_err13))
         | 4 ->
             (* pcre2_valid_utf.c:243-272 — 5-byte and 6-byte characters
                are not allowed by RFC 3629, and will be rejected by the
                length test below. However, we do the appropriate tests
                here so that overlong sequences get diagnosed. 5-byte
                character: check 3rd, 4th, and 5th bytes for 0x80. Then
                check for 1111 1000, xx00 0xxx. *)
             incr p;
             if not (Int.equal (Char.code s.[!p] land 0xc0) 0x80) then (
               (* Third byte *)
               erroroffset := !p - start - 2;
               raise_notrace (Bad_utf Errors.error_utf8_err7));
             incr p;
             if not (Int.equal (Char.code s.[!p] land 0xc0) 0x80) then (
               (* Fourth byte *)
               erroroffset := !p - start - 3;
               raise_notrace (Bad_utf Errors.error_utf8_err8));
             incr p;
             if not (Int.equal (Char.code s.[!p] land 0xc0) 0x80) then (
               (* Fifth byte *)
               erroroffset := !p - start - 4;
               raise_notrace (Bad_utf Errors.error_utf8_err9));
             if Int.equal c 0xf8 && Int.equal (d land 0x38) 0 then (
               erroroffset := !p - start - 4;
               raise_notrace (Bad_utf Errors.error_utf8_err18))
         | _ ->
             (* pcre2_valid_utf.c:274-303 — 6-byte character (ab = 5, the
                table's maximum): check 3rd-6th bytes for 0x80. Then check
                for 1111 1100, xx00 00xx. *)
             incr p;
             if not (Int.equal (Char.code s.[!p] land 0xc0) 0x80) then (
               (* Third byte *)
               erroroffset := !p - start - 2;
               raise_notrace (Bad_utf Errors.error_utf8_err7));
             incr p;
             if not (Int.equal (Char.code s.[!p] land 0xc0) 0x80) then (
               (* Fourth byte *)
               erroroffset := !p - start - 3;
               raise_notrace (Bad_utf Errors.error_utf8_err8));
             incr p;
             if not (Int.equal (Char.code s.[!p] land 0xc0) 0x80) then (
               (* Fifth byte *)
               erroroffset := !p - start - 4;
               raise_notrace (Bad_utf Errors.error_utf8_err9));
             incr p;
             if not (Int.equal (Char.code s.[!p] land 0xc0) 0x80) then (
               (* Sixth byte *)
               erroroffset := !p - start - 5;
               raise_notrace (Bad_utf Errors.error_utf8_err10));
             if Int.equal c 0xfc && Int.equal (d land 0x3c) 0 then (
               erroroffset := !p - start - 5;
               raise_notrace (Bad_utf Errors.error_utf8_err19)));

         (* pcre2_valid_utf.c:306-314 — character is valid under RFC 2279,
            but 4-byte and 5-byte characters (ab = 4, 5) are excluded by
            RFC 3629. The position p is currently at the last byte of the
            character. *)
         if ab > 3 then (
           erroroffset := !p - start - ab;
           raise_notrace
             (Bad_utf
                (if Int.equal ab 4 then Errors.error_utf8_err11
                 else Errors.error_utf8_err12))));
      incr p
    done;
    0
  with Bad_utf rc -> rc

(* ---------- Inline sanity checks (module-initialization asserts) ---------- *)

(* Every UTF8_ERRn code with its exact reported offset, pinned against
   pcre2test 10.44 (pcre2test -error -3,...,-23 message meanings and the
   testoutput10 error-section shapes) and hand-checked against
   pcre2_valid_utf.c:134-315. Each probe validates the whole string from
   0; the "XX" prefixes shift the reported offset. *)
let () =
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
