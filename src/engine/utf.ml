(* UTF-8 primitives for the pure-OCaml PCRE2 10.44 port (8-bit library).

   Ported from the character-handling macros of
   vendor/pcre2/src/pcre2_internal.h (GETUTF8/GETUTF8INC/GETUTF8LEN base
   macros, 264-371) and vendor/pcre2/src/pcre2_intmodedep.h (the 8-bit
   SUPPORT_UNICODE expansions, 275-358), plus PRIV(ord2utf) from
   vendor/pcre2/src/pcre2_ord2utf.c:80-97. The utf8_table1..4 tables live
   in Tables (pcre2_tables.c:100-118).

   Pointer mapping: a C PCRE2_SPTR into a subject/pattern becomes the pair
   of the OCaml string and an int position. Macros that advance the
   pointer (GETCHARINC, GETCHARINCTEST) take the position as an [int ref]
   and advance it, exactly where the C does; macros that do not advance
   (GETCHAR, GETCHARLEN, GETCHARLENTEST) take a plain int. BACKCHAR and
   FORWARDCHARTEST return the moved position.

   Bounds: the caller's contract puts the LEAD byte in bounds (the C
   macros dereference eptr the same way); on a valid UTF string (the
   normal case: Valid_utf.valid_utf has run) the continuation bytes are
   in bounds too. With PCRE2_NO_UTF_CHECK an invalid string can make the
   decoders want bytes past the end — see [peek] below for the defined
   behavior chosen there.

   ACROSSCHAR (pcre2_intmodedep.h:352-353) is a condition/action template
   with call-site-specific parts; it has no closure-free function form
   (port-conventions §8 forbids per-iteration closures in the hot loop),
   so its uses are transcribed at each call site as a loop over
   [not_firstcu], cited against pcre2_intmodedep.h:352-353. *)

(* DEVIATION (defined behavior where the C is undefined): continuation
   bytes are read through [peek], which returns 0 outside the string on
   EITHER side. PCRE2 documents NO_UTF_CHECK with an invalid string as
   undefined; in practice a truncated multi-byte sequence at the end of a
   pcre2test pattern makes the C read the trailing NUL of the
   zero-terminated pattern (compile.ml's [byte_at] in the pso scan is the
   established precedent for supplying that 0 byte), and anything past it
   is unowned memory. Returning 0 for every out-of-range read reproduces
   the C's NUL byte-for-byte at the first overrun position and keeps
   every further read defined — errors stay values, no exception can
   escape compile or match (port-conventions §5, §6). The negative side
   is reachable too: the word-boundary previous-character probe
   (pcre2_match.c:6269-6277) computes lastptr = Feptr - 1 with only a
   `Feptr == mb->check_subject` guard, so at Feptr = start_subject <
   check_subject the C reads subject[-1] — the BACKCHAR-before-subject
   UB family. Reading 0 there agrees with the whole family pin: the
   padded oracle's slack byte at subject[-1] is 0, Newline.was_newline's
   walk reads 0 at position -1, and the C-on-LE header-byte accident
   reads 0 as well. *)
let peek (s : string) (i : int) : int =
  if i >= 0 && i < String.length s then
    (* safe: 0 <= i and i < String.length s checked in the two conjuncts
       above *)
    Char.code (String.unsafe_get s i)
  else 0

(* pcre2_internal.h:225-227 — the largest code point: 0x10ffff. *)
let max_utf_code_point = 0x10ffff

(* pcre2_intmodedep.h:280-282 — the largest UTF code point that can be
   encoded as a single code unit. *)
let max_utf_single_cu = 127

(* pcre2_intmodedep.h:284-286 + pcre2_internal.h:270-272 — HAS_EXTRALEN =
   HASUTF8EXTRALEN: tests whether a UTF-8 code point needs extra bytes to
   decode. *)
let has_extralen (c : int) : bool = c >= 0xc0

(* pcre2_intmodedep.h:288-291 — GET_EXTRALEN: the additional number of
   code units (1-5) when HAS_EXTRALEN(c) is true. Undefined behaviour in
   the C otherwise; here it indexes the table all the same. *)
let get_extralen (c : int) : int = Tables.utf8_table4.(c land 0x3f)

(* pcre2_intmodedep.h:293-296 — NOT_FIRSTCU: true if the given value is
   not the first code unit of a UTF sequence. *)
let not_firstcu (c : int) : bool = Int.equal (c land 0xc0) 0x80

(* GETUTF8 (pcre2_internal.h:280-300) — base macro to pick up the
   remaining bytes of a UTF-8 character whose first byte is [c] at [pos],
   not advancing the position. Caller has established c >= 0xc0. The 5-
   and 6-byte arms are transcribed even though a valid UTF-8 character is
   at most 4 code units; all results fit an OCaml int, matching the C's
   uint32 arithmetic with no wraparound. Continuation bytes go through
   [peek] (see its DEVIATION note): in bounds for valid UTF, 0 past the
   end for NO_UTF_CHECK garbage. *)
let getutf8 (c : int) (s : string) (pos : int) : int =
  if Int.equal (c land 0x20) 0 then
    (* pcre2_internal.h:285-286 — two-byte character *)
    ((c land 0x1f) lsl 6) lor (peek s (pos + 1) land 0x3f)
  else if Int.equal (c land 0x10) 0 then
    (* pcre2_internal.h:287-288 — three-byte character *)
    ((c land 0x0f) lsl 12)
    lor ((peek s (pos + 1) land 0x3f) lsl 6)
    lor (peek s (pos + 2) land 0x3f)
  else if Int.equal (c land 0x08) 0 then
    (* pcre2_internal.h:289-291 — four-byte character *)
    ((c land 0x07) lsl 18)
    lor ((peek s (pos + 1) land 0x3f) lsl 12)
    lor ((peek s (pos + 2) land 0x3f) lsl 6)
    lor (peek s (pos + 3) land 0x3f)
  else if Int.equal (c land 0x04) 0 then
    (* pcre2_internal.h:292-295 — five-byte character (invalid UTF-8) *)
    ((c land 0x03) lsl 24)
    lor ((peek s (pos + 1) land 0x3f) lsl 18)
    lor ((peek s (pos + 2) land 0x3f) lsl 12)
    lor ((peek s (pos + 3) land 0x3f) lsl 6)
    lor (peek s (pos + 4) land 0x3f)
  else
    (* pcre2_internal.h:296-299 — six-byte character (invalid UTF-8) *)
    ((c land 0x01) lsl 30)
    lor ((peek s (pos + 1) land 0x3f) lsl 24)
    lor ((peek s (pos + 2) land 0x3f) lsl 18)
    lor ((peek s (pos + 3) land 0x3f) lsl 12)
    lor ((peek s (pos + 4) land 0x3f) lsl 6)
    lor (peek s (pos + 5) land 0x3f)

(* GETCHAR (pcre2_intmodedep.h:298-303) — get the next UTF-8 character,
   not advancing the position. Called when we know we are in UTF-8
   mode. *)
let getchar (s : string) (pos : int) : int =
  let c = Char.code s.[pos] in
  if c >= 0xc0 then getutf8 c s pos else c

(* GETUTF8 (pcre2_internal.h:280-300) over [Bytes.t] — the same macro read
   from the COMPILED PATTERN instead of a subject string (pcre2_match.c
   decodes pattern characters with the identical GETCHAR* macros, e.g.
   GETCHARLEN(fc, Fecode, Flength) at pcre2_match.c:1001). Pattern
   literals are complete ord2utf encodings emitted by the compiler
   (pcre2_ord2utf.c:86-95 always deposits every continuation byte), so the
   continuation reads use plain [Bytes.get] — in bounds in any complete
   compiled program. Callers add GET_EXTRALEN themselves where the C macro
   advances or accumulates a length. *)
let getutf8_bytes (c : int) (b : Bytes.t) (pos : int) : int =
  if Int.equal (c land 0x20) 0 then
    (* pcre2_internal.h:285-286 — two-byte character *)
    ((c land 0x1f) lsl 6) lor (Char.code (Bytes.get b (pos + 1)) land 0x3f)
  else if Int.equal (c land 0x10) 0 then
    (* pcre2_internal.h:287-288 — three-byte character *)
    ((c land 0x0f) lsl 12)
    lor ((Char.code (Bytes.get b (pos + 1)) land 0x3f) lsl 6)
    lor (Char.code (Bytes.get b (pos + 2)) land 0x3f)
  else if Int.equal (c land 0x08) 0 then
    (* pcre2_internal.h:289-291 — four-byte character *)
    ((c land 0x07) lsl 18)
    lor ((Char.code (Bytes.get b (pos + 1)) land 0x3f) lsl 12)
    lor ((Char.code (Bytes.get b (pos + 2)) land 0x3f) lsl 6)
    lor (Char.code (Bytes.get b (pos + 3)) land 0x3f)
  else if Int.equal (c land 0x04) 0 then
    (* pcre2_internal.h:292-295 — five-byte character (invalid UTF-8, but
       ord2utf-encodable under PCRE2_NO_UTF_CHECK garbage) *)
    ((c land 0x03) lsl 24)
    lor ((Char.code (Bytes.get b (pos + 1)) land 0x3f) lsl 18)
    lor ((Char.code (Bytes.get b (pos + 2)) land 0x3f) lsl 12)
    lor ((Char.code (Bytes.get b (pos + 3)) land 0x3f) lsl 6)
    lor (Char.code (Bytes.get b (pos + 4)) land 0x3f)
  else
    (* pcre2_internal.h:296-299 — six-byte character (invalid UTF-8) *)
    ((c land 0x01) lsl 30)
    lor ((Char.code (Bytes.get b (pos + 1)) land 0x3f) lsl 24)
    lor ((Char.code (Bytes.get b (pos + 2)) land 0x3f) lsl 18)
    lor ((Char.code (Bytes.get b (pos + 3)) land 0x3f) lsl 12)
    lor ((Char.code (Bytes.get b (pos + 4)) land 0x3f) lsl 6)
    lor (Char.code (Bytes.get b (pos + 5)) land 0x3f)

(* GETCHARINC (pcre2_intmodedep.h:312-317) + GETUTF8INC
   (pcre2_internal.h:302-334) — get the next UTF-8 character, advancing
   the position. Called when we know we are in UTF-8 mode. *)
let getcharinc (s : string) (pos : int ref) : int =
  let c = Char.code s.[!pos] in
  incr pos;
  if c >= 0xc0 then (
    let p = !pos in
    (* DEVIATION (structural only): GETUTF8INC advances eptr by the number
       of extra bytes as it reads them, arm by arm
       (pcre2_internal.h:305-334); here the decode reuses the GETUTF8 body
       at p - 1 and the advance is GET_EXTRALEN — the bytes read, the
       value, and the final position are identical in every arm
       (utf8_table4[c & 0x3f] equals the per-arm advance for every
       c >= 0xc0). getcharinctest below shares this shape. *)
    let extra = Tables.utf8_table4.(c land 0x3f) in
    pos := p + extra;
    getutf8 c s (p - 1))
  else c

(* GETCHARINCTEST (pcre2_intmodedep.h:319-324) — get the next character,
   testing for UTF-8 mode, and advancing the position. Called when we
   don't know if we are in UTF-8 mode. *)
let getcharinctest ~(utf : bool) (s : string) (pos : int ref) : int =
  let c = Char.code s.[!pos] in
  incr pos;
  if utf && c >= 0xc0 then (
    let p = !pos in
    let extra = Tables.utf8_table4.(c land 0x3f) in
    pos := p + extra;
    getutf8 c s (p - 1))
  else c

(* GETCHARLEN (pcre2_intmodedep.h:326-331) + GETUTF8LEN
   (pcre2_internal.h:336-371) — get the next UTF-8 character, not
   advancing the position, incrementing [lenptr] by the number of extra
   bytes. Called when we know we are in UTF-8 mode. *)
let getcharlen (s : string) (pos : int) (lenptr : int ref) : int =
  let c = Char.code s.[pos] in
  if c >= 0xc0 then (
    lenptr := !lenptr + Tables.utf8_table4.(c land 0x3f);
    getutf8 c s pos)
  else c

(* GETCHARLENTEST (pcre2_intmodedep.h:333-339) — as GETCHARLEN, testing
   for UTF-8 mode. *)
let getcharlentest ~(utf : bool) (s : string) (pos : int) (lenptr : int ref) :
    int =
  let c = Char.code s.[pos] in
  if utf && c >= 0xc0 then (
    lenptr := !lenptr + Tables.utf8_table4.(c land 0x3f);
    getutf8 c s pos)
  else c

(* BACKCHAR (pcre2_intmodedep.h:341-345) — if the position is not at the
   start of a character, move it back until it is. Called only in UTF-8
   mode on a valid string, so the loop stops at the character's leading
   code unit before running off the front of the string. *)
let backchar (s : string) (pos : int) : int =
  let p = ref pos in
  while Int.equal (Char.code s.[!p] land 0xc0) 0x80 do
    decr p
  done;
  !p

(* FORWARDCHARTEST (pcre2_intmodedep.h:349) — same as BACKCHAR, in the
   other direction, bounded by [endpos]. *)
let forwardchartest (s : string) (pos : int) (endpos : int) : int =
  let p = ref pos in
  while !p < endpos && Int.equal (Char.code s.[!p] land 0xc0) 0x80 do
    incr p
  done;
  !p

(* pcre2_ord2utf.c:80-97 — PRIV(ord2utf): convert a code point to a UTF-8
   string, depositing the code units into [buffer] at [pos]. Returns the
   number of code units used. *)
let ord2utf (cvalue : int) (buffer : Bytes.t) (pos : int) : int =
  (* pcre2_ord2utf.c:86-88 — find the number of extra bytes from the
     breakpoint table. The C loop leaves i = utf8_table1_size when cvalue
     exceeds every breakpoint; unreachable (cvalue <= 0x7fffffff in every
     caller, and <= 0x10ffff in practice). *)
  let i = ref 0 in
  let brk = ref false in
  while (not !brk) && !i < Tables.utf8_table1_size do
    if cvalue <= Tables.utf8_table1.(!i) then brk := true else incr i
  done;
  let i = !i in
  (* pcre2_ord2utf.c:89-95 — fill in the bytes from the end backwards,
     then the first byte with the length-indicator bits. buffer += i;
     for (j = i; j > 0; j--) { *buffer-- = 0x80 | (cvalue & 0x3f);
     cvalue >>= 6; } *buffer = utf8_table2[i] | cvalue. *)
  let cv = ref cvalue in
  for j = i downto 1 do
    Bytes.set buffer (pos + j) (Char.chr (0x80 lor (!cv land 0x3f)));
    cv := !cv lsr 6
  done;
  Bytes.set buffer pos (Char.chr (Tables.utf8_table2.(i) lor !cv land 0xff));
  i + 1

(* PUTCHAR (pcre2_intmodedep.h:355-358) — deposit a character into
   [buffer] at [pos], returning the number of code units used. *)
let putchar ~(utf : bool) (c : int) (buffer : Bytes.t) (pos : int) : int =
  if utf && c > max_utf_single_cu then ord2utf c buffer pos
  else (
    (* *p = c: the C's implicit (PCRE2_UCHAR) truncation is the
       land 0xff. *)
    Bytes.set buffer pos (Char.chr (c land 0xff));
    1)

(* ---------- Inline sanity checks (module-initialization asserts) ---------- *)

(* ord2utf encode / getchar decode roundtrips at the encoding-length
   boundary code points (utf8_table1 breakpoints 0x7f/0x7ff/0xffff and
   their successors, plus the top code point 0x10ffff), pinned against the
   UTF-8 definition (RFC 3629) and pcre2_ord2utf.c:86-95. Surrogates have
   no roundtrip contract here: ord2utf encodes any value mechanically and
   rejection is the callers' job (Valid_utf for subjects/patterns,
   check_escape ERR91 for escapes). *)
let () =
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
let () =
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
