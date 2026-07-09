(* Newline testing for the pure-OCaml PCRE2 10.44 port (8-bit library).

   Ported from vendor/pcre2/src/pcre2_newline.c:1-243.

   pcre2_newline.c:42-48 — internal functions for testing newlines when
   more than one kind of newline is to be recognized. When a newline is
   found, its length is returned. PCRE2 supports NLTYPE_FIXED, which gets
   handled without these functions, NLTYPE_ANYCRLF, and NLTYPE_ANY.

   Signature mapping (C -> OCaml): the C functions receive a PCRE2_SPTR
   [ptr] into the string plus an endptr/startptr bound; an OCaml "pointer"
   is the pair of the [subject] string and an int position, so C's
   (ptr, type, endptr, lenptr, utf) becomes
   (subject, type_, pos, endptr, lenptr, utf). The uint32_t *lenptr
   out-parameter is an [int ref], written exactly where the C writes it
   (only on TRUE returns; untouched on FALSE).

   Ownership of NLTYPE_FIXED: these functions are called only via the
   IS_NEWLINE / WAS_NEWLINE macros (pcre2_internal.h:496-521), which
   dispatch here only when the newline type is NLTYPE_ANY or
   NLTYPE_ANYCRLF. A fixed newline (NLTYPE_FIXED) is compared inline by
   each macro user against NLBLOCK->nl[0..1] / nllen; the callers own that
   arm (compile side: Parse.is_newline_at; match side: the interpreter's
   IS_NEWLINE/WAS_NEWLINE equivalents). Mirroring the C, [type_] here is
   never NLTYPE_FIXED: any type other than NLTYPE_ANYCRLF takes the
   NLTYPE_ANY switch, exactly like the C's if/else. *)

(* pcre2_internal.h:490-492 — newline-convention types. *)
let nltype_fixed = 0 (* Newline is a fixed length string *)
let nltype_any = 1 (* Newline is any Unicode line ending *)
let nltype_anycrlf = 2 (* Newline is CR, LF, or CRLF *)

(* Character constants, ASCII/non-EBCDIC (pcre2_internal.h:678-699). *)
let char_lf = 0x0a (* pcre2_internal.h:678 — CHAR_LF *)
let char_vt = 0x0b (* pcre2_internal.h:697 — CHAR_VT *)
let char_ff = 0x0c (* pcre2_internal.h:698 — CHAR_FF *)
let char_cr = 0x0d (* pcre2_internal.h:699 — CHAR_CR *)
let char_nel = 0x85 (* pcre2_internal.h:680 — CHAR_NEL *)

(* GETCHAR (pcre2_intmodedep.h:298-303) and BACKCHAR
   (pcre2_intmodedep.h:341-345) — the GETCHAR macro family belongs to
   module Utf (naming map); these aliases keep this module's existing
   callers and asserts in the C's vocabulary. *)
let getchar = Utf.getchar
let backchar = Utf.backchar

(* pcre2_newline.c:59-145 — PRIV(is_newline): check for a newline at the
   given position. Called only via the IS_NEWLINE macro, which does so only
   when the newline type is NLTYPE_ANY or NLTYPE_ANYCRLF. It is guaranteed
   that the code unit at [pos] is less than the end of the string
   ([pos] < [endptr]). Returns TRUE or FALSE; on TRUE the newline's length
   in code units is stored through [lenptr]. *)
let is_newline (subject : string) (type_ : int) (pos : int) (endptr : int)
    (lenptr : int ref) (utf : bool) : bool =
  (* pcre2_newline.c:84-85 — if utf GETCHAR(c, ptr) else c = *ptr *)
  let c = if utf then getchar subject pos else Char.code subject.[pos] in
  if Int.equal type_ nltype_anycrlf then
    (* pcre2_newline.c:91-103 — NLTYPE_ANYCRLF *)
    if Int.equal c char_lf then (
      lenptr := 1;
      true)
    else if Int.equal c char_cr then (
      (* pcre2_newline.c:98 — ptr < endptr - 1 && ptr[1] == CHAR_LF *)
      lenptr :=
        if pos < endptr - 1 && Int.equal (Char.code subject.[pos + 1]) char_lf
        then 2
        else 1;
      true)
    else false (* pcre2_newline.c:101-102 *)
  else if
    (* pcre2_newline.c:105-116 — NLTYPE_ANY: LF, VT, FF *)
    Int.equal c char_lf || Int.equal c char_vt || Int.equal c char_ff
  then (
    lenptr := 1;
    true)
  else if Int.equal c char_cr then (
    (* pcre2_newline.c:118-120 *)
    lenptr :=
      if pos < endptr - 1 && Int.equal (Char.code subject.[pos + 1]) char_lf
      then 2
      else 1;
    true)
  else if Int.equal c char_nel then (
    (* pcre2_newline.c:124-126 — 8-bit: NEL is the two code units 0xC2 0x85
       in UTF-8 mode, the single code unit 0x85 otherwise *)
    lenptr := if utf then 2 else 1;
    true)
  else if Int.equal c 0x2028 || Int.equal c 0x2029 then (
    (* pcre2_newline.c:128-131 — LS / PS: three UTF-8 code units. Reachable
       only when utf decoded a multi-byte character (a non-UTF 8-bit code
       unit is always < 0x100). *)
    lenptr := 3;
    true)
  else false (* pcre2_newline.c:142-143 *)

(* pcre2_newline.c:149-241 — PRIV(was_newline): check for a newline
   immediately preceding the given position. Called only via the
   WAS_NEWLINE macro, which does so only when the newline type is
   NLTYPE_ANY or NLTYPE_ANYCRLF. It is guaranteed that the initial value of
   [pos] is greater than the start of the string ([pos] > [startptr]).
   Returns TRUE or FALSE; on TRUE the newline's length in code units is
   stored through [lenptr]. *)
let was_newline (subject : string) (type_ : int) (pos : int) (startptr : int)
    (lenptr : int ref) (utf : bool) : bool =
  (* pcre2_newline.c:173 — ptr-- *)
  let ptr = pos - 1 in
  (* pcre2_newline.c:176-181 — if utf { BACKCHAR(ptr); GETCHAR(c, ptr); }
     else c = *ptr *)
  let ptr = if utf then backchar subject ptr else ptr in
  let c = if utf then getchar subject ptr else Char.code subject.[ptr] in
  if Int.equal type_ nltype_anycrlf then
    (* pcre2_newline.c:187-199 — NLTYPE_ANYCRLF *)
    if Int.equal c char_lf then (
      (* pcre2_newline.c:190 — ptr > startptr && ptr[-1] == CHAR_CR *)
      lenptr :=
        if ptr > startptr && Int.equal (Char.code subject.[ptr - 1]) char_cr
        then 2
        else 1;
      true)
    else if Int.equal c char_cr then (
      lenptr := 1;
      true)
    else false (* pcre2_newline.c:197-198 *)
  else if Int.equal c char_lf then (
    (* pcre2_newline.c:205-207 — NLTYPE_ANY: LF, CRLF when preceded by CR *)
    lenptr :=
      if ptr > startptr && Int.equal (Char.code subject.[ptr - 1]) char_cr then
        2
      else 1;
    true)
  else if Int.equal c char_vt || Int.equal c char_ff || Int.equal c char_cr then (
    (* pcre2_newline.c:212-216 *)
    lenptr := 1;
    true)
  else if Int.equal c char_nel then (
    (* pcre2_newline.c:220-222 — 8-bit: utf? 2 : 1 *)
    lenptr := if utf then 2 else 1;
    true)
  else if Int.equal c 0x2028 || Int.equal c 0x2029 then (
    (* pcre2_newline.c:224-227 — LS / PS: three UTF-8 code units *)
    lenptr := 3;
    true)
  else false (* pcre2_newline.c:238-239 *)

(* ---------- Inline sanity checks (module-initialization asserts) ---------- *)

(* PRIV(is_newline) over whole subjects (endptr = length, startptr = 0),
   checked against pcre2_newline.c:78-145 by hand: NLTYPE_ANY matches LF,
   VT, FF, CR (length 2 when an LF follows before endptr, else 1), NEL and
   (in UTF mode) LS/PS; NLTYPE_ANYCRLF matches only CR, LF, CRLF. *)
let () =
  let probe_is type_ subj pos utf =
    let len = ref 0 in
    let hit = is_newline subj type_ pos (String.length subj) len utf in
    (hit, !len)
  in
  (* ANY, non-UTF: LF / VT / FF are newlines of length 1. *)
  (match probe_is nltype_any "\n" 0 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\x0b" 0 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\x0c" 0 false with
  | true, 1 -> ()
  | _ -> assert false);
  (* ANY: CRLF has length 2; CR before non-LF and CR at the last position
     (the endptr boundary in pcre2_newline.c:119) have length 1. *)
  (match probe_is nltype_any "\r\na" 0 false with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\rx" 0 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "a\r" 1 false with
  | true, 1 -> ()
  | _ -> assert false);
  (* endptr short of a following LF: same CR arm, length 1 not 2. *)
  (let len = ref 0 in
   assert (is_newline "\r\n" nltype_any 0 1 len false);
   assert (Int.equal !len 1));
  (* ANY, non-UTF: NEL is the single code unit 0x85, length 1; the 0xC2
     lead byte of its UTF-8 encoding is NOT a newline without utf. *)
  (match probe_is nltype_any "a\x85b" 1 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\xc2\x85" 0 false with
  | false, _ -> ()
  | _ -> assert false);
  (* ANY, utf: NEL decodes from 0xC2 0x85, length 2; LS U+2028 and PS
     U+2029 decode from three code units, length 3. *)
  (match probe_is nltype_any "\xc2\x85" 0 true with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\xe2\x80\xa8" 0 true with
  | true, 3 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\xe2\x80\xa9" 0 true with
  | true, 3 -> ()
  | _ -> assert false);
  (* ANY: an ordinary character is not a newline; another multi-byte
     character (U+00E9) is not a newline in UTF mode. *)
  (match probe_is nltype_any "a" 0 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\xc3\xa9" 0 true with
  | false, _ -> ()
  | _ -> assert false);
  (* ANYCRLF: LF 1, CR 1, CRLF 2; VT / FF / NEL (either encoding) / LS are
     NOT newlines. *)
  (match probe_is nltype_anycrlf "\n" 0 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "a\r" 1 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "\r\n" 0 false with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "\x0b" 0 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "\x0c" 0 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "a\x85b" 1 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "\xc2\x85" 0 true with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "\xe2\x80\xa8" 0 true with
  | false, _ -> ()
  | _ -> assert false);
  (* FALSE leaves *lenptr untouched, like the C. *)
  let len = ref 42 in
  assert (not (is_newline "abc" nltype_any 0 3 len false));
  assert (Int.equal !len 42)

(* PRIV(was_newline), checked against pcre2_newline.c:168-241: [pos] points
   just past the candidate newline; CRLF is recognized from its LF with the
   CR looked up behind, guarded by startptr. *)
let () =
  let probe_was type_ subj pos utf =
    let len = ref 0 in
    let hit = was_newline subj type_ pos 0 len utf in
    (hit, !len)
  in
  (* ANY, non-UTF: LF preceded by CR gives length 2; plain LF, VT, FF, CR
     give length 1. *)
  (match probe_was nltype_any "a\r\n" 3 false with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "a\n" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "a\x0b" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "a\x0c" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "a\r" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (* startptr boundary (pcre2_newline.c:206): the LF is at the start of the
     string, so no CR can be looked up before it — length 1. *)
  (match probe_was nltype_any "\nx" 1 false with
  | true, 1 -> ()
  | _ -> assert false);
  (* Same boundary via a non-zero startptr: the CR exists in the string but
     lies before startptr, so it is not consulted. *)
  (let len = ref 0 in
   assert (was_newline "\r\n" nltype_any 2 1 len false);
   assert (Int.equal !len 1));
  (* ANY, non-UTF: NEL single byte, length 1. *)
  (match probe_was nltype_any "a\x85" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (* ANY, utf: BACKCHAR walks back over the continuation byte(s); NEL is
     length 2, LS/PS length 3. *)
  (match probe_was nltype_any "a\xc2\x85" 3 true with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "\xe2\x80\xa8" 3 true with
  | true, 3 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "\xe2\x80\xa9" 3 true with
  | true, 3 -> ()
  | _ -> assert false);
  (* ANY: not a newline (also multi-byte non-newline in UTF mode). *)
  (match probe_was nltype_any "ab" 2 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_was nltype_any "\xc3\xa9" 2 true with
  | false, _ -> ()
  | _ -> assert false);
  (* ANYCRLF: CRLF 2, LF 1, CR 1; VT and NEL are NOT newlines. *)
  (match probe_was nltype_anycrlf "\r\n" 2 false with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_was nltype_anycrlf "a\n" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_was nltype_anycrlf "a\r" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_was nltype_anycrlf "a\x0b" 2 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_was nltype_anycrlf "a\x85" 2 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_was nltype_anycrlf "a\xc2\x85" 3 true with
  | false, _ -> ()
  | _ -> assert false);
  (* FALSE leaves *lenptr untouched, like the C. *)
  let len = ref 42 in
  assert (not (was_newline "ab" nltype_any 2 0 len false));
  assert (Int.equal !len 42)
