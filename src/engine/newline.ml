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

(* GETCHAR (pcre2_intmodedep.h:298-303) — the GETCHAR macro family belongs
   to module Utf (naming map); this alias keeps this module's callers in
   the C's vocabulary. BACKCHAR (pcre2_intmodedep.h:341-345) is not
   aliased from Utf.backchar: its only use here, in was_newline, is
   transcribed inline with a lower bound (see the DEVIATION note there). *)
let getchar = Utf.getchar

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
     else c = *ptr.
     DEVIATION (defined behavior where the C is undefined — the Utf.peek /
     Interpreter.backchar_subject precedent): when every code unit before
     [pos] is a UTF-8 continuation byte, C 10.44's BACKCHAR
     (pcre2_newline.c:178) walks PAST the start of the subject — an
     out-of-bounds read, confirmed by ASan on the vendored sources and
     still present upstream. It is reachable only with
     PCRE2_MATCH_INVALID_UTF: the bad-start skip moves matching beyond
     invalid leading code units (pcre2_match.c:6829-6851) while
     WAS_NEWLINE's guard stays the true subject start, mb->start_subject
     (pcre2_internal.h:510-521, pcre2_match.c:60-66 and 6962). The C
     oracle receives the OCaml string data pointer directly, so the byte
     it reads before the subject is the last byte of the OCaml block
     header — on the little-endian oracle host in use, the header's high
     size byte, deterministically 0x00 for any realistic length (a
     big-endian host would expose the tag byte >= 0xc0 there instead, and
     the C would decode garbage): BACKCHAR stops on the 0x00 (not a
     continuation byte) and GETCHAR yields c = 0, which matches no arm of
     either switch below. The pinned behavior — stop the walk at position
     -1 and read the character there as 0 — is itself
     platform-independent and matches the recorded oracle outcome. *)
  let ptr =
    if utf then (
      let p = ref ptr in
      while !p >= 0 && Int.equal (Char.code subject.[!p] land 0xc0) 0x80 do
        decr p
      done;
      !p)
    else ptr
  in
  let c =
    if ptr < 0 then 0 (* the oracle's 0x00 header byte before the subject *)
    else if utf then getchar subject ptr
    else Char.code subject.[ptr]
  in
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
