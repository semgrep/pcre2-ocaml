(* Horizontal / vertical space byte predicates, shared between the mainline
   interpreter (interpreter.ml) and the M11 fast engine (src/fast/runner.ml).

   These are a pure extraction of the two module-level helpers that lived in
   interpreter.ml (identical bodies, same citations); the interpreter now
   aliases them so the two engines cannot diverge on the \h / \H / \v / \V byte
   tests. No behavior change — proved by the full engine conformance
   (--driver=engine) staying byte-identical. *)

(* pcre2_internal.h:424-427 — HSPACE_BYTE_CASES: HT, SPACE, NBSP. The 8-bit
   code-unit switches in pcre2_match.c use only these (the
   HSPACE_MULTIBYTE_CASES arms are compiled out at PCRE2_CODE_UNIT_WIDTH ==
   8). *)
let hspace_byte (c : int) : bool =
  Int.equal c 0x09 || Int.equal c 0x20 || Int.equal c 0xa0

(* pcre2_internal.h:440-445 — VSPACE_BYTE_CASES: LF, VT, FF, CR, NEL. *)
let vspace_byte (c : int) : bool =
  Int.equal c Newline.char_lf
  || Int.equal c Newline.char_vt
  || Int.equal c Newline.char_ff
  || Int.equal c Newline.char_cr
  || Int.equal c Newline.char_nel

(* pcre2_internal.h:416-431 — HSPACE_CASES: the byte cases plus
   HSPACE_MULTIBYTE_CASES, for the UTF single/repeat arms that switch on a
   DECODED code point (\h / \H in UTF mode). Extracted here so the interpreter
   and the M11 fast engine share one definition (no behavior change — the body
   is identical to the interpreter's former hspace_char). *)
let hspace_char (c : int) : bool =
  match c with
  | 0x09 | 0x20 | 0xa0 | 0x1680 | 0x180e | 0x2000 | 0x2001 | 0x2002 | 0x2003
  | 0x2004 | 0x2005 | 0x2006 | 0x2007 | 0x2008 | 0x2009 | 0x200a | 0x202f
  | 0x205f | 0x3000 ->
      true
  | _ -> false

(* pcre2_internal.h:433-449 — VSPACE_CASES: the byte cases plus
   VSPACE_MULTIBYTE_CASES (U+2028 LS, U+2029 PS). *)
let vspace_char (c : int) : bool =
  match c with
  | 0x0a | 0x0b | 0x0c | 0x0d | 0x85 | 0x2028 | 0x2029 -> true
  | _ -> false
