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
