(* Engine module-initialization asserts for [Pcre2_engine.Debug_printer], migrated
   verbatim from src/engine/debug_printer.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Debug_printer

(* Pin the match-arm int literals against the Opcodes constants
   (interpreter.ml precedent). *)
let test_0 () =
  List.iter
    (fun (c, v) -> assert (Int.equal c v))
    [
      (Opcodes.op_end, 0);
      (Opcodes.op_notprop, 15);
      (Opcodes.op_prop, 16);
      (Opcodes.op_char, 29);
      (Opcodes.op_chari, 30);
      (Opcodes.op_not, 31);
      (Opcodes.op_noti, 32);
      (Opcodes.op_star, 33);
      (Opcodes.op_minstar, 34);
      (Opcodes.op_plus, 35);
      (Opcodes.op_minplus, 36);
      (Opcodes.op_query, 37);
      (Opcodes.op_minquery, 38);
      (Opcodes.op_upto, 39);
      (Opcodes.op_minupto, 40);
      (Opcodes.op_exact, 41);
      (Opcodes.op_posstar, 42);
      (Opcodes.op_posplus, 43);
      (Opcodes.op_posquery, 44);
      (Opcodes.op_posupto, 45);
      (Opcodes.op_stari, 46);
      (Opcodes.op_minstari, 47);
      (Opcodes.op_plusi, 48);
      (Opcodes.op_minplusi, 49);
      (Opcodes.op_queryi, 50);
      (Opcodes.op_minqueryi, 51);
      (Opcodes.op_uptoi, 52);
      (Opcodes.op_minuptoi, 53);
      (Opcodes.op_exacti, 54);
      (Opcodes.op_posstari, 55);
      (Opcodes.op_posplusi, 56);
      (Opcodes.op_posqueryi, 57);
      (Opcodes.op_posuptoi, 58);
      (Opcodes.op_notstar, 59);
      (Opcodes.op_notminstar, 60);
      (Opcodes.op_notplus, 61);
      (Opcodes.op_notminplus, 62);
      (Opcodes.op_notquery, 63);
      (Opcodes.op_notminquery, 64);
      (Opcodes.op_notupto, 65);
      (Opcodes.op_notminupto, 66);
      (Opcodes.op_notexact, 67);
      (Opcodes.op_notposstar, 68);
      (Opcodes.op_notposplus, 69);
      (Opcodes.op_notposquery, 70);
      (Opcodes.op_notposupto, 71);
      (Opcodes.op_notstari, 72);
      (Opcodes.op_notminstari, 73);
      (Opcodes.op_notplusi, 74);
      (Opcodes.op_notminplusi, 75);
      (Opcodes.op_notqueryi, 76);
      (Opcodes.op_notminqueryi, 77);
      (Opcodes.op_notuptoi, 78);
      (Opcodes.op_notminuptoi, 79);
      (Opcodes.op_notexacti, 80);
      (Opcodes.op_notposstari, 81);
      (Opcodes.op_notposplusi, 82);
      (Opcodes.op_notposqueryi, 83);
      (Opcodes.op_notposuptoi, 84);
      (Opcodes.op_typestar, 85);
      (Opcodes.op_typeminstar, 86);
      (Opcodes.op_typeplus, 87);
      (Opcodes.op_typeminplus, 88);
      (Opcodes.op_typequery, 89);
      (Opcodes.op_typeminquery, 90);
      (Opcodes.op_typeupto, 91);
      (Opcodes.op_typeminupto, 92);
      (Opcodes.op_typeexact, 93);
      (Opcodes.op_typeposstar, 94);
      (Opcodes.op_typeposplus, 95);
      (Opcodes.op_typeposquery, 96);
      (Opcodes.op_typeposupto, 97);
      (Opcodes.op_crstar, 98);
      (Opcodes.op_crminstar, 99);
      (Opcodes.op_crplus, 100);
      (Opcodes.op_crminplus, 101);
      (Opcodes.op_crquery, 102);
      (Opcodes.op_crminquery, 103);
      (Opcodes.op_crrange, 104);
      (Opcodes.op_crminrange, 105);
      (Opcodes.op_crposstar, 106);
      (Opcodes.op_crposplus, 107);
      (Opcodes.op_crposquery, 108);
      (Opcodes.op_crposrange, 109);
      (Opcodes.op_class, 110);
      (Opcodes.op_nclass, 111);
      (Opcodes.op_xclass, 112);
      (Opcodes.op_ref, 113);
      (Opcodes.op_refi, 114);
      (Opcodes.op_dnref, 115);
      (Opcodes.op_dnrefi, 116);
      (Opcodes.op_recurse, 117);
      (Opcodes.op_callout, 118);
      (Opcodes.op_callout_str, 119);
      (Opcodes.op_alt, 120);
      (Opcodes.op_ket, 121);
      (Opcodes.op_ketrmax, 122);
      (Opcodes.op_ketrmin, 123);
      (Opcodes.op_ketrpos, 124);
      (Opcodes.op_reverse, 125);
      (Opcodes.op_vreverse, 126);
      (Opcodes.op_assert, 127);
      (Opcodes.op_assert_not, 128);
      (Opcodes.op_assertback, 129);
      (Opcodes.op_assertback_not, 130);
      (Opcodes.op_assert_na, 131);
      (Opcodes.op_assertback_na, 132);
      (Opcodes.op_once, 133);
      (Opcodes.op_script_run, 134);
      (Opcodes.op_bra, 135);
      (Opcodes.op_brapos, 136);
      (Opcodes.op_cbra, 137);
      (Opcodes.op_cbrapos, 138);
      (Opcodes.op_cond, 139);
      (Opcodes.op_sbra, 140);
      (Opcodes.op_sbrapos, 141);
      (Opcodes.op_scbra, 142);
      (Opcodes.op_scbrapos, 143);
      (Opcodes.op_scond, 144);
      (Opcodes.op_cref, 145);
      (Opcodes.op_dncref, 146);
      (Opcodes.op_rref, 147);
      (Opcodes.op_dnrref, 148);
      (Opcodes.op_false, 149);
      (Opcodes.op_true, 150);
      (Opcodes.op_mark, 154);
      (Opcodes.op_prune_arg, 156);
      (Opcodes.op_skip_arg, 158);
      (Opcodes.op_then, 159);
      (Opcodes.op_then_arg, 160);
      (Opcodes.op_commit_arg, 162);
      (Opcodes.op_close, 166);
    ]

(* End-to-end dump checks. Each expected string is the verbatim
   fullbincode listing (everything pcre2_printint prints: first opcode
   line through the trailing rule of 66 dashes) produced by the real PCRE2
   10.44 8-bit library: `printf '/<pat>/<mods>\n\n...' | pcre2test -d`
   with "PCRE2 version 10.44 2024-06-07". Our compiled programs are
   byte-identical for these patterns, so the dumps must match exactly. *)
let test_1 () =
  let dump ?(options = 0) (pattern : string) : string =
    match Compile.pcre2_compile pattern ~options with
    | Error _ -> assert false
    | Ok re ->
        let buf = Buffer.create 256 in
        pcre2_printint buf re ~print_lengths:true;
        Buffer.contents buf
  in
  let rule =
    "------------------------------------------------------------------\n"
  in
  (* /abc/ — Bra/Ket link values, char-string run. *)
  assert (
    String.equal (dump "abc")
      ("  0   9 Bra\n  3     abc\n  9   9 Ket\n 12     End\n" ^ rule));
  (* /(a|b)+c/i — CBra with group number, /i chars, Alt, KetRmax. *)
  assert (
    String.equal
      (dump ~options:Options.caseless "(a|b)+c")
      ("  0  20 Bra\n\
       \  3   7 CBra 1\n\
       \  8  /i a\n\
       \ 10   5 Alt\n\
       \ 13  /i b\n\
       \ 15  12 KetRmax\n\
       \ 18  /i c\n\
       \ 20  20 Ket\n\
       \ 23     End\n" ^ rule));
  (* /[a-z]{2,4}d/ — class bitmap range + CRRANGE repeat. *)
  assert (
    String.equal (dump "[a-z]{2,4}d")
      ("  0  43 Bra\n  3     [a-z]{2,4}\n 41     d\n 43  43 Ket\n 46     End\n"
     ^ rule));
  (* /^x$/m — the default arm's /m flag (OP_CIRCM/OP_DOLLM). *)
  assert (
    String.equal
      (dump ~options:Options.multiline "^x$")
      ("  0   7 Bra\n\
       \  3  /m ^\n\
       \  4     x\n\
       \  6  /m $\n\
       \  7   7 Ket\n\
       \ 10     End\n" ^ rule));
  (* /a*ab+?bc??cd{4}e{2,7}?e/ — single-char repeat names (each repeat is
     followed by its own character so the auto-possessify pass leaves
     them alone), EXACT, and MINUPTO's {0,n}? form. *)
  assert (
    String.equal
      (dump "a*ab+?bc??cd{4}e{2,7}?e")
      ("  0  29 Bra\n\
       \  3     a*\n\
       \  5     a\n\
       \  7     b+?\n\
       \  9     b\n\
       \ 11     c??\n\
       \ 13     c\n\
       \ 15     d{4}\n\
       \ 19     e{2}\n\
       \ 23     e{0,5}?\n\
       \ 27     e\n\
       \ 29  29 Ket\n\
       \ 32     End\n" ^ rule));
  (* /\d{2,}8\s*?\sx/ — TYPEEXACT, TYPESTAR, TYPEMINSTAR (again arranged
     so the auto-possessify pass does not fire). *)
  assert (
    String.equal (dump "\\d{2,}8\\s*?\\sx")
      ("  0  16 Bra\n\
       \  3     \\d{2}\n\
       \  7     \\d*\n\
       \  9     8\n\
       \ 11     \\s*?\n\
       \ 13     \\s\n\
       \ 14     x\n\
       \ 16  16 Ket\n\
       \ 19     End\n" ^ rule));
  (* /[^a]+x{3}/ — OP_NOT with repeat ([^a]+) and EXACT. *)
  assert (
    String.equal (dump "[^a]+x{3}")
      ("  0   9 Bra\n  3     [^a]+\n  5     x{3}\n  9   9 Ket\n 12     End\n"
     ^ rule));
  (* /(?:a|b)*+/ — Braposzero, BraPos, KetRpos. *)
  assert (
    String.equal (dump "(?:a|b)*+")
      ("  0  17 Bra\n\
       \  3     Braposzero\n\
       \  4   5 BraPos\n\
       \  7     a\n\
       \  9   5 Alt\n\
       \ 12     b\n\
       \ 14  10 KetRpos\n\
       \ 17  17 Ket\n\
       \ 20     End\n" ^ rule));
  (* /a\b\d/ — data-less type opcodes through the default arm. *)
  assert (
    String.equal (dump "a\\b\\d")
      ("  0   7 Bra\n\
       \  3     a\n\
       \  5     \\b\n\
       \  6     \\d\n\
       \  7   7 Ket\n\
       \ 10     End\n" ^ rule));
  (* /\x{0d}[\x00-\x08]z/ — non-printable chars: \x%02x in a char run and
     inside a class bitmap. *)
  assert (
    String.equal
      (dump "\\x{0d}[\\x00-\\x08]z")
      ("  0  40 Bra\n\
       \  3     \\x0d\n\
       \  5     [\\x00-\\x08]\n\
       \ 38     z\n\
       \ 40  40 Ket\n\
       \ 43     End\n" ^ rule));
  (* /[]a-]/ — '-' and ']' are backslash-escaped in class output. *)
  assert (
    String.equal (dump "[]a-]")
      ("  0  36 Bra\n  3     [\\-\\]a]\n 36  36 Ket\n 39     End\n" ^ rule));
  (* Auto-possessified forms (M9): with the auto_possessify pass live,
     these dumps show the possessive rewrites byte-identically to
     `pcre2test -d` on the real 10.44 library (previously the engine
     dumps showed the pre-possess forms, e.g. a* vs the C's a*+). *)
  (* /a*b/ — STAR → POSSTAR before a disjoint character. *)
  assert (
    String.equal (dump "a*b")
      ("  0   7 Bra\n  3     a*+\n  5     b\n  7   7 Ket\n 10     End\n" ^ rule));
  (* /\d+x/ — TYPEPLUS → TYPEPOSPLUS. *)
  assert (
    String.equal (dump "\\d+x")
      ("  0   7 Bra\n  3     \\d++\n  5     x\n  7   7 Ket\n 10     End\n"
     ^ rule));
  (* /[a-z]*z/ — 'z' is inside the class, so CRSTAR must NOT be
     possessified. *)
  assert (
    String.equal (dump "[a-z]*z")
      ("  0  39 Bra\n  3     [a-z]*\n 37     z\n 39  39 Ket\n 42     End\n"
     ^ rule));
  (* /é{2,4}/utf — the UPTO tail becomes POSUPTO ({0,2}+). *)
  assert (
    String.equal
      (dump ~options:Options.utf "\xc3\xa9{2,4}")
      ("  0  13 Bra\n\
       \  3     \\x{e9}{2}\n\
       \  8     \\x{e9}{0,2}+\n\
       \ 13  13 Ket\n\
       \ 16     End\n" ^ rule));
  (* /\p{Nd}{2,}/utf — the TYPESTAR PROP tail becomes TYPEPOSSTAR. *)
  assert (
    String.equal
      (dump ~options:Options.utf "\\p{Nd}{2,}")
      ("  0  13 Bra\n\
       \  3     prop Nd {2}\n\
       \  9     prop Nd *+\n\
       \ 13  13 Ket\n\
       \ 16     End\n" ^ rule));
  (* /(a|b)*c/ — group repeats (KETRMAX) are never rewritten by the
     pass. *)
  assert (
    String.equal (dump "(a|b)*c")
      ("  0  21 Bra\n\
       \  3     Brazero\n\
       \  4   7 CBra 1\n\
       \  9     a\n\
       \ 11   5 Alt\n\
       \ 14     b\n\
       \ 16  12 KetRmax\n\
       \ 19     c\n\
       \ 21  21 Ket\n\
       \ 24     End\n" ^ rule));
  (* /( *NO_AUTO_POSSESS)a*b/ — PCRE2_NO_AUTO_POSSESS suppresses the pass
     (pcre2_compile.c:10801): a* stays. *)
  assert (
    String.equal
      (dump "(*NO_AUTO_POSSESS)a*b")
      ("  0   7 Bra\n  3     a*\n  5     b\n  7   7 Ket\n 10     End\n" ^ rule))

let tests =
  [
    Alcotest.test_case "debug_printer 0" `Quick test_0;
    Alcotest.test_case "debug_printer 1" `Quick test_1;
  ]
