(* Engine module-initialization asserts for [Pcre2_engine.Script_run], migrated
   verbatim from src/engine/script_run.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Script_run

(* Map sizes pinned against pcre2_ucp.h: ucp_Unknown = 68 and
   ucp_Script_Count = 164, so the item size of ucd_script_sets is
   ucd_mapsize = 3 (pcre2_ucp.h:390-392) and the full maps hold 6
   words. *)
let test_0 () =
  assert (Int.equal ucd_mapsize 3);
  assert (Int.equal full_mapsize 6);
  assert (Int.equal (Array.length Ucd_tables.ucd_script_sets mod 3) 0)

(* Behavior pinned against pcre2test 10.44 `/^(*sr:.*)/utf` oracle runs
   (and the rules in pcre2_script_run.c's comments). *)
let test_1 () =
  let buf = Bytes.make 64 '\000' in
  let enc (cs : int list) : string =
    let p = ref 0 in
    List.iter (fun c -> p := !p + Utf.ord2utf c buf !p) cs;
    Bytes.sub_string buf 0 !p
  in
  let sr (cs : int list) : bool =
    let s = enc cs in
    script_run s 0 (String.length s) true
  in
  (* Fewer than 2 characters is always a valid run. *)
  assert (sr []);
  assert (sr [ 0x3b1 ]);
  (* Single-script runs hold; mixed Latin/Greek/Cyrillic do not. *)
  assert (sr [ 0x61; 0x62; 0x63 ]);
  assert (sr [ 0x3b1; 0x3b2 ]);
  assert (not (sr [ 0x61; 0x3b1 ]));
  assert (not (sr [ 0x430; 0x61 ]));
  (* Common (digits, punctuation) combines with anything. *)
  assert (sr [ 0x61; 0x31; 0x62 ]);
  assert (sr [ 0x3b1; 0x20; 0x3b2 ]);
  (* Han interplay: Han+Hiragana, Han+Katakana, Han+Bopomofo, Han+Hangul
     are allowed; Hiragana+Hangul and Bopomofo+Hiragana are not.
     U+6F22 Han, U+3042 Hiragana, U+30AB Katakana, U+3105 Bopomofo,
     U+AC00 Hangul. *)
  assert (Int.equal (Ucd.script 0x6f22) Ucp.ucp_han);
  assert (Int.equal (Ucd.script 0x3042) Ucp.ucp_hiragana);
  assert (Int.equal (Ucd.script 0x30ab) Ucp.ucp_katakana);
  assert (Int.equal (Ucd.script 0x3105) Ucp.ucp_bopomofo);
  assert (Int.equal (Ucd.script 0xac00) Ucp.ucp_hangul);
  assert (sr [ 0x6f22; 0x3042 ]);
  assert (sr [ 0x3042; 0x6f22; 0x30ab ]);
  assert (sr [ 0x6f22; 0x3105 ]);
  assert (sr [ 0x6f22; 0xac00 ]);
  assert (not (sr [ 0x3042; 0xac00 ]));
  assert (not (sr [ 0x3105; 0x3042 ]));
  (* Script extensions: U+30FC (Common, scx {Hiragana, Katakana}) is fine
     in a Katakana run but not in a Latin one. *)
  assert (sr [ 0x30ab; 0x30fc ]);
  assert (not (sr [ 0x61; 0x30fc ]));
  (* Unknown script: only runs of length one. U+0378 is unassigned. *)
  assert (Int.equal (Ucd.script 0x378) Ucp.ucp_unknown);
  assert (sr [ 0x378 ]);
  assert (not (sr [ 0x378; 0x378 ]));
  (* Digit-set rule: ASCII digits mix with each other but not with
     Arabic-Indic digits (different sets — and different scripts: use
     Common U+0964-adjacent case instead). Devanagari digits U+0966/U+096F
     share a set; mixing them with ASCII digits fails on the digit rule
     even though ASCII digits are Common. *)
  assert (sr [ 0x31; 0x32; 0x33 ]);
  assert (Int.equal (Ucd.script 0x966) Ucp.ucp_devanagari);
  assert (sr [ 0x966; 0x96f ]);
  assert (not (sr [ 0x31; 0x966 ]));
  (* Non-UTF (byte) mode: bytes are Latin-1 code points. *)
  assert (script_run "abc" 0 3 false);
  assert (script_run "a1b" 0 3 false)

let tests =
  [
    Alcotest.test_case "script_run 0" `Quick test_0;
    Alcotest.test_case "script_run 1" `Quick test_1;
  ]
