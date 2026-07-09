(* Engine module-initialization asserts for [Pcre2_engine.Ucd], migrated
   verbatim from src/engine/ucd.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Ucd

(* Inline sanity checks: ASCII case pairs and the caseless sets that
   compile_branch's class optimizations depend on (pcre2_compile.c:5962,
   5931-5934): k/K/s/S have multi-character caseless sets (with 0x212a
   KELVIN SIGN and 0x17f LONG S); other ASCII letters do not. *)
let test_0 () =
  assert (Int.equal (chartype (Char.code 'a')) Ucp.ucp_ll);
  assert (Int.equal (chartype (Char.code 'A')) Ucp.ucp_lu);
  assert (Int.equal (chartype (Char.code '0')) Ucp.ucp_nd);
  assert (Int.equal (chartype 0x0660) Ucp.ucp_nd (* ARABIC-INDIC DIGIT 0 *));
  assert (Int.equal (chartype 0xe9) Ucp.ucp_ll (* U+00E9 *));
  assert (Int.equal (othercase (Char.code 'a')) (Char.code 'A'));
  assert (Int.equal (othercase (Char.code 'Z')) (Char.code 'z'));
  assert (Int.equal (othercase (Char.code '0')) (Char.code '0'));
  assert (Int.equal (caseset (Char.code 'a')) 0);
  assert (Int.equal (caseset (Char.code 'B')) 0);
  let ks = caseset (Char.code 'k') in
  assert (not (Int.equal ks 0));
  assert (not (Int.equal (caseset (Char.code 'K')) 0));
  assert (not (Int.equal (caseset (Char.code 's')) 0));
  assert (not (Int.equal (caseset (Char.code 'S')) 0));
  (* pcre2_ucd.c:114-143 — the k/K set is {K, k, 0x212a}, NOTACHAR-ended. *)
  assert (Int.equal Ucd_tables.ucd_caseless_sets.(ks) 0x4b);
  assert (Int.equal Ucd_tables.ucd_caseless_sets.(ks + 1) 0x6b);
  assert (Int.equal Ucd_tables.ucd_caseless_sets.(ks + 2) 0x212a);
  assert (Int.equal Ucd_tables.ucd_caseless_sets.(ks + 3) Tables.notachar);
  (* The out-of-range clamp (DEVIATION above): U+10FFFF's record carries
     the same field values as the 32-bit library's dummy record
     (pcre2_ucd.c:99-108) for the accessors asserted here (bidiclass and
     bprops diverge; see the record_index comment), and values above the
     maximum read that record instead of overrunning stage1. *)
  assert (Int.equal (chartype 0x10ffff) Ucp.ucp_cn);
  assert (Int.equal (caseset 0x10ffff) 0);
  assert (Int.equal (othercase 0x10ffff) 0x10ffff);
  assert (Int.equal (chartype 0x200000) Ucp.ucp_cn);
  assert (Int.equal (caseset 0x7fffffff) 0);
  assert (Int.equal (othercase 0x200000) 0x200000);
  (* Script, bidi class, script-extension and Boolean-property probes
     (values per UCD 15.0.0, the vendored tables' version): Latin a,
     GREEK SMALL LETTER ALPHA, CYRILLIC A, DEVANAGARI KA; U+0964
     DEVANAGARI DANDA is script Common but its Script Extensions set
     holds Devanagari and Bengali (among ~20 Indic scripts) — the PT_SCX
     shape; a's own-script probe goes through the "== prop->script" arm,
     so its (empty-set) scriptx bit for Latin is clear. *)
  assert (Int.equal (script (Char.code 'a')) Ucp.ucp_latin);
  assert (Int.equal (script 0x3b1) Ucp.ucp_greek);
  assert (Int.equal (script 0x430) Ucp.ucp_cyrillic);
  assert (Int.equal (script 0x915) Ucp.ucp_devanagari);
  assert (Int.equal (script 0x964) Ucp.ucp_common);
  assert (script_set_contains (record_index 0x964) Ucp.ucp_devanagari);
  assert (script_set_contains (record_index 0x964) Ucp.ucp_bengali);
  assert (not (script_set_contains (record_index 0x964) Ucp.ucp_greek));
  assert (not (script_set_contains (record_index (Char.code 'a')) Ucp.ucp_latin));
  (* U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK: script Common, scx
     {Hiragana, Katakana}. *)
  assert (Int.equal (script 0x30fc) Ucp.ucp_common);
  assert (script_set_contains (record_index 0x30fc) Ucp.ucp_katakana);
  assert (script_set_contains (record_index 0x30fc) Ucp.ucp_hiragana);
  (* Bidi classes: 'a' L, ALEF R, ARABIC LETTER AIN AL, '0' EN. *)
  assert (Int.equal (bidiclass (Char.code 'a')) Ucp.ucp_bidi_l);
  assert (Int.equal (bidiclass 0x5d0) Ucp.ucp_bidi_r);
  assert (Int.equal (bidiclass 0x639) Ucp.ucp_bidi_al);
  assert (Int.equal (bidiclass (Char.code '0')) Ucp.ucp_bidi_en);
  (* Boolean properties: Cased holds for a/A but not 0 or space; ASCII
     for 'a' but not U+00E9; White_Space for space. *)
  assert (boolprop_set_contains (record_index (Char.code 'a')) Ucp.ucp_cased);
  assert (boolprop_set_contains (record_index (Char.code 'A')) Ucp.ucp_cased);
  assert (
    not (boolprop_set_contains (record_index (Char.code '0')) Ucp.ucp_cased));
  assert (
    not (boolprop_set_contains (record_index (Char.code ' ')) Ucp.ucp_cased));
  assert (boolprop_set_contains (record_index (Char.code 'a')) Ucp.ucp_ascii);
  assert (not (boolprop_set_contains (record_index 0xe9) Ucp.ucp_ascii));
  assert (
    boolprop_set_contains (record_index (Char.code ' ')) Ucp.ucp_white_space);
  (* Grapheme break properties: CR/LF/ZWJ/COMBINING GRAVE (Extend)/
     REGIONAL INDICATOR U+1F1E6. *)
  assert (Int.equal (gbprop 0x0d) Ucp.ucp_gb_cr);
  assert (Int.equal (gbprop 0x0a) Ucp.ucp_gb_lf);
  assert (Int.equal (gbprop 0x200d) Ucp.ucp_gb_zwj);
  assert (Int.equal (gbprop 0x300) Ucp.ucp_gb_extend);
  assert (Int.equal (gbprop 0x1f1e6) Ucp.ucp_gb_regional_indicator);
  assert (Int.equal (gbprop (Char.code 'a')) Ucp.ucp_gb_other)

let tests = [ Alcotest.test_case "ucd 0" `Quick test_0 ]
