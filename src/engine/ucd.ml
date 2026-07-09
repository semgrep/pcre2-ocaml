(* UCD record access: the GET_UCD macro family from pcre2_internal.h over
   the generated Ucd_tables (docs/ocaml-engine/00-architecture.md maps this
   module to those macros). Only the accessors needed so far are defined;
   the M7 (UCP) chunks extend this module with the remaining UCD_* lookups
   (script, gbprop, ...). *)

(* pcre2_internal.h:1864-1867 — REAL_GET_UCD(ch): the ucd_record for code
   point ch is
     ucd_records[ucd_stage2[ucd_stage1[ch / UCD_BLOCK_SIZE] * UCD_BLOCK_SIZE
                            + ch % UCD_BLOCK_SIZE]]
   returned here as the record index into the per-field tables of
   Ucd_tables.

   DEVIATION (defined behavior where the C is undefined): code points
   above MAX_UTF_CODE_POINT are clamped to 0x10ffff before indexing. Such
   values arise from decoding invalid UTF (5/6-byte forms / invalid lead
   bytes in Utf.getutf8) — reachable both under PCRE2_NO_UTF_CHECK garbage
   AND under PCRE2_MATCH_INVALID_UTF: an invalid lead byte such as 0xff
   (>= 0xc0) sent through GETCHARINCTEST in the OP_XCLASS \p{...} path
   decodes to a code point >= 0x40000000 (fuzz_diff seed 101 case 153814,
   subject 0xff 0x60 under match_invalid_utf; ASan pcre2_xclass.c:137).
   PCRE2 documents this as undefined: the 8-bit GET_UCD is a bare
   REAL_GET_UCD and indexes off the end of stage1 (pcre2_internal.h:
   1869-1873 guards only in the 32-bit library, where GET_UCD returns
   PRIV(dummy_ucd_record) — script Unknown, type Cn, no case set, no other
   case, pcre2_ucd.c:95-108). Clamping yields U+10FFFF's record, whose
   fields agree with the dummy record for chartype/caseset/other_case/
   script/gbprop/scriptx (asserted below); bidiclass (bidiBN vs the
   dummy's bidiL) and bprops (non-empty set vs the dummy's empty) DIVERGE
   from the dummy — but only under 8-bit C UB, so no defined-behavior
   divergence exists. The dev-only differential oracle's libpcre2 build is
   patched to clamp GET_UCD to MAX_UTF_CODE_POINT identically (see
   oracle/patches/pcre2-10.44-oracle-ucd-clamp.patch), so oracle == engine
   record-for-record on these code points. No dummy row needed in the
   generated tables. *)
let record_index (ch : int) : int =
  let ch = if ch > 0x10ffff then 0x10ffff else ch in
  Ucd_tables.stage2
    (Ucd_tables.stage1 (ch / Ucd_tables.ucd_block_size)
     * Ucd_tables.ucd_block_size
    + (ch mod Ucd_tables.ucd_block_size))

(* pcre2_internal.h:1884 — UCD_CHARTYPE(ch): the particular character
   type (Ucp.ucp_cc..ucp_zs) from ch's ucd_record. *)
let chartype (ch : int) : int = Ucd_tables.chartype (record_index ch)

(* pcre2_internal.h:1885 — UCD_SCRIPT(ch): the script (Ucp.ucp_latin..)
   from ch's ucd_record. *)
let script (ch : int) : int = Ucd_tables.script (record_index ch)

(* pcre2_internal.h:1887 — UCD_GRAPHBREAK(ch): the grapheme break
   property (Ucp.ucp_gb_cr..ucp_gb_extended_pictographic) from ch's
   ucd_record. *)
let gbprop (ch : int) : int = Ucd_tables.gbprop (record_index ch)

(* pcre2_internal.h:1869-1882 — the packed uint16 record fields:
     UCD_SCRIPTX_PROP(prop)   = prop->scriptx_bidiclass & UCD_SCRIPTX_MASK
                                (0x3ff, 1876)
     UCD_BIDICLASS_PROP(prop) = prop->scriptx_bidiclass
                                >> UCD_BIDICLASS_SHIFT (11, 1877)
     UCD_BPROPS_PROP(prop)    = prop->bprops & UCD_BPROPS_MASK (0xfff, 1878)
   These take the ucd_record (here: its index [ri] = [record_index ch],
   the C's GET_UCD pointer), letting a caller that probes several fields
   fetch the record once, as the C does. *)
let scriptx_prop (ri : int) : int = Ucd_tables.scriptx_bidiclass ri land 0x3ff
let bidiclass_prop (ri : int) : int = Ucd_tables.scriptx_bidiclass ri lsr 11
let bprops_prop (ri : int) : int = Ucd_tables.bprops ri land 0xfff

(* pcre2_internal.h:1892 — UCD_BIDICLASS(ch): the bidi class
   (Ucp.ucp_bidi_al..ucp_bidi_ws) from ch's ucd_record. *)
let bidiclass (ch : int) : int = bidiclass_prop (record_index ch)

(* pcre2_internal.h:1894-1898 — the scriptx and bprops fields are offsets
   into vectors of 32-bit words that form bitmaps, tested by bit number:
   MAPBIT(map,n) = map[n/32] & (1u << (n%32)). [n] is non-negative, so
   lsr 5 / land 31 are the C's unsigned n/32 and n%32. *)

(* MAPBIT(PRIV(ucd_script_sets) + UCD_SCRIPTX_PROP(prop), n) != 0
   (pcre2_match.c:2525, pcre2_xclass.c:168): is script [n] in record
   [ri]'s Script Extensions set? In bounds: scriptx_prop is a word offset
   to the start of a ucd_script_sets_item_size = 3-word item
   (pcre2_ucp.h:390-392, validated at generation), and every PT_SCX
   property value reaching a compiled pattern is <= ucp_old_uyghur = 67
   (the only Ucptables.utt entry type carrying PT_SCX values; scripts
   beyond 67 have no cross-script characters and get PT_SC entries,
   pcre2_ucp.h:215-218 + pcre2_ucptables.c), so n lsr 5 <= 2. *)
let script_set_contains (ri : int) (n : int) : bool =
  not
    (Int.equal
       (Ucd_tables.ucd_script_sets.(scriptx_prop ri + (n lsr 5))
       land (1 lsl (n land 31)))
       0)

(* MAPBIT(PRIV(ucd_boolprop_sets) + UCD_BPROPS_PROP(prop), n) != 0
   (pcre2_match.c:2600-2601, pcre2_xclass.c:227-228): does record [ri]
   have Boolean property [n]? In bounds: bprops_prop is a word offset to
   the start of a ucd_boolprop_sets_item_size = 2-word item
   (pcre2_ucp.h:162-164, validated at generation) and every PT_BOOL
   property value is < ucp_bprop_count = 52 (pcre2_ucp.h:103-160), so
   n lsr 5 <= 1. *)
let boolprop_set_contains (ri : int) (n : int) : bool =
  not
    (Int.equal
       (Ucd_tables.ucd_boolprop_sets.(bprops_prop ri + (n lsr 5))
       land (1 lsl (n land 31)))
       0)

(* pcre2_internal.h:1888 — UCD_CASESET(ch): offset into
   Ucd_tables.ucd_caseless_sets of ch's multi-character caseless set, or 0
   if it has none. *)
let caseset (ch : int) : int = Ucd_tables.caseset (record_index ch)

(* pcre2_internal.h:1889 — UCD_OTHERCASE(ch): ch plus the signed
   other-case offset from its ucd_record (ch itself when there is no other
   case). *)
let othercase (ch : int) : int = ch + Ucd_tables.other_case.(record_index ch)

(* Inline sanity checks: ASCII case pairs and the caseless sets that
   compile_branch's class optimizations depend on (pcre2_compile.c:5962,
   5931-5934): k/K/s/S have multi-character caseless sets (with 0x212a
   KELVIN SIGN and 0x17f LONG S); other ASCII letters do not. *)
let () =
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
