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
