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
   values only arise from decoding invalid UTF under PCRE2_NO_UTF_CHECK
   (5/6-byte forms in Utf.getutf8), which PCRE2 documents as undefined:
   the 8-bit GET_UCD is a bare REAL_GET_UCD and indexes off the end of
   stage1 (pcre2_internal.h:1869-1873 guards only in the 32-bit library,
   where GET_UCD returns PRIV(dummy_ucd_record) — script Unknown, type
   Cn, no case set, no other case, pcre2_ucd.c:95-108). Clamping yields
   U+10FFFF's record, whose fields agree with the dummy record for every
   accessor defined here (chartype Cn, caseset 0, other_case 0 — asserted
   below), without needing a dummy row in the generated tables. *)
let record_index (ch : int) : int =
  let ch = if ch > 0x10ffff then 0x10ffff else ch in
  Ucd_tables.stage2
    (Ucd_tables.stage1 (ch / Ucd_tables.ucd_block_size)
     * Ucd_tables.ucd_block_size
    + (ch mod Ucd_tables.ucd_block_size))

(* pcre2_internal.h:1884 — UCD_CHARTYPE(ch): the particular character
   type (Ucp.ucp_cc..ucp_zs) from ch's ucd_record. *)
let chartype (ch : int) : int = Ucd_tables.chartype (record_index ch)

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
     (pcre2_ucd.c:99-108) for every accessor defined here, and values
     above the maximum read that record instead of overrunning stage1. *)
  assert (Int.equal (chartype 0x10ffff) Ucp.ucp_cn);
  assert (Int.equal (caseset 0x10ffff) 0);
  assert (Int.equal (othercase 0x10ffff) 0x10ffff);
  assert (Int.equal (chartype 0x200000) Ucp.ucp_cn);
  assert (Int.equal (caseset 0x7fffffff) 0);
  assert (Int.equal (othercase 0x200000) 0x200000)
