(* UCD record access: the GET_UCD macro family from pcre2_internal.h over
   the generated Ucd_tables (docs/ocaml-engine/00-architecture.md maps this
   module to those macros). Only the accessors needed so far are defined;
   M6 (UTF) and M7 (UCP) chunks extend this module with the remaining
   UCD_* lookups (script, chartype, gbprop, ...). *)

(* pcre2_internal.h:1864-1867 — REAL_GET_UCD(ch): the ucd_record for code
   point ch is
     ucd_records[ucd_stage2[ucd_stage1[ch / UCD_BLOCK_SIZE] * UCD_BLOCK_SIZE
                            + ch % UCD_BLOCK_SIZE]]
   returned here as the record index into the per-field tables of
   Ucd_tables. pcre2_internal.h:1869-1873: GET_UCD guards against
   ch > MAX_UTF_CODE_POINT only in the 32-bit library; in the 8-bit library
   GET_UCD(ch) = REAL_GET_UCD(ch), and every caller passes a code point
   <= 0x10ffff (enforced at parse time), so ch / 128 <= 8703 = the last
   stage1 index. *)
let record_index (ch : int) : int =
  Ucd_tables.stage2
    (Ucd_tables.stage1 (ch / Ucd_tables.ucd_block_size)
     * Ucd_tables.ucd_block_size
    + (ch mod Ucd_tables.ucd_block_size))

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
  assert (Int.equal Ucd_tables.ucd_caseless_sets.(ks + 3) Tables.notachar)
