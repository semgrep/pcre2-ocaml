(* pcre2_tables.c - fixed tables used by more than one module (PCRE2 10.44).
   Hand-ported: only the non-generated parts. Skipped here:
   - OP_LENGTHS / PRIV(OP_lengths) (pcre2_tables.c:60) -> belongs to opcodes.ml;
   - callout string delimiters (pcre2_tables.c:73-81) -> callouts are out of
     scope (type-only, never implemented);
   - PRIV(ucp_typerange) (pcre2_tables.c:216-224) -> JIT-only;
   - the #included generated UCD/UCP tables (pcre2_tables.c:230) -> emitted by
     gen/gen_tables.exe into ucd_tables.ml / ucptables.ml. *)

(* pcre2_internal.h:223 - terminates the [hv]space and caseless-set lists. *)
let notachar = 0xffffffff

(* pcre2_tables.c:100-101 - breakpoints for different numbers of bytes in a
   UTF-8 character. *)
let utf8_table1 = [| 0x7f; 0x7ff; 0xffff; 0x1fffff; 0x3ffffff; 0x7fffffff |]

(* pcre2_tables.c:103 - sizeof(utf8_table1)/sizeof(int): the maximum number
   of code units per UTF-8 character, as used by ord2utf
   (pcre2_ord2utf.c:87). *)
let utf8_table1_size = Array.length utf8_table1

(* pcre2_tables.c:108-109 - indicator bits (table2) and data-bit masks
   (table3) for the first byte of a character, indexed by the number of
   additional bytes. *)
let utf8_table2 = [| 0; 0xc0; 0xe0; 0xf0; 0xf8; 0xfc |]
let utf8_table3 = [| 0xff; 0x1f; 0x0f; 0x07; 0x03; 0x01 |]

(* pcre2_tables.c:114-118 - number of extra bytes, indexed by the first byte
   masked with 0x3f (the highest valid UTF-8 first byte masks to 0x3d). *)
let utf8_table4 =
  [|
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    1;
    2;
    2;
    2;
    2;
    2;
    2;
    2;
    2;
    2;
    2;
    2;
    2;
    2;
    2;
    2;
    2;
    3;
    3;
    3;
    3;
    3;
    3;
    3;
    3;
    4;
    4;
    4;
    4;
    5;
    5;
    5;
    5;
  |]

(* pcre2_tables.c:66 = HSPACE_LIST (pcre2_internal.h:400-404): horizontal
   whitespace, ascending, NOTACHAR-terminated. CHAR_HT = 0x09, CHAR_SPACE =
   0x20, CHAR_NBSP = 0xa0 (pcre2_internal.h:696,703,683). *)
let hspace_list =
  [|
    0x09;
    0x20;
    0xa0;
    0x1680;
    0x180e;
    0x2000;
    0x2001;
    0x2002;
    0x2003;
    0x2004;
    0x2005;
    0x2006;
    0x2007;
    0x2008;
    0x2009;
    0x200a;
    0x202f;
    0x205f;
    0x3000;
    notachar;
  |]

(* pcre2_tables.c:67 = VSPACE_LIST (pcre2_internal.h:433-434): vertical
   whitespace, ascending, NOTACHAR-terminated. CHAR_LF = 0x0a, CHAR_VT =
   0x0b, CHAR_FF = 0x0c, CHAR_CR = 0x0d, CHAR_NEL = 0x85
   (pcre2_internal.h:678,697-699,680). *)
let vspace_list = [| 0x0a; 0x0b; 0x0c; 0x0d; 0x85; 0x2028; 0x2029; notachar |]

(* pcre2_tables.c:130-139 - PRIV(ucp_gentype): particular character type
   (ucp_cc..ucp_zs) -> general category (ucp_c..ucp_z). *)
let ucp_gentype =
  [|
    Ucp.ucp_c;
    Ucp.ucp_c;
    Ucp.ucp_c;
    Ucp.ucp_c;
    Ucp.ucp_c;
    (* Cc, Cf, Cn, Co, Cs *)
    Ucp.ucp_l;
    Ucp.ucp_l;
    Ucp.ucp_l;
    Ucp.ucp_l;
    Ucp.ucp_l;
    (* Ll, Lu, Lm, Lo, Lt *)
    Ucp.ucp_m;
    Ucp.ucp_m;
    Ucp.ucp_m;
    (* Mc, Me, Mn *)
    Ucp.ucp_n;
    Ucp.ucp_n;
    Ucp.ucp_n;
    (* Nd, Nl, No *)
    Ucp.ucp_p;
    Ucp.ucp_p;
    Ucp.ucp_p;
    Ucp.ucp_p;
    Ucp.ucp_p;
    (* Pc, Pd, Pe, Pf, Pi *)
    Ucp.ucp_p;
    Ucp.ucp_p;
    (* Ps, Po *)
    Ucp.ucp_s;
    Ucp.ucp_s;
    Ucp.ucp_s;
    Ucp.ucp_s;
    (* Sc, Sk, Sm, So *)
    Ucp.ucp_z;
    Ucp.ucp_z;
    Ucp.ucp_z;
    (* Zl, Zp, Zs *)
  |]

(* pcre2_tables.c:141-208 - PRIV(ucp_gbtable): the extended grapheme cluster
   pair table, indexed by the grapheme break properties of two adjacent code
   points:

     ucp_gbtable.(left_prop) land (1 lsl right_prop) <> 0

   means a break is NOT permitted between them. Extend chars inside emoji
   zwj sequences (rule 7) and counting regional indicators (rule 8) cannot
   be represented here; the code has to deal with them. *)
let ucp_gbtable =
  (* pcre2_tables.c:186 - ESZ, #undef'd after the table in the C. *)
  let esz =
    (1 lsl Ucp.ucp_gb_extend)
    lor (1 lsl Ucp.ucp_gb_spacing_mark)
    lor (1 lsl Ucp.ucp_gb_zwj)
  in
  [|
    1 lsl Ucp.ucp_gb_lf;
    (*  0 CR *)
    0;
    (*  1 LF *)
    0;
    (*  2 Control *)
    esz;
    (*  3 Extend *)
    esz (*  4 Prepend *) lor (1 lsl Ucp.ucp_gb_prepend)
    lor (1 lsl Ucp.ucp_gb_l) lor (1 lsl Ucp.ucp_gb_v) lor (1 lsl Ucp.ucp_gb_t)
    lor (1 lsl Ucp.ucp_gb_lv) lor (1 lsl Ucp.ucp_gb_lvt)
    lor (1 lsl Ucp.ucp_gb_other)
    lor (1 lsl Ucp.ucp_gb_regional_indicator);
    esz;
    (*  5 SpacingMark *)
    esz (*  6 L *) lor (1 lsl Ucp.ucp_gb_l)
    lor (1 lsl Ucp.ucp_gb_v) lor (1 lsl Ucp.ucp_gb_lv) lor (1 lsl Ucp.ucp_gb_lvt);
    esz lor (1 lsl Ucp.ucp_gb_v) lor (1 lsl Ucp.ucp_gb_t);
    (*  7 V *)
    esz lor (1 lsl Ucp.ucp_gb_t);
    (*  8 T *)
    esz lor (1 lsl Ucp.ucp_gb_v) lor (1 lsl Ucp.ucp_gb_t);
    (*  9 LV *)
    esz lor (1 lsl Ucp.ucp_gb_t);
    (* 10 LVT *)
    1 lsl Ucp.ucp_gb_regional_indicator;
    (* 11 Regional Indicator *)
    esz;
    (* 12 Other *)
    esz lor (1 lsl Ucp.ucp_gb_extended_pictographic);
    (* 13 ZWJ *)
    esz;
    (* 14 Extended Pictographic *)
  |]
