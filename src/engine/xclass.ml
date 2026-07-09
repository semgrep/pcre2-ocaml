(* Extended-class runtime matcher for the pure-OCaml PCRE2 10.44 port
   (8-bit library).

   Ported from vendor/pcre2/src/pcre2_xclass.c:53-306 — PRIV(xclass): match
   a character against an extended class that might contain codepoints
   above 255 and/or Unicode properties. Used by pcre2_match()'s OP_XCLASS
   arm (and later by pcre2_auto_possessify, M9).

   Pointer mapping: the C's `PCRE2_SPTR data` (points to the flag code unit
   of the XCLASS data inside the compiled program) becomes the pair of the
   code [Bytes.t] and an int offset [data]. All reads use [Bytes.get]:
   every offset touched lies inside the OP_XCLASS item of a complete
   compiled program (flag unit, optional 32-byte map, XCL_END-terminated
   item list whose SINGLE/RANGE characters are complete ord2utf encodings
   and whose PROP/NOTPROP items carry two property code units —
   pcre2_compile.c:6383-6462), so the reads are in bounds by the
   complete-program invariant the interpreter already relies on.

   Return value: the C's BOOL as an OCaml bool. [xclass] and its helpers
   run inside the interpreter frame loop (once per subject character in
   the OP_XCLASS repeats), so they are module-level functions carrying ALL
   state as parameters — no local closures, zero allocation
   (port-conventions §8). *)

(* pcre2_internal.h:400-431 — HSPACE_CASES (byte + multibyte), for the
   PT_SPACE/PT_PXSPACE switch on a decoded character. *)
let hspace_char (c : int) : bool =
  match c with
  | 0x09 | 0x20 | 0xa0 | 0x1680 | 0x180e | 0x2000 | 0x2001 | 0x2002 | 0x2003
  | 0x2004 | 0x2005 | 0x2006 | 0x2007 | 0x2008 | 0x2009 | 0x200a | 0x202f
  | 0x205f | 0x3000 ->
      true
  | _ -> false

(* pcre2_internal.h:433-449 — VSPACE_CASES (byte + multibyte). *)
let vspace_char (c : int) : bool =
  match c with
  | 0x0a | 0x0b | 0x0c | 0x0d | 0x85 | 0x2028 | 0x2029 -> true
  | _ -> false

(* The map probe at pcre2_xclass.c:88 and 91: byte c/8 of the 32-byte map
   that starts one unit past the flag unit at [data], bit c&7 — callers
   established c < 256, so c/8 <= 31 stays inside the map. *)
let map_bit (code : Bytes.t) (data : int) (c : int) : int =
  Char.code (Bytes.get code (data + 1 + (c lsr 3))) land (1 lsl (c land 7))

(* pcre2_xclass.c:141-296 — the XCL_PROP/XCL_NOTPROP switch( *data ) over
   the property type at [ptype] with value [pdata], for the ucd_record
   [ri] (the C's `const ucd_record *prop = GET_UCD(c)`, 137). Returns
   1 when the character has the property, 0 when it does not, and -1 for
   the C's default case — a property type never compiled into an XCLASS
   (294-295: `return FALSE` for the whole class). Cases in the C's
   order. *)
let prop_case (c : int) (ri : int) (ptype : int) (pdata : int) : int =
  if Int.equal ptype Opcodes.pt_any then 1 (* 143-145 *)
  else if Int.equal ptype Opcodes.pt_lamp then
    (* pcre2_xclass.c:147-151 *)
    let chartype = Ucd_tables.chartype ri in
    if
      Int.equal chartype Ucp.ucp_lu
      || Int.equal chartype Ucp.ucp_ll
      || Int.equal chartype Ucp.ucp_lt
    then 1
    else 0
  else if Int.equal ptype Opcodes.pt_gc then
    (* pcre2_xclass.c:153-156 — data[1] == PRIV(ucp_gentype)[chartype]. *)
    if Int.equal pdata Tables.ucp_gentype.(Ucd_tables.chartype ri) then 1 else 0
  else if Int.equal ptype Opcodes.pt_pc then
    (* pcre2_xclass.c:158-160 *)
    if Int.equal pdata (Ucd_tables.chartype ri) then 1 else 0
  else if Int.equal ptype Opcodes.pt_sc then
    (* pcre2_xclass.c:162-164 *)
    if Int.equal pdata (Ucd_tables.script ri) then 1 else 0
  else if Int.equal ptype Opcodes.pt_scx then
    (* pcre2_xclass.c:166-170 — script match, or the Script Extensions
       set bit (MAPBIT bound proof: Ucd.script_set_contains). *)
    if
      Int.equal pdata (Ucd_tables.script ri) || Ucd.script_set_contains ri pdata
    then 1
    else 0
  else if Int.equal ptype Opcodes.pt_alnum then
    (* pcre2_xclass.c:172-177 *)
    let gentype = Tables.ucp_gentype.(Ucd_tables.chartype ri) in
    if Int.equal gentype Ucp.ucp_l || Int.equal gentype Ucp.ucp_n then 1 else 0
  else if Int.equal ptype Opcodes.pt_space || Int.equal ptype Opcodes.pt_pxspace
  then
    (* pcre2_xclass.c:179-197 — Perl space and POSIX space are identical
       since Perl 5.18 / PCRE 8.34: HSPACE/VSPACE cases, else general
       category Z. *)
    if hspace_char c || vspace_char c then 1
    else if Int.equal Tables.ucp_gentype.(Ucd_tables.chartype ri) Ucp.ucp_z then
      1
    else 0
  else if Int.equal ptype Opcodes.pt_word then
    (* pcre2_xclass.c:199-205 *)
    let chartype = Ucd_tables.chartype ri in
    let gentype = Tables.ucp_gentype.(chartype) in
    if
      Int.equal gentype Ucp.ucp_l
      || Int.equal gentype Ucp.ucp_n
      || Int.equal chartype Ucp.ucp_mn
      || Int.equal chartype Ucp.ucp_pc
    then 1
    else 0
  else if Int.equal ptype Opcodes.pt_ucnc then
    (* pcre2_xclass.c:207-219 — CHAR_DOLLAR_SIGN 0x24, CHAR_COMMERCIAL_AT
       0x40, CHAR_GRAVE_ACCENT 0x60. *)
    if c < 0xa0 then
      if Int.equal c 0x24 || Int.equal c 0x40 || Int.equal c 0x60 then 1 else 0
    else if c < 0xd800 || c > 0xdfff then 1
    else 0
  else if Int.equal ptype Opcodes.pt_bidicl then
    (* pcre2_xclass.c:221-224 — UCD_BIDICLASS_PROP(prop) == data[1]. *)
    if Int.equal (Ucd.bidiclass_prop ri) pdata then 1 else 0
  else if Int.equal ptype Opcodes.pt_bool then
    (* pcre2_xclass.c:226-230 — MAPBIT over the Boolean-property set
       (bound proof: Ucd.boolprop_set_contains). *)
    if Ucd.boolprop_set_contains ri pdata then 1 else 0
  else if Int.equal ptype Opcodes.pt_pxgraph then
    (* pcre2_xclass.c:232-252 — [:graph:]: not Z and not C, except Cf
       outside U+061C, U+180E, U+2066-U+2069. *)
    let chartype = Ucd_tables.chartype ri in
    if
      (not (Int.equal Tables.ucp_gentype.(chartype) Ucp.ucp_z))
      && ((not (Int.equal Tables.ucp_gentype.(chartype) Ucp.ucp_c))
         || Int.equal chartype Ucp.ucp_cf
            && (not (Int.equal c 0x061c))
            && (not (Int.equal c 0x180e))
            && (c < 0x2066 || c > 0x2069))
    then 1
    else 0
  else if Int.equal ptype Opcodes.pt_pxprint then
    (* pcre2_xclass.c:254-266 — [:print:]: as graphic plus Zs and
       U+180E. *)
    let chartype = Ucd_tables.chartype ri in
    if
      (not (Int.equal chartype Ucp.ucp_zl))
      && (not (Int.equal chartype Ucp.ucp_zp))
      && ((not (Int.equal Tables.ucp_gentype.(chartype) Ucp.ucp_c))
         || Int.equal chartype Ucp.ucp_cf
            && (not (Int.equal c 0x061c))
            && (c < 0x2066 || c > 0x2069))
    then 1
    else 0
  else if Int.equal ptype Opcodes.pt_pxpunct then
    (* pcre2_xclass.c:268-277 — [:punct:]: Unicode P, plus ASCII
       characters in S. *)
    let gentype = Tables.ucp_gentype.(Ucd_tables.chartype ri) in
    if Int.equal gentype Ucp.ucp_p || (c < 128 && Int.equal gentype Ucp.ucp_s)
    then 1
    else 0
  else if Int.equal ptype Opcodes.pt_pxxdigit then
    (* pcre2_xclass.c:279-289 — [:xdigit:]: ASCII hex digits plus the
       fullwidth forms. *)
    if
      (c >= Char.code '0' && c <= Char.code '9')
      || (c >= Char.code 'A' && c <= Char.code 'F')
      || (c >= Char.code 'a' && c <= Char.code 'f')
      || (c >= 0xff10 && c <= 0xff19)
      || (c >= 0xff21 && c <= 0xff26)
      || (c >= 0xff41 && c <= 0xff46)
    then 1
    else 0
  else -1 (* pcre2_xclass.c:291-295 — default: return FALSE. *)

(* pcre2_xclass.c:101-303 — while ((t = *data++) != XCL_END) over the item
   list at [pos]. Each GETCHARINC(x, data) is the UTF decode of a pattern
   character (pcre2_intmodedep.h:312-317; in 8-bit mode utf is forced
   TRUE, pcre2_xclass.c:74-77): lead byte + GET_EXTRALEN advance. *)
let rec scan_items (c : int) (code : Bytes.t) (pos : int) (negated : bool) :
    bool =
  let t = Char.code (Bytes.get code pos) in
  let pos = pos + 1 in
  if Int.equal t Opcodes.xcl_end then
    (* pcre2_xclass.c:305 — char did not match: return negated. *)
    negated
  else if Int.equal t Opcodes.xcl_single then
    (* pcre2_xclass.c:104-115 — XCL_SINGLE: GETCHARINC(x, data);
       if (c == x) return !negated. *)
    let c0 = Char.code (Bytes.get code pos) in
    let x = if c0 >= 0xc0 then Utf.getutf8_bytes c0 code pos else c0 in
    let pos = if c0 >= 0xc0 then pos + 1 + Utf.get_extralen c0 else pos + 1 in
    if Int.equal c x then not negated
    else (scan_items [@tailcall]) c code pos negated
  else if Int.equal t Opcodes.xcl_range then
    (* pcre2_xclass.c:116-131 — XCL_RANGE: GETCHARINC(x, data);
       GETCHARINC(y, data); if (c >= x && c <= y) return !negated. *)
    let c0 = Char.code (Bytes.get code pos) in
    let x = if c0 >= 0xc0 then Utf.getutf8_bytes c0 code pos else c0 in
    let pos = if c0 >= 0xc0 then pos + 1 + Utf.get_extralen c0 else pos + 1 in
    let c1 = Char.code (Bytes.get code pos) in
    let y = if c1 >= 0xc0 then Utf.getutf8_bytes c1 code pos else c1 in
    let pos = if c1 >= 0xc0 then pos + 1 + Utf.get_extralen c1 else pos + 1 in
    if c >= x && c <= y then not negated
    else (scan_items [@tailcall]) c code pos negated
  else
    (* pcre2_xclass.c:133-299 — XCL_PROP / XCL_NOTPROP: [pos] holds the
       property type, [pos + 1] its value; data += 2 after the switch
       (298). A case whose property test equals isprop returns !negated;
       otherwise scanning continues past the item. The switch fetches the
       ucd_record once (GET_UCD(c), 137). *)
    let isprop = Int.equal t Opcodes.xcl_prop in
    let ptype = Char.code (Bytes.get code pos) in
    let pdata = Char.code (Bytes.get code (pos + 1)) in
    let ok = prop_case c (Ucd.record_index c) ptype pdata in
    if ok < 0 then false (* the switch default: return FALSE (294-295) *)
    else if Bool.equal (Int.equal ok 1) isprop then not negated
    else (scan_items [@tailcall]) c code (pos + 2) negated

(* pcre2_xclass.c:68-306 — PRIV(xclass)(c, data, utf). The [utf] argument
   is transcribed for interface fidelity but ignored: in 8-bit mode it
   must always be TRUE (pcre2_xclass.c:74-77 forces it), so the character
   decodes always take the UTF path. *)
let xclass (c : int) (code : Bytes.t) (data : int) (_utf : bool) : bool =
  (* pcre2_xclass.c:71-72 — BOOL negated = ( *data & XCL_NOT) != 0. *)
  let flags = Char.code (Bytes.get code data) in
  let negated = not (Int.equal (flags land Opcodes.xcl_not) 0) in
  (* pcre2_xclass.c:79-93 — code points < 256 are matched against a
     bitmap, if one is present. If not, we still carry on, because there
     may be ranges that start below 256 in the additional data. *)
  if
    c < 256
    && Int.equal (flags land Opcodes.xcl_hasprop) 0
    && Int.equal (flags land Opcodes.xcl_map) 0
  then (* pcre2_xclass.c:87 — no map, no props: return negated. *)
    negated
  else if
    c < 256
    && Int.equal (flags land Opcodes.xcl_hasprop) 0
    && not (Int.equal (flags land Opcodes.xcl_map) 0)
  then
    (* pcre2_xclass.c:88 — bitmap decides outright when there are no
       property checks. *)
    not (Int.equal (map_bit code data c) 0)
  else if
    (* pcre2_xclass.c:90-92 — with properties present, a bitmap hit for
       c < 256 is a definitive match; a miss falls through to the item
       list. *)
    c < 256
    && (not (Int.equal (flags land Opcodes.xcl_map) 0))
    && not (Int.equal (map_bit code data c) 0)
  then not negated
  else
    (* pcre2_xclass.c:95-99 — first skip the bit map if present. Then
       match against the list of Unicode properties or large chars or
       ranges that end with a large char. *)
    let pos =
      if not (Int.equal (flags land Opcodes.xcl_map) 0) then data + 1 + 32
      else data + 1
    in
    (scan_items [@tailcall]) c code pos negated

(* ---------- Inline sanity checks (module-initialization asserts) ---------- *)

(* Hand-assembled XCLASS data blocks (the layout pcre2_compile.c:6436-6457
   emits: flag unit, optional 32-byte map, items, XCL_END), checked against
   the pcre2_xclass.c semantics. *)
let () =
  let mk (units : int list) : Bytes.t =
    let b = Bytes.create (List.length units) in
    List.iteri (fun i u -> Bytes.set b i (Char.chr u)) units;
    b
  in
  (* No map, positive class, single wide char U+20AC (E2 82 AC) and range
     U+00E0..U+0800 (C3 A0 .. E0 A0 80). *)
  let b =
    mk
      ([ 0; Opcodes.xcl_single; 0xe2; 0x82; 0xac; Opcodes.xcl_range ]
      @ [ 0xc3; 0xa0; 0xe0; 0xa0; 0x80; Opcodes.xcl_end ])
  in
  assert (xclass 0x20ac b 0 true);
  assert (not (xclass 0x20ad b 0 true));
  assert (xclass 0x100 b 0 true);
  assert (xclass 0x800 b 0 true);
  assert (not (xclass 0x801 b 0 true));
  (* c < 256 with no map and no props returns negated outright
     (pcre2_xclass.c:83-87) — the item list is never consulted for such
     characters, even though the range above starts below 256 (the
     compiler always emits the bitmap when a class has 8-bit
     characters). *)
  assert (not (xclass 0xe0 b 0 true));
  assert (not (xclass 0x61 b 0 true));
  (* ...which is TRUE for a negated class. *)
  let bn =
    mk
      [ Opcodes.xcl_not; Opcodes.xcl_single; 0xe2; 0x82; 0xac; Opcodes.xcl_end ]
  in
  assert (xclass 0x61 bn 0 true);
  assert (not (xclass 0x20ac bn 0 true));
  assert (xclass 0x20ad bn 0 true);
  (* With a map: bitmap decides for c < 256 (bit for 'a' = 0x61 set:
     byte 12, bit 1); wide chars fall through to the items. *)
  let map = Array.make 32 0 in
  map.(0x61 lsr 3) <- 1 lsl (0x61 land 7);
  let bm =
    mk
      ([ Opcodes.xcl_map ] @ Array.to_list map
      @ [ Opcodes.xcl_single; 0xe2; 0x82; 0xac; Opcodes.xcl_end ])
  in
  assert (xclass 0x61 bm 0 true);
  assert (not (xclass 0x62 bm 0 true));
  assert (xclass 0x20ac bm 0 true);
  (* An offset base: the data need not start at 0. *)
  let off =
    mk ([ 0x7f; 0 ] @ [ Opcodes.xcl_single; 0xe2; 0x82; 0xac; Opcodes.xcl_end ])
  in
  assert (xclass 0x20ac off 1 true);
  assert (not (xclass 0x20ad off 1 true));
  (* XCL_PROP items (XCL_HASPROP set, as the compiler emits whenever
     property items are present): \p{L} shapes — GREEK SMALL ALPHA is Lu?
     no: Ll, gentype L; '!' is not. c < 256 WITH XCL_HASPROP consults the
     items (pcre2_xclass.c:83-99). *)
  let bp =
    mk
      [
        Opcodes.xcl_hasprop;
        Opcodes.xcl_prop;
        Opcodes.pt_gc;
        Ucp.ucp_l;
        Opcodes.xcl_end;
      ]
  in
  assert (xclass 0x3b1 bp 0 true);
  assert (xclass 0x61 bp 0 true);
  assert (not (xclass 0x21 bp 0 true));
  (* XCL_NOTPROP inverts the item, XCL_NOT the class. *)
  let bnp =
    mk
      [
        Opcodes.xcl_hasprop;
        Opcodes.xcl_notprop;
        Opcodes.pt_gc;
        Ucp.ucp_l;
        Opcodes.xcl_end;
      ]
  in
  assert (not (xclass 0x3b1 bnp 0 true));
  assert (xclass 0x21 bnp 0 true);
  let bnn =
    mk
      [
        Opcodes.xcl_not lor Opcodes.xcl_hasprop;
        Opcodes.xcl_prop;
        Opcodes.pt_gc;
        Ucp.ucp_l;
        Opcodes.xcl_end;
      ]
  in
  assert (not (xclass 0x3b1 bnn 0 true));
  assert (xclass 0x21 bnn 0 true);
  (* PT_SCX inside a class: U+0964 has Devanagari in its Script
     Extensions though its script is Common. *)
  let bscx =
    mk
      [
        Opcodes.xcl_hasprop;
        Opcodes.xcl_prop;
        Opcodes.pt_scx;
        Ucp.ucp_devanagari;
        Opcodes.xcl_end;
      ]
  in
  assert (xclass 0x964 bscx 0 true);
  assert (xclass 0x915 bscx 0 true);
  assert (not (xclass 0x3b1 bscx 0 true));
  (* The POSIX-only forms: PT_PXPUNCT includes ASCII '$' (an S character)
     but not the non-ASCII currency sign U+20AC; PT_PXXDIGIT takes the
     fullwidth digit U+FF10. *)
  let bpunct =
    mk
      [
        Opcodes.xcl_hasprop;
        Opcodes.xcl_prop;
        Opcodes.pt_pxpunct;
        0;
        Opcodes.xcl_end;
      ]
  in
  assert (xclass 0x24 bpunct 0 true);
  assert (xclass 0x2c bpunct 0 true);
  assert (not (xclass 0x20ac bpunct 0 true));
  let bxdig =
    mk
      [
        Opcodes.xcl_hasprop;
        Opcodes.xcl_prop;
        Opcodes.pt_pxxdigit;
        0;
        Opcodes.xcl_end;
      ]
  in
  assert (xclass (Char.code 'f') bxdig 0 true);
  assert (not (xclass (Char.code 'g') bxdig 0 true));
  assert (xclass 0xff10 bxdig 0 true)
