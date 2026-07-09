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
   item list whose SINGLE/RANGE characters are complete ord2utf encodings —
   pcre2_compile.c:6383-6462), so the reads are in bounds by the
   complete-program invariant the interpreter already relies on.

   Return value: the C returns BOOL. DEVIATION (loud deferral, no
   exceptions may cross the interpreter frame loop — port-conventions §6):
   this port returns an int — 1 for TRUE, 0 for FALSE, and [prop_unported]
   (a negative sentinel) when an XCL_PROP/XCL_NOTPROP item is reached. The
   compiler defers every XCL_PROP/XCL_NOTPROP producer to M7
   (08-ucp.md), so no compiled program contains one yet; the sentinel makes
   an early arrival impossible to mistake for a match result (the
   interpreter surfaces it as its unported-arm marker). *)

(* The property items XCL_PROP/XCL_NOTPROP (pcre2_xclass.c:133-299) are
   M7 (08-ucp.md). *)
let prop_unported = -1

(* The int encoding of the C's BOOL results (see the module header). A
   module-level function: [xclass] runs inside the interpreter frame loop
   (once per subject character in the OP_XCLASS repeats), so it and its
   helpers below carry ALL state as parameters — no local closures, zero
   allocation (port-conventions §8). *)
let bool_result (b : bool) : int = if b then 1 else 0

(* The map probe at pcre2_xclass.c:88 and 91: byte c/8 of the 32-byte map
   that starts one unit past the flag unit at [data], bit c&7 — callers
   established c < 256, so c/8 <= 31 stays inside the map. *)
let map_bit (code : Bytes.t) (data : int) (c : int) : int =
  Char.code (Bytes.get code (data + 1 + (c lsr 3))) land (1 lsl (c land 7))

(* pcre2_xclass.c:101-303 — while ((t = *data++) != XCL_END) over the item
   list at [pos]. Each GETCHARINC(x, data) is the UTF decode of a pattern
   character (pcre2_intmodedep.h:312-317; in 8-bit mode utf is forced
   TRUE, pcre2_xclass.c:74-77): lead byte + GET_EXTRALEN advance. *)
let rec scan_items (c : int) (code : Bytes.t) (pos : int) (negated : bool) : int
    =
  let t = Char.code (Bytes.get code pos) in
  let pos = pos + 1 in
  if Int.equal t Opcodes.xcl_end then
    (* pcre2_xclass.c:305 — char did not match: return negated. *)
    bool_result negated
  else if Int.equal t Opcodes.xcl_single then
    (* pcre2_xclass.c:104-115 — XCL_SINGLE: GETCHARINC(x, data);
       if (c == x) return !negated. *)
    let c0 = Char.code (Bytes.get code pos) in
    let x = if c0 >= 0xc0 then Utf.getutf8_bytes c0 code pos else c0 in
    let pos = if c0 >= 0xc0 then pos + 1 + Utf.get_extralen c0 else pos + 1 in
    if Int.equal c x then bool_result (not negated)
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
    if c >= x && c <= y then bool_result (not negated)
    else (scan_items [@tailcall]) c code pos negated
  else
    (* pcre2_xclass.c:133-299 — XCL_PROP / XCL_NOTPROP: the UCD property
       switch is M7 (08-ucp.md); loud sentinel until then (see the module
       header). *)
    prop_unported

(* pcre2_xclass.c:68-306 — PRIV(xclass)(c, data, utf). The [utf] argument
   is transcribed for interface fidelity but ignored: in 8-bit mode it
   must always be TRUE (pcre2_xclass.c:74-77 forces it), so the character
   decodes always take the UTF path. *)
let xclass (c : int) (code : Bytes.t) (data : int) (_utf : bool) : int =
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
  then
    (* pcre2_xclass.c:87 — no map, no props: return negated. *)
    bool_result negated
  else if
    c < 256
    && Int.equal (flags land Opcodes.xcl_hasprop) 0
    && not (Int.equal (flags land Opcodes.xcl_map) 0)
  then
    (* pcre2_xclass.c:88 — bitmap decides outright when there are no
       property checks. *)
    bool_result (not (Int.equal (map_bit code data c) 0))
  else if
    (* pcre2_xclass.c:90-92 — with properties present, a bitmap hit for
       c < 256 is a definitive match; a miss falls through to the item
       list. *)
    c < 256
    && (not (Int.equal (flags land Opcodes.xcl_map) 0))
    && not (Int.equal (map_bit code data c) 0)
  then bool_result (not negated)
  else
    (* pcre2_xclass.c:95-99 — first skip the bit map if present. Then
       match against the list of Unicode properties or large chars or
       ranges that end with a large char. We won't ever encounter XCL_PROP
       or XCL_NOTPROP when UTF support is not compiled (they are M7
       here). *)
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
  assert (Int.equal (xclass 0x20ac b 0 true) 1);
  assert (Int.equal (xclass 0x20ad b 0 true) 0);
  assert (Int.equal (xclass 0x100 b 0 true) 1);
  assert (Int.equal (xclass 0x800 b 0 true) 1);
  assert (Int.equal (xclass 0x801 b 0 true) 0);
  (* c < 256 with no map and no props returns negated outright
     (pcre2_xclass.c:83-87) — the item list is never consulted for such
     characters, even though the range above starts below 256 (the
     compiler always emits the bitmap when a class has 8-bit
     characters). *)
  assert (Int.equal (xclass 0xe0 b 0 true) 0);
  assert (Int.equal (xclass 0x61 b 0 true) 0);
  (* ...which is TRUE for a negated class. *)
  let bn =
    mk
      [ Opcodes.xcl_not; Opcodes.xcl_single; 0xe2; 0x82; 0xac; Opcodes.xcl_end ]
  in
  assert (Int.equal (xclass 0x61 bn 0 true) 1);
  assert (Int.equal (xclass 0x20ac bn 0 true) 0);
  assert (Int.equal (xclass 0x20ad bn 0 true) 1);
  (* With a map: bitmap decides for c < 256 (bit for 'a' = 0x61 set:
     byte 12, bit 1); wide chars fall through to the items. *)
  let map = Array.make 32 0 in
  map.(0x61 lsr 3) <- 1 lsl (0x61 land 7);
  let bm =
    mk
      ([ Opcodes.xcl_map ] @ Array.to_list map
      @ [ Opcodes.xcl_single; 0xe2; 0x82; 0xac; Opcodes.xcl_end ])
  in
  assert (Int.equal (xclass 0x61 bm 0 true) 1);
  assert (Int.equal (xclass 0x62 bm 0 true) 0);
  assert (Int.equal (xclass 0x20ac bm 0 true) 1);
  (* An offset base: the data need not start at 0. *)
  let off =
    mk ([ 0x7f; 0 ] @ [ Opcodes.xcl_single; 0xe2; 0x82; 0xac; Opcodes.xcl_end ])
  in
  assert (Int.equal (xclass 0x20ac off 1 true) 1);
  assert (Int.equal (xclass 0x20ad off 1 true) 0);
  (* XCL_PROP is the loud M7 sentinel (XCL_HASPROP set, as the compiler
     emits whenever property items are present). *)
  let bp =
    mk [ Opcodes.xcl_hasprop; Opcodes.xcl_prop; 9; 0; Opcodes.xcl_end ]
  in
  assert (Int.equal (xclass 0x61 bp 0 true) prop_unported)
