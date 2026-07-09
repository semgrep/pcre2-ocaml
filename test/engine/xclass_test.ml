(* Engine module-initialization asserts for [Pcre2_engine.Xclass], migrated
   verbatim from src/engine/xclass.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Xclass

(* Hand-assembled XCLASS data blocks (the layout pcre2_compile.c:6436-6457
   emits: flag unit, optional 32-byte map, items, XCL_END), checked against
   the pcre2_xclass.c semantics. *)
let test_0 () =
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

let tests = [ Alcotest.test_case "xclass 0" `Quick test_0 ]
