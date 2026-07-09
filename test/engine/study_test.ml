(* Engine module-initialization asserts for [Pcre2_engine.Study], migrated
   verbatim from src/engine/study.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Study

(* Helper-level checks on a dummy pattern block (whole-pattern checks
   live with the Compile/Interpreter assert sections, which can compile
   patterns; this module cannot depend on Compile). *)
let test_0 () =
  let mk_re () =
    {
      code = Bytes.make 8 '\000';
      name_table = Bytes.create 0;
      name_entry_size = 0;
      overall_options = 0;
      flags = 0;
      first_codeunit = 0;
      last_codeunit = 0;
      minlength = 0;
      top_backref = 0;
      start_bitmap = Bytes.make 32 '\000';
    }
  in
  let bit_set re c =
    not
      (Int.equal
         (Char.code (Bytes.get re.start_bitmap (c lsr 3))
         land (1 lsl (c land 7)))
         0)
  in
  let count_bits re =
    let n = ref 0 in
    for c = 0 to 255 do
      if bit_set re c then incr n
    done;
    !n
  in
  (* SET_BIT places each code unit at bit c&7 of byte c/8. *)
  let re = mk_re () in
  set_bit re 0x61;
  set_bit re 0xff;
  assert (bit_set re 0x61);
  assert (bit_set re 0xff);
  assert (Int.equal (count_bits re) 2);
  (* set_type_bits with cbit_digit sets exactly the ten digits (default
     tables, non-UTF limit 32). *)
  let re = mk_re () in
  set_type_bits re Chartables.cbit_digit 32;
  for c = 0x30 to 0x39 do
    assert (bit_set re c)
  done;
  assert (Int.equal (count_bits re) 10);
  (* set_nottype_bits with cbit_digit sets everything except the ten
     digits. *)
  let re = mk_re () in
  set_nottype_bits re Chartables.cbit_digit 32;
  assert (not (bit_set re 0x30));
  assert (not (bit_set re 0x39));
  assert (bit_set re 0x2f);
  assert (bit_set re 0x3a);
  assert (Int.equal (count_bits re) 246);
  (* set_type_bits in UTF mode (table_limit 16): only the \s bits below
     128 — the default C-locale tables mark no characters >= 128 as
     space, so the 128-255 leading-byte transfer loop (pcre2_study.c:
     874-885) sets nothing. *)
  let re = mk_re () in
  set_type_bits re Chartables.cbit_space 16;
  assert (bit_set re 0x20);
  assert (bit_set re 0x09);
  assert (Int.equal (count_bits re) 6 (* HT LF VT FF CR SP *));
  (* set_table_bit, caseless non-UTF/non-UCP: 'a' also sets 'A' via
     fcc. *)
  let re = mk_re () in
  Bytes.set re.code 0 'a';
  let after = set_table_bit re 0 ~caseless:true ~utf:false ~ucp:false in
  assert (Int.equal after 1);
  assert (bit_set re (Char.code 'a'));
  assert (bit_set re (Char.code 'A'));
  assert (Int.equal (count_bits re) 2);
  (* set_table_bit, caseless UTF: U+00E9 (0xc3 0xa9) sets its first byte
     and the first byte of U+00C9 (also 0xc3), consuming both code
     units. *)
  let re = mk_re () in
  Bytes.set re.code 0 '\xc3';
  Bytes.set re.code 1 '\xa9';
  let after = set_table_bit re 0 ~caseless:true ~utf:true ~ucp:false in
  assert (Int.equal after 2);
  assert (bit_set re 0xc3);
  assert (Int.equal (count_bits re) 1);
  (* HSPACE/VSPACE bit sets, non-UTF vs UTF. *)
  let re = mk_re () in
  set_hspace_bits re ~utf:false;
  assert (bit_set re 0x09 && bit_set re 0x20 && bit_set re 0xa0);
  assert (Int.equal (count_bits re) 3);
  let re = mk_re () in
  set_vspace_bits re ~utf:true;
  assert (
    bit_set re 0x0a && bit_set re 0x0b && bit_set re 0x0c && bit_set re 0x0d
    && bit_set re 0xc2 && bit_set re 0xe2);
  assert (Int.equal (count_bits re) 6)

let tests = [ Alcotest.test_case "study 0" `Quick test_0 ]
