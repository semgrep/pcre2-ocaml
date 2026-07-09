(* Engine module-initialization asserts for [Pcre2_engine.Extuni], migrated
   verbatim from src/engine/extuni.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Extuni

(* Cluster boundaries pinned against pcre2test 10.44 `/\X/g,aftertext`
   oracle runs (and UAX #29 rules GB3-GB13). [cluster_end_at] fetches the
   first character exactly like the interpreter's GETCHARINCTEST + extuni
   call sites (pcre2_match.c:2629-2631). *)
let test_0 () =
  let buf = Bytes.make 64 '\000' in
  let enc (cs : int list) : string =
    let p = ref 0 in
    List.iter (fun c -> p := !p + Utf.ord2utf c buf !p) cs;
    Bytes.sub_string buf 0 !p
  in
  let cluster_end_at (s : string) (pos : int) (utf : bool) : int =
    let c0 = Char.code s.[pos] in
    let c = if utf && c0 >= 0xc0 then Utf.getutf8 c0 s pos else c0 in
    let ep =
      if utf && c0 >= 0xc0 then pos + 1 + Utf.get_extralen c0 else pos + 1
    in
    extuni c s ep 0 (String.length s) utf
  in
  let cluster_end (cs : int list) : int = cluster_end_at (enc cs) 0 true in
  (* Property pins for the code points used below (UCD 15.0.0, the
     vendored tables' version). *)
  assert (Int.equal (Ucd.gbprop 0x301) Ucp.ucp_gb_extend);
  assert (Int.equal (Ucd.gbprop 0x1100) Ucp.ucp_gb_l);
  assert (Int.equal (Ucd.gbprop 0x1161) Ucp.ucp_gb_v);
  assert (Int.equal (Ucd.gbprop 0x11a8) Ucp.ucp_gb_t);
  assert (Int.equal (Ucd.gbprop 0x1f600) Ucp.ucp_gb_extended_pictographic);
  (* GB9: e + COMBINING ACUTE (U+0301) is one cluster ("é"); a following
     base character starts a new one. *)
  assert (Int.equal (cluster_end [ 0x65; 0x301 ]) 3);
  assert (Int.equal (cluster_end [ 0x65; 0x301; 0x7a ]) 3);
  (* GB3/GB4: CRLF is one cluster; LF CR is two. *)
  assert (Int.equal (cluster_end [ 0x0d; 0x0a; 0x61 ]) 2);
  assert (Int.equal (cluster_end [ 0x0a; 0x0d ]) 1);
  (* GB6-GB8: Hangul jamo L V T form one cluster. *)
  assert (Int.equal (cluster_end [ 0x1100; 0x1161; 0x11a8 ]) 9);
  (* GB12/GB13: two Regional Indicators pair into one cluster; a third
     starts a new cluster (2 RIs = 1 cluster, 3 RIs = 2 clusters); an RI
     at an odd position in a run pairs with the one after it, not the one
     before. *)
  assert (Int.equal (cluster_end [ 0x1f1e6; 0x1f1e7 ]) 8);
  assert (Int.equal (cluster_end [ 0x1f1e6; 0x1f1e7; 0x1f1e8 ]) 8);
  assert (
    Int.equal (cluster_end_at (enc [ 0x1f1e6; 0x1f1e7; 0x1f1e8 ]) 8 true) 12);
  (* RI#2 of a run: one preceding RI (odd) forces a break after it. *)
  assert (
    Int.equal (cluster_end_at (enc [ 0x1f1e6; 0x1f1e7; 0x1f1e8 ]) 4 true) 8);
  (* GB11: EP + ZWJ + EP is one cluster, also with Extends after the first
     EP; a ZWJ + EP without a preceding EP breaks before the EP. *)
  assert (Int.equal (cluster_end [ 0x1f600; 0x200d; 0x1f600 ]) 11);
  assert (Int.equal (cluster_end [ 0x1f600; 0x301; 0x200d; 0x1f600 ]) 13);
  assert (Int.equal (cluster_end [ 0x200d; 0x1f600 ]) 3);
  (* Non-UTF mode steps one code unit at a time: CRLF still clusters;
     'a' + 0xcc (in Latin-1, a lead byte here) does not. *)
  assert (Int.equal (cluster_end_at "\x0d\x0a" 0 false) 2);
  assert (Int.equal (cluster_end_at "a\xcc" 0 false) 1)

let tests = [ Alcotest.test_case "extuni 0" `Quick test_0 ]
