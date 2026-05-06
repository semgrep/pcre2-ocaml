open OUnit2

let compile_exn pattern =
  match Pcre2.Interp.compile pattern with
  | Ok re -> re
  | Error _ -> assert_failure ("compile failed: " ^ pattern)

let bin_prot_roundtrip _ctxt =
  let module CP = Pcre2_bin_prot.Compiled_pattern in
  let re = compile_exn "(\\d+)-(\\d+)" in
  let buf = Bin_prot.Utils.bin_dump CP.bin_writer_t re in
  let pos_ref = ref 0 in
  let re' = CP.bin_read_t buf ~pos_ref in
  match Pcre2.Interp.find re' "abc 12-34 def" with
  | Ok (Some m) ->
      let r = Pcre2.Interp.range_of_match m in
      assert_equal ~printer:string_of_int 4 r.Pcre2.Interp.start;
      assert_equal ~printer:string_of_int 9 r.Pcre2.Interp.end_
  | Ok None -> assert_failure "expected match"
  | Error _ -> assert_failure "match errored"

let assert_raises_any f =
  match
    try
      f ();
      None
    with e -> Some e
  with
  | Some _ -> ()
  | None -> assert_failure "expected an exception"

let bin_prot_jit_refused _ctxt =
  let module CP = Pcre2_bin_prot.Compiled_pattern in
  let jit =
    match Pcre2.Jit.compile "abc" with
    | Ok j -> j
    | Error _ -> assert_failure "jit compile failed"
  in
  (* Cast a Jit.t into Interp.t shape via Obj.magic to confirm bin_prot
     surfaces a clean Failure rather than crashing. Real callers can't trip
     this through the public API; this just exercises the failure path. *)
  let interp_view : Pcre2.Interp.t = Obj.magic jit in
  assert_raises_any (fun () ->
      ignore (Bin_prot.Utils.bin_dump CP.bin_writer_t interp_view))

let suite =
  "pcre2-bin-prot"
  >::: [
         "roundtrip" >:: bin_prot_roundtrip;
         "jit_refused" >:: bin_prot_jit_refused;
       ]

let _ = run_test_tt_main suite
