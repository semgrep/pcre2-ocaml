open OUnit2
open! Pcre2

let ( >+= ) x f =
  match x with
  | Ok (Some x) -> Ok (Some (f x))
  | Ok None -> Ok None
  | Error e -> Error e

let ( >>= ) x f =
  match x with
  | Ok (Some x) -> Ok (f x)
  | Ok None -> Ok None
  | Error e -> Error e

module MakeTests
    (M :
      Pcre2.Matcher
        with type compile_error = compile_error
         and type match_error = match_error) : sig
  val tests : test list
end = struct
  open M

  let simple_test ctxt =
    match compile "abc" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 3 }))
          (find re "abc" >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 3; end_ = 6 }))
          (find re "123abc456" >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 3; end_ = 6 }))
          (find re "123abc" >+= range_of_match);
        assert_equal ~printer (Ok None) (find re "123ac" >+= range_of_match)

  let simple_captures ctxt =
    match compile "(a)(b)(c)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        let c = captures re "abc" in
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 3 }))
          (c >+= range_of_captures);
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 1 }))
          (c >>= (fun c -> match_of_captures c 1) >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 1; end_ = 2 }))
          (c >>= (fun c -> match_of_captures c 2) >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 2; end_ = 3 }))
          (c >>= (fun c -> match_of_captures c 3) >+= range_of_match);
        assert_equal ~printer (Ok None)
          (c >>= (fun c -> match_of_captures c 4) >+= range_of_match)

  let non_contiguous_capture ctxt =
    match compile "(a)(?:(b)|(c))" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        let c = captures re "ac" in
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 2 }))
          (c >>= (fun c -> match_of_captures c 0) >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 1 }))
          (c >>= (fun c -> match_of_captures c 1) >+= range_of_match);
        assert_equal ~printer (Ok None)
          (c >>= (fun c -> match_of_captures c 2) >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 1; end_ = 2 }))
          (c >>= (fun c -> match_of_captures c 3) >+= range_of_match);
        assert_equal ~printer (Ok None)
          (c >>= (fun c -> match_of_captures c 4) >+= range_of_match)

  let non_contiguous_named_capture ctxt =
    match compile "(?<A>a)(?:(?<B>b)|(?<C>c))" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        let c = captures re "ac" in
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 1 }))
          (c >>= (fun c -> named_match_of_captures c "A") >+= range_of_match);
        assert_equal ~printer (Ok None)
          (c >>= (fun c -> named_match_of_captures c "B") >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 1; end_ = 2 }))
          (c >>= (fun c -> named_match_of_captures c "C") >+= range_of_match)

  let bad_pattern ctxt =
    match compile "ab(" with
    | Error MISSING_CLOSING_PARENTHESIS -> ()
    | Error e ->
        assert_failure ("Incorrectly error for pattern: " ^ show_compile_error e)
    | Ok _ -> assert_failure "Incorrectly compiled invalid pattern"

  let bad_offset ctxt =
    match compile "abc" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        assert_equal ~printer ~msg:"Negative offset" (Error BADOFFSET)
          (find ~subject_offset:(-1) re "ab" >+= range_of_match);
        assert_equal ~printer ~msg:"Offset too large" (Error BADOFFSET)
          (find ~subject_offset:10 re "123abc456" >+= range_of_match)

  let split_comma ctxt =
    match compile "," with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (string list, match_error) result] in
        assert_equal ~printer (Ok [ "a"; "b"; "c" ]) (split re "a,b,c");
        assert_equal ~printer (Ok [ "a"; "b"; "c"; "" ]) (split re "a,b,c,");
        assert_equal ~printer (Ok [ "a"; "b,c," ]) (split ~limit:2 re "a,b,c,")

  let capture_group_names ctxt =
    (match compile "(?<A>a)(?<B>b)(?<C>c)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        assert_equal ~printer:[%show: (string * int) list]
          (* 0 is the whole match *)
          [ ("A", 1); ("B", 2); ("C", 3) ]
          (capture_groups re));
    match compile "(?<A>a)(b)(?<C>c)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        assert_equal ~printer:[%show: (string * int) list]
          (* 0 is the whole match *)
          [ ("A", 1); ("C", 3) ]
          (capture_groups re)

  let find_iter_test ctxt =
    match compile "a+" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let results = find_iter re "aaa bb aaaa cc a" |> List.of_seq in
        let printer = [%show: (range, match_error) result list] in
        assert_equal ~printer
          [
            Ok { start = 0; end_ = 3 };
            Ok { start = 7; end_ = 11 };
            Ok { start = 15; end_ = 16 };
          ]
          (List.map (Result.map range_of_match) results)

  let find_iter_empty ctxt =
    match compile "x+" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let results = find_iter re "aaa bb aaaa cc a" |> List.of_seq in
        assert_equal ~printer:[%show: (range, match_error) result list] []
          (List.map (Result.map range_of_match) results)

  let find_iter_with_offset ctxt =
    match compile "a+" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let results =
          find_iter ~subject_offset:5 re "aaa bb aaaa cc a" |> List.of_seq
        in
        let printer = [%show: (range, match_error) result list] in
        assert_equal ~printer
          [ Ok { start = 7; end_ = 11 }; Ok { start = 15; end_ = 16 } ]
          (List.map (Result.map range_of_match) results)

  let captures_iter_test ctxt =
    match compile "(a+)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let results = captures_iter re "aaa bb aaaa cc a" |> List.of_seq in
        let get_whole_and_first c =
          match c with
          | Ok c ->
              let whole = range_of_captures c in
              let first =
                match match_of_captures c 1 with
                | Some m -> Some (range_of_match m)
                | None -> None
              in
              Ok (whole, first)
          | Error e -> Error e
        in
        let printer =
          [%show: (range * range option, match_error) result list]
        in
        assert_equal ~printer
          [
            Ok ({ start = 0; end_ = 3 }, Some { start = 0; end_ = 3 });
            Ok ({ start = 7; end_ = 11 }, Some { start = 7; end_ = 11 });
            Ok ({ start = 15; end_ = 16 }, Some { start = 15; end_ = 16 });
          ]
          (List.map get_whole_and_first results)

  let is_match_test ctxt =
    match compile "a+" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (bool, match_error) result] in
        assert_equal ~printer (Ok true) (is_match re "aaa bb");
        assert_equal ~printer (Ok false) (is_match re "bbb cc");
        assert_equal ~printer (Ok true)
          (is_match ~subject_offset:7 re "bbb cc aaaa");
        assert_equal ~printer (Ok false)
          (is_match ~subject_offset:11 re "bbb cc aaaa")

  let substring_of_match_test ctxt =
    match compile "a+" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re -> (
        (match find re "bbb aaa ccc" with
        | Ok (Some m) ->
            assert_equal ~printer:[%show: string] "aaa" (substring_of_match m)
        | Ok None -> assert_failure "Expected to find a match"
        | Error e -> assert_failure ("Match error: " ^ show_match_error e));
        match find re "bbb ccc" with
        | Ok None -> () (* Expected *)
        | Ok (Some _) -> assert_failure "Unexpected match found"
        | Error e -> assert_failure ("Match error: " ^ show_match_error e))

  let captures_length_test ctxt =
    match compile "(a+)(b+)(c+)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re -> (
        (match captures re "aaabbbccc" with
        | Ok (Some c) ->
            assert_equal ~printer:[%show: int] 4 (captures_length c)
            (* whole match + 3 groups *)
        | Ok None -> assert_failure "Expected to find captures"
        | Error e -> assert_failure ("Match error: " ^ show_match_error e));
        (* NB: here, the second capture group is optional *)
        match compile "(a+)(b+)?(c+)" with
        | Error e ->
            assert_failure ("failed to compile: " ^ show_compile_error e)
        | Ok re -> (
            match captures re "aaaccc" with
            | Ok (Some c) ->
                assert_equal ~printer:[%show: int] 4 (captures_length c);
                (* still 4, even with unmatched group *)
                let m = match_of_captures c 2 in
                assert_equal ~printer:[%show: range option] None
                  (Option.map range_of_match m)
            | Ok None -> assert_failure "Expected to find captures"
            | Error e -> assert_failure ("Match error: " ^ show_match_error e)))

  let split_advanced_test ctxt =
    match compile {|\s+|} with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (string list, match_error) result] in
        assert_equal ~printer
          (Ok [ "hello"; "world"; "test" ])
          (split re "hello   world\t\ntest");
        assert_equal ~printer
          (Ok [ ""; "hello"; "world"; "" ])
          (split re " hello world ");
        assert_equal ~printer
          (Ok [ "hello"; "world\t\ntest" ])
          (split ~limit:2 re "hello   world\t\ntest")

  let split_with_offset_test ctxt =
    match compile "," with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (string list, match_error) result] in
        assert_equal ~printer
          (Ok [ "a,b"; "c"; "d" ])
          (split ~subject_offset:2 re "a,b,c,d");
        assert_equal ~printer
          (Ok [ "a,b,c"; "d" ])
          (split ~subject_offset:4 re "a,b,c,d")

  let empty_pattern_test ctxt =
    match compile "" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        (* Empty pattern matches at every position *)
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 0 }))
          (find re "abc" >+= range_of_match);
        (* Test that is_match works with empty patterns *)
        let bool_printer = [%show: (bool, match_error) result] in
        assert_equal ~printer:bool_printer (Ok true) (is_match re "abc");
        assert_equal ~printer:bool_printer (Ok true) (is_match re "")

  let unicode_test ctxt =
    match compile "café" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re -> (
        let printer = [%show: (range option, match_error) result] in
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 5 })) (* café is 5 bytes in UTF-8 *)
          (find re "café" >+= range_of_match);
        match find re "café" with
        | Ok (Some m) ->
            assert_equal ~printer:[%show: string] "café" (substring_of_match m)
        | Ok None -> assert_failure "Expected to find a match"
        | Error e -> assert_failure ("Match error: " ^ show_match_error e))

  let overlapping_matches_test ctxt =
    match compile "aa" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let results = find_iter re "aaaa" |> List.of_seq in
        let printer = [%show: (range, match_error) result list] in
        (* Should find non-overlapping matches *)
        assert_equal ~printer
          [ Ok { start = 0; end_ = 2 }; Ok { start = 2; end_ = 4 } ]
          (List.map (Result.map range_of_match) results)

  let tests =
    [
      "simple_test" >:: simple_test;
      "simple_captures" >:: simple_captures;
      "split_comma" >:: split_comma;
      "non_contiguous_capture" >:: non_contiguous_capture;
      "non_contiguous_named_capture" >:: non_contiguous_named_capture;
      "bad_pattern" >:: bad_pattern;
      "bad_offset" >:: bad_offset;
      "capture_group_names" >:: capture_group_names;
      "find_iter" >:: find_iter_test;
      "find_iter_empty" >:: find_iter_empty;
      "find_iter_with_offset" >:: find_iter_with_offset;
      "captures_iter" >:: captures_iter_test;
      "is_match" >:: is_match_test;
      "substring_of_match" >:: substring_of_match_test;
      "captures_length" >:: captures_length_test;
      "split_advanced" >:: split_advanced_test;
      "split_with_offset" >:: split_with_offset_test;
      "empty_pattern" >:: empty_pattern_test;
      "unicode" >:: unicode_test;
      "overlapping_matches" >:: overlapping_matches_test;
    ]
end

let check_version ctxt =
  assert_bool "Version is older than newest tested" (Pcre2.version >= (10, 43))

(* --- Serialization tests (Interp only; PCRE2 doesn't preserve JIT state) --- *)

let compile_interp_exn pattern =
  match Interp.compile pattern with
  | Ok re -> re
  | Error e ->
      assert_failure
        ("failed to compile: " ^ Interp.show_compile_error e)

let assert_same_interp_match ~pattern ~subject re =
  let printer = [%show: (Interp.range option, Interp.match_error) result] in
  assert_equal ~printer ~msg:("pattern=" ^ pattern ^ " subject=" ^ subject)
    (Interp.find (compile_interp_exn pattern) subject
    >+= Interp.range_of_match)
    (Interp.find re subject >+= Interp.range_of_match)

let marshal_roundtrip_simple ctxt =
  let re = compile_interp_exn "abc" in
  let s = Marshal.to_string re [] in
  let re' : Interp.t = Marshal.from_string s 0 in
  assert_same_interp_match ~pattern:"abc" ~subject:"123abc456" re';
  assert_same_interp_match ~pattern:"abc" ~subject:"123ac" re'

let marshal_roundtrip_captures ctxt =
  let re = compile_interp_exn "(a)(b)(c)" in
  let s = Marshal.to_string re [] in
  let re' : Interp.t = Marshal.from_string s 0 in
  let printer =
    [%show: (Interp.range option, Interp.match_error) result]
  in
  let c = Interp.captures re' "abc" in
  assert_equal ~printer
    (Ok (Some { Interp.start = 0; end_ = 3 }))
    (c >+= Interp.range_of_captures);
  assert_equal ~printer
    (Ok (Some { Interp.start = 1; end_ = 2 }))
    (c
    >>= (fun c -> Interp.match_of_captures c 2)
    >+= Interp.range_of_match)

let marshal_roundtrip_named_groups ctxt =
  let pattern = "(?<word>\\w+)-(?<num>\\d+)" in
  let re = compile_interp_exn pattern in
  let original_names = Interp.capture_groups re in
  let s = Marshal.to_string re [] in
  let re' : Interp.t = Marshal.from_string s 0 in
  assert_equal ~printer:[%show: (string * int) list]
    original_names (Interp.capture_groups re');
  let c = Interp.captures re' "hello-42" in
  let m =
    c >>= fun c -> Interp.named_match_of_captures c "num"
  in
  assert_equal ~printer:[%show: (Interp.range option, Interp.match_error) result]
    (Ok (Some { Interp.start = 6; end_ = 8 }))
    (m >+= Interp.range_of_match)

let to_bytes_roundtrip ctxt =
  let re = compile_interp_exn "(\\d+)-(\\d+)" in
  match Interp.to_bytes re with
  | Error e ->
      assert_failure ("to_bytes failed: " ^ Interp.show_serialize_error e)
  | Ok b -> (
      match Interp.of_bytes b with
      | Error e ->
          assert_failure ("of_bytes failed: " ^ Interp.show_serialize_error e)
      | Ok re' ->
          let printer =
            [%show: (Interp.range option, Interp.match_error) result]
          in
          assert_equal ~printer
            (Ok (Some { Interp.start = 4; end_ = 9 }))
            (Interp.find re' "abc 12-34 def" >+= Interp.range_of_match))

let jit_marshal_refused ctxt =
  match Jit.compile "abc" with
  | Error e ->
      assert_failure ("failed to compile: " ^ Jit.show_compile_error e)
  | Ok jit ->
      assert_raises
        ~msg:"Marshal.to_string on JIT pattern should raise Failure"
        (Failure
           "Pcre2: cannot serialize a JIT-compiled pattern; serialize before \
            calling Jit.of_interp / Jit.compile")
        (fun () -> Marshal.to_string jit [])

let of_bytes_empty_buffer ctxt =
  match Interp.of_bytes Bytes.empty with
  | Error Interp.Empty_buffer -> ()
  | Error e ->
      assert_failure
        ("expected Empty_buffer, got " ^ Interp.show_serialize_error e)
  | Ok _ -> assert_failure "expected Empty_buffer error"

let of_bytes_corrupted ctxt =
  let re = compile_interp_exn "abc" in
  match Interp.to_bytes re with
  | Error _ -> assert_failure "to_bytes failed"
  | Ok b ->
      let b = Bytes.copy b in
      (* Flip the first byte to invalidate PCRE2's magic. *)
      Bytes.set b 0 (Char.chr (Char.code (Bytes.get b 0) lxor 0xff));
      (match Interp.of_bytes b with
      | Error Interp.Pcre2_decode_failed -> ()
      | Error e ->
          assert_failure
            ("expected Pcre2_decode_failed, got "
            ^ Interp.show_serialize_error e)
      | Ok _ -> assert_failure "expected decode failure")

(* OUnit2 has no built-in "raises any exception"; tiny shim. *)
let assert_raises_any f =
  match
    try
      f ();
      None
    with e -> Some e
  with
  | Some _ -> ()
  | None -> assert_failure "expected an exception, none raised"

let marshal_corrupted_blob ctxt =
  let re = compile_interp_exn "abc" in
  let s = Marshal.to_string re [] in
  (* Marshal blobs prefix the custom-block payload with bookkeeping. We can't
     trivially locate the embedded PCRE2 buffer, so just truncate from the
     middle to force either "bad magic", "unsupported schema", "bad length",
     or "decode failed" — any of which should raise rather than crash. *)
  let truncated = String.sub s 0 (max 1 (String.length s - 16)) in
  assert_raises_any (fun () -> ignore (Marshal.from_string truncated 0))

let serialization_tests =
  [
    "marshal_roundtrip_simple" >:: marshal_roundtrip_simple;
    "marshal_roundtrip_captures" >:: marshal_roundtrip_captures;
    "marshal_roundtrip_named_groups" >:: marshal_roundtrip_named_groups;
    "to_bytes_roundtrip" >:: to_bytes_roundtrip;
    "jit_marshal_refused" >:: jit_marshal_refused;
    "of_bytes_empty_buffer" >:: of_bytes_empty_buffer;
    "of_bytes_corrupted" >:: of_bytes_corrupted;
    "marshal_corrupted_blob" >:: marshal_corrupted_blob;
  ]

let suite =
  let module Interp_Tests = MakeTests (Interp) in
  let module Jit_Tests = MakeTests (Jit) in
  "Test pcre"
  >::: [
         "version" >:: check_version;
         "Interp" >::: Interp_Tests.tests;
         "JIT" >::: Jit_Tests.tests;
         "Serialization" >::: serialization_tests;
       ]

let _ = if not !Sys.interactive then run_test_tt_main suite else ()
