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
    ]
end

let check_version ctxt =
  assert_bool "Version is older than newest tested" (Pcre2.version >= (10, 43))

let suite =
  let module Interp_Tests = MakeTests (Interp) in
  let module Jit_Tests = MakeTests (Jit) in
  "Test pcre"
  >::: [
         "version" >:: check_version;
         "Interp" >::: Interp_Tests.tests;
         "JIT" >::: Jit_Tests.tests;
       ]

let _ = if not !Sys.interactive then run_test_tt_main suite else ()
