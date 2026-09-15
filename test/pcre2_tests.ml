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
    | Error ({ code = MISSING_CLOSING_PARENTHESIS; offset } as e) ->
        assert_equal ~printer:string_of_int ~msg:"Error offset" 3 offset;
        assert_equal ~printer:Fun.id ~msg:"Error message"
          "missing closing parenthesis (at offset 3)" (show_compile_error e)
    | Error e ->
        assert_failure ("Incorrectly error for pattern: " ^ show_compile_error e)
    | Ok _ -> assert_failure "Incorrectly compiled invalid pattern"

  let bad_pattern_lookbehind ctxt =
    match compile "x(?<=a*)b" with
    | Error { code = LOOKBEHIND_NOT_FIXED_LENGTH; offset } ->
        assert_equal ~printer:string_of_int
          ~msg:"Error offset points to start of the failing assertion" 1 offset
    | Error e ->
        assert_failure ("Incorrectly error for pattern: " ^ show_compile_error e)
    | Ok _ -> assert_failure "Incorrectly compiled invalid pattern"

  let bad_offset ctxt =
    match compile "abc" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        assert_equal ~printer:Fun.id ~msg:"Error message" "bad offset value"
          (show_match_error BADOFFSET);
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

  let find_iter_empty_matches ctxt =
    match compile "a*" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range, match_error) result list] in
        (* An empty match at the end position of the previous match is skipped
           rather than repeated forever. *)
        assert_equal ~printer
          [
            Ok { start = 0; end_ = 0 };
            Ok { start = 1; end_ = 1 };
            Ok { start = 2; end_ = 2 };
            Ok { start = 3; end_ = 3 };
          ]
          (find_iter re "bcd" |> List.of_seq
          |> List.map (Result.map range_of_match));
        assert_equal ~printer
          [ Ok { start = 0; end_ = 2 }; Ok { start = 3; end_ = 3 } ]
          (find_iter re "aab" |> List.of_seq
          |> List.map (Result.map range_of_match))

  let find_iter_empty_matches_utf ctxt =
    match compile "(*UTF)x*" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range, match_error) result list] in
        (* Skipping an empty match in a UTF pattern resumes at the next
           character boundary; \xc3\xa9 is a two-byte character. *)
        assert_equal ~printer
          [
            Ok { start = 0; end_ = 0 };
            Ok { start = 1; end_ = 1 };
            Ok { start = 3; end_ = 3 };
          ]
          (find_iter re "a\xc3\xa9" |> List.of_seq
          |> List.map (Result.map range_of_match))

  let captures_iter_empty_matches ctxt =
    match compile "(a*)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range, match_error) result list] in
        assert_equal ~printer
          [
            Ok { start = 0; end_ = 0 };
            Ok { start = 1; end_ = 1 };
            Ok { start = 2; end_ = 2 };
          ]
          (captures_iter re "bc" |> List.of_seq
          |> List.map (Result.map range_of_captures))

  let split_empty_pattern ctxt =
    match compile "" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        assert_equal
          ~printer:[%show: (string list, match_error) result]
          (Ok [ ""; "a"; "b"; "" ])
          (split re "ab")

  let split_propagates_match_error ctxt =
    match compile "," with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        (* A match error in the delimiter stream must not be swallowed: an
           invalid (negative) offset makes matching fail immediately. *)
        assert_equal
          ~printer:[%show: (string list, match_error) result]
          (Error BADOFFSET)
          (split ~subject_offset:(-1) re "a,b,c")

  let tests =
    [
      "simple_test" >:: simple_test;
      "simple_captures" >:: simple_captures;
      "split_comma" >:: split_comma;
      "non_contiguous_capture" >:: non_contiguous_capture;
      "non_contiguous_named_capture" >:: non_contiguous_named_capture;
      "bad_pattern" >:: bad_pattern;
      "bad_pattern_lookbehind" >:: bad_pattern_lookbehind;
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
      "find_iter_empty_matches" >:: find_iter_empty_matches;
      "find_iter_empty_matches_utf" >:: find_iter_empty_matches_utf;
      "captures_iter_empty_matches" >:: captures_iter_empty_matches;
      "split_empty_pattern" >:: split_empty_pattern;
      "split_propagates_match_error" >:: split_propagates_match_error;
      "unicode" >:: unicode_test;
      "overlapping_matches" >:: overlapping_matches_test;
    ]
end

let check_version ctxt =
  assert_bool "Version is older than newest tested" (Pcre2.version >= (10, 43))

let check_config ctxt =
  (* These are queried from the linked library and should be positive defaults,
     not the -1 sentinel they previously returned. *)
  assert_bool "config_match_limit is positive" (Pcre2.config_match_limit > 0);
  assert_bool "config_depth_limit is positive" (Pcre2.config_depth_limit > 0);
  assert_bool "config_heap_limit is positive" (Pcre2.config_heap_limit > 0)

(* A pattern that backtracks catastrophically on a non-matching subject, so a
   low match/depth limit is reliably exceeded. The limit options are a compile
   option shared structurally by both [Interp] and [Jit], but the test functor
   above treats [compile_option] abstractly, so these tests live out here and
   exercise each engine concretely. *)
let pathological_pattern = "(a+)+$"
let catastrophic_subject = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa!"
let result_printer = [%show: (bool, Pcre2.match_error) result]

(* The match limit is exceeded on the catastrophic subject. *)
let assert_match_limit_hit result =
  assert_equal ~printer:result_printer ~msg:"low match limit is exceeded"
    (Error Pcre2.MATCHLIMIT) result

(* The interpreter honours the depth limit; JIT ignores it and on this
   catastrophic input instead hits its own stack limit or the default match
   limit, or fails to match. Any of the latter is fine -- it must just not
   spuriously succeed. *)
let assert_depth_limit_hit ~jit result =
  match result with
  | Error Pcre2.DEPTHLIMIT when not jit -> ()
  | (Error Pcre2.JIT_STACKLIMIT | Error Pcre2.MATCHLIMIT | Ok false) when jit ->
      ()
  | other ->
      assert_failure
        (Printf.sprintf "unexpected depth-limit result (jit=%b): %s" jit
           (result_printer other))

let compile_or_fail c =
  match c with
  | Ok re -> re
  | Error e ->
      assert_failure ("failed to compile: " ^ Pcre2.show_compile_error e)

let interp_limit_tests =
  [
    ( "match_limit" >:: fun _ ->
      let re =
        compile_or_fail
          (Interp.compile ~options:[ `MATCH_LIMIT 100 ] pathological_pattern)
      in
      assert_match_limit_hit (Interp.is_match re catastrophic_subject);
      (* A limited pattern still matches easy input. *)
      assert_equal ~printer:result_printer (Ok true)
        (Interp.is_match re "aaaa") );
    ( "depth_limit" >:: fun _ ->
      let re =
        compile_or_fail
          (Interp.compile ~options:[ `DEPTH_LIMIT 10 ] pathological_pattern)
      in
      assert_depth_limit_hit ~jit:false
        (Interp.is_match re catastrophic_subject) );
    ( "limits_dont_affect_normal_patterns" >:: fun _ ->
      let re =
        compile_or_fail
          (Interp.compile
             ~options:[ `MATCH_LIMIT 1000; `DEPTH_LIMIT 1000; `HEAP_LIMIT 1000 ]
             "a+")
      in
      assert_equal
        ~printer:[%show: (Interp.range option, Pcre2.match_error) result]
        (Ok (Some Interp.{ start = 0; end_ = 3 }))
        (Interp.find re "aaa" |> Result.map (Option.map Interp.range_of_match))
    );
  ]

let jit_limit_tests =
  [
    ( "match_limit" >:: fun _ ->
      let re =
        compile_or_fail
          (Jit.compile ~options:[ `MATCH_LIMIT 100 ] pathological_pattern)
      in
      assert_match_limit_hit (Jit.is_match re catastrophic_subject);
      assert_equal ~printer:result_printer (Ok true) (Jit.is_match re "aaaa") );
    ( "depth_limit" >:: fun _ ->
      let re =
        compile_or_fail
          (Jit.compile ~options:[ `DEPTH_LIMIT 10 ] pathological_pattern)
      in
      assert_depth_limit_hit ~jit:true (Jit.is_match re catastrophic_subject) );
    ( "limits_dont_affect_normal_patterns" >:: fun _ ->
      let re =
        compile_or_fail
          (Jit.compile
             ~options:[ `MATCH_LIMIT 1000; `DEPTH_LIMIT 1000; `HEAP_LIMIT 1000 ]
             "a+")
      in
      assert_equal
        ~printer:[%show: (Jit.range option, Pcre2.match_error) result]
        (Ok (Some Jit.{ start = 0; end_ = 3 }))
        (Jit.find re "aaa" |> Result.map (Option.map Jit.range_of_match)) );
  ]

let suite =
  let module Interp_Tests = MakeTests (Interp) in
  let module Jit_Tests = MakeTests (Jit) in
  "Test pcre"
  >::: [
         "version" >:: check_version;
         "config" >:: check_config;
         "Interp" >::: Interp_Tests.tests @ interp_limit_tests;
         "JIT" >::: Jit_Tests.tests @ jit_limit_tests;
       ]

let _ = if not !Sys.interactive then run_test_tt_main suite else ()
