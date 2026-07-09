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
    (M : Pcre2.Matcher
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

  let alloc_per_attempt_test ctxt =
    (* Perf chunk 1 (interpreter.ml DEVIATION (perf), match_state): the
       dispatch nest is module-level, so a match attempt allocates no
       minor words. Pattern "qz": the first code unit 'q' hits at every
       position of a 100k-'q' subject (defeating the driver's memchr
       fast-forward), so this single exec runs ~100k failing attempts.
       Before the hoist each attempt rebuilt the interpreter closure
       nest (~66 minor words per attempt, ~6.6M for this exec); now the
       whole exec is O(1). The 1_000-word slack covers the per-exec
       setup (mb/match_state/driver closures) and the float boxing of
       Gc.minor_words itself. *)
    match compile "qz" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let subject = String.make 100_000 'q' in
        let bool_printer = [%show: (bool, match_error) result] in
        (* warm-up exec: one-time lazy initialization out of the way *)
        assert_equal ~printer:bool_printer (Ok false) (is_match re subject);
        let before = Gc.minor_words () in
        let result = is_match re subject in
        let delta = Gc.minor_words () -. before in
        assert_equal ~printer:bool_printer (Ok false) result;
        assert_bool
          (Printf.sprintf
             "expected O(1) minor allocation for ~100k attempts in one exec, \
              measured %.0f words"
             delta)
          (delta < 1000.)

  let alloc_per_exec_major_test ctxt =
    (* Perf chunk 2 P1 (frames.ml scratch arena — the C's keep-if-big-
       enough frames-vector reuse, pcre2_match.c:7062-7077): the
       backtracking-frame arena is retained across execs in a module
       scratch slot, so N execs allocate O(1) MAJOR words instead of one
       fresh ~4,000-word zeroed int array each (> Max_young_wosize, so it
       was a major-heap allocation becoming major garbage per call).
       Pattern "(q)z" (one capture group) against a small subject: 1,000
       execs must stay under 2,000 major words TOTAL. Before the reuse
       each exec allocated ~4,200 major words (~4.2M for this loop); the
       2,000-word slack covers minor-to-major promotions of live per-exec
       setup data at minor-GC boundaries. *)
    match compile "(q)z" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let subject = "qqqq" in
        let bool_printer = [%show: (bool, match_error) result] in
        (* warm-up exec: lazy initialization + scratch-slot seeding *)
        assert_equal ~printer:bool_printer (Ok false) (is_match re subject);
        (* The loop body stays allocation-lean (no assert_equal: its
           allocations trigger minor GCs that promote live harness state
           into the major heap, polluting the measurement); correctness
           is checked once, after. *)
        let wrong = ref 0 in
        let before = (Gc.quick_stat ()).Gc.major_words in
        for _ = 1 to 1_000 do
          match is_match re subject with
          | Ok false -> ()
          | Ok true | Error _ -> incr wrong
        done;
        let delta = (Gc.quick_stat ()).Gc.major_words -. before in
        assert_equal ~printer:string_of_int ~msg:"unexpected is_match results" 0
          !wrong;
        assert_bool
          (Printf.sprintf
             "expected O(1) major allocation for 1,000 execs (frames arena \
              reused across execs), measured %.0f major words"
             delta)
          (delta < 2000.)

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
      "alloc_per_attempt" >:: alloc_per_attempt_test;
      "alloc_per_exec_major" >:: alloc_per_exec_major_test;
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
