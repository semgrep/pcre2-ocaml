open! Pcre2

let check_eq show msg expected actual =
  Alcotest.check
    (Alcotest.testable (fun fmt x -> Format.pp_print_string fmt (show x)) ( = ))
    msg expected actual

let check_true msg cond = Alcotest.(check bool) msg true cond

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
  val tests : unit Alcotest.test_case list
end = struct
  open M

  let simple_test () =
    match compile "abc" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        check_eq printer ""
          (Ok (Some { start = 0; end_ = 3 }))
          (find re "abc" >+= range_of_match);
        check_eq printer ""
          (Ok (Some { start = 3; end_ = 6 }))
          (find re "123abc456" >+= range_of_match);
        check_eq printer ""
          (Ok (Some { start = 3; end_ = 6 }))
          (find re "123abc" >+= range_of_match);
        check_eq printer "" (Ok None) (find re "123ac" >+= range_of_match)

  let simple_captures () =
    match compile "(a)(b)(c)" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        let c = captures re "abc" in
        check_eq printer ""
          (Ok (Some { start = 0; end_ = 3 }))
          (c >+= range_of_captures);
        check_eq printer ""
          (Ok (Some { start = 0; end_ = 1 }))
          (c >>= (fun c -> match_of_captures c 1) >+= range_of_match);
        check_eq printer ""
          (Ok (Some { start = 1; end_ = 2 }))
          (c >>= (fun c -> match_of_captures c 2) >+= range_of_match);
        check_eq printer ""
          (Ok (Some { start = 2; end_ = 3 }))
          (c >>= (fun c -> match_of_captures c 3) >+= range_of_match);
        check_eq printer "" (Ok None)
          (c >>= (fun c -> match_of_captures c 4) >+= range_of_match)

  let non_contiguous_capture () =
    match compile "(a)(?:(b)|(c))" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        let c = captures re "ac" in
        check_eq printer ""
          (Ok (Some { start = 0; end_ = 2 }))
          (c >>= (fun c -> match_of_captures c 0) >+= range_of_match);
        check_eq printer ""
          (Ok (Some { start = 0; end_ = 1 }))
          (c >>= (fun c -> match_of_captures c 1) >+= range_of_match);
        check_eq printer "" (Ok None)
          (c >>= (fun c -> match_of_captures c 2) >+= range_of_match);
        check_eq printer ""
          (Ok (Some { start = 1; end_ = 2 }))
          (c >>= (fun c -> match_of_captures c 3) >+= range_of_match);
        check_eq printer "" (Ok None)
          (c >>= (fun c -> match_of_captures c 4) >+= range_of_match)

  let non_contiguous_named_capture () =
    match compile "(?<A>a)(?:(?<B>b)|(?<C>c))" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        let c = captures re "ac" in
        check_eq printer ""
          (Ok (Some { start = 0; end_ = 1 }))
          (c >>= (fun c -> named_match_of_captures c "A") >+= range_of_match);
        check_eq printer "" (Ok None)
          (c >>= (fun c -> named_match_of_captures c "B") >+= range_of_match);
        check_eq printer ""
          (Ok (Some { start = 1; end_ = 2 }))
          (c >>= (fun c -> named_match_of_captures c "C") >+= range_of_match)

  let bad_pattern () =
    match compile "ab(" with
    | Error MISSING_CLOSING_PARENTHESIS -> ()
    | Error e ->
        Alcotest.fail ("Incorrectly error for pattern: " ^ show_compile_error e)
    | Ok _ -> Alcotest.fail "Incorrectly compiled invalid pattern"

  let bad_offset () =
    match compile "abc" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        check_eq printer "Negative offset" (Error BADOFFSET)
          (find ~subject_offset:(-1) re "ab" >+= range_of_match);
        check_eq printer "Offset too large" (Error BADOFFSET)
          (find ~subject_offset:10 re "123abc456" >+= range_of_match)

  let split_comma () =
    match compile "," with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (string list, match_error) result] in
        check_eq printer "" (Ok [ "a"; "b"; "c" ]) (split re "a,b,c");
        check_eq printer "" (Ok [ "a"; "b"; "c"; "" ]) (split re "a,b,c,");
        check_eq printer "" (Ok [ "a"; "b,c," ]) (split ~limit:2 re "a,b,c,")

  let capture_group_names () =
    (match compile "(?<A>a)(?<B>b)(?<C>c)" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        check_eq [%show: (string * int) list] ""
          (* 0 is the whole match *)
          [ ("A", 1); ("B", 2); ("C", 3) ]
          (capture_groups re));
    match compile "(?<A>a)(b)(?<C>c)" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        check_eq [%show: (string * int) list] ""
          (* 0 is the whole match *)
          [ ("A", 1); ("C", 3) ]
          (capture_groups re)

  let find_iter_test () =
    match compile "a+" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let results = find_iter re "aaa bb aaaa cc a" |> List.of_seq in
        let printer = [%show: (range, match_error) result list] in
        check_eq printer ""
          [
            Ok { start = 0; end_ = 3 };
            Ok { start = 7; end_ = 11 };
            Ok { start = 15; end_ = 16 };
          ]
          (List.map (Result.map range_of_match) results)

  let find_iter_empty () =
    match compile "x+" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let results = find_iter re "aaa bb aaaa cc a" |> List.of_seq in
        check_eq [%show: (range, match_error) result list] "" []
          (List.map (Result.map range_of_match) results)

  let find_iter_with_offset () =
    match compile "a+" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let results =
          find_iter ~subject_offset:5 re "aaa bb aaaa cc a" |> List.of_seq
        in
        let printer = [%show: (range, match_error) result list] in
        check_eq printer ""
          [ Ok { start = 7; end_ = 11 }; Ok { start = 15; end_ = 16 } ]
          (List.map (Result.map range_of_match) results)

  let captures_iter_test () =
    match compile "(a+)" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
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
        check_eq printer ""
          [
            Ok ({ start = 0; end_ = 3 }, Some { start = 0; end_ = 3 });
            Ok ({ start = 7; end_ = 11 }, Some { start = 7; end_ = 11 });
            Ok ({ start = 15; end_ = 16 }, Some { start = 15; end_ = 16 });
          ]
          (List.map get_whole_and_first results)

  let is_match_test () =
    match compile "a+" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (bool, match_error) result] in
        check_eq printer "" (Ok true) (is_match re "aaa bb");
        check_eq printer "" (Ok false) (is_match re "bbb cc");
        check_eq printer "" (Ok true)
          (is_match ~subject_offset:7 re "bbb cc aaaa");
        check_eq printer "" (Ok false)
          (is_match ~subject_offset:11 re "bbb cc aaaa")

  let substring_of_match_test () =
    match compile "a+" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re -> (
        (match find re "bbb aaa ccc" with
        | Ok (Some m) ->
            check_eq [%show: string] "" "aaa" (substring_of_match m)
        | Ok None -> Alcotest.fail "Expected to find a match"
        | Error e -> Alcotest.fail ("Match error: " ^ show_match_error e));
        match find re "bbb ccc" with
        | Ok None -> () (* Expected *)
        | Ok (Some _) -> Alcotest.fail "Unexpected match found"
        | Error e -> Alcotest.fail ("Match error: " ^ show_match_error e))

  let captures_length_test () =
    match compile "(a+)(b+)(c+)" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re -> (
        (match captures re "aaabbbccc" with
        | Ok (Some c) ->
            check_eq [%show: int] "" 4 (captures_length c)
            (* whole match + 3 groups *)
        | Ok None -> Alcotest.fail "Expected to find captures"
        | Error e -> Alcotest.fail ("Match error: " ^ show_match_error e));
        (* NB: here, the second capture group is optional *)
        match compile "(a+)(b+)?(c+)" with
        | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
        | Ok re -> (
            match captures re "aaaccc" with
            | Ok (Some c) ->
                check_eq [%show: int] "" 4 (captures_length c);
                (* still 4, even with unmatched group *)
                let m = match_of_captures c 2 in
                check_eq [%show: range option] "" None
                  (Option.map range_of_match m)
            | Ok None -> Alcotest.fail "Expected to find captures"
            | Error e -> Alcotest.fail ("Match error: " ^ show_match_error e)))

  let split_advanced_test () =
    match compile {|\s+|} with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (string list, match_error) result] in
        check_eq printer ""
          (Ok [ "hello"; "world"; "test" ])
          (split re "hello   world\t\ntest");
        check_eq printer ""
          (Ok [ ""; "hello"; "world"; "" ])
          (split re " hello world ");
        check_eq printer ""
          (Ok [ "hello"; "world\t\ntest" ])
          (split ~limit:2 re "hello   world\t\ntest")

  let split_with_offset_test () =
    match compile "," with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (string list, match_error) result] in
        check_eq printer ""
          (Ok [ "a,b"; "c"; "d" ])
          (split ~subject_offset:2 re "a,b,c,d");
        check_eq printer ""
          (Ok [ "a,b,c"; "d" ])
          (split ~subject_offset:4 re "a,b,c,d")

  let empty_pattern_test () =
    match compile "" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        (* Empty pattern matches at every position *)
        check_eq printer ""
          (Ok (Some { start = 0; end_ = 0 }))
          (find re "abc" >+= range_of_match);
        (* Test that is_match works with empty patterns *)
        let bool_printer = [%show: (bool, match_error) result] in
        check_eq bool_printer "" (Ok true) (is_match re "abc");
        check_eq bool_printer "" (Ok true) (is_match re "")

  let unicode_test () =
    match compile "café" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re -> (
        let printer = [%show: (range option, match_error) result] in
        check_eq printer ""
          (Ok (Some { start = 0; end_ = 5 })) (* café is 5 bytes in UTF-8 *)
          (find re "café" >+= range_of_match);
        match find re "café" with
        | Ok (Some m) ->
            check_eq [%show: string] "" "café" (substring_of_match m)
        | Ok None -> Alcotest.fail "Expected to find a match"
        | Error e -> Alcotest.fail ("Match error: " ^ show_match_error e))

  let overlapping_matches_test () =
    match compile "aa" with
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let results = find_iter re "aaaa" |> List.of_seq in
        let printer = [%show: (range, match_error) result list] in
        (* Should find non-overlapping matches *)
        check_eq printer ""
          [ Ok { start = 0; end_ = 2 }; Ok { start = 2; end_ = 4 } ]
          (List.map (Result.map range_of_match) results)

  let alloc_per_attempt_test () =
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
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let subject = String.make 100_000 'q' in
        let bool_printer = [%show: (bool, match_error) result] in
        (* warm-up exec: one-time lazy initialization out of the way *)
        check_eq bool_printer "" (Ok false) (is_match re subject);
        let before = Gc.minor_words () in
        let result = is_match re subject in
        let delta = Gc.minor_words () -. before in
        check_eq bool_printer "" (Ok false) result;
        check_true
          (Printf.sprintf
             "expected O(1) minor allocation for ~100k attempts in one exec, \
              measured %.0f words"
             delta)
          (delta < 1000.)

  let alloc_per_exec_major_test () =
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
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let subject = "qqqq" in
        let bool_printer = [%show: (bool, match_error) result] in
        (* warm-up exec: lazy initialization + scratch-slot seeding *)
        check_eq bool_printer "" (Ok false) (is_match re subject);
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
        check_eq string_of_int "unexpected is_match results" 0 !wrong;
        check_true
          (Printf.sprintf
             "expected O(1) major allocation for 1,000 execs (frames arena \
              reused across execs), measured %.0f major words"
             delta)
          (delta < 2000.)

  let tests =
    [
      Alcotest.test_case "simple_test" `Quick simple_test;
      Alcotest.test_case "simple_captures" `Quick simple_captures;
      Alcotest.test_case "split_comma" `Quick split_comma;
      Alcotest.test_case "non_contiguous_capture" `Quick non_contiguous_capture;
      Alcotest.test_case "non_contiguous_named_capture" `Quick
        non_contiguous_named_capture;
      Alcotest.test_case "bad_pattern" `Quick bad_pattern;
      Alcotest.test_case "bad_offset" `Quick bad_offset;
      Alcotest.test_case "capture_group_names" `Quick capture_group_names;
      Alcotest.test_case "find_iter" `Quick find_iter_test;
      Alcotest.test_case "find_iter_empty" `Quick find_iter_empty;
      Alcotest.test_case "find_iter_with_offset" `Quick find_iter_with_offset;
      Alcotest.test_case "captures_iter" `Quick captures_iter_test;
      Alcotest.test_case "is_match" `Quick is_match_test;
      Alcotest.test_case "substring_of_match" `Quick substring_of_match_test;
      Alcotest.test_case "captures_length" `Quick captures_length_test;
      Alcotest.test_case "split_advanced" `Quick split_advanced_test;
      Alcotest.test_case "split_with_offset" `Quick split_with_offset_test;
      Alcotest.test_case "empty_pattern" `Quick empty_pattern_test;
      Alcotest.test_case "unicode" `Quick unicode_test;
      Alcotest.test_case "overlapping_matches" `Quick overlapping_matches_test;
      Alcotest.test_case "alloc_per_attempt" `Quick alloc_per_attempt_test;
      Alcotest.test_case "alloc_per_exec_major" `Quick alloc_per_exec_major_test;
    ]
end

let check_version () =
  check_true "Version is older than newest tested" (Pcre2.version >= (10, 43))

let alloc_per_exec_minor () =
  (* Perf chunk 2 P4b pin at the ENGINE seam (plan target < ~40 minor
     words/exec): the per-exec mb/match_state/driver_state records are
     cached and re-filled (interpreter.ml fresh_trio, one scratch bundle
     under the frames-arena busy flag), the newline-config refs became
     direct mb writes, and the bool/pair seam skips exec_full's Match
     record + Array.sub — leaving only the per-exec leftovers (three
     driver setup refs, the arena record + Ok, the match data + its
     ovector, the result option), ~30 words for a 1-capture pattern.
     Measured at Engine.exec, not through Pcre2.Interp: the frozen
     pcre2.ml layer adds ~29 words/exec of its own wrappers that this
     project must not change (its find/is_match semantics are FROZEN).
     Pre-P4b the engine seam measured ~123 words/exec. This test is
     engine-seam-single (Interp and Jit share Engine.exec — bindings.ml
     aliases them), so it lives outside the Interp/Jit functor. *)
  let re =
    match Pcre2_engine.Engine.compile "(q)z" 0l with
    | Ok r -> r
    | Error code ->
        Alcotest.fail ("failed to compile: error " ^ string_of_int code)
  in
  let subject = "qqqq" in
  (* warm-up exec: lazy initialization + scratch seeding *)
  (match Pcre2_engine.Engine.exec re subject 0 0l with
  | Ok None -> ()
  | _ -> Alcotest.fail "expected no match");
  let wrong = ref 0 in
  let before = Gc.minor_words () in
  for _ = 1 to 1_000 do
    match Pcre2_engine.Engine.exec re subject 0 0l with
    | Ok None -> ()
    | Ok (Some _) | Error _ -> incr wrong
  done;
  let delta = (Gc.minor_words () -. before) /. 1000. in
  check_eq string_of_int "unexpected exec results" 0 !wrong;
  check_true
    (Printf.sprintf
       "expected < 40 minor words per exec at the engine seam (scratch-trio \
        reuse + exec fast path), measured %.1f words/exec"
       delta)
    (delta < 40.)

let suite =
  let module Interp_Tests = MakeTests (Interp) in
  let module Jit_Tests = MakeTests (Jit) in
  [
    ( "misc",
      [
        Alcotest.test_case "version" `Quick check_version;
        Alcotest.test_case "alloc_per_exec_minor" `Quick alloc_per_exec_minor;
      ] );
    ("Interp", Interp_Tests.tests);
    ("JIT", Jit_Tests.tests);
  ]

let () = if not !Sys.interactive then Alcotest.run "pcre2" suite
