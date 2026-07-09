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

  let compile_exn pat =
    match compile pat with
    | Ok re -> re
    | Error e -> Alcotest.fail ("failed to compile: " ^ show_compile_error e)

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

  (* --- Convenience layer re-added from tag 7.5.3 --- *)

  let full_split_basic () =
    (* Ported from the 7.5.3 test suite (test/pcre2_tests.ml, simple_test). *)
    let re = compile_exn "(x)|(u)" in
    let printer = [%show: (split_result list, match_error) result] in
    check_eq printer ""
      (Ok [ Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd" ])
      (full_split re "abxcd");
    check_eq printer ""
      (Ok
         [
           Text "ab";
           Delim "x";
           Group (1, "x");
           NoGroup;
           Text "cd";
           Delim "u";
           NoGroup;
           Group (2, "u");
           Text "ef";
         ])
      (full_split re "abxcduef")

  let full_split_max_and_strip () =
    let re = compile_exn "," in
    let printer = [%show: (split_result list, match_error) result] in
    check_eq printer "max=1 keeps the whole subject" (Ok [ Text "a,b," ])
      (full_split ~max:1 re "a,b,");
    check_eq printer "default max=0 strips trailing delimiters"
      (Ok [ Text "a"; Delim ","; Text "b" ])
      (full_split re "a,b,");
    check_eq printer "negative max keeps trailing delimiters"
      (Ok [ Text "a"; Delim ","; Text "b"; Delim "," ])
      (full_split ~max:(-1) re "a,b,");
    check_eq printer "empty subject" (Ok []) (full_split re "")

  let full_split_empty_match () =
    (* Empty delimiters follow the 7.5.3 loop: an ANCHORED|NOTEMPTY retry at
       the same position, else a one-char Text and an advance by one. *)
    let re = compile_exn "x*" in
    let printer = [%show: (split_result list, match_error) result] in
    check_eq printer ""
      (Ok [ Delim ""; Text "a"; Delim "x"; Delim ""; Text "b" ])
      (full_split re "axb");
    check_eq printer "trailing empty delimiter kept with negative max"
      (Ok [ Delim ""; Text "a"; Delim "x"; Delim ""; Text "b"; Delim "" ])
      (full_split ~max:(-1) re "axb")

  let replace_basic () =
    let re = compile_exn "a" in
    let printer = [%show: (string, match_error) result] in
    check_eq printer "" (Ok "bbb") (replace ~templ:"b" re "aaa");
    check_eq printer "no match returns the subject" (Ok "xyz")
      (replace ~templ:"b" re "xyz");
    check_eq printer "prefix before subject_offset is preserved" (Ok "aaXX")
      (replace ~subject_offset:2 ~templ:"X" re "aaaa");
    check_eq printer "default template deletes matches" (Ok "bc")
      (replace re "abca")

  let replace_templates () =
    let printer = [%show: (string, match_error) result] in
    let re = compile_exn "(a+)(b+)?" in
    check_eq printer "backreference; unset group expands to nothing"
      (Ok "<aa|>")
      (replace ~templ:"<$1|$2>" re "aa");
    let re_b = compile_exn "b+" in
    check_eq printer "whole match, pre-match and post-match"
      (Ok "aa[aa|bb|cc]cc")
      (replace_first ~templ:"[$`|$&|$']" re_b "aabbcc");
    let re_a = compile_exn "a" in
    check_eq printer "$$ is a literal dollar" (Ok "$")
      (replace ~templ:"$$" re_a "a");
    let re_g = compile_exn "(a)" in
    check_eq printer "$! separates a backref from following digits" (Ok "a1")
      (replace ~templ:"$1$!1" re_g "a");
    let re_alt = compile_exn "(x)|(u)" in
    check_eq printer "$+ is the last group that matched" (Ok "a<u>")
      (replace ~templ:"<$+>" re_alt "au");
    let re_10 = compile_exn "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)" in
    check_eq printer "multi-digit backreference" (Ok "j")
      (replace ~templ:"$10" re_10 "abcdefghij");
    check_eq printer "precompiled itempl" (Ok "<aa>")
      (replace ~itempl:(Pcre2.subst "<$1>") re "aa");
    check_eq printer "templ wins over itempl" (Ok "X")
      (replace ~itempl:(Pcre2.subst "<$1>") ~templ:"X" re "aa")

  let replace_empty_match () =
    (* 7.5.3 empty-match rule: append the expansion, keep one subject char,
       resume one past the match start. *)
    let re = compile_exn "x*" in
    let printer = [%show: (string, match_error) result] in
    check_eq printer "" (Ok "-a-b-c-") (replace ~templ:"-" re "abc");
    check_eq printer "" (Ok "--a--b-") (replace ~templ:"-" re "xaxb")

  let replace_template_failures () =
    let re = compile_exn "(a)" in
    Alcotest.check_raises "nonexistent backreference"
      (Failure "Pcre2.replace: backreference denotes nonexistent subpattern")
      (fun () -> ignore (replace ~templ:"$3" re "aaa"));
    Alcotest.check_raises "replace_first message prefix"
      (Failure
         "Pcre2.replace_first: backreference denotes nonexistent subpattern")
      (fun () -> ignore (replace_first ~templ:"$3" re "aaa"));
    let re0 = compile_exn "a" in
    Alcotest.check_raises "$+ with no capture groups"
      (Failure "Pcre2.replace: no backreferences") (fun () ->
        ignore (replace ~templ:"$+" re0 "a"));
    (* Deviation from 7.5.3: validation happens per match (no capturecount
       accessor), so a bad template on a non-matching subject is not an
       error. *)
    let printer = [%show: (string, match_error) result] in
    check_eq printer "no match, no validation" (Ok "zzz")
      (replace ~templ:"$3" re "zzz")

  let replace_first_test () =
    let re = compile_exn "a+" in
    let printer = [%show: (string, match_error) result] in
    check_eq printer "" (Ok "X bb aa") (replace_first ~templ:"X" re "aa bb aa");
    check_eq printer "no match returns the subject" (Ok "bb")
      (replace_first ~templ:"X" re "bb")

  let qreplace_tests () =
    let re = compile_exn "a+" in
    let printer = [%show: (string, match_error) result] in
    check_eq printer "" (Ok "x<>y<>") (qreplace ~templ:"<>" re "xaayaaa");
    check_eq printer "default template deletes matches" (Ok "xy")
      (qreplace re "xaay");
    check_eq printer "no $-parsing in qreplace" (Ok "$1")
      (qreplace ~templ:"$1" re "aa");
    check_eq printer "first only" (Ok "x<>yaaa")
      (qreplace_first ~templ:"<>" re "xaayaaa")

  let substitute_tests () =
    let printer = [%show: (string, match_error) result] in
    let re = compile_exn "[a-z]+" in
    check_eq printer "" (Ok "HELLO WORLD")
      (substitute ~subst:String.uppercase_ascii re "hello world");
    check_eq printer "first only" (Ok "HELLO world")
      (substitute_first ~subst:String.uppercase_ascii re "hello world");
    let re2 = compile_exn "(a+)(b+)" in
    let swap c =
      let get i =
        match match_of_captures c i with
        | Some m -> substring_of_match m
        | None -> ""
      in
      get 2 ^ get 1
    in
    check_eq printer "captures callback" (Ok "x bbaa y")
      (substitute_substrings ~subst:swap re2 "x aabb y");
    check_eq printer "captures callback, first only" (Ok "ba ab")
      (substitute_substrings_first ~subst:swap re2 "ab ab")

  let extract_tests () =
    let re = compile_exn "(a+)(b+)?" in
    let printer = [%show: (string array option, match_error) result] in
    check_eq printer "unset group is the empty string"
      (Ok (Some [| "aa"; "aa"; "" |]))
      (extract re "aa");
    check_eq printer "without the full match"
      (Ok (Some [| "aa"; "" |]))
      (extract ~full_match:false re "aa");
    check_eq printer "no match" (Ok None) (extract re "zz");
    let opt_printer =
      [%show: (string option array option, match_error) result]
    in
    check_eq opt_printer "unset group is None"
      (Ok (Some [| Some "aa"; Some "aa"; None |]))
      (extract_opt re "aa");
    check_eq opt_printer "no match" (Ok None) (extract_opt re "zz")

  let extract_all_tests () =
    let re = compile_exn "([a-z])([0-9])" in
    let printer = [%show: (string array array, match_error) result] in
    check_eq printer ""
      (Ok [| [| "a1"; "a"; "1" |]; [| "b2"; "b"; "2" |] |])
      (extract_all re "a1 b2");
    check_eq printer "no match" (Ok [||]) (extract_all re "999");
    (* Empty-match iteration follows 7.5.3 exec_all: after an empty match the
       next attempt is at the SAME position with NOTEMPTY, and iteration
       stops if that fails — hence no row for the final position 3. *)
    let re_x = compile_exn "x*" in
    check_eq printer ""
      (Ok [| [| "" |]; [| "x" |]; [| "" |] |])
      (extract_all re_x "axb");
    let opt_printer =
      [%show: (string option array array, match_error) result]
    in
    let re2 = compile_exn "(a)|(b)" in
    check_eq opt_printer ""
      (Ok [| [| Some "a"; Some "a"; None |]; [| Some "b"; None; Some "b" |] |])
      (extract_all_opt re2 "ab")

  let convenience_bad_offset () =
    let re = compile_exn "a" in
    let printer = [%show: (string, match_error) result] in
    check_eq printer "replace, negative offset" (Error BADOFFSET)
      (replace ~subject_offset:(-1) ~templ:"b" re "aaa");
    let fs_printer = [%show: (split_result list, match_error) result] in
    check_eq fs_printer "full_split, offset past the end" (Error BADOFFSET)
      (full_split ~subject_offset:99 re "aaa");
    let ex_printer = [%show: (string array option, match_error) result] in
    check_eq ex_printer "extract, negative offset" (Error BADOFFSET)
      (extract ~subject_offset:(-1) re "aaa")

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
      Alcotest.test_case "full_split_basic" `Quick full_split_basic;
      Alcotest.test_case "full_split_max_and_strip" `Quick
        full_split_max_and_strip;
      Alcotest.test_case "full_split_empty_match" `Quick full_split_empty_match;
      Alcotest.test_case "replace_basic" `Quick replace_basic;
      Alcotest.test_case "replace_templates" `Quick replace_templates;
      Alcotest.test_case "replace_empty_match" `Quick replace_empty_match;
      Alcotest.test_case "replace_template_failures" `Quick
        replace_template_failures;
      Alcotest.test_case "replace_first" `Quick replace_first_test;
      Alcotest.test_case "qreplace" `Quick qreplace_tests;
      Alcotest.test_case "substitute" `Quick substitute_tests;
      Alcotest.test_case "extract" `Quick extract_tests;
      Alcotest.test_case "extract_all" `Quick extract_all_tests;
      Alcotest.test_case "convenience_bad_offset" `Quick convenience_bad_offset;
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

let quote_test () =
  check_eq [%show: string] "escapes every special character"
    {|\\\^\$\.\[\|\(\)\?\*\+\{|}
    (Pcre2.quote {|\^$.[|()?*+{|});
  check_eq [%show: string] "leaves ordinary text alone" "a-b]c}d"
    (Pcre2.quote "a-b]c}d");
  (* Round trip: the quoted pattern matches the original string literally. *)
  let specials = {|a\b^c$d.e[f|g(h)i?j*k+l{m}n]o|} in
  match Pcre2.Interp.compile (Pcre2.quote specials) with
  | Error _ -> Alcotest.fail "failed to compile quoted pattern"
  | Ok re -> (
      match Pcre2.Interp.is_match re specials with
      | Ok true -> ()
      | _ -> Alcotest.fail "quoted pattern did not match the literal string")

let suite =
  let module Interp_Tests = MakeTests (Interp) in
  let module Jit_Tests = MakeTests (Jit) in
  [
    ( "misc",
      [
        Alcotest.test_case "version" `Quick check_version;
        Alcotest.test_case "alloc_per_exec_minor" `Quick alloc_per_exec_minor;
        Alcotest.test_case "quote" `Quick quote_test;
      ] );
    ("Interp", Interp_Tests.tests);
    ("JIT", Jit_Tests.tests);
  ]

let () = if not !Sys.interactive then Alcotest.run "pcre2" suite
