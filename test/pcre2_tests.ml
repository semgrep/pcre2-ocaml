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
    (match compile "(?<A>a)(b)(?<C>c)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        assert_equal ~printer:[%show: (string * int) list]
          (* 0 is the whole match *)
          [ ("A", 1); ("C", 3) ]
          (capture_groups re));
    (* A pattern with no named groups at all: the name table is empty rather
       than absent, so it is a distinct case from the two above. *)
    match compile "(a+)(b+)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re -> (
        assert_equal ~printer:[%show: (string * int) list] []
          (capture_groups re);
        match captures re "aaabbb" with
        | Ok (Some c) ->
            assert_equal ~printer:[%show: string option] None
              (named_match_of_captures c "A" |> Option.map substring_of_match)
        | Ok None -> assert_failure "expected a match for aaabbb"
        | Error e -> assert_failure ("match error: " ^ show_match_error e))

  (* Regression: for a pattern with no named groups the name table must be the
     shared atom, not a zero-length block, which the major GC cannot walk.  The
     retention below is what makes this bite -- a table that dies before the
     next collection is never scanned, so the values have to survive the GC. *)
  let name_table_compaction ctxt =
    match compile "(a+)(b+)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let kept = ref [] in
        for _ = 1 to 200 do
          match captures re "aaabbb" with
          | Ok (Some c) -> kept := c :: !kept
          | Ok None -> assert_failure "expected a match for aaabbb"
          | Error e -> assert_failure ("match error: " ^ show_match_error e)
        done;
        Gc.minor ();
        Gc.full_major ();
        Gc.compact ();
        assert_equal ~printer:[%show: int] 200 (List.length !kept);
        (* Read the retained values back, so a table the compactor moved
           incorrectly is not merely walked but used. *)
        List.iter
          (fun c ->
            assert_equal ~printer:[%show: int] 3 (captures_length c);
            assert_equal ~printer:[%show: (string * int) list] []
              (capture_groups re))
          !kept

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

  (* Substrings of the capture groups, excluding the whole match.  [None] for a
     group the match left unset. *)
  let group_substrings c =
    List.init (captures_length c - 1) (fun i ->
        match_of_captures c (i + 1) |> Option.map substring_of_match)

  let big_pattern =
    "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)(m)(n)(o)(p)(q)(r)(s)(t)"

  let with_pattern pat f =
    match compile pat with
    | Error e ->
        assert_failure ("failed to compile " ^ pat ^ ": " ^ show_compile_error e)
    | Ok re -> f re

  (* The match data cache is per-thread and grow-only, so alternating between
     patterns with few and many capture groups exercises both the growth path
     and reuse of an oversized ovector.  The small pattern's assertions are the
     interesting half: reading the cache's capacity rather than the current
     match's capture count would surface stale groups from the big pattern.

     Note this asserts behavioural equivalence, not that reuse happens: nothing
     about the cache is observable from OCaml, so the same assertions pass if
     every match allocates its own block. *)
  let match_data_reuse ctxt =
    match (compile "([a-z]+)-([0-9]+)", compile big_pattern) with
    | Error e, _ | _, Error e ->
        assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok small, Ok big ->
        let printer = [%show: string option list] in
        let expected_big =
          List.init 20 (fun k ->
              Some (String.make 1 (Char.chr (Char.code 'a' + k))))
        in
        for i = 0 to 999 do
          let subj = Printf.sprintf "item-%d" i in
          (match captures small subj with
          | Ok (Some c) ->
              assert_equal ~printer
                [ Some "item"; Some (string_of_int i) ]
                (group_substrings c)
          | Ok None -> assert_failure ("expected a match for " ^ subj)
          | Error e -> assert_failure ("match error: " ^ show_match_error e));
          (* Grows the cache to 21 pairs, against the 3 the small pattern needs. *)
          (match captures big "abcdefghijklmnopqrst" with
          | Ok (Some c) ->
              assert_equal ~printer:[%show: int] 21 (captures_length c);
              assert_equal ~printer expected_big (group_substrings c)
          | Ok None -> assert_failure "expected a match for the 20-group pattern"
          | Error e -> assert_failure ("match error: " ^ show_match_error e));
          (* Back to the small pattern, now on the grown cache.  Asserting the
             range rather than just a bool is deliberate: [is_match] discards
             the offsets, so it cannot see the non-capture path reading the
             wrong ovector slots. *)
          assert_equal
            ~printer:[%show: (range option, match_error) result]
            (Ok (Some { start = 0; end_ = String.length subj }))
            (find small subj >+= range_of_match)
        done

  (* Every case here runs against a cache already grown to 21 pairs, so each one
     would break if a result were sized from the cache's capacity rather than
     from the current match's capture count. *)
  let grown_cache_edge_cases ctxt =
    let grow () =
      with_pattern big_pattern (fun re ->
          match captures re "abcdefghijklmnopqrst" with
          | Ok (Some c) ->
              assert_equal ~printer:[%show: int] 21 (captures_length c)
          | Ok None -> assert_failure "expected a match for the 20-group pattern"
          | Error e -> assert_failure ("match error: " ^ show_match_error e))
    in
    let check ~pat ~subject ~len ~groups =
      with_pattern pat (fun re ->
          match captures re subject with
          | Ok (Some c) ->
              assert_equal ~printer:[%show: int]
                ~msg:(pat ^ " on " ^ subject ^ ": captures_length")
                len (captures_length c);
              assert_equal ~printer:[%show: string option list]
                ~msg:(pat ^ " on " ^ subject ^ ": groups")
                groups (group_substrings c)
          | Ok None ->
              assert_failure ("expected a match: " ^ pat ^ " on " ^ subject)
          | Error e -> assert_failure ("match error: " ^ show_match_error e))
    in
    grow ();
    (* No capture groups at all needs one pair against the cache's 21 -- the
       widest gap between capacity and capture count. *)
    check ~pat:"abc" ~subject:"xxabc" ~len:1 ~groups:[];
    (* An interior group the match left unset comes back [None] via
       PCRE2_UNSET; a trailing one via the capture count instead. *)
    check ~pat:"(x)|(y)" ~subject:"y" ~len:3 ~groups:[ None; Some "y" ];
    check ~pat:"(1)(z)?" ~subject:"1" ~len:2 ~groups:[ Some "1" ];
    (* Zero-width match on an empty subject. *)
    check ~pat:"()" ~subject:"" ~len:2 ~groups:[ Some "" ];
    grow ();
    with_pattern "abc" (fun re ->
        assert_equal ~printer:[%show: range] { start = 2; end_ = 5 }
          (match captures re "xxabc" with
          | Ok (Some c) -> range_of_captures c
          | _ -> assert_failure "expected a match for xxabc"));
    (* An offset at the very end of the subject, on a grown cache. *)
    with_pattern "a*" (fun re ->
        assert_equal
          ~printer:[%show: (range option, match_error) result]
          (Ok (Some { start = 3; end_ = 3 }))
          (find ~subject_offset:3 re "bbb" >+= range_of_match))

  (* [find] and [is_match] acquire match data too, but every other test grows
     the cache through [captures].  A fresh thread starts with an empty cache,
     so this is the only place growth driven by the non-capture path is
     observable.  Results are funnelled out rather than asserted in the worker:
     an [assert_failure] raised in a thread body is lost. *)
  let growth_via_find ctxt =
    let errors = ref [] in
    let record msg = errors := msg :: !errors in
    let worker () =
      try
        (match compile big_pattern with
        | Error _ -> record "the 20-group pattern failed to compile"
        | Ok re -> (
            match is_match re "abcdefghijklmnopqrst" with
            | Ok true -> ()
            | Ok false -> record "is_match found nothing for the 20-group pattern"
            | Error e -> record ("is_match errored: " ^ show_match_error e)));
        match compile "([a-z]+)-([0-9]+)" with
        | Error _ -> record "the small pattern failed to compile"
        | Ok re -> (
            match captures re "item-42" with
            | Ok (Some c) ->
                if captures_length c <> 3 then
                  record
                    (Printf.sprintf "captures_length = %d, want 3"
                       (captures_length c));
                if group_substrings c <> [ Some "item"; Some "42" ] then
                  record "wrong groups after growth driven by is_match"
            | Ok None -> record "expected a match for item-42"
            | Error e -> record ("captures errored: " ^ show_match_error e))
      with e -> record ("worker raised: " ^ Printexc.to_string e)
    in
    Thread.join (Thread.create worker ());
    assert_equal ~printer:[%show: string list]
      ~msg:"growth driven through the non-capture path" [] !errors

  (* Each thread caches its own match data, so matching through one shared
     compiled regexp from several threads must stay correct.  Note these are
     systhreads, which take turns holding the domain lock, so they interleave
     rather than run in parallel -- see the Domain-based test for that.  This is
     also the only coverage of the thread-exit destructor: eight threads each
     populate a cache and then die. *)
  let concurrent_matching ctxt =
    match compile "([a-z]+)-([0-9]+)-([a-z]+)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let iterations = 5_000 in
        let workers = 8 in
        let failures = Atomic.make 0 in
        let completed = Atomic.make 0 in
        let errors = Atomic.make [] in
        (* A worker killed by an exception still lets [Thread.join] return
           normally, so without recording the exception and counting completed
           iterations this test would pass having matched nothing. *)
        let record e =
          let msg = Printexc.to_string e in
          let rec push () =
            let cur = Atomic.get errors in
            if not (Atomic.compare_and_set errors cur (msg :: cur)) then push ()
          in
          push ()
        in
        let worker tid () =
          (* Letters only, since the outer groups are [a-z]+. *)
          let tag = String.make 3 (Char.chr (Char.code 'a' + tid)) in
          try
            for i = 0 to iterations - 1 do
              let subj = Printf.sprintf "left%s-%d-right%s" tag i tag in
              let expected =
                [
                  Some ("left" ^ tag); Some (string_of_int i);
                  Some ("right" ^ tag);
                ]
              in
              let ok =
                match captures re subj with
                | Ok (Some c) -> group_substrings c = expected
                | Ok None | Error _ -> false
              in
              if not ok then Atomic.incr failures;
              Atomic.incr completed
            done
          with e -> record e
        in
        let threads =
          List.init workers (fun tid -> Thread.create (worker tid) ())
        in
        List.iter Thread.join threads;
        assert_equal ~printer:[%show: string list] ~msg:"worker exceptions" []
          (Atomic.get errors);
        assert_equal ~printer:string_of_int ~msg:"iterations completed"
          (workers * iterations) (Atomic.get completed);
        assert_equal ~printer:string_of_int ~msg:"mismatched captures" 0
          (Atomic.get failures)

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
      "name_table_compaction" >:: name_table_compaction;
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
      "match_data_reuse" >:: match_data_reuse;
      "grown_cache_edge_cases" >:: grown_cache_edge_cases;
      "growth_via_find" >:: growth_via_find;
      "concurrent_matching" >:: concurrent_matching;
    ]
end

let check_version ctxt =
  assert_bool "Version is older than newest tested" (Pcre2.version >= (10, 43))

let wide_20 = "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)(m)(n)(o)(p)(q)(r)(s)(t)"
let wide_20_subject = "abcdefghijklmnopqrst"

(* One thread's match data is shared by both matchers, but [MakeTests] is
   instantiated once per matcher, so nothing else mixes them on a single thread.
   Alternating widths across the two means each call runs against a block last
   sized by the other. *)
let jit_and_interp_share_a_cache ctxt =
  let wide_24 = wide_20 ^ "(u)(v)(w)(x)" in
  let wide_24_subject = wide_20_subject ^ "uvwx" in
  let named = "(?<word>[a-z]+)-(?<num>[0-9]+)" in
  let named_subject = "item-42" in
  match
    ( Interp.compile wide_20,
      Interp.compile named,
      Jit.compile wide_24,
      Jit.compile named )
  with
  | Error e, _, _, _ | _, Error e, _, _ ->
      assert_failure ("failed to compile: " ^ Interp.show_compile_error e)
  | _, _, Error e, _ | _, _, _, Error e ->
      assert_failure ("failed to compile: " ^ Jit.show_compile_error e)
  | Ok i_wide, Ok i_named, Ok j_wide, Ok j_named ->
      for _ = 1 to 1_000 do
        (match Interp.captures i_wide wide_20_subject with
        | Ok (Some c) ->
            assert_equal ~printer:[%show: int] ~msg:"interp 20-group" 21
              (Interp.captures_length c)
        | _ -> assert_failure "expected an interp match for the 20-group pattern");
        (match Jit.captures j_named named_subject with
        | Ok (Some c) ->
            assert_equal ~printer:[%show: int] ~msg:"jit named" 3
              (Jit.captures_length c);
            assert_equal ~printer:[%show: string option] ~msg:"jit named group"
              (Some "item")
              (Jit.named_match_of_captures c "word"
              |> Option.map Jit.substring_of_match)
        | _ -> assert_failure "expected a jit match for the named pattern");
        (match Jit.captures j_wide wide_24_subject with
        | Ok (Some c) ->
            assert_equal ~printer:[%show: int] ~msg:"jit 24-group" 25
              (Jit.captures_length c)
        | _ -> assert_failure "expected a jit match for the 24-group pattern");
        match Interp.captures i_named named_subject with
        | Ok (Some c) ->
            assert_equal ~printer:[%show: int] ~msg:"interp named" 3
              (Interp.captures_length c);
            assert_equal ~printer:[%show: string option]
              ~msg:"interp named group" (Some "42")
              (Interp.named_match_of_captures c "num"
              |> Option.map Interp.substring_of_match)
        | _ -> assert_failure "expected an interp match for the named pattern"
      done

let rss_kb () =
  if Sys.os_type <> "Unix" then None
  else
    try
      let ic =
        Unix.open_process_in
          (Printf.sprintf "ps -o rss= -p %d" (Unix.getpid ()))
      in
      let line = try Some (input_line ic) with End_of_file -> None in
      ignore (Unix.close_process_in ic);
      Option.map (fun l -> int_of_string (String.trim l)) line
    with _ -> None

(* The thread-exit destructor is otherwise unobservable from OCaml: the rest of
   the suite passes with it unregistered entirely.  A block that is not freed
   shows up as resident memory growing with the number of threads that have come
   and gone, so churn threads and watch it.  Each leaked block is far larger than
   its ovector, since PCRE2 also attaches a heap frame vector.

   This is also the only check that the destructor reads the block from the
   pthread TSD value rather than from thread-local storage, which some platforms
   (macOS among them) tear down before running TSD destructors. *)
let thread_exit_frees_match_data ctxt =
  skip_if (rss_kb () = None) "cannot read RSS on this platform";
  let churn = 8_000 in
  match Interp.compile wide_20 with
  | Error e ->
      assert_failure ("failed to compile: " ^ Interp.show_compile_error e)
  | Ok re ->
      let run n =
        for _ = 1 to n do
          Thread.join
            (Thread.create
               (fun () -> ignore (Interp.captures re wide_20_subject))
               ())
        done
      in
      (* Warm up first, so the measured window excludes the main thread's own
         cache and the allocator's arena growth, which settles early. *)
      run 2_000;
      let before = Option.get (rss_kb ()) in
      run churn;
      let after = Option.get (rss_kb ()) in
      let growth = after - before in
      assert_bool
        (Printf.sprintf
           "resident memory grew %d KB across %d thread exits (%.2f KB per \
            thread); a retained match data is roughly 20 KB per thread"
           growth churn
           (float_of_int growth /. float_of_int churn))
        (growth < 20_000)

let suite =
  let module Interp_Tests = MakeTests (Interp) in
  let module Jit_Tests = MakeTests (Jit) in
  "Test pcre"
  >::: [
         "version" >:: check_version;
         "jit_and_interp_share_a_cache" >:: jit_and_interp_share_a_cache;
         "thread_exit_frees_match_data" >:: thread_exit_frees_match_data;
         "Interp" >::: Interp_Tests.tests;
         "JIT" >::: Jit_Tests.tests;
       ]

let _ = if not !Sys.interactive then run_test_tt_main suite else ()
