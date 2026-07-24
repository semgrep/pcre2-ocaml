(* Per-call match-context limits on the public Matcher surface (chunk 3 of the
   per-call-limits plan). The three optional args ?match_limit / ?depth_limit /
   ?heap_limit were added to find / find_iter / captures / captures_iter in
   Pcre2_matcher.Intf.Matcher; they are combined with a pattern's ( *LIMIT_*=)
   verb by taking the smaller value (Limits.resolve_limit, pcre2_match.c:7036-
   7046) and default to the build limits (Limits, pcre2_context.c:166-179).

   The suite is ONE functor over Pcre2_matcher.Intf.Matcher instantiated for
   Pcre2.Interp and Pcre2.Jit, so every assertion is exercised against both
   engines (they must agree — the interpreter is the oracle). Because
   Pcre2.match_error is abstract in the frozen public surface (src/pcre2.mli:168;
   distinct from Pcre2_matcher.Error.match_error), the functor stays over the
   plain Matcher signature (match_error abstract) and takes the three expected
   error values as a per-instance parameter, comparing with the
   signature-provided [equal_match_error]. *)

module Make
    (M : Pcre2_matcher.Intf.Matcher) (P : sig
      val matchlimit : M.match_error
      val depthlimit : M.match_error
      val heaplimit : M.match_error
    end) =
struct
  (* [match_error] is abstract in the Matcher sig, but its [@@deriving eq/show]
     surface [equal_match_error]/[pp_match_error] into the sig, so we can build
     an Alcotest testable without knowing the concrete constructors. *)
  let match_error_t : M.match_error Alcotest.testable =
    Alcotest.testable M.pp_match_error M.equal_match_error

  let compile pat =
    match M.compile pat with
    | Ok re -> re
    | Error _ -> Alcotest.failf "compile failed for %S" pat

  (* A pattern with real backtracking work that all three engines accept:
     (a)*b grabs every 'a', then matches 'b'. Under the build defaults it
     completes; a limit of 0 trips before it can. *)
  let re_body = "(a)*b"
  let re_subj = "aaab"

  (* find / captures with each limit at 0 trip the corresponding error: 0 is
     min'd to the effective limit, so match_limit fails on the first
     backtracking tick (-47), depth_limit at the frame-0 rdepth check (-53),
     heap_limit inside the initial frames-vector sizing (-63). *)
  let find_limit_tests =
    [
      Alcotest.test_case "find ~match_limit:0 -> MATCHLIMIT" `Quick (fun () ->
          let re = compile re_body in
          match M.find ~match_limit:0 re re_subj with
          | Error e -> Alcotest.check match_error_t "matchlimit" P.matchlimit e
          | Ok _ -> Alcotest.fail "expected MATCHLIMIT, got Ok");
      Alcotest.test_case "find ~depth_limit:0 -> DEPTHLIMIT" `Quick (fun () ->
          let re = compile re_body in
          match M.find ~depth_limit:0 re re_subj with
          | Error e -> Alcotest.check match_error_t "depthlimit" P.depthlimit e
          | Ok _ -> Alcotest.fail "expected DEPTHLIMIT, got Ok");
      Alcotest.test_case "find ~heap_limit:0 -> HEAPLIMIT" `Quick (fun () ->
          let re = compile re_body in
          match M.find ~heap_limit:0 re re_subj with
          | Error e -> Alcotest.check match_error_t "heaplimit" P.heaplimit e
          | Ok _ -> Alcotest.fail "expected HEAPLIMIT, got Ok");
      (* Trip ORDERING: with all three at 0 the frames-vector sizing (heap)
         runs before any match/depth tick, so heap trips first (-63). *)
      Alcotest.test_case "find all-limits:0 -> HEAPLIMIT (heap first)" `Quick
        (fun () ->
          let re = compile re_body in
          match
            M.find ~match_limit:0 ~depth_limit:0 ~heap_limit:0 re re_subj
          with
          | Error e ->
              Alcotest.check match_error_t "heap trips first" P.heaplimit e
          | Ok _ -> Alcotest.fail "expected HEAPLIMIT, got Ok");
    ]

  let captures_limit_tests =
    [
      Alcotest.test_case "captures ~match_limit:0 -> MATCHLIMIT" `Quick
        (fun () ->
          let re = compile re_body in
          match M.captures ~match_limit:0 re re_subj with
          | Error e -> Alcotest.check match_error_t "matchlimit" P.matchlimit e
          | Ok _ -> Alcotest.fail "expected MATCHLIMIT, got Ok");
      Alcotest.test_case "captures ~depth_limit:0 -> DEPTHLIMIT" `Quick
        (fun () ->
          let re = compile re_body in
          match M.captures ~depth_limit:0 re re_subj with
          | Error e -> Alcotest.check match_error_t "depthlimit" P.depthlimit e
          | Ok _ -> Alcotest.fail "expected DEPTHLIMIT, got Ok");
      Alcotest.test_case "captures ~heap_limit:0 -> HEAPLIMIT" `Quick (fun () ->
          let re = compile re_body in
          match M.captures ~heap_limit:0 re re_subj with
          | Error e -> Alcotest.check match_error_t "heaplimit" P.heaplimit e
          | Ok _ -> Alcotest.fail "expected HEAPLIMIT, got Ok");
    ]

  (* find_iter / captures_iter surface a tripping limit as an Error element of
     the sequence. The limit trips on the first attempt, so the Error is the
     head element; we take just it (the frozen iter re-searches at end-of-
     subject after an error, so a tripping limit would re-trip forever — never
     forced here, port-conventions.md par. 5). *)
  let iter_limit_tests =
    [
      Alcotest.test_case "find_iter ~match_limit:0 -> Error element" `Quick
        (fun () ->
          let re = compile re_body in
          match
            M.find_iter ~match_limit:0 re re_subj |> Seq.take 1 |> List.of_seq
          with
          | [ Error e ] ->
              Alcotest.check match_error_t "matchlimit" P.matchlimit e
          | [ Ok _ ] -> Alcotest.fail "expected Error element, got Ok"
          | _ -> Alcotest.fail "expected exactly one (Error) element");
      Alcotest.test_case "captures_iter ~depth_limit:0 -> Error element" `Quick
        (fun () ->
          let re = compile re_body in
          match
            M.captures_iter ~depth_limit:0 re re_subj
            |> Seq.take 1 |> List.of_seq
          with
          | [ Error e ] ->
              Alcotest.check match_error_t "depthlimit" P.depthlimit e
          | [ Ok _ ] -> Alcotest.fail "expected Error element, got Ok"
          | _ -> Alcotest.fail "expected exactly one (Error) element");
    ]

  (* Omitting the args reproduces the pre-arg behavior: the backtracking
     pattern completes under the build defaults. *)
  let default_tests =
    [
      Alcotest.test_case "find (no limit args) matches under defaults" `Quick
        (fun () ->
          let re = compile re_body in
          match M.find re re_subj with
          | Ok (Some m) ->
              Alcotest.(check string)
                "full match" "aaab" (M.substring_of_match m)
          | Ok None -> Alcotest.fail "expected a match under defaults"
          | Error _ -> Alcotest.fail "unexpected error under defaults");
      (* A per-call limit generous enough to leave the default behavior intact
         still matches. *)
      Alcotest.test_case "find ~match_limit:huge matches" `Quick (fun () ->
          let re = compile re_body in
          match M.find ~match_limit:1_000_000 re re_subj with
          | Ok (Some _) -> ()
          | Ok None -> Alcotest.fail "expected a match"
          | Error _ -> Alcotest.fail "unexpected error");
      (* A negative arg is its C uint32 wraparound (0xFFFF_FFFF), effectively
         unlimited, so the match runs. *)
      Alcotest.test_case "find ~match_limit:-1 (uint32 wrap) matches" `Quick
        (fun () ->
          let re = compile re_body in
          match
            M.find ~match_limit:(-1) ~depth_limit:(-1) ~heap_limit:(-1) re
              re_subj
          with
          | Ok (Some _) -> ()
          | Ok None -> Alcotest.fail "expected a match"
          | Error _ -> Alcotest.fail "unexpected error");
    ]

  (* Per-call arg vs pattern ( *LIMIT_MATCH=) verb: the effective limit is the
     MIN of the two. The six-branch alternation matching "f" needs 7 ticks,
     tripping at 6 (matches the fast/interp boundary sweeps). *)
  let verb_body = "a|b|c|d|e|f"
  let verb_subj = "f"

  let verb_min_tests =
    [
      Alcotest.test_case "per-call tighter than verb trips" `Quick (fun () ->
          let re = compile ("(*LIMIT_MATCH=1000000)" ^ verb_body) in
          match M.find ~match_limit:6 re verb_subj with
          | Error e ->
              Alcotest.check match_error_t "per-call 6 wins" P.matchlimit e
          | Ok _ -> Alcotest.fail "expected MATCHLIMIT (per-call tighter)");
      Alcotest.test_case "verb tighter than per-call trips" `Quick (fun () ->
          let re = compile ("(*LIMIT_MATCH=6)" ^ verb_body) in
          match M.find ~match_limit:1_000_000 re verb_subj with
          | Error e ->
              Alcotest.check match_error_t "verb 6 wins" P.matchlimit e
          | Ok _ -> Alcotest.fail "expected MATCHLIMIT (verb tighter)");
      Alcotest.test_case "both above the trip point -> match" `Quick (fun () ->
          let re = compile ("(*LIMIT_MATCH=1000000)" ^ verb_body) in
          match M.find ~match_limit:7 re verb_subj with
          | Ok (Some m) ->
              Alcotest.(check string) "matches 'f'" "f" (M.substring_of_match m)
          | Ok None -> Alcotest.fail "expected a match at limit 7"
          | Error _ -> Alcotest.fail "unexpected error at limit 7");
    ]

  let tests =
    find_limit_tests @ captures_limit_tests @ iter_limit_tests @ default_tests
    @ verb_min_tests
end

module Interp_tests =
  Make
    (Pcre2.Interp)
    (struct
      let matchlimit = Pcre2.MATCHLIMIT
      let depthlimit = Pcre2.DEPTHLIMIT
      let heaplimit = Pcre2.HEAPLIMIT
    end)

module Jit_tests =
  Make
    (Pcre2.Jit)
    (struct
      let matchlimit = Pcre2.MATCHLIMIT
      let depthlimit = Pcre2.DEPTHLIMIT
      let heaplimit = Pcre2.HEAPLIMIT
    end)

let () =
  Alcotest.run "pcre2_limits"
    [
      ("interp per-call limits", Interp_tests.tests);
      ("jit per-call limits", Jit_tests.tests);
    ]
