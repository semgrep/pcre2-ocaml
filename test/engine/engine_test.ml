(* Engine module-initialization asserts for [Pcre2_engine.Engine], migrated
   verbatim from src/engine/engine.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Engine

(* End-to-end boundary checks over the wired compile pipeline. Expected
   values verified against pcre2test on the real 10.44 library
   (testoutput2:128-129, 484-488 shapes). *)
let test_0 () =
  (* Successful compile: build-default info values. *)
  (match compile "abc" 0l with
  | Ok re ->
      let i = info re in
      assert (Int.equal i.argoptions 0);
      assert (Int.equal i.alloptions 0);
      assert (Int.equal i.newline Options.newline_lf);
      assert (Int.equal i.bsr Options.bsr_unicode);
      assert (Int.equal i.capture_count 0);
      assert (Int.equal (Array.length (capture_groups re)) 0)
  | Result.Error _ -> assert false);
  (* Compile errors with exact code and offset. *)
  (match compile_ctx "(" 0l with
  | Result.Error (e, o) ->
      assert (Int.equal e Errors.err14);
      assert (Int.equal o 1)
  | Ok _ -> assert false);
  (match compile_ctx "a{2,1}" 0l with
  | Result.Error (e, o) ->
      assert (Int.equal e Errors.err4);
      assert (Int.equal o 5)
  | Ok _ -> assert false);
  (* Unknown option bits: 117 at the raw int seam (port-conventions §5). *)
  (match compile "a" 0x08000000l with
  | Result.Error (e, _) -> assert (Int.equal e Errors.err17)
  | Ok _ -> assert false);
  (* A leading (?i) reaches neither options word (pcre2test shows no
     "Options:" line for /(?i)abc/I, testoutput2:484-488). *)
  (match compile "(?i)abc" 0l with
  | Ok re ->
      let i = info re in
      assert (Int.equal i.argoptions 0);
      assert (Int.equal i.alloptions 0)
  | Result.Error _ -> assert false);
  (* Caseless as an option is ARGOPTIONS and ALLOPTIONS. *)
  (match compile "abc" (Int32.of_int Options.caseless) with
  | Ok re ->
      let i = info re in
      assert (Int.equal i.argoptions Options.caseless);
      assert (Int.equal i.alloptions Options.caseless)
  | Result.Error _ -> assert false);
  (* ( *CR) in-pattern newline setting. *)
  (match compile "(*CR)a" 0l with
  | Ok re -> assert (Int.equal (info re).newline Options.newline_cr)
  | Result.Error _ -> assert false);
  (* compile_ctx newline/bsr knobs; 0 = build default. *)
  (match
     compile_ctx ~newline:Options.newline_crlf ~bsr:Options.bsr_anycrlf "a" 0l
   with
  | Ok re ->
      let i = info re in
      assert (Int.equal i.newline Options.newline_crlf);
      assert (Int.equal i.bsr Options.bsr_anycrlf)
  | Result.Error _ -> assert false);
  (* Invalid newline value from the context: the driver's ERR56. *)
  (match compile_ctx ~newline:99 "a" 0l with
  | Result.Error (e, o) ->
      assert (Int.equal e Errors.err56);
      assert (Int.equal o 0)
  | Ok _ -> assert false);
  (* Name table order and numbers at the boundary. *)
  (match compile "(?<x>a)(?<y>b)" 0l with
  | Ok re -> (
      let i = info re in
      assert (Int.equal i.capture_count 2);
      let nt = capture_groups re in
      assert (Int.equal (Array.length nt) 2);
      match (nt.(0), nt.(1)) with
      | (n0, g0), (n1, g1) ->
          assert (String.equal n0 "x");
          assert (Int.equal g0 1);
          assert (String.equal n1 "y");
          assert (Int.equal g1 2))
  | Result.Error _ -> assert false);
  (* The wired match boundary (Interpreter.pcre2_match via exec_full).
     Expected values verified against pcre2test on the real 10.44
     library. *)
  match compile "(a)(b)?" 0l with
  | Result.Error _ -> assert false
  | Ok re -> (
      (* Bump-along match: exec folds to the group-0 pair. *)
      (match exec re "xab" 0 0l with
      | Ok (Some (1, 3)) -> ()
      | _ -> assert false);
      (* exec_full: rc = 2 pairs -> ovector length 4; startchar = attempt
         start. *)
      (match exec_full re "xa" 0 0l with
      | Match { ovector; mark = None; start_char = 1 } ->
          assert (Int.equal (Array.length ovector) 4);
          assert (Int.equal ovector.(0) 1);
          assert (Int.equal ovector.(1) 2);
          assert (Int.equal ovector.(2) 1);
          assert (Int.equal ovector.(3) 2)
      | _ -> assert false);
      (* exec_captures pads unset tail groups with (-1, -1)
         (port-conventions §5). *)
      (match exec_captures re "a" 0 0l with
      | Ok (Some (pairs, _)) -> (
          assert (Int.equal (Array.length pairs) 3);
          match pairs with
          | [| (0, 1); (0, 1); (-1, -1) |] -> ()
          | _ -> assert false)
      | _ -> assert false);
      (* NOMATCH -> Ok None. *)
      (match exec re "x" 0 0l with Ok None -> () | _ -> assert false);
      (* PARTIAL -> Ok None at the exec seam (§5), Partial at exec_full.
         Hard partial returns as soon as the subject end is hit mid-attempt
         (oracle: "Partial match: a"), while soft partial lets the complete
         match win (oracle: 0: a, 1: a). *)
      (match exec re "xa" 0 (Int32.of_int Options.partial_hard) with
      | Ok None -> ()
      | _ -> assert false);
      (match exec_full re "xa" 0 (Int32.of_int Options.partial_hard) with
      | Partial { start = 1; mark = None } -> ()
      | _ -> assert false);
      (match exec re "xa" 0 (Int32.of_int Options.partial_soft) with
      | Ok (Some (1, 2)) -> ()
      | _ -> assert false);
      (match compile "abc" 0l with
      | Result.Error _ -> assert false
      | Ok re3 -> (
          (match exec re3 "xab" 0 (Int32.of_int Options.partial_hard) with
          | Ok None -> ()
          | _ -> assert false);
          match exec_full re3 "xab" 0 (Int32.of_int Options.partial_hard) with
          | Partial { start = 1; mark = None } -> ()
          | _ -> assert false));
      (* BADOFFSET (-33) for negative or beyond-end offsets; unknown match
         option bits -> BADOPTION (-34) (§5). *)
      (match exec re "a" 5 0l with
      | Result.Error e -> assert (Int.equal e Errors.error_badoffset)
      | Ok _ -> assert false);
      (match exec re "a" (-1) 0l with
      | Result.Error e -> assert (Int.equal e Errors.error_badoffset)
      | Ok _ -> assert false);
      match exec re "a" 0 0x00000100l with
      | Result.Error e -> assert (Int.equal e Errors.error_badoption)
      | Ok _ -> assert false)

(* PCRE2_NO_UTF_CHECK with an invalid UTF pattern: the C documents this as
   undefined; the port pins it to defined VALUES (never an escaping
   exception, port-conventions §5/§6) via Utf.peek's clamped reads and
   Ucd.record_index's code-point clamp — both DEVIATION-documented at
   their definitions. With the check ON, the valid_utf error codes surface
   unchanged. All option combinations here are public compile bits, so
   every case is reachable end-to-end through this seam. *)
let test_1 () =
  let uopts = Int32.of_int (Options.utf lor Options.no_utf_check) in
  let copts =
    Int32.of_int (Options.utf lor Options.no_utf_check lor Options.caseless)
  in
  (* Truncated 2-byte lead at pattern end: decodes with a clamped 0
     continuation byte (= the C's NUL read) to U+00C0 and compiles. *)
  (match compile "\xc3" uopts with
  | Ok _ -> ()
  | Result.Error _ -> assert false);
  (* 5-byte form: decodes to 0x200000 (> MAX_UTF_CODE_POINT), compiles as
     an ord2utf-encoded literal; with caseless, the UCD lookups hit the
     record_index clamp instead of overrunning stage1. *)
  (match compile "\xf8\x88\x80\x80\x80" uopts with
  | Ok _ -> ()
  | Result.Error _ -> assert false);
  (match compile "\xf8\x88\x80\x80\x80" copts with
  | Ok _ -> ()
  | Result.Error _ -> assert false);
  (* Caseless classes containing the garbage code point: the one-char
     class caseset probe, and a range walked by get_othercase_range. *)
  (match compile "[\xf8\x88\x80\x80\x80]" copts with
  | Ok _ -> ()
  | Result.Error _ -> assert false);
  (match compile "[\xf8\x88\x80\x80\x80-\xf8\x88\x80\x80\x81]" copts with
  | Ok _ -> ()
  | Result.Error _ -> assert false);
  (* Caseless class with the truncated tail: the clamped decode eats the
     missing continuation byte and the unterminated class is ERR6, with
     the same past-the-end offset the C reports after reading its NUL. *)
  (match compile_ctx "(?i)[\xc3" uopts with
  | Result.Error (e, o) ->
      assert (Int.equal e Errors.err6);
      assert (Int.equal o 7)
  | Ok _ -> assert false);
  (* WITH the UTF check (no NO_UTF_CHECK): the PRIV(valid_utf) error is
     unchanged (testoutput10:9 shape). *)
  match compile "\xc3(" (Int32.of_int Options.utf) with
  | Result.Error (e, _) -> assert (Int.equal e Errors.error_utf8_err6)
  | Ok _ -> assert false

(* The fuzz repro pinned by fuzz/corpus/regressions/204387-b7915f8f.txt:
   PCRE2_MATCH_INVALID_UTF's bad-start skip (pcre2_match.c:6829-6851)
   moves matching past the invalid leading code unit, and the startline
   bump-along scan (pcre2_match.c:7318-7349) then probes WAS_NEWLINE at
   the first valid position — where C 10.44's BACKCHAR walks out of the
   subject (see the DEVIATION note in Newline.was_newline). Expected
   values are the real 10.44 oracle's recorded result: rc = -1 (NOMATCH),
   all ovector slots unset, no mark. *)
let test_2 () =
  let copts = Int32.of_int (Options.match_invalid_utf lor Options.multiline) in
  match
    compile_ctx ~newline:Options.newline_anycrlf ~extra:Options.extra_match_line
      "(*LIMIT_MATCH=80000)()()((()()))" copts
  with
  | Result.Error _ -> assert false
  | Ok re -> (
      match exec_full re "\xb9t" 0 0l with
      | No_match { mark = None } -> ()
      | _ -> assert false)

(* The fuzz repro pinned by fuzz/corpus/regressions/125828-1cfb496a.txt:
   OP_VREVERSE's per-character back-step `Feptr--; BACKCHAR(Feptr)`
   (pcre2_match.c:5854-5855) walks below the subject under
   PCRE2_MATCH_INVALID_UTF when the subject starts with UTF-8
   continuation bytes; the engine bounds the walk at the subject start
   and caps the step when it would cross (see the DEVIATION note on
   Interpreter.op_vreverse_utf_loop; the dev oracle carries the matching
   patch). Hand-minimized form: the lookbehind at the fragment start
   (offset 1, after the bad-start skip) must NOT step back into the
   invalid prefix, so (.)? matches empty and group 1 stays unset: rc = 1,
   group 0 = (1,1). Expected values are the patched 10.44 oracle's
   recorded result. *)
let test_3 () =
  let copts = Int32.of_int Options.match_invalid_utf in
  (match compile "(?<=(.)?)" copts with
  | Result.Error _ -> assert false
  | Ok re -> (
      match exec_full re "\x80" 0 0l with
      | Match { ovector = [| 1; 1 |]; mark = None; start_char = 1 } -> ()
      | _ -> assert false));
  (* The full discovery-time shape (fuzz seed 20260708 case 125828): the
     BRAZERO'd capture group inside the lookbehind — with its nested
     group and ( *ACCEPT)-terminated lookahead — is skipped, not matched
     against the invalid 0x80 byte, so groups 1-2 stay unset while the
     zero-width groups 6-9 record (1,1). Expected ovector is the 10.44
     oracle's recorded result (rc = 10 pairs). *)
  match
    compile
      "(*LIMIT_MATCH=80000)(?<=(((?:[\\PL[:lower:]]{1}))(*positive_lookahead:(*ACCEPT)()))?(?!()H())(?!Z)()((?=()())))"
      copts
  with
  | Result.Error _ -> assert false
  | Ok re -> (
      match exec_full re "\x80" 0 0l with
      | Match
          {
            ovector =
              [|
                1;
                1;
                -1;
                -1;
                -1;
                -1;
                -1;
                -1;
                -1;
                -1;
                -1;
                -1;
                1;
                1;
                1;
                1;
                1;
                1;
                1;
                1;
              |];
            mark = None;
            start_char = 1;
          } ->
          ()
      | _ -> assert false)

(* The defined-behavior corner that forbids a check_subject floor on the
   OP_VREVERSE walk (found in fidelity review of the 125828 fix): 10.44's
   max_lookbehind does not count nested lookbehinds ("A nested lookbehind
   does not contribute any length", pcre2_compile.c:9604-9612), so with a
   start offset beyond max_lookbehind (check_subject = start_match -
   max_lookbehind, pcre2_match.c:6851-6872) an inner lookbehind
   legitimately walks below check_subject on fully valid UTF — every read
   in bounds, no UB. Real C matches the inner a{3,4} max-first from
   offset 0: group 1 = (0,4), the empty overall match at 5. Verified
   against the patched oracle (whose pin never fires on valid data) AND
   an unpatched real pcre2test (10.46 prints "1: aaaa", four chars =
   start 0; on the UB repro above it prints group 1 offsets 0x1
   0xffffffffffffffff, confirming the family). *)
let test_4 () =
  match compile "(?<=(?<=(a{3,4}))a)" (Int32.of_int Options.utf) with
  | Result.Error _ -> assert false
  | Ok re -> (
      match exec_full re "aaaaaa" 5 0l with
      | Match { ovector = [| 5; 5; 0; 4 |]; mark = None; start_char = 5 } -> ()
      | _ -> assert false)

(* The option-independent (plain-UTF) route through the same OP_VREVERSE
   pin: with only PCRE2_UTF and a nonzero start offset, valid_utf checks
   the subject only from check_subject (pcre2_match.c:6891) — here
   start_match(7) - max_lookbehind(5) = 2 — so the all-continuation
   prefix "\x80\x80" below it goes unchecked, and the inner lookbehind's
   OP_VREVERSE (below check_subject via the max_lookbehind undercount,
   pcre2_compile.c:9604-9612) walks into it: at the fifth back-step from
   offset 2 the bounded BACKCHAR lands on the continuation byte at 0
   (unpatched C reads subject[-1]) and the pin caps Lmax at 4, so the
   branch is tried from offset 2: a{3,5} takes the four a's (2,6), the
   inner assertion ends at 6, the outer matches "a" to 7. Expected
   values are the patched 10.44 oracle's recorded result, and the
   final result also agrees with unpatched real pcre2test (10.46: "1:
   aaaa" — its doomed subject-1 candidate fails and it converges on the
   same answer; only the OOB read differs). *)
let test_5 () =
  match compile "(?<=(?<=(a{3,5}))a)" (Int32.of_int Options.utf) with
  | Result.Error _ -> assert false
  | Ok re -> (
      match exec_full re "\x80\x80aaaaaa" 7 0l with
      | Match { ovector = [| 7; 7; 2; 6 |]; mark = None; start_char = 7 } -> ()
      | _ -> assert false)

(* The word-boundary previous-character probe below check_subject (the
   pinned form of pcre2_match.c:6269-6277, whose only guard is Feptr ==
   mb->check_subject — see the DEVIATION note on the word-boundary arm in
   Interpreter). Two shapes, both engine == patched oracle:

   1. Fully valid UTF: the inner OP_VREVERSE walks to offset 0 <
      check_subject (= 5 - max_lookbehind(4) = 1, nested-lookbehind
      undercount), so \b runs at eptr = 0 with eptr <> check_subject and
      the C computes lastptr = subject - 1 — an out-of-bounds read (the
      padded oracle reads its slack 0). Pinned: lastptr = -1 reads fc = 0
      (non-word), 'a' at 0 is a word char, the boundary holds, and the
      inner branch matches a{3,4} max-first: group 1 = (0,4). *)
let test_6 () =
  match compile "(?<=(?<=\\b(a{3,4}))a)" (Int32.of_int Options.utf) with
  | Result.Error _ -> assert false
  | Ok re -> (
      match exec_full re "aaaaaa" 5 0l with
      | Match { ovector = [| 5; 5; 0; 4 |]; mark = None; start_char = 5 } -> ()
      | _ -> assert false)

(* 2. The pre-fix OBSERVABLE divergence (UCP): subject \xb5\xf8a under
      MATCH_INVALID_UTF has fragment start (= check_subject) 2; the
      lookbehind's OP_VREVERSE lands on the invalid non-continuation
      byte at 1, and \b (OP_UCP_WORD_BOUNDARY) probes the previous
      character: the bounded walk from offset 0 lands on the
      continuation byte \xb5 — C's unbounded BACKCHAR crosses to
      subject[-1] (slack 0 in the oracle, fc = 0, non-word). The
      engine's former 0-clamp read fc = 0xb5 = U+00B5 MICRO SIGN, a UCP
      letter, flipping prev_is_word and matching the branch from 1
      (group 1 = (1,2)) where the oracle matched from 2 — the divergence
      this pin fixes. Pinned: the crossing resolves to lastptr = -1,
      fc = 0, the branch from 1 fails at \b, and the shorter candidate
      wins: group 1 = (2,2), empty match at 3. *)
let test_7 () =
  let copts =
    Int32.of_int (Options.utf lor Options.ucp lor Options.match_invalid_utf)
  in
  match compile "(?<=\\b(.{0,2})a)" copts with
  | Result.Error _ -> assert false
  | Ok re -> (
      match exec_full re "\xb5\xf8a" 0 0l with
      | Match { ovector = [| 3; 3; 2; 2 |]; mark = None; start_char = 3 } -> ()
      | _ -> assert false)

let tests =
  [
    Alcotest.test_case "engine 0" `Quick test_0;
    Alcotest.test_case "engine 1" `Quick test_1;
    Alcotest.test_case "engine 2" `Quick test_2;
    Alcotest.test_case "engine 3" `Quick test_3;
    Alcotest.test_case "engine 4" `Quick test_4;
    Alcotest.test_case "engine 5" `Quick test_5;
    Alcotest.test_case "engine 6" `Quick test_6;
    Alcotest.test_case "engine 7" `Quick test_7;
  ]
