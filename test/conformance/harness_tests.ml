(* Minimal unit tests for the harness building blocks that do not need the
   C oracle: the subject escape decoder, the modifier decoder, and the
   unit splitter / expected-output aligner. *)

module Units = Conformance_lib.Units
module Subject = Pcre2test_harness.Subject
module Modifiers = Pcre2test_harness.Modifiers
module Pchars = Pcre2test_harness.Pchars
module Ctl = Pcre2test_harness.Ctl

let failures = ref 0

let check name cond =
  if not cond then begin
    Printf.printf "FAIL: %s\n" name;
    incr failures
  end

let decode_subject ?(utf = false) s =
  let msgs = ref [] in
  let len = Pcre2test_harness.Cstr.rstrip_len s in
  let start = Pcre2test_harness.Cstr.skip_space ~limit:len s 0 in
  match
    Subject.decode ~utf ~subject_literal:false
      ~emit:(fun m -> msgs := m :: !msgs)
      s ~start ~len
  with
  | None -> Error (List.rev !msgs)
  | Some r -> Ok (r.Subject.subject, r.Subject.mods_from, List.rev !msgs)

(* ---------------- escape decoder ---------------- *)

let () =
  (* plain text, leading spaces stripped, trailing whitespace stripped *)
  (match decode_subject "    abc   \n" with
  | Ok ("abc", None, []) -> ()
  | _ -> check "plain subject" false);
  (* standard escapes *)
  (match decode_subject "    a\\tb\\n\\x41\\0759;" with
  | Ok ("a\tb\nA=9;", None, []) -> ()
  | Ok (s, _, _) -> check (Printf.sprintf "escapes got %S" s) false
  | Error _ -> check "escapes" false);
  (* \x{..} wide char in non-UTF mode: warning + truncation *)
  (match decode_subject "    \\x{123}" with
  | Ok ("\x23", None, [ m1; _m2 ]) ->
      check "wide warn text"
        (String.equal m1
           "** Character \\x{123} is greater than 255 and UTF-8 mode is not \
            enabled.")
  | _ -> check "wide char truncation" false);
  (* \x{..} in UTF mode: encoded as UTF-8 *)
  (match decode_subject ~utf:true "    \\x{123}" with
  | Ok ("\xc4\xa3", None, []) -> ()
  | _ -> check "utf-8 encoding" false);
  (* replication *)
  (match decode_subject "    \\[ab]{3}c" with
  | Ok ("abababc", None, []) -> ()
  | Ok (s, _, _) -> check (Printf.sprintf "replication got %S" s) false
  | Error _ -> check "replication" false);
  (* \= starts modifiers *)
  (match decode_subject "    abc\\=notbol" with
  | Ok ("abc", Some i, []) -> check "mods index" (i = 9)
  | _ -> check "mods split" false);
  (* unrecognized alphanumeric escape is fatal *)
  (match decode_subject "    a\\yb" with
  | Error [ m ] ->
      check "bad escape text"
        (String.equal m "** Unrecognized escape sequence \"\\y\"")
  | _ -> check "bad escape" false);
  (* escaped non-alnum char comes through *)
  (match decode_subject "    \\$\\?" with
  | Ok ("$?", None, []) -> ()
  | _ -> check "escaped punctuation" false)

(* ---------------- modifier decoder ---------------- *)

let () =
  let decode_pat s =
    let pat = Ctl.new_patctl () in
    let msgs = ref [] in
    let skips = ref [] in
    let ok =
      Modifiers.decode ~ctx:Modifiers.CtxPat ~pctl:(Some pat) ~dctl:None
        ~restrict_perl:false
        ~emit:(fun m -> msgs := m :: !msgs)
        ~skip:(fun r -> skips := r :: !skips)
        s
    in
    (ok, pat, List.rev !msgs, List.rev !skips)
  in
  let ok, pat, msgs, skips = decode_pat "xi\n" in
  check "xi ok" (ok && msgs = [] && skips = []);
  check "xi bits"
    (pat.Ctl.options
    = Pcre2test_harness.Flags.extended lor Pcre2test_harness.Flags.caseless);
  let ok, pat, _, _ = decode_pat "xx\n" in
  check "xx -> extended_more"
    (ok && pat.Ctl.options = Pcre2test_harness.Flags.extended_more);
  let ok, pat, _, _ = decode_pat "g,dupnames\n" in
  check "g,dupnames"
    (ok
    && pat.Ctl.control land Ctl.ctl_global <> 0
    && pat.Ctl.options land Pcre2test_harness.Flags.dupnames <> 0);
  let ok, _, msgs, _ = decode_pat "nonsense_mod\n" in
  check "unknown modifier fails" (not ok);
  check "unknown modifier message"
    (match msgs with
    | m :: _ ->
        (* single-char parse rejects at the first bad character *)
        String.length m > 0 && String.sub m 0 2 = "**"
    | [] -> false);
  let ok, _, _, skips = decode_pat "jit\n" in
  check "jit recognized as skip" (ok && skips = [ "modifier:jit" ]);
  let ok, pat, _, _ = decode_pat "newline=CRLF\n" in
  check "newline=CRLF"
    (ok
    && pat.Ctl.ctx_newline = Pcre2test_harness.Flags.newline_crlf
    && pat.Ctl.control2 land Ctl.ctl2_nl_set <> 0)

(* ---------------- splitter / aligner ---------------- *)

let () =
  let input =
    [
      "# comment\n";
      "\n";
      "/abc/\n";
      "    abc\n";
      "\\= Expect no match\n";
      "    xyz\n";
      "\n";
      "/d(e)f/g\n";
      "    def\n";
      "\n";
    ]
  in
  let units = Units.split input in
  check "split count" (List.length units = 2);
  (match units with
  | [ u1; u2 ] ->
      check "u1 ordinal" (u1.Units.ordinal = 1);
      check "u1 pattern" (String.equal u1.Units.pattern_line "/abc/");
      check "u1 lines" (List.length u1.Units.lines = 7);
      check "u2 ordinal" (u2.Units.ordinal = 2);
      check "u2 lines" (List.length u2.Units.lines = 3)
  | _ -> check "split shape" false);
  (* multi-line pattern *)
  let units2 = Units.split [ "/ab\n"; "cd/i\n"; "    x\n"; "\n" ] in
  check "continued pattern is one unit" (List.length units2 = 1);
  (* aligner: result lines equal to input lines elsewhere must not confuse
     block boundaries *)
  let expected =
    [
      "# comment\n";
      "\n";
      "/abc/\n";
      "    abc\n";
      " 0: abc\n";
      "\\= Expect no match\n";
      "    xyz\n";
      "No match\n";
      "\n";
      "/d(e)f/g\n";
      "    def\n";
      " 0: def\n";
      " 1: e\n";
      "\n";
    ]
  in
  (match Units.align units expected with
  | Ok [ b1; b2 ] ->
      check "block1" (List.length b1 = 9);
      check "block2 head" (match b2 with l :: _ -> String.equal l "/d(e)f/g" | [] -> false)
  | Ok _ -> check "align blocks" false
  | Error e -> check ("align error: " ^ e) false);
  (* misalignment must be detected *)
  (match Units.align units [ "/different/\n" ] with
  | Error _ -> ()
  | Ok _ -> check "align must fail on mismatch" false)

let () =
  if !failures > 0 then begin
    Printf.printf "%d harness unit test(s) failed\n" !failures;
    exit 1
  end
  else print_endline "harness unit tests passed"
