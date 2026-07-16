(* White-box tests for the fast engine's IR / IR compiler / static verifier
   (M11 chunk C1). Four suites:
     1. IR golden dumps (inline expected strings) for representative patterns.
     2. Unsupported-reason exact strings for out-of-subset constructs.
     3. Verifier: [check] passes on every supported compile; hand-corrupted
        IR (patched int array) fails.
     4. Sweep: for a mix of ~40 patterns, if [Ir_compile.compile] succeeds
        then [Ir_verify.check] must pass. *)

module Ir = Pcre2_fast.Ir
module Ir_compile = Pcre2_fast.Ir_compile
module Ir_verify = Pcre2_fast.Ir_verify
module C = Pcre2_engine.Compile

(* ---------- helpers ---------- *)

let compile_re (pat : string) : C.re =
  match C.pcre2_compile pat ~options:0 with
  | Ok re -> re
  | Error (code, off) ->
      Alcotest.failf "pattern %S failed to compile: error %d at offset %d" pat
        code off

let ir_of (pat : string) : Ir.t =
  match Ir_compile.compile (compile_re pat) with
  | Ok ir -> ir
  | Error r -> Alcotest.failf "pattern %S unexpectedly Unsupported: %s" pat r

let dump_of (pat : string) : string = Format.asprintf "%a" Ir.dump (ir_of pat)

(* Build an expected dump from a list of lines (each gets a trailing \n, as
   [Ir.dump] emits). *)
let g (lines : string list) : string =
  String.concat "" (List.map (fun s -> s ^ "\n") lines)

(* ---------- 1. golden dumps ---------- *)

let goldens : (string * string) list =
  [
    ("", g [ "  0 BRA"; "  1 KET"; "  2 END" ]);
    ("a", g [ "  0 BRA"; "  1 CHAR_RUN \"a\""; "  4 KET"; "  5 END" ]);
    ("abc", g [ "  0 BRA"; "  1 CHAR_RUN \"abc\""; "  4 KET"; "  5 END" ]);
    ( "a|b",
      g
        [
          "  0 BRA";
          "  1 ALT next=8";
          "  3 CHAR_RUN \"a\"";
          "  6 JMP 11";
          "  8 CHAR_RUN \"b\"";
          " 11 KET";
          " 12 END";
        ] );
    ( "a|bc|def",
      g
        [
          "  0 BRA";
          "  1 ALT next=8";
          "  3 CHAR_RUN \"a\"";
          "  6 JMP 18";
          "  8 ALT next=15";
          " 10 CHAR_RUN \"bc\"";
          " 13 JMP 18";
          " 15 CHAR_RUN \"def\"";
          " 18 KET";
          " 19 END";
        ] );
    ( "(?:abc)",
      g
        [
          "  0 BRA";
          "  1 BRA";
          "  2 CHAR_RUN \"abc\"";
          "  5 KET";
          "  6 KET";
          "  7 END";
        ] );
    ( "(?:a|bc|d)e",
      g
        [
          "  0 BRA";
          "  1 BRA";
          "  2 ALT next=9";
          "  4 CHAR_RUN \"a\"";
          "  7 JMP 19";
          "  9 ALT next=16";
          " 11 CHAR_RUN \"bc\"";
          " 14 JMP 19";
          " 16 CHAR_RUN \"d\"";
          " 19 KET";
          " 20 CHAR_RUN \"e\"";
          " 23 KET";
          " 24 END";
        ] );
    ( "^abc$",
      g
        [
          "  0 BRA";
          "  1 CIRC";
          "  2 CHAR_RUN \"abc\"";
          "  5 DOLL";
          "  6 KET";
          "  7 END";
        ] );
    ( "\\Aabc\\z",
      g
        [
          "  0 BRA";
          "  1 SOD";
          "  2 CHAR_RUN \"abc\"";
          "  5 EOD";
          "  6 KET";
          "  7 END";
        ] );
    ( "a\\Zb",
      g
        [
          "  0 BRA";
          "  1 CHAR_RUN \"a\"";
          "  4 EODN";
          "  5 CHAR_RUN \"b\"";
          "  8 KET";
          "  9 END";
        ] );
    ( "\\Ga",
      g [ "  0 BRA"; "  1 SOM"; "  2 CHAR_RUN \"a\""; "  5 KET"; "  6 END" ] );
    ( "(?i)abc",
      g
        [
          "  0 BRA";
          "  1 CHARI \"a\"";
          "  3 CHARI \"b\"";
          "  5 CHARI \"c\"";
          "  7 KET";
          "  8 END";
        ] );
    ( "a(?:b(?:c|d)e|f)g",
      g
        [
          "  0 BRA";
          "  1 CHAR_RUN \"a\"";
          "  4 BRA";
          "  5 ALT next=27";
          "  7 CHAR_RUN \"b\"";
          " 10 BRA";
          " 11 ALT next=18";
          " 13 CHAR_RUN \"c\"";
          " 16 JMP 21";
          " 18 CHAR_RUN \"d\"";
          " 21 KET";
          " 22 CHAR_RUN \"e\"";
          " 25 JMP 30";
          " 27 CHAR_RUN \"f\"";
          " 30 KET";
          " 31 CHAR_RUN \"g\"";
          " 34 KET";
          " 35 END";
        ] );
    ( "aXb(?:c|d)",
      g
        [
          "  0 BRA";
          "  1 CHAR_RUN \"aXb\"";
          "  4 BRA";
          "  5 ALT next=12";
          "  7 CHAR_RUN \"c\"";
          " 10 JMP 15";
          " 12 CHAR_RUN \"d\"";
          " 15 KET";
          " 16 KET";
          " 17 END";
        ] );
    ("(?:)", g [ "  0 BRA"; "  1 BRA"; "  2 KET"; "  3 KET"; "  4 END" ]);
    (* Empty first branch: its choice point + JMP are all it emits; the ALT
       handler is the second branch's entry. *)
    ( "(?:|a)",
      g
        [
          "  0 BRA";
          "  1 BRA";
          "  2 ALT next=6";
          "  4 JMP 9";
          "  6 CHAR_RUN \"a\"";
          "  9 KET";
          " 10 KET";
          " 11 END";
        ] );
    (* Empty LAST branch: its entry coincides with the group KET, so the
       first branch's ALT handler and JMP target are both the KET. *)
    ( "(?:a|)",
      g
        [
          "  0 BRA";
          "  1 BRA";
          "  2 ALT next=9";
          "  4 CHAR_RUN \"a\"";
          "  7 JMP 9";
          "  9 KET";
          " 10 KET";
          " 11 END";
        ] );
  ]

let golden_tests =
  List.map
    (fun (pat, expected) ->
      Alcotest.test_case
        (Printf.sprintf "dump %S" pat)
        `Quick
        (fun () ->
          Alcotest.(check string)
            (Printf.sprintf "IR dump for %S" pat)
            expected (dump_of pat)))
    goldens

(* ---------- 2. unsupported reasons ---------- *)

let unsupported_reason (pat : string) : string =
  match Ir_compile.compile (compile_re pat) with
  | Ok _ -> Alcotest.failf "pattern %S was accepted but should be Unsupported" pat
  | Error r -> r

let unsupported_cases : (string * string * string) list =
  [
    (* (label, pattern, expected reason) *)
    ("capture group", "(abc)", "fast: capturing groups (chunk D)");
    ("character class", "[abc]", "fast: OP_CLASS (chunk E)");
    (* A backreference needs a capturing group, so the top_bracket gate
       (chunk D) fires before the OP_REF walk reason (chunk F). *)
    ("backreference", "(a)\\1", "fast: capturing groups (chunk D)");
    ("UTF mode", "(*UTF)abc", "fast: UTF mode (chunk I)");
    ("verb", "a(*FAIL)", "fast: verb (chunk H)");
  ]

let unsupported_tests =
  List.map
    (fun (label, pat, expected) ->
      Alcotest.test_case label `Quick (fun () ->
          Alcotest.(check string)
            (Printf.sprintf "reason for %S" pat)
            expected (unsupported_reason pat)))
    unsupported_cases

(* ---------- 3. verifier ---------- *)

(* Patterns whose compile the verifier must accept. *)
let supported_patterns =
  [
    "";
    "a";
    "abc";
    "a|b";
    "a|bc|def";
    "(?:abc)";
    "(?:a|bc|d)e";
    "^abc$";
    "\\Aabc\\z";
    "a\\Zb";
    "\\Ga";
    "(?i)abc";
    "a(?:b(?:c|d)e|f)g";
    "aXb(?:c|d)";
    "(?:)";
  ]

let verify_ok = function
  | Ok () -> ()
  | Error e -> Alcotest.failf "verifier unexpectedly rejected valid IR: %s" e

let verify_err = function
  | Ok () -> Alcotest.fail "verifier accepted corrupted IR"
  | Error _ -> ()

(* Return a mutable copy of an IR with a fresh code array to corrupt. *)
let with_code (ir : Ir.t) (code : int array) : Ir.t = { ir with Ir.code }

let verifier_tests =
  let pass_cases =
    List.map
      (fun pat ->
        Alcotest.test_case
          (Printf.sprintf "check %S" pat)
          `Quick
          (fun () -> verify_ok (Ir_verify.check (ir_of pat))))
      supported_patterns
  in
  let corrupt_cases =
    [
      Alcotest.test_case "unknown tag" `Quick (fun () ->
          let ir = ir_of "a|b" in
          let code = Array.copy ir.Ir.code in
          code.(0) <- 99 (* BRA head -> nonexistent tag *);
          verify_err (Ir_verify.check (with_code ir code)));
      Alcotest.test_case "jump target out of range" `Quick (fun () ->
          let ir = ir_of "a|b" in
          let code = Array.copy ir.Ir.code in
          code.(2) <- 9999 (* ALT next operand -> past end *);
          verify_err (Ir_verify.check (with_code ir code)));
      Alcotest.test_case "jump target mid-instruction" `Quick (fun () ->
          let ir = ir_of "a|b" in
          let code = Array.copy ir.Ir.code in
          (* CHAR_RUN occupies pc 3..5; pc 4 is an operand, not a head. *)
          code.(2) <- 4;
          verify_err (Ir_verify.check (with_code ir code)));
      Alcotest.test_case "char_run lit overrun" `Quick (fun () ->
          let ir = ir_of "abc" in
          let code = Array.copy ir.Ir.code in
          (* CHAR_RUN at pc 1: [tag; off=0; len]; blow up the length. *)
          code.(3) <- 9999;
          verify_err (Ir_verify.check (with_code ir code)));
      Alcotest.test_case "operand overruns code array" `Quick (fun () ->
          let ir = ir_of "abc" in
          (* "abc" is [BRA; CHAR_RUN off len; KET; END] (length 6). Truncate
             to length 3 so CHAR_RUN's 3-slot head (pc 1) no longer fits: the
             head walk must report the overrun. *)
          let code = Array.sub ir.Ir.code 0 3 in
          verify_err (Ir_verify.check (with_code ir code)));
      Alcotest.test_case "interior END" `Quick (fun () ->
          let ir = ir_of "a|b" in
          let code = Array.copy ir.Ir.code in
          (* "a|b" is [BRA; ALT n; CHAR_RUN o l; JMP t; CHAR_RUN o l; KET;
             END]; pc 11 is the KET head — turn it into a premature END
             (width 1, so the walk stays aligned and the trailing END at
             pc 12 remains): only the FINAL head may be END. *)
          code.(11) <- 0;
          verify_err (Ir_verify.check (with_code ir code)));
      Alcotest.test_case "missing END" `Quick (fun () ->
          let ir = ir_of "a|b" in
          let n = Array.length ir.Ir.code in
          (* Drop the trailing END (width 1); last head becomes KET. *)
          let code = Array.sub ir.Ir.code 0 (n - 1) in
          verify_err (Ir_verify.check (with_code ir code)));
      Alcotest.test_case "empty IR" `Quick (fun () ->
          let ir = ir_of "a" in
          verify_err (Ir_verify.check (with_code ir [||])));
    ]
  in
  pass_cases @ corrupt_cases

(* ---------- 4. sweep ---------- *)

let sweep_patterns =
  [
    (* supported *)
    "";
    "a";
    "abc";
    "hello world";
    "a|b";
    "a|b|c|d";
    "(?:abc)";
    "(?:a|b)";
    "(?:a|b|c)d";
    "x(?:y|z)w";
    "^abc";
    "abc$";
    "^abc$";
    "\\Aabc";
    "abc\\z";
    "abc\\Z";
    "\\Gabc";
    "(?i)abc";
    "(?i)a|b";
    "foo(?:bar|baz)qux";
    "(?:(?:a|b)|c)";
    "a(?:b(?:c|d))e";
    "\\A(?:x|y)\\z";
    "(?:)";
    "(?:)(?:)";
    "(?:|a)";
    "(?:a|)";
    "aa|bb|cc";
    "abcdefghij";
    "(?i)HELLO";
    "^(?:a|b)$";
    "\\Ga|\\Gb";
    (* unsupported (declined; verify not invoked) *)
    "(abc)";
    "[a-z]";
    "a+";
    "a.b";
    "\\d+";
    "(a)\\1";
    "(?=x)";
    "(?!x)";
    "(?<=x)";
    "a{2,3}";
    "(*UTF)a";
    "\\p{L}";
    "(?m)^a";
    "a\\b";
    "\\Kabc";
    "(?>ab)";
  ]

let sweep_tests =
  [
    Alcotest.test_case "compile-then-verify sweep" `Quick (fun () ->
        List.iter
          (fun pat ->
            match C.pcre2_compile pat ~options:0 with
            | Error _ -> () (* real compile error: not our concern here *)
            | Ok re -> (
                match Ir_compile.compile re with
                | Error _ -> () (* declined: no IR to verify *)
                | Ok ir -> (
                    match Ir_verify.check ir with
                    | Ok () -> ()
                    | Error e ->
                        Alcotest.failf
                          "accepted pattern %S produced IR the verifier \
                           rejects: %s"
                          pat e)))
          sweep_patterns);
  ]

let () =
  Alcotest.run "pcre2_fast_ir"
    [
      ("golden dumps", golden_tests);
      ("unsupported reasons", unsupported_tests);
      ("verifier", verifier_tests);
      ("sweep", sweep_tests);
    ]
