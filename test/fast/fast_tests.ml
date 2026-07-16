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
module E = Pcre2_engine.Engine
module F = Pcre2_fast

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
    (* Chunk D: a capturing group lowers grouploop-style (EVERY branch gets a
       choice point; the last ALT points to a FAIL). CAP_START opens the
       capture, CAP_END is its ket. *)
    ( "(a)",
      g
        [
          "  0 BRA";
          "  1 CAP_START ovbase=2";
          "  3 BRA";
          "  4 ALT next=11";
          "  6 CHAR_RUN \"a\"";
          "  9 JMP 12";
          " 11 FAIL";
          " 12 CAP_END ovbase=2";
          " 14 KET";
          " 15 END";
        ] );
    ( "(a|b)",
      g
        [
          "  0 BRA";
          "  1 CAP_START ovbase=2";
          "  3 BRA";
          "  4 ALT next=11";
          "  6 CHAR_RUN \"a\"";
          "  9 JMP 19";
          " 11 ALT next=18";
          " 13 CHAR_RUN \"b\"";
          " 16 JMP 19";
          " 18 FAIL";
          " 19 CAP_END ovbase=2";
          " 21 KET";
          " 22 END";
        ] );
    (* Char-repeat superinstructions (auto-possessified / minimizing forms). *)
    ("a*", g [ "  0 BRA"; "  1 REP pos {0,inf} \"a\""; "  6 KET"; "  7 END" ]);
    ("a+?", g [ "  0 BRA"; "  1 REP min {1,inf} \"a\""; "  6 KET"; "  7 END" ]);
    (* {2,4} decomposes into an EXACT(2) then a POSUPTO(0,2). *)
    ( "a{2,4}",
      g
        [
          "  0 BRA";
          "  1 REP min {2,2} \"a\"";
          "  6 REP pos {0,2} \"a\"";
          " 11 KET";
          " 12 END";
        ] );
    (* Caseless repeat carries the fcc fold-pair. *)
    ( "(?i)a*",
      g [ "  0 BRA"; "  1 REPI pos {0,inf} \"a/A\""; "  7 KET"; "  8 END" ] );
    (* Multiline anchors. *)
    ( "(?m)^a$",
      g
        [
          "  0 BRA";
          "  1 CIRCM";
          "  2 CHAR_RUN \"a\"";
          "  5 DOLLM";
          "  6 KET";
          "  7 END";
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
    ("character class", "[abc]", "fast: OP_CLASS (chunk E)");
    (* A back-referenced capture is declined at CAP_START by the
       optimized_cbracket gate (chunk F/J/K): its ovector slot cannot be used
       as the in-progress start scratch. *)
    ("referenced capture", "(a)\\1", "fast: referenced capture (chunk F/J/K)");
    ("optional group", "(a)?b", "fast: OP_BRAZERO (chunk D)");
    ("repeated group", "(a)+", "fast: repeated group (KETRMAX) (chunk D)");
    ("type repeat", "\\d+", "fast: type repeat (chunk E)");
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
    (* Chunk D: captures + char repeats + multiline anchors. *)
    "(a)";
    "(a|b)";
    "(a)(b)";
    "((a)(b))";
    "(a)b|c";
    "(?:(a)|b)c";
    "a*";
    "a+?";
    "a{2,4}";
    "(?i)a*";
    "a*b";
    "(a*)b";
    "(?m)^a$";
    "(a)(b)(c)(d)";
    "x(a+)x";
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
    (* Chunk D: captures, char repeats, multiline anchors. *)
    "(abc)";
    "(a)(b)(c)";
    "((a)(b))";
    "(a|b)c";
    "(?:(a)x|ay)";
    "a*";
    "a+";
    "a?";
    "a*?";
    "a+?";
    "a??";
    "a{2,4}";
    "a{3}";
    "a{2,}";
    "(?i)a*";
    "[^a]*x";  (* [^a] compiles to OP_NOT, so this is a supported NOTREP *)
    "a*b";
    "(a*)b";
    "(a+)(b+)";
    "(?m)^abc$";
    "(?m)^(a|b)$";
    "a{2,4}b";
    "(a)b|c";
    (* unsupported (declined; verify not invoked) *)
    "[a-z]";
    "a.b";
    "\\d+";
    "(a)\\1";
    "(?=x)";
    "(?!x)";
    "(?<=x)";
    "(*UTF)a";
    "\\p{L}";
    "a\\b";
    "\\Kabc";
    "(?>ab)";
    "(a)?b";
    "(a)+";
    "\\p{Common}{3}(())";
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

(* ---------- 5. runner exec parity vs the interpreter ----------

   On every pattern the fast engine ACCEPTS, exec_full must be byte-identical
   to Pcre2_engine.Engine.exec_full (the differential oracle): same rc class,
   ovector, and startchar. A pattern the fast engine declines (Unsupported)
   is skipped (the runner never sees it). *)

(* Match options (pcre2.h.generic). *)
let o_notbol = 0x00000001l
let o_noteol = 0x00000002l
let o_notempty = 0x00000004l
let o_notempty_atstart = 0x00000008l
let o_partial_soft = 0x00000010l
let o_partial_hard = 0x00000020l
let o_anchored = 0x80000000l
let o_endanchored = 0x20000000l
let o_bad = 0x00000100l (* PCRE2_FIRSTLINE — not a valid MATCH option -> -34 *)

(* A comparable normal form of an exec_result (mark is always None in this
   subset, so it is not compared; startchar is folded in for Match/Partial).
   The WHOLE ovector is compared (rc pair count = its length / 2) so capture
   values / rc high-water divergences are caught, not just the overall span. *)
let norm : E.exec_result -> string = function
  | E.Match { ovector; start_char; _ } ->
      let b = Buffer.create 32 in
      Buffer.add_string b (Printf.sprintf "M@%d[" start_char);
      Array.iteri
        (fun i x ->
          if i > 0 then Buffer.add_char b ',';
          Buffer.add_string b (string_of_int x))
        ovector;
      Buffer.add_char b ']';
      Buffer.contents b
  | E.No_match _ -> "NM"
  | E.Partial { start; _ } -> Printf.sprintf "P%d" start
  | E.Error { code; _ } -> Printf.sprintf "E%d" code

let parity_cases : (string * string * int * int32) list =
  [
    (* literal match / miss at various offsets *)
    ("abc", "abc", 0, 0l);
    ("abc", "xabc", 0, 0l);
    ("abc", "xabc", 1, 0l);
    ("abc", "ab", 0, 0l);
    ("abc", "", 0, 0l);
    ("abc", "zzz", 0, 0l);
    (* alternation backtracking order (leftmost branch wins) *)
    ("a|ab", "ab", 0, 0l);
    ("ab|a", "ab", 0, 0l);
    ("foo|foobar", "foobar", 0, 0l);
    ("(?:a|b)c", "bc", 0, 0l);
    ("(?:a|b|c)d", "cd", 0, 0l);
    ("a(?:b(?:c|d)e|f)g", "abdeg", 0, 0l);
    ("a(?:b(?:c|d)e|f)g", "afg", 0, 0l);
    (* empty-branch groups: empty match ovector [k,k] *)
    ("(?:|a)", "a", 0, 0l);
    ("(?:a|)", "b", 0, 0l);
    ("(?:)", "x", 0, 0l);
    ("(?:|a)b", "ab", 0, 0l);
    (* caseless single chars *)
    ("(?i)abc", "ABC", 0, 0l);
    ("(?i)abc", "aBc", 0, 0l);
    ("(?i)abc", "abd", 0, 0l);
    (* anchors *)
    ("^abc", "abc", 0, 0l);
    ("^abc", "xabc", 1, 0l);
    ("abc$", "abc", 0, 0l);
    ("\\Aabc", "abc", 0, 0l);
    ("abc\\z", "abc", 0, 0l);
    ("abc\\Z", "abc\n", 0, 0l);
    ("a\\Zb", "a\nb", 0, 0l);
    (* \G at nonzero offset *)
    ("\\Gabc", "abc", 0, 0l);
    ("\\Gbc", "abc", 1, 0l);
    ("\\Gbc", "abc", 0, 0l);
    (* notbol / noteol *)
    ("^a", "a", 0, o_notbol);
    ("a$", "a", 0, o_noteol);
    ("^a", "a", 0, 0l);
    (* NOTEMPTY / NOTEMPTY_ATSTART on empty-capable patterns *)
    ("(?:|a)", "b", 0, o_notempty);
    ("(?:a|)", "b", 0, o_notempty);
    ("(?:a|)", "b", 0, o_notempty_atstart);
    ("(?:|a)", "ba", 0, o_notempty);
    (* ANCHORED at match time *)
    ("abc", "xabc", 0, o_anchored);
    ("abc", "abc", 0, o_anchored);
    ("a|b", "xb", 0, o_anchored);
    (* ENDANCHORED *)
    ("a", "ab", 0, o_endanchored);
    ("ab", "ab", 0, o_endanchored);
    ("a|ab", "ab", 0, o_endanchored);
    (* PARTIAL soft / hard: "ab" pattern vs "a" subject *)
    ("ab", "a", 0, o_partial_soft);
    ("ab", "a", 0, o_partial_hard);
    ("abc", "ab", 0, o_partial_soft);
    ("abc", "ab", 0, o_partial_hard);
    ("abc", "xab", 0, o_partial_soft);
    ("\\Aabc", "ab", 0, o_partial_hard);
    ("(?:a|b)cd", "bc", 0, o_partial_soft);
    (* BADOFFSET / bad option bits *)
    ("abc", "ab", 5, 0l);
    ("abc", "abc", -1, 0l);
    ("abc", "abc", 0, o_bad);
    (* --- Chunk D: captures --- *)
    ("(a)", "a", 0, 0l);
    ("(a)(b)", "ab", 0, 0l);
    ("((a)(b))", "ab", 0, 0l);
    (* alternation clobbers group 1, restored on backtrack (unset in result) *)
    ("(?:(a)x|ay)", "ay", 0, 0l);
    ("(?:a(b)y|abz)(c)", "abzc", 0, 0l);
    (* unset trailing group + rc high-water *)
    ("(?:(a)|(b))c", "bc", 0, 0l);
    ("(a)b|c", "c", 0, 0l);
    ("(a)b|c", "ab", 0, 0l);
    (* nested + adjacent captures, rc = 5 *)
    ("(a)(b)(c)(d)", "abcd", 0, 0l);
    (* caseless capture *)
    ("(?i)(a)b", "Ab", 0, 0l);
    (* capture around a repeat *)
    ("(a*)b", "aaab", 0, 0l);
    ("(a*)ab", "aaab", 0, 0l);
    ("x(a+)x", "xaaax", 0, 0l);
    (* --- Chunk D: char repeats at boundaries --- *)
    ("a*", "aaa", 0, 0l);
    ("a*", "", 0, 0l);
    ("a*b", "aab", 0, 0l);
    ("a+", "aaa", 0, 0l);
    ("a+", "", 0, 0l);
    ("a+?b", "aaab", 0, 0l);
    ("a*?b", "aaab", 0, 0l);
    ("a??b", "ab", 0, 0l);
    ("a{2,4}", "aaaaa", 0, 0l); (* EXACT + POSUPTO *)
    ("a{2,4}", "a", 0, 0l);
    ("a{3}", "aaaa", 0, 0l);
    ("a{2,}", "aaaa", 0, 0l);
    ("ab*c", "abbbc", 0, 0l);
    ("ab*c", "ac", 0, 0l);
    (* NOT variants (negated char repeats) *)
    ("[^a]*b", "xyzb", 0, 0l);
    ("[^a]+b", "b", 0, 0l);
    ("(?i)[^a]*z", "XYz", 0, 0l);
    (* empty-capable repeat matching empty *)
    ("a*", "b", 0, 0l);
    (* possessive-vs-following backtrack boundary *)
    ("a*a", "aaa", 0, 0l);
    (* PARTIAL interplay with repeats *)
    ("a{3}", "aa", 0, o_partial_hard);
    ("a{3}", "aa", 0, o_partial_soft);
    ("xa+", "xaa", 0, o_partial_soft);
    ("a+b", "aaa", 0, o_partial_hard);
    (* --- Chunk D: multiline anchors --- *)
    ("(?m)^b", "a\nb", 0, 0l);
    ("(?m)b$", "b\na", 0, 0l);
    ("(?m)^a$", "x\na\ny", 0, 0l);
    ("(?m)^", "a\nb", 1, 0l);
    ("(?m)^a", "a", 0, o_notbol);
    ("(?m)a$", "a", 0, o_noteol);
  ]

let parity_tests =
  List.map
    (fun (pat, subj, off, opts) ->
      Alcotest.test_case
        (Printf.sprintf "parity %S %S off=%d opt=0x%lx" pat subj off opts)
        `Quick
        (fun () ->
          match (F.compile pat 0l, E.compile pat 0l) with
          | Ok fre, Ok ere ->
              let f = norm (F.exec_full fre subj off opts) in
              let e = norm (E.exec_full ere subj off opts) in
              Alcotest.(check string)
                (Printf.sprintf "fast vs interp for %S/%S" pat subj)
                e f
          | Error (F.Unsupported _), _ ->
              (* declined: nothing to compare (the runner never runs it) *)
              ()
          | Error _, _ | _, Error _ ->
              Alcotest.failf "compile mismatch for %S" pat))
    parity_cases

(* ---------- 6. LIMIT_MATCH tick-boundary parity ----------

   The pattern below tries all six top-level branches before matching (or
   failing) the last one; the interpreter and the fast engine must tick
   identically and therefore trip PCRE2_ERROR_MATCHLIMIT (-47) at the SAME N.
   Boundary found empirically against the interpreter: matching subject "f"
   needs 7 ticks (frame 0 + 6 branch entries), so N <= 6 => -47, N >= 7 =>
   Match. Both engines are pinned. *)
let limit_match_boundary_test =
  let run_both pat_body n subj =
    let pat = Printf.sprintf "(*LIMIT_MATCH=%d)%s" n pat_body in
    match (F.compile pat 0l, E.compile pat 0l) with
    | Ok fre, Ok ere ->
        (norm (F.exec_full fre subj 0 0l), norm (E.exec_full ere subj 0 0l))
    | _ -> Alcotest.failf "compile failed for %S" pat
  in
  [
    Alcotest.test_case "LIMIT_MATCH boundary (fast == interp)" `Quick (fun () ->
        (* below the boundary: both hit the match limit *)
        let f6, e6 = run_both "a|b|c|d|e|f" 6 "f" in
        Alcotest.(check string) "N=6 interp is MATCHLIMIT" "E-47" e6;
        Alcotest.(check string) "N=6 fast == interp" e6 f6;
        (* at the boundary: both complete the match *)
        let f7, e7 = run_both "a|b|c|d|e|f" 7 "f" in
        Alcotest.(check string) "N=7 interp matches" "M@0[0,1]" e7;
        Alcotest.(check string) "N=7 fast == interp" e7 f7;
        (* a non-matching subject: the start bitmap skips every attempt on
           both sides, so neither ticks -> both NOMATCH regardless of N *)
        let f1z, e1z = run_both "a|b|c|d|e|f" 1 "z" in
        Alcotest.(check string) "N=1/z interp NOMATCH" "NM" e1z;
        Alcotest.(check string) "N=1/z fast == interp" e1z f1z);
    Alcotest.test_case "LIMIT_MATCH boundary, greedy repeat (fast == interp)"
      `Quick (fun () ->
        (* [a*ab] on "aaaaab": the greedy a* over-eats, then backs off one 'a'
           at a time (each retry a tick). Boundary N=4 on both engines: the
           frame-0 tick + the group branch + the give-back attempts. *)
        let f3, e3 = run_both "a*ab" 3 "aaaaab" in
        Alcotest.(check string) "N=3 interp is MATCHLIMIT" "E-47" e3;
        Alcotest.(check string) "N=3 fast == interp" e3 f3;
        let f4, e4 = run_both "a*ab" 4 "aaaaab" in
        Alcotest.(check string) "N=4 interp matches" "M@0[0,6]" e4;
        Alcotest.(check string) "N=4 fast == interp" e4 f4);
  ]

(* ---------- 7. alloc pins ----------

   The fast runner allocates nothing per attempt and only O(1) small records
   per exec (fast-design.md §3 / port-conventions §8). Methods mirror
   test/pcre2_tests.ml:327-354 (per-attempt) and :647-687 (per-exec). *)
let alloc_tests =
  [
    Alcotest.test_case "alloc: O(1) per ~100k failing attempts" `Slow
      (fun () ->
        match F.compile "qz" 0l with
        | Error _ -> Alcotest.fail "compile qz failed"
        | Ok re ->
            let subject = String.make 100_000 'q' in
            (* warm-up: one-time lazy init + scratch seeding *)
            (match F.exec re subject 0 0l with
            | Ok None -> ()
            | _ -> Alcotest.fail "expected no match (warm-up)");
            let before = Gc.minor_words () in
            let r = F.exec re subject 0 0l in
            let delta = Gc.minor_words () -. before in
            (match r with
            | Ok None -> ()
            | _ -> Alcotest.fail "expected no match");
            Alcotest.(check bool)
              (Printf.sprintf
                 "expected O(1) minor allocation for ~100k attempts, measured \
                  %.0f words"
                 delta)
              true (delta < 1000.));
    Alcotest.test_case "alloc: < 60 minor words per exec" `Slow (fun () ->
        match F.compile "qz" 0l with
        | Error _ -> Alcotest.fail "compile qz failed"
        | Ok re ->
            let subject = "qqqq" in
            (match F.exec re subject 0 0l with
            | Ok None -> ()
            | _ -> Alcotest.fail "expected no match (warm-up)");
            let before = Gc.minor_words () in
            for _ = 1 to 1_000 do
              ignore (F.exec re subject 0 0l)
            done;
            let delta = (Gc.minor_words () -. before) /. 1000. in
            Alcotest.(check bool)
              (Printf.sprintf
                 "expected < 60 minor words/exec at the Fast seam, measured \
                  %.1f words/exec"
                 delta)
              true (delta < 60.));
    (* A capture + repeat pattern with heavy backtracking must still allocate
       O(1) per failing attempt (the CAP cleanup and REP records live in the
       reused save stack; the ovector is the reused scratch). *)
    Alcotest.test_case "alloc: O(1) per ~50k failing capture+repeat attempts"
      `Slow (fun () ->
        match F.compile "(a+)(b+)Z" 0l with
        | Error _ -> Alcotest.fail "compile failed"
        | Ok re ->
            let subject = String.make 50_000 'a' in
            (match F.exec re subject 0 0l with
            | Ok None -> ()
            | _ -> Alcotest.fail "expected no match (warm-up)");
            let before = Gc.minor_words () in
            let r = F.exec re subject 0 0l in
            let delta = Gc.minor_words () -. before in
            (match r with
            | Ok None -> ()
            | _ -> Alcotest.fail "expected no match");
            Alcotest.(check bool)
              (Printf.sprintf
                 "expected O(1) minor allocation for ~50k capture+repeat \
                  attempts, measured %.0f words"
                 delta)
              true (delta < 1000.));
  ]

(* ---------- 8. ulimit-free stack-safety smoke ----------

   A 1 MiB subject with an alternation-heavy pattern that fails at every
   position: the bump-along + runner are iterative ([@tailcall]), so the run
   completes without a native-stack blow-up. *)
let stack_safety_tests =
  [
    Alcotest.test_case "1MB subject, alternation-heavy, completes" `Slow
      (fun () ->
        match F.compile "(?:a|b|c|d|e|f|g|h)(?:i|j|k|l)Z" 0l with
        | Error _ -> Alcotest.fail "compile failed"
        | Ok re ->
            let subject = String.make (1024 * 1024) 'x' in
            (match F.exec re subject 0 0l with
            | Ok None -> ()
            | Ok (Some _) -> Alcotest.fail "unexpected match"
            | Error c -> Alcotest.failf "unexpected error %d" c));
  ]

let () =
  Alcotest.run "pcre2_fast_ir"
    [
      ("golden dumps", golden_tests);
      ("unsupported reasons", unsupported_tests);
      ("verifier", verifier_tests);
      ("sweep", sweep_tests);
      ("runner parity", parity_tests);
      ("limit-match boundary", limit_match_boundary_test);
      ("alloc pins", alloc_tests);
      ("stack safety", stack_safety_tests);
    ]
