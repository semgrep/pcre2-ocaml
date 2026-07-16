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
    (* Chunk D2: a greedy repeated capturing group (OP_CBRA + KETRMAX). The
       group entry is GROUP_START (records the iteration start for the empty
       check) then CAP_START; the ket is CAP_END + KET_RMAX looping back to
       the GROUP_START (entry=1). *)
    ( "(a)+",
      g
        [
          "  0 BRA";
          "  1 GROUP_START g=0";
          "  3 CAP_START ovbase=2";
          "  5 BRA";
          "  6 ALT next=13";
          "  8 CHAR_RUN \"a\"";
          " 11 JMP 14";
          " 13 FAIL";
          " 14 CAP_END ovbase=2";
          " 16 KET_RMAX entry=1 g=0";
          " 19 KET";
          " 20 END";
        ] );
    (* Greedy zero-or-more: BRAZERO wraps the repeated group; its skip target
       is past the group (skip=21). *)
    ( "(a)*",
      g
        [
          "  0 BRA";
          "  1 BRAZERO skip=21";
          "  3 GROUP_START g=0";
          "  5 CAP_START ovbase=2";
          "  7 BRA";
          "  8 ALT next=15";
          " 10 CHAR_RUN \"a\"";
          " 13 JMP 16";
          " 15 FAIL";
          " 16 CAP_END ovbase=2";
          " 18 KET_RMAX entry=3 g=0";
          " 21 KET";
          " 22 END";
        ] );
    (* Greedy zero-or-one: BRAZERO wraps a NON-repeating group (plain KET, no
       GROUP_START / KET_RMAX). *)
    ( "(a)?",
      g
        [
          "  0 BRA";
          "  1 BRAZERO skip=16";
          "  3 CAP_START ovbase=2";
          "  5 BRA";
          "  6 ALT next=13";
          "  8 CHAR_RUN \"a\"";
          " 11 JMP 14";
          " 13 FAIL";
          " 14 CAP_END ovbase=2";
          " 16 KET";
          " 17 END";
        ] );
    (* Lazy zero-or-one: BRAMINZERO. *)
    ( "(a)??",
      g
        [
          "  0 BRA";
          "  1 BRAMINZERO skip=16";
          "  3 CAP_START ovbase=2";
          "  5 BRA";
          "  6 ALT next=13";
          "  8 CHAR_RUN \"a\"";
          " 11 JMP 14";
          " 13 FAIL";
          " 14 CAP_END ovbase=2";
          " 16 KET";
          " 17 END";
        ] );
    (* Lazy one-or-more capturing group: KET_RMIN. *)
    ( "(a)+?",
      g
        [
          "  0 BRA";
          "  1 GROUP_START g=0";
          "  3 CAP_START ovbase=2";
          "  5 BRA";
          "  6 ALT next=13";
          "  8 CHAR_RUN \"a\"";
          " 11 JMP 14";
          " 13 FAIL";
          " 14 CAP_END ovbase=2";
          " 16 KET_RMIN entry=1 g=0";
          " 19 KET";
          " 20 END";
        ] );
    (* Non-capturing repeated group: OP_BRA lowers bra_loop-style (single
       branch, no ALT), so the ket carries g=-1 (no empty check — the C's
       P == NULL) and loops back to the BRA marker (entry=1). *)
    ( "(?:ab)+",
      g
        [
          "  0 BRA";
          "  1 BRA";
          "  2 CHAR_RUN \"ab\"";
          "  5 KET_RMAX entry=1 g=-1";
          "  8 KET";
          "  9 END";
        ] );
    (* Empty-capable non-capturing repeat: OP_SBRA lowers grouploop-style
       (ALT every branch + FAIL) WITH a tracked group id (g=0) for the
       empty-string loop check. *)
    ( "(?:a|)+",
      g
        [
          "  0 BRA";
          "  1 GROUP_START g=0";
          "  3 BRA";
          "  4 ALT next=11";
          "  6 CHAR_RUN \"a\"";
          "  9 JMP 16";
          " 11 ALT next=15";
          " 13 JMP 16";
          " 15 FAIL";
          " 16 KET_RMAX entry=1 g=0";
          " 19 KET";
          " 20 END";
        ] );
    (* Empty-capable capturing repeat (OP_SCBRA): the classic nested
       star-of-star. *)
    ( "(a*)*",
      g
        [
          "  0 BRA";
          "  1 BRAZERO skip=23";
          "  3 GROUP_START g=0";
          "  5 CAP_START ovbase=2";
          "  7 BRA";
          "  8 ALT next=17";
          " 10 REP max {0,inf} \"a\"";
          " 15 JMP 18";
          " 17 FAIL";
          " 18 CAP_END ovbase=2";
          " 20 KET_RMAX entry=3 g=0";
          " 23 KET";
          " 24 END";
        ] );
    (* SKIPZERO: a {0}-quantified group is elided entirely (no IR). *)
    ( "(?:abc){0}x",
      g [ "  0 BRA"; "  1 CHAR_RUN \"x\""; "  4 KET"; "  5 END" ] );
    (* --- Chunk E: classes, types, word boundaries --- *)
    (* A class (any of [abc] / [a-z]) compiles to one 32-byte-bitmap CLASS; the
       IR records the map's byte offset in re.code. *)
    ("[abc]", g [ "  0 BRA"; "  1 CLASS map=4"; "  3 KET"; "  4 END" ]);
    ("[a-z]", g [ "  0 BRA"; "  1 CLASS map=4"; "  3 KET"; "  4 END" ]);
    (* [^a] is a single negated char (OP_NOT), lowered as a NOT-char repeat
       with lmin=lmax=1 (no choice point). *)
    ( "[^a]",
      g [ "  0 BRA"; "  1 NOTREP min {1,1} \"a\""; "  6 KET"; "  7 END" ] );
    (* Single character types (the type opcode name is shown). *)
    ("\\d", g [ "  0 BRA"; "  1 TYPE \\d"; "  3 KET"; "  4 END" ]);
    (".", g [ "  0 BRA"; "  1 TYPE Any"; "  3 KET"; "  4 END" ]);
    ("\\R", g [ "  0 BRA"; "  1 TYPE \\R"; "  3 KET"; "  4 END" ]);
    ("\\h", g [ "  0 BRA"; "  1 TYPE \\h"; "  3 KET"; "  4 END" ]);
    ("\\b", g [ "  0 BRA"; "  1 WORDBOUND \\b"; "  3 KET"; "  4 END" ]);
    ("\\B", g [ "  0 BRA"; "  1 WORDBOUND \\B"; "  3 KET"; "  4 END" ]);
    (* Type / class repeats. A trailing repeat auto-possessifies (POS). *)
    ("\\d+", g [ "  0 BRA"; "  1 TYPE_REP pos {1,inf} \\d"; "  6 KET"; "  7 END" ]);
    ( "\\w*",
      g [ "  0 BRA"; "  1 TYPE_REP pos {0,inf} \\w"; "  6 KET"; "  7 END" ] );
    ( "(?s).*",
      g [ "  0 BRA"; "  1 TYPE_REP pos {0,inf} AllAny"; "  6 KET"; "  7 END" ] );
    ( "[a-z]+",
      g [ "  0 BRA"; "  1 CLASS_REP pos {1,inf} map=4"; "  6 KET"; "  7 END" ] );
    ( "[0-9]{2,4}",
      g [ "  0 BRA"; "  1 CLASS_REP pos {2,4} map=4"; "  6 KET"; "  7 END" ] );
    (* A repeat followed by an OVERLAPPING atom stays greedy (max): the
       following 'y' is in [a-z] / a digit is a digit, so auto-possessify does
       not fire. (\\d+x auto-possessifies since 'x' is not a digit.) *)
    ( "[a-z]*y",
      g
        [
          "  0 BRA";
          "  1 CLASS_REP max {0,inf} map=4";
          "  6 CHAR_RUN \"y\"";
          "  9 KET";
          " 10 END";
        ] );
    ( "\\d+5",
      g
        [
          "  0 BRA";
          "  1 TYPE_REP max {1,inf} \\d";
          "  6 CHAR_RUN \"5\"";
          "  9 KET";
          " 10 END";
        ] );
    (* --- Chunk F: backreferences --- *)
    (* A referenced capture lowers with the referenced protocol
       (CAP_START_REF / CAP_END_REF): the in-progress start lives in
       mb.cap_start, both ovector slots written only at close. The reference
       itself is REF (numbered, ci=0). *)
    ( "(a)\\1",
      g
        [
          "  0 BRA";
          "  1 CAP_START_REF ovbase=2";
          "  3 BRA";
          "  4 ALT next=11";
          "  6 CHAR_RUN \"a\"";
          "  9 JMP 12";
          " 11 FAIL";
          " 12 CAP_END_REF ovbase=2";
          " 14 REF ovbase=2 ci=0";
          " 17 KET";
          " 18 END";
        ] );
    (* Caseless reference -> REF ci=1 (the referenced group's 'a' is CHARI). *)
    ( "(?i)(a)\\1",
      g
        [
          "  0 BRA";
          "  1 CAP_START_REF ovbase=2";
          "  3 BRA";
          "  4 ALT next=10";
          "  6 CHARI \"a\"";
          "  8 JMP 11";
          " 10 FAIL";
          " 11 CAP_END_REF ovbase=2";
          " 13 REF ovbase=2 ci=1";
          " 16 KET";
          " 17 END";
        ] );
    (* Greedy ref repeat (\1+) -> REF_REP; the group is still referenced. *)
    ( "(a)\\1+",
      g
        [
          "  0 BRA";
          "  1 CAP_START_REF ovbase=2";
          "  3 BRA";
          "  4 ALT next=11";
          "  6 CHAR_RUN \"a\"";
          "  9 JMP 12";
          " 11 FAIL";
          " 12 CAP_END_REF ovbase=2";
          " 14 REF_REP max {1,inf} ovbase=2 ci=0";
          " 20 KET";
          " 21 END";
        ] );
    (* Bounded ref repeat \1{2,4}. *)
    ( "(a)\\1{2,4}",
      g
        [
          "  0 BRA";
          "  1 CAP_START_REF ovbase=2";
          "  3 BRA";
          "  4 ALT next=11";
          "  6 CHAR_RUN \"a\"";
          "  9 JMP 12";
          " 11 FAIL";
          " 12 CAP_END_REF ovbase=2";
          " 14 REF_REP max {2,4} ovbase=2 ci=0";
          " 20 KET";
          " 21 END";
        ] );
    (* A named non-duplicate reference is a numbered REF; only the referenced
       group (n) is CAP_START_REF, the unreferenced one (m) stays CAP_START. *)
    ( "(?<n>a)(?<m>b)\\k<n>",
      g
        [
          "  0 BRA";
          "  1 CAP_START_REF ovbase=2";
          "  3 BRA";
          "  4 ALT next=11";
          "  6 CHAR_RUN \"a\"";
          "  9 JMP 12";
          " 11 FAIL";
          " 12 CAP_END_REF ovbase=2";
          " 14 CAP_START ovbase=4";
          " 16 BRA";
          " 17 ALT next=24";
          " 19 CHAR_RUN \"b\"";
          " 22 JMP 25";
          " 24 FAIL";
          " 25 CAP_END ovbase=4";
          " 27 REF ovbase=2 ci=0";
          " 30 KET";
          " 31 END";
        ] );
    (* ---- Chunk G: lookaround / atomic / possessive ---- *)
    ( "a(?=b)c",
      g
        [
          "  0 BRA";
          "  1 CHAR_RUN \"a\"";
          "  4 ONCE";
          "  5 GROUP_START g=0";
          "  7 BRA";
          "  8 ALT next=15";
          " 10 CHAR_RUN \"b\"";
          " 13 JMP 16";
          " 15 FAIL";
          " 16 ASSERT_END atomic g=0";
          " 19 CHAR_RUN \"c\"";
          " 22 KET";
          " 23 END";
        ] );
    ( "a(?!b)",
      g
        [
          "  0 BRA";
          "  1 CHAR_RUN \"a\"";
          "  4 NASSERT g=-1 cont=17";
          "  7 BRA";
          "  8 ALT next=15";
          " 10 CHAR_RUN \"b\"";
          " 13 JMP 16";
          " 15 FAIL";
          " 16 NASSERT_MATCH";
          " 17 KET";
          " 18 END";
        ] );
    ( "(?<=ab)c",
      g
        [
          "  0 BRA";
          "  1 ONCE";
          "  2 GROUP_START g=0";
          "  4 BRA";
          "  5 ALT next=14";
          "  7 REVERSE 2";
          "  9 CHAR_RUN \"ab\"";
          " 12 JMP 15";
          " 14 FAIL";
          " 15 ASSERT_END atomic g=0";
          " 18 CHAR_RUN \"c\"";
          " 21 KET";
          " 22 END";
        ] );
    ( "(?<=\\d{2,3})x",
      g
        [
          "  0 BRA";
          "  1 ONCE";
          "  2 GROUP_START g=0";
          "  4 BRA";
          "  5 ALT next=24";
          "  7 VREVERSE {2,3}";
          " 10 TYPE_REP min {2,2} \\d";
          " 15 TYPE_REP max {0,1} \\d";
          " 20 ASSERTBACK_CHECK g=0";
          " 22 JMP 25";
          " 24 FAIL";
          " 25 ASSERT_END atomic g=0";
          " 28 CHAR_RUN \"x\"";
          " 31 KET";
          " 32 END";
        ] );
    ( "(?>a|b)c",
      g
        [
          "  0 BRA";
          "  1 ONCE";
          "  2 BRA";
          "  3 ALT next=10";
          "  5 CHAR_RUN \"a\"";
          "  8 JMP 18";
          " 10 ALT next=17";
          " 12 CHAR_RUN \"b\"";
          " 15 JMP 18";
          " 17 FAIL";
          " 18 ONCE_END";
          " 19 CHAR_RUN \"c\"";
          " 22 KET";
          " 23 END";
        ] );
    ( "(?:ab)++c",
      g
        [
          "  0 BRA";
          "  1 POSSESS ovbase=0 zero=0";
          "  4 BRA";
          "  5 ALT next=15";
          "  7 CHAR_RUN \"ab\"";
          " 10 JMP 12";
          " 12 KETRPOS entry=4 ovbase=0";
          " 15 POSSESS_DONE";
          " 16 CHAR_RUN \"c\"";
          " 19 KET";
          " 20 END";
        ] );
    ( "(a)*+",
      g
        [
          "  0 BRA";
          "  1 POSSESS ovbase=2 zero=1";
          "  4 BRA";
          "  5 ALT next=15";
          "  7 CHAR_RUN \"a\"";
          " 10 JMP 12";
          " 12 KETRPOS entry=4 ovbase=2";
          " 15 POSSESS_DONE";
          " 16 KET";
          " 17 END";
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

(* Chunk F: a DNREF golden needs DUPNAMES (duplicate group names), so it
   compiles with that option rather than through the plain [dump_of]. Both
   alternatives are referenced captures; the reference scans the name-table
   list (slot=0, count=2) for the first set group. *)
let dupnames = 0x00000040l

let dnref_golden_test =
  Alcotest.test_case "dump DNREF (dupnames)" `Quick (fun () ->
      let ir =
        match
          C.pcre2_compile "(?:(?<n>a)|(?<n>b))\\k<n>"
            ~options:(Pcre2_engine.Options.of_int32 dupnames)
        with
        | Ok re -> (
            match Ir_compile.compile re with
            | Ok ir -> ir
            | Error r -> Alcotest.failf "unexpectedly Unsupported: %s" r)
        | Error (c, o) -> Alcotest.failf "compile err %d @ %d" c o
      in
      let expected =
        g
          [
            "  0 BRA";
            "  1 BRA";
            "  2 ALT next=19";
            "  4 CAP_START_REF ovbase=2";
            "  6 BRA";
            "  7 ALT next=14";
            "  9 CHAR_RUN \"a\"";
            " 12 JMP 15";
            " 14 FAIL";
            " 15 CAP_END_REF ovbase=2";
            " 17 JMP 32";
            " 19 CAP_START_REF ovbase=4";
            " 21 BRA";
            " 22 ALT next=29";
            " 24 CHAR_RUN \"b\"";
            " 27 JMP 30";
            " 29 FAIL";
            " 30 CAP_END_REF ovbase=4";
            " 32 KET";
            " 33 DNREF slot=0 count=2 ci=0";
            " 37 KET";
            " 38 END";
          ]
      in
      Alcotest.(check string)
        "DNREF dump" expected
        (Format.asprintf "%a" Ir.dump ir))

(* ---------- 2. unsupported reasons ---------- *)

let unsupported_reason (pat : string) : string =
  match Ir_compile.compile (compile_re pat) with
  | Ok _ -> Alcotest.failf "pattern %S was accepted but should be Unsupported" pat
  | Error r -> r

let unsupported_cases : (string * string * string) list =
  [
    (* (label, pattern, expected reason) *)
    (* Chunk F: a back-referenced capture is now LOWERED (referenced protocol),
       so (a)\1 is accepted — no unsupported entry. A capture referenced by a
       CONDITIONAL still declines, but at the OP_COND (chunk J), not the
       referenced cbracket: the referenced-capture lowering is generic. *)
    ("conditional ref", "(a)(?(1)b|c)", "fast: OP_COND (chunk J)");
    (* Chunk G supports atomic groups (OP_ONCE), so a possessive ref repeat
       "(a)\\1++" (compiled as (?>\\1+)) is now ACCEPTED — no unsupported entry. *)
    (* Chunk G supports possessive groups (BRAPOS/CBRAPOS/... + KETRPOS +
       BRAPOSZERO), so "(a)++", "(?:ab)++", "(a)*+" are now ACCEPTED. A REPEATED
       atomic group / assertion (Once ... KetRmax, e.g. "(?>a)+") still declines
       (the per-iteration atomic commit combined with a repeating ket is out of
       subset). *)
    ( "repeated atomic group",
      "(?>a)+",
      "fast: repeated atomic group / assertion (chunk G+)" );
    (* Chunk E declines: a \p property in a class compiles to OP_XCLASS, which
       needs the property machinery (chunk I); \p / \X singles and repeats and
       the UCP word boundary are likewise chunk I. *)
    ("prop in class (XCLASS)", "[\\p{L}]", "fast: OP_XCLASS (\\p in class) (chunk I)");
    ("prop", "\\p{L}", "fast: \\p (chunk I)");
    ("prop type repeat", "\\p{L}+", "fast: \\p (chunk I)");
    ("extuni", "\\X", "fast: \\X (chunk I)");
    (* The UCP verb turns on UCP mode, caught by the compile-level gate before
       the UCP word-boundary opcode is even reached. *)
    ("ucp mode", "(*UCP)\\ba", "fast: UCP mode (chunk I)");
    ("UTF mode", "(*UTF)abc", "fast: UTF mode (chunk I)");
    (* Chunk H supports MARK/PRUNE/SKIP/THEN/COMMIT (+_ARG), FAIL, ACCEPT and
       OP_CLOSE (so "a(*FAIL)", "(*MARK:x)a", "a(*PRUNE)b", "a(*ACCEPT)", and
       ( *THEN) with atomic positive assertions, are now ACCEPTED). Two
       combinations still decline: ( *ACCEPT) inside an assertion (needs
       MATCH_ACCEPT propagation to the assertion boundary with capture fishing),
       and ( *THEN) in a pattern that also has a NON-ATOMIC positive assertion
       (no KIND_ONCE boundary to contain the THEN). *)
    ( "accept in assertion",
      "(?=a(*ACCEPT))",
      "fast: (*ACCEPT) inside assertion (chunk H+)" );
    ( "then with non-atomic lookahead (inside)",
      "(*napla:a(*THEN)b)c",
      "fast: (*THEN) with non-atomic assertion (chunk H+)" );
    ( "then with non-atomic lookahead (after)",
      "(*napla:a)b(*THEN)",
      "fast: (*THEN) with non-atomic assertion (chunk H+)" );
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
    (* Chunk D2: quantified + optional groups. *)
    "(a)+";
    "(a)*";
    "(a)?";
    "(a)??";
    "(a)*?";
    "(a)+?";
    "(?:ab)+";
    "(?:ab)*";
    "(?:a|)+";
    "(a*)*";
    "(?:a?)+";
    "(ab|c)*";
    "((a)(b))+";
    "(?:(a)|(b))+";
    "(a){2,4}";
    "(?:abc){0}x";
    "^(?:a?b?)*$";
    "(a|b)+c";
    (* Chunk E: classes, types, word boundaries. *)
    "[abc]";
    "[a-z]";
    "[^a]";
    "[^a-z]";
    "\\d";
    "\\D";
    "\\w\\W\\s\\S";
    ".";
    "(?s).";
    "\\R";
    "\\h\\H\\v\\V";
    "\\bfoo\\b";
    "\\Bfoo";
    "\\d+";
    "\\w*";
    "[a-z]+";
    "[0-9]{2,4}";
    "\\d+x";
    "[a-z]*y";
    "[a-z]++x";
    "([a-z]+)(\\d+)";
    "\\R+";
    "(?:\\d|[a-f])+";
    "a\\bc\\d[e-h]";
    (* Chunk F: backreferences (numbered; DNREF needs dupnames, covered
       separately). *)
    "(a)\\1";
    "(?i)(a)\\1";
    "(abc)\\1";
    "(a|b)\\1";
    "(.)\\1";
    "(a)\\1+";
    "(a)\\1*";
    "(a)\\1?";
    "(ab)\\1{2,3}";
    "(a)\\1+?";
    "(a)(b)\\2\\1";
    "((a|b))\\1";
    "(\\1a|b)+";
    "((a)\\2)+";
    "(a*)\\1";
    "\\1(a)";
    (* Chunk G: lookaround + atomic groups. *)
    "(?=abc)";
    "(?!abc)";
    "a(?=b)";
    "a(?!b)";
    "(?=a|b)c";
    "(?<=abc)";
    "(?<!abc)";
    "(?<=a)b";
    "(?<=ab|c)d";
    "(?<=\\d{2,3})x";
    "(?<=a\\d?b)";
    "(?<!\\d{1,2})x";
    "(?>abc)";
    "(?>a|ab)c";
    "(?>a+)b";
    "(?>(a)b)c";
    "a(?>b*)b";
    "((?>a|ab))c";
    "(?=(a))\\1";
    "(?:(?=(a+))\\1)+";
    "(a)\\1++";
    "(?>(a)|(b))\\1";
    "(?<=(a))b";
    "(*napla:a|b)c";
    "(?>x(?=y))";
    "(?=(?>a+))a";
    (* Chunk G: possessive brackets. *)
    "(?:ab)++";
    "(?:ab)*+";
    "(?:ab)?+";
    "(a)++";
    "(a)*+";
    "(a|b)++";
    "(?:a|b)*+c";
    "((a)b)++";
    "(a)++\\1";
    "x(?:\\d)++y";
    "(?:a+)++";
    "(?:)*+";
    "(a?)*+";
    (* Chunk H: backtracking control verbs, FAIL, ACCEPT, CLOSE. *)
    "a(*FAIL)";
    "a(*F)";
    "a(*ACCEPT)";
    "(a(*ACCEPT))b";
    "(*MARK:x)abc";
    "(*:x)abc";
    "a(*PRUNE)b";
    "a(*PRUNE:x)b";
    "a(*SKIP)b";
    "a(*SKIP:x)b";
    "a(*COMMIT)b";
    "a(*COMMIT:x)b";
    "a(*THEN)b|c";
    "a(*THEN:x)b|c";
    "(a(*THEN)b|c(*THEN)d)e";
    "(*MARK:a)x(*SKIP:a)y|z";
    "(?:a(*COMMIT)b|c)";
    "(?>a(*:m))b|ac";
    "(?!a(*COMMIT)b)c";
    "(?!a(*THEN)b|c)d";
    "(?:a|b(*THEN)c)+";
    (* ( *THEN) with atomic positive assertions: contained via the KIND_ONCE
       pos-assert subtype (inside and after the assertion). *)
    "(?=a(*THEN)b)";
    "(?=a(*THEN)b|c)d";
    "(?<=a(*THEN))b";
    "\\V??(?=(*MARK:c))(*THEN)";
    "x(?=y)(*THEN)z|w";
    "(?>a(*THEN)b)c";
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
    (* Chunk D2: quantified/optional groups (accepted; verify must pass). *)
    "(a)?b";
    "(a)+";
    "(a)*";
    "(a)??";
    "(a)*?";
    "(a)+?";
    "(?:ab)+";
    "(?:xy)*z";
    "(?:a|)+";
    "(a*)*";
    "(a*)+";
    "(?:a?)*";
    "(ab|c)*d";
    "((a)(b))+";
    "(?:(a)|(b))*";
    "(a){2,4}b";
    "(?:abc){0}x";
    "^(?:a?b?)*$";
    "(a|bb|ccc)+";
    (* Chunk E: classes / types / \R / \h\v / \b\B (accepted; verify passes). *)
    "[a-z]";
    "a.b";
    "\\d+";
    "[abc]";
    "[^abc]";
    "[a-z0-9_]+";
    "\\D\\S\\W";
    "(?s).*";
    "\\R";
    "\\R+";
    "(*BSR_ANYCRLF)\\R+";
    "\\h+\\v*";
    "\\bword\\b";
    "\\Bx";
    "\\d{2,5}";
    "[0-9]{3}";
    "[a-f]++";
    "(\\w+)\\s+(\\w+)";
    "^\\d+$";
    "a[bc]d[^e]f";
    (* Chunk F: backreferences (accepted; verify passes). *)
    "(a)\\1";
    "(?i)(a)\\1";
    "(abc)\\1";
    "(a|b)\\1";
    "(.)\\1+";
    "(ab)\\1{2,3}";
    "(a)\\1*b";
    "(a)\\1+?";
    "(a)(b)\\2\\1";
    "((a|b))\\1";
    "(\\1a|b)+";
    "((a)\\2)+";
    "(a*)\\1";
    "\\1(a)";
    "(a+)\\1";
    (* unsupported (declined; verify not invoked) *)
    "(?=x)";
    "(?!x)";
    "(?<=x)";
    "(*UTF)a";
    "\\p{L}";
    "a\\b";
    "\\Kabc";
    "(?>ab)";
    (* possessive group repeats stay declined (chunk G) *)
    "(a)++";
    "(?:ab)*+";
    "(a*)++";
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
    (* --- Chunk D2: quantified + optional groups --- *)
    (* greedy / lazy quantifiers on a capturing group; last-iteration capture
       wins, earlier iterations restored on give-back *)
    ("(a)+", "aaa", 0, 0l);
    ("(a)+", "", 0, 0l);
    ("(a)*", "aaa", 0, 0l);
    ("(a)*", "", 0, 0l);
    ("(a)?", "a", 0, 0l);
    ("(a)?", "", 0, 0l);
    ("(a)??", "a", 0, 0l);
    ("(a)*?", "aaa", 0, 0l);
    ("(a)+?", "aaa", 0, 0l);
    ("(a)+?b", "aaab", 0, 0l);
    (* nested alternation group, greedy repeat; capture is the last branch hit *)
    ("(ab|c)*", "abcab", 0, 0l);
    ("(ab|c)*", "", 0, 0l);
    ("(a|b)+c", "abababc", 0, 0l);
    (* non-capturing bra_loop repeat (no empty check) *)
    ("(?:ab)+", "abab", 0, 0l);
    ("(?:ab)+", "aba", 0, 0l);
    ("(?:ab)*c", "ababc", 0, 0l);
    ("(?:(a)|(b))+", "ab", 0, 0l);
    ("(?:(a)|(b))+", "ba", 0, 0l);
    (* nested captures across iterations, rc high-water *)
    ("((a)(b))+", "abab", 0, 0l);
    ("(a)(b)|(a)", "a", 0, 0l);
    (* empty-capable repeated groups — the classic empty-loop cases *)
    ("(a*)*", "aaa", 0, 0l);
    ("(a*)*", "", 0, 0l);
    ("(a*)+", "aaa", 0, 0l);
    ("(?:a|)+", "aaa", 0, 0l);
    ("(?:a|)+", "", 0, 0l);
    ("(?:a?)+", "b", 0, 0l);
    ("(?:a?b?)*", "ab", 0, 0l);
    ("^(?:a?b?)*$", "aabb", 0, 0l);
    ("^(?:a?b?)*$", "a--", 0, 0l);
    ("^(?:a?b?)*$", "", 0, 0l);
    ("(a+)+", "aaa", 0, 0l);
    ("(a+)+b", "aaac", 0, 0l);
    (* {n,m} on a group unrolls into BRAZERO-nested plain groups + SKIPZERO *)
    ("(a){2,4}", "aaaaa", 0, 0l);
    ("(a){2,4}", "a", 0, 0l);
    ("(a){2}", "aa", 0, 0l);
    ("(?:abc){0}x", "x", 0, 0l);
    (* anchored group repeat *)
    ("(a)+", "aa", 0, o_anchored);
    (* NOTEMPTY on a zero-capable group repeat *)
    ("(a)*", "", 0, o_notempty);
    ("(?:a|)+", "b", 0, o_notempty);
    (* PARTIAL × group repeat *)
    ("(a)+", "aa", 0, o_partial_hard);
    ("(a){3}", "aa", 0, o_partial_hard);
    ("(ab)+", "aba", 0, o_partial_soft);
    ("(ab)+c", "abab", 0, o_partial_soft);
    ("(?:ab)+", "aba", 0, o_partial_hard);
    (* --- Chunk E: character classes --- *)
    ("[abc]", "b", 0, 0l);
    ("[abc]", "d", 0, 0l);
    ("[abc]", "", 0, 0l);
    ("[a-z]", "m", 0, 0l);
    ("[a-z]", "5", 0, 0l);
    ("[a-z]", "a", 0, 0l); (* range low boundary *)
    ("[a-z]", "z", 0, 0l); (* range high boundary *)
    ("[^abc]", "d", 0, 0l);
    ("[^abc]", "a", 0, 0l);
    ("[^a-z]", "5", 0, 0l);
    ("[0-9]", "7", 0, 0l);
    ("x[abc]y", "xby", 0, 0l);
    ("(?i)[a-c]", "B", 0, 0l);
    ("[abc]", "zzzb", 0, 0l); (* bump-along to the class hit *)
    (* class at the subject boundaries *)
    ("[a-z]", "abc", 2, 0l);
    ("[a-z]$", "abc", 0, 0l);
    ("^[a-z]", "abc", 0, 0l);
    (* --- Chunk E: class repeats (greedy / lazy / possessive / empty) --- *)
    ("[a-z]+", "abc", 0, 0l);
    ("[a-z]+", "123", 0, 0l);
    ("[a-z]*b", "aab", 0, 0l);
    ("[a-z]*b", "b", 0, 0l);
    ("[0-9]{2,4}", "12345", 0, 0l);
    ("[0-9]{2,4}", "1", 0, 0l);
    ("[0-9]{3}", "1234", 0, 0l);
    ("[abc]*?d", "abcd", 0, 0l);
    ("[abc]+?", "abc", 0, 0l);
    ("[a-z]*", "", 0, 0l); (* empty-capable class repeat *)
    ("[a-z]*", "123", 0, 0l);
    ("[a-z]*a", "aaa", 0, 0l); (* greedy give-back boundary *)
    ("[a-z]++x", "abcx", 0, 0l); (* possessive class repeat *)
    ("[a-z]*+b", "aaab", 0, 0l); (* possessive eats all -> NOMATCH *)
    ("([a-z]+)([0-9]+)", "abc123", 0, 0l);
    (* --- Chunk E: type singles --- *)
    ("\\d", "5", 0, 0l);
    ("\\d", "a", 0, 0l);
    ("\\D", "a", 0, 0l);
    ("\\D", "5", 0, 0l);
    ("\\s", " ", 0, 0l);
    ("\\s", "a", 0, 0l);
    ("\\S", "a", 0, 0l);
    ("\\w", "a", 0, 0l);
    ("\\w", "_", 0, 0l);
    ("\\w", ".", 0, 0l);
    ("\\W", ".", 0, 0l);
    (".", "a", 0, 0l);
    (".", "\n", 0, 0l); (* dot does not match newline *)
    ("(?s).", "\n", 0, 0l); (* dotall: OP_ALLANY matches newline *)
    ("a.c", "abc", 0, 0l);
    ("a.c", "a\nc", 0, 0l);
    (* --- Chunk E: type repeats --- *)
    ("\\d+", "123", 0, 0l);
    ("\\d+", "abc", 0, 0l);
    ("\\d+x", "123x", 0, 0l);
    ("\\d*", "", 0, 0l);
    ("\\d*", "abc", 0, 0l);
    ("\\w{2,3}", "abcd", 0, 0l);
    ("\\w{2,3}", "a", 0, 0l);
    ("\\d+?", "123", 0, 0l);
    ("\\d+?x", "123x", 0, 0l);
    (".*", "abc", 0, 0l);
    (".*b", "aabb", 0, 0l);
    ("(?s).*", "a\nb", 0, 0l);
    ("\\d{3}", "1234", 0, 0l);
    ("\\S+", "ab cd", 0, 0l);
    ("\\D*\\d", "abc5", 0, 0l);
    (* --- Chunk E: \R (default BSR_UNICODE and BSR_ANYCRLF) --- *)
    ("\\R", "\n", 0, 0l);
    ("\\R", "\r", 0, 0l);
    ("\\R", "\r\n", 0, 0l); (* CRLF absorbed as one \R *)
    ("\\R", "\r\nx", 0, 0l);
    ("\\R", "a", 0, 0l);
    ("\\R", "\x0b", 0, 0l); (* VT: matches under default unicode BSR *)
    ("\\R", "\x85", 0, 0l); (* NEL *)
    ("(*BSR_ANYCRLF)\\R", "\x0b", 0, 0l); (* VT: no match under ANYCRLF *)
    ("(*BSR_ANYCRLF)\\R", "\r\n", 0, 0l);
    ("(*BSR_ANYCRLF)\\R", "\n", 0, 0l);
    ("\\R+", "\r\n\n\r", 0, 0l);
    ("\\R*x", "\n\nx", 0, 0l);
    ("a\\Rb", "a\r\nb", 0, 0l);
    (* --- Chunk E: \h \H \v \V --- *)
    ("\\h", " ", 0, 0l);
    ("\\h", "\t", 0, 0l);
    ("\\h", "\xa0", 0, 0l);
    ("\\h", "a", 0, 0l);
    ("\\H", "a", 0, 0l);
    ("\\H", " ", 0, 0l);
    ("\\v", "\n", 0, 0l);
    ("\\v", "\x0b", 0, 0l);
    ("\\v", "\r", 0, 0l);
    ("\\v", "a", 0, 0l);
    ("\\V", "a", 0, 0l);
    ("\\V", "\n", 0, 0l);
    ("\\h+", " \t ", 0, 0l);
    ("\\v*", "\n\r", 0, 0l);
    ("\\H+", "abc def", 0, 0l);
    (* --- Chunk E: \b \B at start / end / between --- *)
    ("\\bfoo", "foo", 0, 0l); (* boundary at start *)
    ("\\bfoo", "xfoo", 0, 0l); (* boundary between x and f, matches at 1 *)
    ("\\bfoo", " foo", 0, 0l);
    ("foo\\b", "foo", 0, 0l); (* boundary at end *)
    ("foo\\b", "foobar", 0, 0l); (* no boundary o|b -> NOMATCH *)
    ("\\bfoo\\b", "foo bar", 0, 0l);
    ("\\bfoo\\b", "foobar", 0, 0l);
    ("a\\bb", "ab", 0, 0l); (* no boundary a|b -> NOMATCH *)
    ("a\\Bb", "ab", 0, 0l); (* no boundary -> \B matches *)
    ("\\Bfoo", "xfoo", 0, 0l); (* no boundary before foo (x|f) -> \B matches *)
    ("\\Bfoo", " foo", 0, 0l); (* boundary -> \B fails at 1 *)
    ("\\w+\\b", "abc def", 0, 0l);
    ("\\b", "", 0, 0l); (* empty subject: no word char -> NOMATCH *)
    ("\\b", "a", 0, 0l); (* boundary at start -> empty match [0,0] *)
    ("\\B", "", 0, 0l); (* empty subject -> \B matches empty [0,0] *)
    ("\\B", "a", 0, 0l); (* every position is a boundary -> NOMATCH *)
    ("\\ba\\b", "a b a", 0, 0l);
    (* \b lowering start_used_ptr affects PARTIAL (prev-char read) *)
    ("\\bfoo", "xfo", 0, o_partial_soft);
    ("\\bfoo", "xfo", 0, o_partial_hard);
    (* --- Chunk E: PARTIAL interplay with classes / types / \R --- *)
    ("\\d\\d", "1", 0, o_partial_soft);
    ("\\d\\d", "1", 0, o_partial_hard);
    ("[a-z]{3}", "ab", 0, o_partial_hard);
    ("[a-z]{3}", "ab", 0, o_partial_soft);
    ("\\d+x", "12", 0, o_partial_soft);
    ("\\R", "\r", 0, o_partial_hard); (* lone CR: partial CRLF *)
    ("a\\Rb", "a\r", 0, o_partial_soft);
    (".{5}", "ab", 0, o_partial_soft);
    ("(?s).{5}", "ab", 0, o_partial_soft);
    (* anchored / endanchored with classes and types *)
    ("[a-z]+", "abc", 0, o_anchored);
    ("\\d+", "12ab", 0, o_endanchored);
    ("\\d+", "12", 0, o_endanchored);
    ("[a-z]", "5a", 0, o_anchored);
    (* --- Chunk F: backreferences --- *)
    (* basic numbered ref (match / miss) *)
    ("(a)\\1", "aa", 0, 0l);
    ("(a)\\1", "ab", 0, 0l);
    ("(abc)\\1", "abcabc", 0, 0l);
    ("(a|b)\\1", "aa", 0, 0l);
    ("(a|b)\\1", "ab", 0, 0l);
    ("(.)\\1", "xx", 0, 0l);
    (* caseless ref *)
    ("(?i)(a)\\1", "aA", 0, 0l);
    ("(?i)(abc)\\1", "abcABC", 0, 0l);
    ("(?i)(a)\\1", "ab", 0, 0l);
    (* unset ref: default NM (forward ref, group not yet defined) *)
    ("\\1(a)", "a", 0, 0l);
    ("(a)?\\1b", "b", 0, 0l); (* optional group unset -> \1 empty -> "b" *)
    (* zero-length group ref *)
    ("(a*)\\1", "aaaa", 0, 0l);
    ("(a*)\\1", "", 0, 0l);
    ("(a?)b\\1", "b", 0, 0l);
    (* nested / sibling referenced captures *)
    ("((a|b))\\1", "aa", 0, 0l);
    ("((a|b))\\1", "ab", 0, 0l);
    ("(a)(b)\\2\\1", "abba", 0, 0l);
    ("(a)(b)\\2\\1", "abab", 0, 0l);
    (* ref repeats: greedy / lazy / bounded, with give-back *)
    ("(ab)\\1*c", "ababc", 0, 0l);
    ("(ab)\\1*?c", "ababc", 0, 0l);
    ("(ab)\\1{2}", "ababab", 0, 0l);
    ("(ab)\\1{2,3}", "abababab", 0, 0l);
    ("(a)\\1+", "aaaa", 0, 0l);
    ("(a)\\1+", "a", 0, 0l);
    ("(.)\\1+", "aabbb", 0, 0l);
    (* self-referential loops (the interpreter is the oracle) *)
    ("(\\1a|b)", "ba", 0, 0l);
    ("(\\1a|b)+", "baa", 0, 0l);
    ("(a\\1?)", "a", 0, 0l);
    ("((a)\\2)+", "aa", 0, 0l);
    (* referenced group inside a repeat, re-entered on backtrack *)
    ("((?:a|ab))+\\1", "abab", 0, 0l);
    ("((a|ab))\\1", "abab", 0, 0l);
    ("(a+)\\1", "aaaa", 0, 0l);
    ("(a+)\\1", "aaaaa", 0, 0l);
    (* PARTIAL x ref *)
    ("(a)\\1", "a", 0, o_partial_soft);
    ("(abc)\\1", "abcab", 0, o_partial_soft);
    ("(abc)\\1", "abcab", 0, o_partial_hard);
    ("(a)\\1+", "aa", 0, o_partial_soft);
    (* anchored / endanchored x ref *)
    ("(a)\\1", "xaa", 0, o_anchored);
    ("(a)\\1", "aa", 0, o_endanchored);
    ("(a)\\1b", "aab", 0, o_endanchored);
    (* ---- Chunk G: lookahead (positive / negative) ---- *)
    ("a(?=b)", "ab", 0, 0l);
    ("a(?=b)", "ac", 0, 0l);
    ("a(?=b)c", "abc", 0, 0l);
    ("a(?!b)", "ac", 0, 0l);
    ("a(?!b)", "ab", 0, 0l);
    ("(?=abc)", "abc", 0, 0l);
    ("(?=abc)a", "abc", 0, 0l);
    ("(?!abc)", "abd", 0, 0l);
    ("(?=a|b)[ab]c", "bc", 0, 0l);
    ("foo(?=bar|baz)", "foobaz", 0, 0l);
    (* captures set inside a positive assertion PERSIST *)
    ("(?=(a))\\1", "a", 0, 0l);
    ("(?=(ab))a", "ab", 0, 0l);
    ("a(?=(b))(b)", "ab", 0, 0l);
    (* backtrack INTO a positive assertion fails for the atomic (?=...) *)
    ("(?=(a)|(a))\\2", "a", 0, 0l);
    ("(?:(?=(a+))\\1)+", "aaa", 0, 0l);
    (* negative assertion captures do NOT leak on success *)
    ("(?!(b))a", "a", 0, 0l);
    ("a(?!(b))c", "ac", 0, 0l);
    (* ---- Chunk G: lookbehind (fixed / variable) ---- *)
    ("(?<=a)b", "ab", 0, 0l);
    ("(?<=a)b", "xb", 0, 0l);
    ("(?<=abc)d", "abcd", 0, 0l);
    ("a(?<=a)", "a", 0, 0l);
    ("(?<!a)b", "cb", 0, 0l);
    ("(?<!a)b", "ab", 0, 0l);
    ("(?<=\\d{2,3})x", "12x", 0, 0l);
    ("(?<=\\d{2,3})x", "123x", 0, 0l);
    ("(?<=\\d{2,3})x", "1x", 0, 0l);
    ("(?<=a\\d?b)c", "abc", 0, 0l);
    ("(?<=a\\d?b)c", "a1bc", 0, 0l);
    ("(?<=(ab|c))d", "cd", 0, 0l);
    ("(?<=(ab|c))d", "abd", 0, 0l);
    ("(?<!\\d{2})x", "1x", 0, 0l);
    ("(?<!\\d{2})x", "12x", 0, 0l);
    (* \b-style boundary at subject start with lookbehind *)
    ("(?<![a-z])a", "a", 0, 0l);
    ("(?<![a-z])a", "ba", 0, 0l);
    ("\\b(?<=\\s)x", " x", 0, 0l);
    (* lookbehind + PARTIAL interplay + start floor *)
    ("(?<=abc)d", "abc", 0, o_partial_soft);
    ("a(?<=\\d?a)", "a", 0, 0l);
    (* ---- Chunk G: atomic groups ---- *)
    ("(?>a+)a", "aaa", 0, 0l);
    ("(?>a+)b", "aaab", 0, 0l);
    ("(?>a|ab)c", "abc", 0, 0l);
    ("(?>a|ab)c", "ac", 0, 0l);
    ("(?>(a)|(ab))c", "abc", 0, 0l);
    ("(?>(a)b)c", "abc", 0, 0l);
    ("a(?>bc|b)c", "abcc", 0, 0l);
    ("a(?>bc|b)c", "abc", 0, 0l);
    ("(?>\\d+)\\d", "123", 0, 0l);
    ("((?>a|ab))(c)", "abc", 0, 0l);
    (* captures inside atomic groups survive; backtrack-past restores them *)
    ("(?>(a)(b))\\1", "aba", 0, 0l);
    ("(?>(a)|b)x", "ax", 0, 0l);
    ("(?>(a+))(b)", "aaab", 0, 0l);
    ("x(?>(a)|(b))*y", "xaby", 0, 0l);
    (* possessive ref repeat (lowers to atomic group) *)
    ("(a)\\1++", "aaa", 0, 0l);
    ("(a)\\1++b", "aab", 0, 0l);
    (* atomic group + anchors / partial *)
    ("(?>abc)", "abc", 0, o_partial_soft);
    ("(?>a+)", "aaa", 0, o_anchored);
    (* ---- Chunk G: possessive brackets ---- *)
    ("(?:ab)++", "ababab", 0, 0l);
    ("(?:ab)++c", "ababc", 0, 0l);
    ("(?:ab)++a", "abab", 0, 0l); (* possessive over-eats -> no give-back -> fail *)
    ("(?:ab)*+c", "c", 0, 0l);
    ("(?:ab)*+c", "ababc", 0, 0l);
    ("(a)++", "aaa", 0, 0l);
    ("(a)++", "aaab", 0, 0l);
    ("(a)*+b", "aaab", 0, 0l);
    ("(a)*+b", "b", 0, 0l);
    ("(a|b)++", "abab", 0, 0l);
    ("(a|b)++x", "abx", 0, 0l);
    ("((a)b)++", "abab", 0, 0l);
    (* possessive capture value = last committed iteration; backref after *)
    ("(a)++\\1", "aaaa", 0, 0l);
    ("(ab)++\\1", "ababab", 0, 0l);
    ("(\\d)++", "123", 0, 0l);
    (* possessive that can match empty *)
    ("(a?)*+", "aa", 0, 0l);
    ("(?:a?)*+b", "aab", 0, 0l);
    ("(?:)*+x", "x", 0, 0l);
    (* nested possessive / possessive inside atomic *)
    ("(?:a+)++b", "aaab", 0, 0l);
    ("(?>(a)++)b", "aaab", 0, 0l);
    ("x(?:(a)|(b))++y", "xaby", 0, 0l);
    (* possessive + partial / anchored *)
    ("(?:ab)++", "abab", 0, o_partial_soft);
    ("(a)++", "aaa", 0, o_anchored);
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
    (* Chunk D2: group-repeat tick parity. A repeated group is frame-per-
       iteration in the C (grouploop RMATCH + KETRMAX RM7), so the two engines
       must tick identically at EVERY match-limit N — sweeping N across the
       whole boundary is a stronger pin than a single value. Both must give
       -47 below the trip point and the SAME result at/above it. *)
    Alcotest.test_case "LIMIT_MATCH sweep, group repeat (fast == interp)" `Quick
      (fun () ->
        List.iter
          (fun (body, subj) ->
            for n = 1 to 40 do
              let f, e = run_both body n subj in
              Alcotest.(check string)
                (Printf.sprintf "fast == interp for /%s/ on %S at N=%d" body subj
                   n)
                e f
            done)
          [
            ("(a)+", "aaa");
            ("(a)*b", "aaab");
            ("(ab|c)+", "abcab");
            ("(?:ab)+", "abab");
            ("(a)+?b", "aaab");
            ("(a*)*", "aaa");
            ("(?:a|)+", "aaa");
            ("(a){2,4}", "aaaaa");
          ]);
    (* Chunk E: type/class-repeat tick parity. A greedy type/class repeat
       over-eats then backs off one code unit per RMATCH (each a tick); the
       CLASS maxbt also ticks the floor position (pcre2_match.c:2143), so the
       two engines must trip -47 at the SAME N. Sweep N across the boundary. *)
    Alcotest.test_case "LIMIT_MATCH sweep, type/class repeat (fast == interp)"
      `Quick (fun () ->
        List.iter
          (fun (body, subj) ->
            for n = 1 to 40 do
              let f, e = run_both body n subj in
              Alcotest.(check string)
                (Printf.sprintf "fast == interp for /%s/ on %S at N=%d" body subj
                   n)
                e f
            done)
          [
            ("\\d*\\d", "1111"); (* type-repeat greedy give-back *)
            ("[a-z]*a", "aaaa"); (* class-repeat greedy give-back (floor tick) *)
            ("\\d+?\\d", "1111"); (* lazy type-repeat extend *)
            ("[a-z]{2,5}z", "aaaaaz"); (* class range repeat give-back *)
            ("\\w+\\d", "abc1"); (* type-repeat over a mixed run *)
            ("\\R+x", "\r\n\nx"); (* \R repeat give-back over CRLF *)
          ]);
    (* Chunk F: ref-repeat tick parity. A greedy ref repeat over-eats then gives
       back one COPY per RMATCH (RM21) down to and including the floor
       (pcre2_match.c:5154-5162); a lazy one extends by one copy per RMATCH
       (RM20). The two engines must trip -47 at the SAME N. Sweep across the
       boundary; include self-referential loops and captured group repeats. *)
    Alcotest.test_case "LIMIT_MATCH sweep, ref repeat (fast == interp)" `Quick
      (fun () ->
        List.iter
          (fun (body, subj) ->
            for n = 1 to 40 do
              let f, e = run_both body n subj in
              Alcotest.(check string)
                (Printf.sprintf "fast == interp for /%s/ on %S at N=%d" body subj
                   n)
                e f
            done)
          [
            ("(a)\\1*b", "aaaaab"); (* greedy ref give-back *)
            ("(a)\\1*?b", "aaaaab"); (* lazy ref extend *)
            ("(a)\\1{2,4}b", "aaaaaab"); (* bounded ref give-back *)
            ("(ab)\\1+c", "ababababc"); (* multi-unit ref repeat *)
            ("(\\1a|b)+", "baaa"); (* self-referential loop *)
            ("((a)\\2)+", "aaaa"); (* nested captured ref repeat *)
            ("(a+)\\1", "aaaaaa"); (* variable-length ref *)
          ]);
    (* Chunk G: lookaround / atomic tick parity. Positive/negative assertions
       loop over branches (RM3/RM4, each a tick), atomic groups run grouploop
       (RM2) then commit, variable lookbehinds try back-lengths one at a time
       (RM37, each a tick). The two engines must trip -47 at the SAME N. Sweep
       across the boundary. *)
    Alcotest.test_case "LIMIT_MATCH sweep, lookaround/atomic (fast == interp)"
      `Quick (fun () ->
        List.iter
          (fun (body, subj) ->
            for n = 1 to 40 do
              let f, e = run_both body n subj in
              Alcotest.(check string)
                (Printf.sprintf "fast == interp for /%s/ on %S at N=%d" body subj
                   n)
                e f
            done)
          [
            ("a(?=b|c|d)x", "ax"); (* pos lookahead branch loop, fails *)
            ("a(?=b|c|d)", "ad"); (* pos lookahead, last branch matches *)
            ("a(?!b|c|d)", "ae"); (* neg lookahead all branches fail *)
            ("a(?!b|c|d)", "ab"); (* neg lookahead a branch matches *)
            ("(?>a|ab|abc)c", "abc"); (* atomic commits first branch *)
            ("(?>a+)b", "aaaab"); (* atomic possessive-like, then continue *)
            ("(?>a+)a", "aaaa"); (* atomic over-eats, no give-back -> fail *)
            ("(?<=\\d{2,4})x", "1234x"); (* variable lookbehind back-length loop *)
            ("(?<=a{1,3})b", "aaab"); (* variable lookbehind, bounded *)
            ("(?:(?=(a+))\\1)+b", "aaab"); (* nested pos-assert capture loop *)
            ("(a)\\1++b", "aaaab"); (* possessive ref repeat -> atomic *)
            ("(?:a|b)++x", "abab"); (* possessive: one RM8 tick per branch attempt *)
            ("(?:ab)++c", "ababab"); (* possessive multi-unit iterations *)
            ("(a)++b", "aaab"); (* possessive capture iterations *)
            ("(?:a?)*+b", "aab"); (* possessive with empty-match break *)
            ("(?:a+)++b", "aaab"); (* nested possessive *)
          ]);
    Alcotest.test_case "LIMIT_MATCH sweep, verbs (fast == interp)" `Quick
      (fun () ->
        List.iter
          (fun (body, subj) ->
            for n = 1 to 40 do
              let f, e = run_both body n subj in
              Alcotest.(check string)
                (Printf.sprintf "fast == interp for /%s/ on %S at N=%d" body subj
                   n)
                e f
            done)
          [
            ("a(*PRUNE)b|ac", "ac"); (* PRUNE fails to bumpalong *)
            ("a(*SKIP)b|ac", "ac"); (* SKIP advances the start *)
            ("a(*THEN)b|ad", "ad"); (* THEN to next alternative *)
            ("(a(*THEN)b|c(*THEN)d)e", "cde"); (* THEN in nested alternation *)
            ("a(*COMMIT)b|ac", "ac"); (* COMMIT disables bumpalong *)
            ("(*MARK:a)x(*SKIP:a)y|z", "xyz"); (* SKIP:name matched mark *)
            ("(*MARK:b)x(*SKIP:a)y|z", "xzz"); (* SKIP:name unmatched -> rerun *)
            ("\\d(*SKIP:x)\\d\\d\\d(*SKIP:x)|", "12345"); (* skiparg cubic-ish *)
            ("(?:a(*THEN)b|c)+d", "cccd"); (* THEN inside a repeated group *)
            ("(?=a(*THEN)b|c)cd", "cd"); (* THEN in an atomic pos assertion *)
            ("(?!a(*COMMIT))b", "b"); (* COMMIT in negative assertion *)
            ("a(*MARK:m)(b(*PRUNE)c|d)", "ad"); (* MARK + PRUNE in a group *)
          ]);
  ]

(* ---------- 6b. backref compile-option parity vs the interpreter ----------

   MATCH_UNSET_BACKREF (0x200, compile option) and DUPNAMES (0x40, needed for
   OP_DNREF) are compile options, so [parity_tests] (which compiles with 0
   options) cannot exercise them; these compile with the option and compare
   exec_full to the interpreter over each subject. *)
let munset = 0x00000200l

let ref_options_cases : (string * int32 * string) list =
  [
    (* MATCH_UNSET_BACKREF: an unset reference matches empty rather than failing
       (compare with the default in parity_cases: "\\1(a)" NM there). *)
    ("\\1(a)", munset, "a");
    ("(a\\1)", munset, "a"); (* self-ref, group in progress -> unset -> empty *)
    ("(a)?\\1b", munset, "b"); (* optional group skipped: \1 empty *)
    ("(a)?\\1b", munset, "ab"); (* group set: \1 = "a" *)
    ("(a)\\1{2}", munset, "aaa"); (* set group: MUB has no effect *)
    ("(a)?\\1{2,4}c", munset, "c"); (* unset repeated ref -> empty, then 'c' *)
    (* DUPNAMES + \k<n> => OP_DNREF: first-set-wins scan across the name list. *)
    ("(?:(?<n>a)|(?<n>b))\\k<n>", dupnames, "aa");
    ("(?:(?<n>a)|(?<n>b))\\k<n>", dupnames, "bb");
    ("(?:(?<n>a)|(?<n>b))\\k<n>", dupnames, "ab");
    ("(?:(?<n>a)|(?<n>ab))\\k<n>", dupnames, "abab");
    ("(?:(?<n>a)|(?<n>b))\\k<n>+", dupnames, "baaa");
    ("(?:(?<n>a)|(?<n>b))\\k<n>*c", dupnames, "aaac");
    (* DUPNAMES with MATCH_UNSET_BACKREF: an unmatched-name-alternative ref. *)
    ( "(?:(?<n>a)(?<n>b)|(?<n>c))\\k<n>",
      Int32.logor dupnames munset,
      "cc" );
  ]

let ref_options_tests =
  dnref_golden_test
  :: List.map
       (fun (pat, copts, subj) ->
         Alcotest.test_case
           (Printf.sprintf "ref-opts %S copt=0x%lx on %S" pat copts subj)
           `Quick
           (fun () ->
             match (F.compile pat copts, E.compile pat copts) with
             | Ok fre, Ok ere ->
                 let f = norm (F.exec_full fre subj 0 0l) in
                 let e = norm (E.exec_full ere subj 0 0l) in
                 Alcotest.(check string)
                   (Printf.sprintf "fast vs interp for %S/%S" pat subj)
                   e f
             | Error (F.Unsupported _), _ -> ()
             | Error _, _ | _, Error _ ->
                 Alcotest.failf "compile mismatch for %S" pat))
       ref_options_cases

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
    (* Chunk E: a lazy \R repeat extending over a newline-heavy subject drives
       the KIND_REP_MIN \R extension handler (rep_bt_min_anynl) once per CRLF
       pair (~50k times before the trailing 'z' matches) — it must stay
       allocation-free (§8: no tuples per iteration in the frame loop).
       NO_AUTO_POSSESS keeps \R+? a true MINPLUS (without it the compiler
       auto-possessifies \R+?z since 'z' cannot match \R, and the REP_MIN path
       never runs); the trailing 'z' also satisfies the req_cu start
       optimization so the attempt runs at all. Path heaviness verified
       empirically: with a LIMIT_MATCH=10000 verb prefix this exec trips -47,
       so the extension handler runs > 10k times. *)
    Alcotest.test_case "alloc: O(1) lazy \\R extension over ~50k CRLF pairs"
      `Slow (fun () ->
        let o_no_auto_possess = 0x00004000l (* PCRE2_NO_AUTO_POSSESS *) in
        match F.compile "\\R+?z" o_no_auto_possess with
        | Error _ -> Alcotest.fail "compile failed"
        | Ok re ->
            let b = Buffer.create ((2 * 50_000) + 1) in
            for _ = 1 to 50_000 do
              Buffer.add_string b "\r\n"
            done;
            Buffer.add_char b 'z';
            let subject = Buffer.contents b in
            let expect_match r =
              match r with
              | Ok (Some (0, 100_001)) -> ()
              | Ok (Some _) | Ok None -> Alcotest.fail "expected match [0,100001]"
              | Error c -> Alcotest.failf "unexpected error %d" c
            in
            expect_match (F.exec re subject 0 0l) (* warm-up *);
            let before = Gc.minor_words () in
            let r = F.exec re subject 0 0l in
            let delta = Gc.minor_words () -. before in
            expect_match r;
            Alcotest.(check bool)
              (Printf.sprintf
                 "expected O(1) minor allocation for lazy \\R extension over \
                  ~50k CRLF pairs, measured %.0f words"
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
      ("backref option parity", ref_options_tests);
      ("limit-match boundary", limit_match_boundary_test);
      ("alloc pins", alloc_tests);
      ("stack safety", stack_safety_tests);
    ]
