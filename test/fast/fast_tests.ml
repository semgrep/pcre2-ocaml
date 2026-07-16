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
    (* SKIPZERO: a {0}-quantified group is COMPILED behind a forward-skip JMP
       (chunk K2, so a recursion can call a group defined only in a {0} block);
       forward flow jumps over the dead group. *)
    ( "(?:abc){0}x",
      g
        [
          "  0 BRA";
          "  1 JMP 8";
          "  3 BRA";
          "  4 CHAR_RUN \"abc\"";
          "  7 KET";
          "  8 CHAR_RUN \"x\"";
          " 11 KET";
          " 12 END";
        ] );
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
          "  6 GROUP_START g=0";
          "  8 BRA";
          "  9 ALT next=16";
          " 11 CHAR_RUN \"b\"";
          " 14 JMP 17";
          " 16 FAIL";
          " 17 ASSERT_END atomic g=0";
          " 20 CHAR_RUN \"c\"";
          " 23 KET";
          " 24 END";
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
          " 15 FAIL_NASSERT";
          " 16 NASSERT_MATCH";
          " 17 KET";
          " 18 END";
        ] );
    ( "(?<=ab)c",
      g
        [
          "  0 BRA";
          "  1 ONCE";
          "  3 GROUP_START g=0";
          "  5 BRA";
          "  6 ALT next=15";
          "  8 REVERSE 2";
          " 10 CHAR_RUN \"ab\"";
          " 13 JMP 16";
          " 15 FAIL";
          " 16 ASSERT_END atomic g=0";
          " 19 CHAR_RUN \"c\"";
          " 22 KET";
          " 23 END";
        ] );
    ( "(?<=\\d{2,3})x",
      g
        [
          "  0 BRA";
          "  1 ONCE";
          "  3 GROUP_START g=0";
          "  5 BRA";
          "  6 ALT next=25";
          "  8 VREVERSE {2,3}";
          " 11 TYPE_REP min {2,2} \\d";
          " 16 TYPE_REP max {0,1} \\d";
          " 21 ASSERTBACK_CHECK g=0";
          " 23 JMP 26";
          " 25 FAIL";
          " 26 ASSERT_END atomic g=0";
          " 29 CHAR_RUN \"x\"";
          " 32 KET";
          " 33 END";
        ] );
    ( "(?>a|b)c",
      g
        [
          "  0 BRA";
          "  1 ONCE";
          "  3 BRA";
          "  4 ALT next=11";
          "  6 CHAR_RUN \"a\"";
          "  9 JMP 19";
          " 11 ALT next=18";
          " 13 CHAR_RUN \"b\"";
          " 16 JMP 19";
          " 18 FAIL";
          " 19 ONCE_END";
          " 20 CHAR_RUN \"c\"";
          " 23 KET";
          " 24 END";
        ] );
    ( "(?:ab)++c",
      g
        [
          "  0 BRA";
          "  1 POSSESS ovbase=0 zero=0";
          "  4 BRA";
          "  5 ALT next=16";
          "  7 CHAR_RUN \"ab\"";
          " 10 JMP 12";
          " 12 KETRPOS entry=4 ovbase=0 n=-2";
          " 16 POSSESS_DONE n=-2";
          " 18 CHAR_RUN \"c\"";
          " 21 KET";
          " 22 END";
        ] );
    ( "(a)*+",
      g
        [
          "  0 BRA";
          "  1 POSSESS ovbase=2 zero=1";
          "  4 BRA";
          "  5 ALT next=16";
          "  7 CHAR_RUN \"a\"";
          " 10 JMP 12";
          " 12 KETRPOS entry=4 ovbase=2 n=1";
          " 16 POSSESS_DONE n=1";
          " 18 KET";
          " 19 END";
        ] );
  ]

(* Chunk I2 goldens — the four new tags + the UCP word boundary encoding.
   pt_any = 0, pt_gc = 2 (pdata Ucp.ucp_l = 1), pt_pc = 3 (pdata Ucp.ucp_lu =
   9); PROP width 4, PROP_REP width 7, EXTUNI width 1, EXTUNI_REP width 4. *)
let i2_goldens : (string * string) list =
  [
    (* \p{Any} itself compiles to OP_ALLANY (a compile-time optimization), so
       the PROP single golden uses a real property. *)
    ( "\\p{Lu}",
      g [ "  0 BRA"; "  1 PROP ptype=3 pdata=9"; "  5 KET"; "  6 END" ] );
    ( "\\P{L}",
      g [ "  0 BRA"; "  1 NOTPROP ptype=2 pdata=1"; "  5 KET"; "  6 END" ] );
    (* auto-possessified: \p{Lu}+ at the end of the pattern becomes POSPLUS. *)
    ( "\\p{Lu}+",
      g
        [
          "  0 BRA";
          "  1 PROP_REP pos {1,inf} is ptype=3 pdata=9";
          "  8 KET";
          "  9 END";
        ] );
    (* the shared compiler splits {2,4}? into EXACT{2} + MINUPTO{0,2}. *)
    ( "\\P{L}{2,4}?",
      g
        [
          "  0 BRA";
          "  1 PROP_REP min {2,2} not ptype=2 pdata=1";
          "  8 PROP_REP min {0,2} not ptype=2 pdata=1";
          " 15 KET";
          " 16 END";
        ] );
    ("\\X", g [ "  0 BRA"; "  1 EXTUNI"; "  2 KET"; "  3 END" ]);
    (* {2,} splits into EXACT{2} + auto-possessified STAR. *)
    ( "\\X{2,}",
      g
        [
          "  0 BRA";
          "  1 EXTUNI_REP min {2,2}";
          "  5 EXTUNI_REP pos {0,inf}";
          "  9 KET";
          " 10 END";
        ] );
    ( "(*UCP)\\bx\\B",
      g
        [
          "  0 BRA";
          "  1 WORDBOUND \\b ucp";
          "  3 CHAR_RUN \"x\"";
          "  6 WORDBOUND \\B ucp";
          "  8 KET";
          "  9 END";
        ] );
  ]

(* Chunk J goldens — conditionals (OP_COND/OP_SCOND). The CREF group in a
   conditional is non-optimized (CAP_START_REF); a DEFINE group NOT otherwise
   referenced stays optimized (CAP_START). *)
let j_goldens : (string * string) list =
  [
    (* Two-branch numbered CREF: the referenced group 1 is a CAP_*_REF; the
       COND_CREF tests ovbase=2, jumping to the no-branch on FALSE. *)
    ( "(a)(?(1)b|c)",
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
          " 14 COND_CREF ovbase=2 no=22";
          " 17 CHAR_RUN \"b\"";
          " 20 JMP 25";
          " 22 CHAR_RUN \"c\"";
          " 25 KET";
          " 26 KET";
          " 27 END";
        ] );
    (* Positive assertion condition: the KIND_NASSERT boundary (COND_ASSERT)
       runs the assertion body grouploop-style; a match reaches
       COND_ASSERT_MATCH -> yes-branch, else -> no-branch (nomatch=). *)
    ( "(?(?=a)b|c)",
      g
        [
          "  0 BRA";
          "  1 COND_ASSERT nomatch=19";
          "  3 BRA";
          "  4 ALT next=11";
          "  6 CHAR_RUN \"a\"";
          "  9 JMP 12";
          " 11 FAIL_NASSERT";
          " 12 COND_ASSERT_MATCH match=14";
          " 14 CHAR_RUN \"b\"";
          " 17 JMP 22";
          " 19 CHAR_RUN \"c\"";
          " 22 KET";
          " 23 KET";
          " 24 END";
        ] );
    (* Negative assertion condition: match_target/nomatch_target are swapped
       (a matching assertion body -> the NO branch). *)
    ( "(?(?!a)b|c)",
      g
        [
          "  0 BRA";
          "  1 COND_ASSERT nomatch=14";
          "  3 BRA";
          "  4 ALT next=11";
          "  6 CHAR_RUN \"a\"";
          "  9 JMP 12";
          " 11 FAIL_NASSERT";
          " 12 COND_ASSERT_MATCH match=19";
          " 14 CHAR_RUN \"b\"";
          " 17 JMP 22";
          " 19 CHAR_RUN \"c\"";
          " 22 KET";
          " 23 KET";
          " 24 END";
        ] );
    (* (?(DEFINE)…) compiles the condition to OP_FALSE -> COND_FALSE (always the
       no-branch = the group KET, matching empty); the DEFINE body is the
       never-taken yes-branch (an optimized capture, never referenced here). *)
    ( "(?(DEFINE)(?<x>y))z",
      g
        [
          "  0 BRA";
          "  1 COND_FALSE no=16";
          "  3 CAP_START ovbase=2";
          "  5 BRA";
          "  6 ALT next=13";
          "  8 CHAR_RUN \"y\"";
          " 11 JMP 14";
          " 13 FAIL";
          " 14 CAP_END ovbase=2";
          " 16 KET";
          " 17 CHAR_RUN \"z\"";
          " 20 KET";
          " 21 END";
        ] );
    (* A repeated single-branch conditional that can match empty -> OP_SCOND:
       the GROUP_START/SCOND_DESCEND pair (empty-check + RM35 descend) before the
       condition test, and a KET_RMAX looping back to the GROUP_START. *)
    ( "(a)?(?(1)b)*",
      g
        [
          "  0 BRA";
          "  1 BRAZERO skip=16";
          "  3 CAP_START_REF ovbase=2";
          "  5 BRA";
          "  6 ALT next=13";
          "  8 CHAR_RUN \"a\"";
          " 11 JMP 14";
          " 13 FAIL";
          " 14 CAP_END_REF ovbase=2";
          " 16 BRAZERO skip=30";
          " 18 GROUP_START g=0";
          " 20 SCOND_DESCEND";
          " 21 COND_CREF ovbase=2 no=27";
          " 24 CHAR_RUN \"b\"";
          " 27 KET_RMAX entry=18 g=0";
          " 30 KET";
          " 31 END";
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
    (goldens @ i2_goldens @ j_goldens)

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

(* Chunk J: a COND_DNCREF golden needs DUPNAMES (the duplicate-name group list
   the condition scans). Two alternatives (?<A>a) / (?<A>b) share the name A;
   the COND_DNCREF tests whether either is set (slot=0, count=2). *)
let dncref_golden_test =
  Alcotest.test_case "dump COND_DNCREF (dupnames)" `Quick (fun () ->
      let ir =
        match
          C.pcre2_compile "(?:(?<A>a)|(?<A>b))(?(<A>)x|y)"
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
            " 33 COND_DNCREF slot=0 count=2 no=42";
            " 37 CHAR_RUN \"x\"";
            " 40 JMP 45";
            " 42 CHAR_RUN \"y\"";
            " 45 KET";
            " 46 KET";
            " 47 END";
          ]
      in
      Alcotest.(check string)
        "COND_DNCREF dump" expected
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
       so (a)\1 is accepted — no unsupported entry. Chunk J supports conditionals
       (OP_COND/OP_SCOND + CREF/DNCREF/RREF/DNRREF/FALSE/TRUE + assertion
       conditions), so "(a)(?(1)b|c)" and the like are now ACCEPTED (parity pinned
       in parity_cases below). *)
    (* Chunk G supports atomic groups (OP_ONCE), so a possessive ref repeat
       "(a)\\1++" (compiled as (?>\\1+)) is now ACCEPTED — no unsupported entry. *)
    (* Chunk G supports possessive groups (BRAPOS/CBRAPOS/... + KETRPOS +
       BRAPOSZERO), so "(a)++", "(?:ab)++", "(a)*+" are now ACCEPTED. Chunk K's
       G+ cleanup lowers a REPEATED atomic group (Once ... KetRmax, e.g.
       "(?>a)+" / "(?>a|ab)*") — each iteration re-pushes the KIND_ONCE boundary
       (the loop-back target is the t_once) so the atomic commit is per-iteration
       and the snapshot restores the pre-iteration state on give-back — so it is
       now ACCEPTED (parity pinned in parity_cases below). *)
    (* Chunk K1b lowers OP_RECURSE in all forms (whole-pattern (?R)/(?0),
       subroutine calls (?n)/(?&name)/(?P>name)/relative, mutual/self recursion,
       (?(R)…)/(?(Rn)…)/(?(R&name)…) conditions, ( *ACCEPT)-in-recursion, verbs
       and the RECURSELOOP -52 check via last_used_ptr) — the FAT KIND_RECURSE
       record snapshots the whole ovector + group_start + cap_start + mark +
       current_recurse + once_base, so "(?R)", "(a(?1)?b)", "(?(R)a|b)" etc. are
       now ACCEPTED (parity pinned in parity_cases / the LIMIT_MATCH sweeps).
       Chunk K2 lowered the last recursion shape — a call INTO a possessive
       capture (OP_CBRAPOS/OP_SCBRAPOS, e.g. "(a)++(?1)b"): the recursion runs the
       branches grouploop-style and returns at OP_KETRPOS like a normal recursion
       (pcre2_match.c:6056-6074, bypassing the possessive loop), so it is now
       ACCEPTED (parity in parity_cases). *)
    (* Chunk I supports UTF mode and OP_XCLASS (wide chars, ranges AND \p
       properties, via the self-contained Xclass helper), so "[\\p{L}]" and
       "(*UTF)abc" are ACCEPTED. Chunk I2 lowered the rest of the chunk-I
       decline list — standalone \p / \P and their repeats, \X, UCP mode,
       PCRE2_MATCH_INVALID_UTF, caseless backrefs in UTF, UTF lookbehind and
       \R / (?s). / \C repeats in UTF — so none of those decline any more
       (parity is pinned in parity_cases / the LIMIT_MATCH sweeps below). *)
    (* Chunk H supports MARK/PRUNE/SKIP/THEN/COMMIT (+_ARG), FAIL, ACCEPT and
       OP_CLOSE (so "a(*FAIL)", "(*MARK:x)a", "a(*PRUNE)b", "a(*ACCEPT)" are
       ACCEPTED). Chunk K2 lowered the last two verb combinations — ( *ACCEPT)
       inside an assertion (propagated to the innermost assertion boundary by
       backtrack_accept) and ( *THEN) with a NON-ATOMIC positive assertion (the
       once_na_assert KIND_ONCE boundary now contains it) — so both are now
       ACCEPTED (parity in parity_cases). *)
    (* Chunk K lowers OP_CALLOUT / OP_CALLOUT_STR as pure no-ops (this library
       surfaces no callout function, so do_callout is always 0 in both engines)
       and the OP_SCRIPT_RUN group (the sr / script_run verb, its ket applies the
       script-checking rules to the matched span), so "a(?C1)b", AUTO_CALLOUT
       patterns, a MANUAL callout before a condition assertion "(?(?C1)(?=a)b|c)",
       "(*sr:\\d+)" and "(*asr:\\d+)" are now ACCEPTED (parity in parity_cases). *)
    (* Chunk K2 lowered \K (OP_SET_SOM), ( *ACCEPT)-in-assertion, ( *THEN)+NA and
       OP_RECURSE-into-possessive-capture. Chunk L lowered PCRE2_FIRSTLINE (the
       last compile-level OPTION gate) at match time (runner bump_top /
       bump_bottom), so the compiler now declines NOTHING in subset: [Unsupported]
       is unreachable for every in-scope pattern (asserted by [zero_unsupported_test]
       below, which compiles a sweep over all previously-declined constructs). The
       list is intentionally empty. *)
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
    (* Chunk I: UTF mode + XCLASS (wide chars / ranges / \p in a class). *)
    "(*UTF)abc";
    "(*UTF)\\x{100}\\x{1000}\\x{10000}";
    "(*UTF).\\C[a-z]";
    "(*UTF)\\x{100}+";
    "(*UTF)[\\x{100}-\\x{200}]+";
    "[\\p{L}]";
    "(*UTF)[\\p{Nd}\\x{100}-\\x{200}]*";
    "(*UTF)(?i)\\x{c9}";
    "(*UTF)\\b\\w+\\B";
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
    (* Chunk K (K1a): callout no-ops, script runs, repeated atomic groups /
       assertions must be ACCEPTED (ir_of fails on decline — a regression
       re-introducing an Unsupported here fails these cases, not just the
       silently-skipping parity/sweep suites). *)
    "a(?C1)b";
    "(*sr:\\d+)";
    "(*asr:\\d+)";
    "(?(?C1)(?=a)b|c)";
    "(?=a)*";
    "(?!x)*";
    "(?<=x)+";
    "(?>ab){2,4}";
    "(?>a)+";
    "(?>a|ab)+c";
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
    (* Chunk J: conditionals. *)
    "(a)(?(1)b|c)";
    "(a)?(?(1)b)";
    "(a)?(?(1)b|c)*";
    "(a)?(?(1)b)*";
    "(a)?(?(1)b)+?";
    "(a)?(?(1)|c)";
    "(?(?=a)b|c)";
    "(?(?!a)b|c)";
    "(?(?<=a)b|c)";
    "(?(?<=a{2,3})x|y)";
    "(?(?=(a))b\\1|c)";
    "(?(DEFINE)(?<x>abc))d";
    "(?(R)a|b)";
    "(?(VERSION>=10)y|n)";
    "(a)?(b)?(?(1)(?(2)x|y)|z)";
    "(?>(a)(?(1)b|c))";
    "(?:(a)(?(1)b|c))++";
    "(a)?(?(1)b)*+";
    "(?(?=a)b)*";
    "(?(?=a(*THEN)b)x|y)";
    "(a)(?(1)b(*THEN)c|d)";
    (* Chunk K: callouts (no-ops), script runs, repeated atomic groups /
       assertions. *)
    "a(?C1)b";
    "a(?C\"m\")b(?C2)c";
    "(a)(?C1)(?(1)b|c)";
    "(?(?C1)(?=a)b|c)";
    "(*sr:\\d+)";
    "(*sr:.*(*ACCEPT)ZZ)";
    "^(*sr:A|)*BCD";
    "(*asr:\\d+)";
    "(?>a|ab)+c";
    "(?>a)+";
    "(?>a)*";
    "(?>a?)*b";
    "(?>a|ab)*?c";
    "(?>(a)|(b))+";
    "(?=a)*ab";
    "(?=abc)+abc";
    "(?!x)*ab";
    "(?<=x)+y";
    "(?>ab){2,4}c";
    (* Chunk K1b: recursion — the compiled IR must always verify. *)
    "\\(([^()]|(?R))*\\)";
    "(a(b)|(c))(?1)";
    "(?(DEFINE)(?<A>a))B(?1)C";
    "(?P<L1>(?P<L2>0)(?P>L1)|(?P>L2))";
    "(?(R)a+|(?R)b)";
    "^((.)(?1)\\2|.)$";
    "(?<=b(?1))(a)";
    "(?:(?1)|B)(A(*ACCEPT)XX|C)D";
    "(?+1)x([ab])";
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
    (* ---------- Chunk I: UTF-8 parity spot-checks ----------
       Subjects are raw UTF-8 byte strings: é = C3 A9, Ⱥ = C8 BA, ⱥ = E2 B1 A5,
       Ā = C4 80, α = CE B1, Α = CE 91, € = E2 82 AC (U+20AC),
       em-space = E2 80 83 (U+2003), 😀 = F0 9F 98 80 (U+1F600). *)
    (* multi-byte literal (byte-exact caseful run), 2/3/4-byte *)
    ("(*UTF)caf\\x{e9}", "caf\xc3\xa9", 0, 0l);
    ("(*UTF)\\x{20ac}", "\xe2\x82\xac", 0, 0l);
    ("(*UTF)\\x{1f600}x", "\xf0\x9f\x98\x80x", 0, 0l);
    ("(*UTF)b", "\xc3\xa9b", 0, 0l) (* match past a 2-byte char *);
    (* dot over code points (not a byte) *)
    ("(*UTF)a.b", "a\xc3\xa9b", 0, 0l);
    ("(*UTF)a.b", "a\xe2\x82\xacb", 0, 0l);
    (* caseless code-point fold > 127 (Greek Α/α, Latin É/é) *)
    ("(*UTF)(?i)\\x{391}", "\xce\xb1", 0, 0l);
    ("(*UTF)(?i)\\x{c9}", "\xc3\xa9", 0, 0l);
    ("(*UTF)(?i)\\x{23a}", "\xe2\xb1\xa5", 0, 0l) (* Ⱥ matches ⱥ *);
    (* single negated char over a multi-byte char *)
    ("(*UTF)[^x]", "\xc3\xa9", 0, 0l);
    ("(*UTF)(?i)[^\\x{391}]", "\xce\xb1", 0, 0l) (* caseless NOT: no match *);
    (* class with wide range / bitmap; NCLASS matches > 255 *)
    ("(*UTF)[\\x{100}-\\x{200}]", "\xc4\x80", 0, 0l);
    ("(*UTF)[a-z]", "\xc3\xa9", 0, 0l) (* wide char not in <256 class *);
    ("(*UTF)[^a]", "\xc3\xa9", 0, 0l) (* NCLASS matches wide char *);
    (* XCLASS: wide range + \p property in a class *)
    ("(*UTF)[\\p{L}]+", "ab\xce\xb1\xce\xb2!", 0, 0l);
    ("(*UTF)[\\x{100}-\\x{2000}]+", "\xc4\x80\xce\xb1\xe2\x80\x80", 0, 0l);
    (* ASCII types (non-UCP): \d \w step whole chars, wide char is not word *)
    ("(*UTF)\\w+", "ab\xc3\xa9", 0, 0l);
    ("(*UTF)\\d+", "12\xc3\xa9", 0, 0l);
    (* \h horizontal space multibyte (U+2003) *)
    ("(*UTF)\\h", "\xe2\x80\x83", 0, 0l);
    ("(*UTF)\\H", "\xe2\x80\x83", 0, 0l) (* em-space is \h, not \H: no match *);
    (* repeats over code points: greedy give-back one CHARACTER at a time *)
    ("(*UTF)\\x{100}+", "\xc4\x80\xc4\x80\xc4\x80", 0, 0l);
    ("(*UTF)\\x{100}{2,}x", "\xc4\x80\xc4\x80\xc4\x80x", 0, 0l);
    ("(*UTF).*b", "\xc3\xa9\xe2\x82\xac b", 0, 0l);
    ("(*UTF)[\\x{100}-\\x{300}]*\\x{101}", "\xc4\x80\xc4\x81", 0, 0l);
    ("(*UTF)(?i)\\x{c9}+", "\xc3\xa9\xc3\x89\xc3\xa9", 0, 0l);
    (* word boundary over multi-byte chars (non-UCP: wide char not a word char) *)
    ("(*UTF)\\bab\\b", "\xc3\xa9ab\xc3\xa9", 0, 0l);
    ("(*UTF)a\\Bb", "a\xc3\xa9b", 0, 0l) (* not adjacent -> a\Bb won't match here *);
    (* anchors with multi-byte content *)
    ("(*UTF)^\\x{100}$", "\xc4\x80", 0, 0l);
    ("(*UTF)\\x{100}\\z", "x\xc4\x80", 0, 0l);
    (* offset mid-character -> BADUTFOFFSET (both engines) *)
    ("(*UTF)a", "\xc3\xa9", 1, 0l);
    (* partial over a multi-byte tail *)
    ("(*UTF)\\x{100}\\x{101}", "\xc4\x80", 0, o_partial_soft);
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
    (* Chunk L — the caseless first_cu memchr CACHE across many bump iterations
       (both cases present, only the last start matches; the cache must find the
       SAME positions as the uncached scan). *)
    ("(?i)abz", "aBxAbyaBz", 0, 0l);
    ("(?i)abz", "AbAbAbAbAB", 0, 0l) (* no z: NM after scanning all starts *);
    ("(?i)qz", "QqQqQqQz", 0, 0l);
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
    (* ---------- Chunk M: CHAR_RUN word-compare boundary pins ----------
       A fused caseful run >= 8 code units exercises the word-at-a-time
       [char_run_eq] loop (8-byte chunks + byte tail); a mismatch/partial at
       various offsets is the differential oracle against the interpreter. The
       word compare is used ONLY where the whole run fits before end_subject
       (no partial boundary in the span); a subject that ends INSIDE the run
       must route through the per-byte [char_run_cmp_partial] with its
       SCHECK_PARTIAL placement. These pins would fail if the word path were
       wrongly used across end_subject, or if [char_run_eq] mis-compared a word
       or the tail. *)
    (* full-span match / mismatch across the 8-byte boundary (no partial) *)
    ("abcdefghij", "abcdefghij", 0, 0l) (* exact: 1 word + 2-byte tail *);
    ("abcdefghij", "abcdefghijk", 0, 0l) (* match then extra *);
    ("abcdefghij", "Zabcdefghij", 0, 0l) (* match after one bump *);
    ("abcdefghij", "abcXefghij", 0, 0l) (* mismatch in the first word (idx 3) *);
    ("abcdefghij", "abcdefghiX", 0, 0l) (* mismatch in the byte tail (idx 9) *);
    ("abcdefghijklmnop", "abcdefghijklmnop", 0, 0l) (* exact: 2 full words *);
    ("abcdefghijklmnop", "abcdefghijklXnop", 0, 0l) (* mismatch 2nd word idx 12 *);
    ("abcdefghijklmnopqr", "abcdefghijklmnopqr", 0, 0l) (* 2 words + 2 tail *);
    (* subject ends EXACTLY at the 8-byte word boundary, mid-run -> PARTIAL *)
    ("abcdefghij", "abcdefgh", 0, o_partial_soft);
    ("abcdefghij", "abcdefgh", 0, o_partial_hard);
    ("abcdefghij", "abcdefgh", 0, 0l) (* same, no partial -> NM *);
    ("\\Aabcdefghij", "abcdefgh", 0, o_partial_hard) (* anchored boundary *);
    (* subject ends in the byte tail, mid-run -> PARTIAL *)
    ("abcdefghij", "abcdefghi", 0, o_partial_soft);
    ("abcdefghij", "abcdefghi", 0, o_partial_hard);
    (* subject ends WITHIN the first word -> PARTIAL (per-byte path) *)
    ("abcdefghij", "abcdefg", 0, o_partial_soft);
    ("abcdefghij", "abc", 0, o_partial_hard);
    (* two-word run, subject ends mid-second-word -> PARTIAL *)
    ("abcdefghijklmnop", "abcdefghijklmno", 0, o_partial_hard);
    ("abcdefghijklmnop", "abcdefghijkl", 0, o_partial_soft) (* ends at 2nd bnd *);
    (* multi-byte UTF fused run crossing the 8-byte boundary, ends mid-char *)
    ("(*UTF)\\x{100}\\x{101}\\x{102}\\x{103}\\x{104}",
      "\xc4\x80\xc4\x81\xc4\x82\xc4\x83", 0, o_partial_soft);
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
    (* ---------- Chunk I2: UCP mode, properties, \X, UTF remainder ----------
       Extra code points: Α/α = CE 91 / CE B1, β = CE B2, σ/ς/Σ = CF 83 /
       CF 82 / CE A3, KELVIN U+212A = E2 84 AA, long s U+017F = C5 BF,
       Ⱥ U+023A = C8 BA, ⱥ U+2C65 = E2 B1 A5, ARABIC-INDIC ONE U+0661 = D9 A1,
       U+30FC = E3 83 BC (sc Common, scx Hiragana+Katakana), NEL = C2 85,
       LS U+2028 = E2 80 A8, PS U+2029 = E2 80 A9, combining acute = CC 81,
       regional indicators A/B U+1F1E6/U+1F1E7 = F0 9F 87 A6 / F0 9F 87 A7,
       thumbs-up U+1F44D = F0 9F 91 8D, ZWJ U+200D = E2 80 8D. *)
    (* single properties: general category, particular, script, negation *)
    ("(*UTF)\\p{L}", "\xce\xb1", 0, 0l);
    ("(*UTF)\\p{L}", "1", 0, 0l);
    ("(*UTF)\\p{Lu}", "\xce\x91", 0, 0l);
    ("(*UTF)\\p{Lu}", "\xce\xb1", 0, 0l);
    ("(*UTF)\\p{L&}", "\xce\xb1", 0, 0l);
    ("(*UTF)\\p{Nd}+x", "12\xd9\xa1x", 0, 0l);
    ("(*UTF)\\P{L}", "\xce\xb1", 0, 0l);
    ("(*UTF)\\P{L}+x", "12 x", 0, 0l);
    ("(*UTF)\\p{Greek}+a", "\xce\xb1\xce\xb2a", 0, 0l);
    ("(*UTF)\\p{Han}", "\xce\xb1", 0, 0l);
    (* script vs script-extensions on U+30FC (sc=Common, scx incl. Katakana) *)
    ("(*UTF)\\p{sc:Katakana}", "\xe3\x83\xbc", 0, 0l);
    ("(*UTF)\\p{scx:Katakana}", "\xe3\x83\xbc", 0, 0l);
    ("(*UTF)\\p{Katakana}", "\xe3\x83\xbc", 0, 0l);
    (* PCRE2 special property types: alnum / space / word / UCNC / bidi / bool *)
    ("(*UTF)\\p{Xan}+!", "a1\xce\xb1!", 0, 0l);
    ("(*UTF)\\p{Xps}+x", " \xe2\x80\x83x", 0, 0l);
    ("(*UTF)\\p{Xsp}+x", " \tx", 0, 0l);
    ("(*UTF)\\p{Xwd}+!", "a_\xce\xb1!", 0, 0l);
    ("(*UTF)\\p{Xuc}", "$", 0, 0l);
    ("(*UTF)\\p{Xuc}", "b", 0, 0l);
    ("(*UTF)\\p{bc:L}", "a", 0, 0l);
    ("(*UTF)\\p{bc:AL}", "a", 0, 0l);
    ("(*UTF)\\p{Alphabetic}", "\xce\xb1", 0, 0l);
    ("(*UTF)\\p{White_Space}", "\xe2\x80\x83", 0, 0l);
    (* PT_ANY, incl. the hoisted NOTPROP min-loop check *)
    ("\\p{Any}+", "ab", 0, 0l);
    ("\\P{Any}", "a", 0, 0l);
    ("\\P{Any}+", "a", 0, 0l);
    ("\\P{Any}*x", "x", 0, 0l);
    ("\\P{Any}+", "", 0, o_partial_soft) (* hoist precedes SCHECK_PARTIAL *);
    (* property repeats: greedy give-back / lazy extend / bounded / possessive;
       non-UTF \p works on bytes too *)
    ("(*UTF)(*NO_AUTO_POSSESS)\\p{L}*!", "ab\xce\xb1!", 0, 0l);
    ("(*UTF)(*NO_AUTO_POSSESS)\\p{L}*\\x{3b1}", "ab\xce\xb1", 0, 0l);
    ("(*UTF)\\p{L}+?x", "abx", 0, 0l);
    ("(*UTF)\\p{L}{2,3}!", "abcd!", 0, 0l);
    ("(*UTF)\\p{L}++a", "aaa", 0, 0l);
    ("\\p{L}+", "ab\xe9", 0, 0l) (* non-UTF: é (0xE9) is Ll *);
    ("\\p{Lu}+x", "AB\xc9x", 0, 0l);
    (* UCP mode: \d \w \s become properties at parse time *)
    ("(*UCP)\\w+", "ab\xe9", 0, 0l);
    ("\\w+", "ab\xe9", 0, 0l) (* ASCII contrast: stops before é *);
    ("(*UCP)\\d+x", "12\xb2x", 0, 0l) (* superscript two is No, not Nd *);
    ("(*UCP)\\s+x", "\xa0 x", 0, 0l) (* NBSP is Zs under UCP *);
    ("\\s+x", "\xa0 x", 0, 0l);
    ("(*UTF)(*UCP)\\w+!", "a\xce\xb1_1!", 0, 0l);
    ("(*UTF)(*UCP)\\d+!", "1\xd9\xa1!", 0, 0l);
    (* UCP word boundary (\b/\B), with and without UTF *)
    ("(*UCP)a\\b", "a\xe9", 0, 0l) (* é is a word char under UCP: no boundary *);
    ("a\\b", "a\xe9", 0, 0l) (* ASCII \b: boundary after 'a' *);
    ("(*UCP)\\ba", "\xe9a", 0, 0l);
    ("\\ba", "\xe9a", 0, 0l);
    ("(*UCP)a\\B\\S", "a\xe9", 0, 0l);
    ("(*UTF)(*UCP)\\b\\x{3b1}", "a \xce\xb1", 0, 0l);
    ("(*UTF)(*UCP)a\\b", "a\xce\xb1", 0, 0l);
    ("(*UTF)a\\b", "a\xce\xb1", 0, 0l) (* non-UCP contrast in UTF *);
    ("(*UTF)(*UCP)\\B\\x{3b1}", "b\xce\xb1", 0, 0l);
    (* UCP caseless without UTF: CHARI/NOTI/repeat folds via Ucd.othercase;
       othercase(0xFF) = U+0178 truncates to 0x78 in first_cu2 (the C's
       PCRE2_UCHAR cast) — parity covers the start-scan too *)
    ("(*UCP)(?i)\\xe9", "\xc9", 0, 0l);
    ("(*UCP)(?i)[^\\xe9]", "\xc9", 0, 0l);
    ("(*UCP)(?i)\\xe9+x", "\xe9\xc9\xe9x", 0, 0l);
    ("(*UCP)(?i)\\xff", "x\xff", 0, 0l);
    ("(*UCP)(?i)a\\xff", "a\xff", 0, 0l);
    (* multi-case caseless sets (PT_CLIST): k/K/KELVIN, s/S/long-s, the sigmas —
       singles, repeats, classes and backrefs *)
    ("(*UTF)(?i)k+x", "kK\xe2\x84\xaax", 0, 0l);
    ("(*UTF)(?i)\\x{212a}", "k", 0, 0l);
    ("(*UTF)(?i)s+x", "sS\xc5\xbfx", 0, 0l);
    ("(*UTF)(?i)\\x{3c3}+x", "\xcf\x83\xcf\x82\xce\xa3x", 0, 0l);
    ("(*UTF)(?i)[\\x{3c3}]+x", "\xcf\x82\xce\xa3x", 0, 0l);
    ("(*UTF)(?i)[^k]", "\xe2\x84\xaa", 0, 0l);
    ("(*UTF)(?i)(k)\\1x", "k\xe2\x84\xaax", 0, 0l);
    ("(*UTF)(?i)(\\x{212a})\\1", "\xe2\x84\xaak", 0, 0l);
    ("(*UTF)(?i)(\\x{3c3})\\1+x", "\xcf\x83\xcf\x82\xce\xa3x", 0, 0l);
    (* caseless UTF backrefs with LENGTH-CHANGING folds (Ⱥ 2 bytes / ⱥ 3
       bytes): singles, repeats, and the RM22 varied-lengths give-back *)
    ("(*UTF)(?i)(\\x{23a})\\1", "\xc8\xba\xe2\xb1\xa5", 0, 0l);
    ("(*UTF)(?i)(\\x{23a})\\1", "\xe2\xb1\xa5\xc8\xba", 0, 0l);
    ("(*UTF)(?i)(\\x{23a}x)\\1\\1", "\xc8\xbax\xe2\xb1\xa5x\xc8\xbax", 0, 0l);
    ( "(*UTF)(?i)(\\x{23a})\\1*\\x{23a}",
      "\xc8\xba\xe2\xb1\xa5\xc8\xba\xe2\xb1\xa5",
      0,
      0l );
    ("(*UTF)(?i)(\\x{23a})\\1*?\\x{2c65}", "\xc8\xba\xe2\xb1\xa5", 0, 0l);
    ("(*UTF)(?i)(k)\\1{1,3}x", "k\xe2\x84\xaakx", 0, 0l);
    ("(*UTF)(?i)(k)\\1{2}", "k\xe2\x84\xaak", 0, 0l);
    (* caseless backref under UCP without UTF (uni fold, one unit per char) *)
    ("(*UCP)(?i)(\\xe9)\\1", "\xe9\xc9", 0, 0l);
    ("(*UCP)(?i)(\\xe9)\\1+x", "\xe9\xc9\xe9x", 0, 0l);
    (* \X grapheme clusters: combining marks, CRLF, RI pairs, ZWJ joins;
       single, repeated, lazy, bounded, and the cluster-wise give-back *)
    ("(*UTF)\\X", "e\xcc\x81", 0, 0l);
    ("(*UTF)\\X\\X", "e\xcc\x81a", 0, 0l);
    ("(*UTF)\\X+", "e\xcc\x81e\xcc\x81", 0, 0l);
    ("(*UTF)(*NO_AUTO_POSSESS)\\X*e", "e\xcc\x81e", 0, 0l);
    ("(*UTF)\\X+?a", "e\xcc\x81a", 0, 0l);
    ("(*UTF)\\X{2}", "e\xcc\x81a", 0, 0l);
    ("(*UTF)\\X{3}", "e\xcc\x81a", 0, 0l);
    ("\\X+", "ab\r\n", 0, 0l) (* non-UTF \X; CRLF is one cluster *);
    ("\\X", "\r\n", 0, o_partial_soft);
    ( "(*UTF)\\X",
      "\xf0\x9f\x87\xa6\xf0\x9f\x87\xa7\xf0\x9f\x87\xa6",
      0,
      0l ) (* RI pair = one cluster, third RI starts the next *);
    ("(*UTF)\\X+", "\xf0\x9f\x87\xa6\xf0\x9f\x87\xa7\xf0\x9f\x87\xa6", 0, 0l);
    ( "(*UTF)\\X",
      "\xf0\x9f\x91\x8d\xe2\x80\x8d\xf0\x9f\x91\x8d",
      0,
      0l ) (* EP ZWJ EP joins into one cluster *);
    ("(*UTF)\\Xx", "e\xcc\x81\xcc\x81x", 0, 0l) (* stacked marks *);
    (* \R in UTF: multi-byte members NEL/LS/PS, repeats, give-back, BSR *)
    ("(*UTF)\\R", "\xc2\x85", 0, 0l);
    ("(*UTF)\\R", "\xe2\x80\xa8", 0, 0l);
    ("(*UTF)\\R", "\xe2\x80\xa9", 0, 0l);
    ("(*UTF)\\R+x", "\r\n\xc2\x85\xe2\x80\xa9x", 0, 0l);
    ("(*UTF)(*NO_AUTO_POSSESS)\\R*\\x{85}", "\r\n\xc2\x85", 0, 0l);
    ("(*UTF)\\R+?\\n", "\xe2\x80\xa8\n", 0, 0l);
    ("(*UTF)\\R{2}x", "\xc2\x85\xe2\x80\xa8x", 0, 0l);
    ("(*BSR_ANYCRLF)(*UTF)\\R", "\xc2\x85", 0, 0l);
    ("(*BSR_ANYCRLF)(*UTF)\\R+x", "\r\n\xc2\x85x", 0, 0l);
    (* (?s). and \C repeats in UTF: char-step vs code-unit-step *)
    ("(*UTF)(?s).*x", "\xce\xb1\n\xce\xb2x", 0, 0l);
    ("(*UTF)(?s).{2}$", "\xce\xb1\xce\xb2", 0, 0l);
    ("(*UTF)(*NO_AUTO_POSSESS)(?s).*\\x{3b2}", "\xce\xb1\xce\xb2", 0, 0l);
    ("(*UTF)(?s).+?x", "\xce\xb1x", 0, 0l);
    ("(*UTF)\\C\\C$", "\xce\xb1", 0, 0l) (* \C: two units of one char *);
    ("(*UTF)\\C+\\n", "\xce\xb1x\n", 0, 0l);
    ("(*UTF)\\C{2,}", "\xce\xb1x", 0, 0l);
    ("(*UTF)\\C{3}", "\xce\xb1", 0, o_partial_soft) (* \C min: NO scheck *);
    ( "(*UTF)a\\C{3}",
      "a\xce\xb1",
      0,
      o_partial_soft )
    (* the no-SCHECK \C min bound (pcre2_match.c:3041-3044) is only
       observable once something was consumed (start_used < eptr): a spurious
       SCHECK would report a partial here, the C reports NOMATCH *);
    ("(*UTF)a\\C{3}", "a\xce\xb1", 0, o_partial_hard);
    ("(*UTF)(*NO_AUTO_POSSESS)a\\C*x", "a\xce\xb1x", 0, 0l);
    (* UTF lookbehind: fixed and variable, at multi-byte boundaries *)
    ("(*UTF)(?<=\\x{3b1})b", "\xce\xb1b", 0, 0l);
    ("(*UTF)(?<=\\x{3b1})b", "ab", 0, 0l);
    ("(*UTF)(?<=a\\x{3b1})b", "a\xce\xb1b", 0, 0l);
    ("(*UTF)(?<!\\x{3b1})b", "\xce\xb1b", 0, 0l);
    ("(*UTF)(?<!\\x{3b1})b", "ab", 0, 0l);
    ("(*UTF)(?<=\\x{3b1}{2,3})x", "\xce\xb1\xce\xb1x", 0, 0l);
    ("(*UTF)(?<=\\x{3b1}{2,3})x", "\xce\xb1x", 0, 0l);
    ("(*UTF)(?<=\\d{1,2}\\x{3b1})x", "12\xce\xb1x", 0, 0l);
    ("(*UTF)(?<=.)x", "\xf0\x9f\x98\x80x", 0, 0l) (* 4-byte back-step *);
    ("(*UTF)(?<=^.)x", "\xf0\x9f\x98\x80x", 0, 0l);
    ("(*UTF)(?<=\\x{3b1})\\x{3b2}", "\xce\xb1\xce\xb2", 2, 0l)
    (* start offset: max_lookbehind back-up + check_subject floor *);
    ("(*UTF)(?<=(\\x{3b1}|\\x{3b2}\\x{3b1}))x", "\xce\xb2\xce\xb1x", 0, 0l);
    (* ---------- Chunk J: conditionals ----------
       Numbered CREF (?(1)…) both set/unset paths, single- and two-branch. *)
    ("(a)(?(1)b|c)", "ab", 0, 0l);
    ("(a)?(?(1)b|c)", "ab", 0, 0l);
    ("(a)?(?(1)b|c)", "c", 0, 0l);
    ("(a)?(?(1)b|c)", "b", 0, 0l);
    ("(a)?(?(1)b)", "ab", 0, 0l);
    ("(a)?(?(1)b)", "a", 0, 0l);
    ("(a)?(?(1)b)", "", 0, 0l);
    ("(a)?(?(1)|c)", "", 0, 0l) (* empty yes-branch *);
    ("(a)?(?(1)|c)", "c", 0, 0l);
    ("(a)?(?(1)x|)", "a", 0, 0l) (* empty no-branch *);
    (* Named DNCREF via (?J) inline DUPNAMES. *)
    ("(?J)(?:(?<A>a)|(?<A>b))(?(<A>)x|y)", "ax", 0, 0l);
    ("(?J)(?:(?<A>a)|(?<A>b))(?(<A>)x|y)", "bx", 0, 0l);
    ("(?J)(?:(?<A>a)|z)(?(<A>)x|y)", "zy", 0, 0l);
    ("(?<A>a)?(?(<A>)b|c)", "ab", 0, 0l);
    ("(?<A>a)?(?(<A>)b|c)", "c", 0, 0l);
    (* DEFINE / OP_FALSE: subroutine-less body, condition always false. *)
    ("(?(DEFINE)(?<x>abc))d", "d", 0, 0l);
    ("(?(DEFINE)(?<x>abc))d", "abc", 0, 0l);
    ("(?(VERSION>=0)y|n)", "y", 0, 0l);
    ("(?(VERSION>=99)y|n)", "n", 0, 0l);
    (* Recursion condition (?(R)…) with recursion out of subset -> always false. *)
    ("(?(R)a|b)", "a", 0, 0l);
    ("(?(R)a|b)", "b", 0, 0l);
    (* Assertion conditions, positive and negative, captures inside persist. *)
    ("(?(?=a)b|c)", "ab", 0, 0l);
    ("(?(?=a)b|c)", "c", 0, 0l);
    ("(?(?!a)b|c)", "b", 0, 0l);
    ("(?(?!a)b|c)", "ab", 0, 0l);
    ("(?(?=a)b)", "ab", 0, 0l);
    ("(?(?=a)b)", "b", 0, 0l);
    ("(?(?=(a))b\\1|c)", "aba", 0, 0l) (* pos-assert capture persists into yes *);
    ("x(?(?=(a))\\1y|z)", "xay", 0, 0l);
    ("x(?(?!(a))y|\\1z)", "xaz", 0, 0l) (* neg-assert capture persists into no *);
    ("(?(?=(a)(b))\\1\\2x|y)", "abababx", 0, 0l);
    (* Lookbehind conditions (fixed + variable). *)
    ("(?(?<=a)b|c)", "ab", 0, 0l);
    ("(?(?<=a)b|c)", "c", 0, 0l);
    ("(?(?<=a{2,3})x|y)", "aax", 0, 0l);
    ("(?(?<=a{2,3})x|y)", "ay", 0, 0l);
    (* Nested conditionals. *)
    ("(a)?(b)?(?(1)(?(2)x|y)|z)", "abx", 0, 0l);
    ("(a)?(b)?(?(1)(?(2)x|y)|z)", "ay", 0, 0l);
    ("(a)?(b)?(?(1)(?(2)x|y)|z)", "z", 0, 0l);
    (* Conditionals inside repeats and atomic groups. *)
    ("(a)?(?(1)b)*", "abbb", 0, 0l);
    ("(a)?(?(1)b)*", "bbb", 0, 0l);
    ("(a)?(?(1)b|c)*", "abcbc", 0, 0l);
    ("(a)?(?(1)b|c)*", "ccc", 0, 0l);
    ("(a)?(?(1)b)+?c", "abbc", 0, 0l);
    ("(?(?=a)b)*", "abab", 0, 0l);
    ("(?(?=a)b)+", "ab", 0, 0l);
    ("(?>(a)(?(1)b|c))", "ab", 0, 0l) (* conditional inside atomic group *);
    ("(?:(a)(?(1)b|c))++", "abab", 0, 0l) (* conditional inside possessive *);
    ("(x)(?(1)a|b){2,3}", "xaaa", 0, 0l);
    (* Possessive repeated conditional (BRAPOS/SBRAPOS wrapping COND). *)
    ("(a)?(?(1)b)*+c", "abbc", 0, 0l);
    ("(?(?=a)ab|c)*+d", "abd", 0, 0l);
    (* THEN / verb interplay at conditional branch boundaries. *)
    ("(?(?=a(*THEN)b)x|y)", "ac", 0, 0l);
    ("(?(?=a(*THEN)b)x|y)", "y", 0, 0l);
    ("(?(?=aa|a(*THEN)b)x|y)", "ac", 0, 0l);
    ("(a)(?(1)b(*THEN)c|d)", "abd", 0, 0l);
    ("(a)(?(1)b(*THEN)c|d)", "abc", 0, 0l);
    ("(?:(?(?=a)x(*THEN)y|z)|w)", "ac", 0, 0l);
    ("(a)(?(1)(*COMMIT)b|c)d", "aXd", 0, 0l);
    ("(?(?=a)(*MARK:m)b|c)", "cc", 0, 0l);
    ("(a)?(?(1)(*MARK:y)b|c)", "ab", 0, 0l);
    ("(a)?(?(1)(*MARK:y)b|c)", "c", 0, 0l);
    (* ---------- Chunk K: callouts (no-ops), script runs, repeated atomic
       groups ---------- *)
    (* Callouts are pure no-ops (do_callout is 0: no callout function surface). *)
    ("a(?C1)b(?C2)c", "abc", 0, 0l);
    ("a(?C\"x\")b", "ab", 0, 0l) (* string-arg callout *);
    ("(a)(?C1)(?(1)b|c)", "ab", 0, 0l) (* callout before a condition *);
    ("(?(?C1)(?=a)b|c)", "ab", 0, 0l) (* manual callout before condition assert *);
    ("(?(?C1)(?=a)b|c)", "c", 0, 0l);
    (* Script runs (the sr / script_run verb): non-atomic, ket span check. *)
    ("(*sr:\\d+)x", "123x", 0, 0l);
    ("(*sr:.*(*ACCEPT)ZZ)", "ab", 0, 0l) (* ( *ACCEPT) in a script run body *);
    ("^(*sr:A|)*BCD", "AABCD", 0, 0l) (* repeated script run, empty-loop break *);
    ("^(*sr:A|)*BCD", "BCD", 0, 0l);
    ("(*asr:\\d+)", "42", 0, 0l) (* atomic script run = OP_SCRIPT_RUN + OP_ONCE *);
    (* Repeated atomic groups (Once ... KetRmax / KetRmin): per-iteration atomic
       commit + give-back with the KIND_ONCE snapshot. *)
    ("(?>a|ab)+c", "abc", 0, 0l);
    ("(?>a|ab)+c", "aabc", 0, 0l);
    ("(?>a)+", "aaa", 0, 0l);
    ("(?>a)*", "aaa", 0, 0l);
    ("(?>a?)*b", "aaab", 0, 0l);
    ("(?>a|ab)*?c", "abc", 0, 0l);
    ("(?>(a)|(b))+", "ab", 0, 0l) (* captures inside a repeated atomic group *);
    ("x(?>ab|a)+y", "xabay", 0, 0l);
    (* Repeated assertions (BRAZERO + non-repeating assertion). *)
    ("(?=a)*ab", "ab", 0, 0l);
    ("(?=abc)+abc", "abc", 0, 0l);
    ("(?!x)*ab", "ab", 0, 0l);
    ("(?<=x)+y", "xy", 1, 0l);
    ("(?>ab){2,4}c", "ababc", 0, 0l);
    (* ---------- Chunk K2: \K, ( *ACCEPT)-in-assertion, ( *THEN)+NA,
       recursion into a possessive capture ---------- *)
    (* \K (OP_SET_SOM): the reported start moves to the \K position (ovector[0]),
       while pcre2_get_startchar stays at the attempt start. *)
    ("(foo)\\Kbar", "foobar", 0, 0l);
    ("abc\\K123", "abc123", 0, 0l);
    ("a\\K", "a", 0, 0l) (* empty match after \K *);
    ("\\++\\KZ|\\d+X|9+Y", "++++Z", 0, 0l);
    ("(a|b)*\\Kc", "abac", 0, 0l) (* \K after an alternation loop (backtrack) *);
    ("(?:a\\Kb|x)c", "xc", 0, 0l) (* \K in a failed branch must be undone *);
    (* \K inside committing constructs — the boundary snapshot reverts start_match
       on backtrack-past. *)
    ("(?>a\\Kb)z|(ab)", "ab", 0, 0l) (* \K in an atomic group, then backtrack *);
    ("(?:a\\Kb)*+c", "abababc", 0, 0l) (* \K in a possessive group *);
    (* \K in a positive lookahead ((?=a\Kb)ab) needs PCRE2_EXTRA_ALLOW_LOOKAROUND_
       BSK (else compile error 199); covered by conformance testinput2:1786. *)
    ("a\\K.(?0)*", "abcd", 0, 0l) (* \K with recursion *);
    (* ( *ACCEPT) inside assertions. *)
    ("^(?=a(*ACCEPT)b)", "ab", 0, 0l) (* positive lookahead *);
    ("(?=a(*ACCEPT)bc)axyz", "axyz", 0, 0l);
    ("(?<=a(*ACCEPT)b)c", "abc", 2, 0l) (* positive lookbehind *);
    ("(?<=(a(*ACCEPT)b))c", "abc", 2, 0l);
    ("(?(?=(a(*ACCEPT)z))a)", "az", 0, 0l) (* assertion condition (positive) *);
    ("^(?(?=(a)(*ACCEPT))abc|def)", "abc", 0, 0l);
    ("^(?(?!(a)(*ACCEPT))def|abc)", "abc", 0, 0l) (* negative cond *);
    ("(*napla:a|(.)(*ACCEPT)zz)\\1..", "bbcd", 0, 0l) (* ACCEPT in a NA lookahead *);
    ("(*napla:a(*ACCEPT)zz|(.))\\1..", "aabb", 0, 0l);
    (* ( *THEN) with a non-atomic positive assertion (NA boundary contains it). *)
    ("(*napla:a(*THEN)b)c", "ac", 0, 0l);
    ("(*napla:a(*THEN)b|d)c", "adc", 0, 0l);
    ("(*napla:a)b(*THEN)c|x", "abx", 0, 0l);
    ("(?*a(*THEN)b|c)d", "cd", 0, 0l);
    (* Recursion INTO a possessive capture (OP_CBRAPOS/OP_SCBRAPOS). *)
    ("(a)++(?1)b", "aaab", 0, 0l);
    ("(a)*+(?1)", "aaa", 0, 0l);
    ("(?:(a(*PRUNE)b)){0}(?:(?1)|ac)", "ac", 0, 0l) (* recursion into a {0} group *);
    ("(?1)(?:(b)){0}", "b", 0, 0l);
    ("^(X(*THEN)Y|AB){0}(?1)", "AB", 0, 0l);
    ("(a(*MARK:m)(*ACCEPT)){0}(?1)", "a", 0, 0l);
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

(* ---------- 5a1. Chunk L — PCRE2_FIRSTLINE parity ----------

   FIRSTLINE (a COMPILE option, 0x100) constrains the match to START at or
   before the first newline following the start offset (it MAY continue over
   the newline). The runner mirrors the interpreter exactly: [bump_top] shortens
   end_subject to the first newline for the start-of-match scans
   (pcre2_match.c:7164-7186), and [bump_bottom] stops when an attempt STARTING
   at a newline failed (7584-7588). firstline is FALSE when anchored (6942), so
   \A / ( *ANCHORED) disables it. These cases cross the newline conventions ×
   first_cu / startline / start_bits / no-scan × the "continue over the newline"
   (DOTALL) × anchored interplay; the interpreter is the oracle (fast == interp
   is the assertion; the interpreter's FIRSTLINE is already conformance-tested). *)
let firstline_opt = 0x00000100l (* PCRE2_FIRSTLINE compile option *)
let nl_cr = Pcre2_engine.Options.newline_cr
let nl_lf = Pcre2_engine.Options.newline_lf
let nl_crlf = Pcre2_engine.Options.newline_crlf
let nl_any = Pcre2_engine.Options.newline_any
let nl_anycrlf = Pcre2_engine.Options.newline_anycrlf

let firstline_parity_tests =
  let run_both ~newline ~copts pat subj off opts =
    match
      (F.compile_ctx ~newline pat copts, E.compile_ctx ~newline pat copts)
    with
    | Ok fre, Ok ere ->
        ( norm (F.exec_full fre subj off opts),
          norm (E.exec_full ere subj off opts) )
    | Error _, Error _ -> ("CE", "CE") (* both reject: agree *)
    | _ -> Alcotest.failf "compile mismatch for %S" pat
  in
  (* (label, pattern, subject, offset, newline, copts, match_opts) *)
  let cases : (string * string * string * int * int * int32 * int32) list =
    [
      (* has_first_cu, default (LF) convention *)
      ("fc match line1", "a", "xa\nya", 0, nl_lf, firstline_opt, 0l);
      ("fc line2 -> NM", "a", "x\nya", 0, nl_lf, firstline_opt, 0l);
      ("fc newline@start", "a", "\na", 0, nl_lf, firstline_opt, 0l);
      ("fc no newline", "a", "xya", 0, nl_lf, firstline_opt, 0l);
      ("fc at first nl pos", "a", "ab\na", 0, nl_lf, firstline_opt, 0l);
      (* offset into the subject: firstline is relative to the start offset *)
      ("fc offset", "a", "z\nxa\nb", 2, nl_lf, firstline_opt, 0l);
      (* match STARTS on line1 but CONTINUES over the newline (DOTALL) *)
      ("continue over nl", "(?s)a.b", "a\nb", 0, nl_lf, firstline_opt, 0l);
      ("continue, line2 start -> NM", "(?s)a.b", "x\na\nb", 0, nl_lf,
        firstline_opt, 0l );
      (* caseless first_cu + the memchr cache under firstline shortening *)
      ("caseless fc line1", "(?i)a", "XA\nB", 0, nl_lf, firstline_opt, 0l);
      ("caseless fc line2 -> NM", "(?i)a", "X\nA", 0, nl_lf, firstline_opt, 0l);
      (* start_bits (class, no unique first cu) *)
      ("start_bits line1", "[ab]", "xb\ny", 0, nl_lf, firstline_opt, 0l);
      ("start_bits line2 -> NM", "[ab]", "x\nb", 0, nl_lf, firstline_opt, 0l);
      (* startline (multiline ^) + firstline *)
      ("startline ^ line1", "(?m)^a", "a\nb", 0, nl_lf, firstline_opt, 0l);
      ("startline ^ line2 -> NM", "(?m)^x", "a\nx", 0, nl_lf, firstline_opt, 0l);
      (* no-scan plain path (NO_START_OPTIMIZE forces run_attempt directly) *)
      ("no_start_opt", "(*NO_START_OPT)a", "x\nya", 0, nl_lf, firstline_opt, 0l);
      ("no_start_opt line1", "(*NO_START_OPT)a", "xa\ny", 0, nl_lf,
        firstline_opt, 0l );
      (* CR convention *)
      ("cr line1", "a", "xa\ry", 0, nl_cr, firstline_opt, 0l);
      ("cr line2 -> NM", "a", "x\rya", 0, nl_cr, firstline_opt, 0l);
      (* CRLF convention (the CR-then-LF bump interplay) *)
      ("crlf line1", "a", "xa\r\ny", 0, nl_crlf, firstline_opt, 0l);
      ("crlf line2 -> NM", "a", "x\r\nya", 0, nl_crlf, firstline_opt, 0l);
      ("crlf class line2 -> NM", "[ab]", "x\r\nb", 0, nl_crlf, firstline_opt, 0l);
      (* ANY convention (the variable is_newline path: scan_firstline_var) *)
      ("any cr line2 -> NM", "a", "x\rya", 0, nl_any, firstline_opt, 0l);
      ("any lf line1", "a", "xa\ny", 0, nl_any, firstline_opt, 0l);
      ("any startline", "(?m)^b", "a\rb", 0, nl_any, firstline_opt, 0l);
      (* ANYCRLF convention *)
      ("anycrlf crlf line2 -> NM", "a", "x\r\nya", 0, nl_anycrlf, firstline_opt,
        0l );
      ("anycrlf lf line1", "a", "za\nb", 0, nl_anycrlf, firstline_opt, 0l);
      (* anchored disables firstline (firstline = !anchored && FIRSTLINE) *)
      ("anchored \\A", "\\Aab", "ab\ncd", 0, nl_lf, firstline_opt, 0l);
      ("match-time ANCHORED", "a", "x\nya", 0, nl_lf, firstline_opt,
        0x80000000l );
      (* partial matching keeps every attempt (minlength/req_cu disabled), but
         firstline still stops the bump at a failed newline start *)
      ("partial soft", "abc", "ab\nx", 0, nl_lf, firstline_opt, 0x10l);
      ("partial line2 -> NM", "abc", "x\nabc", 0, nl_lf, firstline_opt, 0x10l);
      (* NOTBOL interplay *)
      ("notbol", "^a", "a\nb", 0, nl_lf, firstline_opt, 0x1l);
      (* firstline + req_cu (last code unit) tail optimization *)
      ("reqcu line1", "a.*z", "az\nq", 0, nl_lf, firstline_opt, 0l);
      ("reqcu line2 -> NM", "a.*z", "q\naz", 0, nl_lf, firstline_opt, 0l);
      (* firstline is TICK-NEUTRAL (the runner attempts the interpreter's exact
         position set, §5.1), so ( *LIMIT_MATCH=N) trips at the SAME N in both
         engines. A ticking alternation prefix over several failing first-line
         starts exercises the per-attempt -47 point under firstline. *)
      ("limit_match 1", "(*LIMIT_MATCH=1)a(b|c)d", "xaXdaXe\nz", 0, nl_lf,
        firstline_opt, 0l );
      ("limit_match 2", "(*LIMIT_MATCH=2)a(b|c)d", "xaXdaXe\nz", 0, nl_lf,
        firstline_opt, 0l );
      ("limit_match 3", "(*LIMIT_MATCH=3)a(b|c)d", "xaXdaXe\nz", 0, nl_lf,
        firstline_opt, 0l );
      ("limit_match 6", "(*LIMIT_MATCH=6)a(b|c)d", "xaXdaXe\nz", 0, nl_lf,
        firstline_opt, 0l );
    ]
  in
  List.map
    (fun (label, pat, subj, off, nl, copts, opts) ->
      Alcotest.test_case
        (Printf.sprintf "firstline %s: %S %S off=%d nl=%d" label pat subj off nl)
        `Quick
        (fun () ->
          let f, e = run_both ~newline:nl ~copts pat subj off opts in
          Alcotest.(check string)
            (Printf.sprintf "fast vs interp for %S/%S (nl=%d)" pat subj nl)
            e f))
    cases

(* ---------- 5a2. Chunk L — zero-Unsupported invariant ----------

   After chunk L the fast IR compiler declines NOTHING in subset: [Unsupported]
   is unreachable for every in-scope pattern (fast-design.md §5/§6). The
   conformance run (--driver=fast) is the full witness — 0 [fast:] skips across
   all testinput files — but this white-box test pins the invariant directly by
   compiling a sweep over every construct family that was declined in an earlier
   chunk (B..K2) AND PCRE2_FIRSTLINE (chunk L, compiled with the option), and
   asserting each yields [Ok] (never [Unsupported]). Re-introducing any decline
   fails here immediately, without waiting for the conformance diff. *)
let zero_unsupported_tests =
  let must_compile ?(copts = 0l) ?(newline = 0) pat =
    match F.compile_ctx ~newline pat copts with
    | Ok _ -> ()
    | Error (F.Unsupported r) ->
        Alcotest.failf "pattern %S unexpectedly Unsupported: %s" pat r
    | Error (F.Compile_error { errcode; erroroffset }) ->
        Alcotest.failf "pattern %S failed to compile: error %d at offset %d" pat
          errcode erroroffset
  in
  (* One construct per previously-declined family (labels track the chunk that
     lowered it). The point is coverage of every family, not exhaustiveness. *)
  let construct_patterns =
    [
      (* D/D2 — captures, repeats, groups, multiline anchors *)
      "(a)(b)(c)";
      "(a)+";
      "(?:ab)*c";
      "(?m)^a$";
      (* E — types, classes, their repeats *)
      "\\d+\\w*[a-z]{2,4}";
      "[^a]*b";
      (* F — backreferences *)
      "(a)\\1";
      "(?<n>a)(?<m>b)\\k<n>";
      "(a)\\1{2,4}";
      (* G — lookaround, atomic, possessive *)
      "a(?=b)c";
      "a(?!b)";
      "(?<=ab)c";
      "(?>a|b)c";
      "(?:ab)++c";
      "(a)*+";
      (* H — verbs *)
      "(*MARK:x)a(*PRUNE)b";
      "a(*COMMIT)b|c";
      "a(*THEN)b|c";
      "a(*SKIP)b|c";
      "a(*ACCEPT)b";
      (* I/I2 — UTF, XCLASS, \p, \X, UCP *)
      "[\\p{L}]+";
      "(*UTF)caf\\x{e9}";
      "\\p{Lu}+";
      "\\X{2,}";
      "(*UCP)\\bx\\B";
      (* J — conditionals *)
      "(a)(?(1)b|c)";
      "(?(?=a)b|c)";
      "(?(DEFINE)(?<x>y))z";
      (* K — script runs, callouts, repeated atomic *)
      "(*sr:\\d+)";
      "a(?C1)b";
      "(?>a)+";
      (* K1b/K2 — recursion (all forms), \K, ( *ACCEPT)/( *THEN) edge cases *)
      "(?R)?z";
      "(a(?1)?b)";
      "(?(R)a|b)";
      "\\((?:[^()]|(?R))*\\)";
      "(a)++(?1)b";
      "a\\Kb";
      "(?=a(*ACCEPT))a" (* ( *ACCEPT) inside a positive assertion (K2) *);
      "(*napla:a(*THEN)b|c)d";
    ]
  in
  let construct_tests =
    List.map
      (fun pat ->
        Alcotest.test_case
          (Printf.sprintf "compiles (no Unsupported): %S" pat)
          `Quick
          (fun () -> must_compile pat))
      construct_patterns
  in
  (* L — PCRE2_FIRSTLINE itself, across the newline conventions, over a mix of
     patterns that hit first_cu / startline / start_bits / no-scan. *)
  let firstline_compile_test =
    Alcotest.test_case "compiles with PCRE2_FIRSTLINE (all conventions)" `Quick
      (fun () ->
        let pats =
          [
            "a"; "(?i)a"; "[ab]"; "(?m)^a"; "(?s)a.b"; "(*NO_START_OPT)a";
            "abc"; "\\d+"; "(a)(b)"; "a(?=b)c"; "(?R)?z";
          ]
        in
        List.iter
          (fun pat ->
            List.iter
              (fun nl -> must_compile ~copts:firstline_opt ~newline:nl pat)
              [ 0; nl_cr; nl_lf; nl_crlf; nl_any; nl_anycrlf ])
          pats)
  in
  construct_tests @ [ firstline_compile_test ]

(* ---------- 5b. K2 \K-in-assertion ( *ACCEPT) witnesses ----------

   The positive-assertion ( *ACCEPT) commit must REVERT mb.start_match to the
   assertion entry: the C's RM3 MATCH_ACCEPT memcpy covers ONLY ovector +
   offset_top + mark from the accept frame (pcre2_match.c:5522-5526) and
   execution continues in the assertion ENTRY frame — a \K terminated by
   ( *ACCEPT) does NOT escape a positive assertion (fidelity-review witness:
   without the revert, /(?=a\K( *ACCEPT))a/ on "aa" gave fast [1,1] vs interp
   [0,1]). The NORMAL success path is asymmetric BY DESIGN (the ket `break`
   :5534-5535 continues in the matching branch's frame): \K DOES escape there —
   pinned too so the fix is never "generalized" to t_assert_end. \K needs
   PCRE2_EXTRA_ALLOW_LOOKAROUND_BSK (0x40) lexically inside a lookaround; the
   recursion variant (\K in a {0}-defined group called by (?1) from inside the
   assertion) needs NO option (the 199 check is lexical). *)
let k2_bsk_accept_tests =
  let bsk = Pcre2_engine.Options.extra_allow_lookaround_bsk in
  let run_both ?(extra = 0) pat subj =
    match (F.compile_ctx ~extra pat 0l, E.compile_ctx ~extra pat 0l) with
    | Ok fre, Ok ere ->
        (norm (F.exec_full fre subj 0 0l), norm (E.exec_full ere subj 0 0l))
    | _ -> Alcotest.failf "compile failed for %S" pat
  in
  [
    Alcotest.test_case "\\K + (*ACCEPT) in assertions (fast == interp)" `Quick
      (fun () ->
        List.iter
          (fun (extra, pat, subj, expect_interp) ->
            let f, e = run_both ~extra pat subj in
            Alcotest.(check string)
              (Printf.sprintf "interp result for %S/%S" pat subj)
              expect_interp e;
            Alcotest.(check string)
              (Printf.sprintf "fast == interp for %S/%S" pat subj)
              e f)
          [
            (* ACCEPT path: \K reverts (the blocking-finding witness). *)
            (bsk, "(?=a\\K(*ACCEPT))a", "aa", "M@0[0,1]");
            (* recursion variant, NO option: \K executes in a group called by
               (?1) from inside the assertion, ACCEPT after — still reverts. *)
            (0, "(?:(a\\K)){0}(?=(?1)(*ACCEPT))a.", "ab", "M@0[0,2]");
            (* NORMAL success path (no ACCEPT): \K escapes — the asymmetry. *)
            (bsk, "(?=a\\Kb)ab", "ab", "M@0[1,2]");
            (* negative assertion: ACCEPT fails it; \K reverts with the
               snapshot (the assertion succeeds on "bb" via no match of "a"). *)
            (bsk, "(?!a\\K(*ACCEPT)).", "bb", "M@0[0,1]");
            (* condition assertion: ACCEPT = condition TRUE; \K reverts (RM5
               copies only ovector+offset_top). *)
            (bsk, "(?(?=a\\K(*ACCEPT))ab)", "ab", "M@0[0,2]");
          ])
  ]

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
    (* Chunk I: XCLASS-repeat tick parity. The XCLASS maxbt RMATCHes FIRST and
       gives back one CHARACTER per tick down to and INCLUDING the floor
       (pcre2_match.c:2278-2288, RM101 — exactly the CLASS RM24/:2143 shape,
       NOT the char/type floor-in-place one), so the two engines must trip -47
       at the SAME N. Every subject below forces the maximizing scan to give
       back all the way to the floor (the continuation fails at every
       position), pinning the floor tick; UTF entries give back over
       multi-byte characters (BACKCHAR). *)
    Alcotest.test_case "LIMIT_MATCH sweep, XCLASS repeat (fast == interp)"
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
            (* Entry design notes. (1) \A pins ONE attempt, so the divergent
               tick is the BINDING one — match_call_count resets per attempt
               (pcre2_match.c:7506), so an unanchored pattern's earlier
               higher-tick attempts would mask a last-attempt divergence.
               (2) Every subject keeps the req_cu ('9', or the 0x80 tail of a
               multi-byte char) present — otherwise the tail_opts req_cu scan
               skips the attempt entirely (zero ticks in BOTH engines) and the
               sweep pins nothing. (3) ( *NO_AUTO_POSSESS) keeps the greedy
               repeats true maximizers — the follower is outside the class, so
               auto-possession would otherwise remove the give-back path
               entirely (a possessive repeat never backtracks). *)
            (* floor-exact bounded repeat: the min loop eats everything, the
               greedy scan adds nothing (pmax == floor), and the floor RMATCH
               itself — the C's RM101-first tick — matches '9'. A
               floor-in-place mislowering completes one N earlier. *)
            ("(*NO_AUTO_POSSESS)\\A[\\p{L}]{2,5}9", "aa9");
            ("(*UTF)(*NO_AUTO_POSSESS)\\A[\\p{L}]{2,5}9", "\xce\xb1\xce\xb29");
            (* pmax == floor == 0: the scan matches nothing, '9' at the floor *)
            ("(*NO_AUTO_POSSESS)\\A[\\p{L}]*9", "9aaa");
            (* greedy give-back all the way DOWN TO the floor (each give-back
               AND the floor try one tick, then NOMATCH) — pins the
               backtrack_rep_max tag-58 floor tick *)
            ("(*NO_AUTO_POSSESS)\\A[\\p{L}]*9", "aaa!9");
            ( "(*UTF)(*NO_AUTO_POSSESS)\\A[\\x{100}-\\x{200}]*\\x{300}",
              "\xc4\x80\xc4\x80!\xcc\x80" );
            (* UTF give-back over 2-byte chars, matching mid-way (BACKCHAR) *)
            ( "(*UTF)(*NO_AUTO_POSSESS)\\A[\\x{100}-\\x{200}]*\\x{101}",
              "\xc4\x80\xc4\x80\xc4\x81" );
            (* lazy XCLASS repeat: extend one char per tick (RM100) *)
            ("(*UTF)\\A[\\p{L}]*?9", "\xce\xb1\xce\xb19");
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
            (* Chunk K: repeated atomic group (Once ... KetRmax): the
               per-iteration RM7 loop-back + grouploop RM2 branch tick, with the
               KIND_ONCE snapshot restore on greedy give-back. *)
            ("(?>a|ab)+c", "ababab"); (* atomic commits per iteration, then fail *)
            ("(?>a|ab)+c", "abababc"); (* ... then succeed *)
            ("(?>a)+b", "aaaab"); (* greedy give-back over atomic iterations *)
            ("(?>a|b)*c", "abab"); (* lazy/greedy repeated atomic, no match *)
            ("(?>a?)*b", "aaab"); (* empty-iteration loop break *)
            ("(*sr:a|b)+c", "abab"); (* repeated script run tick parity *)
          ]);
    (* Chunk I2: property / \X / \R-in-UTF / \C / UCP / caseless-UTF-ref tick
       parity. Property repeats take the char/type maxbt shape (floor tried IN
       PLACE, no tick — pcre2_match.c:4387-4398 RM222); \X repeats give back
       one CLUSTER per ticked RMATCH with the floor in place (4426-4437
       RM220); \R gives back one CHARACTER (with the mid-CRLF skip) per RM202
       tick; a caseless UTF ref repeat with differing copy lengths re-scans
       per RM22 tick. Same design rules as the XCLASS sweep above: \A pins ONE
       attempt so the divergent tick is binding, a req_cu ('9' / a multi-byte
       tail) keeps tail_opts from skipping the attempt, and
       ( *NO_AUTO_POSSESS) keeps the greedy repeats true maximizers. *)
    Alcotest.test_case "LIMIT_MATCH sweep, I2 prop/extuni/UTF (fast == interp)"
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
            (* prop greedy give-back down to the floor (floor in place) *)
            ("(*NO_AUTO_POSSESS)\\A\\p{L}*9", "abc!9");
            ("(*UTF)(*NO_AUTO_POSSESS)\\A\\p{L}*9", "\xce\xb1\xce\xb2!9");
            (* prop floor-exact bounded repeat *)
            ("(*NO_AUTO_POSSESS)\\A\\p{L}{2,5}9", "ab9");
            (* prop lazy extend *)
            ("(*UTF)\\A\\p{L}*?9", "\xce\xb1\xce\xb29");
            (* PT_ANY NOTPROP hoist (no per-char ticks before NOMATCH) *)
            ("\\A\\P{Any}{1,3}9", "ab9");
            (* \X cluster give-back (each give-back a tick, floor in place) *)
            ("(*UTF)(*NO_AUTO_POSSESS)\\A\\X*9", "e\xcc\x81e\xcc\x81!9");
            ("(*NO_AUTO_POSSESS)\\A\\X*9", "ab!9");
            (* \X lazy extend (RM218) *)
            ("(*UTF)\\A\\X+?9", "e\xcc\x81e9");
            (* \X bounded give-back exhausting to the FLOOR (\X matches any
               cluster, so only a bounded lmin > 0 makes the floor reachable
               with the continuation failing there — pins the char/type
               floor-IN-PLACE shape against the class floor-tick one) *)
            ("(*NO_AUTO_POSSESS)\\A\\X{2,4}9", "abcd!9");
            ( "(*UTF)(*NO_AUTO_POSSESS)\\A\\X{2,4}9",
              "e\xcc\x81e\xcc\x81e\xcc\x81!9" );
            (* \R in UTF: greedy give-back over multi-byte members + CRLF *)
            ("(*UTF)(*NO_AUTO_POSSESS)\\A\\R*9", "\r\n\xc2\x85!9");
            ("(*UTF)\\A\\R+?9", "\xe2\x80\xa8\n9");
            (* (?s). in UTF: char-step greedy give-back *)
            ("(*UTF)(*NO_AUTO_POSSESS)\\A(?s).*9", "\xce\xb1\xce\xb2!9");
            (* \C in UTF: byte-bulk greedy, char-wise give-back *)
            ("(*UTF)(*NO_AUTO_POSSESS)\\A\\C{2,4}9", "\xce\xb1x9");
            (* UCP \w = property repeat; UCP \b transitions *)
            ("(*UCP)(*NO_AUTO_POSSESS)\\A\\w*9", "ab\xe9!9");
            ("(*UCP)\\A.\\b.", "a!b");
            (* UTF variable lookbehind: one RM37 tick per back-length *)
            ("(*UTF)\\A..(?<=\\x{3b1}{1,3})x", "\xce\xb1\xce\xb1x");
            (* caseless UTF backref repeat, samelengths (RM21 give-back) *)
            ("(*UTF)(?i)\\A(k)\\1*9", "kkk!9");
            (* caseless UTF backref repeat, DIFFERING lengths (RM22 rescan) *)
            ( "(*UTF)(?i)\\A(\\x{23a})\\1*9",
              "\xc8\xba\xe2\xb1\xa5\xc8\xba!9" );
            ("(*UTF)(?i)\\A(\\x{23a})\\1*?9", "\xc8\xba\xe2\xb1\xa59");
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
    (* Chunk J: conditional tick parity. The tick sites are the OP_SCOND descend
       (RM35 — one tick per iteration of a repeated might-be-empty conditional),
       the assertion-condition branch RMATCHes (RM5 — one tick per assertion
       branch tried), and the repeating ket (RM7). The two engines must trip -47
       at the SAME N. Same discipline as the I2 sweep above: \A pins ONE attempt
       so the divergent tick is binding, a req_cu tail ('9') keeps the attempt
       from being skipped, and ( *NO_AUTO_POSSESS) keeps the greedy repeats true
       maximizers (so the ket give-back / descend ticks fire). *)
    Alcotest.test_case "LIMIT_MATCH sweep, conditionals (fast == interp)" `Quick
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
            (* SCOND greedy: RM35 descend + RM7 ket per iteration (pins the
               t_scond_descend tick — dropping it trips -47 one N earlier). *)
            ("(*NO_AUTO_POSSESS)\\A(a)?(?(1)b)*9", "abbb9");
            ("(*NO_AUTO_POSSESS)\\A(a)?(?(1)b)*9", "9");
            ("(*NO_AUTO_POSSESS)\\A(a)?(?(1)b)+9", "abbb9");
            (* SCOND lazy: RM35 descend + RM6 ket extend per iteration. *)
            ("(*NO_AUTO_POSSESS)\\A(a)?(?(1)b)*?9", "abbb9");
            (* SCOND, empty could-match two-branch (empty yes / empty no). *)
            ("(*NO_AUTO_POSSESS)\\A(a)?(?(1)|c)*9", "cc9");
            ("(*NO_AUTO_POSSESS)\\A(a)?(?(1)b|)*9", "abb9");
            (* Repeated two-branch OP_COND (no descend: RM7 ket only). *)
            ("(*NO_AUTO_POSSESS)\\A(a)(?(1)b|c)*9", "abbb9");
            (* Bounded repeated conditional (each unrolled copy ticks). *)
            ("(*NO_AUTO_POSSESS)\\A(a)(?(1)b|c){2,4}9", "abbb9");
            (* Assertion-condition SCOND: RM5 (assert branch) + RM35 + RM7. *)
            ("(*NO_AUTO_POSSESS)\\A(?(?=a)a)*9", "aaa9");
            ("(*NO_AUTO_POSSESS)\\A(?(?=a)a)+9", "aaa9");
            (* Assertion condition with a multi-branch assertion (RM5 per
               branch tried before the condition is decided). *)
            ("\\A(?(?=aa|a)ax|bx)9", "ac9");
            ("\\A(?(?!x|y|z)a|b)9", "a9");
            (* Conditional inside a greedy repeat that backtracks. *)
            ("(*NO_AUTO_POSSESS)\\A(?:(a)?(?(1)b|c))*9", "abcbc9");
          ]);
    (* Chunk K2: \K, ( *ACCEPT)-in-assertion, ( *THEN)+NA, recursion into a
       possessive capture. \K / ( *ACCEPT) commits add NO ticks (the C's SET_SOM /
       MATCH_ACCEPT propagation dispatch in-frame); the NA-assertion THEN retries
       its live branches via resume_alt (the RM5 next-branch RMATCH tick); a
       recursion into a possessive capture ticks RM11 per branch. The two engines
       must trip -47 at the SAME N — sweep across the boundary. *)
    Alcotest.test_case "LIMIT_MATCH sweep, K2 (fast == interp)" `Quick (fun () ->
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
            ("(?:a\\Kb|x)+c", "abababc"); (* \K in a repeated group + backtrack *)
            ("(a|b)*\\Kz", "ababz");
            ("(?>a\\Kb)+z|(ab)+", "abab"); (* \K in a repeated atomic group *)
            ("^(?=a(*ACCEPT)b)", "ab"); (* ( *ACCEPT) in a positive lookahead *)
            ("(?:(?=a(*ACCEPT))|b)+c", "aac");
            ("(*napla:a(*THEN)b|x)+y", "axxy"); (* ( *THEN) in a NA lookahead *)
            ("(*napla:a|(.)(*ACCEPT)z)+\\1", "bb"); (* ( *ACCEPT) in a NA lookahead *)
            ("(a)++(?1)b", "aaab"); (* recursion into a possessive capture *)
            ("(?:(a(*PRUNE)b)){0}(?:(?1)|ac)+z", "acacz"); (* recursion into {0} *)
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
  dnref_golden_test :: dncref_golden_test
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

(* ---------- 6c. MATCH_INVALID_UTF fragment parity vs the interpreter ----------

   PCRE2_MATCH_INVALID_UTF (0x04000000, compile option — no inline verb
   exists) turns on the fragmented-matching driver (pcre2_match.c:7650-7699,
   chunk I2): the subject is split at invalid UTF-8, each fragment matched
   with per-fragment NOTBOL/NOTEOL, \z/bumpalong against the true end, and a
   partial returned only from the final fragment. Each case compares
   exec_full (rc class + ovector + startchar) against the interpreter, with
   match options for the partial/anchored interplay. *)
let miu = 0x04080000l (* PCRE2_UTF lor PCRE2_MATCH_INVALID_UTF *)

let invalid_utf_cases : (string * string * int * int32) list =
  [
    (* fully valid subject: the plain path *)
    ("ab", "ab", 0, 0l);
    (* bad lead byte before/inside/after the match *)
    ("ab", "\xffab", 0, 0l);
    ("ab", "a\xffab", 0, 0l);
    ("ab", "ab\xff", 0, 0l);
    ("ab", "a\xff\xffab\xff", 0, 0l);
    ("xyz", "\xffab\xffcd", 0, 0l);
    (* truncated character at the end = a final invalid fragment boundary *)
    ("ab", "ab\xce", 0, 0l);
    ("ab$", "ab\xce", 0, 0l);
    ("ab\\z", "ab\xce", 0, 0l);
    (* continuation-byte garbage start: entry check skips bad units *)
    ("ab", "\x80\x80ab", 0, 0l);
    ("^ab", "\x80ab", 0, 0l) (* start skipped, ^ anchors the true start *);
    (* per-fragment NOTBOL: a later fragment never matches ^ *)
    ("^b", "a\xffb", 0, 0l);
    ("(?m)^b", "a\xffb", 0, 0l);
    ("(?m)^b", "a\n\xffb", 0, 0l);
    (* per-fragment NOTEOL: $ fails at a non-final fragment end *)
    ("ab$", "ab\xffcd", 0, 0l);
    ("ab$", "\xffab", 0, 0l);
    ("(?m)b$", "ab\xffcd", 0, 0l);
    (* \z tests the TRUE end across fragments *)
    ("ab\\z", "ab\xffab", 0, 0l);
    ("ab\\Z", "ab\xffab", 0, 0l);
    (* multi-byte chars inside fragments *)
    ("\\x{3b1}+x", "\xff\xce\xb1\xce\xb1x", 0, 0l);
    ("a.b", "\xffa\xce\xb1b", 0, 0l);
    (* start offset into / past a bad unit *)
    ("b", "\xffab", 2, 0l);
    ("b", "a\xffb", 1, 0l);
    (* partial matching: only the FINAL fragment may report a partial *)
    ("abcd", "\xffab", 0, o_partial_hard);
    ("abcd", "\xffab", 0, o_partial_soft);
    ("abcd", "ab\xffab", 0, o_partial_soft);
    ("abcd", "ab\xffab", 0, o_partial_hard);
    ("abcd", "\xffabc", 0, o_partial_soft);
    ("ab", "ab\xffa", 0, o_partial_soft) (* full match beats a partial *);
    (* anchored: one attempt per fragment start *)
    ("ab", "\xffab", 0, o_anchored);
    ("ab", "\xffxab", 0, o_anchored);
    (* lookbehind/word-boundary floors are the fragment start (check_subject) *)
    ("(?<=a)b", "\xffab", 0, 0l);
    ("(?<=a)b", "a\xffb", 0, 0l);
    ("\\bb", "a\xffb", 0, 0l);
    ("\\Bb", "a\xffb", 0, 0l);
    (* verbs interacting with the fragment driver *)
    ("a(*COMMIT)b", "\xffacab", 0, 0l);
    ("a(*SKIP)b|ac", "\xffacab", 0, 0l);
    (* ---------- \C (OP_ANYBYTE) leaving eptr mid-character, then OP_ANY /
       OP_ALLANY forward advance (regression 61565-059bb076) ----------

       \C consumes single code units, so after \C{n} the eptr can sit inside a
       multi-byte character (a continuation byte). The forward advance of `.`
       (OP_ANY) and DOTALL `.` (OP_ALLANY) is Feptr++; ACROSSCHAR
       (pcre2_match.c:962-971 / 3024-3038 / 4495-4518), which from a
       continuation byte consumes the REST of the interrupted character — NOT
       one code unit (GETCHARLEN). The bug: the fast forward loops stepped by
       cp_len, over-consuming across a split character (matching where the
       interpreter/C report NOMATCH, or landing at the wrong span). Subject
       "\xc9\x93\xe8\xaf\x8c\x64" is valid UTF-8: U+0253 (c9 93), U+8BCC
       (e8 af 8c), 'd' (64). \C{3} lands at offset 3 (0xaf, mid-U+8BCC). *)
    (* the minimized repro pattern family (exact + verb + optional literal) *)
    ("(*THEN)-?\\C{3}.{3}", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    ("(*THEN)-?\\C{3}.{3}", "\xc9\x93\xe8\xaf\x8c\x6a\x64", 0, 0l);
    (* OP_ANY exact / bounded / minimize / possessive after a mid-char \C *)
    ("\\C{3}.{3}", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l) (* exact: NOMATCH *);
    ("\\C{3}.{2}", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l) (* exact: MATCH *);
    ("\\C{3}.{2,5}", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    ("\\C{3}.{2,5}?", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l) (* minimize forward min *);
    ("\\C{3}.{2,5}+", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    ("\\C{3}.{0,5}\\x{8bcc}", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l) (* greedy give-back *);
    (* OP_ALLANY (DOTALL .) exact / bounded / greedy / lazy after mid-char \C *)
    ("\\C{3}(?s).{3}", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l) (* exact: NOMATCH *);
    ("\\C{3}(?s).{2,5}", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    ("(?s)\\C{3}.*", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    ("(?s)\\C{3}.*?", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    ("(?s)\\C{3}.{1,3}?d", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    (* single (unquantified) OP_ANY / OP_ALLANY straddling a split character *)
    ("\\C.\\C.", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    ("\\C(?s).\\C.", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    (* \C landing at other mid-char offsets (1 = 0x93, 4 = 0x8c) *)
    ("\\C{1}.{3}", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    ("\\C{4}.{2}", "\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    (* \C repeat / OP_ANY spanning a REAL invalid-UTF fragment boundary *)
    ("\\C{3}.{3}", "\xc9\x93\xe8\xaf\x8c\xff\x64", 0, 0l);
    ("\\C{2}.{2}", "\xc9\x93\xff\xe8\xaf\x8c\x64", 0, 0l);
    (* verb before the fragment end, then a mid-char \C advance *)
    ("a(*COMMIT)\\C.{2}", "a\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    ("(*THEN)\\C{3}.{2}", "\xff\xc9\x93\xe8\xaf\x8c\x64", 0, 0l);
    ("\\C+(*THEN)x", "\xce\xb1\xff\x78", 0, 0l);
    (* partial matching at a mid-char \C + OP_ANY tail *)
    ("\\C{3}.{3}", "\xc9\x93\xe8\xaf\x8c\x64", 0, o_partial_soft);
    ("\\C{5}.{2}", "\xc9\x93\xe8\xaf\x8c\x64", 0, o_partial_hard);
  ]

let invalid_utf_tests =
  List.map
    (fun (pat, subj, off, mopts) ->
      Alcotest.test_case
        (Printf.sprintf "invalid-utf %S %S off=%d opt=0x%lx" pat subj off mopts)
        `Quick
        (fun () ->
          match (F.compile pat miu, E.compile pat miu) with
          | Ok fre, Ok ere ->
              let f = norm (F.exec_full fre subj off mopts) in
              let e = norm (E.exec_full ere subj off mopts) in
              Alcotest.(check string)
                (Printf.sprintf "fast vs interp for %S/%S" pat subj)
                e f
          | Error (F.Unsupported _), _ -> ()
          | Error _, _ | _, Error _ ->
              Alcotest.failf "compile mismatch for %S" pat))
    invalid_utf_cases

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
    (* Chunk I: the UTF greedy give-back path. An anchored maximizing char
       repeat over ~50k TWO-BYTE characters (U+0100 = C4 80) whose continuation
       fails everywhere: one attempt, greedy scan to the end, then ~50k
       give-back steps — each stores the previous CHARACTER boundary via the
       allocation-free [backchar_sub] (rep_giveback) and re-runs the
       continuation. Must stay O(1) minor allocation (§8 — an int-ref per
       BACKCHAR would measure ~150k words here). NO_AUTO_POSSESS keeps the
       repeat a true STAR (the compiler would otherwise auto-possessify
       \x{100}* before 'x', skipping the give-back path entirely); \A pins a
       single attempt. *)
    Alcotest.test_case "alloc: O(1) UTF give-back over ~50k 2-byte chars" `Slow
      (fun () ->
        let o_no_auto_possess = 0x00004000l (* PCRE2_NO_AUTO_POSSESS *) in
        match F.compile "(*UTF)\\A\\x{100}*x" o_no_auto_possess with
        | Error _ -> Alcotest.fail "compile failed"
        | Ok re ->
            let b = Buffer.create (2 * 50_000) in
            for _ = 1 to 50_000 do
              Buffer.add_string b "\xc4\x80"
            done;
            let subject = Buffer.contents b in
            let expect_nomatch r =
              match r with
              | Ok None -> ()
              | Ok (Some _) -> Alcotest.fail "unexpected match"
              | Error c -> Alcotest.failf "unexpected error %d" c
            in
            expect_nomatch (F.exec re subject 0 0l) (* warm-up *);
            let before = Gc.minor_words () in
            let r = F.exec re subject 0 0l in
            let delta = Gc.minor_words () -. before in
            expect_nomatch r;
            Alcotest.(check bool)
              (Printf.sprintf
                 "expected O(1) minor allocation for the UTF give-back over \
                  ~50k 2-byte chars, measured %.0f words"
                 delta)
              true (delta < 1000.));
    (* Regression 61565: the UTF OP_ANY / OP_ALLANY FORWARD advance now steps
       Feptr++; ACROSSCHAR via [any_advance] (was cp_len). That continuation-
       byte skip is the module-level tail-recursive [skip_cont_bytes] (NOT a
       ref/while), so an anchored greedy `.` over ~50k TWO-BYTE chars — one
       forward scan of ~50k [any_advance] calls, then ~50k give-back steps when
       the trailing 'x' never matches — must stay O(1) minor allocation (a ref
       box per char would measure ~100k words). NO_AUTO_POSSESS keeps .* a true
       STAR; \A pins one attempt; U+0100 (C4 80) is a non-newline 2-byte char so
       OP_ANY matches every char and the forward loop calls [any_advance]. *)
    Alcotest.test_case
      "alloc: O(1) UTF OP_ANY forward+give-back over ~50k 2-byte chars" `Slow
      (fun () ->
        let o_no_auto_possess = 0x00004000l in
        match F.compile "(*UTF)\\A.*x" o_no_auto_possess with
        | Error _ -> Alcotest.fail "compile failed"
        | Ok re ->
            let b = Buffer.create (2 * 50_000) in
            for _ = 1 to 50_000 do
              Buffer.add_string b "\xc4\x80"
            done;
            let subject = Buffer.contents b in
            let expect_nomatch r =
              match r with
              | Ok None -> ()
              | Ok (Some _) -> Alcotest.fail "unexpected match"
              | Error c -> Alcotest.failf "unexpected error %d" c
            in
            expect_nomatch (F.exec re subject 0 0l) (* warm-up *);
            let before = Gc.minor_words () in
            let r = F.exec re subject 0 0l in
            let delta = Gc.minor_words () -. before in
            expect_nomatch r;
            Alcotest.(check bool)
              (Printf.sprintf
                 "expected O(1) minor allocation for the UTF OP_ANY \
                  forward+give-back over ~50k 2-byte chars, measured %.0f words"
                 delta)
              true (delta < 1000.));
    (* Chunk I2: the grapheme-cluster GIVE-BACK path — Extuni.extuni forward
       steps (int-only, compiler-eliminated local refs) AND the per-give-back
       [extuni_back] cluster re-walk, which must be closure-free
       (runner.ml extuni_back_walk is module-level; a local `let rec` would
       cost ~6 words per step = ~150k words here). ~25k two-char clusters
       (e + COMBINING ACUTE) with NO digit anywhere: the greedy scan eats
       every cluster, then \d fails at ALL ~25k give-back positions down to
       the floor (a full give-back sweep), then NOMATCH. \d carries no req_cu
       and \A pins the single attempt. *)
    Alcotest.test_case "alloc: O(1) \\X cluster give-back over ~25k clusters"
      `Slow (fun () ->
        let o_no_auto_possess = 0x00004000l in
        match F.compile "(*UTF)\\A\\X*\\d" o_no_auto_possess with
        | Error _ -> Alcotest.fail "compile failed"
        | Ok re ->
            let b = Buffer.create (3 * 25_000) in
            for _ = 1 to 25_000 do
              Buffer.add_string b "e\xcc\x81"
            done;
            let subject = Buffer.contents b in
            let expect_nomatch r =
              match r with
              | Ok None -> ()
              | Ok (Some _) -> Alcotest.fail "unexpected match"
              | Error c -> Alcotest.failf "unexpected error %d" c
            in
            expect_nomatch (F.exec re subject 0 0l) (* warm-up *);
            let before = Gc.minor_words () in
            let r = F.exec re subject 0 0l in
            let delta = Gc.minor_words () -. before in
            expect_nomatch r;
            Alcotest.(check bool)
              (Printf.sprintf
                 "expected O(1) minor allocation for the \\X give-back over \
                  ~25k clusters, measured %.0f words"
                 delta)
              true (delta < 1000.));
    (* Chunk I2: the property test (prop_test / Ucd record reads) in the
       repeat hot loop, forward scan AND full char-wise give-back: ~50k
       two-byte letters with NO digit, so \d fails at every give-back
       position down to the floor. *)
    Alcotest.test_case "alloc: O(1) \\p{L} give-back over ~50k chars" `Slow
      (fun () ->
        let o_no_auto_possess = 0x00004000l in
        match F.compile "(*UTF)\\A\\p{L}*\\d" o_no_auto_possess with
        | Error _ -> Alcotest.fail "compile failed"
        | Ok re ->
            let b = Buffer.create (2 * 50_000) in
            for _ = 1 to 50_000 do
              Buffer.add_string b "\xce\xb1"
            done;
            let subject = Buffer.contents b in
            let expect_nomatch r =
              match r with
              | Ok None -> ()
              | Ok (Some _) -> Alcotest.fail "unexpected match"
              | Error c -> Alcotest.failf "unexpected error %d" c
            in
            expect_nomatch (F.exec re subject 0 0l) (* warm-up *);
            let before = Gc.minor_words () in
            let r = F.exec re subject 0 0l in
            let delta = Gc.minor_words () -. before in
            expect_nomatch r;
            Alcotest.(check bool)
              (Printf.sprintf
                 "expected O(1) minor allocation for the \\p{L} give-back \
                  over ~50k chars, measured %.0f words"
                 delta)
              true (delta < 1000.));
    (* Chunk I2: the RM22 differing-lengths caseless-UTF ref-repeat backtrack
       ([backtrack_ref_max2] + the module-level [ref_max2_rescan] — a local
       `let rec` would cost ~6 words per RM22 step = ~6k words here). Group =
       2-byte U+023A; ~1000 copies alternating 3-byte U+2C65 / 2-byte U+023A
       (differing lengths force RM22); \d fails at every retry, so the
       maximize gives back one copy per step with a forward rescan each
       time. *)
    Alcotest.test_case "alloc: O(1) RM22 ref give-back over ~1000 copies"
      `Slow (fun () ->
        match F.compile "(*UTF)(?i)\\A(\\x{23a})\\1*\\d" 0l with
        | Error _ -> Alcotest.fail "compile failed"
        | Ok re ->
            let b = Buffer.create 4096 in
            Buffer.add_string b "\xc8\xba" (* the group: U+023A *);
            for _ = 1 to 500 do
              Buffer.add_string b "\xe2\xb1\xa5" (* U+2C65, 3 bytes *);
              Buffer.add_string b "\xc8\xba" (* U+023A, 2 bytes *)
            done;
            let subject = Buffer.contents b in
            let expect_nomatch r =
              match r with
              | Ok None -> ()
              | Ok (Some _) -> Alcotest.fail "unexpected match"
              | Error c -> Alcotest.failf "unexpected error %d" c
            in
            expect_nomatch (F.exec re subject 0 0l) (* warm-up *);
            let before = Gc.minor_words () in
            let r = F.exec re subject 0 0l in
            let delta = Gc.minor_words () -. before in
            expect_nomatch r;
            Alcotest.(check bool)
              (Printf.sprintf
                 "expected O(1) minor allocation for the RM22 give-back over \
                  ~1000 copies, measured %.0f words"
                 delta)
              true (delta < 1000.));
    (* Chunk L — the PCRE2_FIRSTLINE scan runs once per bump attempt. The
       NON-UTF scan paths are module-level + tail-recursive and allocation-free
       (§8 named hazard); the UTF scans allocate a ref per character stepped,
       faithfully mirroring the interpreter's own (interpreter.ml:8820) —
       cold pre-match scans outside the frame loop, NOT covered by this pin.
       ANY convention (non-UTF) exercises the VARIABLE [scan_firstline_var]
       path (the one a ref regression would hit); ~2000 'q' on the first line
       = ~2000 failing 'qz' attempts, each re-running the firstline scan.
       Reverting [scan_firstline_var] to an inline `let t = ref …` allocates a
       ref per attempt (~6k words) and trips this pin. *)
    Alcotest.test_case "alloc: O(1) FIRSTLINE scan over ~2000 attempts" `Slow
      (fun () ->
        match F.compile_ctx ~newline:nl_any "qz" firstline_opt with
        | Error _ -> Alcotest.fail "compile failed"
        | Ok re ->
            let subject = String.make 2000 'q' ^ "\rz" in
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
                 "expected O(1) minor allocation for the FIRSTLINE scan over \
                  ~2000 attempts, measured %.0f words"
                 delta)
              true (delta < 1000.));
    (* Chunk M — the CHAR_RUN word compare ([char_run_eq]) reads 8 bytes per
       chunk via String.get_int64_ne and compares them with Int64.equal. Under
       ocamlopt (non-flambda 4.14) the two local int64s are UNBOXED; a
       regression that let them escape (e.g. routing them through a non-inlined
       [int64 -> int64 -> bool] helper, binding to a tuple, or returning the
       int64) boxes each read — ~3 words per 8-byte chunk (measured ~6 for the
       pair). A 40-'a' + 'X' run against a long all-'a' subject makes EVERY bump
       attempt run 5 matching word-compares (bytes 0..39) then fail on the tail
       'X'; NO_START_OPTIMIZE (0x10000) is REQUIRED so the required-code-unit
       'X' (absent from the subject) does NOT short-circuit the whole match to
       NOMATCH before any word compare runs. ~50k attempts * 5 chunks = ~250k
       word-compares: the boxed form allocates ~1.5M words and trips this pin
       (verified by the [@inline never] mutation), while the unboxed form stays
       O(1). Confirms §8 zero-alloc for the word path. *)
    Alcotest.test_case "alloc: O(1) CHAR_RUN word-compare over ~250k chunks"
      `Slow (fun () ->
        let pat = String.make 40 'a' ^ "X" in
        match F.compile pat 0x00010000l (* NO_START_OPTIMIZE *) with
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
                 "expected O(1) minor allocation for the CHAR_RUN word compare \
                  over ~250k chunks, measured %.0f words"
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

(* ---------- 9. Recursion (chunk K1b) parity vs the interpreter ----------

   OP_RECURSE in all supported forms, compared byte-for-byte (rc/ovector/
   start_char via [norm]) to the interpreter (the differential oracle); mark
   parity is covered by the fuzz fast-vs-interp differential (which compares
   exec_full whole). Captures-do-NOT-escape a recursion is pinned by the
   ovector comparison (an inner group set only during a (?n) call must read
   unset afterwards). *)
let recurse_parity_cases : (string * string * int * int32) list =
  [
    (* whole-pattern (?R)/(?0): balanced-paren classics. *)
    ("\\(([^()]|(?R))*\\)", "x(a(b(c)d)e)y", 0, 0l);
    ("\\(([^()]|(?R))*\\)", "(a(b)c", 0, 0l) (* unbalanced -> NM *);
    ("(?0)x|a", "a", 0, 0l);
    (* subroutine (?n): captures do NOT escape (group 2/3 read unset after). *)
    ("(a(b)|(c))(?1)", "abc", 0, 0l);
    ("(a(b)|(c))(?1)", "cab", 0, 0l);
    ("(a(?1)?b)", "aabb", 0, 0l) (* self-recursion *);
    ("^(a)(?1)+ab", "aaaab", 0, 0l) (* recursion + greedy repeat *);
    ("^(a)(?1)++ab", "aaaab", 0, 0l) (* recursion + possessive repeat *);
    ("(a)(?2){2}(.)", "abccd", 0, 0l) (* forward subroutine (?2) *);
    ("(?1)(a(b)|(c))", "abc", 0, 0l) (* forward whole-group call *);
    (* named subroutine (?&name)/(?P>name)/DEFINE. *)
    ("(?(DEFINE)(?<A>a))B(?1)C", "BAC", 0, 0l);
    ("(?(DEFINE)((A)\\2))B(?1)C", "BAAC", 0, 0l);
    ("(?<all>(?:(?:a(?&all))|(b))(c?))", "aabc", 0, 0l);
    ("(?P<L1>(?P<L2>0)(?P>L1)|(?P>L2))", "00", 0, 0l);
    ( "(?(DEFINE)(?<byte>2[0-4]\\d|25[0-5]|1\\d\\d|[1-9]?\\d))\\b(?&byte)(\\.(?&byte)){3}",
      "192.168.1.1", 0, 0l );
    (* relative (?+1)/(?-1). *)
    ("(?+1)x([ab])", "xa", 0, 0l);
    ("([ab])x(?-1)", "axb", 0, 0l);
    (* recursion conditions (?(R)/(?(Rn). *)
    ("(?(R)a+|(?R)b)", "aaaab", 0, 0l);
    ("(?(R)a+|((?R))b)", "aaaab", 0, 0l);
    ("((?(R)a+|(?1)b))", "aaab", 0, 0l);
    ("((?(R1)a+|(?1)b))", "aaab", 0, 0l);
    ("((?(R)a|(?1)))*", "aaa", 0, 0l);
    (* recursion + backref (palindrome). *)
    ("^((.)(?1)\\2|.)$", "abcba", 0, 0l);
    ("^((.)(?1)\\2|.)$", "abca", 0, 0l) (* not a palindrome -> NM *);
    ("^(?:((.)(?1)\\2|)|((.)(?3)\\4|.))$", "abccba", 0, 0l);
    ("^(\\d+|\\((?1)([+*-])(?1)\\)|-(?1))$", "(1+(2*3))", 0, 0l) (* calc grammar *);
    (* recursion + atomic / PRUNE inside a subroutine. *)
    ("(?>(?&t)c|(?&t))(?(DEFINE)(?<t>a|b(*PRUNE)c))", "ac", 0, 0l);
    ("(?>(a)|b)(?1)", "aa", 0, 0l) (* recursion into an atomic-wrapped group *);
    (* recursion + lookbehind (recursion inside (?<=...)). *)
    ("(a)(?<=b(?1))", "ba", 0, 0l);
    ("(?<=b(?1))(a)", "ba", 0, 0l);
    ("(?<X>a)(?<=b(?&X))", "ba", 0, 0l);
    (* recursion + verbs: ( *F), ( *ACCEPT)-in-recursion, COMMIT. *)
    ("(?:(?1)|B)(A(*F)|C)", "BC", 0, 0l);
    ("^(?:(?1)|B)(A(*F)|C)", "BC", 0, 0l);
    ("(?:(?1)|B)(A(*ACCEPT)XX|C)D", "BAD", 0, 0l) (* ( *ACCEPT) inside (?1) *);
    ("(?:(?1)|B)(A(*ACCEPT)XX|C)D", "AAD", 0, 0l);
    ("(?1)(A(*COMMIT)|B)D", "AD", 0, 0l);
    (* branch reset + recursion. *)
    ("^(?|(abc)|(def))(?1)", "abcabc", 0, 0l);
    ("^(?|(abc)|(def))(?1)", "defabc", 0, 0l);
    ("(?|(aaa)|(b))(?1)", "aaaaaa", 0, 0l);
    ("^X(?5)(a)(?|(b)|(q))(c)(d)(Y)", "XYabcdY", 0, 0l);
    (* the fuzz-caught adversarial snapshot shapes: a recursion re-enters a
       group whose GROUP_START (an enclosing non-atomic assertion / repeated
       group) would otherwise be clobbered — the group_start/cap_start restore
       on the recursion return (fast-design.md §3). *)
    ("(?*()c(?R)|(*:x))", "c", 0, 0l);
    ("(a)(?1)()((((((\\1++))\\x85)+)|))", "aa", 0, 0l);
    ("^(?&t)*+(?(DEFINE)(?<t>a))\\w$", "aaab", 0, 0l);
    (* recursion inside a conditional / DEFINE with an optional wrapper. *)
    ("(?(DEFINE)(a))?b(?1)", "ba", 0, 0l);
    ("(a|)*(?1)b", "aaab", 0, 0l);
    ("(?1)(?:(b)){0}", "b", 0, 0l);
  ]

let recurse_parity_tests =
  List.map
    (fun (pat, subj, off, opts) ->
      Alcotest.test_case
        (Printf.sprintf "recurse %S %S" pat subj)
        `Quick
        (fun () ->
          match (F.compile pat 0l, E.compile pat 0l) with
          | Ok fre, Ok ere ->
              let f = norm (F.exec_full fre subj off opts) in
              let e = norm (E.exec_full ere subj off opts) in
              Alcotest.(check string)
                (Printf.sprintf "fast vs interp for %S/%S" pat subj)
                e f
          | Error (F.Unsupported _), _ -> ()
          | Error _, _ | _, Error _ ->
              Alcotest.failf "compile mismatch for %S" pat))
    recurse_parity_cases

(* RECURSELOOP (-52): an infinite recursion (same group, same position, no
   progress) must trip PCRE2_ERROR_RECURSELOOP on BOTH engines — the fast
   engine's last_used_ptr tracking (fast-design.md §3) reproduces the C's check
   (pcre2_match.c:5438-5453) exactly. Under PCRE2_DISABLE_RECURSELOOP_CHECK the
   loop is instead caught by the match limit (-47), again identically. *)
let recurseloop_tests =
  let disable = 0x00040000l in
  let both pat subj opts =
    match (F.compile pat 0l, E.compile pat 0l) with
    | Ok fre, Ok ere ->
        (norm (F.exec_full fre subj 0 opts), norm (E.exec_full ere subj 0 opts))
    | _ -> Alcotest.failf "compile failed for %S" pat
  in
  [
    Alcotest.test_case "RECURSELOOP -52 (fast == interp)" `Quick (fun () ->
        List.iter
          (fun (pat, subj) ->
            let f, e = both pat subj 0l in
            Alcotest.(check string)
              (Printf.sprintf "%S/%S interp = E-52" pat subj)
              "E-52" e;
            Alcotest.(check string)
              (Printf.sprintf "%S/%S fast == interp" pat subj)
              e f)
          [
            ("(?R)", "x");
            ("(?R)b", "b");
            ("(*NO_JIT)((?2)+)((?1)){", "abcd{");
            ("(?(R)a*(?1)|((?R))b)", "aaaabcde");
          ]);
    Alcotest.test_case "DISABLE_RECURSELOOP_CHECK -> limit (fast == interp)"
      `Quick (fun () ->
        (* ( *LIMIT_MATCH=N) bounds the now-uncaught loop; both engines must trip
           the SAME limit code (-47), and sweeping N pins tick parity too. *)
        List.iter
          (fun (body, subj) ->
            for n = 1 to 30 do
              let pat = Printf.sprintf "(*LIMIT_MATCH=%d)%s" n body in
              let f, e = both pat subj disable in
              Alcotest.(check string)
                (Printf.sprintf "%S/%S fast == interp N=%d" body subj n)
                e f
            done)
          [ ("(?R)", "x"); ("(?R)b", "b"); ("(a|(?R))", "a") ]);
    Alcotest.test_case "LIMIT_MATCH sweep, recursion (fast == interp)" `Quick
      (fun () ->
        (* Recursion tick parity: the (?R)/(?n) call enters at the group's
           first branch (RM11 per branch, §4); the shared-ket return and the
           backtrack into a completed recursion tick as the interpreter's
           frames do. Sweep N across the boundary; both engines must agree at
           every N (trip -47 below the boundary, same result at/above). *)
        List.iter
          (fun (body, subj) ->
            for n = 1 to 45 do
              let pat = Printf.sprintf "(*LIMIT_MATCH=%d)%s" n body in
              match (F.compile pat 0l, E.compile pat 0l) with
              | Ok fre, Ok ere ->
                  let f = norm (F.exec_full fre subj 0 0l) in
                  let e = norm (E.exec_full ere subj 0 0l) in
                  Alcotest.(check string)
                    (Printf.sprintf "fast == interp for /%s/ on %S at N=%d" body
                       subj n)
                    e f
              | _ -> Alcotest.failf "compile failed for %S" pat
            done)
          [
            ("\\A(?:a(?R)|b)c", "aabc");
            ("\\A(?:a(?R)|b)c", "aadc") (* fails deep in the recursion *);
            ("\\A\\((?:[^()]|(?R))*\\)9", "(x(y)z)9");
            ("\\A(a)(?1)*b9", "aaab9");
            ("\\A((.)(?1)\\2|.)9", "aba9");
            ("\\A(?(DEFINE)(?<t>a(?&t)?b))(?&t)9", "aabb9");
            (* Reviewer witnesses: last_used_ptr must ALSO be recorded at the
               conditional-assertion and negative-assertion MATCH-returns (the
               C's RRETURN(MATCH_MATCH) kets, pcre2_match.c:5947/6042-6043,
               through RETURN_SWITCH :6470) — the assertion body's forward reach
               feeds the RECURSELOOP -52 depth, so dropping either update flips
               the -52-vs--47 class across this sweep (mutation-tested: with the
               NASSERT_MATCH/COND_ASSERT_MATCH updates disabled, these diverge
               at N=5-8). *)
            ("((?(R)(?(?=aa)(?1))|(?1)))", "aa");
            ("((?(R)(?:(?!aa)b|(?1))|(?1)))", "aa");
            (* Reviewer witness (round 3): a NEGATIVE assertion's branch
               exhaustion must NOT record last_used_ptr — the C's
               ASSERT_NOT_FAILED is a same-frame goto with no RRETURN
               (pcre2_match.c:5561-5564 -> :5582), so the assertion-entry
               position is never recorded, while every OTHER FAIL head (a
               grouploop/positive-assertion/atomic exhaustion RRETURN,
               :5410/:5531) is. The (?=a) lift sets last_used = 1 (the lookahead
               ket record) so the lookbehind branch's below-entry mismatch
               record (0 -> C:1) is absorbed, isolating the FAIL split: with
               FAIL_NASSERT recording like FAIL, this diverges at N=5-6
               (mutation-tested). *)
            ("(?(R)(?<!xyz)(?R)|(?=a)abc(?R))", "abc");
            (* Consuming-arm mismatch positions feed the same check: the C's
               OP_CHAR mismatch RRETURNs AFTER the post-increment
               (`Fecode[1] != *Feptr++`, pcre2_match.c:1022), so a de-fused
               has_recurse char run records p+1 where the fast eptr stops at p
               — without char_run_cfail this diverges at N=4-5 (the -52 fires
               one recursion level early). The sibling shapes pin the per-family
               table (rep_fail_pos): negative lookbehind over a char repeat
               ({2}: caseful INCTEST p+1), a class ([a-c]: read-first p+1), a
               ctype (\d\d: min-loop tests in place, p), and a single-type /
               optional-type forward probe. *)
            ("(?(R)(?<!xy)(?R)|ab(?R))", "ab");
            ("(?(R)(?!xy)(?R)|ab(?R))", "ab");
            ("(?(R)(?<!k{2})(?R)|ab(?R))", "ab");
            ("(?(R)(?<![a-c])(?R)|ab(?R))", "ab");
            ("(?(R)(?<!\\d\\d)(?R)|ab(?R))", "ab");
            ("(?(R)(?=x)?(?R)|ab(?R))", "ab");
            ("(?(R)\\d?(?R)|ab(?R))", "ab");
            ("(?(R)[^x]?(?R)|ab(?R))", "abq");
            ("(?(R)(?i)q?(?R)|ab(?R))", "ab");
            ("(?(R)(?<!\\1x)(?R)|(a)b(?R))", "ab");
            (* Reviewer witnesses (round 4): a VERB code passing a KIND_ONCE /
               KIND_POS boundary is an RRETURN from that construct's frame in
               the C (pcre2_match.c:5408 RM2 / :5529 RM3 / :5315 RM8), recording
               the construct's ENTRY Feptr — which exceeds the verb-fire
               position when the verb fired inside a LOOKBEHIND body. Here the
               PRUNE/COMMIT/SKIP fires at position <= 1 inside (?<=...), passes
               the lookbehind's KIND_ONCE (entry 2 recorded by the C), and is
               CONTAINED at the enclosing (?! (a same-frame goto, NO record,
               :5567-5571); the (?=a) lift sets last_used = 1 so the boundary
               record is the -52-depth discriminator. Mutation-tested: with the
               KIND_ONCE verb-path record disabled these diverge at N=7-10.
               The atomic-group variant pins the RM2 record; the bounded
               possessive-in-lookbehind variant exercises the KIND_POS record
               (kept C-exact per :5315, though provably subsumed — a verb
               passing KIND_POS either fired in-body, whose fire-prerequisite
               already recorded >= iter_start, or inside an inner rewound
               boundary whose own record >= iter_start); the SKIP:name variant
               exercises the KIND_NASSERT SKIP_ARG escape (:5574, an RRETURN
               that DOES record the nassert entry — kept C-exact; its
               observability needs a doubly-nested still-open recursion, so no
               compact mutation witness). *)
            ("(?(R)(?!(?<=(*PRUNE)xq))(?R)|(?=a)ab(?R))", "ab");
            ("(?(R)(?!(?<=(*COMMIT)xq))(?R)|(?=a)ab(?R))", "ab");
            ("(?(R)(?!(?<=(*SKIP)xq))(?R)|(?=a)ab(?R))", "ab");
            ("(?(R)(?!(?<=(?>(*PRUNE)x)q))(?R)|(?=a)ab(?R))", "ab");
            ("(?(R)(?!(?<=(?:(*PRUNE)x){1,2}+q))(?R)|(?=a)ab(?R))", "ab");
            ("(?(R)c(?<!(*SKIP:q)xw)(?R)|(?=a)ab(?R))", "abc");
          ]);
  ]

(* Alloc pin for the recursion path: the FAT KIND_RECURSE / KIND_RECURSE_RET
   records are O(oveccount + n_groups) int copies (module-level helpers, no
   boxing). A deep balanced-paren recursion over a long subject must allocate
   only O(1) minor words beyond the save-stack int array (which is reused). *)
let recurse_alloc_tests =
  [
    Alcotest.test_case "alloc: O(1)/exec deep recursion" `Slow (fun () ->
        (* Anchorless so (?R) recurses only the paren matcher (not ^/$). *)
        match F.compile "\\((?:[^()]|(?R))*\\)" 0l with
        | Error _ -> Alcotest.fail "compile failed"
        | Ok re ->
            (* nested parentheses "((((...))))" ~400 deep. *)
            let depth = 400 in
            let subject = String.make depth '(' ^ String.make depth ')' in
            let expect_match r =
              match r with
              | Ok (Some _) -> ()
              | Ok None -> Alcotest.fail "unexpected non-match"
              | Error c -> Alcotest.failf "unexpected error %d" c
            in
            expect_match (F.exec re subject 0 0l) (* warm-up: grow the stack *);
            let before = Gc.minor_words () in
            let r = F.exec re subject 0 0l in
            let delta = Gc.minor_words () -. before in
            expect_match r;
            Alcotest.(check bool)
              (Printf.sprintf
                 "expected O(1) minor allocation for a depth-%d recursion, \
                  measured %.0f words"
                 depth delta)
              true (delta < 2000.));
  ]

let () =
  Alcotest.run "pcre2_fast_ir"
    [
      ("golden dumps", golden_tests);
      ("unsupported reasons", unsupported_tests);
      ("verifier", verifier_tests);
      ("sweep", sweep_tests);
      ("runner parity", parity_tests);
      ("firstline parity", firstline_parity_tests);
      ("zero unsupported", zero_unsupported_tests);
      ("K2 bsk/accept witnesses", k2_bsk_accept_tests);
      ("backref option parity", ref_options_tests);
      ("invalid-utf fragment parity", invalid_utf_tests);
      ("limit-match boundary", limit_match_boundary_test);
      ("recursion parity", recurse_parity_tests);
      ("recursion loop / limit", recurseloop_tests);
      ("alloc pins", alloc_tests);
      ("recursion alloc pin", recurse_alloc_tests);
      ("stack safety", stack_safety_tests);
    ]
