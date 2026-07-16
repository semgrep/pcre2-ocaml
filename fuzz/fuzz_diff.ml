(* Differential fuzzer: pure-OCaml PCRE2 engine vs the C oracle.

   For each generated case the fuzzer compiles a grammar-generated pattern
   under a random draw of public option bits with BOTH implementations and,
   when both compile, runs a batch of (subject, start-offset, match-option)
   draws through both.  Any divergence in

     - compile result (Ok / Error, error code, error offset),
     - the load-bearing pattern-info fields (capture count, newline, BSR,
       the UTF bit) and the name table,
     - a match result (result class / rc, full ovector, MARK, startchar),

   is a mismatch.  Mismatches are ddmin-minimized (pattern chars, then
   subject bytes, then the option-bit sets) down to 1-minimal and written as
   a replayable pcre2test-format repro into fuzz/corpus/regressions/, and the
   run continues until --max-failures repros have been collected.

   NO_UTF_CHECK is deliberately kept OUT of the random option pool: with an
   invalid-UTF subject the C skips validation (undefined behaviour we do NOT
   reproduce -- the pure engine always validates, the strictly-safer choice
   from the approved plan), so a NO_UTF_CHECK divergence would be a spurious
   report of C undefined behaviour rather than an engine bug.

   Only libraries: pcre2.engine + pcre2-dev.test-driver, stdlib otherwise. *)

module E = Pcre2_engine.Engine
module O = Pcre2_test_driver.Test_driver
module Fast = Pcre2_fast

(* Differential mode. Default [`Oracle]: the pure engine vs the C oracle
   (unchanged behavior). [`Fast]: the fast engine (Pcre2_fast) vs the pure
   interpreter (Pcre2_engine.Engine) — pure OCaml, no oracle. In fast mode a
   pattern the fast engine declines ([Unsupported]) is a SKIP, not a
   divergence; both-Ok cases compare exec_full byte-for-byte, including the
   imposed LIMIT_MATCH trip point (fast and interp must tick identically,
   fast-design.md §4 — so the oracle-mode -47/-53 confound suppression does
   NOT apply here). *)
let mode : [ `Oracle | `Fast ] ref = ref `Oracle
let fast_skipped = ref 0 (* fast declined (Unsupported) — per diff_case call *)
let fast_compared = ref 0 (* both compiled OK, exec compared — per call *)

(* ================================================================== *)
(*  PCRE2 option-bit constants (pcre2.h.generic 10.44, mirrored so the *)
(*  fuzzer needs no dependency on the harness Flags module).           *)
(* ================================================================== *)

module F = struct
  (* pcre2.h.generic:105-107 *)
  let anchored = 0x80000000
  let no_utf_check = 0x40000000
  let endanchored = 0x20000000

  (* pcre2.h.generic:119-145 — compile options *)
  let allow_empty_class = 0x00000001
  let alt_bsux = 0x00000002
  let caseless = 0x00000008
  let dollar_endonly = 0x00000010
  let dotall = 0x00000020
  let dupnames = 0x00000040
  let extended = 0x00000080
  let firstline = 0x00000100
  let match_unset_backref = 0x00000200
  let multiline = 0x00000400
  let never_ucp = 0x00000800
  let never_utf = 0x00001000
  let no_auto_capture = 0x00002000
  let no_auto_possess = 0x00004000
  let no_dotstar_anchor = 0x00008000
  let no_start_optimize = 0x00010000
  let ucp = 0x00020000
  let ungreedy = 0x00040000
  let utf = 0x00080000
  let never_backslash_c = 0x00100000
  let alt_circumflex = 0x00200000
  let alt_verbnames = 0x00400000
  let use_offset_limit = 0x00800000
  let extended_more = 0x01000000
  let literal = 0x02000000
  let match_invalid_utf = 0x04000000

  (* pcre2.h.generic:149-161 — compile-context extra options *)
  let extra_allow_surrogate_escapes = 0x00000001
  let extra_bad_escape_is_literal = 0x00000002
  let extra_match_word = 0x00000004
  let extra_match_line = 0x00000008
  let extra_escaped_cr_is_lf = 0x00000010
  let extra_alt_bsux = 0x00000020
  let extra_allow_lookaround_bsk = 0x00000040
  let extra_caseless_restrict = 0x00000080
  let extra_ascii_bsd = 0x00000100
  let extra_ascii_bss = 0x00000200
  let extra_ascii_bsw = 0x00000400
  let extra_ascii_posix = 0x00000800
  let extra_ascii_digit = 0x00001000

  (* pcre2.h.generic:176-194 — match options *)
  let notbol = 0x00000001
  let noteol = 0x00000002
  let notempty = 0x00000004
  let notempty_atstart = 0x00000008
  let partial_soft = 0x00000010
  let partial_hard = 0x00000020
  let no_jit = 0x00002000
  let copy_matched_subject = 0x00004000
  let disable_recurseloop_check = 0x00040000

  (* Documented-but-excluded from the random pool (see file header). *)
  let () = ignore no_utf_check
end

(* Option-bit -> pcre2test modifier name, so a repro replays faithfully
   through the conformance runner's harness.  Every drawn bit maps to a
   modifier whose harness action is Apply/Extra (never Skip), and the drawn
   pool excludes NO_UTF_CHECK and AUTO_CALLOUT (the latter is a harness Skip,
   so it could not be replayed). *)

let compile_pool : (int * string) array =
  [|
    (F.allow_empty_class, "allow_empty_class");
    (F.alt_bsux, "alt_bsux");
    (F.caseless, "caseless");
    (F.dollar_endonly, "dollar_endonly");
    (F.dotall, "dotall");
    (F.dupnames, "dupnames");
    (F.extended, "extended");
    (F.extended_more, "extended_more");
    (F.firstline, "firstline");
    (F.match_unset_backref, "match_unset_backref");
    (F.multiline, "multiline");
    (F.never_ucp, "never_ucp");
    (F.never_utf, "never_utf");
    (F.no_auto_capture, "no_auto_capture");
    (F.no_auto_possess, "no_auto_possess");
    (F.no_dotstar_anchor, "no_dotstar_anchor");
    (F.no_start_optimize, "no_start_optimize");
    (F.ucp, "ucp");
    (F.ungreedy, "ungreedy");
    (F.utf, "utf");
    (F.never_backslash_c, "never_backslash_c");
    (F.alt_circumflex, "alt_circumflex");
    (F.alt_verbnames, "alt_verbnames");
    (F.use_offset_limit, "use_offset_limit");
    (F.literal, "literal");
    (F.match_invalid_utf, "match_invalid_utf");
  |]

let extra_pool : (int * string) array =
  [|
    (F.extra_allow_surrogate_escapes, "allow_surrogate_escapes");
    (F.extra_bad_escape_is_literal, "bad_escape_is_literal");
    (F.extra_match_word, "match_word");
    (F.extra_match_line, "match_line");
    (F.extra_escaped_cr_is_lf, "escaped_cr_is_lf");
    (F.extra_alt_bsux, "extra_alt_bsux");
    (F.extra_allow_lookaround_bsk, "allow_lookaround_bsk");
    (F.extra_caseless_restrict, "caseless_restrict");
    (F.extra_ascii_bsd, "ascii_bsd");
    (F.extra_ascii_bss, "ascii_bss");
    (F.extra_ascii_bsw, "ascii_bsw");
    (F.extra_ascii_posix, "ascii_posix");
    (F.extra_ascii_digit, "ascii_digit");
  |]

let match_pool : (int * string) array =
  [|
    (F.anchored, "anchored");
    (F.endanchored, "endanchored");
    (F.notbol, "notbol");
    (F.noteol, "noteol");
    (F.notempty, "notempty");
    (F.notempty_atstart, "notempty_atstart");
    (F.partial_soft, "partial_soft");
    (F.partial_hard, "partial_hard");
    (F.no_jit, "no_jit");
    (F.copy_matched_subject, "copy_matched_subject");
    (F.disable_recurseloop_check, "disable_recurseloop_check");
  |]

(* index = PCRE2_NEWLINE_* value; 0 = build default (omit modifier) *)
let newline_names = [| ""; "CR"; "LF"; "CRLF"; "ANY"; "ANYCRLF"; "NUL" |]

(* index = PCRE2_BSR_* value; 0 = build default (omit) *)
let bsr_names = [| ""; "UNICODE"; "ANYCRLF" |]

(* ================================================================== *)
(*  Seeded PRNG (xorshift* over a 63-bit state; reproducible).         *)
(* ================================================================== *)

type rng = { mutable st : int }

let rng_make seed =
  let s = (seed lxor 0x2545F4914F6CDD1D) land max_int in
  { st = (if s = 0 then 0x1E3779B97F4A7C15 else s) }

let next r =
  let x = ref r.st in
  x := !x lxor (!x lsr 12);
  x := (!x lxor (!x lsl 25)) land max_int;
  x := !x lxor (!x lsr 27);
  r.st <- !x;
  (!x * 0x2545F4914F6CDD1D) land max_int

let rint r n = if n <= 1 then 0 else next r mod n
let rbool r = rint r 2 = 0

(* per-mille chance *)
let chance r p = rint r 1000 < p
let rrange r lo hi = if hi <= lo then lo else lo + rint r (hi - lo + 1)
let rpick r arr = arr.(rint r (Array.length arr))

(* ================================================================== *)
(*  Pattern generator (grammar-based, fuel-bounded).                   *)
(* ================================================================== *)

type gstate = {
  r : rng;
  buf : Buffer.t;
  lits : Buffer.t; (* literal chars, for weighting subjects *)
  mutable ncap : int;
  mutable names : string list;
  mutable fuel : int;
  mutable rec_budget : int; (* max recursion constructs left in this pattern *)
}

let max_depth = 4
let add st s = Buffer.add_string st.buf s
let addc st c = Buffer.add_char st.buf c

let lit_alphabet = "abcABZ019 _-"

let emit_literal st =
  let c = lit_alphabet.[rint st.r (String.length lit_alphabet)] in
  (match c with
  | '\\' | '^' | '$' | '.' | '[' | ']' | '|' | '(' | ')' | '?' | '*' | '+'
  | '{' | '}' | ' ' ->
      addc st '\\';
      addc st c
  | _ -> addc st c);
  Buffer.add_char st.lits c

let gen_name st =
  let letters = "abcxyz" in
  let n = rrange st.r 1 3 in
  let b = Buffer.create 4 in
  for _ = 1 to n do
    Buffer.add_char b letters.[rint st.r (String.length letters)]
  done;
  Buffer.contents b

let escape_classes =
  [| "\\d"; "\\D"; "\\w"; "\\W"; "\\s"; "\\S"; "\\h"; "\\H"; "\\v"; "\\V";
     "\\R"; "\\N"; "\\C" |]

let anchors = [| "^"; "$"; "\\b"; "\\B"; "\\A"; "\\Z"; "\\z"; "\\G"; "\\K" |]

let prop_names =
  [| "L"; "Lu"; "Ll"; "Lt"; "Lm"; "Lo"; "N"; "Nd"; "Nl"; "No"; "P"; "Pc";
     "Z"; "Zs"; "C"; "Cc"; "Cf"; "M"; "Mn"; "S"; "Sm"; "Latin"; "Greek";
     "Han"; "Cyrillic"; "Common"; "Arabic"; "Hebrew"; "Hiragana"; "Xan";
     "Xsp"; "Xps"; "Xwd"; "Xuc"; "Any"; "sc:Latin"; "scx:Greek"; "Sc" |]

let verbs_noarg = [| "(*ACCEPT)"; "(*FAIL)"; "(*F)"; "(*COMMIT)"; "(*PRUNE)";
                     "(*SKIP)"; "(*THEN)" |]

(* "" selects the bare-colon MARK shorthand, i.e. star-colon-name *)
let verbs_arg = [| "MARK"; "PRUNE"; "THEN"; "SKIP"; "" |]

let inline_flags = "imsxUJnu"

let gen_prop st =
  let neg = rbool st.r in
  let name = rpick st.r prop_names in
  if String.length name = 1 && chance st.r 400 then (
    (* single-letter shorthand \pL / \PL *)
    add st (if neg then "\\P" else "\\p");
    add st name)
  else (
    add st (if neg then "\\P{" else "\\p{");
    add st name;
    addc st '}')

let gen_class st =
  addc st '[';
  if chance st.r 350 then addc st '^';
  (* first char: if ] it must be first or escaped; we always escape ] *)
  let items = rrange st.r 1 4 in
  for _ = 1 to items do
    match rint st.r 100 with
    | n when n < 45 ->
        (* literal char, escaping class-specials *)
        let c = lit_alphabet.[rint st.r (String.length lit_alphabet)] in
        (match c with
        | '\\' | ']' | '^' | '-' | '[' ->
            addc st '\\';
            addc st c
        | ' ' -> addc st ' '
        | _ -> addc st c);
        Buffer.add_char st.lits c
    | n when n < 65 ->
        (* a range a-z / 0-9 *)
        let lo, hi =
          match rint st.r 3 with
          | 0 -> ('a', 'z')
          | 1 -> ('A', 'Z')
          | _ -> ('0', '9')
        in
        addc st lo;
        addc st '-';
        addc st hi;
        Buffer.add_char st.lits lo;
        Buffer.add_char st.lits hi
    | n when n < 80 ->
        (* posix class *)
        let names =
          [| "alpha"; "digit"; "alnum"; "space"; "upper"; "lower"; "punct";
             "word"; "xdigit"; "graph"; "print"; "cntrl"; "blank"; "ascii" |]
        in
        add st "[:";
        if chance st.r 250 then addc st '^';
        add st (rpick st.r names);
        add st ":]"
    | n when n < 92 -> add st (rpick st.r [| "\\d"; "\\w"; "\\s"; "\\h"; "\\v" |])
    | _ -> gen_prop st
  done;
  addc st ']'

let rec gen_regex st depth =
  gen_seq st depth;
  let alts = ref 0 in
  while st.fuel > 0 && !alts < 3 && chance st.r 260 do
    addc st '|';
    gen_seq st depth;
    incr alts
  done

and gen_seq st depth =
  let n = ref 0 in
  let go = ref true in
  while !go do
    gen_atom_q st depth;
    incr n;
    if st.fuel <= 0 || !n >= 6 || not (chance st.r 620) then go := false
  done

and gen_atom_q st depth =
  let quantifiable = gen_atom st depth in
  if quantifiable && chance st.r 520 then gen_quant st

and gen_quant st =
  (* Bias toward bounded/tiny quantifiers; unbounded * / + over complex or
     recursive content is the main catastrophic-backtracking source, so keep
     it (for coverage) but rare. *)
  (match rint st.r 10 with
  | 0 -> addc st '*'
  | 1 -> addc st '+'
  | 2 | 3 -> addc st '?'
  | 4 -> add st (Printf.sprintf "{%d}" (rrange st.r 0 3))
  | 5 -> add st (Printf.sprintf "{%d,}" (rrange st.r 0 2))
  | _ ->
      let a = rrange st.r 0 2 in
      let b = a + rrange st.r 0 2 in
      add st (Printf.sprintf "{%d,%d}" a b));
  (* greedy / lazy / possessive *)
  match rint st.r 3 with 0 -> addc st '?' | 1 -> addc st '+' | _ -> ()

and gen_sub st depth =
  if depth < max_depth && st.fuel > 0 then gen_regex st (depth + 1)
  else emit_literal st

(* returns whether the produced atom may carry a quantifier *)
and gen_atom st depth : bool =
  st.fuel <- st.fuel - 1;
  let deep = depth < max_depth && st.fuel > 0 in
  match rint st.r 100 with
  | n when n < 29 ->
      emit_literal st;
      true
  | n when n < 39 ->
      gen_class st;
      true
  | n when n < 45 ->
      addc st '.';
      true
  | n when n < 55 ->
      add st (rpick st.r escape_classes);
      true
  | n when n < 58 ->
      (* \X and unicode singles *)
      (match rint st.r 3 with
      | 0 -> add st "\\X"
      | 1 -> gen_prop st
      | _ ->
          add st "\\x{";
          add st (Printf.sprintf "%x" (rrange st.r 0 0x2ffff));
          addc st '}');
      true
  | n when n < 64 ->
      add st (rpick st.r anchors);
      false
  | n when n < 74 && deep ->
      (* groups *)
      (match rint st.r 5 with
      | 0 ->
          st.ncap <- st.ncap + 1;
          addc st '(';
          gen_sub st depth;
          addc st ')'
      | 1 ->
          add st "(?:";
          gen_sub st depth;
          addc st ')'
      | 2 ->
          let nm = gen_name st in
          st.ncap <- st.ncap + 1;
          st.names <- nm :: st.names;
          let style = rint st.r 3 in
          (match style with
          | 0 -> add st (Printf.sprintf "(?<%s>" nm)
          | 1 -> add st (Printf.sprintf "(?'%s'" nm)
          | _ -> add st (Printf.sprintf "(?P<%s>" nm));
          gen_sub st depth;
          addc st ')'
      | 3 ->
          add st "(?|";
          gen_sub st depth;
          addc st ')'
      | _ ->
          add st "(?>";
          gen_sub st depth;
          addc st ')');
      true
  | n when n < 79 && deep ->
      (* lookarounds *)
      add st
        (rpick st.r [| "(?="; "(?!"; "(?<="; "(?<!"; "(?*"; "(*positive_lookahead:" |]);
      gen_sub st depth;
      addc st ')';
      false
  | n when n < 83 && deep ->
      (* atomic + script runs *)
      (match rint st.r 4 with
      | 0 -> add st "(?>"
      | 1 -> add st "(*script_run:"
      | 2 -> add st "(*sr:"
      | _ -> add st "(*atomic_script_run:");
      gen_sub st depth;
      addc st ')';
      true
  | n when n < 86 ->
      (* backref *)
      (if st.ncap > 0 then
         match rint st.r 4 with
         | 0 -> add st (Printf.sprintf "\\%d" (rrange st.r 1 st.ncap))
         | 1 -> add st (Printf.sprintf "\\g{%d}" (rrange st.r 1 st.ncap))
         | 2 -> add st (Printf.sprintf "\\g%d" (rrange st.r 1 st.ncap))
         | _ -> (
             match st.names with
             | [] -> add st (Printf.sprintf "\\%d" (rrange st.r 1 st.ncap))
             | l ->
                 let nm = List.nth l (rint st.r (List.length l)) in
                 add st
                   (rpick st.r
                      [|
                        Printf.sprintf "\\k<%s>" nm;
                        Printf.sprintf "\\k'%s'" nm;
                        Printf.sprintf "\\k{%s}" nm;
                        Printf.sprintf "\\g{%s}" nm;
                        Printf.sprintf "(?P=%s)" nm;
                      |]))
       else add st (Printf.sprintf "\\%d" (rrange st.r 1 3)));
      true
  | n when n < 89 && deep ->
      (* conditionals *)
      (match rint st.r 6 with
      | 0 ->
          add st (Printf.sprintf "(?(%d)" (rrange st.r 1 (max 1 st.ncap)));
          gen_sub st depth;
          addc st ')'
      | 1 -> (
          match st.names with
          | [] ->
              add st "(?(1)";
              gen_sub st depth;
              addc st ')'
          | l ->
              let nm = List.nth l (rint st.r (List.length l)) in
              add st (Printf.sprintf "(?(<%s>)" nm);
              gen_sub st depth;
              addc st ')')
      | 2 ->
          add st "(?(R)";
          gen_sub st depth;
          addc st ')'
      | 3 ->
          add st (Printf.sprintf "(?(R%d)" (rrange st.r 1 (max 1 st.ncap)));
          gen_sub st depth;
          addc st ')'
      | 4 ->
          add st "(?(DEFINE)";
          gen_sub st depth;
          addc st ')'
      | _ ->
          add st "(?(?=";
          gen_sub st depth;
          addc st ')';
          gen_sub st depth;
          addc st ')');
      false
  | n when n < 92 && st.rec_budget > 0 ->
      (* recursion (budgeted: at most a couple per pattern -- recursion inside
         quantified groups is the main catastrophic-backtracking source) *)
      st.rec_budget <- st.rec_budget - 1;
      (match rint st.r 5 with
      | 0 -> add st "(?R)"
      | 1 -> add st (Printf.sprintf "(?%d)" (rrange st.r 0 (max 1 st.ncap)))
      | 2 -> add st (Printf.sprintf "(?+%d)" (rrange st.r 1 3))
      | 3 -> (
          match st.names with
          | [] -> add st "(?R)"
          | l ->
              let nm = List.nth l (rint st.r (List.length l)) in
              add st (rpick st.r [| Printf.sprintf "(?&%s)" nm;
                                    Printf.sprintf "(?P>%s)" nm;
                                    Printf.sprintf "\\g<%s>" nm |]))
      | _ -> add st (Printf.sprintf "\\g<%d>" (rrange st.r 0 (max 1 st.ncap))));
      false
  | n when n < 96 ->
      (* verbs *)
      (if chance st.r 500 then add st (rpick st.r verbs_noarg)
       else
         let v = rpick st.r verbs_arg in
         let nm = gen_name st in
         if String.equal v "" then add st (Printf.sprintf "(*:%s)" nm)
         else add st (Printf.sprintf "(*%s:%s)" v nm));
      false
  | _ ->
      (* inline options, scoped or global *)
      let k = rrange st.r 1 3 in
      let fl = Buffer.create 4 in
      for _ = 1 to k do
        Buffer.add_char fl inline_flags.[rint st.r (String.length inline_flags)]
      done;
      let flags = Buffer.contents fl in
      (if deep && chance st.r 500 then (
         (match rint st.r 3 with
         | 0 -> add st (Printf.sprintf "(?%s:" flags)
         | 1 -> add st (Printf.sprintf "(?^%s:" flags)
         | _ -> add st (Printf.sprintf "(?%s-i:" flags));
         gen_sub st depth;
         addc st ')')
       else
         match rint st.r 3 with
         | 0 -> add st (Printf.sprintf "(?%s)" flags)
         | 1 -> add st "(?^)"
         | _ -> add st (Printf.sprintf "(?-%s)" flags));
      false

let gen_pattern r : string * string =
  let st =
    { r; buf = Buffer.create 64; lits = Buffer.create 16; ncap = 0; names = [];
      fuel = rrange r 6 34; rec_budget = (if chance r 220 then 1 else 0) }
  in
  gen_regex st 0;
  let pat = Buffer.contents st.buf in
  let lits = Buffer.contents st.lits in
  (pat, if String.length lits = 0 then lit_alphabet else lits)

(* ================================================================== *)
(*  Subject generator.                                                 *)
(* ================================================================== *)

let utf8_encode cp b =
  if cp < 0x80 then Buffer.add_char b (Char.chr cp)
  else if cp < 0x800 then (
    Buffer.add_char b (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))))
  else if cp < 0x10000 then (
    Buffer.add_char b (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))))
  else (
    Buffer.add_char b (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))))

let gen_codepoint r =
  match rint r 100 with
  | n when n < 55 -> rrange r 0x20 0x7e (* ASCII printable *)
  | n when n < 80 -> rrange r 0x80 0x7ff (* 2-byte *)
  | n when n < 95 -> rrange r 0x800 0xffff (* BMP 3-byte (may hit surrogates) *)
  | _ -> rrange r 0x10000 0x10ffff (* astral *)

(* Subjects are kept short (<= ~12 bytes): long subjects combined with
   ambiguous quantifiers hit the 10M match-limit on BOTH engines (the pure
   engine mirrors the C tick sites), which is identical behaviour but wastes
   wall-clock -- short subjects keep the campaign fast without losing the
   edge-case coverage the fuzzer is after. *)
let gen_subject r lits : string =
  let short () = rrange r 0 8 in
  match rint r 100 with
  | n when n < 22 ->
      (* sampled from the pattern's literal chars (match-friendly) *)
      let len = short () in
      let b = Buffer.create len in
      for _ = 1 to len do
        Buffer.add_char b lits.[rint r (String.length lits)]
      done;
      Buffer.contents b
  | n when n < 38 ->
      (* random ASCII *)
      let len = short () in
      String.init len (fun _ -> Char.chr (rrange r 0x20 0x7e))
  | n when n < 52 ->
      (* full random bytes (0..255), incl NULs *)
      let len = rrange r 0 10 in
      String.init len (fun _ -> Char.chr (rint r 256))
  | n when n < 68 ->
      (* valid UTF-8 (weighted toward small code points) *)
      let nc = rrange r 0 6 in
      let b = Buffer.create nc in
      for _ = 1 to nc do
        let cp = gen_codepoint r in
        (* avoid surrogates so it stays valid UTF-8 *)
        let cp = if cp >= 0xd800 && cp <= 0xdfff then cp - 0xd800 + 0x20 else cp in
        utf8_encode cp b
      done;
      Buffer.contents b
  | n when n < 82 ->
      (* deliberately INVALID UTF-8 *)
      let b = Buffer.create 8 in
      (* seed with a few valid chars, then inject a malformation *)
      for _ = 1 to rrange r 0 2 do
        utf8_encode (rrange r 0x20 0x7e) b
      done;
      (match rint r 6 with
      | 0 -> Buffer.add_char b '\x80' (* lone continuation *)
      | 1 -> Buffer.add_char b '\xC0'; Buffer.add_char b '\x20' (* bad 2nd byte *)
      | 2 -> Buffer.add_char b '\xE0'; Buffer.add_char b '\x80' (* truncated *)
      | 3 -> Buffer.add_char b '\xF5'; Buffer.add_char b '\x80';
             Buffer.add_char b '\x80'; Buffer.add_char b '\x80' (* > 0x10ffff *)
      | 4 -> Buffer.add_char b '\xED'; Buffer.add_char b '\xA0';
             Buffer.add_char b '\x80' (* surrogate encoding *)
      | _ -> Buffer.add_char b '\xFF');
      for _ = 1 to rrange r 0 2 do
        utf8_encode (rrange r 0x20 0x7e) b
      done;
      Buffer.contents b
  | n when n < 88 -> "" (* empty *)
  | n when n < 94 ->
      (* newline-flavoured *)
      rpick r [| "\r\n"; "\n"; "\r"; "a\r\nb"; "\n\n"; "\x00"; "a\x00b";
                 "\xe2\x80\xa8" (* U+2028 *) |]
  | _ ->
      (* longer boundary case *)
      let len = rrange r 10 14 in
      String.init len (fun _ -> lits.[rint r (String.length lits)])

(* ================================================================== *)
(*  Differential comparison.                                           *)
(* ================================================================== *)

type nres = { rc : int; ov : int array; mark : string option; sc : int }
type eres = Res of nres | Crash of string

let eng_exec c subj off mo : eres =
  try
    match E.exec_full c subj off (Int32.of_int mo) with
    | E.Match { ovector; mark; start_char } ->
        let oc = (E.info c).E.capture_count + 1 in
        let full = Array.make (2 * oc) (-1) in
        Array.blit ovector 0 full 0 (Array.length ovector);
        Res { rc = Array.length ovector / 2; ov = full; mark; sc = start_char }
    | E.No_match { mark } -> Res { rc = -1; ov = [||]; mark; sc = 0 }
    | E.Partial { start; mark } ->
        Res { rc = -2; ov = [| start; String.length subj |]; mark; sc = start }
    | E.Error { code; start_char } ->
        Res { rc = code; ov = [||]; mark = None; sc = start_char }
  with e -> Crash (Printexc.to_string e)

let ora_exec c subj off mo : eres =
  try
    let r = O.exec ~options:mo ~subject:subj ~offset:off c in
    Res { rc = r.O.rc; ov = r.O.ovector; mark = r.O.mark; sc = r.O.startchar }
  with e -> Crash (Printexc.to_string e)

(* Fast engine, same shape as [eng_exec] (both return E.exec_result). *)
let fast_exec c subj off mo : eres =
  try
    match Fast.exec_full c subj off (Int32.of_int mo) with
    | E.Match { ovector; mark; start_char } ->
        let oc = (Fast.info c).E.capture_count + 1 in
        let full = Array.make (2 * oc) (-1) in
        Array.blit ovector 0 full 0 (Array.length ovector);
        Res { rc = Array.length ovector / 2; ov = full; mark; sc = start_char }
    | E.No_match { mark } -> Res { rc = -1; ov = [||]; mark; sc = 0 }
    | E.Partial { start; mark } ->
        Res { rc = -2; ov = [| start; String.length subj |]; mark; sc = start }
    | E.Error { code; start_char } ->
        Res { rc = code; ov = [||]; mark = None; sc = start_char }
  with e -> Crash (Printexc.to_string e)

let mark_eq a b =
  match (a, b) with
  | None, None -> true
  | Some x, Some y -> String.equal x y
  | _ -> false

let mark_str = function None -> "<none>" | Some s -> Printf.sprintf "%S" s
let ov_str a = "[" ^ String.concat ";" (Array.to_list (Array.map string_of_int a)) ^ "]"

(* the UTF error-code range whose startchar pcre2test prints (" at offset N") *)
let is_utf_err rc = rc <= -3 && rc >= -28

let res_summary = function
  | Crash s -> "CRASH:" ^ s
  | Res r ->
      Printf.sprintf "rc=%d ov=%s mark=%s sc=%d" r.rc (ov_str r.ov)
        (mark_str r.mark) r.sc

(* Compare a single exec; returns Some human-readable-reason on divergence. *)
let cmp_exec (a : eres) (b : eres) : string option =
  match (a, b) with
  | Crash _, Crash _ -> None (* both raised: treat as agreeing *)
  | Crash s, Res _ -> Some ("engine raised: " ^ s)
  | Res _, Crash s -> Some ("oracle raised: " ^ s)
  | Res a, Res b ->
      if a.rc <> b.rc then
        Some (Printf.sprintf "rc %d vs %d" a.rc b.rc)
      else if a.rc > 0 then
        if Array.length a.ov <> Array.length b.ov then
          Some (Printf.sprintf "ovector length %d vs %d" (Array.length a.ov)
                  (Array.length b.ov))
        else if a.ov <> b.ov then
          Some (Printf.sprintf "ovector %s vs %s" (ov_str a.ov) (ov_str b.ov))
        else if a.sc <> b.sc then
          Some (Printf.sprintf "startchar %d vs %d" a.sc b.sc)
        else if not (mark_eq a.mark b.mark) then
          Some (Printf.sprintf "mark %s vs %s" (mark_str a.mark) (mark_str b.mark))
        else None
      else if a.rc = -1 then
        if not (mark_eq a.mark b.mark) then
          Some (Printf.sprintf "nomatch mark %s vs %s" (mark_str a.mark)
                  (mark_str b.mark))
        else None
      else if a.rc = -2 then
        (* PARTIAL: only ovector[0..1] are meaningful at this seam (the engine
           driver reports just [start; end]; the oracle returns the full padded
           ovector with the rest unset). Compare the two load-bearing slots. *)
        let two v = if Array.length v >= 2 then (v.(0), v.(1)) else (-99, -99) in
        let a0, a1 = two a.ov and b0, b1 = two b.ov in
        if a0 <> b0 || a1 <> b1 then
          Some (Printf.sprintf "partial ovector [%d;%d] vs [%d;%d]" a0 a1 b0 b1)
        else if a.sc <> b.sc then
          Some (Printf.sprintf "partial startchar %d vs %d" a.sc b.sc)
        else if not (mark_eq a.mark b.mark) then
          Some (Printf.sprintf "partial mark %s vs %s" (mark_str a.mark)
                  (mark_str b.mark))
        else None
      else if is_utf_err a.rc && a.sc <> b.sc then
        Some (Printf.sprintf "utf-error startchar %d vs %d" a.sc b.sc)
      else None

(* A full case: pattern + compile draw + one exec draw.

   [limit] > 0 prepends "(*LIMIT_MATCH=N)" to the pattern: PCRE2 and the pure
   engine both honour this verb (the engine caps at min(default, re.limit_match),
   pcre2 likewise), so it bounds catastrophic backtracking / deep recursion
   IDENTICALLY on both sides without any API change.  The shrinker tries to
   drop it, so a repro keeps it only when the divergence needs it. *)
type case = {
  pat : string;
  copts : int;
  newline : int;
  bsr : int;
  extra : int;
  limit : int;
  subj : string;
  off : int;
  mopts : int;
}

let effective_pat c =
  if c.limit > 0 then Printf.sprintf "(*LIMIT_MATCH=%d)%s" c.limit c.pat
  else c.pat

(* MATCHLIMIT (-47) / DEPTHLIMIT (-53): with our imposed LIMIT_MATCH these are
   confounded (a side that is merely slower, not wrong, trips the cap), so a
   one-sided limit result is NOT reported as a divergence. *)
let is_limit_rc rc = rc = -47 || rc = -53

type kind = Compile_diff | Info_diff | Exec_diff

(* Returns Some (kind, human reason) if the case diverges (oracle mode). *)
let diff_case_oracle (c : case) : (kind * string) option =
  let pat = effective_pat c in
  let eng =
    try
      match E.compile_ctx ~newline:c.newline ~bsr:c.bsr ~extra:c.extra pat
              (Int32.of_int c.copts) with
      | Ok code -> `Ok code
      | Error (ec, eo) -> `Err (ec, eo)
    with e -> `Crash (Printexc.to_string e)
  in
  let ora =
    try
      match O.compile ~options:c.copts ~newline:c.newline ~bsr:c.bsr
              ~extra:c.extra pat with
      | Ok code -> `Ok code
      | Error { O.errcode; erroroffset } -> `Err (errcode, erroroffset)
    with e -> `Crash ("oracle:" ^ Printexc.to_string e)
  in
  match (eng, ora) with
  | `Crash s, _ -> Some (Compile_diff, "engine compile raised: " ^ s)
  | _, `Crash s -> Some (Compile_diff, "oracle compile raised: " ^ s)
  | `Err (e1, o1), `Err (e2, o2) ->
      if e1 <> e2 then
        Some (Compile_diff, Printf.sprintf "compile error code %d vs %d" e1 e2)
      else if o1 <> o2 then
        Some (Compile_diff, Printf.sprintf "compile error offset %d vs %d" o1 o2)
      else None
  | `Err (e1, _), `Ok _ ->
      Some (Compile_diff, Printf.sprintf "engine error %d, oracle Ok" e1)
  | `Ok _, `Err (e2, _) ->
      Some (Compile_diff, Printf.sprintf "engine Ok, oracle error %d" e2)
  | `Ok ec, `Ok oc ->
      let ei = E.info ec and oi = O.info oc in
      if ei.E.capture_count <> oi.O.capture_count then
        Some
          (Info_diff,
           Printf.sprintf "capture_count %d vs %d" ei.E.capture_count
             oi.O.capture_count)
      else if ei.E.newline <> oi.O.newline then
        Some (Info_diff, Printf.sprintf "newline %d vs %d" ei.E.newline oi.O.newline)
      else if ei.E.bsr <> oi.O.bsr then
        Some (Info_diff, Printf.sprintf "bsr %d vs %d" ei.E.bsr oi.O.bsr)
      else if
        ei.E.alloptions land F.utf <> oi.O.alloptions land F.utf
      then Some (Info_diff, "utf option bit differs")
      else if E.capture_groups ec <> O.name_table oc then
        Some (Info_diff, "name table differs")
      else
        let a = eng_exec ec c.subj c.off c.mopts in
        let b = ora_exec oc c.subj c.off c.mopts in
        (* suppress divergences confounded by the imposed LIMIT_MATCH cap *)
        let confounded =
          match (a, b) with
          | Res x, Res y -> is_limit_rc x.rc || is_limit_rc y.rc
          | _ -> false
        in
        if confounded then None
        else match cmp_exec a b with
          | Some reason -> Some (Exec_diff, reason)
          | None -> None

(* Fast engine vs pure interpreter (fast-vs-interp mode). Both use the SHARED
   compiler, so compile outcomes agree except that the fast side may decline a
   valid pattern ([Unsupported]) — counted as a skip, never a divergence.
   Both-Ok cases compare exec_full over the same draw. No limit-confound
   suppression: fast and interp must tick identically (fast-design.md §4). *)
let diff_case_fast (c : case) : (kind * string) option =
  let pat = effective_pat c in
  let eng =
    try
      match
        E.compile_ctx ~newline:c.newline ~bsr:c.bsr ~extra:c.extra pat
          (Int32.of_int c.copts)
      with
      | Ok code -> `Ok code
      | Error (ec, eo) -> `Err (ec, eo)
    with e -> `Crash (Printexc.to_string e)
  in
  let fst =
    try
      match
        Fast.compile_ctx ~newline:c.newline ~bsr:c.bsr ~extra:c.extra pat
          (Int32.of_int c.copts)
      with
      | Ok code -> `Ok code
      | Error (Fast.Compile_error { errcode; erroroffset }) ->
          `Err (errcode, erroroffset)
      | Error (Fast.Unsupported reason) -> `Unsup reason
    with e -> `Crash (Printexc.to_string e)
  in
  match (eng, fst) with
  | `Crash s, _ -> Some (Compile_diff, "engine compile raised: " ^ s)
  | _, `Crash s -> Some (Compile_diff, "fast compile raised: " ^ s)
  | _, `Unsup _ ->
      incr fast_skipped;
      None (* fast declined this pattern: skip, not a divergence *)
  | `Err (e1, o1), `Err (e2, o2) ->
      if e1 <> e2 then
        Some (Compile_diff, Printf.sprintf "compile error code %d vs %d" e1 e2)
      else if o1 <> o2 then
        Some (Compile_diff, Printf.sprintf "compile error offset %d vs %d" o1 o2)
      else None
  | `Err (e1, _), `Ok _ ->
      Some (Compile_diff, Printf.sprintf "engine error %d, fast Ok" e1)
  | `Ok _, `Err (e2, _) ->
      Some (Compile_diff, Printf.sprintf "engine Ok, fast error %d" e2)
  | `Ok ec, `Ok fc ->
      incr fast_compared;
      let a = eng_exec ec c.subj c.off c.mopts in
      let b = fast_exec fc c.subj c.off c.mopts in
      (match cmp_exec a b with Some reason -> Some (Exec_diff, reason) | None -> None)

let diff_case (c : case) : (kind * string) option =
  match !mode with `Oracle -> diff_case_oracle c | `Fast -> diff_case_fast c

let reproduces c = diff_case c <> None

(* The shrinker's predicate budget: bounds worst-case shrink time on a
   pathological pattern.  When exhausted, [shrink] returns the best result so
   far (still a valid, if not fully 1-minimal, repro). *)
let shrink_budget = ref 0

let reproduces_budgeted c =
  if !shrink_budget <= 0 then false
  else (
    decr shrink_budget;
    reproduces c)

(* ================================================================== *)
(*  ddmin-style shrinker (greedy 1-minimization per dimension).        *)
(* ================================================================== *)

let clear_bits_one c set_int get_int =
  (* try clearing each set bit; keep the clear if the case still reproduces *)
  let v = ref (get_int c) in
  let bit = ref 1 in
  while !bit <= 0x80000000 do
    if !v land !bit <> 0 then (
      let cand = set_int c (!v land lnot !bit) in
      if reproduces_budgeted cand then v := !v land lnot !bit);
    bit := !bit lsl 1
  done;
  set_int c !v

let shrink_string cur set_str get_str =
  (* greedy single-element deletion to a fixpoint *)
  let changed = ref true in
  let c = ref cur in
  while !changed do
    changed := false;
    let s = get_str !c in
    let i = ref 0 in
    while !i < String.length (get_str !c) do
      let s = get_str !c in
      if String.length s = 0 then i := max_int
      else
        let cand_s =
          String.sub s 0 !i ^ String.sub s (!i + 1) (String.length s - !i - 1)
        in
        let cand = set_str !c cand_s in
        if reproduces_budgeted cand then (
          c := cand;
          changed := true)
        else incr i
    done;
    ignore s
  done;
  !c

let shrink (c0 : case) : case =
  shrink_budget := 4000;
  let c = ref c0 in
  let rounds = ref 0 in
  let stable = ref false in
  while (not !stable) && !rounds < 4 do
    let before = !c in
    (* pattern *)
    c := shrink_string !c (fun c s -> { c with pat = s }) (fun c -> c.pat);
    (* subject *)
    c := shrink_string !c (fun c s -> { c with subj = s }) (fun c -> c.subj);
    (* option bit sets *)
    c := clear_bits_one !c (fun c v -> { c with copts = v }) (fun c -> c.copts);
    c := clear_bits_one !c (fun c v -> { c with extra = v }) (fun c -> c.extra);
    c := clear_bits_one !c (fun c v -> { c with mopts = v }) (fun c -> c.mopts);
    (* newline / bsr / offset *)
    (if !c.newline <> 0 then
       let cand = { !c with newline = 0 } in
       if reproduces_budgeted cand then c := cand);
    (if !c.bsr <> 0 then
       let cand = { !c with bsr = 0 } in
       if reproduces_budgeted cand then c := cand);
    (if !c.off <> 0 then
       let cand = { !c with off = 0 } in
       if reproduces_budgeted cand then c := cand);
    (* NB: the imposed LIMIT_MATCH prefix is intentionally NOT dropped here --
       removing it would let a catastrophic pattern grind unbounded (the
       budget is a call count, not a wall-clock cap). It stays in the repro; it
       is harmless for the fast divergences (they use far fewer ticks) and
       keeps the repro's match cost bounded. *)
    incr rounds;
    if before = !c then stable := true
  done;
  !c

(* ================================================================== *)
(*  Repro writer (pcre2test format, replayable by the conformance      *)
(*  runner's differential regressions mode).                           *)
(* ================================================================== *)

let rec find_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project")
     && Sys.file_exists (Filename.concat dir "vendor/pcre2/testdata")
  then dir
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then Sys.getcwd () else find_root parent

let regressions_dir = lazy (Filename.concat (find_root (Sys.getcwd ()))
                              "fuzz/corpus/regressions")

(* Portable recursive directory creation (Sys.mkdir tolerates a race). *)
let ensure_dir dir =
  let rec go d =
    if String.length d > 0 && not (Sys.file_exists d) then (
      go (Filename.dirname d);
      (try Sys.mkdir d 0o755 with Sys_error _ -> ()))
  in
  go dir

let fnv1a s =
  let h = ref 0x811c9dc5 in
  String.iter
    (fun c ->
      h := (!h lxor Char.code c) land 0xffffffff;
      h := (!h * 0x01000193) land 0xffffffff)
    s;
  !h

let hex_bytes s =
  let b = Buffer.create (3 * String.length s) in
  String.iteri
    (fun i c ->
      if i > 0 then Buffer.add_char b ' ';
      Buffer.add_string b (Printf.sprintf "%02x" (Char.code c)))
    s;
  Buffer.contents b

let byte_escapes s =
  let b = Buffer.create (4 * String.length s) in
  String.iter (fun c -> Buffer.add_string b (Printf.sprintf "\\x%02x" (Char.code c))) s;
  Buffer.contents b

let bits_to_mods v pool =
  Array.to_list pool
  |> List.filter_map (fun (bit, name) -> if v land bit <> 0 then Some name else None)

let compile_modifiers c =
  let ms = ref [ "hex" ] in
  ms := !ms @ bits_to_mods c.copts compile_pool;
  ms := !ms @ bits_to_mods c.extra extra_pool;
  if c.newline <> 0 then ms := !ms @ [ "newline=" ^ newline_names.(c.newline) ];
  if c.bsr <> 0 then ms := !ms @ [ "bsr=" ^ bsr_names.(c.bsr) ];
  String.concat "," !ms

let data_modifiers c =
  (* always request mark + startchar so any such divergence is visible *)
  let ms = [ Printf.sprintf "offset=%d" c.off; "mark"; "startchar" ] in
  String.concat "," (ms @ bits_to_mods c.mopts match_pool)

let write_repro ~seed ~case_no ~kind ~reason (c : case) : string =
  let dir = Lazy.force regressions_dir in
  ensure_dir dir;
  let kind_s =
    match kind with
    | Compile_diff -> "compile"
    | Info_diff -> "info"
    | Exec_diff -> "exec"
  in
  let epat = effective_pat c in
  let hash = fnv1a (epat ^ "|" ^ c.subj ^ Printf.sprintf "|%x|%x" c.copts c.mopts) in
  let base = Printf.sprintf "%03d-%08x.txt" case_no hash in
  let file = Filename.concat dir base in
  let oc = open_out file in
  let p fmt = Printf.fprintf oc fmt in
  p "# fuzz-diff repro (differential: pure engine vs C oracle)\n";
  p "# seed=%d case=%d kind=%s\n" seed case_no kind_s;
  p "# reason: %s\n" reason;
  p "# pattern (ascii): %s\n" (String.escaped epat);
  p "# compile: options=0x%08x extra=0x%08x newline=%d bsr=%d\n" c.copts c.extra
    c.newline c.bsr;
  (if kind = Exec_diff then (
     p "# exec: subject=%S offset=%d mopts=0x%08x\n" c.subj c.off c.mopts;
     let eng =
       match E.compile_ctx ~newline:c.newline ~bsr:c.bsr ~extra:c.extra epat
               (Int32.of_int c.copts) with
       | Ok code -> res_summary (eng_exec code c.subj c.off c.mopts)
       | Error (e, o) -> Printf.sprintf "compile Error %d @ %d" e o
     in
     let ora =
       match O.compile ~options:c.copts ~newline:c.newline ~bsr:c.bsr
               ~extra:c.extra epat with
       | Ok code -> res_summary (ora_exec code c.subj c.off c.mopts)
       | Error { O.errcode; erroroffset } ->
           Printf.sprintf "compile Error %d @ %d" errcode erroroffset
     in
     p "# engine: %s\n" eng;
     p "# oracle: %s\n" ora));
  (* the replayable pcre2test unit *)
  p "/%s/%s\n" (hex_bytes epat) (compile_modifiers c);
  if kind = Exec_diff then
    p "    %s\\=%s\n" (byte_escapes c.subj) (data_modifiers c);
  p "\n";
  close_out oc;
  file

(* ================================================================== *)
(*  Search loop + CLI.                                                 *)
(* ================================================================== *)

let default_seed = 0x5C2E2F00

let draw_bits r pool max_k =
  let k = rint r (max_k + 1) in
  let v = ref 0 in
  for _ = 1 to k do
    let bit, _ = rpick r pool in
    v := !v lor bit
  done;
  !v

(* --dump-case N: deterministically re-derive the N-th generated case for
   the given seed and print it (pattern, compile draws, and every
   subject/offset/match-option exec draw) WITHOUT executing case N against
   either implementation.  This exists to extract cases that kill the whole
   process (e.g. C-oracle undefined behaviour turning into SIGSEGV): the
   crashing case index from a run can be replayed here safely because
   nothing is executed for case N.

   RNG-path fidelity: cases 1..N-1 must consume the RNG exactly as a real
   run does.  Generation is RNG-only EXCEPT that a divergence found while
   sweeping case i's exec draws exits that sweep early (skipping the
   remaining draws' RNG consumption), so the prefix cases ARE executed by
   dump mode, exactly like a real run.  That is safe: a process crash at
   case N means the prefix completed.  Shrinking and repro-writing consume
   no RNG, so the dump skips them (and ignores --max-failures, which would
   only stop a real run early).  For case N itself nothing is executed: the
   full 4-draw sweep is printed even though a real run may have stopped
   partway through it (crash or divergence) -- the extra draws are harmless
   because nothing after case N consumes the RNG. *)
let dump_case_print ~seed ~case_no r (base : case) lits =
  let epat = effective_pat base in
  Printf.printf "# fuzz-diff dump-case (no execution; see --dump-case)\n";
  Printf.printf "# seed=%d case=%d\n" seed case_no;
  Printf.printf "# pattern (ascii): %s\n" (String.escaped epat);
  Printf.printf "# pattern (hex): %s\n" (hex_bytes epat);
  Printf.printf "# compile: options=0x%08x extra=0x%08x newline=%d bsr=%d\n"
    base.copts base.extra base.newline base.bsr;
  (* Draw 0 is the compile-level base check: diff_case on [base] runs one
     exec with the empty subject, offset 0, mopts 0 (no RNG consumed). *)
  let draws = ref [ { base with subj = ""; off = 0; mopts = 0 } ] in
  (* Replicate the exec sweep's RNG consumption (search loop below) verbatim,
     minus execution and minus early exit. *)
  let ndraw = 4 in
  for _ = 1 to ndraw do
    let subj = gen_subject r lits in
    let noff = if String.length subj = 0 then 1 else rrange r 1 2 in
    for o = 1 to noff do
      let off =
        if o = 1 then 0
        else
          let l = String.length subj in
          rrange r 0 (l + 1)
      in
      let mopts = draw_bits r match_pool 3 in
      draws := { base with subj; off; mopts } :: !draws
    done
  done;
  let draws = List.rev !draws in
  List.iteri
    (fun i c ->
      Printf.printf "# draw %d: subject(hex)=[%s] offset=%d mopts=0x%08x\n" i
        (hex_bytes c.subj) c.off c.mopts)
    draws;
  (* Replayable pcre2test-format unit (same shape write_repro emits), with
     one data line per draw in sweep order. *)
  Printf.printf "/%s/%s\n" (hex_bytes epat) (compile_modifiers base);
  List.iter
    (fun c ->
      Printf.printf "    %s\\=%s\n" (byte_escapes c.subj) (data_modifiers c))
    draws;
  print_newline ()

(* A coarse signature to dedup distinct bug CLASSES: quoted mark strings ->
   S, bracketed ovectors -> V, integers kept (rc/offset values distinguish
   genuinely different divergences). *)
let signature kind reason =
  let b = Buffer.create 32 in
  let n = String.length reason in
  let i = ref 0 in
  while !i < n do
    (match reason.[!i] with
    | '"' ->
        Buffer.add_char b 'S';
        incr i;
        while !i < n && reason.[!i] <> '"' do incr i done
    | '[' ->
        Buffer.add_char b 'V';
        while !i < n && reason.[!i] <> ']' do incr i done
    | c -> Buffer.add_char b c);
    incr i
  done;
  let k = match kind with Compile_diff -> "C" | Info_diff -> "I" | Exec_diff -> "E" in
  k ^ "|" ^ Buffer.contents b

let usage () =
  prerr_endline
    "usage: fuzz_diff [--cases N] [--seed S] [--max-failures N] [--verbose] \
     [--dump-case N] [--selftest] [--mode oracle|fast-vs-interp]";
  exit 2

let () =
  let cases = ref 10000 in
  let seed = ref default_seed in
  let max_failures = ref 10 in
  let verbose = ref false in
  let selftest = ref false in
  let dump_case = ref 0 in
  let rec parse = function
    | [] -> ()
    | "--cases" :: n :: t -> cases := int_of_string n; parse t
    | "--seed" :: n :: t -> seed := int_of_string n; parse t
    | "--max-failures" :: n :: t -> max_failures := int_of_string n; parse t
    | "--verbose" :: t -> verbose := true; parse t
    | "--selftest" :: t -> selftest := true; parse t
    | "--mode" :: "oracle" :: t -> mode := `Oracle; parse t
    | "--mode" :: "fast-vs-interp" :: t -> mode := `Fast; parse t
    | "--mode" :: m :: _ ->
        Printf.eprintf "fuzz_diff: unknown --mode %s\n" m; usage ()
    | "--dump-case" :: n :: t -> dump_case := int_of_string n; parse t
    | "--help" :: _ | "-help" :: _ -> usage ()
    | a :: _ -> Printf.eprintf "fuzz_diff: unknown argument %s\n" a; usage ()
  in
  parse (List.tl (Array.to_list Sys.argv));
  if !dump_case > 0 then cases := !dump_case;

  if !selftest then (
    (* Self-check the shrinker + repro plumbing without needing a real bug. *)
    let ok = ref true in
    (* ddmin on a string predicate: keep only what is necessary. *)
    let cur =
      { pat = "aaZbbb"; copts = 0; newline = 0; bsr = 0; extra = 0; limit = 0;
        subj = "xxQyy"; off = 0; mopts = 0 }
    in
    (* fake predicate: reproduces iff pat contains 'Z' AND subj contains 'Q' *)
    let contains s ch = String.exists (fun c -> c = ch) s in
    let saved = ref cur in
    let repro_stub c = contains c.pat 'Z' && contains c.subj 'Q' in
    (* temporarily swap reproduces via a local re-implementation of shrink *)
    let shrink_str cur setf getf =
      let changed = ref true in
      let c = ref cur in
      while !changed do
        changed := false;
        let i = ref 0 in
        while !i < String.length (getf !c) do
          let s = getf !c in
          let cand_s =
            String.sub s 0 !i ^ String.sub s (!i + 1) (String.length s - !i - 1)
          in
          let cand = setf !c cand_s in
          if repro_stub cand then (c := cand; changed := true) else incr i
        done
      done;
      !c
    in
    saved := shrink_str !saved (fun c s -> { c with pat = s }) (fun c -> c.pat);
    saved := shrink_str !saved (fun c s -> { c with subj = s }) (fun c -> c.subj);
    if not (String.equal !saved.pat "Z" && String.equal !saved.subj "Q") then (
      Printf.eprintf "selftest: shrink got pat=%S subj=%S\n" !saved.pat
        !saved.subj;
      ok := false);
    (* repro rendering round-trips into a file *)
    (let f = write_repro ~seed:0 ~case_no:0 ~kind:Exec_diff ~reason:"selftest"
               { pat = "a.b"; copts = F.caseless; newline = 0; bsr = 0;
                 extra = 0; limit = 0; subj = "A\x00B"; off = 0;
                 mopts = F.notbol } in
     if not (Sys.file_exists f) then (
       Printf.eprintf "selftest: repro file not written\n"; ok := false)
     else Sys.remove f);
    if !ok then (print_endline "selftest: OK"; exit 0)
    else (print_endline "selftest: FAILED"; exit 1));

  let r = rng_make !seed in
  let t0 = Sys.time () in
  let failures = ref 0 in
  let repros = ref [] in
  let seen_sigs = ref [] in
  let dup_hits = ref 0 in
  let stop = ref false in
  let ci = ref 0 in
  let tprev = ref (Sys.time ()) in
  while (not !stop) && !ci < !cases do
    incr ci;
    let pat, lits = gen_pattern r in
    let copts = draw_bits r compile_pool 4 in
    let extra = draw_bits r extra_pool 3 in
    let newline = if chance r 300 then rrange r 1 6 else 0 in
    let bsr = if chance r 200 then rrange r 1 2 else 0 in
    (* base case for compile-level checks; subject/exec filled per draw.
       A default LIMIT_MATCH bounds catastrophic backtracking / deep recursion
       during the sweep (dropped by the shrinker when the repro doesn't need
       it).

       The imposed limit bounds each match ATTEMPT, not a whole exec: when a
       ( *SKIP:name) fails with no matching mark, pcre2_match re-runs the
       attempt at the SAME start with mb->ignore_skip_arg bumped
       (pcre2_match.c:6407-6424, 7536-7539) and mb->match_call_count RESET
       (pcre2_match.c:7506) — the re-run count is unbounded by the limit, so
       worst-case total work is Theta(limit^3) on BOTH implementations.  At
       the old limit of 80000, seed 4242 case 26842
       ("(?0)( *SKIP:z)" + PCRE2_DISABLE_RECURSELOOP_CHECK) measured 39 min
       (C oracle) / ~3.2 h (engine) for ONE exec draw — an apparent campaign
       hang.  2000 caps that saga at ~0.04 s (C) / ~0.2 s (engine), measured
       via the same case's cubic scaling (time x8 per limit doubling, both
       sides).  Divergence coverage lost to the lower cap is limited to
       cases needing > 2000 frame ticks to demonstrate a diff: one-sided
       -47/-53 results are already suppressed as limit-confounded by
       is_limit_rc above (see its definition and the `confounded` check in
       diff_case). *)
    let base =
      { pat; copts; newline; bsr; extra; limit = 2000; subj = ""; off = 0;
        mopts = 0 }
    in
    if !dump_case = !ci then (
      dump_case_print ~seed:!seed ~case_no:!ci r base lits;
      exit 0);
    let found = ref None in
    (match diff_case base with
    | Some (((Compile_diff | Info_diff) as k), reason) -> found := Some (k, reason, base)
    | Some (Exec_diff, _) ->
        (* the empty-subject draw already diverges on exec *)
        found := Some (Exec_diff, "empty-subject exec", base)
    | None ->
        (* both compile OK & info agrees: sweep exec draws *)
        let ndraw = 4 in
        let d = ref 0 in
        while !found = None && !d < ndraw do
          incr d;
          let subj = gen_subject r lits in
          let noff = if String.length subj = 0 then 1 else rrange r 1 2 in
          let o = ref 0 in
          while !found = None && !o < noff do
            incr o;
            let off =
              if !o = 1 then 0
              else
                let l = String.length subj in
                rrange r 0 (l + 1)
            in
            let mopts = draw_bits r match_pool 3 in
            let c = { base with subj; off; mopts } in
            (match diff_case c with
            | Some (k, reason) -> found := Some (k, reason, c)
            | None -> ())
          done
        done);
    (if !verbose then
       let now = Sys.time () in
       if now -. !tprev > 0.15 then
         Printf.eprintf "SLOW %.2fs case %d: %S\n%!" (now -. !tprev) !ci pat;
       tprev := now);
    match !found with
    | None -> ()
    | Some _ when !dump_case > 0 ->
        (* dump-mode prefix: the divergence's RNG consumption (the sweep's
           early exit) already happened above; shrink/write consume no RNG,
           so skipping them keeps the path identical while avoiding side
           effects on the way to case N. *)
        ()
    | Some (kind, reason, c) ->
        let minimized = shrink c in
        (* recompute kind/reason on the minimized case for accuracy *)
        let kind, reason =
          match diff_case minimized with Some kr -> kr | None -> (kind, reason)
        in
        let sg = signature kind reason in
        if List.mem sg !seen_sigs then incr dup_hits
        else (
          seen_sigs := sg :: !seen_sigs;
          let file =
            write_repro ~seed:!seed ~case_no:!ci ~kind ~reason minimized
          in
          incr failures;
          repros := file :: !repros;
          Printf.printf "MISMATCH #%d (case %d) [%s]: %s\n  repro: %s\n" !failures
            !ci sg reason file;
          if !verbose then
            Printf.printf "  minimized pattern: %S  subject: %S\n" minimized.pat
              minimized.subj;
          flush stdout;
          if !failures >= !max_failures then stop := true)
  done;
  let dt = Sys.time () -. t0 in
  let rate = if dt > 0.0 then float_of_int !ci /. dt else 0.0 in
  let mode_str = match !mode with `Oracle -> "oracle" | `Fast -> "fast-vs-interp" in
  Printf.printf
    "\nfuzz_diff: mode=%s seed=%d cases_run=%d distinct-classes=%d dup-hits=%d  \
     (%.0f cases/sec, %.1fs)\n"
    mode_str !seed !ci !failures !dup_hits rate dt;
  (match !mode with
  | `Fast ->
      Printf.printf "fuzz_diff: fast-vs-interp compared=%d skipped(unsupported)=%d\n"
        !fast_compared !fast_skipped
  | `Oracle -> ());
  if !failures > 0 then (
    Printf.printf "repros written under %s:\n" (Lazy.force regressions_dir);
    List.iter (fun f -> Printf.printf "  %s\n" f) (List.rev !repros);
    exit 1)
  else (print_endline "fuzz_diff: no divergences found."; exit 0)
