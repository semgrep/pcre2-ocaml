open OUnit2
open Pcre2

let simple_test ctxt =
  assert_equal 0 0
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"]
      (full_split ~pat:"(x)|(u)" "abxcd")
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"; Delim "u";
                  NoGroup; Group (2, "u"); Text "ef"]
      (full_split ~pat:"(x)|(u)" "abxcduef")

let bad_pattern ctxt =
  try
    ignore (regexp "?");
    assert_failure "Regex should fail to parse"
  with Error (BadPattern (s, _)) ->
    assert_bool
      "String contains a zero byte. In 8-bit mode this indicates an error in \
       the creation of the error message since strings created by PCRE2 should \
       be null terminated."
      (not @@ String.exists (fun c -> c = '\000') s)

(* The match_data cache is per-thread and grow-only: alternating between
   patterns with few and many capture groups must keep producing correct
   captures as the cached ovector grows and gets reused. *)
let match_data_reuse ctxt =
  let small = regexp "([a-z]+)-([0-9]+)" in
  let big =
    regexp "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)(m)(n)(o)(p)(q)(r)(s)(t)"
  in
  for i = 0 to 999 do
    let subj = Printf.sprintf "item-%d" i in
    let groups = extract ~rex:small ~full_match:false subj in
    assert_equal [| "item"; string_of_int i |] groups;
    let groups_big =
      extract ~rex:big ~full_match:false "abcdefghijklmnopqrst"
    in
    assert_equal 20 (Array.length groups_big);
    assert_equal "a" groups_big.(0);
    assert_equal "t" groups_big.(19);
    (* And back to the small pattern with the grown cache. *)
    assert_bool "pmatch after growth" (pmatch ~rex:small subj)
  done

(* Concurrent matching on shared compiled regexps: each thread has its own
   match_data cache, so results must be correct under parallelism. *)
let concurrent_matching ctxt =
  let rex = regexp "([a-z]+)-([0-9]+)-([a-z]+)" in
  let failures = ref 0 in
  let mutex = Mutex.create () in
  let worker tid () =
    (* Letters only: the pattern's outer groups are [a-z]+. *)
    let tag = String.make 3 (Char.chr (Char.code 'a' + tid)) in
    for i = 0 to 4_999 do
      let subj = Printf.sprintf "left%s-%d-right%s" tag i tag in
      let expected =
        [| "left" ^ tag; string_of_int i; "right" ^ tag |]
      in
      let ok =
        try extract ~rex ~full_match:false subj = expected with
        | _ -> false
      in
      if not ok then (
        Mutex.lock mutex;
        incr failures;
        Mutex.unlock mutex)
    done
  in
  let threads = List.init 8 (fun tid -> Thread.create (worker tid) ()) in
  List.iter Thread.join threads;
  assert_equal ~printer:string_of_int 0 !failures

let show_strings a = "[|" ^ String.concat ";" (Array.to_list a) ^ "|]"
let show_ints a = "[|" ^ String.concat ";" (List.map string_of_int a) ^ "|]"

(* A callout function runs OCaml code in the middle of a match, and that code
   may match again on the same thread. The nested match must not borrow the
   outer match's match_data: PCRE2 keeps the backtracking frames vector inside
   it, so sharing one block either overwrites the frames the outer match is
   walking or frees them outright when the nested pattern needs a wider block.
   Both matches must come out with the captures they would have had alone. *)
let reentrant_callout ctxt =
  let outer = regexp ~flags:[ `AUTO_CALLOUT ] "(a+)(b+)(c+)" in
  (* Wider than the outer pattern, so a shared block would be grown -- and
     backtracking-heavy, so it would be rewritten as well. *)
  let nested = regexp "(x+)(x+)(x+)(x+)(x+)(x+)(x+)(x+)" in
  let nested_subject = String.make 64 'x' in
  let calls = ref 0 in
  let nested_failures = ref 0 in
  let callout _ =
    incr calls;
    (* [`AUTO_CALLOUT] fires on every item of the pattern, so bound the work. *)
    if !calls <= 100 then
      match extract ~rex:nested ~full_match:false nested_subject with
      | groups ->
          if
            Array.length groups <> 8
            || String.concat "" (Array.to_list groups) <> nested_subject
          then incr nested_failures
      | exception _ -> incr nested_failures
  in
  let substrings = exec ~rex:outer ~callout "aaabbbccc" in
  assert_bool "the callout never ran" (!calls > 0);
  assert_equal ~printer:string_of_int
    ~msg:"nested matches run from inside the callout" 0 !nested_failures;
  assert_equal ~printer:show_strings ~msg:"captures of the outer match"
    [| "aaa"; "bbb"; "ccc" |]
    (get_substrings ~full_match:false substrings);
  (* The nested match handed its block back, so matching still works here. *)
  assert_equal ~printer:show_strings [| "aaa"; "bbb"; "ccc" |]
    (extract ~rex:(regexp "(a+)(b+)(c+)") ~full_match:false "aaabbbccc")

(* An exception from a callout function leaves the stub through [caml_raise],
   which still has to hand the match data back before it goes. *)
let callout_exceptions ctxt =
  let rex = regexp ~flags:[ `AUTO_CALLOUT ] "(a+)(b+)" in
  for _ = 1 to 100 do
    assert_raises Exit (fun () ->
        ignore (exec ~rex ~callout:(fun _ -> raise Exit) "aaabbb"))
  done;
  assert_equal ~printer:show_strings [| "aaa"; "bbb" |]
    (get_substrings ~full_match:false (exec ~rex "aaabbb"))

(* A PCRE2 error return leaves through [handle_match_error], which also has to
   hand the match data back before raising. *)
let match_limit_error ctxt =
  let rex = regexp ~limit:1 "(a+)+$" in
  assert_raises (Error MatchLimit) (fun () ->
      ignore (exec ~rex (String.make 40 'a' ^ "b")));
  assert_equal ~printer:show_strings [| "aaa" |]
    (get_substrings ~full_match:false (exec ~rex:(regexp "(a+)") "aaa"))

(* [pcre2_dfa_match] uses the ovector for alternative match lengths rather than
   captures, so its yield -- and with it the number of pairs copied into the
   OCaml-side array -- grows with the width of the ovector it is handed. A block
   from the shared cache is wider than the pattern needs, which would both
   change the result and overrun that array, so DFA matching gets an
   exactly-sized block of its own.

   Every pattern here is ungreedy, since auto-possessification would otherwise
   leave the DFA with a single alternative and nothing to overflow with. *)
let dfa_ovector_width ctxt =
  let subj = "This is <something> <something else> <something further> no more" in
  let dfa pat =
    Array.to_list
      (pcre2_dfa_match ~rex:(regexp pat) ~workspace:(Array.make 200 0) subj)
  in
  (* Three matches start at offset 8, and the pattern has no capture groups, so
     the OCaml-side ovector holds a single pair. PCRE2 reports "too many
     matches to fit" as a yield of zero, and nothing is copied out. *)
  assert_equal ~printer:show_ints ~msg:"three matches, room for one"
    [ 0; 0; 0 ] (dfa "<.*?>");
  (* Two capture groups make room for three pairs, so the same three matches
     fit and come back longest first. *)
  assert_equal ~printer:show_ints ~msg:"three matches, room for three"
    [ 8; 56; 8; 36; 8; 19; 0; 0; 0 ]
    (dfa "((<.*?>))");
  (* Widening the cache with an unrelated match must not change either result:
     five pairs here against the one the narrow pattern needs. *)
  assert_bool "priming match" (pmatch ~rex:(regexp "(a)(b)(c)(d)") "abcd");
  assert_equal ~printer:show_ints ~msg:"three matches, room for one, warm cache"
    [ 0; 0; 0 ] (dfa "<.*?>");
  assert_equal ~printer:show_ints
    ~msg:"three matches, room for three, warm cache"
    [ 8; 56; 8; 36; 8; 19; 0; 0; 0 ]
    (dfa "((<.*?>))")

let suite = "Test pcre" >::: [
      "simple_test"   >:: simple_test;
      "bad_pattern"   >:: bad_pattern;
      "match_data_reuse" >:: match_data_reuse;
      "concurrent_matching" >:: concurrent_matching;
      "reentrant_callout" >:: reentrant_callout;
      "callout_exceptions" >:: callout_exceptions;
      "match_limit_error" >:: match_limit_error;
      "dfa_ovector_width" >:: dfa_ovector_width
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

