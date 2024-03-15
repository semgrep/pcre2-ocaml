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

let suite = "Test pcre" >::: [
      "simple_test"   >:: simple_test;
      "bad_pattern"   >:: bad_pattern
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

