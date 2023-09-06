open OUnit2
open Pcre2

let simple_test ctxt =
  assert_equal 0 0
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"]
      (full_split ~pat:"(x)|(u)" "abxcd")
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"; Delim "u";
                  NoGroup; Group (2, "u"); Text "ef"]
      (full_split ~pat:"(x)|(u)" "abxcduef")

let suite = "Test pcre" >::: [
      "simple_test"   >:: simple_test
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

