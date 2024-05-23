open OUnit2
open! Pcre2

let simple_test ctxt = assert_equal 0 0

let bad_pattern ctxt = assert_bool "todo" true

let suite =
  "Test pcre"
  >::: [ "simple_test" >:: simple_test; "bad_pattern" >:: bad_pattern ]

let _ = if not !Sys.interactive then run_test_tt_main suite else ()
