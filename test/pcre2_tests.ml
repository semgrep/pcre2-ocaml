open OUnit2
open! Pcre2

let simple_test ctxt = assert_equal 0 0

let bad_pattern ctxt =
  match compile "ab(" with
  | Error MISSING_CLOSING_PARENTHESIS -> ()
  | Error e ->
      assert_failure ("Incorrectly error for pattern: " ^ show_compile_error e)
  | Ok _ -> assert_failure "Incorrectly compiled invalid pattern"

let check_version ctxt =
  let major, minor = Pcre2.version in
  assert_equal ~printer:string_of_int 10 major;
  assert_equal ~printer:string_of_int 42 minor

let suite =
  "Test pcre"
  >::: [
         "simple_test" >:: simple_test;
         "bad_pattern" >:: bad_pattern;
         "version" >:: check_version;
       ]

let _ = if not !Sys.interactive then run_test_tt_main suite else ()
