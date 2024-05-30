open OUnit2
open! Pcre2

let ( >+= ) = Fun.flip Option.map
let ( >>= ) = Option.bind

let simple_test ctxt =
  match compile "abc" with
  | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
  | Ok re ->
      let printer = [%show: range option] in
      assert_equal ~printer
        (find re "abc" >+= range_of_match)
        (Some { start = 0; end_ = 3 });
      assert_equal ~printer
        (find re "123abc456" >+= range_of_match)
        (Some { start = 3; end_ = 6 });
      assert_equal ~printer
        (find re "123abc" >+= range_of_match)
        (Some { start = 3; end_ = 6 });
      assert_equal ~printer (find re "123ac" >+= range_of_match) None

let simple_captures ctxt =
  match compile "(a)(b)(c)" with
  | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
  | Ok re ->
      let printer = [%show: range option] in
      let c = captures re "abc" in
      assert_equal ~printer
        (c >>= (fun c -> match_of_captures c 0) >+= range_of_match)
        (Some { start = 0; end_ = 3 });
      assert_equal ~printer
        (c >>= (fun c -> match_of_captures c 1) >+= range_of_match)
        (Some { start = 0; end_ = 1 });
      assert_equal ~printer
        (c >>= (fun c -> match_of_captures c 2) >+= range_of_match)
        (Some { start = 1; end_ = 2 });
      assert_equal ~printer
        (c >>= (fun c -> match_of_captures c 3) >+= range_of_match)
        (Some { start = 2; end_ = 3 })

let bad_pattern ctxt =
  match compile "ab(" with
  | Error MISSING_CLOSING_PARENTHESIS -> ()
  | Error e ->
      assert_failure ("Incorrectly error for pattern: " ^ show_compile_error e)
  | Ok _ -> assert_failure "Incorrectly compiled invalid pattern"

let check_version ctxt =
  let major, minor = Pcre2.version in
  assert_equal ~printer:string_of_int 10 major;
  assert_equal ~printer:string_of_int 43 minor

let suite =
  "Test pcre"
  >::: [
         "simple_test" >:: simple_test;
         "simple_test" >:: simple_captures;
         "bad_pattern" >:: bad_pattern;
         "version" >:: check_version;
       ]

let _ = if not !Sys.interactive then run_test_tt_main suite else ()
