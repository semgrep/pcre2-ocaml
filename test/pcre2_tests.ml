open OUnit2
open! Pcre2

let ( >+= ) x f =
  match x with
  | Ok (Some x) -> Ok (Some (f x))
  | Ok None -> Ok None
  | Error e -> Error e

let ( >>= ) x f =
  match x with
  | Ok (Some x) -> Ok (f x)
  | Ok None -> Ok None
  | Error e -> Error e

let simple_test ctxt =
  Interp.(
    match compile "abc" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 3 }))
          (find re "abc" >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 3; end_ = 6 }))
          (find re "123abc456" >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 3; end_ = 6 }))
          (find re "123abc" >+= range_of_match);
        assert_equal ~printer (Ok None) (find re "123ac" >+= range_of_match))

let simple_captures ctxt =
  Interp.(
    match compile "(a)(b)(c)" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        let c = captures re "abc" in
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 3 }))
          (c >>= (fun c -> match_of_captures c 0) >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 0; end_ = 1 }))
          (c >>= (fun c -> match_of_captures c 1) >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 1; end_ = 2 }))
          (c >>= (fun c -> match_of_captures c 2) >+= range_of_match);
        assert_equal ~printer
          (Ok (Some { start = 2; end_ = 3 }))
          (c >>= (fun c -> match_of_captures c 3) >+= range_of_match);
        assert_equal ~printer (Ok None)
          (c >>= (fun c -> match_of_captures c 4) >+= range_of_match))

let bad_pattern ctxt =
  Interp.(
    match compile "ab(" with
    | Error MISSING_CLOSING_PARENTHESIS -> ()
    | Error e ->
        assert_failure ("Incorrectly error for pattern: " ^ show_compile_error e)
    | Ok _ -> assert_failure "Incorrectly compiled invalid pattern")

let bad_offset ctxt =
  Interp.(
    match compile "abc" with
    | Error e -> assert_failure ("failed to compile: " ^ show_compile_error e)
    | Ok re ->
        let printer = [%show: (range option, match_error) result] in
        assert_equal ~printer (Error BADOFFSET)
          (find ~subject_offset:(-1) re "ab" >+= range_of_match);
        assert_equal ~printer (Error BADOFFSET)
          (find ~subject_offset:10 re "123abc456" >+= range_of_match))

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
         "bad_offset" >:: bad_offset;
         "version" >:: check_version;
       ]

let _ = if not !Sys.interactive then run_test_tt_main suite else ()
