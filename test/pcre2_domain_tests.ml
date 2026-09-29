open OUnit2
open Pcre2.Interp

let parallel_matching _ =
  let compile_or_fail pattern =
    match compile pattern with
    | Ok re -> re
    | Error e -> assert_failure (show_compile_error e)
  in
  let wide =
    compile_or_fail
      "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)(m)(n)(o)(p)(q)(r)(s)(t)"
  in
  let narrow = compile_or_fail "([a-z]+)-([0-9]+)" in
  let worker id () =
    let tag = String.make 3 (Char.chr (Char.code 'a' + id)) in
    let failures = ref 0 in
    for i = 0 to 999 do
      if id mod 2 = 0 then
        match captures wide "abcdefghijklmnopqrst" with
        | Ok (Some c) when captures_length c = 21 -> ()
        | _ -> incr failures;
      (match captures narrow (tag ^ "-" ^ string_of_int i) with
      | Ok (Some c) ->
          let group n =
            match_of_captures c n |> Option.map substring_of_match
          in
          if group 1 <> Some tag || group 2 <> Some (string_of_int i) then
            incr failures
      | _ -> incr failures)
    done;
    !failures
  in
  let domains = List.init 8 (fun id -> Domain.spawn (worker id)) in
  let failures = List.map Domain.join domains in
  assert_equal ~printer:[%show: int list] (List.init 8 (fun _ -> 0)) failures

let suite = "domains" >::: [ "parallel_matching" >:: parallel_matching ]
let () = if not !Sys.interactive then run_test_tt_main suite
