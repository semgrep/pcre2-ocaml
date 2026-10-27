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

let parallel_jit_stack_retry _ =
  let re =
    match
      Pcre2.Jit.compile
        ~options:[ `UTF; `MATCH_LIMIT 1_000_000; `DEPTH_LIMIT 10_000 ]
        "(?<letter>a|b)*c"
    with
    | Ok re -> re
    | Error e -> assert_failure (Pcre2.show_compile_error e)
  in
  let start = Atomic.make false in
  let worker id () =
    while not (Atomic.get start) do
      Domain.cpu_relax ()
    done;
    let letter = if id mod 2 = 0 then 'a' else 'b' in
    let subject = "ac " ^ String.make 3_000 letter ^ "c bc" in
    let expected = [ Some "a"; Some (String.make 1 letter); Some "b" ] in
    let failures = ref 0 in
    for _ = 1 to 20 do
      let groups =
        Pcre2.Jit.captures_iter re subject
        |> List.of_seq
        |> List.map (function
          | Ok c ->
              Pcre2.Jit.named_match_of_captures c "letter"
              |> Option.map Pcre2.Jit.substring_of_match
          | Error _ -> None)
      in
      if groups <> expected then incr failures
    done;
    !failures
  in
  let domains = List.init 4 (fun id -> Domain.spawn (worker id)) in
  Atomic.set start true;
  let failures = List.map Domain.join domains in
  assert_equal ~printer:[%show: int list] [ 0; 0; 0; 0 ] failures

let suite =
  "domains"
  >::: [
         "parallel_matching" >:: parallel_matching;
         "parallel_jit_stack_retry" >:: parallel_jit_stack_retry;
       ]

let () = if not !Sys.interactive then run_test_tt_main suite
