(* Systhreads within one domain take turns holding the domain lock, and the
   stubs never release the runtime lock, so the thread-based test in
   [pcre2_tests] interleaves rather than parallelises.  Domains hold separate
   locks, so this is where a cache that was process-global instead of
   thread-local would actually corrupt results. *)

open OUnit2
open Pcre2.Interp

let narrow_pattern = "([a-z]+)-([0-9]+)"
let wide_pattern = "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)(m)(n)(o)(p)(q)(r)(s)(t)"
let wide_subject = "abcdefghijklmnopqrst"

let group_substrings c =
  List.init (captures_length c - 1) (fun i ->
      match_of_captures c (i + 1) |> Option.map substring_of_match)

let parallel_matching ctxt =
  match (compile narrow_pattern, compile wide_pattern) with
  | Error e, _ | _, Error e ->
      assert_failure ("failed to compile: " ^ show_compile_error e)
  | Ok narrow, Ok wide ->
      let iterations = 5_000 in
      let domains = 8 in
      let worker tid () =
        (* Only odd domains grow their cache, so the domains are not all running
           at the same ovector width. *)
        let grows = tid mod 2 = 1 in
        let tag = String.make 3 (Char.chr (Char.code 'a' + tid)) in
        let bad = ref 0 in
        let completed = ref 0 in
        for i = 0 to iterations - 1 do
          if grows then (
            match captures wide wide_subject with
            | Ok (Some c) when captures_length c = 21 -> ()
            | _ -> incr bad);
          (match captures narrow (tag ^ "-" ^ string_of_int i) with
          | Ok (Some c) ->
              if group_substrings c <> [ Some tag; Some (string_of_int i) ] then
                incr bad
          | Ok None | Error _ -> incr bad);
          incr completed
        done;
        (!bad, !completed)
      in
      (* Every domain must be spawned before any is joined, or they run in
         sequence and the test proves nothing. *)
      let spawned = List.init domains (fun tid -> Domain.spawn (worker tid)) in
      let results = List.map Domain.join spawned in
      assert_equal ~printer:[%show: int list] ~msg:"mismatches per domain"
        (List.init domains (fun _ -> 0))
        (List.map fst results);
      assert_equal ~printer:[%show: int list]
        ~msg:"iterations completed per domain"
        (List.init domains (fun _ -> iterations))
        (List.map snd results)

let suite = "Test pcre2 across domains" >::: [ "parallel_matching" >:: parallel_matching ]
let _ = if not !Sys.interactive then run_test_tt_main suite else ()
