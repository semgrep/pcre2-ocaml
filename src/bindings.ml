type jit = [ `JIT ]
type interp = [ `Interp ]
type 'a regex constraint 'a = [< jit | interp ]

external pcre2_ocaml_init : unit -> unit = "pcre2_ocaml_init"

external pcre2_compile :
  string -> (int32[@unboxed]) -> (interp regex, int) Result.t
  = "compile" "compile_unboxed"

external pcre2_match :
  _ regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  (int * int, int) Result.t = "match" "match_unboxed"

external pcre2_capture :
  _ regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  ((int * int) array * (string * int) array, int) Result.t
  = "capture" "capture_unboxed"

external pcre2_jit_compile :
  interp regex -> (int32[@unboxed]) -> (jit regex, int) Result.t
  = "jit_compile" "jit_compile_unboxed"

external pcre2_jit_match :
  jit regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  (int * int, int) Result.t = "jit_match" "jit_match_unboxed"

external get_version : unit -> int * int = "get_version"

(* TODO: maybe testo dune integration??? *)
(*
module Test = struct
  let ( let* ) x f = match x with Ok x -> f x | Error _ -> false

  let%test "compile basic pattern" =
    match pcre2_compile "abc" 0l with Ok _ -> true | Error _ -> false

  type match_result = (int * int, int) Result.result [@@deriving show]

  let match_expect re subj predicate =
    match pcre2_match re subj 0 0l with
    | x when predicate x -> true
    | x ->
        Format.(fprintf err_formatter "%a\n" pp_match_result x);
        false

  let%test "match basic pattern" =
    match pcre2_compile "abc" 0l with
    | Error e ->
        Format.(printf "failed to compile: %d\n" e);
        false
    | Ok re ->
        match_expect re "abc" (function Ok (0, 3) -> true | _ -> false)
        && match_expect re "123abc456" (function
             | Ok (3, 6) -> true
             | _ -> false)
        && match_expect re "123abc" (function Ok (3, 6) -> true | _ -> false)
        && match_expect re "123ac" (function Error _ -> true | _ -> false)

  let%test "compile basic pattern (jit)" =
    match pcre2_compile "abc" (Int32.of_int 0) with
    | Ok _ -> true
    | Error _ -> false

  let match_expect_jit re subj predicate =
    match pcre2_jit_match re subj 0 0l with
    | x when predicate x -> true
    | x ->
        Format.(fprintf err_formatter "%a\n" pp_match_result x);
        false

  let%test "match basic pattern (jit)" =
    let* re = pcre2_compile "abc" 0l in
    let* re = pcre2_jit_compile re 0l in
    match_expect re "abc" (function Ok (0, 3) -> true | _ -> false)
    && match_expect_jit re "123abc456" (function
         | Ok (3, 6) -> true
         | _ -> false)
    && match_expect_jit re "123abc" (function Ok (3, 6) -> true | _ -> false)
    && match_expect_jit re "123ac" (function Error _ -> true | _ -> false)
end*)
