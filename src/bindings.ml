type jit = [ `JIT ]
type interp = [ `Interp ]
type 'a regex constraint 'a = [< jit | interp ]

external pcre2_ocaml_init : unit -> unit = "pcre2_ocaml_init"

external pcre2_compile :
  string -> (int32[@unboxed]) -> (interp regex, int) Result.t
  = "pcre2_compile_stub" "pcre2_compile_stub"

external pcre2_match :
  _ regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  (int * int, int) Result.t = "pcre2_match_stub" "pcre2_match_stub"

external get_version : unit -> int * int = "version_stub"

(* TODO: maybe testo dune integration??? *)
module Test = struct
  let ( let* ) x f = match x with Ok x -> f x | Error _ -> false

  let%test "compile basic pattern" =
    match pcre2_compile "abc" (Int32.of_int 0) with
    | Ok _ -> true
    | Error _ -> false

  type match_result = (int * int, int) Result.result [@@deriving show]

  let match_expect re subj predicate =
    match pcre2_match re subj 0 0l with
    | x when predicate x -> true
    | x ->
        Format.(fprintf err_formatter "%a\n" pp_match_result x);
        false

  let%test "match basic pattern" =
    let* re = pcre2_compile "abc" 0l in
    match_expect    re "abc"        (function Ok (0, 3) -> true | _ -> false)
    && match_expect re "123abc456"  (function Ok (3, 6) -> true | _ -> false)
    && match_expect re "123abc"     (function Ok (3, 6) -> true | _ -> false)
    && match_expect re "123ac"      (function Error _   -> true | _ -> false)
end
