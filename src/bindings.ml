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
  (int, int) Result.t = "pcre2_match_stub" "pcre2_match_stub"

let%test _ = match pcre2_compile "abc" (Int32.of_int 0) with Ok _ -> true | Error _ -> false
