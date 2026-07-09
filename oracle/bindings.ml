type jit = [ `JIT ]
type interp = [ `Interp ]
type 'a regex constraint 'a = [< jit | interp ]

let unset = (-1, -1)
(* Since -1 == PCRE2_UNSET.
   TODO: obtain this value as part of the bindings, rather than like so *)

external pcre2_ocaml_init : unit -> unit = "oracle_pcre2_ocaml_init"

external pcre2_compile :
  string -> (int32[@unboxed]) -> (interp regex, int) Result.t
  = "oracle_compile" "oracle_compile_unboxed"

external pcre2_match :
  _ regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  ((int * int) option, int) Result.t = "oracle_match" "oracle_match_unboxed"

external pcre2_capture :
  _ regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  (((int * int) array * (string * int) array) option, int) Result.t
  = "oracle_capture" "oracle_capture_unboxed"

external pcre2_jit_compile :
  interp regex -> (int32[@unboxed]) -> (jit regex, int) Result.t
  = "oracle_jit_compile" "oracle_jit_compile_unboxed"

external pcre2_jit_match :
  jit regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  ((int * int) option, int) Result.t
  = "oracle_jit_match" "oracle_jit_match_unboxed"

external pcre2_jit_capture :
  jit regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  (((int * int) array * (string * int) array) option, int) Result.t
  = "oracle_jit_capture" "oracle_jit_capture_unboxed"

external get_version : unit -> int * int = "oracle_get_version"

external get_capture_groups : _ regex -> (string * int) array
  = "oracle_get_capture_groups"
