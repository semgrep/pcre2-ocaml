type jit = [ `JIT ]
type interp = [ `Interp ]
type 'a regex constraint 'a = [< jit | interp ]

let unset = (-1, -1)
(* Since -1 == PCRE2_UNSET.
   TODO: obtain this value as part of the bindings, rather than like so *)

external pcre2_ocaml_init : unit -> unit = "pcre2_ocaml_init"

external pcre2_compile :
  string -> (int32[@unboxed]) -> (interp regex, int) Result.t
  = "compile" "compile_unboxed"

external pcre2_match :
  _ regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  ((int * int) option, int) Result.t = "match" "match_unboxed"

external pcre2_capture :
  _ regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  (((int * int) array * (string * int) array) option, int) Result.t
  = "capture" "capture_unboxed"

external pcre2_jit_compile :
  interp regex -> (int32[@unboxed]) -> (jit regex, int) Result.t
  = "jit_compile" "jit_compile_unboxed"

external pcre2_jit_match :
  jit regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  ((int * int) option, int) Result.t = "jit_match" "jit_match_unboxed"

external pcre2_jit_capture :
  jit regex ->
  string ->
  (int[@untagged]) ->
  (int32[@unboxed]) ->
  (((int * int) array * (string * int) array) option, int) Result.t
  = "jit_capture" "jit_capture_unboxed"

external get_version : unit -> int * int = "get_version"

external get_capture_groups : _ regex -> (string * int) array
  = "get_capture_groups"

external pcre2_to_bytes : _ regex -> bytes = "pcre2_to_bytes"
(** May raise [Failure] if called on a JIT-compiled pattern; PCRE2 cannot
    serialize JIT state. *)

external pcre2_of_bytes : bytes -> (interp regex, int) Result.t
  = "pcre2_of_bytes"
(** Negative integer error codes: -1 = decode failed (corruption,
    cross-version, cross-platform), -2 = empty buffer. *)
