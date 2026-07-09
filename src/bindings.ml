(* Pure-OCaml implementation of the former C-stub boundary (pcre2_stubs.c).
   Names and types are frozen: src/pcre2.ml{,i} must never need a diff.
   The C originals live on as the dev-only oracle (oracle/, package pcre2-dev).

   The jit/interp phantom mirrors the C library, where pcre2_jit_compile
   augmented the same pcre2_code value: both tags wrap the same engine type,
   and the "JIT" paths are aliases of the interpreter (strictly safer: the C
   JIT skipped UTF validation; the pure engine never does). *)

type jit = [ `JIT ]
type interp = [ `Interp ]
type 'a regex = Pcre2_engine.Engine.t constraint 'a = [< jit | interp ]

let unset = (-1, -1)
(* Since -1 == PCRE2_UNSET (as exposed through the former stubs). *)

let pcre2_ocaml_init : unit -> unit = fun () -> ()

let pcre2_compile (pattern : string) (options : int32) :
    (interp regex, int) Result.t =
  Pcre2_engine.Engine.compile pattern options

let pcre2_match (re : _ regex) (subject : string) (offset : int)
    (options : int32) : ((int * int) option, int) Result.t =
  Pcre2_engine.Engine.exec re subject offset options

let pcre2_capture (re : _ regex) (subject : string) (offset : int)
    (options : int32) :
    (((int * int) array * (string * int) array) option, int) Result.t =
  Pcre2_engine.Engine.exec_captures re subject offset options

(* Re-tags the same compiled value; always Ok (port-conventions.md §5). *)
let pcre2_jit_compile (re : interp regex) (_options : int32) :
    (jit regex, int) Result.t =
  Ok re

let pcre2_jit_match (re : jit regex) (subject : string) (offset : int)
    (options : int32) : ((int * int) option, int) Result.t =
  pcre2_match re subject offset options

let pcre2_jit_capture (re : jit regex) (subject : string) (offset : int)
    (options : int32) :
    (((int * int) array * (string * int) array) option, int) Result.t =
  pcre2_capture re subject offset options

let get_version () : int * int = Pcre2_engine.Engine.version

let get_capture_groups (re : _ regex) : (string * int) array =
  Pcre2_engine.Engine.capture_groups re
