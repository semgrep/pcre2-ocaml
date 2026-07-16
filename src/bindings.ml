(* Pure-OCaml implementation of the former C-stub boundary (pcre2_stubs.c).
   Names and types track the boundary the Pcre2 module compiles against; the
   compile path carries pcre2_compile's &erroroffset out-parameter alongside
   the error code so callers can report the failing position, like the C stub.
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
    (interp regex, int * int) Result.t =
  Pcre2_engine.Engine.compile pattern options

(* The three optional args are the per-call match-context limit knobs
   (pcre2_set_match_limit / pcre2_set_depth_limit / pcre2_set_heap_limit);
   omitting all three reproduces "no pcre2_match_context supplied" (build
   defaults). When all three are None we call [Engine.exec] — the 4-arg,
   alloc-pinned fast path (engine.ml DEVIATION: [exec] deliberately does NOT
   carry the optional-limit prefix, to protect the frozen engine-seam alloc
   pin). When any is Some, route through [Engine.exec_full] (which does carry
   the knobs) and project its variant result back to [exec]'s exact contract:
   rc>0 -> the group-0 pair; No_match/Partial -> Ok None; Error -> Error rc. *)
let pcre2_match ?match_limit ?depth_limit ?heap_limit (re : _ regex)
    (subject : string) (offset : int) (options : int32) :
    ((int * int) option, int) Result.t =
  match (match_limit, depth_limit, heap_limit) with
  | None, None, None -> Pcre2_engine.Engine.exec re subject offset options
  | _ -> (
      let open Pcre2_engine.Engine in
      match
        exec_full ?match_limit ?depth_limit ?heap_limit re subject offset
          options
      with
      | Match { ovector; _ } -> Ok (Some (ovector.(0), ovector.(1)))
      | No_match _ | Partial _ -> Ok None
      | Error { code; _ } -> Result.Error code)

let pcre2_capture ?match_limit ?depth_limit ?heap_limit (re : _ regex)
    (subject : string) (offset : int) (options : int32) :
    (((int * int) array * (string * int) array) option, int) Result.t =
  Pcre2_engine.Engine.exec_captures ?match_limit ?depth_limit ?heap_limit re
    subject offset options

(* Re-tags the same compiled value; always Ok (port-conventions.md §5). *)
let pcre2_jit_compile (re : interp regex) (_options : int32) :
    (jit regex, int) Result.t =
  Ok re

(* The JIT-tagged aliases honor all three limit knobs, matching the interp
   paths. This is a strictly-safer DEVIATION from the real C JIT, whose
   jit_arguments carries limit_match but neither depth nor heap limit
   (pcre2_jit_compile.c:183-199) — it
   ignores depth/heap at match time; here they are enforced (port-conventions
   §5 strictly-safer-alias contract). *)
let pcre2_jit_match ?match_limit ?depth_limit ?heap_limit (re : jit regex)
    (subject : string) (offset : int) (options : int32) :
    ((int * int) option, int) Result.t =
  pcre2_match ?match_limit ?depth_limit ?heap_limit re subject offset options

let pcre2_jit_capture ?match_limit ?depth_limit ?heap_limit (re : jit regex)
    (subject : string) (offset : int) (options : int32) :
    (((int * int) array * (string * int) array) option, int) Result.t =
  pcre2_capture ?match_limit ?depth_limit ?heap_limit re subject offset options

let get_version () : int * int = Pcre2_engine.Engine.version

let get_capture_groups (re : _ regex) : (string * int) array =
  Pcre2_engine.Engine.capture_groups re
