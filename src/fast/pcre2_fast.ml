(* Fast engine seam — chunk B skeleton (fast-design.md par. 1).

   Compilation runs the SHARED compiler (Pcre2_engine.Engine.compile_ctx →
   Pcre2_engine.Compile), so real PCRE2 compile errors surface with exact
   number/offset parity from day one. The IR compiler does not exist yet
   (chunk C), so every successfully compiled pattern is rejected with
   [Unsupported] — NEVER executed via the interpreter (no-fallback
   contract). [t] is consequently uninhabited in this chunk; the match-side
   functions are refutation cases the type checker proves unreachable, and
   they gain bodies when chunk C introduces the IR record. *)

module E = Pcre2_engine.Engine

(** {1 Unstable internals — exposed for tests only (M11 chunk C1)}

    [Ir], [Ir_compile] and [Ir_verify] are the fast engine's IR, its compiler
    and its static verifier. They are NOT part of the public seam and may
    change without notice; only [test/fast] should reference them. The seam's
    public functions/types below are unchanged (chunk C1 adds no runner, so
    [compile] still yields [Unsupported] for every pattern). *)

module Ir = Ir
module Ir_compile = Ir_compile
module Ir_verify = Ir_verify

type t = |

type compile_error =
  | Compile_error of { errcode : int; erroroffset : int }
  | Unsupported of string

let compile_ctx ?(newline = 0) ?(bsr = 0) ?(extra = 0) (pattern : string)
    (options : int32) : (t, compile_error) result =
  match E.compile_ctx ~newline ~bsr ~extra pattern options with
  | Error (errcode, erroroffset) -> Error (Compile_error { errcode; erroroffset })
  | Ok _re ->
      Error
        (Unsupported "fast: IR compiler not yet implemented (chunk C, 11-fast-engine.md)")

let compile (pattern : string) (options : int32) : (t, compile_error) result =
  compile_ctx pattern options

(* [t] is uninhabited until chunk C: these are compiler-checked dead code. *)
let exec (re : t) (_subject : string) (_offset : int) (_options : int32) :
    ((int * int) option, int) result =
  match re with _ -> .

let exec_full (re : t) (_subject : string) (_offset : int) (_options : int32) :
    E.exec_result =
  match re with _ -> .

let exec_captures (re : t) (_subject : string) (_offset : int)
    (_options : int32) :
    (((int * int) array * (string * int) array) option, int) result =
  match re with _ -> .

let capture_groups (re : t) : (string * int) array = match re with _ -> .
let info (re : t) : E.info = match re with _ -> .
let error_message = E.error_message
let version = E.version

(* The frozen Matcher convenience surface, assembled from the shared
   pcre2.matcher pieces exactly like Pcre2.Interp/Jit do (src/pcre2.ml),
   with the raw seam functions above in place of Bindings. *)
type fast_compile_error = compile_error =
  | Compile_error of { errcode : int; erroroffset : int }
  | Unsupported of string

module Matcher = struct
  include Pcre2_matcher.Options.Interp
  include Pcre2_matcher.Match
  include Pcre2_matcher.Error

  type nonrec t = t

  (* Shadows the frozen [Pcre2_matcher.Error.compile_error] brought in by the
     include above: Fast's compile can also decline a valid pattern
     ([Unsupported]), which the frozen variant cannot express. Hand-written
     show/pp/equal keep pcre2.fast ppx-free. *)
  type compile_error = fast_compile_error =
    | Compile_error of { errcode : int; erroroffset : int }
    | Unsupported of string

  let pp_compile_error fmt = function
    | Compile_error { errcode; erroroffset } ->
        Format.fprintf fmt "Pcre2_fast.Compile_error { errcode = %d; erroroffset = %d }"
          errcode erroroffset
    | Unsupported reason -> Format.fprintf fmt "Pcre2_fast.Unsupported %S" reason

  let show_compile_error e = Format.asprintf "%a" pp_compile_error e

  let equal_compile_error a b =
    match (a, b) with
    | Compile_error x, Compile_error y ->
        Int.equal x.errcode y.errcode && Int.equal x.erroroffset y.erroroffset
    | Unsupported x, Unsupported y -> String.equal x y
    | Compile_error _, Unsupported _ | Unsupported _, Compile_error _ -> false

  let compile ?(options : compile_option list = []) (pattern : string) :
      (t, compile_error) Result.t =
    compile pattern (bitvector_of_compile_options options)

  let capture_groups (r : t) = capture_groups r |> Array.to_list

  include Pcre2_matcher.Convenience.MakeMatcher (struct
    type nonrec t = t
    type nonrec match_option = match_option

    let bitvector_of_match_options = bitvector_of_match_options
    let match_raw = exec
    let capture_raw = exec_captures
  end)

  include Pcre2_matcher.Convenience.MakeConvenience (struct
    type nonrec t = t
    type nonrec match_option = match_option

    let bitvector_of_match_options = bitvector_of_match_options
    let capture_raw = exec_captures
  end)
end
