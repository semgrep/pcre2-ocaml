(* Public seam of the fast engine (M11, docs/ocaml-engine/11-fast-engine.md;
   design: docs/ocaml-engine/fast-design.md). Mirrors src/engine/engine.mli,
   as a SEPARATE interface from the frozen [Pcre2] API.

   NO FALLBACK (fast-design.md par. 1): [compile] rejects patterns the fast
   engine cannot yet execute with [Unsupported]; it NEVER routes a match
   through the interpreter. The caller picks the engine. On patterns it
   accepts, observable behavior is identical to [Pcre2_engine.Engine]
   (same rc/ovector/mark/startchar, same limit trip points). *)

(** {1 Unstable internals — exposed for tests only (M11 chunk C1)}

    [Ir], [Ir_compile] and [Ir_verify] are the fast engine's IR, its compiler
    and its static verifier, surfaced so [test/fast] can drive them directly
    (the wrapped library otherwise hides them behind this entry module). They
    are NOT part of the stable seam and may change without notice. *)

module Ir = Ir

module Ir_compile : sig
  val compile : Pcre2_engine.Compile.re -> (Ir.t, string) result
end

module Ir_verify : sig
  val check : Ir.t -> (unit, string) result
end

type t
(** A pattern compiled for the fast engine. *)

type compile_error =
  | Compile_error of { errcode : int; erroroffset : int }
      (** A real PCRE2 compile error — exact number/offset parity with the
          engine (the shared compiler, [Pcre2_engine.Compile], produces
          both). *)
  | Unsupported of string
      (** The pattern compiled, but uses a construct the fast engine does
          not support yet (see the 11-fast-engine.md chunk checklist; the
          string names the construct). *)

val compile : string -> int32 -> (t, compile_error) result

val compile_ctx :
  ?newline:int ->
  ?bsr:int ->
  ?extra:int ->
  string ->
  int32 ->
  (t, compile_error) result
(** [newline]/[bsr] take PCRE2_NEWLINE_* / PCRE2_BSR_* values, 0 = build
    default — the same knobs as [Pcre2_engine.Engine.compile_ctx]. *)

val exec : t -> string -> int -> int32 -> ((int * int) option, int) result

val exec_full : t -> string -> int -> int32 -> Pcre2_engine.Engine.exec_result
(** Same result type as the engine seam so differential harnesses compare
    the two engines directly. *)

val exec_captures :
  t ->
  string ->
  int ->
  int32 ->
  (((int * int) array * (string * int) array) option, int) result

val capture_groups : t -> (string * int) array
val info : t -> Pcre2_engine.Engine.info

val error_message : int -> string
(** Exactly pcre2_get_error_message (delegates to the engine's table). *)

val version : int * int

(** The full frozen [Pcre2.Matcher] convenience surface over the fast
    engine, via the shared pcre2.matcher functors (find/captures/split and
    the 7.5.3 layer; find_iter/captures_iter empty-match semantics FROZEN,
    port-conventions.md par. 5). [compile] surfaces [Unsupported] through
    [compile_error] below. *)
module Matcher :
  Pcre2_matcher.Intf.Matcher
    with type t = t
     and type compile_error = compile_error
