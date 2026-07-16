(* The driver seam: everything the harness needs from a PCRE2 implementation.

   The C-oracle implementation is Pcre2_test_driver.Test_driver (oracle/); the
   pure-OCaml engine provides a structurally identical module from M1 on. *)

module type S = sig
  type code
  type compile_error = { errcode : int; erroroffset : int }

  val compile :
    ?options:int ->
    ?newline:int ->
    ?bsr:int ->
    ?extra:int ->
    string ->
    (code, compile_error) Result.t
  (** [newline]/[bsr] take PCRE2_NEWLINE_* / PCRE2_BSR_* values, 0 = build
      default. *)

  type exec_result = {
    rc : int;
        (** pcre2_match return: >0 pairs, 0 ovector too small, <0 error *)
    ovector : int array;  (** 2 * ovector-count entries; unset = -1 *)
    mark : string option;
    startchar : int;
  }

  val exec :
    ?options:int ->
    ?match_limit:int ->
    ?depth_limit:int ->
    ?heap_limit:int ->
    subject:string ->
    offset:int ->
    code ->
    exec_result
  (** [match_limit]/[depth_limit]/[heap_limit] are the pcre2_match_context
      limit knobs (pcre2test's MOD_CTM modifiers, pcre2_set_*_limit). [None]
      means the build default (no context / NULL mcontext, exactly C's
      default). *)

  type info = {
    argoptions : int;
    alloptions : int;
    newline : int;
    bsr : int;
    capture_count : int;
  }

  val info : code -> info
  val name_table : code -> (string * int) array

  val error_message : int -> string
  (** Exactly pcre2_get_error_message; empty string for unknown codes. *)

  val unsupported_of_error : compile_error -> string option
  (** Fast-engine drivers: [Some reason] when this compile "error" means the
      engine DECLINES the pattern (no-fallback contract, fast-design.md §1) —
      the harness records the unit as an "unsupported:<reason>" skip instead
      of emitting a [Failed:] line. Real PCRE2 compile errors (and the
      oracle/engine drivers always): [None]. *)
end
