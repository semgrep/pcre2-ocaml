(** Boundary of the pure-OCaml PCRE2 engine.

    Contracts (see .claude/rules/port-conventions.md §5):
    - [compile] errors are PCRE2 compile error codes (101..199 range).
    - [exec]/[exec_captures] fold NOMATCH (-1) and PARTIAL (-2) to [Ok None];
      any other negative code is [Error code]. Offset validation returns
      BADOFFSET (-33) uniformly. Unknown option bits: 117 at compile,
      -34 at match.
    - Capture group [i] occupies ovector slots [2i, 2i+1]; unset = (-1, -1).
    - [exec_full] is the rich entry point for the pcre2test harness. *)

type t

type exec_result =
  | Match of { ovector : int array; mark : string option; start_char : int }
      (** [ovector] holds pcre2_match's rc pairs (rc = its positive return,
          so [Array.length ovector = 2 * rc]); groups at and above rc are
          unset in the match data — pad with (-1, -1) to reconstruct it.
          [start_char] is pcre2_get_startchar (the attempt start; \K can
          move [ovector.(0)] past it). *)
  | No_match
  | Partial of { start : int; mark : string option }
  | Error of int  (** negative PCRE2 error code *)

val compile : string -> int32 -> (t, int) result

val compile_ctx :
  ?newline:int ->
  ?bsr:int ->
  ?extra:int ->
  string ->
  int32 ->
  (t, int * int) result
(** [compile] plus the pcre2_compile_context knobs the pcre2test harness
    drives: [newline]/[bsr] take PCRE2_NEWLINE_* / PCRE2_BSR_* numeric
    values, [extra] the extra-options word; 0 = build default. On failure
    returns [(errcode, erroroffset)] — the harness prints both. *)

type info = {
  argoptions : int;  (** options as passed to compile (PCRE2_INFO_ARGOPTIONS) *)
  alloptions : int;  (** options after (?...) etc. (PCRE2_INFO_ALLOPTIONS) *)
  newline : int;  (** PCRE2_NEWLINE_* (PCRE2_INFO_NEWLINE) *)
  bsr : int;  (** PCRE2_BSR_* (PCRE2_INFO_BSR) *)
  capture_count : int;  (** highest capture number (PCRE2_INFO_CAPTURECOUNT) *)
}

val info : t -> info
(** The pcre2_pattern_info() subset the pcre2test harness reads. *)

val error_message : int -> string
(** Total variant of [Errors.message] (pcre2_get_error_message): returns
    [""] for codes where the C returns PCRE2_ERROR_BADDATA. *)

val exec_full : t -> string -> int -> int32 -> exec_result
val exec : t -> string -> int -> int32 -> ((int * int) option, int) result

val exec_captures :
  t ->
  string ->
  int ->
  int32 ->
  (((int * int) array * (string * int) array) option, int) result

val capture_groups : t -> (string * int) array
val version : int * int
val print_code : Format.formatter -> t -> unit
