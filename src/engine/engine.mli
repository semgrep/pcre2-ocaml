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
  | No_match
  | Partial of { start : int; mark : string option }
  | Error of int  (** negative PCRE2 error code *)

val compile : string -> int32 -> (t, int) result

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
