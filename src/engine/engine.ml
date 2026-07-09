(* SKELETON (M0): the boundary shape is final; every entry point fails with a
   real PCRE2 code until the M1 core compile/match chunks land. Kept minimal
   deliberately — the milestone docs own the porting order. *)

(* M1 adds the compiled program (OP_* bytecode in Bytes.t, LINK_SIZE=2) and
   study data; the skeleton carries only what the boundary functions touch. *)
type t = {
  name_table : (string * int) array;  (* PCRE2 name-table order *)
  top_bracket : int;  (* capture group count *)
}

type exec_result =
  | Match of { ovector : int array; mark : string option; start_char : int }
  | No_match
  | Partial of { start : int; mark : string option }
  | Error of int

(* pcre2_error.c: ERR23 "internal error: code overflow" — placeholder until
   parse.ml/compile.ml exist; never a legitimate M1+ result for a valid
   pattern. *)
let compile (_pattern : string) (_options : int32) : (t, int) result =
  Error (Errors.compile_error_base + 23)

let exec_full (_re : t) (_subject : string) (_offset : int) (_options : int32) :
    exec_result =
  Error Errors.error_internal

let exec (re : t) (subject : string) (offset : int) (options : int32) :
    ((int * int) option, int) result =
  match exec_full re subject offset options with
  | Match { ovector; _ } -> Ok (Some (ovector.(0), ovector.(1)))
  | No_match | Partial _ -> Ok None (* pcre2_stubs.c: PARTIAL -> Ok None *)
  | Error e -> Error e

let exec_captures (re : t) (subject : string) (offset : int) (options : int32)
    : (((int * int) array * (string * int) array) option, int) result =
  match exec_full re subject offset options with
  | Match { ovector; _ } ->
      let n = re.top_bracket + 1 in
      let pairs =
        Array.init n (fun i ->
            let s = ovector.(2 * i) and e = ovector.((2 * i) + 1) in
            (s, e))
      in
      Ok (Some (pairs, re.name_table))
  | No_match | Partial _ -> Ok None
  | Error e -> Error e

let capture_groups (re : t) = re.name_table

(* Port target is pinned to PCRE2 10.44 (vendor/pcre2/VERSION). *)
let version = (10, 44)

let print_code (_fmt : Format.formatter) (_re : t) : unit =
  (* debug_printer.ml lands with M1 (pcre2_printint.c). *)
  ()
