(* C-oracle driver for the pcre2test-compatible harness.

   This is the reference implementation the harness is validated against at
   gate G0; the pure-OCaml engine provides a structurally identical module
   from M1 on. All ints are plain OCaml ints (option bits fit in 63 bits). *)

type code
type compile_error = { errcode : int; erroroffset : int }

external compile_raw :
  string -> int -> int -> int -> int -> (code, int * int) Result.t
  = "oracle_test_compile"

(** [compile ~options ~newline ~bsr ~extra pattern]. [newline]/[bsr] use
    PCRE2_NEWLINE_* / PCRE2_BSR_* numeric values, [0] meaning build default. *)
let compile ?(options = 0) ?(newline = 0) ?(bsr = 0) ?(extra = 0) pattern :
    (code, compile_error) Result.t =
  match compile_raw pattern options newline bsr extra with
  | Ok c -> Ok c
  | Error (errcode, erroroffset) -> Error { errcode; erroroffset }

type exec_result = {
  rc : int;  (** pcre2_match return: >0 pairs, 0 ovector too small, <0 error *)
  ovector : int array;  (** 2 * ovector-count entries; unset = -1 *)
  mark : string option;
  startchar : int;
}

(* 7 args (>5) => bytecode/native external pair. The three trailing limits are
   the pcre2_match_context knobs (pcre2test MOD_CTM modifiers); -1 = unset, in
   which case the stub passes a NULL match context (build default), exactly as
   before this arg existed. *)
external exec_raw :
  code ->
  string ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int * int array * string option * int = "oracle_test_exec_byte"
    "oracle_test_exec"

let exec ?(options = 0) ?match_limit ?depth_limit ?heap_limit ~subject ~offset
    code : exec_result =
  let lim = function None -> -1 | Some v -> v in
  let rc, ovector, mark, startchar =
    exec_raw code subject offset options (lim match_limit) (lim depth_limit)
      (lim heap_limit)
  in
  { rc; ovector; mark; startchar }

type info = {
  argoptions : int;
  alloptions : int;
  newline : int;
  bsr : int;
  capture_count : int;
}

external info_raw : code -> int * int * int * int * int = "oracle_test_info"

let info code : info =
  let argoptions, alloptions, newline, bsr, capture_count = info_raw code in
  { argoptions; alloptions; newline; bsr; capture_count }

external name_table : code -> (string * int) array = "oracle_test_name_table"

external error_message : int -> string = "oracle_test_error_message"
(** Exactly [pcre2_get_error_message]; empty string for unknown codes. *)

(* The C oracle never declines a pattern (test/pcre2test/driver.ml seam doc). *)
let unsupported_of_error (_ : compile_error) : string option = None
