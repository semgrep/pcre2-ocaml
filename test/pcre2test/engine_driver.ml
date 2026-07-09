(* Pure-engine driver for the pcre2test-compatible harness: adapts
   Pcre2_engine.Engine (src/engine/) to the DRIVER seam (driver.ml).

   From M1 on the conformance frontier tracks THIS driver; the C oracle
   (oracle/test_driver.ml) remains the fixed reference implementation. *)

module E = Pcre2_engine.Engine

type code = E.t
type compile_error = { errcode : int; erroroffset : int }

let compile ?(options = 0) ?(newline = 0) ?(bsr = 0) ?(extra = 0) pattern :
    (code, compile_error) Result.t =
  (* Option bits cross to the engine boundary as int32 exactly once, per
     port-conventions.md §3 (Int32.of_int keeps the low 32 bits). *)
  match E.compile_ctx ~newline ~bsr ~extra pattern (Int32.of_int options) with
  | Ok c -> Ok c
  | Error (errcode, erroroffset) -> Error { errcode; erroroffset }

type exec_result = {
  rc : int;  (** pcre2_match return: >0 pairs, 0 ovector too small, <0 error *)
  ovector : int array;  (** 2 * ovector-count entries; unset = -1 *)
  mark : string option;
  startchar : int;
}

(* rc mirrors pcre2_match return conventions: the engine seam encodes the
   C's match count (match_data->rc = end_offset_top/2 + 1, which counts
   unset middle groups below the high-water mark) as the LENGTH of the
   Match ovector — 2*rc entries, with everything at and above rc unset in
   the match data. Rebuild the match-data ovector at its
   created-from-pattern size (oveccount = capture_count + 1, exactly the
   oracle stub's pcre2_match_data_create_from_pattern shape) by padding
   with -1. rc = 0 (ovector too small) is never produced at this seam; the
   harness emulates pcre2test's finite match-data ovector on top. *)
let exec ?(options = 0) ~subject ~offset code : exec_result =
  match E.exec_full code subject offset (Int32.of_int options) with
  | E.Match { ovector; mark; start_char } ->
      let oveccount = (E.info code).E.capture_count + 1 in
      let full = Array.make (2 * oveccount) (-1) in
      Array.blit ovector 0 full 0 (Array.length ovector);
      {
        rc = Array.length ovector / 2;
        ovector = full;
        mark;
        startchar = start_char;
      }
  | E.No_match { mark } ->
      (* pcre2_get_mark after a failed match returns mb->nomatch_mark
         (pcre2_match.c:7741) — pcre2test's "No match, mark = X" line. *)
      { rc = Flags.error_nomatch; ovector = [||]; mark; startchar = 0 }
  | E.Partial { start; mark } ->
      (* pcre2_match PARTIAL: ovector[0] = start of the partial match,
         ovector[1] = end of the inspected subject (its length: a partial
         match always runs off the end). *)
      {
        rc = Flags.error_partial;
        ovector = [| start; String.length subject |];
        mark;
        startchar = start;
      }
  | E.Error { code; start_char } ->
      (* pcre2test reads PCRE2_GET_STARTCHAR after a failed match too: the
         UTF error codes print " at offset <startchar>" (harness.ml:741 =
         pcre2test.c's SUBJECT_FAILED output). *)
      { rc = code; ovector = [||]; mark = None; startchar = start_char }

type info = {
  argoptions : int;
  alloptions : int;
  newline : int;
  bsr : int;
  capture_count : int;
}

let info code : info =
  let i = E.info code in
  {
    argoptions = i.E.argoptions;
    alloptions = i.E.alloptions;
    newline = i.E.newline;
    bsr = i.E.bsr;
    capture_count = i.E.capture_count;
  }

let name_table code = E.capture_groups code

(** Exactly pcre2_get_error_message; empty string for unknown codes. *)
let error_message = E.error_message
