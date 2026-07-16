(* Fast-engine driver for the pcre2test-compatible harness: adapts
   Pcre2_fast (src/fast/) to the DRIVER seam (driver.ml).

   No-fallback contract (fast-design.md §1): [Pcre2_fast.compile] declines
   patterns it cannot execute with [Unsupported]. At this seam — where
   [compile_error] is the pcre2test errcode/erroroffset pair — that is
   encoded as the sentinel [unsupported_errcode] (outside PCRE2's compile
   error range 101-199) plus a reason cell that [unsupported_of_error]
   reads back; the harness turns it into an "unsupported:<reason>" skip
   (conformance counts it as skipped, never passed or failed). Real PCRE2
   compile errors pass through with exact number/offset parity, so
   compile-error units are judged byte-for-byte like the engine driver. *)

module F = Pcre2_fast

type code = F.t
type compile_error = { errcode : int; erroroffset : int }

(* Sentinel: outside both PCRE2 compile errors (101-199) and BAD_OPTIONS
   territory; only meaningful to [unsupported_of_error] below. *)
let unsupported_errcode = 998
let last_unsupported : string option ref = ref None

let compile ?(options = 0) ?(newline = 0) ?(bsr = 0) ?(extra = 0) pattern :
    (code, compile_error) Result.t =
  match F.compile_ctx ~newline ~bsr ~extra pattern (Int32.of_int options) with
  | Ok c ->
      last_unsupported := None;
      Ok c
  | Error (F.Compile_error { errcode; erroroffset }) ->
      last_unsupported := None;
      Error { errcode; erroroffset }
  | Error (F.Unsupported reason) ->
      last_unsupported := Some reason;
      Error { errcode = unsupported_errcode; erroroffset = 0 }

let unsupported_of_error (e : compile_error) : string option =
  if Int.equal e.errcode unsupported_errcode then !last_unsupported else None

type exec_result = {
  rc : int;  (** pcre2_match return: >0 pairs, 0 ovector too small, <0 error *)
  ovector : int array;  (** 2 * ovector-count entries; unset = -1 *)
  mark : string option;
  startchar : int;
}

(* Identical shape to engine_driver.exec (same seam semantics; see the rc
   comment there). [F.t] is uninhabited until chunk C, so this body is
   compiler-checked dead code until the IR runner lands. *)
let exec ?(options = 0) ~subject ~offset code : exec_result =
  match F.exec_full code subject offset (Int32.of_int options) with
  | Pcre2_engine.Engine.Match { ovector; mark; start_char } ->
      let oveccount = (F.info code).Pcre2_engine.Engine.capture_count + 1 in
      let full = Array.make (2 * oveccount) (-1) in
      Array.blit ovector 0 full 0 (Array.length ovector);
      {
        rc = Array.length ovector / 2;
        ovector = full;
        mark;
        startchar = start_char;
      }
  | Pcre2_engine.Engine.No_match { mark } ->
      { rc = Flags.error_nomatch; ovector = [||]; mark; startchar = 0 }
  | Pcre2_engine.Engine.Partial { start; mark } ->
      {
        rc = Flags.error_partial;
        ovector = [| start; String.length subject |];
        mark;
        startchar = start;
      }
  | Pcre2_engine.Engine.Error { code; start_char } ->
      { rc = code; ovector = [||]; mark = None; startchar = start_char }

type info = {
  argoptions : int;
  alloptions : int;
  newline : int;
  bsr : int;
  capture_count : int;
}

let info code : info =
  let i = F.info code in
  {
    argoptions = i.Pcre2_engine.Engine.argoptions;
    alloptions = i.Pcre2_engine.Engine.alloptions;
    newline = i.Pcre2_engine.Engine.newline;
    bsr = i.Pcre2_engine.Engine.bsr;
    capture_count = i.Pcre2_engine.Engine.capture_count;
  }

let name_table code = F.capture_groups code

(** Exactly pcre2_get_error_message; empty string for unknown codes. *)
let error_message = F.error_message
