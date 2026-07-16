(* Boundary of the pure-OCaml PCRE2 10.44 engine (see engine.mli).

   The compile side is wired to the real pipeline (Compile.pcre2_compile,
   pcre2_compile.c:10096-10993); [t] is the compiled pattern record. The
   match side is wired to the real driver (Interpreter.pcre2_match,
   pcre2_match.c:6530-7777) via [exec_full]. *)

type t = Compile.re

type exec_result =
  | Match of { ovector : int array; mark : string option; start_char : int }
  | No_match of { mark : string option }
  | Partial of { start : int; mark : string option }
  | Error of { code : int; start_char : int }

(* pcre2_compile.c:10096-10993 via Compile.pcre2_compile. Option bits cross
   the boundary as int32 and are widened exactly once here
   (port-conventions §3); unknown bits yield error 117 inside the driver
   (§5). On failure returns [(errcode, erroroffset)] like [compile_ctx];
   callers surface the offset (pcre2_compile's &erroroffset out-parameter). *)
let compile (pattern : string) (options : int32) : (t, int * int) result =
  Compile.pcre2_compile pattern ~options:(Options.of_int32 options)

(* [compile] plus the pcre2_compile_context knobs the pcre2test harness
   drives (pcre2test.c: pcre2_set_newline / pcre2_set_bsr /
   pcre2_set_compile_extra_options are called only when the corresponding
   modifier is present; 0 = leave the build default). Invalid newline
   values surface as the driver's ERR56, exactly as a hand-built C context
   would (pcre2_compile.c:10467-10469). *)
let compile_ctx ?(newline = 0) ?(bsr = 0) ?(extra = 0) (pattern : string)
    (options : int32) : (t, int * int) result =
  let dflt = Compile.default_compile_context in
  let ccontext =
    {
      dflt with
      Compile.newline_convention =
        (if Int.equal newline 0 then dflt.Compile.newline_convention
         else newline);
      bsr_convention =
        (if Int.equal bsr 0 then dflt.Compile.bsr_convention else bsr);
      extra_options = extra;
    }
  in
  Compile.pcre2_compile ~ccontext pattern ~options:(Options.of_int32 options)

type info = {
  argoptions : int;
  alloptions : int;
  newline : int;
  bsr : int;
  capture_count : int;
}

(* The pcre2_pattern_info() subset the pcre2test harness reads
   (pcre2_pattern_info.c:65-176): ARGOPTIONS = re->compile_options,
   ALLOPTIONS = re->overall_options, NEWLINE/BSR = the stored conventions,
   CAPTURECOUNT = re->top_bracket. *)
let info (re : t) : info =
  {
    argoptions = re.Compile.compile_options;
    alloptions = re.Compile.overall_options;
    newline = re.Compile.newline_convention;
    bsr = re.Compile.bsr_convention;
    capture_count = re.Compile.top_bracket;
  }

(* Total wrapper over Errors.message: where the C pcre2_get_error_message
   returns PCRE2_ERROR_BADDATA (unknown / 0..99 codes), the driver seam wants
   the empty string (mirrors oracle/pcre2test_stubs.c oracle_test_error_message). *)
let error_message (code : int) : string =
  match Errors.message code with s -> s | exception Invalid_argument _ -> ""

(* Decode the compiled name/number table (pcre2_intmodedep.h layout, filled
   by add_name_to_table, pcre2_compile.c:9162-9219): name_count entries of
   name_entry_size code units — a GET2 group number, then the
   NUL-terminated name — returned as (name, group_number) pairs in PCRE2
   name-table order (port-conventions §5). *)
let capture_groups (re : t) : (string * int) array =
  Array.init re.Compile.name_count (fun i ->
      let base = i * re.Compile.name_entry_size in
      let number = Compile.get2 re.Compile.name_table base in
      let start = base + Limits.imm2_size in
      let len = ref 0 in
      while
        not (Char.equal (Bytes.get re.Compile.name_table (start + !len)) '\000')
      do
        incr len
      done;
      (Bytes.sub_string re.Compile.name_table start !len, number))

(* mb->mark / mb->nomatch_mark reach the match data as pointers to the
   mark name stored inline in the compiled code: a verb name is emitted
   as a length code unit, the name, and a terminating zero
   (pcre2_compile.c:6540-6573), and the OP_MARK arm points Fmark past the
   length byte (pcre2_match.c:6341, Fecode + 2). The name length is the
   code unit BEFORE the name (mark[-1]) — NOT a NUL scan: verb names can
   contain binary zeros (pcre2test.c's PCHARSV(mark, -1, ...) reads the
   preceding length unit, as does oracle/pcre2test_stubs.c:117). -1 =
   NULL -> None. *)
let mark_of_offset (re : t) (off : int) : string option =
  if off < 0 then None
  else
    let len = Char.code (Bytes.get re.Compile.code (off - 1)) in
    Some (Bytes.sub_string re.Compile.code off len)

(* pcre2_match.c:6530-7777 via Interpreter.pcre2_match, with the match
   data created from the pattern (pcre2_match_data_create_from_pattern:
   oveccount = re->top_bracket + 1 — the shape the conformance oracle stub
   uses) and its ovector preset to PCRE2_UNSET.

   Result mapping (port-conventions §5):
   - rc > 0 -> [Match]. DEVIATION (seam encoding): [exec_result] carries
     no rc field, so the C's pair count travels as the ovector LENGTH —
     the returned array is the match-data ovector truncated to rc pairs.
     Nothing is lost: the C driver leaves every slot >= 2*rc unset
     (ovector copy-out + tail fill, pcre2_match.c:934-939, and
     rc = end_offset_top/2 + 1 at 7716-7717), so callers reconstruct the
     full match data by padding with (-1, -1). The clip-to-0 case
     (end_offset_top >= 2*oveccount) cannot occur here because
     end_offset_top <= 2*top_bracket < 2*oveccount.
   - -1 (NOMATCH) -> [No_match] carrying the nomatch mark (the driver
     stored mb->nomatch_mark in match_data->mark, pcre2_match.c:7741);
     -2 (PARTIAL) -> [Partial] with startchar = the partial start
     (= ovector[0], pcre2_match.c:7756-7758); any other negative code ->
     [Error] carrying pcre2_get_startchar (the subject UTF-error offset
     for the UTF error codes, pcre2_match.c:6899-6903; pcre2test prints
     it as " at offset N"). *)
let exec_full ?(match_limit = Limits.match_limit)
    ?(depth_limit = Limits.match_limit_depth) ?(heap_limit = Limits.heap_limit)
    (re : t) (subject : string) (offset : int) (options : int32) : exec_result =
  let oveccount = re.Compile.top_bracket + 1 in
  let md =
    {
      Interpreter.ovector = Array.make (2 * oveccount) (-1);
      oveccount;
      rc = 0;
      startchar = 0;
      leftchar = 0;
      rightchar = 0;
      mark = -1;
    }
  in
  (* This is the seam that stands in for "no pcre2_match_context supplied":
     an omitted limit arg defaults to the build value (module Limits = the C
     default match context, pcre2_context.c:166-179), so the driver behaves
     bit-for-bit like the pre-knob path. The worker masks (uint32) and does
     the min-with-verb resolution; call the worker (not the optional
     [pcre2_match] wrapper) to stay allocation-free (interpreter.ml). *)
  let rc =
    Interpreter.pcre2_match_with_limits re ~match_limit ~depth_limit ~heap_limit
      ~subject ~start_offset:offset ~options:(Options.of_int32 options) md
  in
  if rc > 0 then
    Match
      {
        ovector = Array.sub md.Interpreter.ovector 0 (2 * rc);
        mark = mark_of_offset re md.Interpreter.mark;
        start_char = md.Interpreter.startchar;
      }
  else if Int.equal rc Errors.error_nomatch then
    No_match { mark = mark_of_offset re md.Interpreter.mark }
  else if Int.equal rc Errors.error_partial then
    Partial
      {
        start = md.Interpreter.startchar;
        mark = mark_of_offset re md.Interpreter.mark;
      }
  else Error { code = rc; start_char = md.Interpreter.startchar }

(* Internal fast path for the (start, end)-only seam (the public type is
   unchanged — engine.mli): run the match exactly like [exec_full] (same
   md shape, same Interpreter.pcre2_match call) but read the match-data
   ovector directly, skipping the [Match] record, the Array.sub rc-clip
   and the mark decode that [exec_full] builds for captures/marks
   consumers. Result mapping identical to going through [exec_full]
   (port-conventions §5): rc > 0 -> the pair (the clip kept >= 2 slots
   when rc >= 1, so ovector.(0)/(1) are the same values); NOMATCH /
   PARTIAL -> Ok None (pcre2_stubs.c: PARTIAL -> Ok None); any other rc
   (including the unreachable 0, see [exec_full]) -> Error rc.

   DEVIATION (alloc pin, port-conventions §8): [exec] does NOT take the
   per-call limit args that [exec_full]/[exec_captures] gained. Optional
   arguments on this function cost ~36 extra minor words/exec at every call
   site under the stock (non-flambda) compiler (measured: 28 -> 64
   words/exec), which would regress the frozen engine-seam alloc pin
   (test/pcre2_tests.ml:648, < 40 words at Engine.exec) — a real
   default-path allocation regression, not just a test artifact. So [exec]
   stays 4-arg and always uses the build-default limits; the limit-carrying
   pair/bool path (bindings.ml, a later chunk) routes through [exec_full]
   when a per-call limit is supplied and through [exec] otherwise. *)
let exec (re : t) (subject : string) (offset : int) (options : int32) :
    ((int * int) option, int) result =
  let oveccount = re.Compile.top_bracket + 1 in
  let md =
    {
      Interpreter.ovector = Array.make (2 * oveccount) (-1);
      oveccount;
      rc = 0;
      startchar = 0;
      leftchar = 0;
      rightchar = 0;
      mark = -1;
    }
  in
  let rc =
    Interpreter.pcre2_match_with_limits re ~match_limit:Limits.match_limit
      ~depth_limit:Limits.match_limit_depth ~heap_limit:Limits.heap_limit
      ~subject ~start_offset:offset ~options:(Options.of_int32 options) md
  in
  if rc > 0 then
    Ok (Some (md.Interpreter.ovector.(0), md.Interpreter.ovector.(1)))
  else if Int.equal rc Errors.error_nomatch || Int.equal rc Errors.error_partial
  then Ok None
  else Result.Error rc

let exec_captures ?match_limit ?depth_limit ?heap_limit (re : t)
    (subject : string) (offset : int) (options : int32) :
    (((int * int) array * (string * int) array) option, int) result =
  match exec_full ?match_limit ?depth_limit ?heap_limit re subject offset options
  with
  | Match { ovector; _ } ->
      let n = re.Compile.top_bracket + 1 in
      (* The Match ovector holds rc pairs (see exec_full); groups at and
         above rc are unset in the C match data — pad with (-1, -1). *)
      let len = Array.length ovector in
      let pairs =
        Array.init n (fun i ->
            if (2 * i) + 1 < len then (ovector.(2 * i), ovector.((2 * i) + 1))
            else (-1, -1))
      in
      Ok (Some (pairs, capture_groups re))
  | No_match _ | Partial _ -> Ok None
  | Error { code; _ } -> Result.Error code

(* Port target is pinned to PCRE2 10.44 (vendor/pcre2/VERSION). *)
let version = (10, 44)

(* pcre2_printint.c:337-884 via Debug_printer.pcre2_printint. pcre2test's
   fullbincode/debug modifier calls pcre2_printint with print_lengths =
   TRUE (pcre2test.c:4560-4564, CTL_FULLBINCODE); the opening rule of
   dashes is pcre2test's own, not part of the dump. The dump is built in a
   Buffer and emitted verbatim (Format must not reflow it). *)
let print_code (fmt : Format.formatter) (re : t) : unit =
  let buf = Buffer.create 256 in
  Debug_printer.pcre2_printint buf re ~print_lengths:true;
  Format.pp_print_string fmt (Buffer.contents buf)
