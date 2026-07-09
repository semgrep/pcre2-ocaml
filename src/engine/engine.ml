(* Boundary of the pure-OCaml PCRE2 10.44 engine (see engine.mli).

   The compile side is wired to the real pipeline (Compile.pcre2_compile,
   pcre2_compile.c:10096-10993); [t] is the compiled pattern record. The
   match side is wired to the real driver (Interpreter.pcre2_match,
   pcre2_match.c:6530-7777) via [exec_full]. *)

type t = Compile.re

type exec_result =
  | Match of { ovector : int array; mark : string option; start_char : int }
  | No_match
  | Partial of { start : int; mark : string option }
  | Error of int

(* pcre2_compile.c:10096-10993 via Compile.pcre2_compile. Option bits cross
   the boundary as int32 and are widened exactly once here
   (port-conventions §3); unknown bits yield error 117 inside the driver
   (§5). This entry point drops the error offset (pcre2_stubs.c shape). *)
let compile (pattern : string) (options : int32) : (t, int) result =
  match Compile.pcre2_compile pattern ~options:(Options.of_int32 options) with
  | Ok re -> Ok re
  | Result.Error (errorcode, _erroroffset) -> Result.Error errorcode

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
   with a terminating zero (pcre2_compile.c:6571-6572), and the OP_MARK
   arm points Fmark at it (pcre2_match.c:6341, Fecode + 2). -1 = NULL ->
   None. Dormant until the M5 verbs chunk makes the interpreter set
   mb->mark. *)
let mark_of_offset (re : t) (off : int) : string option =
  if off < 0 then None
  else
    let len = ref 0 in
    while not (Char.equal (Bytes.get re.Compile.code (off + !len)) '\000') do
      incr len
    done;
    Some (Bytes.sub_string re.Compile.code off !len)

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
   - -1 (NOMATCH) -> [No_match]; -2 (PARTIAL) -> [Partial] with
     startchar = the partial start (= ovector[0], pcre2_match.c:7756-7758);
     any other negative code -> [Error]. *)
let exec_full (re : t) (subject : string) (offset : int) (options : int32) :
    exec_result =
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
    Interpreter.pcre2_match re ~subject ~start_offset:offset
      ~options:(Options.of_int32 options) md
  in
  if rc > 0 then
    Match
      {
        ovector = Array.sub md.Interpreter.ovector 0 (2 * rc);
        mark = mark_of_offset re md.Interpreter.mark;
        start_char = md.Interpreter.startchar;
      }
  else if Int.equal rc Errors.error_nomatch then No_match
  else if Int.equal rc Errors.error_partial then
    Partial
      {
        start = md.Interpreter.startchar;
        mark = mark_of_offset re md.Interpreter.mark;
      }
  else Error rc

let exec (re : t) (subject : string) (offset : int) (options : int32) :
    ((int * int) option, int) result =
  match exec_full re subject offset options with
  | Match { ovector; _ } -> Ok (Some (ovector.(0), ovector.(1)))
  | No_match | Partial _ -> Ok None (* pcre2_stubs.c: PARTIAL -> Ok None *)
  | Error e -> Result.Error e

let exec_captures (re : t) (subject : string) (offset : int) (options : int32) :
    (((int * int) array * (string * int) array) option, int) result =
  match exec_full re subject offset options with
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
  | No_match | Partial _ -> Ok None
  | Error e -> Result.Error e

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

(* ---------- Inline sanity checks (module-initialization asserts) ---------- *)

(* End-to-end boundary checks over the wired compile pipeline. Expected
   values verified against pcre2test on the real 10.44 library
   (testoutput2:128-129, 484-488 shapes). *)
let () =
  (* Successful compile: build-default info values. *)
  (match compile "abc" 0l with
  | Ok re ->
      let i = info re in
      assert (Int.equal i.argoptions 0);
      assert (Int.equal i.alloptions 0);
      assert (Int.equal i.newline Options.newline_lf);
      assert (Int.equal i.bsr Options.bsr_unicode);
      assert (Int.equal i.capture_count 0);
      assert (Int.equal (Array.length (capture_groups re)) 0)
  | Result.Error _ -> assert false);
  (* Compile errors with exact code and offset. *)
  (match compile_ctx "(" 0l with
  | Result.Error (e, o) ->
      assert (Int.equal e Errors.err14);
      assert (Int.equal o 1)
  | Ok _ -> assert false);
  (match compile_ctx "a{2,1}" 0l with
  | Result.Error (e, o) ->
      assert (Int.equal e Errors.err4);
      assert (Int.equal o 5)
  | Ok _ -> assert false);
  (* Unknown option bits: 117 at the raw int seam (port-conventions §5). *)
  (match compile "a" 0x08000000l with
  | Result.Error e -> assert (Int.equal e Errors.err17)
  | Ok _ -> assert false);
  (* A leading (?i) reaches neither options word (pcre2test shows no
     "Options:" line for /(?i)abc/I, testoutput2:484-488). *)
  (match compile "(?i)abc" 0l with
  | Ok re ->
      let i = info re in
      assert (Int.equal i.argoptions 0);
      assert (Int.equal i.alloptions 0)
  | Result.Error _ -> assert false);
  (* Caseless as an option is ARGOPTIONS and ALLOPTIONS. *)
  (match compile "abc" (Int32.of_int Options.caseless) with
  | Ok re ->
      let i = info re in
      assert (Int.equal i.argoptions Options.caseless);
      assert (Int.equal i.alloptions Options.caseless)
  | Result.Error _ -> assert false);
  (* ( *CR) in-pattern newline setting. *)
  (match compile "(*CR)a" 0l with
  | Ok re -> assert (Int.equal (info re).newline Options.newline_cr)
  | Result.Error _ -> assert false);
  (* compile_ctx newline/bsr knobs; 0 = build default. *)
  (match
     compile_ctx ~newline:Options.newline_crlf ~bsr:Options.bsr_anycrlf "a" 0l
   with
  | Ok re ->
      let i = info re in
      assert (Int.equal i.newline Options.newline_crlf);
      assert (Int.equal i.bsr Options.bsr_anycrlf)
  | Result.Error _ -> assert false);
  (* Invalid newline value from the context: the driver's ERR56. *)
  (match compile_ctx ~newline:99 "a" 0l with
  | Result.Error (e, o) ->
      assert (Int.equal e Errors.err56);
      assert (Int.equal o 0)
  | Ok _ -> assert false);
  (* Name table order and numbers at the boundary. *)
  (match compile "(?<x>a)(?<y>b)" 0l with
  | Ok re -> (
      let i = info re in
      assert (Int.equal i.capture_count 2);
      let nt = capture_groups re in
      assert (Int.equal (Array.length nt) 2);
      match (nt.(0), nt.(1)) with
      | (n0, g0), (n1, g1) ->
          assert (String.equal n0 "x");
          assert (Int.equal g0 1);
          assert (String.equal n1 "y");
          assert (Int.equal g1 2))
  | Result.Error _ -> assert false);
  (* The wired match boundary (Interpreter.pcre2_match via exec_full).
     Expected values verified against pcre2test on the real 10.44
     library. *)
  match compile "(a)(b)?" 0l with
  | Result.Error _ -> assert false
  | Ok re -> (
      (* Bump-along match: exec folds to the group-0 pair. *)
      (match exec re "xab" 0 0l with
      | Ok (Some (1, 3)) -> ()
      | _ -> assert false);
      (* exec_full: rc = 2 pairs -> ovector length 4; startchar = attempt
         start. *)
      (match exec_full re "xa" 0 0l with
      | Match { ovector; mark = None; start_char = 1 } ->
          assert (Int.equal (Array.length ovector) 4);
          assert (Int.equal ovector.(0) 1);
          assert (Int.equal ovector.(1) 2);
          assert (Int.equal ovector.(2) 1);
          assert (Int.equal ovector.(3) 2)
      | _ -> assert false);
      (* exec_captures pads unset tail groups with (-1, -1)
         (port-conventions §5). *)
      (match exec_captures re "a" 0 0l with
      | Ok (Some (pairs, _)) -> (
          assert (Int.equal (Array.length pairs) 3);
          match pairs with
          | [| (0, 1); (0, 1); (-1, -1) |] -> ()
          | _ -> assert false)
      | _ -> assert false);
      (* NOMATCH -> Ok None. *)
      (match exec re "x" 0 0l with Ok None -> () | _ -> assert false);
      (* PARTIAL -> Ok None at the exec seam (§5), Partial at exec_full.
         Hard partial returns as soon as the subject end is hit mid-attempt
         (oracle: "Partial match: a"), while soft partial lets the complete
         match win (oracle: 0: a, 1: a). *)
      (match exec re "xa" 0 (Int32.of_int Options.partial_hard) with
      | Ok None -> ()
      | _ -> assert false);
      (match exec_full re "xa" 0 (Int32.of_int Options.partial_hard) with
      | Partial { start = 1; mark = None } -> ()
      | _ -> assert false);
      (match exec re "xa" 0 (Int32.of_int Options.partial_soft) with
      | Ok (Some (1, 2)) -> ()
      | _ -> assert false);
      (match compile "abc" 0l with
      | Result.Error _ -> assert false
      | Ok re3 -> (
          (match exec re3 "xab" 0 (Int32.of_int Options.partial_hard) with
          | Ok None -> ()
          | _ -> assert false);
          match exec_full re3 "xab" 0 (Int32.of_int Options.partial_hard) with
          | Partial { start = 1; mark = None } -> ()
          | _ -> assert false));
      (* BADOFFSET (-33) for negative or beyond-end offsets; unknown match
         option bits -> BADOPTION (-34) (§5). *)
      (match exec re "a" 5 0l with
      | Result.Error e -> assert (Int.equal e Errors.error_badoffset)
      | Ok _ -> assert false);
      (match exec re "a" (-1) 0l with
      | Result.Error e -> assert (Int.equal e Errors.error_badoffset)
      | Ok _ -> assert false);
      match exec re "a" 0 0x00000100l with
      | Result.Error e -> assert (Int.equal e Errors.error_badoption)
      | Ok _ -> assert false)
