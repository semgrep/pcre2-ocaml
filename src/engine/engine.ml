(* Boundary of the pure-OCaml PCRE2 10.44 engine (see engine.mli).

   The compile side is wired to the real pipeline (Compile.pcre2_compile,
   pcre2_compile.c:10096-10993); [t] is the compiled pattern record.
   exec/exec_full still fail with a real PCRE2 code until the M1
   interpreter chunks land (docs/ocaml-engine/02-core-compile-match.md,
   match phase). *)

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
  match
    Compile.pcre2_compile pattern ~options:(Options.of_int32 options)
  with
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
  Compile.pcre2_compile ~ccontext pattern
    ~options:(Options.of_int32 options)

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

let exec_full (_re : t) (_subject : string) (_offset : int) (_options : int32) :
    exec_result =
  Error Errors.error_internal

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
      let pairs =
        Array.init n (fun i ->
            let s = ovector.(2 * i) and e = ovector.((2 * i) + 1) in
            (s, e))
      in
      Ok (Some (pairs, capture_groups re))
  | No_match | Partial _ -> Ok None
  | Error e -> Result.Error e

(* Port target is pinned to PCRE2 10.44 (vendor/pcre2/VERSION). *)
let version = (10, 44)

let print_code (_fmt : Format.formatter) (_re : t) : unit =
  (* debug_printer.ml lands with the M1 match-phase chunks
     (pcre2_printint.c). *)
  ()

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
  (match compile_ctx ~newline:Options.newline_crlf ~bsr:Options.bsr_anycrlf
           "a" 0l
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
  (* exec on a compiled pattern still reports the interpreter gap as a
     real PCRE2 error code (matcher chunks are next). *)
  match compile "a" 0l with
  | Ok re -> (
      match exec re "a" 0 0l with
      | Result.Error e -> assert (Int.equal e Errors.error_internal)
      | Ok _ -> assert false)
  | Result.Error _ -> assert false
