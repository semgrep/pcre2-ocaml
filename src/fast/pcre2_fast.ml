(* Fast engine seam — chunk C2 (fast-design.md §1). Compilation runs the
   SHARED compiler (Pcre2_engine.Engine.compile_ctx → Pcre2_engine.Compile), so
   real PCRE2 compile errors surface with exact number/offset parity, then the
   IR compiler (Ir_compile) lowers the bytecode; any construct it does not
   handle yields [Unsupported]. A compiled [t] is executed ONLY by the fast
   runner (Runner) — NEVER by the interpreter (the no-fallback contract). On
   accepted patterns observable behavior is identical to
   Pcre2_engine.Engine.exec/exec_full/exec_captures (rc/ovector/mark/startchar
   and limit trip points, fast-design.md §4). *)

module E = Pcre2_engine.Engine
module C = Pcre2_engine.Compile
module Opt = Pcre2_engine.Options
module Errors = Pcre2_engine.Errors
module R = Runner

(** {1 Unstable internals — exposed for tests only (M11 chunk C1/C2)}

    [Ir], [Ir_compile] and [Ir_verify] are the fast engine's IR, its compiler
    and its static verifier. They are NOT part of the public seam and may
    change without notice; only [test/fast] should reference them. *)

module Ir = Ir
module Ir_compile = Ir_compile
module Ir_verify = Ir_verify

(* The compiled fast pattern is the pre-decoded IR (Ir.t = { code; lit; re }):
   the runner reads [code]/[lit] and derives limits/flags from the pinned
   [re] per exec (Runner.exec, mirroring the interpreter's per-exec mb fill,
   pcre2_match.c:6956-7046). Abstract at the seam (pcre2_fast.mli). *)
type t = Ir.t

type compile_error =
  | Compile_error of { errcode : int; erroroffset : int }
  | Unsupported of string

(* Compile through the SHARED compiler (Pcre2_engine.Compile) directly — the
   [Pcre2_engine.Engine.t] returned by Engine.compile_ctx is abstract, but the
   IR compiler needs the concrete [Compile.re]. This mirrors Engine.compile_ctx
   (engine.ml:31-45) exactly, so real compile errors carry the same
   number/offset pair the engine driver reports. *)
let compile_ctx ?(newline = 0) ?(bsr = 0) ?(extra = 0) (pattern : string)
    (options : int32) : (t, compile_error) result =
  let dflt = C.default_compile_context in
  let ccontext =
    {
      dflt with
      C.newline_convention =
        (if Int.equal newline 0 then dflt.C.newline_convention else newline);
      bsr_convention =
        (if Int.equal bsr 0 then dflt.C.bsr_convention else bsr);
      extra_options = extra;
    }
  in
  match C.pcre2_compile ~ccontext pattern ~options:(Opt.of_int32 options) with
  | Error (errcode, erroroffset) ->
      Error (Compile_error { errcode; erroroffset })
  | Ok re -> (
      match Ir_compile.compile re with
      | Error reason -> Error (Unsupported reason)
      | Ok ir -> (
          (* Always-on verifier: since the fast engine has NO runtime
             fallback (fast-design.md §1/§9), a compiled program that could
             reach an unhandled instruction must be declined, never executed.
             Ir_compile is deterministic and correct, so this never fires in
             practice; if it ever did, an [Unsupported] skip is strictly safe
             (a decline, never a wrong answer). The full walk is cheap
             (linear, compile is cold). *)
          match Ir_verify.check ir with
          | Ok () -> Ok ir
          | Error diag ->
              Error
                (Unsupported
                   ("fast: internal IR verification failed: " ^ diag))))

let compile (pattern : string) (options : int32) : (t, compile_error) result =
  compile_ctx pattern options

let exec (re : t) (subject : string) (offset : int) (options : int32) :
    ((int * int) option, int) result =
  let o = R.exec re ~subject ~offset ~options:(Opt.of_int32 options) in
  if o.R.orc > 0 then Ok (Some (o.R.ostart, o.R.oend))
  else if Int.equal o.R.orc Errors.error_nomatch
          || Int.equal o.R.orc Errors.error_partial
  then Ok None
  else Error o.R.orc

let exec_full (re : t) (subject : string) (offset : int) (options : int32) :
    E.exec_result =
  let o = R.exec re ~subject ~offset ~options:(Opt.of_int32 options) in
  if o.R.orc > 0 then
    E.Match { ovector = [| o.R.ostart; o.R.oend |]; mark = None; start_char = o.R.ostart }
  else if Int.equal o.R.orc Errors.error_nomatch then E.No_match { mark = None }
  else if Int.equal o.R.orc Errors.error_partial then
    E.Partial { start = o.R.ostart; mark = None }
  else E.Error { code = o.R.orc; start_char = 0 }

let exec_captures (re : t) (subject : string) (offset : int) (options : int32) :
    (((int * int) array * (string * int) array) option, int) result =
  let o = R.exec re ~subject ~offset ~options:(Opt.of_int32 options) in
  if o.R.orc > 0 then
    (* top_bracket = 0 in this subset: a single ovector pair, no name table. *)
    Ok (Some ([| (o.R.ostart, o.R.oend) |], [||]))
  else if Int.equal o.R.orc Errors.error_nomatch
          || Int.equal o.R.orc Errors.error_partial
  then Ok None
  else Error o.R.orc

(* top_bracket = 0 in this subset -> no named groups. Built from the pinned
   [Compile.re] (Engine.t is abstract at the seam, so we cannot delegate to
   Engine.info/Engine.capture_groups): identical fields to Engine.info /
   Engine.capture_groups (engine.ml:59-90). *)
let capture_groups (re : t) : (string * int) array =
  let r = re.Ir.re in
  Array.init r.C.name_count (fun i ->
      let base = i * r.C.name_entry_size in
      let number = C.get2 r.C.name_table base in
      let start = base + Pcre2_engine.Limits.imm2_size in
      let len = ref 0 in
      while
        not (Char.equal (Bytes.get r.C.name_table (start + !len)) '\000')
      do
        incr len
      done;
      (Bytes.sub_string r.C.name_table start !len, number))

let info (re : t) : E.info =
  let r = re.Ir.re in
  {
    E.argoptions = r.C.compile_options;
    alloptions = r.C.overall_options;
    newline = r.C.newline_convention;
    bsr = r.C.bsr_convention;
    capture_count = r.C.top_bracket;
  }
let error_message = E.error_message
let version = E.version

(* The frozen Matcher convenience surface, assembled from the shared
   pcre2.matcher pieces exactly like Pcre2.Interp/Jit do (src/pcre2.ml),
   with the raw seam functions above in place of Bindings. *)
type fast_compile_error = compile_error =
  | Compile_error of { errcode : int; erroroffset : int }
  | Unsupported of string

module Matcher = struct
  include Pcre2_matcher.Options.Interp
  include Pcre2_matcher.Match
  include Pcre2_matcher.Error

  type nonrec t = t

  (* Shadows the frozen [Pcre2_matcher.Error.compile_error] brought in by the
     include above: Fast's compile can also decline a valid pattern
     ([Unsupported]), which the frozen variant cannot express. Hand-written
     show/pp/equal keep pcre2.fast ppx-free. *)
  type compile_error = fast_compile_error =
    | Compile_error of { errcode : int; erroroffset : int }
    | Unsupported of string

  let pp_compile_error fmt = function
    | Compile_error { errcode; erroroffset } ->
        Format.fprintf fmt "Pcre2_fast.Compile_error { errcode = %d; erroroffset = %d }"
          errcode erroroffset
    | Unsupported reason -> Format.fprintf fmt "Pcre2_fast.Unsupported %S" reason

  let show_compile_error e = Format.asprintf "%a" pp_compile_error e

  let equal_compile_error a b =
    match (a, b) with
    | Compile_error x, Compile_error y ->
        Int.equal x.errcode y.errcode && Int.equal x.erroroffset y.erroroffset
    | Unsupported x, Unsupported y -> String.equal x y
    | Compile_error _, Unsupported _ | Unsupported _, Compile_error _ -> false

  let compile ?(options : compile_option list = []) (pattern : string) :
      (t, compile_error) Result.t =
    compile pattern (bitvector_of_compile_options options)

  let capture_groups (r : t) = capture_groups r |> Array.to_list

  include Pcre2_matcher.Convenience.MakeMatcher (struct
    type nonrec t = t
    type nonrec match_option = match_option

    let bitvector_of_match_options = bitvector_of_match_options
    let match_raw = exec
    let capture_raw = exec_captures
  end)

  include Pcre2_matcher.Convenience.MakeConvenience (struct
    type nonrec t = t
    type nonrec match_option = match_option

    let bitvector_of_match_options = bitvector_of_match_options
    let capture_raw = exec_captures
  end)
end
