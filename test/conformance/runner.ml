(* Conformance runner: replays vendored pcre2test corpora through the
   harness (in-process) and byte-compares each unit's output with the
   vendored testoutput file.

   Drivers (--driver=oracle|engine, default oracle):
     oracle   the C PCRE2 10.44 library; baseline =
              test/conformance/baseline_counts.sexp (harness correctness)
     engine   the pure OCaml engine (src/engine/); baseline =
              test/conformance/engine_baseline_counts.sexp — THE port
              frontier lives here from M1 on

   Modes (combine with --driver=...):
     runner                     report per-file counts; if a baseline file
                                exists, fail (exit 1) on any regression
     runner --frontier          print the first failing in-scope unit, exit 1
     runner --only FILE:ORD     run one unit, print a full expected/actual diff
     runner --update-baseline   rewrite the selected driver's baseline file

   Skips: units whose harness run reports an out-of-scope feature
   (reason "modifier:<name>" / "command:<name>") are excluded from pass/fail,
   as are units matching skiplist.sexp. A skiplisted unit that PASSES is
   reported as STALE-SKIP and makes the run fail — ORACLE MODE ONLY: the
   skiplist documents harness scope against the oracle, and the engine is
   expected to fail those units for a long time (they stay excluded from
   engine counts, just without the staleness error). *)

module type HARNESS = sig
  type t

  val create : unit -> t
  val process_line : t -> string -> string list
  val finish : t -> string list
  val reset_unit_skips : t -> unit
  val unit_skips : t -> string list
end

module Oracle_harness =
  Pcre2test_harness.Harness.Make (Pcre2_test_driver.Test_driver)

module Engine_harness =
  Pcre2test_harness.Harness.Make (Pcre2test_harness.Engine_driver)

module Units = Conformance_lib.Units
module Sexp_lite = Conformance_lib.Sexp_lite

type skip_entry = {
  sfile : string;
  lo : int;
  hi : int;
  category : string; (* out-of-scope | env | deferred *)
  reason : string;
}

type unit_outcome = {
  ordinal : int;
  pattern_line : string;
  expected : string list;
  actual : string list;
  passed : bool;
  skip : (string * string) option; (* category, reason *)
  stale_skip : bool;
}

type file_report = {
  file : string;
  outcomes : unit_outcome list;
  n_pass : int;
  n_fail : int;
  n_skip_by_cat : (string * int) list;
}

(* ---------------- filesystem helpers ---------------- *)

let read_raw_lines path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  let n = String.length s in
  let lines = ref [] in
  let start = ref 0 in
  for i = 0 to n - 1 do
    if s.[i] = '\n' then (
      lines := String.sub s !start (i - !start + 1) :: !lines;
      start := i + 1)
  done;
  if !start < n then lines := String.sub s !start (n - !start) :: !lines;
  List.rev !lines

let rec find_root dir =
  let testdata = Filename.concat dir "vendor/pcre2/testdata" in
  let conf = Filename.concat dir "test/conformance" in
  if Sys.file_exists testdata && Sys.file_exists conf then Some dir
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else find_root parent

let root =
  lazy
    (match find_root (Sys.getcwd ()) with
    | Some r -> r
    | None ->
        prerr_endline
          "runner: cannot locate repo root (vendor/pcre2/testdata) from cwd";
        exit 2)

let path rel = Filename.concat (Lazy.force root) rel

(* ---------------- configuration files ---------------- *)

let load_order () =
  let lines = read_raw_lines (path "test/conformance/testdata-order") in
  lines
  |> List.map (fun l -> String.trim l)
  |> List.filter (fun l -> String.length l > 0 && l.[0] <> '#')

let load_skiplist () =
  let file = path "test/conformance/skiplist.sexp" in
  if not (Sys.file_exists file) then []
  else
    match Sexp_lite.parse_file file with
    | [] -> []
    | [ Sexp_lite.List entries ] ->
        List.map
          (function
            | Sexp_lite.List
                [
                  Sexp_lite.Atom sfile;
                  Sexp_lite.List [ Sexp_lite.Atom lo; Sexp_lite.Atom hi ];
                  Sexp_lite.Atom category;
                  Sexp_lite.Atom reason;
                ] ->
                (match category with
                | "out-of-scope" | "env" | "deferred" -> ()
                | c ->
                    prerr_endline ("runner: unknown skiplist category: " ^ c);
                    exit 2);
                {
                  sfile;
                  lo = int_of_string lo;
                  hi = int_of_string hi;
                  category;
                  reason;
                }
            | _ ->
                prerr_endline "runner: malformed skiplist.sexp entry";
                exit 2)
          entries
    | _ ->
        prerr_endline "runner: skiplist.sexp must contain a single list";
        exit 2

let load_baseline baseline_rel =
  let file = path baseline_rel in
  if not (Sys.file_exists file) then None
  else
    match Sexp_lite.parse_file file with
    | [ Sexp_lite.List entries ] ->
        Some
          (List.map
             (function
               | Sexp_lite.List
                   [ Sexp_lite.Atom f; Sexp_lite.Atom p; Sexp_lite.Atom tot ] ->
                   (f, int_of_string p, int_of_string tot)
               | _ ->
                   prerr_endline ("runner: malformed " ^ baseline_rel);
                   exit 2)
             entries)
    | _ ->
        prerr_endline ("runner: malformed " ^ baseline_rel);
        exit 2

(* ---------------- running one file ---------------- *)

let output_name input_name =
  (* testinput1 -> testoutput1 *)
  if String.length input_name >= 9 && String.sub input_name 0 9 = "testinput"
  then "testoutput" ^ String.sub input_name 9 (String.length input_name - 9)
  else input_name ^ ".out"

let run_file (module H : HARNESS) skiplist file : file_report =
  let input_lines = read_raw_lines (path ("vendor/pcre2/testdata/" ^ file)) in
  let expected_file =
    (* some tests have bit-width / link-size specific outputs,
       e.g. testoutput8-8-2 (8-bit library, LINK_SIZE 2) *)
    let base = path ("vendor/pcre2/testdata/" ^ output_name file) in
    let candidates = [ base; base ^ "-8"; base ^ "-8-2" ] in
    match List.find_opt Sys.file_exists candidates with
    | Some f -> f
    | None -> base (* fail with a clear message below *)
  in
  let expected_lines = read_raw_lines expected_file in
  let units = Units.split input_lines in
  let blocks =
    match Units.align units expected_lines with
    | Ok b -> b
    | Error msg ->
        Printf.eprintf "runner: %s: %s\n" file msg;
        exit 2
  in
  let t = H.create () in
  let n_units = List.length units in
  let outcomes =
    List.map2
      (fun (u : Units.unit_t) expected ->
        H.reset_unit_skips t;
        let actual =
          List.concat_map (fun line -> H.process_line t line) u.Units.lines
        in
        let actual =
          (* surface an unterminated-pattern abort at EOF *)
          if u.Units.ordinal = n_units then actual @ H.finish t else actual
        in
        let hskips = H.unit_skips t in
        let sl =
          List.filter
            (fun s ->
              String.equal s.sfile file && u.Units.ordinal >= s.lo
              && u.Units.ordinal <= s.hi)
            skiplist
        in
        let passed =
          List.length actual = List.length expected
          && List.for_all2 String.equal actual expected
        in
        let skip, stale =
          match (sl, hskips) with
          | s :: _, _ ->
              (Some (s.category, s.reason), passed (* STALE-SKIP if passes *))
          | [], r :: _ -> (Some ("harness", r), false)
          | [], [] -> (None, false)
        in
        {
          ordinal = u.Units.ordinal;
          pattern_line = u.Units.pattern_line;
          expected;
          actual;
          passed;
          skip;
          stale_skip = stale;
        })
      units blocks
  in
  let n_pass =
    List.length (List.filter (fun o -> o.skip = None && o.passed) outcomes)
  in
  let n_fail =
    List.length (List.filter (fun o -> o.skip = None && not o.passed) outcomes)
  in
  let cats = [ "out-of-scope"; "env"; "deferred"; "harness" ] in
  let n_skip_by_cat =
    List.map
      (fun c ->
        ( c,
          List.length
            (List.filter
               (fun o ->
                 match o.skip with
                 | Some (c', _) -> String.equal c c'
                 | None -> false)
               outcomes) ))
      cats
  in
  { file; outcomes; n_pass; n_fail; n_skip_by_cat }

(* ---------------- reporting ---------------- *)

let first_diff expected actual =
  let rec go i = function
    | [], [] -> None
    | e :: _, [] -> Some (i, Some e, None)
    | [], a :: _ -> Some (i, None, Some a)
    | e :: es, a :: as_ ->
        if String.equal e a then go (i + 1) (es, as_)
        else Some (i, Some e, Some a)
  in
  go 0 (expected, actual)

(* ---------------- fuzz regressions (differential replay) ----------------

   The differential fuzzer (fuzz/fuzz_diff.exe) writes minimized repros as
   pcre2test-format units into fuzz/corpus/regressions/. There is no vendored
   "expected" output for these (the whole point is engine != oracle at
   discovery), so they are replayed DIFFERENTIALLY: each file is run through
   BOTH the oracle and the engine harness and the two outputs are compared.
   Once a repro's underlying bug is fixed the two agree and it stays green
   forever; while a bug is open the file diverges and the run fails. An empty
   / absent directory is a no-op, so this never interferes with a clean tree. *)

let list_regression_files () =
  let dir = path "fuzz/corpus/regressions" in
  if not (Sys.file_exists dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".txt")
    |> List.sort String.compare
    |> List.map (fun f -> (f, Filename.concat dir f))

let replay_lines (module H : HARNESS) lines =
  let t = H.create () in
  let out = List.concat_map (fun l -> H.process_line t l) lines in
  out @ H.finish t

let first_diff_generic expected actual =
  let rec go i = function
    | [], [] -> None
    | e :: _, [] -> Some (i, Some e, None)
    | [], a :: _ -> Some (i, None, Some a)
    | e :: es, a :: as_ ->
        if String.equal e a then go (i + 1) (es, as_)
        else Some (i, Some e, Some a)
  in
  go 0 (expected, actual)

(* Returns true if any regression file diverges (engine != oracle). *)
let replay_regressions () : bool =
  let files = list_regression_files () in
  if files = [] then false
  else (
    Printf.printf
      "regressions: replaying %d fuzz repro(s) from fuzz/corpus/regressions \
       (engine vs oracle)\n"
      (List.length files);
    let bad = ref false in
    List.iter
      (fun (name, pathf) ->
        let lines = read_raw_lines pathf in
        let o = replay_lines (module Oracle_harness) lines in
        let e = replay_lines (module Engine_harness) lines in
        if List.length o = List.length e && List.for_all2 String.equal o e then
          Printf.printf "  OK       %s\n" name
        else (
          bad := true;
          Printf.printf "  DIVERGE  %s\n" name;
          match first_diff_generic o e with
          | Some (i, eo, ea) ->
              Printf.printf "    line %d\n" (i + 1);
              Printf.printf "    oracle: %s\n"
                (match eo with Some l -> l | None -> "<end of output>");
              Printf.printf "    engine: %s\n"
                (match ea with Some l -> l | None -> "<end of output>")
          | None -> ()))
      files;
    !bad)

let print_report r =
  let skips =
    r.n_skip_by_cat
    |> List.filter (fun (_, n) -> n > 0)
    |> List.map (fun (c, n) -> Printf.sprintf "%s %d" c n)
    |> String.concat ", "
  in
  let total = List.length r.outcomes in
  Printf.printf "%s: passed %d, failed %d, skipped %d%s, total %d\n" r.file
    r.n_pass r.n_fail
    (total - r.n_pass - r.n_fail)
    (if skips = "" then "" else " (" ^ skips ^ ")")
    total

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let driver = ref `Oracle in
  let args =
    List.filter
      (fun a ->
        if String.equal a "--driver=oracle" then (
          driver := `Oracle;
          false)
        else if String.equal a "--driver=engine" then (
          driver := `Engine;
          false)
        else if String.length a >= 9 && String.sub a 0 9 = "--driver=" then (
          prerr_endline ("runner: unknown driver in '" ^ a ^ "'");
          exit 2)
        else true)
      args
  in
  let mode =
    match args with
    | [] -> `Default
    | [ "--frontier" ] -> `Frontier
    | [ "--failures" ] -> `Failures
    | [ "--regressions" ] -> `Regressions
    | [ "--update-baseline" ] -> `Update
    | [ "--only"; spec ] -> (
        match String.index_opt spec ':' with
        | Some i ->
            `Only
              ( String.sub spec 0 i,
                int_of_string
                  (String.sub spec (i + 1) (String.length spec - i - 1)) )
        | None ->
            prerr_endline "runner: --only expects <file>:<ordinal>";
            exit 2)
    | _ ->
        prerr_endline
          "usage: runner [--driver=oracle|engine] [--frontier | --failures | \
           --regressions | --only <file>:<ordinal> | --update-baseline]";
        exit 2
  in
  (* Everything driver-specific in one place: the harness instantiation, the
     baseline file it is compared against / updates, and whether STALE-SKIP
     is enforced (oracle only; see header comment). *)
  let harness, baseline_rel, enforce_stale_skips =
    match !driver with
    | `Oracle ->
        ( (module Oracle_harness : HARNESS),
          "test/conformance/baseline_counts.sexp",
          true )
    | `Engine ->
        ( (module Engine_harness : HARNESS),
          "test/conformance/engine_baseline_counts.sexp",
          false )
  in
  let run_file = run_file harness in
  let skiplist = load_skiplist () in
  let files = load_order () in
  match mode with
  | `Regressions -> exit (if replay_regressions () then 1 else 0)
  | `Only (file, ord) -> (
      let r = run_file skiplist file in
      match List.find_opt (fun o -> o.ordinal = ord) r.outcomes with
      | None ->
          Printf.eprintf "runner: %s has no unit %d (%d units)\n" file ord
            (List.length r.outcomes);
          exit 2
      | Some o -> (
          Printf.printf "%s:%d  pattern: %s\n" file ord o.pattern_line;
          (match o.skip with
          | Some (c, why) -> Printf.printf "skip: [%s] %s\n" c why
          | None -> ());
          Printf.printf "--- expected\n";
          List.iter (fun l -> Printf.printf "  %s\n" l) o.expected;
          Printf.printf "--- actual\n";
          List.iter (fun l -> Printf.printf "  %s\n" l) o.actual;
          match first_diff o.expected o.actual with
          | None ->
              Printf.printf "--- no diff (unit passes)\n";
              exit 0
          | Some (i, e, a) ->
              Printf.printf "--- first diff at output line %d\n" (i + 1);
              Printf.printf "  expected: %s\n"
                (match e with Some l -> l | None -> "<end of output>");
              Printf.printf "  actual:   %s\n"
                (match a with Some l -> l | None -> "<end of output>");
              exit 1))
  | `Failures ->
      (* list every failing in-scope unit with its first diff line *)
      let any = ref false in
      List.iter
        (fun file ->
          let r = run_file skiplist file in
          List.iter
            (fun o ->
              if o.skip = None && not o.passed then (
                any := true;
                Printf.printf "%s:%d  pattern: %s\n" file o.ordinal
                  o.pattern_line;
                match first_diff o.expected o.actual with
                | Some (i, e, a) ->
                    Printf.printf "  diff at line %d\n" (i + 1);
                    Printf.printf "    expected: %s\n"
                      (match e with Some l -> l | None -> "<end of output>");
                    Printf.printf "    actual:   %s\n"
                      (match a with Some l -> l | None -> "<end of output>")
                | None -> ()))
            r.outcomes)
        files;
      exit (if !any then 1 else 0)
  | `Frontier -> (
      let frontier = ref None in
      List.iter
        (fun file ->
          if !frontier = None then
            let r = run_file skiplist file in
            match
              List.find_opt (fun o -> o.skip = None && not o.passed) r.outcomes
            with
            | Some o -> frontier := Some (file, o)
            | None -> ())
        files;
      match !frontier with
      | None ->
          Printf.printf "frontier: none (all in-scope units pass)\n";
          exit 0
      | Some (file, o) ->
          Printf.printf "frontier: %s:%d\n" file o.ordinal;
          Printf.printf "pattern: %s\n" o.pattern_line;
          (match first_diff o.expected o.actual with
          | Some (i, e, a) ->
              Printf.printf "first diff at output line %d\n" (i + 1);
              Printf.printf "  expected: %s\n"
                (match e with Some l -> l | None -> "<end of output>");
              Printf.printf "  actual:   %s\n"
                (match a with Some l -> l | None -> "<end of output>")
          | None -> ());
          exit 1)
  | `Default | `Update -> (
      let reports = List.map (run_file skiplist) files in
      List.iter print_report reports;
      (* list skipped units so the skiplist / harness scope stays auditable *)
      List.iter
        (fun r ->
          List.iter
            (fun o ->
              match o.skip with
              | Some (cat, why) ->
                  Printf.printf "  skipped %s:%d [%s] %s\n" r.file o.ordinal cat
                    why
              | None -> ())
            r.outcomes)
        reports;
      (* STALE-SKIP detection — enforced in oracle mode only (the engine is
         expected to fail skiplisted units for a long time). *)
      let stale =
        if not enforce_stale_skips then []
        else
          List.concat_map
            (fun r ->
              List.filter_map
                (fun o ->
                  if o.stale_skip then Some (r.file, o.ordinal) else None)
                r.outcomes)
            reports
      in
      List.iter
        (fun (f, ord) ->
          Printf.printf "STALE-SKIP: %s:%d passes but is skiplisted\n" f ord)
        stale;
      (* Replay any fuzz-minimized regressions (no-op when the dir is empty). *)
      let regr_bad = replay_regressions () in
      match mode with
      | `Update ->
          let oc = open_out (path baseline_rel) in
          let regen_flag =
            match !driver with `Oracle -> "" | `Engine -> "--driver=engine "
          in
          Printf.fprintf oc
            "; Conformance baseline: ((<file> <passed> <total-in-scope>) ...)\n\
             ; Regenerate with: dune exec test/conformance/runner.exe -- \
             %s--update-baseline\n"
            regen_flag;
          output_string oc "(";
          List.iteri
            (fun i r ->
              if i > 0 then output_string oc "\n ";
              Printf.fprintf oc "(%s %d %d)" r.file r.n_pass
                (r.n_pass + r.n_fail))
            reports;
          output_string oc ")\n";
          close_out oc;
          Printf.printf "%s updated\n" (Filename.basename baseline_rel);
          if stale <> [] || regr_bad then exit 1
      | _ -> (
          if stale <> [] then exit 1;
          if regr_bad then exit 1;
          match load_baseline baseline_rel with
          | None -> ()
          | Some baseline ->
              let regressed = ref false in
              List.iter
                (fun (f, bpass, _btot) ->
                  match
                    List.find_opt (fun r -> String.equal r.file f) reports
                  with
                  | None ->
                      Printf.printf "REGRESSION: %s in baseline but not run\n" f;
                      regressed := true
                  | Some r ->
                      if r.n_pass < bpass then (
                        Printf.printf "REGRESSION: %s passed %d < baseline %d\n"
                          f r.n_pass bpass;
                        regressed := true))
                baseline;
              if !regressed then exit 1))
