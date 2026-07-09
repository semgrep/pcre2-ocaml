(* Conformance runner: replays vendored pcre2test corpora through the
   harness (oracle driver, in-process) and byte-compares each unit's output
   with the vendored testoutput file.

   Modes:
     runner                     report per-file counts; if a baseline file
                                exists, fail (exit 1) on any regression
     runner --frontier          print the first failing in-scope unit, exit 1
     runner --only FILE:ORD     run one unit, print a full expected/actual diff
     runner --update-baseline   rewrite test/conformance/baseline_counts.sexp

   Skips: units whose harness run reports an out-of-scope feature
   (reason "modifier:<name>" / "command:<name>") are excluded from pass/fail,
   as are units matching skiplist.sexp. A skiplisted unit that PASSES is
   reported as STALE-SKIP and makes the run fail. *)

module H = Pcre2test_harness.Harness.Make (Pcre2_test_driver.Test_driver)
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
    if s.[i] = '\n' then begin
      lines := String.sub s !start (i - !start + 1) :: !lines;
      start := i + 1
    end
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

let load_baseline () =
  let file = path "test/conformance/baseline_counts.sexp" in
  if not (Sys.file_exists file) then None
  else
    match Sexp_lite.parse_file file with
    | [ Sexp_lite.List entries ] ->
        Some
          (List.map
             (function
               | Sexp_lite.List
                   [
                     Sexp_lite.Atom f; Sexp_lite.Atom p; Sexp_lite.Atom tot;
                   ] ->
                   (f, int_of_string p, int_of_string tot)
               | _ ->
                   prerr_endline "runner: malformed baseline_counts.sexp";
                   exit 2)
             entries)
    | _ ->
        prerr_endline "runner: malformed baseline_counts.sexp";
        exit 2

(* ---------------- running one file ---------------- *)

let output_name input_name =
  (* testinput1 -> testoutput1 *)
  if String.length input_name >= 9 && String.sub input_name 0 9 = "testinput"
  then "testoutput" ^ String.sub input_name 9 (String.length input_name - 9)
  else input_name ^ ".out"

let run_file skiplist file : file_report =
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
    List.length
      (List.filter (fun o -> o.skip = None && not o.passed) outcomes)
  in
  let cats = [ "out-of-scope"; "env"; "deferred"; "harness" ] in
  let n_skip_by_cat =
    List.map
      (fun c ->
        ( c,
          List.length
            (List.filter
               (fun o ->
                 match o.skip with Some (c', _) -> String.equal c c' | None -> false)
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
  let mode =
    match args with
    | [] -> `Default
    | [ "--frontier" ] -> `Frontier
    | [ "--failures" ] -> `Failures
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
          "usage: runner [--frontier | --only <file>:<ordinal> | \
           --update-baseline]";
        exit 2
  in
  let skiplist = load_skiplist () in
  let files = load_order () in
  match mode with
  | `Only (file, ord) -> (
      let r = run_file skiplist file in
      match List.find_opt (fun o -> o.ordinal = ord) r.outcomes with
      | None ->
          Printf.eprintf "runner: %s has no unit %d (%d units)\n" file ord
            (List.length r.outcomes);
          exit 2
      | Some o ->
          Printf.printf "%s:%d  pattern: %s\n" file ord o.pattern_line;
          (match o.skip with
          | Some (c, why) -> Printf.printf "skip: [%s] %s\n" c why
          | None -> ());
          Printf.printf "--- expected\n";
          List.iter (fun l -> Printf.printf "  %s\n" l) o.expected;
          Printf.printf "--- actual\n";
          List.iter (fun l -> Printf.printf "  %s\n" l) o.actual;
          (match first_diff o.expected o.actual with
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
              if o.skip = None && not o.passed then begin
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
                | None -> ()
              end)
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
              List.find_opt
                (fun o -> o.skip = None && not o.passed)
                r.outcomes
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
                  Printf.printf "  skipped %s:%d [%s] %s\n" r.file o.ordinal
                    cat why
              | None -> ())
            r.outcomes)
        reports;
      (* STALE-SKIP detection *)
      let stale =
        List.concat_map
          (fun r ->
            List.filter_map
              (fun o -> if o.stale_skip then Some (r.file, o.ordinal) else None)
              r.outcomes)
          reports
      in
      List.iter
        (fun (f, ord) ->
          Printf.printf "STALE-SKIP: %s:%d passes but is skiplisted\n" f ord)
        stale;
      match mode with
      | `Update ->
          let oc =
            open_out (path "test/conformance/baseline_counts.sexp")
          in
          output_string oc
            "; Conformance baseline: ((<file> <passed> <total-in-scope>) \
             ...)\n; Regenerate with: dune exec test/conformance/runner.exe \
             -- --update-baseline\n";
          output_string oc "(";
          List.iteri
            (fun i r ->
              if i > 0 then output_string oc "\n ";
              Printf.fprintf oc "(%s %d %d)" r.file r.n_pass
                (r.n_pass + r.n_fail))
            reports;
          output_string oc ")\n";
          close_out oc;
          Printf.printf "baseline_counts.sexp updated\n";
          if stale <> [] then exit 1
      | _ -> (
          if stale <> [] then exit 1;
          match load_baseline () with
          | None -> ()
          | Some baseline ->
              let regressed = ref false in
              List.iter
                (fun (f, bpass, _btot) ->
                  match
                    List.find_opt (fun r -> String.equal r.file f) reports
                  with
                  | None ->
                      Printf.printf
                        "REGRESSION: %s in baseline but not run\n" f;
                      regressed := true
                  | Some r ->
                      if r.n_pass < bpass then begin
                        Printf.printf
                          "REGRESSION: %s passed %d < baseline %d\n" f r.n_pass
                          bpass;
                        regressed := true
                      end)
                baseline;
              if !regressed then exit 1))
