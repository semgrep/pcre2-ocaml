(* M10 perf gate (docs/ocaml-engine/10-performance.md, gate G10): reads the
   results.json written by bench.exe, recomputes per-benchmark engine/oracle
   ratios and their geomean, prints the verdict table, and exits 1 unless

     geomean <= 2.0  AND  every per-benchmark ratio <= 2.0
     AND every benchmark is valid (counts agreed engine vs oracle)

   ("at most 2x slower than the C interpreter through the same OCaml API" —
   the plan's hard gate; any exception needs explicit user sign-off).
   The raw-C column is informational (the honesty number), never gated. *)

let usage () =
  print_string
    "usage: compare [RESULTS.json] [--max-ratio R]\n\
     \  default results file: bench/results.json; default max ratio: 2.0\n";
  exit 64

let () =
  let path = ref "bench/results.json" in
  let max_ratio = ref 2.0 in
  let argc = Array.length Sys.argv in
  let rec parse i =
    if i < argc then
      match Sys.argv.(i) with
      | "--max-ratio" when i + 1 < argc ->
          max_ratio := float_of_string Sys.argv.(i + 1);
          parse (i + 2)
      | "--help" | "-h" -> usage ()
      | a when String.length a > 0 && not (Char.equal a.[0] '-') ->
          path := a;
          parse (i + 1)
      | a ->
          Printf.eprintf "compare: unknown argument %s\n" a;
          usage ()
  in
  parse 1;
  let contents =
    match open_in_bin !path with
    | ic ->
        let n = in_channel_length ic in
        let s = really_input_string ic n in
        close_in ic;
        s
    | exception Sys_error msg ->
        Printf.eprintf "compare: cannot read %s: %s\n" !path msg;
        exit 66
  in
  let json =
    try Bench_json.parse contents
    with Bench_json.Parse_error msg ->
      Printf.eprintf "compare: %s: %s\n" !path msg;
      exit 65
  in
  let open Bench_json in
  let meta = member "meta" json in
  let mode = get_string ~default:"?" (member "mode" meta) in
  let reps = get_int ~default:0 (member "reps" meta) in
  let contended = get_bool ~default:false (member "contended" meta) in
  let timestamp = get_string ~default:"?" (member "timestamp" meta) in
  let benches = get_list (member "benchmarks" json) in
  Printf.printf
    "== M10 perf gate == results: %s (mode=%s reps=%d %s)\n\
     gate: engine/oracle ratio <= %.2f, geomean AND per-benchmark\n"
    timestamp mode reps
    (if contended then "CONTENDED" else "uncontended")
    !max_ratio;
  if contended then
    Printf.printf
      "** advisory: results were taken while a fuzz campaign ran — not a \
       clean gate measurement **\n";
  if String.equal mode "quick" then
    Printf.printf
      "** advisory: quick-mode (smoke) inputs — not the full gate corpus **\n";
  Printf.printf "\n%-16s %12s %12s %7s %11s %8s  %s\n" "benchmark"
    "engine(ms)" "oracle(ms)" "ratio" "raw-C(ms)" "eng/raw" "gate";
  Printf.printf "%s\n" (String.make 92 '-');
  let invalids = ref [] in
  let rows = ref [] (* (name, ratio) for valid benchmarks, in order *) in
  List.iter
    (fun b ->
      let name = get_string ~default:"?" (member "name" b) in
      if not (get_bool ~default:false (member "valid" b)) then (
        let reason = get_string ~default:"?" (member "invalid_reason" b) in
        invalids := (name, reason) :: !invalids;
        Printf.printf "%-16s %12s %12s %7s %11s %8s  INVALID: %s\n" name "-"
          "-" "-" "-" "-" reason)
      else
        let eng = get_float ~default:0. (member "engine_median_ms" b) in
        let orc = get_float ~default:0. (member "oracle_median_ms" b) in
        let raw = get_float ~default:0. (member "raw_c_median_ms" b) in
        let ratio = eng /. Float.max orc 1e-6 in
        let raw_ratio = eng /. Float.max raw 1e-6 in
        rows := (name, ratio) :: !rows;
        Printf.printf "%-16s %12.1f %12.1f %7.2f %11.1f %8.2f  %s\n" name eng
          orc ratio raw raw_ratio
          (if ratio <= !max_ratio then "pass" else "FAIL"))
    benches;
  Printf.printf "%s\n" (String.make 92 '-');
  let rows = List.rev !rows in
  let invalids = List.rev !invalids in
  let violations = List.filter (fun (_, r) -> r > !max_ratio) rows in
  let gm =
    match rows with
    | [] -> Float.nan
    | l ->
        exp
          (List.fold_left (fun acc (_, x) -> acc +. log x) 0. l
          /. float_of_int (List.length l))
  in
  let gm_ok = (not (Float.is_nan gm)) && gm <= !max_ratio in
  Printf.printf "geomean ratio: %.3f (max allowed %.2f) — %s\n" gm !max_ratio
    (if gm_ok then "pass" else "FAIL");
  (match
     List.fold_left
       (fun acc (n, r) ->
         match acc with
         | Some (_, r0) when r0 >= r -> acc
         | _ -> Some (n, r))
       None rows
   with
  | Some (n, r) -> Printf.printf "slowest benchmark: %s at %.2fx\n" n r
  | None -> ());
  let pass =
    match (invalids, violations, rows) with
    | [], [], _ :: _ -> gm_ok
    | _ -> false
  in
  (match invalids with
  | [] -> ()
  | l ->
      Printf.printf
        "\n** %d INVALID benchmark(s) — engine/oracle count mismatch or \
         compile failure (fuzz-grade bug): **\n"
        (List.length l);
      List.iter (fun (n, r) -> Printf.printf "   %s: %s\n" n r) l);
  (match violations with
  | [] -> ()
  | l ->
      Printf.printf "\nper-benchmark gate violations (> %.2fx):\n" !max_ratio;
      List.iter (fun (n, r) -> Printf.printf "   %s: %.2fx\n" n r) l);
  (match rows with
  | [] -> Printf.printf "\nno valid benchmarks in %s\n" !path
  | _ -> ());
  Printf.printf "\nVERDICT: %s\n"
    (if pass then "PASS — engine within the 2.0x gate"
     else "FAIL — gate not met (do not commit perf sign-off)");
  exit (if pass then 0 else 1)
