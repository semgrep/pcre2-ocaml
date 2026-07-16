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
    "usage: compare [RESULTS.json] [--max-ratio R] [--fast-gate-geomean G]\n\
     \  default results file: bench/results.json\n\
     \  --max-ratio R          engine/oracle gate (M10, informational since the\n\
     \                         2.159 sign-off); default 2.0\n\
     \  --fast-gate-geomean G  M11 G11.2 fast/oracle geomean gate; default 1.5.\n\
     \                         The per-benchmark fast<engine half is ALWAYS\n\
     \                         enforced; raise G (e.g. 99) to print the report\n\
     \                         without hard-failing on the geomean.\n";
  exit 64

let () =
  let path = ref "bench/results.json" in
  let max_ratio = ref 2.0 in
  (* M11 chunk N — G11.2 fast/oracle geomean gate (default 1.5); overridable so
     the orchestrator can run the report without hard-failing while a residual
     sign-off question is open (the per-bench fast<engine half stays enforced). *)
  let fast_gate_geomean = ref 1.5 in
  let argc = Array.length Sys.argv in
  let rec parse i =
    if i < argc then
      match Sys.argv.(i) with
      | "--max-ratio" when i + 1 < argc ->
          max_ratio := float_of_string Sys.argv.(i + 1);
          parse (i + 2)
      | "--fast-gate-geomean" when i + 1 < argc ->
          fast_gate_geomean := float_of_string Sys.argv.(i + 1);
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
    "== M11 G11.2 fast-engine perf gate == results: %s (mode=%s reps=%d %s)\n\
     ENFORCED: fast/oracle geomean <= %.2f AND fast < engine on every \
     benchmark.\n\
     engine/oracle ratio (M10, max %.2f) is INFORMATIONAL — closed by the \
     2.159 sign-off.\n"
    timestamp mode reps
    (if contended then "CONTENDED" else "uncontended")
    !fast_gate_geomean !max_ratio;
  if contended then
    Printf.printf
      "** advisory: results were taken while a fuzz campaign ran — not a \
       clean gate measurement **\n";
  if String.equal mode "quick" then
    Printf.printf
      "** advisory: quick-mode (smoke) inputs — not the full gate corpus **\n";
  Printf.printf "\n%-16s %12s %12s %7s %11s %8s %10s %7s  %s\n" "benchmark"
    "engine(ms)" "oracle(ms)" "ratio" "raw-C(ms)" "eng/raw" "fast(ms)" "fratio"
    "gate";
  Printf.printf "%s\n" (String.make 110 '-');
  let invalids = ref [] in
  let rows = ref [] (* (name, ratio) for valid benchmarks, in order *) in
  (* M11 chunk L — fast engine ratios are INFORMATIONAL (chunk N gates them). *)
  let fast_rows = ref [] in
  List.iter
    (fun b ->
      let name = get_string ~default:"?" (member "name" b) in
      if not (get_bool ~default:false (member "valid" b)) then (
        let reason = get_string ~default:"?" (member "invalid_reason" b) in
        invalids := (name, reason) :: !invalids;
        Printf.printf "%-16s %12s %12s %7s %11s %8s %10s %7s  INVALID: %s\n"
          name "-" "-" "-" "-" "-" "-" "-" reason)
      else
        let eng = get_float ~default:0. (member "engine_median_ms" b) in
        let orc = get_float ~default:0. (member "oracle_median_ms" b) in
        let raw = get_float ~default:0. (member "raw_c_median_ms" b) in
        let ratio = eng /. Float.max orc 1e-6 in
        let raw_ratio = eng /. Float.max raw 1e-6 in
        let fast_agrees = get_bool ~default:false (member "fast_agrees" b) in
        let fast_ratio =
          if fast_agrees then get_float ~default:0. (member "fast_ratio" b)
          else 0.
        in
        let fast_ms =
          if fast_agrees then get_float ~default:0. (member "fast_median_ms" b)
          else 0.
        in
        rows := (name, ratio) :: !rows;
        (* M11 chunk N — capture (name, fast/oracle ratio, fast ms, engine ms)
           so the fast gate can check both halves of G11.2. *)
        if fast_agrees then
          fast_rows := (name, fast_ratio, fast_ms, eng) :: !fast_rows;
        Printf.printf "%-16s %12.1f %12.1f %7.2f %11.1f %8.2f %10s %7s  %s\n"
          name eng orc ratio raw raw_ratio
          (if fast_agrees then
             Printf.sprintf "%.1f" (get_float ~default:0.
               (member "fast_median_ms" b))
           else "-")
          (if fast_agrees then Printf.sprintf "%.2f" fast_ratio else "-")
          (if ratio <= !max_ratio then "pass" else "FAIL"))
    benches;
  Printf.printf "%s\n" (String.make 110 '-');
  let rows = List.rev !rows in
  let fast_rows = List.rev !fast_rows in
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
  (* Engine/oracle (M10) numbers are now INFORMATIONAL: the M10 gate was closed
     by explicit sign-off at 2.159 (performance-report.md §4). The ENFORCED gate
     below is M11 G11.2 (fast/oracle). *)
  Printf.printf
    "engine/oracle geomean: %.3f (M10 max %.2f — %s; informational, closed by \
     the 2.159 sign-off)\n"
    gm !max_ratio
    (if gm_ok then "within" else "over");
  (* M11 chunk N — G11.2 fast-engine gate: fast/oracle geomean <= threshold AND
     fast strictly faster than the engine on EVERY supported benchmark. *)
  let fast_gm =
    match fast_rows with
    | [] -> Float.nan
    | l ->
        exp
          (List.fold_left (fun acc (_, r, _, _) -> acc +. log r) 0. l
          /. float_of_int (List.length l))
  in
  let fast_slower =
    List.filter (fun (_, _, fms, ems) -> fms >= ems) fast_rows
  in
  let fast_gm_ok =
    (not (Float.is_nan fast_gm)) && fast_gm <= !fast_gate_geomean
  in
  let fast_perbench_ok = match fast_slower with [] -> true | _ -> false in
  (match fast_rows with
  | [] -> Printf.printf "fast/oracle geomean: n/a (no in-subset benchmarks)\n"
  | l ->
      Printf.printf
        "fast/oracle geomean: %.3f over %d/%d benchmarks (G11.2 max %.2f — %s)\n"
        fast_gm (List.length l) (List.length rows) !fast_gate_geomean
        (if fast_gm_ok then "pass" else "FAIL"));
  Printf.printf "fast<engine on every benchmark: %s\n"
    (if fast_perbench_ok then "pass"
     else Printf.sprintf "FAIL (%d slower)" (List.length fast_slower));
  (match
     List.fold_left
       (fun acc (n, r, _, _) ->
         match acc with
         | Some (_, r0) when r0 >= r -> acc
         | _ -> Some (n, r))
       None fast_rows
   with
  | Some (n, r) -> Printf.printf "slowest fast benchmark: %s at %.2fx\n" n r
  | None -> ());
  (match invalids with
  | [] -> ()
  | l ->
      Printf.printf
        "\n** %d INVALID benchmark(s) — engine/oracle/fast count mismatch or \
         compile failure (fuzz-grade bug): **\n"
        (List.length l);
      List.iter (fun (n, r) -> Printf.printf "   %s: %s\n" n r) l);
  (match violations with
  | [] -> ()
  | l ->
      Printf.printf
        "\nengine/oracle per-benchmark over %.2fx (informational):\n" !max_ratio;
      List.iter (fun (n, r) -> Printf.printf "   %s: %.2fx\n" n r) l);
  (match fast_slower with
  | [] -> ()
  | l ->
      Printf.printf "\nfast NOT faster than engine (G11.2 violation):\n";
      List.iter
        (fun (n, _, fms, ems) ->
          Printf.printf "   %s: fast=%.1fms engine=%.1fms\n" n fms ems)
        l);
  (match rows with
  | [] -> Printf.printf "\nno valid benchmarks in %s\n" !path
  | _ -> ());
  (* The ENFORCED gate is M11 G11.2 (fast). Invalids (correctness) always fail. *)
  let pass =
    match (invalids, fast_rows) with
    | [], _ :: _ -> fast_gm_ok && fast_perbench_ok
    | _ -> false
  in
  Printf.printf "\nVERDICT: %s\n"
    (if pass then
       Printf.sprintf "PASS — fast engine meets G11.2 (geomean <= %.2f, fast < engine)"
         !fast_gate_geomean
     else "FAIL — G11.2 not met (do not commit perf sign-off)");
  exit (if pass then 0 else 1)
