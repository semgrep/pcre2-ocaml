(* Benchmark suite for the pure-OCaml PCRE2 engine — the M10 perf-gate
   corpus (docs/ocaml-engine/10-performance.md).

   The same workload runs through three drivers:
   - engine : Pcre2_engine.Engine (the pure-OCaml port) via its public seam;
   - oracle : Pcre2_test_driver.Test_driver — C libpcre2-8 through the
     dev-only FFI, i.e. measured through the same OCaml API shape as the
     engine so per-call FFI overhead (match-data create/free, ovector
     copy-out) is INCLUDED. This is the gate denominator per the plan.
   - raw-C  : bench_raw_* stubs (raw_c_stubs.c) running the whole scan loop
     inside C, one FFI crossing per repetition — the honesty number showing
     what the C library costs with no OCaml on top. Recorded, not gated.

   Protocol, per benchmark:
   1. compile on all three drivers (compile time is NOT measured);
   2. one warm-up rep per driver — this is also the correctness cross-check:
      engine and oracle match counts / extracted bytes / error codes MUST
      agree, else the benchmark is recorded INVALID (a fuzz-grade bug);
   3. N timed reps, interleaved engine/oracle/raw-C to neutralize
      thermal/cache drift (default N=5, --quick N=1);
   4. MEDIAN per driver; ratio = engine median / oracle median.

   The gate itself (geomean <= 2.0 AND per-benchmark <= 2.0) is enforced by
   compare.exe over the results.json this program writes. bench.exe exits 2
   if any benchmark is INVALID (correctness failure), 0 otherwise. *)

module E = Pcre2_engine.Engine
module O = Pcre2_engine.Options
module T = Pcre2_test_driver.Test_driver

(* ------------------------------------------------------------------ *)
(* Deterministic corpus generation (fixed-seed splitmix64)             *)

let mk_rng (seed : int64) : int -> int =
  let state = ref seed in
  let next64 () =
    state := Int64.add !state 0x9E3779B97F4A7C15L;
    let z = !state in
    let z =
      Int64.mul
        (Int64.logxor z (Int64.shift_right_logical z 30))
        0xBF58476D1CE4E5B9L
    in
    let z =
      Int64.mul
        (Int64.logxor z (Int64.shift_right_logical z 27))
        0x94D049BB133111EBL
    in
    Int64.logxor z (Int64.shift_right_logical z 31)
  in
  fun n ->
    if n <= 0 then invalid_arg "rng: bound must be positive";
    Int64.to_int
      (Int64.rem (Int64.shift_right_logical (next64 ()) 1) (Int64.of_int n))

let words =
  [|
    "the"; "and"; "with"; "from"; "there"; "which"; "would"; "about";
    "steamboat"; "adventure"; "riverbank"; "chapter"; "morning"; "afternoon";
    "wonderful"; "considerable"; "peculiar"; "expected"; "example"; "boxcar";
    "sixpence"; "calculate"; "fisherman"; "lantern"; "whitewash"; "island";
    "village"; "journey"; "current"; "channel"; "harbor"; "window"; "garden";
    "fence"; "cave"; "raft"; "treasure"; "widow"; "aunt"; "school";
  |]

(* Mixed prose with embedded emails / URIs / IPv4s / keywords / doubled
   words, all at fixed rates from the fixed-seed rng — every benchmark
   pattern finds real work in this one text. *)
let gen_text rng target =
  let buf = Buffer.create (target + 256) in
  let tlds = [| "com"; "org"; "net"; "io"; "edu" |] in
  let schemes = [| "http"; "https"; "ftp" |] in
  let keywords = [| "Twain"; "Huck"; "Sawyer"; "river"; "Mississippi" |] in
  let word () = words.(rng (Array.length words)) in
  let add_email () =
    Buffer.add_string buf
      (Printf.sprintf "%s.%s%d@%s-%s.%s" (word ()) (word ()) (rng 100)
         (word ()) (word ())
         tlds.(rng (Array.length tlds)))
  in
  let add_uri () =
    Buffer.add_string buf
      (Printf.sprintf "%s://www.%s%d.%s/%s/%s%s%s"
         schemes.(rng (Array.length schemes))
         (word ()) (rng 100)
         tlds.(rng (Array.length tlds))
         (word ()) (word ())
         (if rng 2 = 0 then
            Printf.sprintf "?%s=%d" (word ()) (rng 1000)
          else "")
         (if rng 3 = 0 then "#" ^ word () else ""))
  in
  let add_ipv4 () =
    Buffer.add_string buf
      (Printf.sprintf "%d.%d.%d.%d" (rng 256) (rng 256) (rng 256) (rng 256))
  in
  let words_in_sentence = ref 0 in
  let sentence_len = ref (6 + rng 9) in
  let sentences = ref 0 in
  while Buffer.length buf < target do
    (match rng 300 with
    | 0 | 1 -> add_email ()
    | 2 | 3 -> add_uri ()
    | 4 | 5 -> add_ipv4 ()
    | n when n < 11 ->
        Buffer.add_string buf keywords.(rng (Array.length keywords))
    | n when n < 17 ->
        (* doubled word — feeds the (\w+) \1 backref scan *)
        let w = word () in
        Buffer.add_string buf w;
        Buffer.add_char buf ' ';
        Buffer.add_string buf w
    | _ -> Buffer.add_string buf (word ()));
    incr words_in_sentence;
    if !words_in_sentence >= !sentence_len then (
      words_in_sentence := 0;
      sentence_len := 6 + rng 9;
      Buffer.add_char buf '.';
      incr sentences;
      if !sentences mod 5 = 0 then Buffer.add_char buf '\n'
      else Buffer.add_char buf ' ')
    else Buffer.add_char buf ' '
  done;
  Buffer.contents buf

let utf8_add buf cp =
  if cp < 0x80 then Buffer.add_char buf (Char.chr cp)
  else if cp < 0x800 then (
    Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
  else if cp < 0x10000 then (
    Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
  else (
    Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))

(* (base, span) ranges of contiguous letters per script — every generated
   "word" codepoint satisfies \p{L}. *)
let scripts =
  [|
    (Char.code 'a', 26) (* ASCII *);
    (0x00E0, 23) (* Latin-1 a-grave..o-diaeresis *);
    (0x03B1, 25) (* Greek alpha..omega *);
    (0x0430, 32) (* Cyrillic a..ya *);
    (0x05D0, 27) (* Hebrew alef..tav *);
    (0x0627, 36) (* Arabic alef..yeh *);
    (0x0905, 53) (* Devanagari a..ha *);
    (0x3042, 82) (* Hiragana *);
    (0x4E00, 512) (* CJK unified *);
  |]

let gen_utf_text rng target =
  let buf = Buffer.create (target + 256) in
  while Buffer.length buf < target do
    let base, span = scripts.(rng (Array.length scripts)) in
    let len = 2 + rng 7 in
    for _ = 1 to len do
      utf8_add buf (base + rng span)
    done;
    match rng 20 with
    | 0 -> Buffer.add_string buf ", "
    | 1 ->
        utf8_add buf 0x3002 (* ideographic full stop, category Po *);
        Buffer.add_char buf ' '
    | 2 -> Buffer.add_string buf (Printf.sprintf " %d " (rng 10000))
    | 3 -> Buffer.add_char buf '\n'
    | _ -> Buffer.add_char buf ' '
  done;
  Buffer.contents buf

(* semgrep-ish shape: many short subjects, one exec each. ~1/8 of the lines
   are headers/comments that do not match the request-line pattern. *)
let gen_lines rng n =
  let methods = [| "GET"; "POST"; "PUT"; "DELETE"; "HEAD"; "OPTIONS" |] in
  let word () = words.(rng (Array.length words)) in
  let lines = Array.make n "" in
  for i = 0 to n - 1 do
    lines.(i) <-
      (if rng 8 = 0 then
         match rng 3 with
         | 0 -> "Host: www." ^ word () ^ ".com"
         | 1 -> "Accept: */*"
         | _ -> "User-Agent: bench/1.0"
       else
         Printf.sprintf "%s /%s/%s/%d HTTP/1.%d"
           methods.(rng (Array.length methods))
           (word ()) (word ()) (rng 10000) (rng 2))
  done;
  lines

type corpus = {
  text : string;
  utf_text : string;
  lines : string array;
  lines_joined : string;
  patho : string;
}

let build_corpus ~quick =
  let text_target = if quick then 512 * 1024 else 5 * 1024 * 1024 in
  let utf_target = if quick then 256 * 1024 else 2 * 1024 * 1024 in
  let nlines = if quick then 20_000 else 200_000 in
  let text = gen_text (mk_rng 0x5EED_0001L) text_target in
  let utf_text = gen_utf_text (mk_rng 0x5EED_0002L) utf_target in
  let lines = gen_lines (mk_rng 0x5EED_0003L) nlines in
  {
    text;
    utf_text;
    lines;
    lines_joined = String.concat "\n" (Array.to_list lines);
    (* (a+)+b vs "a"*30 ^ "!": exponential backtracking until MATCH_LIMIT
       (10M ticks) — both engines must fail with error -47. *)
    patho = String.make 30 'a' ^ "!";
  }

(* ------------------------------------------------------------------ *)
(* Drivers: one API shape, three implementations                       *)

module type DRIVER = sig
  type code

  val id : string
  val compile : string -> int -> (code, string) result

  (* [exec code subject offset options] -> 1 match / 0 no-match / negative
     pcre2 error code; on a match, [last_start]/[last_end] hold
     ovector[0]/[1]. The refs avoid inventing a result type beyond what
     each underlying API already allocates. *)
  val exec : code -> string -> int -> int -> int
  val last_start : int ref
  val last_end : int ref
end

module Engine_driver : DRIVER = struct
  type code = E.t

  let id = "engine"
  let last_start = ref 0
  let last_end = ref 0

  let compile pattern options =
    (* Option bits cross the engine seam as int32 exactly once
       (port-conventions.md par. 3). *)
    match E.compile pattern (Int32.of_int options) with
    | Ok c -> Ok c
    | Error code ->
        Error (Printf.sprintf "compile error %d (%s)" code (E.error_message code))

  let exec code subject offset options =
    match E.exec code subject offset (Int32.of_int options) with
    | Ok (Some (s, e)) ->
        last_start := s;
        last_end := e;
        1
    | Ok None -> 0
    | Error rc -> rc
end

module Oracle_driver : DRIVER = struct
  type code = T.code

  let id = "oracle"
  let last_start = ref 0
  let last_end = ref 0

  let compile pattern options =
    match T.compile ~options pattern with
    | Ok c -> Ok c
    | Error { T.errcode; erroroffset } ->
        Error
          (Printf.sprintf "compile error %d at offset %d (%s)" errcode
             erroroffset (T.error_message errcode))

  let exec code subject offset options =
    let r = T.exec ~options ~subject ~offset code in
    if r.T.rc > 0 then (
      last_start := r.T.ovector.(0);
      last_end := r.T.ovector.(1);
      1)
    else if r.T.rc = -1 || r.T.rc = -2 then 0 (* NOMATCH / PARTIAL *)
    else r.T.rc
end

module Raw_c = struct
  type code

  external compile : string -> int -> (code, string) result
    = "bench_raw_compile"

  (* mode: 0 count_all, 1 single, 2 per_line, 3 count_all+extract *)
  external run : code -> string -> int -> int -> int = "bench_raw_run"
  external version : unit -> string = "bench_raw_version"
end

(* ------------------------------------------------------------------ *)
(* The measured scan loops (identical for both OCaml drivers, mirrored
   byte-for-byte in raw_c_stubs.c)                                     *)

type outcome = { count : int; extracted : int; error : int }

let outcome_equal a b =
  Int.equal a.count b.count
  && Int.equal a.extracted b.extracted
  && Int.equal a.error b.error

module Runner (D : DRIVER) = struct
  (* Find-all scan with pcre2test's bump semantics: empty match -> retry at
     the same offset with NOTEMPTY_ATSTART|ANCHORED; failed retry -> advance
     one code unit, UTF-aware (pcre2test.c:8293-8443, pcre2demo.c shape).
     DEVIATION from pcre2test's loop: for UTF patterns, calls after the
     first pass NO_UTF_CHECK — the first call validated the subject, and
     re-validating O(subject) bytes per match would benchmark the validator
     (quadratic), not the matcher. All three drivers do the same, so the
     ratio is unaffected. *)
  let count_all code subject ~utf ~extract =
    let len = String.length subject in
    let pos = ref 0 in
    let opts = ref 0 in
    let uck = ref 0 in
    let count = ref 0 in
    let extracted = ref 0 in
    let error = ref 0 in
    let stop = ref false in
    while not !stop do
      let rc = D.exec code subject !pos (!opts lor !uck) in
      if utf then uck := O.no_utf_check;
      if rc > 0 then (
        let s = !D.last_start and e = !D.last_end in
        incr count;
        if extract then (
          (* the find_iter shape: materialize the matched substring *)
          let m = String.sub subject s (e - s) in
          extracted := !extracted + String.length m);
        if s = e then
          if e >= len then stop := true
          else (
            opts := O.notempty_atstart lor O.anchored;
            pos := e)
        else (
          opts := 0;
          pos := e))
      else if rc = 0 then
        if !opts = 0 then stop := true
        else (
          incr pos;
          if utf then
            while
              !pos < len && Char.code subject.[!pos] land 0xc0 = 0x80
            do
              incr pos
            done;
          opts := 0;
          if !pos > len then stop := true)
      else (
        error := rc;
        stop := true)
    done;
    { count = !count; extracted = !extracted; error = !error }

  let per_line code lines =
    let count = ref 0 in
    let error = ref 0 in
    (try
       for i = 0 to Array.length lines - 1 do
         let rc = D.exec code lines.(i) 0 0 in
         if rc > 0 then incr count
         else if rc < 0 then (
           error := rc;
           raise Exit)
       done
     with Exit -> ());
    { count = !count; extracted = 0; error = !error }

  let single code subject =
    let rc = D.exec code subject 0 0 in
    if rc > 0 then { count = 1; extracted = 0; error = 0 }
    else if rc = 0 then { count = 0; extracted = 0; error = 0 }
    else { count = 0; extracted = 0; error = rc }
end

module ER = Runner (Engine_driver)
module OR = Runner (Oracle_driver)

let outcome_of_raw r =
  if r < 0 then { count = 0; extracted = 0; error = r }
  else { count = r; extracted = 0; error = 0 }

(* ------------------------------------------------------------------ *)
(* Benchmark descriptors — the corpus from the approved plan            *)

type kind = K_text | K_utf | K_lines | K_patho
type mode = M_count | M_single | M_lines | M_extract

let mode_name = function
  | M_count -> "count_all"
  | M_single -> "single_exec"
  | M_lines -> "per_line"
  | M_extract -> "find_all_extract"

type bench = {
  name : string;
  pattern : string;
  copts : int;
  kind : kind;
  mode : mode;
  utf : bool;
}

let benchmarks =
  [
    (* (a) mariomka regex-benchmark trio *)
    {
      name = "email";
      pattern = {|[\w\.+-]+@[\w\.-]+\.[\w\.-]+|};
      copts = 0;
      kind = K_text;
      mode = M_count;
      utf = false;
    };
    {
      name = "uri";
      pattern = {|[\w]+://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?|};
      copts = 0;
      kind = K_text;
      mode = M_count;
      utf = false;
    };
    {
      name = "ipv4";
      pattern =
        {|(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)|};
      copts = 0;
      kind = K_text;
      mode = M_count;
      utf = false;
    };
    (* (b) keyword alternation *)
    {
      name = "keywords";
      pattern = {|Twain|Huck|Sawyer|river|Mississippi|};
      copts = 0;
      kind = K_text;
      mode = M_count;
      utf = false;
    };
    (* (c) bounded repeats *)
    {
      name = "repeat_bounded";
      pattern = {|[a-z]{8,13}|};
      copts = 0;
      kind = K_text;
      mode = M_count;
      utf = false;
    };
    {
      name = "repeat_negclass";
      pattern = {|[a-q][^u-z]{13}x|};
      copts = 0;
      kind = K_text;
      mode = M_count;
      utf = false;
    };
    (* (d) backref-heavy scan *)
    {
      name = "backref";
      pattern = {|(\w+) \1|};
      copts = 0;
      kind = K_text;
      mode = M_count;
      utf = false;
    };
    (* (e) UTF \p{L}+ scan *)
    {
      name = "utf_letters";
      pattern = {|\p{L}+|};
      copts = O.utf lor O.ucp;
      kind = K_utf;
      mode = M_count;
      utf = true;
    };
    (* (f) pathological MATCHLIMIT exercise. NO_START_OPTIMIZE is required:
       with start optimization the required-code-unit scan ('b' absent from
       the subject) short-circuits to NOMATCH in microseconds on both sides
       and no backtracking ever runs. With it disabled, both engines must
       burn backtracking ticks to the SAME limit (error -47, MATCH_LIMIT =
       10M) — the benchmark measures the time to exhaust it. *)
    {
      name = "pathological";
      pattern = {|(a+)+b|};
      copts = O.no_start_optimize;
      kind = K_patho;
      mode = M_single;
      utf = false;
    };
    (* (g) many small subjects *)
    {
      name = "http_lines";
      pattern = {|^(GET|POST|PUT|DELETE) [^ ]+ HTTP|};
      copts = 0;
      kind = K_lines;
      mode = M_lines;
      utf = false;
    };
    (* (h) find-all iteration with substring extraction *)
    {
      name = "findall_email";
      pattern = {|[\w\.+-]+@[\w\.-]+\.[\w\.-]+|};
      copts = 0;
      kind = K_text;
      mode = M_extract;
      utf = false;
    };
    {
      name = "findall_utf";
      pattern = {|\p{L}+|};
      copts = O.utf lor O.ucp;
      kind = K_utf;
      mode = M_extract;
      utf = true;
    };
  ]

(* ------------------------------------------------------------------ *)
(* Measurement                                                          *)

let time_ms f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  ((Unix.gettimeofday () -. t0) *. 1000., r)

let median (a : float array) : float =
  let b = Array.copy a in
  Array.sort Float.compare b;
  let n = Array.length b in
  if n = 0 then 0.
  else if n mod 2 = 1 then b.(n / 2)
  else (b.((n / 2) - 1) +. b.(n / 2)) /. 2.

type bench_result = {
  b : bench;
  valid : bool;
  reason : string; (* "" when valid *)
  out : outcome;
  eng_ms : float array;
  orc_ms : float array;
  raw_ms : float array;
  eng_med : float;
  orc_med : float;
  raw_med : float;
  ratio : float;
  raw_ratio : float;
  raw_agrees : bool;
  eng_minor_words : float; (* median minor words allocated per engine rep *)
}

let show_outcome o =
  Printf.sprintf "{count=%d; extracted=%d; error=%d}" o.count o.extracted
    o.error

let run_bench ~reps corpus (b : bench) : bench_result =
  Printf.printf "  %-16s %!" b.name;
  Gc.compact ();
  let invalid reason =
    Printf.printf "INVALID: %s\n%!" reason;
    {
      b;
      valid = false;
      reason;
      out = { count = 0; extracted = 0; error = 0 };
      eng_ms = [||];
      orc_ms = [||];
      raw_ms = [||];
      eng_med = 0.;
      orc_med = 0.;
      raw_med = 0.;
      ratio = 0.;
      raw_ratio = 0.;
      raw_agrees = false;
      eng_minor_words = 0.;
    }
  in
  match Engine_driver.compile b.pattern b.copts with
  | Error msg -> invalid ("engine: " ^ msg)
  | Ok ecode -> (
      match Oracle_driver.compile b.pattern b.copts with
      | Error msg -> invalid ("oracle: " ^ msg)
      | Ok ocode -> (
          match Raw_c.compile b.pattern b.copts with
          | Error msg -> invalid ("raw-C: " ^ msg)
          | Ok rcode ->
              let subject =
                match b.kind with
                | K_text -> corpus.text
                | K_utf -> corpus.utf_text
                | K_lines -> corpus.lines_joined
                | K_patho -> corpus.patho
              in
              let eng_run () =
                match b.mode with
                | M_count -> ER.count_all ecode subject ~utf:b.utf ~extract:false
                | M_extract ->
                    ER.count_all ecode subject ~utf:b.utf ~extract:true
                | M_lines -> ER.per_line ecode corpus.lines
                | M_single -> ER.single ecode subject
              in
              let orc_run () =
                match b.mode with
                | M_count -> OR.count_all ocode subject ~utf:b.utf ~extract:false
                | M_extract ->
                    OR.count_all ocode subject ~utf:b.utf ~extract:true
                | M_lines -> OR.per_line ocode corpus.lines
                | M_single -> OR.single ocode subject
              in
              let raw_mode =
                match b.mode with
                | M_count -> 0
                | M_single -> 1
                | M_lines -> 2
                | M_extract -> 3
              in
              let raw_run () =
                outcome_of_raw
                  (Raw_c.run rcode subject raw_mode (if b.utf then 1 else 0))
              in
              (* warm-up + correctness cross-check *)
              let e0 = eng_run () in
              let o0 = orc_run () in
              let r0 = raw_run () in
              if not (outcome_equal e0 o0) then
                invalid
                  (Printf.sprintf "COUNT MISMATCH engine %s vs oracle %s"
                     (show_outcome e0) (show_outcome o0))
              else
                let raw_agrees =
                  Int.equal r0.count e0.count && Int.equal r0.error e0.error
                in
                let eng_ms = Array.make reps 0. in
                let orc_ms = Array.make reps 0. in
                let raw_ms = Array.make reps 0. in
                let eng_mw = Array.make reps 0. in
                let drift = ref "" in
                for i = 0 to reps - 1 do
                  let mw0 = Gc.minor_words () in
                  let t, oc = time_ms eng_run in
                  eng_mw.(i) <- Gc.minor_words () -. mw0;
                  eng_ms.(i) <- t;
                  if not (outcome_equal oc e0) then
                    drift := "engine result drifted across reps";
                  let t, oc = time_ms orc_run in
                  orc_ms.(i) <- t;
                  if not (outcome_equal oc o0) then
                    drift := "oracle result drifted across reps";
                  let t, _ = time_ms raw_run in
                  raw_ms.(i) <- t
                done;
                if String.length !drift > 0 then invalid !drift
                else
                  let eng_med = median eng_ms in
                  let orc_med = median orc_ms in
                  let raw_med = median raw_ms in
                  let ratio = eng_med /. Float.max orc_med 1e-6 in
                  let raw_ratio = eng_med /. Float.max raw_med 1e-6 in
                  Printf.printf
                    "count=%-8d engine=%8.1fms  oracle=%8.1fms  ratio=%5.2f  \
                     raw-C=%8.1fms%s\n\
                     %!"
                    e0.count eng_med orc_med ratio raw_med
                    (if raw_agrees then ""
                     else
                       Printf.sprintf "  ** raw-C DISAGREES: %s"
                         (show_outcome r0));
                  {
                    b;
                    valid = true;
                    reason = "";
                    out = e0;
                    eng_ms;
                    orc_ms;
                    raw_ms;
                    eng_med;
                    orc_med;
                    raw_med;
                    ratio;
                    raw_ratio;
                    raw_agrees;
                    eng_minor_words = median eng_mw;
                  }))

(* ------------------------------------------------------------------ *)
(* Reporting                                                            *)

let geomean = function
  | [] -> Float.nan
  | l ->
      exp
        (List.fold_left (fun acc x -> acc +. log x) 0. l
        /. float_of_int (List.length l))

let print_table results =
  Printf.printf "\n%-16s %10s %12s %12s %7s %11s %8s  %s\n" "benchmark"
    "count" "engine(ms)" "oracle(ms)" "ratio" "raw-C(ms)" "eng/raw" "status";
  Printf.printf "%s\n" (String.make 100 '-');
  List.iter
    (fun r ->
      if r.valid then
        Printf.printf "%-16s %10d %12.1f %12.1f %7.2f %11.1f %8.2f  %s\n"
          r.b.name r.out.count r.eng_med r.orc_med r.ratio r.raw_med
          r.raw_ratio
          (if r.raw_agrees then "ok" else "ok (raw-C count mismatch!)")
      else Printf.printf "%-16s %10s INVALID: %s\n" r.b.name "-" r.reason)
    results;
  Printf.printf "%s\n" (String.make 100 '-')

let iso_now () =
  let t = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (t.Unix.tm_year + 1900)
    (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min
    t.Unix.tm_sec

(* Space-separated PIDs of a running fuzz campaign ("" if none): numbers
   taken while fuzz_diff hogs cores are NOT clean gate measurements.
   -x on the comm name (not -f on the cmdline): shells/monitors whose
   command LINE mentions fuzz_diff must not count as contention. *)
let fuzz_pids () =
  try
    let ic =
      Unix.open_process_in
        "pgrep -x 'fuzz_diff.exe|fuzz_diff|pcre2-fuzz-diff' 2>/dev/null"
    in
    let buf = Buffer.create 64 in
    (try
       while true do
         Buffer.add_channel buf ic 1
       done
     with End_of_file -> ());
    ignore (Unix.close_process_in ic);
    String.concat " "
      (List.filter
         (fun s -> String.length s > 0)
         (String.split_on_char '\n' (String.trim (Buffer.contents buf))))
  with _ -> ""

let json_of_result (r : bench_result) : Bench_json.t =
  let open Bench_json in
  let floats a = List (Array.to_list (Array.map (fun x -> Num x) a)) in
  Obj
    ([
       ("name", Str r.b.name);
       ("pattern", Str r.b.pattern);
       ("compile_options", Num (float_of_int r.b.copts));
       ("mode", Str (mode_name r.b.mode));
       ("valid", Bool r.valid);
       ("invalid_reason", Str r.reason);
       ("count", Num (float_of_int r.out.count));
       ("extracted_bytes", Num (float_of_int r.out.extracted));
       ("error_rc", Num (float_of_int r.out.error));
       ("raw_c_agrees", Bool r.raw_agrees);
     ]
    @
    if not r.valid then []
    else
      [
        ("engine_ms", floats r.eng_ms);
        ("oracle_ms", floats r.orc_ms);
        ("raw_c_ms", floats r.raw_ms);
        ("engine_median_ms", Num r.eng_med);
        ("oracle_median_ms", Num r.orc_med);
        ("raw_c_median_ms", Num r.raw_med);
        ("ratio", Num r.ratio);
        ("engine_vs_raw_c", Num r.raw_ratio);
        ("engine_minor_words_per_rep", Num r.eng_minor_words);
      ])

let contains hay needle =
  let hl = String.length hay and nl = String.length needle in
  let rec go i =
    if i + nl > hl then false
    else if String.equal (String.sub hay i nl) needle then true
    else go (i + 1)
  in
  go 0

let usage () =
  print_string
    "usage: bench [--quick] [--reps N] [--out PATH] [--only SUBSTR]\n\
     \  --quick   1 rep, ~10x smaller inputs (CI smoke)\n\
     \  --reps N  override the rep count (default 5, 1 with --quick)\n\
     \  --out P   results file (default bench/results.json)\n\
     \  --only S  run only benchmarks whose name contains S\n";
  exit 64

let () =
  let quick = ref false in
  let out = ref "bench/results.json" in
  let reps_arg = ref 0 in
  let only = ref "" in
  let argc = Array.length Sys.argv in
  let rec parse i =
    if i < argc then
      match Sys.argv.(i) with
      | "--quick" ->
          quick := true;
          parse (i + 1)
      | "--reps" when i + 1 < argc ->
          reps_arg := int_of_string Sys.argv.(i + 1);
          parse (i + 2)
      | "--out" when i + 1 < argc ->
          out := Sys.argv.(i + 1);
          parse (i + 2)
      | "--only" when i + 1 < argc ->
          only := Sys.argv.(i + 1);
          parse (i + 2)
      | "--help" | "-h" -> usage ()
      | a ->
          Printf.eprintf "bench: unknown argument %s\n" a;
          usage ()
  in
  parse 1;
  let reps =
    if !reps_arg > 0 then !reps_arg else if !quick then 1 else 5
  in
  let pids = fuzz_pids () in
  let contended = String.length pids > 0 in
  let vmaj, vmin = E.version in
  Printf.printf
    "pcre2-ocaml bench — mode=%s reps=%d | engine %d.%d | C pcre2 %s | \
     OCaml %s\n"
    (if !quick then "quick" else "full")
    reps vmaj vmin (Raw_c.version ()) Sys.ocaml_version;
  if contended then
    Printf.printf
      "** CONTENDED: fuzz_diff campaign running (pids %s) — numbers are NOT \
       clean gate measurements **\n"
      pids;
  Printf.printf "generating corpus...%!";
  let corpus = build_corpus ~quick:!quick in
  Printf.printf " text=%dB utf=%dB lines=%d\n%!" (String.length corpus.text)
    (String.length corpus.utf_text)
    (Array.length corpus.lines);
  let selected =
    List.filter
      (fun b -> String.length !only = 0 || contains b.name !only)
      benchmarks
  in
  let results = List.map (run_bench ~reps corpus) selected in
  print_table results;
  let valid_ratios =
    List.filter_map (fun r -> if r.valid then Some r.ratio else None) results
  in
  let gm = geomean valid_ratios in
  Printf.printf "geomean ratio (engine/oracle, %d valid): %.3f%s\n"
    (List.length valid_ratios) gm
    (if contended then "  [CONTENDED]" else "");
  let json =
    Bench_json.Obj
      [
        ( "meta",
          Bench_json.Obj
            [
              ("suite", Bench_json.Str "pcre2-ocaml M10 bench");
              ("mode", Bench_json.Str (if !quick then "quick" else "full"));
              ("reps", Bench_json.Num (float_of_int reps));
              ("contended", Bench_json.Bool contended);
              ("fuzz_pids", Bench_json.Str pids);
              ("timestamp", Bench_json.Str (iso_now ()));
              ( "engine_version",
                Bench_json.Str (Printf.sprintf "%d.%d" vmaj vmin) );
              ("c_pcre2_version", Bench_json.Str (Raw_c.version ()));
              ("ocaml_version", Bench_json.Str Sys.ocaml_version);
              ( "text_bytes",
                Bench_json.Num (float_of_int (String.length corpus.text)) );
              ( "utf_bytes",
                Bench_json.Num (float_of_int (String.length corpus.utf_text))
              );
              ( "http_lines",
                Bench_json.Num (float_of_int (Array.length corpus.lines)) );
              ("gate_max_ratio", Bench_json.Num 2.0);
            ] );
        ("benchmarks", Bench_json.List (List.map json_of_result results));
      ]
  in
  let oc = open_out !out in
  output_string oc (Bench_json.to_string json);
  close_out oc;
  Printf.printf "results written to %s\n" !out;
  let invalids = List.filter (fun r -> not r.valid) results in
  match invalids with
  | [] -> ()
  | _ ->
      Printf.printf
        "\n\
         ** CORRECTNESS CROSS-CHECK FAILED (fuzz-grade bug — engine and \
         oracle disagree): **\n";
      List.iter
        (fun r -> Printf.printf "   %s: %s\n" r.b.name r.reason)
        invalids;
      exit 2
