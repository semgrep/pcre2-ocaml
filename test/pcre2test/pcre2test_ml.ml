(* pcre2test-compatible CLI: pcre2test_ml [--driver=oracle|engine] <input-file>
   ('-' or no file = stdin). Writes the pcre2test-style transcript to stdout.

   Drivers: oracle (default) = the C PCRE2 10.44 library; engine = the pure
   OCaml engine (Pcre2_engine.Engine via the engine_driver seam adapter).

   The output corresponds to pcre2test's non-quiet file mode minus the
   leading "PCRE2 version ..." banner (compare with `pcre2test -q`). *)

module H_oracle = Pcre2test_harness.Harness.Make (Pcre2_test_driver.Test_driver)

module H_engine =
  Pcre2test_harness.Harness.Make (Pcre2test_harness.Engine_driver)

let read_lines_with_newlines ic =
  (* Preserve exact line framing: keep '\n'; the final line may lack one. *)
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let s = Buffer.contents buf in
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

let run (type t) ~(create : unit -> t)
    ~(process_line : t -> string -> string list) ~(finish : t -> string list)
    lines =
  let t = create () in
  List.iter
    (fun line ->
      List.iter (fun out -> print_string (out ^ "\n")) (process_line t line))
    lines;
  List.iter (fun out -> print_string (out ^ "\n")) (finish t)

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let file = ref "-" in
  let driver = ref `Oracle in
  List.iter
    (fun a ->
      if a = "--driver=oracle" then driver := `Oracle
      else if a = "--driver=engine" then driver := `Engine
      else if String.length a >= 9 && String.sub a 0 9 = "--driver=" then (
        prerr_endline ("pcre2test_ml: unknown driver in '" ^ a ^ "'");
        exit 2)
      else file := a)
    args;
  let ic = if !file = "-" then stdin else open_in_bin !file in
  let lines = read_lines_with_newlines ic in
  if !file <> "-" then close_in ic;
  match !driver with
  | `Oracle ->
      run ~create:H_oracle.create ~process_line:H_oracle.process_line
        ~finish:H_oracle.finish lines
  | `Engine ->
      run ~create:H_engine.create ~process_line:H_engine.process_line
        ~finish:H_engine.finish lines
