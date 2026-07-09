(* pcre2test-compatible CLI: pcre2test_ml [--driver=oracle] <input-file>
   ('-' or no file = stdin). Writes the pcre2test-style transcript to stdout.

   The output corresponds to pcre2test's non-quiet file mode minus the
   leading "PCRE2 version ..." banner (compare with `pcre2test -q`). *)

module H = Pcre2test_harness.Harness.Make (Pcre2_test_driver.Test_driver)

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
    if s.[i] = '\n' then begin
      lines := String.sub s !start (i - !start + 1) :: !lines;
      start := i + 1
    end
  done;
  if !start < n then lines := String.sub s !start (n - !start) :: !lines;
  List.rev !lines

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let file = ref "-" in
  List.iter
    (fun a ->
      if a = "--driver=oracle" then () (* only driver available today *)
      else if String.length a >= 9 && String.sub a 0 9 = "--driver=" then begin
        prerr_endline ("pcre2test_ml: unknown driver in '" ^ a ^ "'");
        exit 2
      end
      else file := a)
    args;
  let ic = if !file = "-" then stdin else open_in_bin !file in
  let lines = read_lines_with_newlines ic in
  if !file <> "-" then close_in ic;
  let t = H.create () in
  List.iter
    (fun line ->
      List.iter (fun out -> print_string (out ^ "\n")) (H.process_line t line))
    lines;
  List.iter (fun out -> print_string (out ^ "\n")) (H.finish t)
