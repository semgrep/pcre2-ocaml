(* Test-unit splitting and expected-output alignment for the conformance
   runner.

   A unit = one pattern (with its physical continuation lines) plus its data
   lines through the terminating blank line; '#' command lines, standalone
   comments and blank lines attach to the FOLLOWING unit as preamble. The
   unit ordinal is the 1-based index of pattern-units within the file. *)

module Scan = Pcre2test_harness.Scan
module Cstr = Pcre2test_harness.Cstr

type unit_t = {
  ordinal : int;
  lines : string list; (* raw input lines, newlines preserved *)
  pattern_line : string; (* first physical pattern line, chomped *)
}

let chomp line =
  let n = String.length line in
  if n > 0 && line.[n - 1] = '\n' then String.sub line 0 (n - 1) else line

(* Split a testinput file (list of raw lines) into units. Any trailing
   preamble lines with no following pattern are appended to the last unit
   (they are pure echo). *)
let split (lines : string list) : unit_t list =
  let units = ref [] in
  let preamble = ref [] in
  let current = ref [] in
  let pattern_line = ref "" in
  let ordinal = ref 0 in
  (* mode: `Top | `Pat of (delim, buffer) | `Subject *)
  let mode = ref `Top in
  let finish_unit () =
    incr ordinal;
    units :=
      {
        ordinal = !ordinal;
        lines = List.rev !current;
        pattern_line = !pattern_line;
      }
      :: !units;
    current := [];
    pattern_line := ""
  in
  List.iter
    (fun raw ->
      match !mode with
      | `Pat (delim, buf) ->
          current := raw :: !current;
          Buffer.add_string buf raw;
          if Scan.find_close ~delim (Buffer.contents buf) <> None then
            mode := `Subject
      | `Subject ->
          current := raw :: !current;
          if Scan.is_blank raw then begin
            finish_unit ();
            mode := `Top
          end
      | `Top ->
          if
            String.length raw > 0
            && raw.[0] <> '#'
            && Scan.is_delimiter raw.[0]
            && not (Scan.is_blank raw)
          then begin
            (* [current] is stored reversed; preamble is reversed too, so
               consing the pattern line onto it keeps file order *)
            current := raw :: !preamble;
            preamble := [];
            pattern_line := chomp raw;
            let delim = raw.[0] in
            match Scan.find_close ~delim raw with
            | Some _ -> mode := `Subject
            | None ->
                let buf = Buffer.create 128 in
                Buffer.add_string buf raw;
                mode := `Pat (delim, buf)
          end
          else preamble := raw :: !preamble)
    lines;
  (* flush *)
  (match !mode with
  | `Top -> ()
  | `Pat _ | `Subject ->
      (* unit ran to EOF without a terminating blank line *)
      finish_unit ());
  let units = List.rev !units in
  let trailing = List.rev !preamble in
  match (units, trailing) with
  | [], _ ->
      if trailing = [] then []
      else
        [ { ordinal = 1; lines = trailing; pattern_line = "" } ]
        (* degenerate: no patterns at all *)
  | _, [] -> units
  | _, _ ->
      (* append trailing echo-only lines to the last unit *)
      let rec upd = function
        | [] -> []
        | [ last ] -> [ { last with lines = last.lines @ trailing } ]
        | x :: rest -> x :: upd rest
      in
      upd units

(* Align expected-output lines against the input units.

   Every input line appears in the expected output, echoed in order (the
   result lines are inserted between them). Greedily match the ordered input
   line sequence as a subsequence of the expected lines; expected block k =
   expected lines from the match position of unit k's first input line up to
   (excluding) the match position of unit k+1's first input line. Comparison
   is done on chomped lines.

   Returns Ok blocks (one per unit, chomped) or Error message. *)
let align (units : unit_t list) (expected_lines : string list) :
    (string list list, string) result =
  let expected = Array.of_list (List.map chomp expected_lines) in
  let n = Array.length expected in
  let cursor = ref 0 in
  let unit_starts = ref [] in
  let error = ref None in
  List.iteri
    (fun ui u ->
      match !error with
      | Some _ -> ()
      | None ->
          List.iteri
            (fun li raw ->
              match !error with
              | Some _ -> ()
              | None ->
                  let want = chomp raw in
                  let j = ref !cursor in
                  while !j < n && not (String.equal expected.(!j) want) do
                    incr j
                  done;
                  if !j >= n then
                    error :=
                      Some
                        (Printf.sprintf
                           "alignment failed at unit %d, input line %d: %S \
                            not found in expected output after line %d"
                           u.ordinal (li + 1) want !cursor)
                  else begin
                    if li = 0 then unit_starts := !j :: !unit_starts;
                    cursor := !j + 1
                  end)
            u.lines;
          ignore ui)
    units;
  match !error with
  | Some e -> Error e
  | None ->
      let starts = Array.of_list (List.rev !unit_starts) in
      let k = Array.length starts in
      let blocks = ref [] in
      for i = k - 1 downto 0 do
        let lo = starts.(i) in
        let hi = if i = k - 1 then n else starts.(i + 1) in
        let block = ref [] in
        for j = hi - 1 downto lo do
          block := expected.(j) :: !block
        done;
        blocks := !block :: !blocks
      done;
      Ok !blocks
