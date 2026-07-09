(* Minimal s-expression reader (stdlib only) for skiplist.sexp and
   baseline_counts.sexp. Supports atoms, double-quoted strings (with
   backslash escapes), nested lists, and ';' line comments. *)

type t = Atom of string | List of t list

exception Parse_error of string

let parse_string (s : string) : t list =
  let n = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < n then Some s.[!pos] else None in
  let advance () = incr pos in
  let rec skip_ws () =
    match peek () with
    | Some (' ' | '\t' | '\n' | '\r') ->
        advance ();
        skip_ws ()
    | Some ';' ->
        while !pos < n && s.[!pos] <> '\n' do
          advance ()
        done;
        skip_ws ()
    | _ -> ()
  in
  let read_quoted () =
    advance ();
    (* opening quote *)
    let b = Buffer.create 16 in
    let rec go () =
      match peek () with
      | None -> raise (Parse_error "unterminated string")
      | Some '"' -> advance ()
      | Some '\\' ->
          advance ();
          (match peek () with
          | Some 'n' -> Buffer.add_char b '\n'
          | Some 't' -> Buffer.add_char b '\t'
          | Some c -> Buffer.add_char b c
          | None -> raise (Parse_error "unterminated escape"));
          advance ();
          go ()
      | Some c ->
          Buffer.add_char b c;
          advance ();
          go ()
    in
    go ();
    Buffer.contents b
  in
  let read_atom () =
    let b = Buffer.create 16 in
    let rec go () =
      match peek () with
      | Some c
        when not
               (c = '(' || c = ')' || c = '"' || c = ';' || c = ' ' || c = '\t'
              || c = '\n' || c = '\r') ->
          Buffer.add_char b c;
          advance ();
          go ()
      | _ -> ()
    in
    go ();
    Buffer.contents b
  in
  let rec read_sexp () : t =
    skip_ws ();
    match peek () with
    | None -> raise (Parse_error "unexpected end of input")
    | Some '(' ->
        advance ();
        let items = ref [] in
        let rec go () =
          skip_ws ();
          match peek () with
          | None -> raise (Parse_error "unterminated list")
          | Some ')' -> advance ()
          | _ ->
              items := read_sexp () :: !items;
              go ()
        in
        go ();
        List (List.rev !items)
    | Some ')' -> raise (Parse_error "unexpected ')'")
    | Some '"' -> Atom (read_quoted ())
    | Some _ -> Atom (read_atom ())
  in
  let out = ref [] in
  skip_ws ();
  while !pos < n do
    out := read_sexp () :: !out;
    skip_ws ()
  done;
  List.rev !out

let parse_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  parse_string s
