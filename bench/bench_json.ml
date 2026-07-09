(* Minimal JSON writer/reader shared by bench.exe (writer) and compare.exe
   (reader). Dev-only tooling (package pcre2-dev); deliberately
   dependency-free like the rest of the repo's test infrastructure. Only the
   subset bench.ml emits is supported (in particular \uXXXX escapes are
   decoded for the BMP only — the writer never emits anything above 0x1f). *)

type t =
  | Null
  | Bool of bool
  | Num of float
  | Str of string
  | List of t list
  | Obj of (string * t) list

(* ------------------------------------------------------------------ *)
(* Writer                                                              *)

let add_escaped buf s =
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s

let add_num buf x =
  if Float.is_nan x || Float.is_integer x = false then
    Buffer.add_string buf (Printf.sprintf "%.6f" x)
  else if Float.abs x < 1e15 then
    Buffer.add_string buf (Printf.sprintf "%.0f" x)
  else Buffer.add_string buf (Printf.sprintf "%.6e" x)

let to_string (v : t) : string =
  let buf = Buffer.create 4096 in
  let indent n =
    for _ = 1 to n do
      Buffer.add_string buf "  "
    done
  in
  let rec write depth v =
    match v with
    | Null -> Buffer.add_string buf "null"
    | Bool true -> Buffer.add_string buf "true"
    | Bool false -> Buffer.add_string buf "false"
    | Num x -> add_num buf x
    | Str s ->
        Buffer.add_char buf '"';
        add_escaped buf s;
        Buffer.add_char buf '"'
    | List [] -> Buffer.add_string buf "[]"
    | List items ->
        Buffer.add_string buf "[\n";
        List.iteri
          (fun i item ->
            if i > 0 then Buffer.add_string buf ",\n";
            indent (depth + 1);
            write (depth + 1) item)
          items;
        Buffer.add_char buf '\n';
        indent depth;
        Buffer.add_char buf ']'
    | Obj [] -> Buffer.add_string buf "{}"
    | Obj kvs ->
        Buffer.add_string buf "{\n";
        List.iteri
          (fun i (k, item) ->
            if i > 0 then Buffer.add_string buf ",\n";
            indent (depth + 1);
            Buffer.add_char buf '"';
            add_escaped buf k;
            Buffer.add_string buf "\": ";
            write (depth + 1) item)
          kvs;
        Buffer.add_char buf '\n';
        indent depth;
        Buffer.add_char buf '}'
  in
  write 0 v;
  Buffer.add_char buf '\n';
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* Reader (recursive descent)                                          *)

exception Parse_error of string

let parse (s : string) : t =
  let n = String.length s in
  let pos = ref 0 in
  let fail msg =
    raise (Parse_error (Printf.sprintf "%s at byte %d" msg !pos))
  in
  let peek () = if !pos < n then s.[!pos] else '\000' in
  let skip_ws () =
    while
      !pos < n
      && (match s.[!pos] with ' ' | '\t' | '\n' | '\r' -> true | _ -> false)
    do
      incr pos
    done
  in
  let expect c =
    if !pos < n && Char.equal s.[!pos] c then incr pos
    else fail (Printf.sprintf "expected '%c'" c)
  in
  let literal w v =
    let lw = String.length w in
    if !pos + lw <= n && String.equal (String.sub s !pos lw) w then (
      pos := !pos + lw;
      v)
    else fail "bad literal"
  in
  let parse_string () =
    expect '"';
    let b = Buffer.create 16 in
    let finished = ref false in
    while not !finished do
      if !pos >= n then fail "unterminated string";
      match s.[!pos] with
      | '"' ->
          incr pos;
          finished := true
      | '\\' ->
          incr pos;
          if !pos >= n then fail "truncated escape";
          (match s.[!pos] with
          | '"' -> Buffer.add_char b '"'
          | '\\' -> Buffer.add_char b '\\'
          | '/' -> Buffer.add_char b '/'
          | 'n' -> Buffer.add_char b '\n'
          | 't' -> Buffer.add_char b '\t'
          | 'r' -> Buffer.add_char b '\r'
          | 'b' -> Buffer.add_char b '\b'
          | 'f' -> Buffer.add_char b '\012'
          | 'u' ->
              if !pos + 4 >= n then fail "truncated \\u escape";
              let cp =
                try int_of_string ("0x" ^ String.sub s (!pos + 1) 4)
                with _ -> fail "bad \\u escape"
              in
              pos := !pos + 4;
              (* UTF-8 encode (BMP only; the writer emits only \u00xx). *)
              if cp < 0x80 then Buffer.add_char b (Char.chr cp)
              else if cp < 0x800 then (
                Buffer.add_char b (Char.chr (0xC0 lor (cp lsr 6)));
                Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))))
              else (
                Buffer.add_char b (Char.chr (0xE0 lor (cp lsr 12)));
                Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
                Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))))
          | _ -> fail "bad escape");
          incr pos
      | c ->
          Buffer.add_char b c;
          incr pos
    done;
    Buffer.contents b
  in
  let parse_number () =
    let start = !pos in
    while
      !pos < n
      &&
      match s.[!pos] with
      | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> true
      | _ -> false
    do
      incr pos
    done;
    match float_of_string_opt (String.sub s start (!pos - start)) with
    | Some x -> Num x
    | None -> fail "bad number"
  in
  let rec parse_value () =
    skip_ws ();
    match peek () with
    | '{' -> parse_obj ()
    | '[' -> parse_list ()
    | '"' -> Str (parse_string ())
    | 't' -> literal "true" (Bool true)
    | 'f' -> literal "false" (Bool false)
    | 'n' -> literal "null" Null
    | '-' | '0' .. '9' -> parse_number ()
    | _ -> fail "unexpected character"
  and parse_list () =
    expect '[';
    skip_ws ();
    if Char.equal (peek ()) ']' then (
      incr pos;
      List [])
    else
      let rec items acc =
        let v = parse_value () in
        skip_ws ();
        match peek () with
        | ',' ->
            incr pos;
            items (v :: acc)
        | ']' ->
            incr pos;
            List (List.rev (v :: acc))
        | _ -> fail "expected ',' or ']'"
      in
      items []
  and parse_obj () =
    expect '{';
    skip_ws ();
    if Char.equal (peek ()) '}' then (
      incr pos;
      Obj [])
    else
      let rec items acc =
        skip_ws ();
        let k = parse_string () in
        skip_ws ();
        expect ':';
        let v = parse_value () in
        skip_ws ();
        match peek () with
        | ',' ->
            incr pos;
            items ((k, v) :: acc)
        | '}' ->
            incr pos;
            Obj (List.rev ((k, v) :: acc))
        | _ -> fail "expected ',' or '}'"
      in
      items []
  in
  let v = parse_value () in
  skip_ws ();
  if !pos <> n then fail "trailing garbage";
  v

(* ------------------------------------------------------------------ *)
(* Accessors                                                           *)

let member key = function
  | Obj kvs -> (
      match List.find_opt (fun (k, _) -> String.equal k key) kvs with
      | Some (_, v) -> v
      | None -> Null)
  | _ -> Null

let get_float ?default v =
  match (v, default) with
  | Num x, _ -> x
  | _, Some d -> d
  | _ -> invalid_arg "Bench_json.get_float"

let get_int ?default v =
  match (v, default) with
  | Num x, _ -> int_of_float x
  | _, Some d -> d
  | _ -> invalid_arg "Bench_json.get_int"

let get_string ?default v =
  match (v, default) with
  | Str s, _ -> s
  | _, Some d -> d
  | _ -> invalid_arg "Bench_json.get_string"

let get_bool ?default v =
  match (v, default) with
  | Bool b, _ -> b
  | _, Some d -> d
  | _ -> invalid_arg "Bench_json.get_bool"

let get_list v = match v with List l -> l | _ -> []
