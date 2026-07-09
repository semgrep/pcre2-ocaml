(* gen_tables — parse the machine-generated PCRE2 10.44 table sources in
   vendor/pcre2/src/ and emit the committed OCaml tables in src/engine/:

     pcre2_chartables.c.dist -> chartables.ml
     pcre2_ucd.c             -> ucd_tables.ml
     pcre2_ucptables.c       -> ucptables.ml   (PT_* / ucp_* values resolved
                                from pcre2_internal.h / pcre2_ucp.h)

   Dev-only tool (never shipped); stdlib only. The parsers are line/token
   based and FAIL LOUDLY on anything unexpected rather than skipping input.
   See gen/README.md for the regen command; `dune build @gen-check` asserts
   the committed files are in sync. *)

let failf fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("gen_tables: error: " ^ s);
      exit 1)
    fmt

(* ------------------------------------------------------------------ *)
(* Input helpers                                                       *)
(* ------------------------------------------------------------------ *)

let read_file path =
  let ic =
    try open_in_bin path with Sys_error m -> failf "cannot open %s" m
  in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* 1-based line number of byte offset [idx] in [text]. *)
let line_of text idx =
  let n = ref 1 in
  for i = 0 to idx - 1 do
    if text.[i] = '\n' then incr n
  done;
  !n

(* Index of the first occurrence of [pat] at or after [pos]; -1 if none. *)
let find_from text pos pat =
  let tl = String.length text and pl = String.length pat in
  let limit = tl - pl in
  let rec eq i j = j = pl || (text.[i + j] = pat.[j] && eq i (j + 1)) in
  let rec go i = if i > limit then -1 else if eq i 0 then i else go (i + 1) in
  go (max 0 pos)

(* Remove all C block comments, preserving newlines. *)
let strip_comments ~what s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if !i + 1 < n && s.[!i] = '/' && s.[!i + 1] = '*' then begin
      let close = find_from s (!i + 2) "*/" in
      if close < 0 then failf "%s: unterminated comment" what;
      for j = !i to close + 1 do
        if s.[j] = '\n' then Buffer.add_char b '\n'
      done;
      i := close + 2
    end
    else begin
      Buffer.add_char b s.[!i];
      incr i
    end
  done;
  Buffer.contents b

let is_ident_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_'

(* Split a comment-free array body into tokens: numbers (decimal/hex,
   optional sign, optional u/U suffix), identifiers, and braces. Commas and
   whitespace separate; anything else is fatal. *)
let tokenize ~what s =
  let toks = ref [] in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = ',' then incr i
    else if c = '{' || c = '}' then begin
      toks := String.make 1 c :: !toks;
      incr i
    end
    else if c = '-' || is_ident_char c then begin
      let j = ref (!i + 1) in
      while !j < n && is_ident_char s.[!j] do
        incr j
      done;
      toks := String.sub s !i (!j - !i) :: !toks;
      i := !j
    end
    else failf "%s: unexpected character %C" what c
  done;
  List.rev !toks

let int_of_tok ~what t =
  if String.equal t "NOTACHAR" then 0xffffffff (* pcre2_internal.h:223 *)
  else begin
    let l = String.length t in
    let t' =
      if l > 1 && (t.[l - 1] = 'u' || t.[l - 1] = 'U') then
        String.sub t 0 (l - 1)
      else t
    in
    match int_of_string_opt t' with
    | Some v -> v
    | None -> failf "%s: not an integer token: %S" what t
  end

(* 1-based inclusive line range in a vendored C source. *)
type span = { l0 : int; l1 : int }

(* Body between the '{' following [decl] and the next "};", with comments
   stripped. Also returns the line range decl..terminator for citations. *)
let extract_array text ~what ~after decl =
  let d = find_from text after decl in
  if d < 0 then failf "%s: declaration %S not found" what decl;
  let ob = find_from text (d + String.length decl) "{" in
  if ob < 0 then failf "%s: no '{' after %S" what decl;
  let cb = find_from text ob "};" in
  if cb < 0 then failf "%s: no \"};\" terminating %S" what decl;
  let body = String.sub text (ob + 1) (cb - ob - 1) in
  (strip_comments ~what body, { l0 = line_of text d; l1 = line_of text cb })

let split_ws s =
  let parts = ref [] and b = Buffer.create 16 in
  let flush () =
    if Buffer.length b > 0 then begin
      parts := Buffer.contents b :: !parts;
      Buffer.clear b
    end
  in
  String.iter
    (fun c -> if c = ' ' || c = '\t' || c = '\r' then flush () else Buffer.add_char b c)
    s;
  flush ();
  List.rev !parts

(* ------------------------------------------------------------------ *)
(* pcre2_chartables.c.dist                                             *)
(* ------------------------------------------------------------------ *)

type chartables = {
  ct_data : int array; (* 1088 bytes: lcc @0, fcc @256, cbits @512, ctypes @832 *)
  ct_all : span;
  ct_lcc : span;
  ct_fcc : span;
  ct_cbits : span;
  ct_ctypes : span;
}

let parse_chartables path =
  let what = Filename.basename path in
  let text = read_file path in
  let body, all =
    extract_array text ~what ~after:0 "const uint8_t PRIV(default_tables)[] ="
  in
  let vals = List.map (int_of_tok ~what) (tokenize ~what body) in
  let n = List.length vals in
  if n <> 1088 then failf "%s: expected 1088 bytes in default_tables, got %d" what n;
  let data = Array.of_list vals in
  Array.iteri
    (fun i v -> if v < 0 || v > 255 then failf "%s: byte %d out of range: %d" what i v)
    data;
  let marker pat =
    let idx = find_from text 0 pat in
    if idx < 0 then failf "%s: marker %S not found" what pat;
    line_of text idx
  in
  let m1 = marker "This table is a lower casing table." in
  let m2 = marker "This table is a case flipping table." in
  let m3 = marker "This table contains bit maps" in
  let m4 = marker "This table identifies various classes" in
  if not (all.l0 < m1 && m1 < m2 && m2 < m3 && m3 < m4 && m4 < all.l1) then
    failf "%s: table section markers out of order" what;
  {
    ct_data = data;
    ct_all = all;
    ct_lcc = { l0 = m1; l1 = m2 - 1 };
    ct_fcc = { l0 = m2; l1 = m3 - 1 };
    ct_cbits = { l0 = m3; l1 = m4 - 1 };
    ct_ctypes = { l0 = m4; l1 = all.l1 };
  }

(* ------------------------------------------------------------------ *)
(* pcre2_ucd.c                                                         *)
(* ------------------------------------------------------------------ *)

type ucd = {
  version : string;
  records : int array array; (* nrecords x 7 fields, C order *)
  rec_span : span;
  stage1 : int array;
  s1_span : span;
  stage2 : int array;
  s2_span : span;
  caseless : int array;
  cl_span : span;
  digits : int array;
  dg_span : span;
  script_sets : int array;
  ss_span : span;
  boolprop_sets : int array;
  bp_span : span;
}

let parse_ucd path =
  let what = Filename.basename path in
  let text = read_file path in
  (* The dummy !SUPPORT_UNICODE tables at the top reuse the same declarations;
     anchor all searches after the unicode_version definition. *)
  let anchor = find_from text 0 "const char *PRIV(unicode_version)" in
  if anchor < 0 then failf "%s: unicode_version not found" what;
  let version =
    let q0 = String.index_from text anchor '"' in
    let q1 = String.index_from text (q0 + 1) '"' in
    String.sub text (q0 + 1) (q1 - q0 - 1)
  in
  let ints decl =
    let body, span = extract_array text ~what ~after:anchor decl in
    let vals = List.map (int_of_tok ~what) (tokenize ~what body) in
    (Array.of_list vals, span)
  in
  let caseless, cl_span = ints "const uint32_t PRIV(ucd_caseless_sets)[] =" in
  let digits, dg_span = ints "const uint32_t PRIV(ucd_digit_sets)[] =" in
  let script_sets, ss_span = ints "const uint32_t PRIV(ucd_script_sets)[] =" in
  let boolprop_sets, bp_span = ints "const uint32_t PRIV(ucd_boolprop_sets)[] =" in
  let stage1, s1_span = ints "const uint16_t PRIV(ucd_stage1)[] =" in
  let stage2, s2_span = ints "const uint16_t PRIV(ucd_stage2)[] =" in
  let rbody, rec_span =
    extract_array text ~what ~after:anchor "const ucd_record PRIV(ucd_records)[] ="
  in
  let records =
    let rec go acc = function
      | [] -> List.rev acc
      | "{" :: a :: b :: c :: d :: e :: f :: g :: "}" :: tl ->
          let r = Array.map (int_of_tok ~what) [| a; b; c; d; e; f; g |] in
          go (r :: acc) tl
      | t :: _ -> failf "%s: unexpected token %S in ucd_records" what t
    in
    Array.of_list (go [] (tokenize ~what rbody))
  in
  let nrec = Array.length records in
  if nrec = 0 then failf "%s: no ucd_records parsed" what;
  Array.iteri
    (fun i r ->
      (* uint8 script/chartype/gbprop/caseset; int32 other_case; uint16
         scriptx_bidiclass/bprops — pcre2_internal.h:1852-1860 *)
      for f = 0 to 3 do
        if r.(f) < 0 || r.(f) > 255 then
          failf "%s: record %d field %d out of uint8 range: %d" what i f r.(f)
      done;
      if r.(4) < -0x8000_0000 || r.(4) > 0x7fff_ffff then
        failf "%s: record %d other_case out of int32 range: %d" what i r.(4);
      for f = 5 to 6 do
        if r.(f) < 0 || r.(f) > 0xffff then
          failf "%s: record %d field %d out of uint16 range: %d" what i f r.(f)
      done)
    records;
  (* GET_UCD covers code points 0..0x10ffff with UCD_BLOCK_SIZE = 128
     (pcre2_internal.h:1864-1867). *)
  if Array.length stage1 <> 0x110000 / 128 then
    failf "%s: stage1 has %d entries, expected %d" what (Array.length stage1)
      (0x110000 / 128);
  if Array.length stage2 = 0 || Array.length stage2 mod 128 <> 0 then
    failf "%s: stage2 length %d not a multiple of 128" what (Array.length stage2);
  let nblocks = Array.length stage2 / 128 in
  Array.iteri
    (fun i v ->
      if v < 0 || v >= nblocks then failf "%s: stage1[%d] = %d out of range" what i v)
    stage1;
  Array.iteri
    (fun i v ->
      if v < 0 || v >= nrec then failf "%s: stage2[%d] = %d out of range" what i v)
    stage2;
  if Array.length caseless = 0 || caseless.(0) <> 0xffffffff then
    failf "%s: ucd_caseless_sets must start with NOTACHAR" what;
  if caseless.(Array.length caseless - 1) <> 0xffffffff then
    failf "%s: ucd_caseless_sets must end with NOTACHAR" what;
  if Array.length digits = 0 || digits.(0) <> Array.length digits - 1 then
    failf "%s: ucd_digit_sets count header mismatch" what;
  {
    version;
    records;
    rec_span;
    stage1;
    s1_span;
    stage2;
    s2_span;
    caseless;
    cl_span;
    digits;
    dg_span;
    script_sets;
    ss_span;
    boolprop_sets;
    bp_span;
  }

(* ------------------------------------------------------------------ *)
(* pcre2_ucp.h — ucp_* enums and *_item_size defines                   *)
(* ------------------------------------------------------------------ *)

type ucp_info = {
  ucp_map : (string, int) Hashtbl.t;
  enum_sizes : int array; (* members per enum, in file order *)
  boolprop_item_size : int;
  script_item_size : int;
}

let strip_line_comment ~what line =
  let b = Buffer.create (String.length line) in
  let n = String.length line in
  let i = ref 0 in
  while !i < n do
    if !i + 1 < n && line.[!i] = '/' && line.[!i + 1] = '*' then begin
      let close = find_from line (!i + 2) "*/" in
      if close < 0 then failf "%s: comment spans lines inside an enum: %S" what line;
      i := close + 2
    end
    else begin
      Buffer.add_char b line.[!i];
      incr i
    end
  done;
  Buffer.contents b

let parse_ucp path =
  let what = Filename.basename path in
  let text = read_file path in
  let lines = String.split_on_char '\n' text in
  let map = Hashtbl.create 512 in
  let sizes = ref [] in
  let cur = ref None in
  let define name =
    let pat = "#define " ^ name in
    let rec find = function
      | [] -> failf "%s: %s not found" what pat
      | l :: tl -> (
          if String.length l >= String.length pat
             && String.equal (String.sub l 0 (String.length pat)) pat
          then
            match split_ws (strip_line_comment ~what l) with
            | [ "#define"; _; v ] -> int_of_tok ~what v
            | _ -> failf "%s: cannot parse %S" what l
          else find tl)
    in
    find lines
  in
  List.iter
    (fun raw ->
      match !cur with
      | None -> if String.equal (String.trim raw) "enum {" then cur := Some (ref [])
      | Some members ->
          let t = String.trim (strip_line_comment ~what raw) in
          if String.equal t "};" then begin
            let ms = List.rev !members in
            List.iteri
              (fun i name ->
                if Hashtbl.mem map name then failf "%s: duplicate enum name %s" what name;
                Hashtbl.add map name i)
              ms;
            sizes := List.length ms :: !sizes;
            cur := None
          end
          else if not (String.equal t "") then begin
            let name =
              if t.[String.length t - 1] = ',' then
                String.trim (String.sub t 0 (String.length t - 1))
              else t
            in
            if not (String.equal name "") then begin
              String.iter
                (fun c ->
                  if not (is_ident_char c) then
                    failf "%s: unexpected enum member line %S" what raw)
                name;
              members := name :: !members
            end
          end)
    lines;
  if !cur <> None then failf "%s: unterminated enum" what;
  let enum_sizes = Array.of_list (List.rev !sizes) in
  if Array.length enum_sizes <> 6 then
    failf "%s: expected 6 enums (gentype, chartype, boolprop, bidi, gbprop, script), got %d"
      what (Array.length enum_sizes);
  {
    ucp_map = map;
    enum_sizes;
    boolprop_item_size = define "ucd_boolprop_sets_item_size";
    script_item_size = define "ucd_script_sets_item_size";
  }

(* ------------------------------------------------------------------ *)
(* pcre2_internal.h — PT_* defines                                     *)
(* ------------------------------------------------------------------ *)

let parse_pt path =
  let what = Filename.basename path in
  let text = read_file path in
  let map = Hashtbl.create 32 in
  List.iter
    (fun line ->
      match split_ws line with
      | "#define" :: name :: value :: _
        when String.length name > 3 && String.equal (String.sub name 0 3) "PT_" -> (
          match int_of_string_opt value with
          | Some v -> Hashtbl.replace map name v
          | None -> failf "%s: cannot parse value of %s: %S" what name value)
      | _ -> ())
    (String.split_on_char '\n' text);
  if Hashtbl.length map = 0 then failf "%s: no PT_* defines found" what;
  map

(* ------------------------------------------------------------------ *)
(* pcre2_ucptables.c — utt_names + utt                                 *)
(* ------------------------------------------------------------------ *)

type utt_entry = {
  ue_name : string;
  ue_ptype : int;
  ue_pvalue : int;
  ue_ptype_c : string; (* original C tokens, for review comments *)
  ue_pvalue_c : string;
}

type utt = {
  entries : utt_entry array;
  utt_span : span;
  names_span : span;
  size_line : int;
}

let parse_ucptables path ~ucp ~pt =
  let what = Filename.basename path in
  let text = read_file path in
  let lines = String.split_on_char '\n' text in
  (* STRING_xxx0 macros: "#define STRING_xxx0 STR_x ... "\0"" *)
  let macros = Hashtbl.create 1024 in
  List.iter
    (fun line ->
      let pfx = "#define STRING_" in
      if String.length line > String.length pfx
         && String.equal (String.sub line 0 (String.length pfx)) pfx
      then begin
        match split_ws line with
        | "#define" :: name :: body ->
            let b = Buffer.create 32 in
            List.iter
              (fun tok ->
                if String.equal tok "\"\\0\"" then Buffer.add_char b '\000'
                else if String.equal tok "STR_AMPERSAND" then
                  (* pcre2_internal.h:812; used by STRING_l_AMPERSAND0 ("l&") *)
                  Buffer.add_char b '&'
                else if String.length tok = 5
                        && String.equal (String.sub tok 0 4) "STR_"
                        && is_ident_char tok.[4]
                then Buffer.add_char b tok.[4]
                else failf "%s: unexpected token %S in %s" what tok name)
              body;
            let s = Buffer.contents b in
            if String.length s = 0 || s.[String.length s - 1] <> '\000' then
              failf "%s: %s does not end with a NUL" what name;
            if Hashtbl.mem macros name then failf "%s: duplicate macro %s" what name;
            Hashtbl.add macros name s
        | _ -> failf "%s: cannot parse macro line %S" what line
      end)
    lines;
  (* utt_names: a ';'-terminated concatenation of the STRING_ macros. *)
  let names_decl = "const char PRIV(utt_names)[] =" in
  let nd = find_from text 0 names_decl in
  if nd < 0 then failf "%s: %S not found" what names_decl;
  let semi = String.index_from text (nd + String.length names_decl) ';' in
  let names_body = String.sub text (nd + String.length names_decl)
      (semi - nd - String.length names_decl) in
  let names =
    let b = Buffer.create 4096 in
    List.iter
      (fun tok ->
        match Hashtbl.find_opt macros tok with
        | Some s -> Buffer.add_string b s
        | None -> failf "%s: utt_names references unknown macro %S" what tok)
      (tokenize ~what names_body);
    Buffer.contents b
  in
  let name_at off =
    if off < 0 || off >= String.length names then
      failf "%s: utt name offset %d out of range" what off;
    match String.index_from_opt names off '\000' with
    | Some e -> String.sub names off (e - off)
    | None -> failf "%s: unterminated utt name at offset %d" what off
  in
  (* utt entries: { <offset>, PT_*, <ucp_*|int> } *)
  let ubody, utt_span =
    extract_array text ~what ~after:0 "const ucp_type_table PRIV(utt)[] ="
  in
  let entries =
    let entry off ptok vtok =
      let ue_ptype =
        match Hashtbl.find_opt pt ptok with
        | Some v -> v
        | None -> failf "%s: unknown property type %S" what ptok
      in
      let ue_pvalue =
        if String.length vtok > 4 && String.equal (String.sub vtok 0 4) "ucp_" then
          match Hashtbl.find_opt ucp.ucp_map vtok with
          | Some v -> v
          | None -> failf "%s: unknown ucp value %S" what vtok
        else int_of_tok ~what vtok
      in
      {
        ue_name = name_at (int_of_tok ~what off);
        ue_ptype;
        ue_pvalue;
        ue_ptype_c = ptok;
        ue_pvalue_c = vtok;
      }
    in
    let rec go acc = function
      | [] -> List.rev acc
      | "{" :: off :: ptok :: vtok :: "}" :: tl -> go (entry off ptok vtok :: acc) tl
      | t :: _ -> failf "%s: unexpected token %S in utt" what t
    in
    Array.of_list (go [] (tokenize ~what ubody))
  in
  if Array.length entries = 0 then failf "%s: no utt entries parsed" what;
  (* The table is binary-chopped: names must be strictly ascending. *)
  for i = 1 to Array.length entries - 1 do
    if String.compare entries.(i - 1).ue_name entries.(i).ue_name >= 0 then
      failf "%s: utt not strictly sorted at %S / %S" what entries.(i - 1).ue_name
        entries.(i).ue_name
  done;
  let size_decl = "const size_t PRIV(utt_size)" in
  let sd = find_from text 0 size_decl in
  if sd < 0 then failf "%s: %S not found" what size_decl;
  {
    entries;
    utt_span;
    names_span = { l0 = line_of text nd; l1 = line_of text semi };
    size_line = line_of text sd;
  }

(* ------------------------------------------------------------------ *)
(* Emission helpers                                                    *)
(* ------------------------------------------------------------------ *)

let gen_header src =
  Printf.sprintf
    "(* Generated by gen/gen_tables.exe from vendor/pcre2/src/%s (PCRE2 10.44). DO NOT EDIT. *)\n"
    src

(* let <name> =
     "\x.." 16 bytes per line, using string-literal line continuations. *)
let add_bytes_lit buf ~name get n =
  Printf.bprintf buf "let %s =\n  \"" name;
  for i = 0 to n - 1 do
    if i > 0 && i mod 16 = 0 then Buffer.add_string buf "\\\n   ";
    let v = get i in
    if v < 0 || v > 255 then failf "emit %s: byte %d out of range: %d" name i v;
    Printf.bprintf buf "\\x%02x" v
  done;
  Buffer.add_string buf "\"\n"

(* Pack an int array of uint16 values as a 2-bytes-per-entry little-endian
   string literal. *)
let add_uint16_le_lit buf ~name (a : int array) =
  add_bytes_lit buf ~name
    (fun i ->
      let v = a.(i / 2) in
      if i mod 2 = 0 then v land 0xff else (v lsr 8) land 0xff)
    (2 * Array.length a)

let add_int_array buf ~name ~per_line ~pp (a : int array) =
  Printf.bprintf buf "let %s = [|\n" name;
  let n = Array.length a in
  let line = Buffer.create 128 in
  Array.iteri
    (fun i v ->
      Printf.bprintf line " %s;" (pp v);
      if (i + 1) mod per_line = 0 || i = n - 1 then begin
        Buffer.add_char buf ' ';
        Buffer.add_buffer buf line;
        Buffer.add_char buf '\n';
        Buffer.clear line
      end)
    a;
  Buffer.add_string buf "|]\n"

let dec v = string_of_int v
let hex v = Printf.sprintf "0x%04x" v
let hex8 v = Printf.sprintf "0x%08x" v

(* ------------------------------------------------------------------ *)
(* chartables.ml                                                       *)
(* ------------------------------------------------------------------ *)

let emit_chartables ct =
  let b = Buffer.create (1 lsl 15) in
  Buffer.add_string b (gen_header "pcre2_chartables.c.dist");
  Printf.bprintf b
    "\n\
     (* The four default character tables from PRIV(default_tables)\n\
    \   (pcre2_chartables.c.dist:%d-%d), C locale, split at the offsets given\n\
    \   by pcre2_internal.h:606-609. Used only for characters < 256. *)\n\n"
    ct.ct_all.l0 ct.ct_all.l1;
  Printf.bprintf b "(* pcre2_chartables.c.dist:%d-%d - lower casing table (256 bytes). *)\n"
    ct.ct_lcc.l0 ct.ct_lcc.l1;
  add_bytes_lit b ~name:"lcc_tab" (fun i -> ct.ct_data.(i)) 256;
  Printf.bprintf b "\n(* pcre2_chartables.c.dist:%d-%d - case flipping table (256 bytes). *)\n"
    ct.ct_fcc.l0 ct.ct_fcc.l1;
  add_bytes_lit b ~name:"fcc_tab" (fun i -> ct.ct_data.(256 + i)) 256;
  Printf.bprintf b
    "\n\
     (* pcre2_chartables.c.dist:%d-%d - bit maps for the POSIX-ish character\n\
    \   classes: space, xdigit, digit, upper, lower, word, graph, print,\n\
    \   punct, cntrl. 10 maps x 32 bytes; bits run from the least significant\n\
    \   end of each byte. *)\n"
    ct.ct_cbits.l0 ct.ct_cbits.l1;
  add_bytes_lit b ~name:"cbits_tab" (fun i -> ct.ct_data.(512 + i)) 320;
  Printf.bprintf b
    "\n(* pcre2_chartables.c.dist:%d-%d - character type bits (256 bytes). *)\n"
    ct.ct_ctypes.l0 ct.ct_ctypes.l1;
  add_bytes_lit b ~name:"ctypes_tab" (fun i -> ct.ct_data.(832 + i)) 256;
  Buffer.add_string b
    "\n\
     (* safe: caller passes a code unit 0..255; lcc_tab has 256 bytes. *)\n\
     let lcc c = Char.code (String.unsafe_get lcc_tab c)\n\n\
     (* safe: caller passes a code unit 0..255; fcc_tab has 256 bytes. *)\n\
     let fcc c = Char.code (String.unsafe_get fcc_tab c)\n\n\
     (* safe: caller passes cbit_* + (c lsr 3) <= 288 + 31; cbits_tab has 320\n\
    \   bytes. *)\n\
     let cbits i = Char.code (String.unsafe_get cbits_tab i)\n\n\
     (* safe: caller passes a code unit 0..255; ctypes_tab has 256 bytes. *)\n\
     let ctypes c = Char.code (String.unsafe_get ctypes_tab c)\n\n\
     (* pcre2_internal.h:581-591 - offsets of the class bit maps in cbits. *)\n\
     let cbit_space = 0 (* [:space:] or \\s *)\n\
     let cbit_xdigit = 32 (* [:xdigit:] *)\n\
     let cbit_digit = 64 (* [:digit:] or \\d *)\n\
     let cbit_upper = 96 (* [:upper:] *)\n\
     let cbit_lower = 128 (* [:lower:] *)\n\
     let cbit_word = 160 (* [:word:] or \\w *)\n\
     let cbit_graph = 192 (* [:graph:] *)\n\
     let cbit_print = 224 (* [:print:] *)\n\
     let cbit_punct = 256 (* [:punct:] *)\n\
     let cbit_cntrl = 288 (* [:cntrl:] *)\n\
     let cbit_length = 320 (* length of the cbits table *)\n\n\
     (* pcre2_internal.h:597-601 - bit definitions for ctypes entries. *)\n\
     let ctype_space = 0x01\n\
     let ctype_letter = 0x02\n\
     let ctype_lcletter = 0x04\n\
     let ctype_digit = 0x08\n\
     let ctype_word = 0x10 (* alphanumeric or '_' *)\n\n\
     (* pcre2_internal.h:606-610 - offsets from the base tables pointer, for\n\
    \   code that mirrors the C's single-block table layout. *)\n\
     let lcc_offset = 0\n\
     let fcc_offset = 256\n\
     let cbits_offset = 512\n\
     let ctypes_offset = 832\n\
     let tables_length = 1088\n";
  Buffer.contents b

(* ------------------------------------------------------------------ *)
(* ucd_tables.ml                                                       *)
(* ------------------------------------------------------------------ *)

let emit_ucd u ~version_line =
  let b = Buffer.create (1 lsl 20) in
  let nrec = Array.length u.records in
  Buffer.add_string b (gen_header "pcre2_ucd.c");
  Printf.bprintf b
    "\n\
     (* pcre2_ucd.c:%d *)\n\
     let unicode_version = %S\n\n\
     (* pcre2_internal.h:1864 *)\n\
     let ucd_block_size = 128\n\n\
     (* pcre2_internal.h:1864-1892 - UCD access scheme, for the future ucd.ml.\n\
    \   The ucd_record for code point ch (0 <= ch <= 0x10ffff) is record\n\
    \     let i = stage2 ((stage1 (ch / ucd_block_size)) * ucd_block_size\n\
    \                     + ch mod ucd_block_size)\n\
    \   with fields script i, chartype i, gbprop i, caseset i,\n\
    \   other_case.(i), scriptx_bidiclass i, bprops i.\n\
    \   Packed uint16 fields (pcre2_internal.h:1876-1882):\n\
    \     script extension offset = scriptx_bidiclass i land 0x3ff\n\
    \                               (UCD_SCRIPTX_MASK)\n\
    \     bidi class              = scriptx_bidiclass i lsr 11\n\
    \                               (UCD_BIDICLASS_SHIFT)\n\
    \     bool properties offset  = bprops i land 0xfff (UCD_BPROPS_MASK)\n\
    \   UCD_OTHERCASE(ch) = ch + other_case.(i) (pcre2_internal.h:1889). *)\n\n\
     let nrecords = %d\n\n\
     (* pcre2_ucd.c:%d-%d - PRIV(ucd_records), %d 12-byte records, stored as\n\
    \   parallel per-field tables indexed by record number (field layout:\n\
    \   pcre2_internal.h:1852-1860). *)\n\n"
    version_line u.version nrec u.rec_span.l0 u.rec_span.l1 nrec;
  let field name f =
    Printf.bprintf b "(* uint8 %s of each ucd_record, one byte per record. *)\n" name;
    add_bytes_lit b ~name:(name ^ "_tab") (fun i -> u.records.(i).(f)) nrec;
    Printf.bprintf b
      "\n\
       (* safe: i is a record number 0..nrecords-1 (a stage2 value; every\n\
      \   stage2 entry < nrecords, validated at generation). *)\n\
       let %s i = Char.code (String.unsafe_get %s_tab i)\n\n"
      name name
  in
  field "script" 0;
  field "chartype" 1;
  field "gbprop" 2;
  field "caseset" 3;
  Buffer.add_string b
    "(* int32 other_case of each ucd_record: signed offset to the other case,\n\
    \   or 0 if none. *)\n";
  add_int_array b ~name:"other_case" ~per_line:10 ~pp:dec
    (Array.map (fun r -> r.(4)) u.records);
  let field16 name f =
    Printf.bprintf b
      "\n(* uint16 %s of each ucd_record, 2 bytes little-endian per record. *)\n" name;
    add_uint16_le_lit b ~name:(name ^ "_tab") (Array.map (fun r -> r.(f)) u.records);
    Printf.bprintf b
      "\n\
       (* safe: i is a record number 0..nrecords-1 (a stage2 value; every\n\
      \   stage2 entry < nrecords, validated at generation). *)\n\
       let %s i =\n\
      \  Char.code (String.unsafe_get %s_tab (2 * i))\n\
      \  lor (Char.code (String.unsafe_get %s_tab ((2 * i) + 1)) lsl 8)\n"
      name name name
  in
  field16 "scriptx_bidiclass" 5;
  field16 "bprops" 6;
  Printf.bprintf b
    "\n\
     (* pcre2_ucd.c:%d-%d - PRIV(ucd_stage1), %d uint16 entries, 2 bytes\n\
    \   little-endian each. *)\n"
    u.s1_span.l0 u.s1_span.l1 (Array.length u.stage1);
  add_uint16_le_lit b ~name:"stage1_tab" u.stage1;
  Printf.bprintf b
    "\n\
     (* safe: i = ch / ucd_block_size for ch <= 0x10ffff, so i <= %d and\n\
    \   stage1_tab has %d bytes. *)\n\
     let stage1 i =\n\
    \  Char.code (String.unsafe_get stage1_tab (2 * i))\n\
    \  lor (Char.code (String.unsafe_get stage1_tab ((2 * i) + 1)) lsl 8)\n"
    (Array.length u.stage1 - 1)
    (2 * Array.length u.stage1);
  Printf.bprintf b
    "\n\
     (* pcre2_ucd.c:%d-%d - PRIV(ucd_stage2), %d uint16 entries (%d blocks of\n\
    \   ucd_block_size), 2 bytes little-endian each. *)\n"
    u.s2_span.l0 u.s2_span.l1 (Array.length u.stage2)
    (Array.length u.stage2 / 128);
  add_uint16_le_lit b ~name:"stage2_tab" u.stage2;
  Printf.bprintf b
    "\n\
     (* safe: i = stage1 (ch / ucd_block_size) * ucd_block_size\n\
    \          + ch mod ucd_block_size for ch <= 0x10ffff; every stage1 entry\n\
    \   < %d (validated at generation), so i < %d and stage2_tab has %d\n\
    \   bytes. *)\n\
     let stage2 i =\n\
    \  Char.code (String.unsafe_get stage2_tab (2 * i))\n\
    \  lor (Char.code (String.unsafe_get stage2_tab ((2 * i) + 1)) lsl 8)\n"
    (Array.length u.stage2 / 128)
    (Array.length u.stage2)
    (2 * Array.length u.stage2);
  Printf.bprintf b
    "\n\
     (* pcre2_ucd.c:%d-%d - PRIV(ucd_caseless_sets): lists of characters that\n\
    \   are caseless sets of more than one character, each list terminated by\n\
    \   NOTACHAR (0xffffffff). *)\n"
    u.cl_span.l0 u.cl_span.l1;
  add_int_array b ~name:"ucd_caseless_sets" ~per_line:8 ~pp:hex u.caseless;
  Printf.bprintf b
    "\n\
     (* pcre2_ucd.c:%d-%d - PRIV(ucd_digit_sets): code points of the '9'\n\
    \   characters in each set of decimal digits; entry 0 is the count of\n\
    \   subsequent values. *)\n"
    u.dg_span.l0 u.dg_span.l1;
  add_int_array b ~name:"ucd_digit_sets" ~per_line:8 ~pp:hex u.digits;
  Printf.bprintf b
    "\n\
     (* pcre2_ucd.c:%d-%d - PRIV(ucd_script_sets): script bitsets for the\n\
    \   Script Extension property, ucd_script_sets_item_size (= 3) words\n\
    \   each. *)\n"
    u.ss_span.l0 u.ss_span.l1;
  add_int_array b ~name:"ucd_script_sets" ~per_line:6 ~pp:hex8 u.script_sets;
  Printf.bprintf b
    "\n\
     (* pcre2_ucd.c:%d-%d - PRIV(ucd_boolprop_sets): bitsets for Boolean\n\
    \   properties, ucd_boolprop_sets_item_size (= 2) words each. *)\n"
    u.bp_span.l0 u.bp_span.l1;
  add_int_array b ~name:"ucd_boolprop_sets" ~per_line:6 ~pp:hex8 u.boolprop_sets;
  Buffer.contents b

(* ------------------------------------------------------------------ *)
(* ucptables.ml                                                        *)
(* ------------------------------------------------------------------ *)

let emit_ucptables t =
  let b = Buffer.create (1 lsl 16) in
  Buffer.add_string b (gen_header "pcre2_ucptables.c");
  Printf.bprintf b
    "\n\
     (* pcre2_ucptables.c:%d-%d - PRIV(utt), with the name offsets into\n\
    \   PRIV(utt_names) (pcre2_ucptables.c:%d-%d) resolved to the name\n\
    \   strings. Entries are (name, ptype, pvalue): ptype is a PT_* constant\n\
    \   (pcre2_internal.h:1293-1306), pvalue a ucp_* enum value (pcre2_ucp.h,\n\
    \   see ucp.ml). Searched by binary chop, so names are in strictly\n\
    \   ascending order; they are lower cased with underscores removed, per\n\
    \   Unicode \"loose matching\". *)\n\
     let utt : (string * int * int) array = [|\n"
    t.utt_span.l0 t.utt_span.l1 t.names_span.l0 t.names_span.l1;
  Array.iter
    (fun e ->
      Printf.bprintf b "  (%S, %d, %d); (* %s, %s *)\n" e.ue_name e.ue_ptype e.ue_pvalue
        e.ue_ptype_c e.ue_pvalue_c)
    t.entries;
  Printf.bprintf b
    "|]\n\n(* pcre2_ucptables.c:%d *)\nlet utt_size = %d\n"
    t.size_line (Array.length t.entries);
  Buffer.contents b

(* ------------------------------------------------------------------ *)
(* Sanity checks (--check): known Unicode facts, 10.44 sizes           *)
(* ------------------------------------------------------------------ *)

let run_sanity ct u t ucp pt =
  let ok what cond = if not cond then failf "sanity check failed: %s" what in
  let ucp_v name =
    match Hashtbl.find_opt ucp.ucp_map name with
    | Some v -> v
    | None -> failf "sanity: unknown ucp enum %s" name
  in
  let pt_v name =
    match Hashtbl.find_opt pt name with
    | Some v -> v
    | None -> failf "sanity: unknown PT_ constant %s" name
  in
  (* chartables *)
  let lcc i = ct.ct_data.(i)
  and fcc i = ct.ct_data.(256 + i)
  and cbits i = ct.ct_data.(512 + i)
  and ctypes i = ct.ct_data.(832 + i) in
  ok "lcc 'A' = 'a'" (lcc 0x41 = 0x61);
  ok "lcc 'a' = 'a'" (lcc 0x61 = 0x61);
  ok "fcc 'a' = 'A'" (fcc 0x61 = 0x41);
  ok "fcc 'A' = 'a'" (fcc 0x41 = 0x61);
  ok "ctypes 'a' has letter bit" (ctypes 0x61 land 0x02 <> 0);
  ok "ctypes 'a' has lcletter bit" (ctypes 0x61 land 0x04 <> 0);
  ok "ctypes 'A' has letter, not lcletter" (ctypes 0x41 land 0x06 = 0x02);
  ok "ctypes '5' has digit+word bits" (ctypes 0x35 land 0x18 = 0x18);
  ok "ctypes ' ' has space bit" (ctypes 0x20 land 0x01 <> 0);
  ok "cbit_digit has '5'" (cbits (64 + (0x35 / 8)) land (1 lsl (0x35 mod 8)) <> 0);
  ok "cbit_space has LF" (cbits (0 + (0x0a / 8)) land (1 lsl (0x0a mod 8)) <> 0);
  ok "cbit_word has '_'" (cbits (160 + (0x5f / 8)) land (1 lsl (0x5f mod 8)) <> 0);
  (* ucp / PT spot values (pin the enum and #define parses) *)
  ok "ucp_Latin = 0" (ucp_v "ucp_Latin" = 0);
  ok "ucp_Greek = 1" (ucp_v "ucp_Greek" = 1);
  ok "ucp_Lu = 9" (ucp_v "ucp_Lu" = 9);
  ok "ucp_Ll = 5" (ucp_v "ucp_Ll" = 5);
  ok "ucp_gbOther = 12" (ucp_v "ucp_gbOther" = 12);
  ok "ucp_bidiL = 9" (ucp_v "ucp_bidiL" = 9);
  ok "ucp_Bprop_Count = 52" (ucp_v "ucp_Bprop_Count" = 52);
  ok "ucp_Script_Count = 164" (ucp_v "ucp_Script_Count" = 164);
  ok "6 ucp enums sized 7/30/53/23/15/165"
    (ucp.enum_sizes = [| 7; 30; 53; 23; 15; 165 |]);
  ok "PT_ANY = 0" (pt_v "PT_ANY" = 0);
  ok "PT_SC = 4" (pt_v "PT_SC" = 4);
  ok "PT_SCX = 5" (pt_v "PT_SCX" = 5);
  ok "PT_BOOL = 13" (pt_v "PT_BOOL" = 13);
  (* ucd: 10.44 table sizes *)
  let nrec = Array.length u.records in
  ok "unicode_version = 15.0.0" (String.equal u.version "15.0.0");
  ok "nrecords = 1423" (nrec = 1423);
  ok "stage1 size = 8704" (Array.length u.stage1 = 8704);
  ok "stage2 size = 39040" (Array.length u.stage2 = 39040);
  ok "script_sets multiple of item size (3)"
    (ucp.script_item_size = 3 && Array.length u.script_sets mod 3 = 0);
  ok "boolprop_sets multiple of item size (2)"
    (ucp.boolprop_item_size = 2 && Array.length u.boolprop_sets mod 2 = 0);
  (* two-stage GET_UCD lookups (pcre2_internal.h:1865-1867) *)
  let get_ucd ch = u.records.(u.stage2.((u.stage1.(ch / 128) * 128) + (ch mod 128))) in
  let ra = get_ucd 0x41 in
  ok "U+0041 other_case -> U+0061" (0x41 + ra.(4) = 0x61);
  ok "U+0041 chartype = ucp_Lu" (ra.(1) = ucp_v "ucp_Lu");
  ok "U+0041 script = ucp_Latin" (ra.(0) = ucp_v "ucp_Latin");
  ok "U+0041 bidi class = ucp_bidiL" (ra.(5) lsr 11 = ucp_v "ucp_bidiL");
  let rb = get_ucd 0x61 in
  ok "U+0061 other_case -> U+0041" (0x61 + rb.(4) = 0x41);
  ok "U+0061 chartype = ucp_Ll" (rb.(1) = ucp_v "ucp_Ll");
  let rg = get_ucd 0x393 in
  ok "U+0393 script = ucp_Greek" (rg.(0) = ucp_v "ucp_Greek");
  ok "U+0393 other_case -> U+03B3" (0x393 + rg.(4) = 0x3b3);
  (* U+212A KELVIN SIGN: cased via its multichar caseless set, so its
     other_case offset is 0 and its caseset field is nonzero. *)
  let rk = get_ucd 0x212a in
  ok "U+212A other_case offset = 0" (rk.(4) = 0);
  ok "U+212A has a caseset" (rk.(3) > 0);
  (* caseless set containing Kelvin also has K and k *)
  let cl = u.caseless in
  let ki = ref (-1) in
  Array.iteri (fun i v -> if v = 0x212a then ki := i) cl;
  ok "Kelvin present in a caseless set" (!ki >= 0);
  let s = ref !ki in
  while cl.(!s - 1) <> 0xffffffff do
    decr s
  done;
  let e = ref !ki in
  while cl.(!e) <> 0xffffffff do
    incr e
  done;
  let in_set v =
    let found = ref false in
    for i = !s to !e - 1 do
      if cl.(i) = v then found := true
    done;
    !found
  in
  ok "Kelvin caseless set has 'K'" (in_set 0x4b);
  ok "Kelvin caseless set has 'k'" (in_set 0x6b);
  (* the record's caseset field is the offset of its set's first element *)
  ok "U+212A caseset offset" (rk.(3) = !s);
  (* utt *)
  let find name =
    let r = ref None in
    Array.iter (fun x -> if String.equal x.ue_name name then r := Some x) t.entries;
    match !r with Some x -> x | None -> failf "sanity: utt has no entry %S" name
  in
  ok "utt_size = 489" (Array.length t.entries = 489);
  let greek = find "greek" in
  ok "utt \"greek\" is (PT_SCX, ucp_Greek)"
    (greek.ue_ptype = pt_v "PT_SCX" && greek.ue_pvalue = ucp_v "ucp_Greek");
  let any = find "any" in
  ok "utt \"any\" is (PT_ANY, 0)" (any.ue_ptype = pt_v "PT_ANY" && any.ue_pvalue = 0);
  let xan = find "xan" in
  ok "utt \"xan\" is (PT_ALNUM, 0)" (xan.ue_ptype = pt_v "PT_ALNUM" && xan.ue_pvalue = 0);
  let lamp = find "l&" in
  ok "utt \"l&\" is (PT_LAMP, 0)" (lamp.ue_ptype = pt_v "PT_LAMP" && lamp.ue_pvalue = 0);
  (* every pvalue is in range for its ptype *)
  Array.iter
    (fun x ->
      let bound =
        if x.ue_ptype = pt_v "PT_SC" || x.ue_ptype = pt_v "PT_SCX" then
          Some (ucp_v "ucp_Script_Count")
        else if x.ue_ptype = pt_v "PT_BOOL" then Some (ucp_v "ucp_Bprop_Count")
        else if x.ue_ptype = pt_v "PT_GC" then Some 7
        else if x.ue_ptype = pt_v "PT_PC" then Some 30
        else if x.ue_ptype = pt_v "PT_BIDICL" then Some 23
        else None
      in
      match bound with
      | Some m ->
          if x.ue_pvalue < 0 || x.ue_pvalue >= m then
            failf "sanity: utt %S pvalue %d out of range (< %d)" x.ue_name x.ue_pvalue m
      | None -> ())
    t.entries;
  print_endline "gen_tables: sanity checks passed"

(* ------------------------------------------------------------------ *)
(* Driver                                                              *)
(* ------------------------------------------------------------------ *)

let write_file path content =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc;
  Printf.printf "gen_tables: wrote %s (%d lines, %d bytes)\n" path
    (List.length (String.split_on_char '\n' content) - 1)
    (String.length content)

let () =
  let usage = "usage: gen_tables [--check] [--out DIR] VENDOR_SRC_DIR" in
  let check = ref false and out = ref "." and vendor = ref None in
  let rec parse_args = function
    | [] -> ()
    | "--check" :: tl ->
        check := true;
        parse_args tl
    | "--out" :: dir :: tl ->
        out := dir;
        parse_args tl
    | a :: tl when String.length a > 0 && a.[0] <> '-' && !vendor = None ->
        vendor := Some a;
        parse_args tl
    | a :: _ -> failf "unexpected argument %S\n%s" a usage
  in
  parse_args (List.tl (Array.to_list Sys.argv));
  let vendor = match !vendor with Some v -> v | None -> failf "%s" usage in
  let path f = Filename.concat vendor f in
  let ct = parse_chartables (path "pcre2_chartables.c.dist") in
  let ucd = parse_ucd (path "pcre2_ucd.c") in
  let ucp = parse_ucp (path "pcre2_ucp.h") in
  let pt = parse_pt (path "pcre2_internal.h") in
  let utt = parse_ucptables (path "pcre2_ucptables.c") ~ucp ~pt in
  (* structural cross-file checks (always on) *)
  if Array.length ucd.script_sets mod ucp.script_item_size <> 0 then
    failf "ucd_script_sets length %d not a multiple of item size %d"
      (Array.length ucd.script_sets) ucp.script_item_size;
  if Array.length ucd.boolprop_sets mod ucp.boolprop_item_size <> 0 then
    failf "ucd_boolprop_sets length %d not a multiple of item size %d"
      (Array.length ucd.boolprop_sets) ucp.boolprop_item_size;
  if !check then run_sanity ct ucd utt ucp pt;
  let version_line =
    let text = read_file (path "pcre2_ucd.c") in
    line_of text (find_from text 0 "const char *PRIV(unicode_version)")
  in
  if not (Sys.file_exists !out) then Sys.mkdir !out 0o755;
  write_file (Filename.concat !out "chartables.ml") (emit_chartables ct);
  write_file (Filename.concat !out "ucd_tables.ml") (emit_ucd ucd ~version_line);
  write_file (Filename.concat !out "ucptables.ml") (emit_ucptables utt);
  Printf.printf
    "gen_tables: chartables=1088 bytes; nrecords=%d stage1=%d stage2=%d \
     caseless=%d digit=%d script_sets=%d boolprop_sets=%d; utt_size=%d\n"
    (Array.length ucd.records) (Array.length ucd.stage1) (Array.length ucd.stage2)
    (Array.length ucd.caseless) (Array.length ucd.digits)
    (Array.length ucd.script_sets)
    (Array.length ucd.boolprop_sets)
    (Array.length utt.entries)
