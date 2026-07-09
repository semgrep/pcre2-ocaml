(* Printable rendering of subject/mark text and UTF-8 helpers, ported from
   pcre2test.c (8-bit mode, no locale tables). *)

(* pcre2_tables.c: PRIV(utf8_table1) — also embedded in pcre2test's utf82ord *)
let utf8_table1 = [| 0x7f; 0x7ff; 0xffff; 0x1fffff; 0x3ffffff; 0x7fffffff |]

(* pcre2test.c:2962-3002 utf82ord: extended RFC-2279 UTF-8 (up to 6 bytes,
   values to 0x7fffffff). Returns
   > 0 => number of bytes consumed (value in snd)
   <= 0 => malformed at offset (-return). Reads virtual '\000' past [limit]. *)
let utf82ord ?limit s pos =
  let byte i = Char.code (Cstr.at ?limit s i) in
  let c = byte pos in
  let i =
    (* number of additional bytes: count leading 1 bits - 1 *)
    let rec count d k = if k >= 6 then 6 else if d land 0x80 = 0 then k else count (d lsl 1) (k + 1) in
    count c (-1)
  in
  if i = -1 then (1, c)
  else if i = 0 || i = 6 then (0, 0) (* invalid UTF-8 *)
  else begin
    let lim = match limit with Some l -> min l (String.length s) | None -> String.length s in
    let s6 = 6 * i in
    let mask = [| 0xff; 0x1f; 0x0f; 0x07; 0x03; 0x01 |].(i) in
    let d = ref ((c land mask) lsl s6) in
    let rc = ref (i + 1) in
    (try
       for j = 0 to i - 1 do
         if pos + 1 + j >= lim then begin
           rc := 0;
           raise Exit
         end;
         let cc = byte (pos + 1 + j) in
         if cc land 0xc0 <> 0x80 then begin
           rc := -(j + 1);
           raise Exit
         end;
         d := !d lor ((cc land 0x3f) lsl (s6 - 6 - (6 * j)))
       done;
       (* check unique encoding *)
       let j = ref 0 in
       while !j < 6 && !d > utf8_table1.(!j) do
         incr j
       done;
       if !j <> i then rc := -(i + 1)
     with Exit -> ());
    if !rc > 0 then (!rc, !d) else (!rc, 0)
  end

(* pcre2test.c:3185-3217 ord2utf8 *)
let ord2utf8 c =
  if c > 0x7fffffff then invalid_arg "ord2utf8"
  else begin
    let i = ref 0 in
    while c > utf8_table1.(!i) do
      incr i
    done;
    let n = !i in
    let b = Bytes.create (n + 1) in
    let v = ref c in
    for j = n downto 1 do
      Bytes.set b j (Char.chr (0x80 lor (!v land 0x3f)));
      v := !v lsr 6
    done;
    let table2 = [| 0; 0xc0; 0xe0; 0xf0; 0xf8; 0xfc |] in
    Bytes.set b 0 (Char.chr (table2.(n) lor !v));
    Bytes.to_string b
  end

(* pcre2test.c:249-254 PRINTABLE/PRINTOK (non-EBCDIC, no locale tables) *)
let printok c = c >= 32 && c < 127

(* pcre2test.c:3021-3051 pchar. Appends to [b], returns chars written. *)
let pchar b c ~utf =
  if printok c then begin
    Buffer.add_char b (Char.chr c);
    1
  end
  else if c < 0x100 then
    if utf then begin
      Buffer.add_string b (Printf.sprintf "\\x{%02x}" c);
      6
    end
    else begin
      Buffer.add_string b (Printf.sprintf "\\x%02x" c);
      4
    end
  else begin
    let t = Printf.sprintf "\\x{%02x}" c in
    Buffer.add_string b t;
    String.length t
  end

(* pcre2test.c:3094-3119 pchars8. Renders [len] bytes of [s] from [pos];
   in UTF mode decodes UTF-8 sequences that fit within the range. Returns
   the rendered string and the printed-character count. *)
let pchars ~utf s pos len =
  let b = Buffer.create (len + 8) in
  let yield = ref 0 in
  let p = ref pos in
  let remaining = ref len in
  let endpos = pos + len in
  while !remaining > 0 do
    let consumed = ref 1 in
    let c = ref (Char.code (Cstr.at s !p)) in
    if utf then begin
      let rc, v = utf82ord ~limit:endpos s !p in
      (* pcre2test.c:3106 — mustn't run over the end *)
      if rc > 0 && rc <= !remaining then begin
        consumed := rc;
        c := v
      end
    end;
    p := !p + !consumed;
    remaining := !remaining - !consumed;
    yield := !yield + pchar b !c ~utf
  done;
  (Buffer.contents b, !yield)

let pchars_str ~utf s pos len = fst (pchars ~utf s pos len)

(* Mark strings arrive from the driver as complete byte strings. *)
let mark_str ~utf m = pchars_str ~utf m 0 (String.length m)
