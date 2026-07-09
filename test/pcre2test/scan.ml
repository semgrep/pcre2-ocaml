(* Line-shape helpers shared by the harness and the conformance splitter. *)

(* pcre2test.c:9591 — the set of valid pattern delimiters. *)
let delimiters = "/!\"'`%&-=_:;,@~"
let is_delimiter c = c <> '\000' && String.contains delimiters c

(* pcre2test.c:5301-5311 — find the closing delimiter in the accumulated
   pattern buffer (which may span physical lines, newlines included), starting
   after the opening delimiter. A backslash escapes the next character
   (including a newline at end-of-line, which is how continuation works when a
   line ends in a backslash). Returns the index of the closing delimiter. *)
let find_close ~delim buf =
  let n = String.length buf in
  let rec go i =
    if i >= n then None
    else if buf.[i] = '\\' && i + 1 < n then go (i + 2)
    else if buf.[i] = delim then Some i
    else go (i + 1)
  in
  go 1

(* Blank line in the pcre2test sense: nothing but C-isspace characters. *)
let is_blank line =
  let n = String.length line in
  let rec go i = i >= n || (Cstr.isspace line.[i] && go (i + 1)) in
  go 0

(* Comment/data-comment shape used by the main loop (pcre2test.c:9573):
   after leading whitespace, \= followed by whitespace-or-EOL. *)
let is_data_comment line =
  let p = Cstr.skip_space line 0 in
  Cstr.at line p = '\\'
  && Cstr.at line (p + 1) = '='
  && Cstr.isspace (Cstr.at line (p + 2))
