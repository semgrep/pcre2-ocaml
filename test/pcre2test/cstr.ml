(* C-string style helpers over OCaml strings.

   pcre2test scans NUL-terminated buffers; we emulate that by treating any
   index >= length (or >= an explicit [limit]) as '\000'. Classification
   functions mirror ctype.h in the C locale (pcre2test never calls setlocale
   with a non-"C" LC_CTYPE except for the out-of-scope locale modifier). *)

let at ?limit s i =
  let n =
    match limit with
    | Some l -> min l (String.length s)
    | None -> String.length s
  in
  if i < 0 || i >= n then '\000' else s.[i]

let isspace c =
  match c with ' ' | '\t' | '\n' | '\011' | '\012' | '\r' -> true | _ -> false

let isdigit c = c >= '0' && c <= '9'
let isxdigit c = isdigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let isalnum c = isdigit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let tolower c = if c >= 'A' && c <= 'Z' then Char.chr (Char.code c + 32) else c

let hexval c =
  let c = tolower c in
  if isdigit c then Char.code c - Char.code '0'
  else Char.code c - Char.code 'a' + 10

(* Case-independent comparison like pcre2test's strncmpic (pcre2test.c). *)
let strncmpic s i t n =
  let rec go k =
    if k >= n then true
    else if tolower (at s (i + k)) = tolower (at t k) then go (k + 1)
    else false
  in
  go 0

(* Strip trailing C-isspace characters; returns the effective length. *)
let rstrip_len s =
  let n = ref (String.length s) in
  while !n > 0 && isspace s.[!n - 1] do
    decr n
  done;
  !n

(* First index >= i that is not C-isspace (bounded by limit). *)
let skip_space ?limit s i =
  let j = ref i in
  while isspace (at ?limit s !j) do
    incr j
  done;
  !j
