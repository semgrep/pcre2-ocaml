(* Moved verbatim from src/pcre2.ml (Matcher-API factoring, fast-engine
   milestone chunk A) so additional engines (pcre2.fast) can share the
   match/captures/range representation. The single edit: [Bindings.unset]
   -> local [unset] (same value), dropping the engine dependency. *)

let ( >+= ) x f = Option.map f x

let unset = (-1, -1)
(* Since -1 == PCRE2_UNSET (as exposed through the former stubs). *)

type match_ = string * int * int (* need only ovec? *) [@@deriving show, eq]
type range = { start : int; end_ : int } [@@deriving show, eq]

type captures = string * (int * int) array * (string * int) array
[@@deriving show, eq]

let range_of_match (_, start, end_) = { start; end_ }

let substring_of_match (subject, start, end_) =
  String.sub subject start (end_ - start)

let range_of_captures (_, matches, _) =
  (* Array should always be at least length 1 *)
  let start, end_ = matches.(0) in
  { start; end_ }

let captures_length ((_, matches, _) : captures) : int = Array.length matches

let get_match i matches =
  if 0 <= i && i < Array.length matches then
    let ((start, end_) as match_) = matches.(i) in
    if match_ = unset then None else Some (start, end_)
  else None

let match_of_captures ((subject, matches, _) : captures) (i : int) :
    match_ option =
  get_match i matches >+= fun (start, end_) -> (subject, start, end_)

let named_match_of_captures ((subject, matches, names) : captures)
    (group_name : string) : match_ option =
  Array.find_map
    (fun (s, i) ->
      if String.equal group_name s then get_match i matches else None)
    names
  >+= fun (start, end_) -> (subject, start, end_)
