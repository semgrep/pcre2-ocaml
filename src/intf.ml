(* Shared pieces of the 7.5.3 convenience layer (quote, splitting results,
   substitution templates), re-added on top of the rewritten Result API. They
   live here (rather than in a new module) because the [Matcher] signature
   below references the types, and the oracle mirror pins its module list to
   [pcre2_c bindings intf]. Ported from tag 7.5.3 src/pcre2.ml. *)

type split_result =
  | Text of string  (** Text part of split string *)
  | Delim of string  (** Delimiter part of split string *)
  | Group of int * string
      (** Subgroup of matched delimiter (subgroup_nr, subgroup_str) *)
  | NoGroup  (** Unmatched subgroup *)
[@@deriving show, eq]

(* Elements of a substitution template (7.5.3 `type subst`; renamed so the
   [Match] constructor cannot clash with pcre2.ml's [Match] module — all
   pattern-matching on it stays in this file). *)
type subst_item =
  | SubstString of int * int (* Denotes a substring in the substitution *)
  | Backref of int (* nth backreference ($0 is program name!) *)
  | Match (* The whole matched string *)
  | PreMatch (* The string before the match *)
  | PostMatch (* The string after the match *)
  | LastParenMatch (* The last matched group *)

type substitution =
  string (* The substitution template *)
  * int (* Highest group number of backreferences *)
  * bool (* Makes use of "LastParenMatch" *)
  * subst_item list
(* The substitution elements, in textual order.
   DEVIATION from 7.5.3: the legacy parser kept this list in reverse
   textual order to suit its right-to-left Bytes blitting; we store
   textual order so expansion is a simple Buffer append. *)

(* Only used internally in "subst" (7.5.3) *)
exception FoundAt of int

let zero = Char.code '0'

(* 7.5.3 src/pcre2.ml `subst` — parser ported verbatim, then the item list is
   reversed into textual order (see [substitution]). *)
let subst str : substitution =
  let max_br = ref 0 in
  let with_lp = ref false in
  let lix = String.length str - 1 in
  let rec loop acc n =
    if lix < n then acc
    else
      try
        for i = n to lix do
          if String.unsafe_get str i = '$' then raise (FoundAt i)
        done;
        SubstString (n, lix - n + 1) :: acc
      with FoundAt i -> (
        if i = lix then SubstString (n, lix - n + 1) :: acc
        else
          let i1 = i + 1 in
          let acc = if n = i then acc else SubstString (n, i - n) :: acc in
          match String.unsafe_get str i1 with
          | '0' .. '9' as c -> (
              let subpat_nr = ref (Char.code c - zero) in
              try
                for j = i1 + 1 to lix do
                  let c = String.unsafe_get str j in
                  if c >= '0' && c <= '9' then
                    subpat_nr := (10 * !subpat_nr) + Char.code c - zero
                  else raise (FoundAt j)
                done;
                max_br := max !subpat_nr !max_br;
                Backref !subpat_nr :: acc
              with FoundAt j ->
                max_br := max !subpat_nr !max_br;
                loop (Backref !subpat_nr :: acc) j)
          | '!' -> loop acc (i1 + 1)
          | '$' -> loop (SubstString (i1, 1) :: acc) (i1 + 1)
          | '&' -> loop (Match :: acc) (i1 + 1)
          | '`' -> loop (PreMatch :: acc) (i1 + 1)
          | '\'' -> loop (PostMatch :: acc) (i1 + 1)
          | '+' ->
              with_lp := true;
              loop (LastParenMatch :: acc) (i1 + 1)
          | _ -> loop acc i1)
  in
  let subst_lst = List.rev (loop [] 0) in
  (str, !max_br, !with_lp, subst_lst)

let def_subst = subst ""

(* 7.5.3 src/pcre2.ml `quote` — same escape set, byte-identical output. *)
let quote s =
  let len = String.length s in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    match String.unsafe_get s i with
    | ('\\' | '^' | '$' | '.' | '[' | '|' | '(' | ')' | '?' | '*' | '+' | '{')
      as c ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

(* 7.5.3 validated templates up front against the pattern's capturecount; the
   frozen Bindings seam has no capturecount accessor, so callers validate per
   match against the ovector length (equal to capturecount + 1). *)
let validate_template (fname : string) ((_, max_br, with_lp, _) : substitution)
    (ngroups : int) : unit =
  if max_br > ngroups then
    failwith (fname ^ ": backreference denotes nonexistent subpattern");
  if with_lp && ngroups = 0 then failwith (fname ^ ": no backreferences")

(* Expansion of a parsed template for one match, appended to [buf]. Semantics
   of each element follow 7.5.3 `calc_trans_lst`: unset backreference groups
   expand to nothing; [Backref 0] is the executable name; PreMatch/PostMatch
   are measured from the subject's byte 0 / byte length (not from any search
   offset). *)
let apply_substitution (buf : Buffer.t) (subj : string)
    (ovec : (int * int) array) ((templ, _, _, items) : substitution) : unit =
  let first, last = ovec.(0) in
  let add_range start end_ =
    if end_ > start then Buffer.add_substring buf subj start (end_ - start)
  in
  List.iter
    (fun item ->
      match item with
      | SubstString (ix, len) -> Buffer.add_substring buf templ ix len
      | Backref 0 -> Buffer.add_string buf Sys.argv.(0)
      | Backref n ->
          let start, end_ = ovec.(n) in
          if start >= 0 then add_range start end_
      | Match -> add_range first last
      | PreMatch -> add_range 0 first
      | PostMatch -> add_range last (String.length subj)
      | LastParenMatch ->
          (* Scan from the highest group down to the first one that is set;
             falls through to the whole match at index 0, as in 7.5.3. *)
          let pos = ref (Array.length ovec - 1) in
          while fst ovec.(!pos) < 0 do
            decr pos
          done;
          let start, end_ = ovec.(!pos) in
          add_range start end_)
    items

module type Matcher = sig
  type t
  (** The type of the matcher itself *)

  type match_ [@@deriving show]
  (** A single match in a subject string *)

  type range = {
    start : int;  (** The byte at which the range starts. *)
    end_ : int;  (** The byte at which the range ends (exclusive) *)
  }
  [@@deriving show, eq]
  (** The range of a match, provided as byte offsets. *)

  val range_of_match : match_ -> range
  (** [range_of_match m] is range of the matched text, providing the start and
      end byte offsets. *)

  val substring_of_match : match_ -> string
  (** [substring_of_match m] is the matched substring of the subject *)
  (* TODO: not a huge fan since this requires creating a copy. Consider some way
     we could have a string view type? *)

  type captures [@@deriving show]
  (** A match with capture groups *)
  (* TODO: consider separate CapturingMatcher signature, since captures maybe
     have more variability. No need to generalize now, but useful if we add
     other engines like re2 or vectorscan *)

  val range_of_captures : captures -> range
  (** [range_of_captures c] is range of the matched text, providing the start
      and end byte offsets. *)

  val captures_length : captures -> int
  (** [captures_length c] is the number of matches contained in [c]. *)

  val match_of_captures : captures -> int -> match_ option
  (** [match_of_captures c i] is either [Some m], if the capture group numbered
      [i] matched creating match [m] when creating [c] or [None], if not. *)

  val named_match_of_captures : captures -> string -> match_ option
  (** [named_match_of_captures c n] is either [Some m], if the capture group
      named [n] matched creating match [m] when creating [c] or [None], if not.
      *)
  (* TODO: see comment on [captures]; unclear of generality of this *)

  type compile_option [@@deriving show, eq]
  type match_option [@@deriving show, eq]
  type compile_error [@@deriving show, eq]
  type match_error [@@deriving show, eq]

  val compile :
    ?options:compile_option list -> string -> (t, compile_error) Result.t
  (** [compile options pattern] compiles [pattern] with any specified [options]
      into the matcher type (e.g., a finite automata which can perform
      matching). In the case of an error, [Error c] is returned. *)

  val capture_groups : t -> (string * int) list
  (** [capture_groups re] is a list where elements identify each named capture
      group in the format [(n, i)], where [n] is the name and [i] is the number
      associated with that capture group. Note that numbers may be skipped or
      out of order. *)

  val find :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (match_ option, match_error) Result.t
  (** [find re subject] searches for a match of [re] in [subject]. See
      [match_option] for details on how [options] may affect matching. If
      [subject_offset] is provided, the search will begin at that byte offset
      (otherwise it begins at the start of [subject]).

      If a match is found the result is [Ok (Some m)]. If matching encounters
      no errors but does not result in a match the result is [Ok None].
      Otherwise, an error was encountered and is returned as [Error e].

      NOTE: This function may be less efficient than [is_match] depending on the
      underlying implementation. If you don't care about the range of the
      match, but only if one exists use [is_match] instead. *)

  val find_iter :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (match_, match_error) Result.t Seq.t
  (** [find_iter re subject] is a sequence of all disjoint [match_]es resulting
      from successively searching with the matcher [re]. See [match_option] for
      details on how [options] may affect matching. If [subject_offset] is
      provided then the initial match will be searched for from that byte
      offset in [subject] (otherwise matching begins at the start of
      [subject]).

      The sequence ends when no more matches are found (so no matches in
      [subject] means an empty sequence) or a fatal error is encountered. In
      the latter case the error is returned as the last element of the
      sequence. 
    *)
  (* TODO: Are there any errors which should be non-fatal? *)

  val captures :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (captures option, match_error) Result.t
  (** [captures re subject] searches for a match of [re] in [subject] and binds
      any capture groups present in the matcher. See [match_option] for details
      on how [options] may affect matching. If [subject_offset] is provided,
      the search will begin at that byte offset (otherwise it begins at the
      start of [subject]).

      If a match is found the result is [Ok (Some c)]. If matching encounters
      no errors but does not result in a match the result is [Ok None].
      Otherwise, an error was encountered and is returned as [Error e].

      NOTE: This function may be less efficient than [find] depending on the
      underlying implementation. If you don't need capture groups, you should
      use [find] instead.
    *)

  val captures_iter :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (captures, match_error) Result.t Seq.t
  (** [captures_iter re subject] is a sequence of all disjoint [captures] resulting
      from successively searching with the matcher [re]. See [match_option] for
      details on how [options] may affect matching. If [subject_offset] is
      provided then the initial match will be searched for from that byte
      offset in [subject] (otherwise matching begins at the start of
      [subject]).

      The sequence ends when no more matches are found (so no matches in
      [subject] means an empty sequence) or a fatal error is encountered. In
      the latter case the error is returned as the last element of the
      sequence. 

      NOTE: This function may be less efficient than [find_iter] depending on
      the underlying implementation. If you don't need capture groups, you
      should use [find_iter] instead.
    *)
  (* TODO: see [find_iter] *)

  val split :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?limit:int ->
    t ->
    string ->
    (string list, match_error) Result.t
  (** [split re subject] is a list of substrings of [subject] obtained by
      splitting it by removing matches [re] generates. If [subject_offset] is
      provided, matching to determine where to split starts there instead of
      the start of [subject]. If [limit] is provided then [subject] will be
      split into at most that many substrings.

      If a matching error occurs during this process, [Error e] is returned.
    *)

  val full_split :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?max:int ->
    t ->
    string ->
    (split_result list, match_error) Result.t
  (** [full_split re subject] splits [subject] by the delimiters that [re]
      matches, keeping the parts in a structured list: [Text] for the text
      between delimiters, [Delim] for each matched delimiter, and — for every
      capture group of [re], on every delimiter — [Group (n, str)] when group
      [n] matched or [NoGroup] when it did not. Semantics (including
      empty-delimiter handling) follow pcre-ocaml's [full_split]. If
      [subject_offset] is provided, splitting starts at that byte offset. At
      most [max] substrings are produced; a [max] of 0 (the default) means
      unlimited and additionally strips trailing empty delimiters (a negative
      [max] means unlimited without stripping).

      If a matching error occurs, [Error e] is returned; an out-of-range
      [subject_offset] yields [Error BADOFFSET]. *)

  val replace :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?itempl:substitution ->
    ?templ:string ->
    t ->
    string ->
    (string, match_error) Result.t
  (** [replace re subject] replaces all substrings of [subject] matched by
      [re] with the substitution template [templ] when given (parsed with
      {!subst}; [$1], [$&], etc.), the precompiled [itempl] otherwise
      (default: the empty template, which deletes the matches). Matching
      starts at [subject_offset] when provided; the text before it is kept
      unchanged. If no match is found the subject is returned unchanged as
      [Ok subject]; an out-of-range [subject_offset] yields
      [Error BADOFFSET].

      @raise Failure if the template refers to a nonexistent capture group,
      or uses [$+] while the pattern has no groups. The check runs when a
      match is found (pcre-ocaml raised up front). *)

  val replace_first :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?itempl:substitution ->
    ?templ:string ->
    t ->
    string ->
    (string, match_error) Result.t
  (** [replace_first re subject] is like {!replace} but replaces only the
      first match. *)

  val qreplace :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?templ:string ->
    t ->
    string ->
    (string, match_error) Result.t
  (** [qreplace re subject] replaces all substrings of [subject] matched by
      [re] with the literal string [templ] (default [""]) — no [$]-template
      parsing. No match returns [Ok subject] unchanged. *)

  val qreplace_first :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?templ:string ->
    t ->
    string ->
    (string, match_error) Result.t
  (** [qreplace_first re subject] is like {!qreplace} but replaces only the
      first match. *)

  val substitute_substrings :
    ?options:match_option list ->
    ?subject_offset:int ->
    subst:(captures -> string) ->
    t ->
    string ->
    (string, match_error) Result.t
  (** [substitute_substrings ~subst re subject] replaces all substrings of
      [subject] matched by [re] with the result of applying [subst] to the
      match's captures (use [match_of_captures] / [named_match_of_captures]
      to read groups). No match returns [Ok subject] unchanged. *)

  val substitute_substrings_first :
    ?options:match_option list ->
    ?subject_offset:int ->
    subst:(captures -> string) ->
    t ->
    string ->
    (string, match_error) Result.t
  (** [substitute_substrings_first ~subst re subject] is like
      {!substitute_substrings} but replaces only the first match. *)

  val substitute :
    ?options:match_option list ->
    ?subject_offset:int ->
    subst:(string -> string) ->
    t ->
    string ->
    (string, match_error) Result.t
  (** [substitute ~subst re subject] replaces all substrings of [subject]
      matched by [re] with the result of applying [subst] to the matched
      substring. No match returns [Ok subject] unchanged. *)

  val substitute_first :
    ?options:match_option list ->
    ?subject_offset:int ->
    subst:(string -> string) ->
    t ->
    string ->
    (string, match_error) Result.t
  (** [substitute_first ~subst re subject] is like {!substitute} but replaces
      only the first match. *)

  val extract :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?full_match:bool ->
    t ->
    string ->
    (string array option, match_error) Result.t
  (** [extract re subject] searches for the first match of [re] in [subject]
      (from [subject_offset] when provided) and returns [Ok (Some arr)] with
      the array of matched substrings: the full match at index 0 when
      [full_match] is [true] (the default), the captured substrings only when
      it is [false]. A capture group that did not match yields the empty
      string in the corresponding position. No match returns [Ok None]
      (pcre-ocaml raised [Not_found]). *)

  val extract_opt :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?full_match:bool ->
    t ->
    string ->
    (string option array option, match_error) Result.t
  (** [extract_opt re subject] is like {!extract} except that a capture group
      that did not match yields [None] in the corresponding position instead
      of the empty string. *)

  val extract_all :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?full_match:bool ->
    t ->
    string ->
    (string array array, match_error) Result.t
  (** [extract_all re subject] returns one substring array (as in {!extract})
      per successive match of [re] in [subject]. Iteration follows
      pcre-ocaml's [extract_all]: after an empty match the next attempt is
      made at the same position requiring a non-empty match, and iteration
      stops if that fails. No match returns [Ok [||]] (pcre-ocaml raised
      [Not_found]). *)

  val extract_all_opt :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?full_match:bool ->
    t ->
    string ->
    (string option array array, match_error) Result.t
  (** [extract_all_opt re subject] is like {!extract_all} except that a
      capture group that did not match yields [None] in the corresponding
      position instead of the empty string. *)

  val is_match :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (bool, match_error) Result.t
  (** [is_match re subject] is equivalent to [find re subject |> Result.map
      Option.is_some] but may be implemented more efficiently. *)
end
