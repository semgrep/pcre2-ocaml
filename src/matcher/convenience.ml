(* MakeConvenience moved verbatim from src/pcre2.ml (Matcher-API factoring,
   fast-engine milestone chunk A); MakeMatcher is new (see its comment).
   [open]s replace what top-level scope provided in pcre2.ml. *)

open Intf
open Match
open Error

(* One shared implementation of the 7.5.3 convenience functions, included by
   both Interp and Jit. Built directly on the raw capture function (NOT on
   find_iter/captures_iter, whose empty-match semantics are frozen and wrong
   for these loops) so PCRE2 option bits outside the public match_option
   lists can be OR'd in for the empty-match retries. *)
module MakeConvenience (R : sig
  type t
  type match_option

  val bitvector_of_match_options : match_option list -> int32

  val capture_raw :
    ?match_limit:int ->
    ?depth_limit:int ->
    ?heap_limit:int ->
    t ->
    string ->
    int ->
    int32 ->
    (((int * int) array * (string * int) array) option, int) Result.t
end) =
struct
  (* pcre2.h: PCRE2_NOTEMPTY. OR'd in for the empty-match retries below;
     deliberately not representable in the public match_option lists. *)
  let notempty_bits = 0x00000004l

  (* Single raw capture attempt; maps raw error codes at the seam (an
     out-of-range offset comes back as Error BADOFFSET, per the uniform
     Bindings rule). *)
  let attempt re subj pos bits :
      (((int * int) array * (string * int) array) option, match_error) Result.t
      =
    Result.map_error match_error_of_int (R.capture_raw re subj pos bits)

  (* 7.5.3 get_substrings / get_opt_substrings over the ovector-pairs array:
     unset group -> "" resp. None; ?full_match drops element 0. *)
  let get_substrings ?(full_match = true) subj (ovec : (int * int) array) =
    let get i =
      let start, end_ = ovec.(i) in
      if start < 0 then "" else String.sub subj start (end_ - start)
    in
    if full_match then Array.init (Array.length ovec) get
    else Array.init (Array.length ovec - 1) (fun i -> get (i + 1))

  let get_opt_substrings ?(full_match = true) subj (ovec : (int * int) array) =
    let get i =
      let start, end_ = ovec.(i) in
      if start < 0 then None else Some (String.sub subj start (end_ - start))
    in
    if full_match then Array.init (Array.length ovec) get
    else Array.init (Array.length ovec - 1) (fun i -> get (i + 1))

  (* Global-replacement driver shared by replace/qreplace/substitute*.
     Faithful to the 7.5.3 loops (`replace`/`qreplace`/
     `substitute_substrings`): same match-iteration order, prefix before
     [subject_offset] kept verbatim, and the literal [last < first + 1]
     empty-match rule (append the expansion, keep one subject char, resume at
     first + 1). Internals use a Buffer instead of the legacy reverse
     trans-list + Bytes blitting; outputs are identical. *)
  let replace_loop ~options ~subject_offset re subj
      (expand : Buffer.t -> (int * int) array -> (string * int) array -> unit) :
      (string, match_error) Result.t =
    let subj_len = String.length subj in
    if subject_offset < 0 || subject_offset > subj_len then Error BADOFFSET
    else
      let bits = R.bitvector_of_match_options options in
      let buf = Buffer.create (subj_len + 16) in
      Buffer.add_substring buf subj 0 subject_offset;
      let rec loop cur_pos =
        if cur_pos > subj_len then Ok (Buffer.contents buf)
        else
          match attempt re subj cur_pos bits with
          | Error e -> Error e
          | Ok None ->
              Buffer.add_substring buf subj cur_pos (subj_len - cur_pos);
              Ok (Buffer.contents buf)
          | Ok (Some (ovec, names)) ->
              let first, last = ovec.(0) in
              if first > cur_pos then
                Buffer.add_substring buf subj cur_pos (first - cur_pos);
              expand buf ovec names;
              if last < first + 1 then (
                (* empty match: keep one subject char and resume past it *)
                if first < subj_len then Buffer.add_char buf subj.[first];
                loop (first + 1))
              else loop last
      in
      loop subject_offset

  (* First-match-only driver shared by the *_first variants (7.5.3
     `replace_first`/`qreplace_first`/`substitute_substrings_first`). *)
  let replace_first_loop ~options ~subject_offset re subj expand :
      (string, match_error) Result.t =
    let bits = R.bitvector_of_match_options options in
    match attempt re subj subject_offset bits with
    | Error e -> Error e
    | Ok None -> Ok subj
    | Ok (Some (ovec, names)) ->
        let first, last = ovec.(0) in
        let buf = Buffer.create (String.length subj + 16) in
        Buffer.add_substring buf subj 0 first;
        expand buf ovec names;
        Buffer.add_substring buf subj last (String.length subj - last);
        Ok (Buffer.contents buf)

  (* templ (string) wins over itempl (precompiled), default empty — 7.5.3
     precedence. *)
  let template ~itempl ~templ : substitution =
    match (templ, itempl) with
    | Some str, _ -> Intf.subst str
    | None, Some t -> t
    | None, None -> Intf.def_subst

  let replace ?(options = []) ?(subject_offset = 0) ?itempl ?templ re subj =
    let t = template ~itempl ~templ in
    replace_loop ~options ~subject_offset re subj (fun buf ovec _names ->
        Intf.validate_template "Pcre2.replace" t (Array.length ovec - 1);
        Intf.apply_substitution buf subj ovec t)

  let replace_first ?(options = []) ?(subject_offset = 0) ?itempl ?templ re subj
      =
    let t = template ~itempl ~templ in
    replace_first_loop ~options ~subject_offset re subj (fun buf ovec _names ->
        Intf.validate_template "Pcre2.replace_first" t (Array.length ovec - 1);
        Intf.apply_substitution buf subj ovec t)

  let qreplace ?(options = []) ?(subject_offset = 0) ?(templ = "") re subj =
    replace_loop ~options ~subject_offset re subj (fun buf _ovec _names ->
        Buffer.add_string buf templ)

  let qreplace_first ?(options = []) ?(subject_offset = 0) ?(templ = "") re subj
      =
    replace_first_loop ~options ~subject_offset re subj (fun buf _ovec _names ->
        Buffer.add_string buf templ)

  let substitute_substrings ?(options = []) ?(subject_offset = 0)
      ~(subst : captures -> string) re subj =
    replace_loop ~options ~subject_offset re subj (fun buf ovec names ->
        Buffer.add_string buf (subst (subj, ovec, names)))

  let substitute_substrings_first ?(options = []) ?(subject_offset = 0)
      ~(subst : captures -> string) re subj =
    replace_first_loop ~options ~subject_offset re subj (fun buf ovec names ->
        Buffer.add_string buf (subst (subj, ovec, names)))

  (* 7.5.3 `substitute(_first)`: the callback sees just the matched
     substring. *)
  let whole_match_expand subj (str_subst : string -> string) buf
      (ovec : (int * int) array) _names =
    let first, last = ovec.(0) in
    Buffer.add_string buf (str_subst (String.sub subj first (last - first)))

  let substitute ?(options = []) ?(subject_offset = 0) ~subst re subj =
    replace_loop ~options ~subject_offset re subj
      (whole_match_expand subj subst)

  let substitute_first ?(options = []) ?(subject_offset = 0) ~subst re subj =
    replace_first_loop ~options ~subject_offset re subj
      (whole_match_expand subj subst)

  let extract ?(options = []) ?(subject_offset = 0) ?full_match re subj =
    let bits = R.bitvector_of_match_options options in
    match attempt re subj subject_offset bits with
    | Error e -> Error e
    | Ok None -> Ok None
    | Ok (Some (ovec, _)) -> Ok (Some (get_substrings ?full_match subj ovec))

  let extract_opt ?(options = []) ?(subject_offset = 0) ?full_match re subj =
    let bits = R.bitvector_of_match_options options in
    match attempt re subj subject_offset bits with
    | Error e -> Error e
    | Ok None -> Ok None
    | Ok (Some (ovec, _)) ->
        Ok (Some (get_opt_substrings ?full_match subj ovec))

  (* 7.5.3 `exec_all` iteration: after an empty match at [pos], stop at the
     end of the subject, else retry at the SAME pos with PCRE2_NOTEMPTY and
     stop if that fails (no advance-by-one — legacy quirk, kept). Zero
     matches yield [] (legacy raised Not_found). *)
  let capture_all ~options ~subject_offset re subj :
      ((int * int) array list, match_error) Result.t =
    let bits = R.bitvector_of_match_options options in
    let subj_len = String.length subj in
    match attempt re subj subject_offset bits with
    | Error e -> Error e
    | Ok None -> Ok []
    | Ok (Some (ovec0, _)) ->
        let rec loop acc (pfirst, plast) =
          let next =
            if pfirst = plast then
              if plast = subj_len then Ok None
              else attempt re subj plast (Int32.logor bits notempty_bits)
            else attempt re subj plast bits
          in
          match next with
          | Error e -> Error e
          | Ok None -> Ok (List.rev acc)
          | Ok (Some (ovec, _)) -> (loop [@tailcall]) (ovec :: acc) ovec.(0)
        in
        loop [ ovec0 ] ovec0.(0)

  let extract_all ?(options = []) ?(subject_offset = 0) ?full_match re subj =
    match capture_all ~options ~subject_offset re subj with
    | Error e -> Error e
    | Ok ovecs ->
        Ok (Array.of_list (List.map (get_substrings ?full_match subj) ovecs))

  let extract_all_opt ?(options = []) ?(subject_offset = 0) ?full_match re subj
      =
    match capture_all ~options ~subject_offset re subj with
    | Error e -> Error e
    | Ok ovecs ->
        Ok
          (Array.of_list (List.map (get_opt_substrings ?full_match subj) ovecs))

  (* 7.5.3 `full_split` recursive loop, ported clause by clause. [acc] is in
     reverse order until the final List.rev; [cnt] reaches 0 only when a
     positive [max] runs out; empty delimiters read their groups BEFORE the
     ANCHORED|NOTEMPTY retry overwrites the (conceptual) ovector; a failed
     retry emits one Text char and advances by one. *)
  let handle_subgroups subj (ovec : (int * int) array) acc =
    let acc = ref acc in
    for i = 1 to Array.length ovec - 1 do
      let first, last = ovec.(i) in
      acc :=
        (if first < 0 then NoGroup
         else Group (i, String.sub subj first (last - first)))
        :: !acc
    done;
    !acc

  let full_split ?(options = []) ?(subject_offset = 0) ?(max = 0) re subj =
    let subj_len = String.length subj in
    if subject_offset < 0 || subject_offset > subj_len then Error BADOFFSET
    else if subj_len = 0 then Ok []
    else if max = 1 then Ok [ Text subj ]
    else
      let bits = R.bitvector_of_match_options options in
      let rec loop acc cnt pos prematch =
        let len = subj_len - pos in
        if len < 0 then Ok acc
        else if cnt = 0 then
          (* Checks termination due to max restriction *)
          let finish_with_match =
            if prematch then attempt re subj pos bits else Ok None
          in
          match finish_with_match with
          | Error e -> Error e
          | Ok (Some (ovec, _)) ->
              let first, last = ovec.(0) in
              let delim = Delim (String.sub subj first (last - first)) in
              Ok
                (Text (String.sub subj last (subj_len - last))
                :: handle_subgroups subj ovec (delim :: acc))
          | Ok None ->
              if len = 0 then Ok acc
              else Ok (Text (String.sub subj pos len) :: acc)
        else
          match attempt re subj pos bits with
          | Error e -> Error e
          | Ok None ->
              if len = 0 then Ok acc
              else Ok (Text (String.sub subj pos len) :: acc)
          | Ok (Some (ovec, _)) ->
              let first, last = ovec.(0) in
              if first = pos then
                if last = pos then
                  if len = 0 then
                    Ok (handle_subgroups subj ovec (Delim "" :: acc))
                  else
                    let empty_groups = handle_subgroups subj ovec [] in
                    (* 7.5.3 retried here with ANCHORED|NOTEMPTY. Real
                       pcre2_jit_match silently ignores match-time ANCHORED,
                       so anchor portably instead: an unanchored NOTEMPTY
                       search finds the leftmost non-empty match, and one
                       starting exactly at [pos] exists iff the anchored
                       search would have succeeded — with the same ovector. *)
                    match
                      attempt re subj pos (Int32.logor bits notempty_bits)
                    with
                    | Error e -> Error e
                    | Ok (Some (ovec2, _)) when fst ovec2.(0) = pos ->
                        let first2, last2 = ovec2.(0) in
                        let delim =
                          Delim (String.sub subj first2 (last2 - first2))
                        in
                        let new_acc =
                          handle_subgroups subj ovec2
                            (delim
                            ::
                            (if prematch then acc
                             else empty_groups @ (Delim "" :: acc)))
                        in
                        (loop [@tailcall]) new_acc (cnt - 1) last2 false
                    | Ok (Some _) | Ok None ->
                        let new_acc =
                          Text (String.sub subj pos 1)
                          :: (empty_groups @ (Delim "" :: acc))
                        in
                        (loop [@tailcall]) new_acc (cnt - 1) (pos + 1) true
                else
                  let delim = Delim (String.sub subj first (last - first)) in
                  (loop [@tailcall])
                    (handle_subgroups subj ovec (delim :: acc))
                    cnt last false
              else
                let delim = Delim (String.sub subj first (last - first)) in
                let pre = Text (String.sub subj pos (first - pos)) :: acc in
                (loop [@tailcall])
                  (handle_subgroups subj ovec (delim :: pre))
                  (cnt - 1) last false
      in
      match loop [] (max - 1) subject_offset true with
      | Error e -> Error e
      | Ok acc ->
          (* 7.5.3 strip_all_empty_full: with max = 0, drop only a trailing
             pure-Delim run (trailing Group/NoGroup items block stripping). *)
          let rec strip = function Delim _ :: t -> strip t | l -> l in
          Ok (List.rev (if max = 0 then strip acc else acc))
end

(* NEW code (not a move): the find/captures/split layer that src/pcre2.ml
   duplicates between Interp and Jit, lifted verbatim from Interp's bodies
   (src/pcre2.ml find..is_match) with the two Bindings calls abstracted as
   [match_raw]/[capture_raw]. Interp/Jit keep their original copies
   byte-identical; this functor exists for additional engines (first user:
   pcre2.fast). find_iter/captures_iter empty-match-repeat semantics are
   FROZEN (port-conventions.md par. 5) -- do not "fix" them here. *)
module MakeMatcher (R : sig
  type t
  type match_option

  val bitvector_of_match_options : match_option list -> int32

  val match_raw :
    ?match_limit:int ->
    ?depth_limit:int ->
    ?heap_limit:int ->
    t ->
    string ->
    int ->
    int32 ->
    ((int * int) option, int) Result.t

  val capture_raw :
    ?match_limit:int ->
    ?depth_limit:int ->
    ?heap_limit:int ->
    t ->
    string ->
    int ->
    int32 ->
    (((int * int) array * (string * int) array) option, int) Result.t
end) =
struct
  let ( let* ) = Result.bind

  let find ?(options : R.match_option list = []) ?(subject_offset : int = 0)
      ?match_limit ?depth_limit ?heap_limit (re : R.t) (subject : string) :
      (match_ option, match_error) Result.t =
    let options = R.bitvector_of_match_options options in
    match
      R.match_raw ?match_limit ?depth_limit ?heap_limit re subject
        subject_offset options
    with
    | Ok (Some (start, end_)) -> Ok (Some (subject, start, end_))
    | Ok None -> Ok None
    | Error n -> Error (match_error_of_int n)

  let find_iter ?(options : R.match_option list = [])
      ?(subject_offset : int = 0) ?match_limit ?depth_limit ?heap_limit
      (re : R.t) (subject : string) : (match_, match_error) Result.t Seq.t =
    Seq.unfold
      (fun offset ->
        match
          find ~options ?match_limit ?depth_limit ?heap_limit
            ~subject_offset:offset re subject
        with
        | Ok (Some (m : match_)) -> Some (Ok m, (range_of_match m).end_)
        | Ok None -> None
        | Error e -> Some (Error e, String.length subject))
      subject_offset

  let captures ?(options : R.match_option list = [])
      ?(subject_offset : int = 0) ?match_limit ?depth_limit ?heap_limit
      (re : R.t) (subject : string) : (captures option, match_error) Result.t =
    let options = R.bitvector_of_match_options options in
    match
      R.capture_raw ?match_limit ?depth_limit ?heap_limit re subject
        subject_offset options
    with
    | Ok (Some (arr, names)) -> Ok (Some (subject, arr, names))
    | Ok None -> Ok None
    | Error n -> Error (match_error_of_int n)

  let captures_iter ?(options : R.match_option list = [])
      ?(subject_offset : int = 0) ?match_limit ?depth_limit ?heap_limit
      (re : R.t) (subject : string) : (captures, match_error) Result.t Seq.t =
    Seq.unfold
      (fun offset ->
        match
          captures ~options ?match_limit ?depth_limit ?heap_limit
            ~subject_offset:offset re subject
        with
        | Ok (Some (c : captures)) -> Some (Ok c, (range_of_captures c).end_)
        | Ok None -> None
        | Error e -> Some (Error e, String.length subject))
      subject_offset

  let split ?(options : R.match_option list = []) ?(subject_offset : int = 0)
      ?(limit : int option) (re : R.t) (subject : string) :
      (string list, match_error) Result.t =
    let delims = find_iter ~options ~subject_offset re subject in
    let delims =
      match limit with
      | Some n when n > 0 -> Seq.take (n - 1) delims
      | None -> delims
      | _ -> invalid_arg "todo: decide how to handle 0 or negative limit"
    in
    let* end_offset, substrings =
      Seq.fold_left
        (fun x m ->
          match (x, m) with
          | Ok (start, acc), Ok m ->
              let { start = delim_start; end_ = delim_end } =
                range_of_match m
              in
              let sub = String.sub subject start (delim_start - start) in
              Ok (delim_end, sub :: acc)
          | e, _ -> e)
        (Ok (0, []))
        delims
    in
    Ok
      ((* We still have one more substring to add: the one after the last
          delimiter. *)
       String.(sub subject end_offset (length subject - end_offset))
       :: substrings
      (* ... and we built this in reverse to be fast---but let's return it in
         the right order. *)
      |> List.rev)

  let is_match ?(options : R.match_option list = []) ?(subject_offset : int = 0)
      (re : R.t) (subject : string) : (bool, match_error) Result.t =
    find ~options ~subject_offset re subject |> Result.map Option.is_some
end
