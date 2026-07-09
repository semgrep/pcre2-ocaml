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

  val is_match :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (bool, match_error) Result.t
  (** [is_match re subject] is equivalent to [find re subject |> Result.map
      Option.is_some] but may be implemented more efficiently. *)
end
