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
  val substring_of_match : match_ -> string
  (* TODO: not a huge fan since this requires creating a copy. Consider some way
     we could have a string view type? *)

  type captures [@@deriving show]
  (** A match with capture groups *)
  (* TODO: consider separate CapturingMatcher signature, since captures maybe
     have more variability. No need to generalize now, but useful if we add
     other engines like re2 or vectorscan *)

  val captures_length : captures -> int
  val match_of_captures : captures -> int -> match_ option
  val named_match_of_captures : captures -> string -> match_ option
  (* TODO: see comment on [captures]; unclear of generality of this *)

  type compile_option [@@deriving show, eq]
  type match_option [@@deriving show, eq]
  type compile_error [@@deriving show, eq]
  type match_error [@@deriving show, eq]

  val compile :
    ?options:compile_option list -> string -> (t, compile_error) Result.t

  val find :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (match_ option, match_error) Result.t

  val find_iter :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (match_, match_error) Result.t Seq.t
  (* NOTE: an error is probably always the last element, if one occurs (?) *)

  val captures :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (captures option, match_error) Result.t

  val captures_iter :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (captures, match_error) Result.t Seq.t
  (* NOTE: an error is probably always the last element, if one occurs (?) *)

  val split :
    ?options:match_option list ->
    ?subject_offset:int ->
    ?limit:int ->
    t ->
    string ->
    (string list, match_error) Result.t

  val is_match :
    ?options:match_option list ->
    ?subject_offset:int ->
    t ->
    string ->
    (bool, match_error) Result.t

  type substitution

  val subst : string -> substitution
  (* TODO: options? *)

  val replace :
    ?options:match_option list ->
    (* TODO: subst options? *)
    ?subject_offset:int ->
    t ->
    substitution ->
    string ->
    (string, match_error) Result.t
  (* TODO: Consider more granularity in error types. For now, we just have
     one match_error type for conviencnce, but some error could be local to,
     e.g., replacement/substitution or splitting, so we may benefit from a
     different type for each *)
end
