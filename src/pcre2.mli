(*
   PCRE2-OCAML - Perl Compatibility Regular Expressions for OCaml

   Copyright (C) 1999-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(** Perl Compatibility Regular Expressions for OCaml

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%%}homepage}}
*)


(** {5 Exceptions} *)

type error =
  | Partial  (** String only matched the pattern partially *)
  | BadPattern of string * int  (** [BadPattern (msg, pos)] regular
                                    expression is malformed.  The reason
                                    is in [msg], the position of the
                                    error in the pattern in [pos]. *)
  | BadUTF  (** UTF string being matched is invalid *)
  | BadUTFOffset  (** Gets raised when a UTF string being matched with
                       offset is invalid. *)
  | MatchLimit  (** Maximum allowed number of match attempts with
                    backtracking or recursion is reached during matching.
                    ALL FUNCTIONS CALLING THE MATCHING ENGINE MAY RAISE
                    IT!!! *)
  | DepthLimit
  | WorkspaceSize  (** Raised by {!pcre2_dfa_match} when the provided
                       workspace array is too small. See documention on
                       {!pcre2_dfa_match} for details on workspace array
                       sizing. *)
  | InternalError of string
      (** [InternalError msg] C-library exhibits unknown/undefined
          behaviour.  The reason is in [msg]. *)

(** Exception indicating PCRE errors. *)
exception Error of error

(** [Backtrack] used in callout functions to force backtracking. *)
exception Backtrack

(** [Regexp_or (pat, error)] gets raised for sub-pattern [pat] by [regexp_or]
    if it failed to compile. *)
exception Regexp_or of string * error

(** {6 Compilation and runtime flags and their conversion functions} *)

(** Internal representation of compilation flags *)
type icflag

(** Internal representation of runtime flags *)
and  irflag

(** Compilation flags *)
and cflag =
  [ `ALLOW_EMPTY_CLASS   (** Allow empty classes *)
  | `ALT_BSUX            (** Alternative handling of \u, \U, and \x *)
  | `ALT_CIRCUMFLEX      (** Alternative handling of ^ in multiline mode *)
  | `ALT_VERBNAMES       (** Process backslashes in verb names *)
  | `ANCHORED            (** Pattern matches only at start of string *)
  | `AUTO_CALLOUT        (** Automatically inserts callouts with id 255
                             before each pattern item *)
  | `CASELESS            (** Case insensitive matching *)
  | `DOLLAR_ENDONLY      (** '$' in pattern matches only at end of string *)
  | `DOTALL              (** '.' matches all characters (newlines, too) *)
  | `DUPNAMES            (** Allow duplicate names for subpatterns *)
  | `ENDANCHORED         (** Pattern can match only at end of subject *)
  | `EXTENDED            (** Ignores whitespace and PERL-comments. Behaves
                             like the '/x'-option in PERL *)
  | `EXTENDED_MORE
  | `FIRSTLINE           (** Unanchored patterns must match before/at first NL *)
  | `LITERAL             (** Pattern characters are all literal *)
  | `MATCH_INVALID_UTF   (** Enable support for matching invalid UTF *)
  | `MATCH_UNSET_BACKREF (** Match unset backreferences *)
  | `MULTILINE           (** '^' and '$' match before/after newlines,
                             not just at the beginning/end of a string *)
  | `NEVER_BACKSLASH_C   (** Lock out the use of \C in patterns *)
  | `NEVER_UCP           (** Lock out UCP, e.g. via (\*UCP) *)
  | `NEVER_UTF           (** Lock out UTF, e.g. via (\*UTF) *)
  | `NO_AUTO_CAPTURE     (** Disables the use of numbered capturing parentheses *)
  | `NO_AUTO_POSSESS     (** Disable auto-possessification *)
  | `NO_DOTSTAR_ANCHOR   (** Disable automatic anchoring for .* *)
  | `NO_START_OPTIMIZE   (** Disable match-time start optimizations *)
  | `NO_UTF_CHECK        (** Do not check the pattern for UTF validity (only
                             relevant if UTF is set)
                             WARNING: with this flag enabled, invalid UTF strings
                             may cause a crash, loop, or give incorrect results *)
  | `UCP                 (** Use Unicode properties for \d, \w, etc. *)
  | `UNGREEDY            (** Quantifiers not greedy anymore, only
                             if followed by '?' *)
  | `USE_OFFSET_LIMIT    (** Enable offset limit for unanchored matching *)
  | `UTF                 (** Treat pattern and subjects as UTF strings *)
  ]

val cflags : cflag list -> icflag
(** [cflags cflag_list] converts a list of compilation flags to
    their internal representation. *)

val cflag_list : icflag -> cflag list
(** [cflag_list cflags] converts internal representation of
    compilation flags to a list. *)

(** Runtime flags *)
type rflag =
  [
  | `ANCHORED             (** Match only at the first position *)
  | `COPY_MATCHED_SUBJECT (** On success, make a private subject copy *)
  | `DFA_RESTART          (** Causes matching to proceed presuming the subject
                              string is further to one partially matched previously
                              using the same int-array working set.  May only be
                              used with {!pcre2_dfa_match} or {!unsafe_pcre2_dfa_match},
                              and should always be paired with {!`PARTIAL}. *)
  | `DFA_SHORTEST         (** Return only the shortest match *)
  | `ENDANCHORED          (** Pattern can match only at end of subject *)
  | `NOTBOL               (** Beginning of string is not treated as beginning of line *)
  | `NOTEOL               (** End of string is not treated as end of line *)
  | `NOTEMPTY             (** An empty string is not a valid match *)
  | `NOTEMPTY_ATSTART     (** An empty string at the start of the subject is not
                              a valid match *)
  | `NO_JIT               (** Do not use JIT matching *)
  | `NO_UTF_CHECK         (** Do not check the subject for UTF validity (only
                              relevant if PCRE2_UTF was set at compile time) *)
  | `PARTIAL_HARD         (** Throw Pcre2.Partial for a partial match even if there
                              is a full match *)
  | `PARTIAL_SOFT         (** Throw Pcre2.Partial for a partial match if no full
                              matches are found *)
  ]

val rflags : rflag list -> irflag
(** [rflags rflag_list] converts a list of runtime flags to
    their internal representation. *)

val rflag_list : irflag -> rflag list
(** [rflag_list rflags] converts internal representation of
    runtime flags to a list. *)


(** {6 Information on the PCRE2-configuration (build-time options)} *)

(** Version information *)
val version : string  (** Version of the PCRE2-C-library *)

(** Indicates whether unicode support is enabled *)
val config_unicode : bool

(** Character used as newline *)
val config_newline : char

(** Number of bytes used for internal linkage of regular expressions *)
val config_link_size : int

(** Default limit for calls to internal matching function *)
val config_match_limit : int

(** Default limit for depth of nested backtracking *)
val config_depth_limit : int

(** Indicates use of stack recursion in matching function *)
val config_stackrecurse : bool


(** {3 Information on patterns} *)

(** Information on matching of "first chars" in patterns *)
type firstcodeunit_info =
  [ `Char of char  (** Fixed first character *)
  | `Start_only    (** Pattern matches at beginning and end of newlines *)
  | `ANCHORED      (** Pattern is anchored *)
  ]

type regexp (** Compiled regular expressions *)

(** [options regexp] @return compilation flags of [regexp]. *)
val options : regexp -> icflag

(** [size regexp] @return memory size of [regexp]. *)
val size : regexp -> int

(** [capturecount regexp] @return number of capturing subpatterns in
    [regexp]. *)
val capturecount : regexp -> int

(** [backrefmax regexp] @return number of highest backreference in [regexp]. *)
val backrefmax : regexp -> int

(** [namecount regexp] @return number of named subpatterns in [regexp]. *)
val namecount : regexp -> int

(** [nameentrysize regexp] @return size of longest name of named
    subpatterns in [regexp] + 3. *)
val nameentrysize : regexp -> int

(** [names regex] @return array of names of named substrings in [regexp]. *)
val names : regexp -> string array

(** [firstcodeunit regexp] @return firstcodeunit info on [regexp]. *)
val firstcodeunit : regexp -> firstcodeunit_info

(** [lastcodeunit regexp] @return some last matching character of [regexp]
    if available, [None] otherwise. *)
val lastcodeunit : regexp -> char option

val get_stringnumber : regexp -> string -> int
(** [get_stringnumber rex name] @return the index of the named substring
    [name] in regular expression [rex]. This index can then be used with
    [get_substring].

    @raise Invalid_arg if there is no such named substring. *)

(* val get_match_limit : regexp -> int option *)
(** [get_match_limit rex] @return some match limit of regular expression
    [rex] or [None]. *)

(* val get_depth_limit : regexp -> int option *)
(** [get_depth_limit rex] @return some depth limit of regular expression
    [rex] or [None]. *)


(** {6 Compilation of patterns} *)

type chtables (** Alternative set of char tables for pattern matching *)

val maketables : unit -> chtables
(** Generates new set of char tables for the current locale. *)

val regexp :
  ?jit_compile : bool ->
  ?limit : int ->
  ?depth_limit : int ->
  ?iflags : icflag ->
  ?flags : cflag list ->
  ?chtables : chtables ->
  string -> regexp
(** [regexp ?jit_compile ?limit ?depth_limit ?iflags ?flags ?chtables pattern]
    compiles [pattern] with [flags] when given, with [iflags] otherwise,
    and with char tables [chtables].  If [limit] is specified, this sets
    a limit to the amount of recursion and backtracking (only lower than
    the builtin default!).  If this limit is exceeded, [MatchLimit] will
    be raised during matching.

    @param jit_compile TODO
    @param limit default = no extra limit other than default
    @param depth_limit default = no extra depth_limit other than default
    @param iflags default = no extra flags
    @param flags default = ignored
    @param chtables default = builtin char tables

    @return the regular expression.

    For detailed documentation on how you can specify PERL-style regular
    expressions (= patterns), please consult the PCRE2-documentation
    ("man pcre2pattern") or PERL-manuals.
    @see <http://www.perl.com> www.perl.com *)

val regexp_or :
  ?jit_compile : bool ->
  ?limit : int ->
  ?depth_limit : int ->
  ?iflags : icflag ->
  ?flags : cflag list ->
  ?chtables : chtables ->
  string list -> regexp
(** [regexp_or ?jit_compile ?limit ?depth_limit ?iflags ?flags ?chtables patterns]
    like {!regexp}, but combines [patterns] as alternatives (or-patterns) into
    one regular expression.
*)

val quote : string -> string
(** [quote str] @return the quoted string of [str]. *)


(** {6 Subpattern extraction} *)

type substrings (** Information on substrings after pattern matching *)

val get_subject : substrings -> string
(** [get_subject substrings] @return the subject string of [substrings]. *)

val num_of_subs : substrings -> int
(** [num_of_subs substrings] @return number of strings in [substrings]
    (whole match inclusive). *)

val get_substring : substrings -> int -> string
(** [get_substring substrings n] @return the [n]th substring
    (0 is whole match) of [substrings].

    @raise Invalid_argument if [n] is not in the range of the number of
    substrings.
    @raise Not_found if the corresponding subpattern did not capture
           a substring. *)

val get_substring_ofs : substrings -> int -> int * int
(** [get_substring_ofs substrings n] @return the offset tuple of the
    [n]th substring of [substrings] (0 is whole match).

    @raise Invalid_argument if [n] is not in the range of the number
           of substrings.
    @raise Not_found if the corresponding subpattern did not capture
           a substring. *)

val get_substrings :
  ?full_match : bool ->
  substrings -> string array
(** [get_substrings ?full_match substrings] @return the array of
    substrings in [substrings]. It includes the full match at index 0
    when [full_match] is [true], the captured substrings only when it
    is [false]. If a subpattern did not capture a substring, the empty
    string is returned in the corresponding position instead.

    @param full_match default = true *)

val get_opt_substrings :
  ?full_match : bool ->
  substrings -> string option array
(** [get_opt_substrings ?full_match substrings] @return the array of
    optional substrings in [substrings]. It includes [Some full_match_str]
    at index 0 when [full_match] is [true], [Some captured_substrings]
    only when it is [false]. If a subpattern did not capture a substring,
    [None] is returned in the corresponding position instead.

    @param full_match default = true *)

val get_named_substring : regexp -> string -> substrings -> string
(** [get_named_substring rex name substrings] @return the named substring
    [name] in regular expression [rex] and [substrings].

    @raise Invalid_argument if there is no such named substring.
    @raise Not_found if the corresponding subpattern did not capture
           a substring. *)

val get_named_substring_ofs : regexp -> string -> substrings -> int * int
(** [get_named_substring_ofs rex name substrings] @return the offset
    tuple of the named substring [name] in regular expression [rex] and
    [substrings].

    @raise Invalid_argument if there is no such named substring.
    @raise Not_found if the corresponding subpattern did not capture
           a substring. *)


(** {6 Callouts} *)

type callout_data =
  {
    callout_number : int; (** Callout number *)
    substrings : substrings; (** Substrings matched so far *)
    start_match : int;  (** Subject start offset of current match attempt *)
    current_position : int;  (** Subject offset of current match pointer *)
    capture_top : int;  (** Number of the highest captured substring so far *)
    capture_last : int;  (** Number of the most recently captured substring *)
    pattern_position : int;  (** Offset of next match item in pattern string *)
    next_item_length : int;  (** Length of next match item in pattern string *)
  }

(** Type of callout functions *)
type callout = callout_data -> unit
(** Callouts are referred to in patterns as "(?Cn)" where "n" is a
    [callout_number] ranging from 0 to 255.  Substrings captured so far
    are accessible as usual via [substrings].  You will have to consider
    [capture_top] and [capture_last] to know about the current state of
    valid substrings.

    By raising exception [Backtrack] within a callout function, the user
    can force the pattern matching engine to backtrack to other possible
    solutions.  Other exceptions will terminate matching immediately
    and return control to OCaml.
*)


(** {6 Matching of patterns and subpattern extraction} *)

val pcre2_match :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  string -> int array
(** [pcre2_match ?iflags ?flags ?rex ?pat ?pos ?callout subj] @return an
    array of offsets that describe the position of matched subpatterns in
    the string [subj] starting at position [pos] with pattern [pat] when
    given, regular expression [rex] otherwise. The array also contains
    additional workspace needed by the match engine. Uses [flags] when
    given, the precompiled [iflags] otherwise. Callouts are handled by
    [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val pcre2_dfa_match :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  ?workspace : int array ->
  string -> int array
(** [pcre2_dfa_match ?iflags ?flags ?rex ?pat ?pos ?callout ?workspace subj]
    invokes the "alternative" DFA matching function.

    @return an array of offsets that describe the position of matched
    subpatterns in the string [subj] starting at position [pos] with pattern
    [pat] when given, regular expression [rex] otherwise. The array also
    contains additional workspace needed by the match engine. Uses [flags]
    when given, the precompiled [iflags] otherwise. Requires a
    sufficiently-large [workspace] array. Callouts are handled by [callout].

    Note that the returned array of offsets are quite different from those
    returned by {!pcre2_match} et al.  The motivating use case for the DFA
    match function is to be able to restart a partial match with N additional
    input segments.  Because the match function/workspace does not store
    segments seen previously, the offsets returned when a match completes will
    refer only to the matching portion of the last subject string provided.
    Thus, returned offsets from this function should not be used to support
    extracting captured submatches.  If you need to capture submatches
    from a series of inputs incrementally matched with this function, you'll
    need to concatenate those inputs that yield a successful match here and
    re-run the same pattern against that single subject string.

    Aside from an absolute minimum of [20], PCRE does not provide any
    guidance regarding the size of workspace array needed by any given
    pattern.  Therefore, it is wise to appropriately handle the possible
    [WorkspaceSize] error.  If raised, you can allocate a new, larger
    workspace array and begin the DFA matching process again.

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts
    @param workspace default = fresh array of length [20]

    @raise Not_found if the pattern match has failed
    @raise Error Partial if the pattern has matched partially; a subsequent
                         exec call with the same pattern and workspace
                         (adding the [DFA_RESTART] flag) be made to either
                         further advance or complete the partial match.
    @raise Error WorkspaceSize if the workspace array is too small to
                               accommodate the DFA state required by the
                               supplied pattern *)

val exec :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  string -> substrings
(** [exec ?iflags ?flags ?rex ?pat ?pos ?callout subj] @return substring
    information on string [subj] starting at position [pos] with pattern
    [pat] when given, regular expression [rex] otherwise. Uses [flags]
    when given, the precompiled [iflags] otherwise. Callouts are handled
    by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val exec_all :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  string -> substrings array
(** [exec_all ?iflags ?flags ?rex ?pat ?pos ?callout subj] @return
    an array of substring information of all matching substrings in
    string [subj] starting at position [pos] with pattern [pat] when
    given, regular expression [rex] otherwise. Uses [flags] when given,
    the precompiled [iflags] otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val next_match :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  substrings -> substrings
(** [next_match ?iflags ?flags ?rex ?pat ?pos ?callout substrs] @return
    substring information on the match that follows on the last
    match denoted by [substrs], jumping over [pos] characters (also
    backwards!), using pattern [pat] when given, regular expression
    [rex] otherwise. Uses [flags] when given, the precompiled [iflags]
    otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match.
    @raise Invalid_arg if [pos] let matching start outside of
           the subject string. *)

val extract :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?full_match : bool ->
  ?callout : callout ->
  string -> string array
(** [extract ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj]
    @return the array of substrings that match [subj] starting at
    position [pos], using pattern [pat] when given, regular expression
    [rex] otherwise. Uses [flags] when given, the precompiled [iflags]
    otherwise. It includes the full match at index 0 when [full_match] is
    [true], the captured substrings only when it is [false]. Callouts are
    handled by [callout].  If a subpattern did not capture a substring,
    the empty string is returned in the corresponding position instead.

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param full_match default = true
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val extract_opt :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?full_match : bool ->
  ?callout : callout ->
  string -> string option array
(** [extract_opt ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj]
    @return the array of optional substrings that match [subj] starting
    at position [pos], using pattern [pat] when given, regular expression
    [rex] otherwise. Uses [flags] when given, the precompiled [iflags]
    otherwise. It includes [Some full_match_str] at index 0 when
    [full_match] is [true], [Some captured-substrings] only when it is
    [false]. Callouts are handled by [callout].  If a subpattern did
    not capture a substring, [None] is returned in the corresponding
    position instead.

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param full_match default = true
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val extract_all :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?full_match : bool ->
  ?callout : callout ->
  string -> string array array
(** [extract_all ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj]
    @return an array of arrays of all matching substrings that match
    [subj] starting at position [pos], using pattern [pat] when given,
    regular expression [rex] otherwise. Uses [flags] when given, the
    precompiled [iflags] otherwise. It includes the full match at index
    0 of the extracted string arrays when [full_match] is [true], the
    captured substrings only when it is [false]. Callouts are handled by
    [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param full_match default = true
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val extract_all_opt :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?full_match : bool ->
  ?callout : callout ->
  string -> string option array array
(** [extract_all_opt
      ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj]
    @return an array of arrays of all optional matching substrings that
    match [subj] starting at position [pos], using pattern [pat] when
    given, regular expression [rex] otherwise. Uses [flags] when given,
    the precompiled [iflags] otherwise. It includes [Some full_match_str]
    at index 0 of the extracted string arrays when [full_match] is [true],
    [Some captured_substrings] only when it is [false]. Callouts are
    handled by [callout].  If a subpattern did not capture a substring,
    [None] is returned in the corresponding position instead.

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param full_match default = true
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val pmatch :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  string -> bool
(** [pmatch ?iflags ?flags ?rex ?pat ?pos ?callout subj] @return [true]
    if [subj] is matched by pattern [pat] when given, regular expression
    [rex] otherwise, starting at position [pos]. Uses [flags] when given,
    the precompiled [iflags] otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts *)


(** {6 String substitution} *)

(** Information on substitution patterns *)
type substitution

val subst : string -> substitution
(** [subst str] converts the string [str] representing a
    substitution pattern to the internal representation

    The contents of the substitution string [str] can be normal text
    mixed with any of the following (mostly as in PERL):

    - {e $\[0-9\]+}  - a "$" immediately followed by an arbitrary number.
                       "$0" stands for the name of the executable,
                       any other number for the n-th backreference.
    - {e $&}         - the whole matched pattern
    - {e $`}         - the text before the match
    - {e $'}         - the text after the match
    - {e $+}         - the last group that matched
    - {e $$}         - a single "$"
    - {e $!}         - delimiter which does not appear in the substitution.
                       Can be used to part "$[0-9]+" from an immediately
                       following other number. *)

val replace :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?itempl : substitution ->
  ?templ : string ->
  ?callout : callout ->
  string -> string
(** [replace ?iflags ?flags ?rex ?pat ?pos ?itempl ?templ ?callout subj]
    replaces all substrings of [subj] matching pattern [pat] when given,
    regular expression [rex] otherwise, starting at position [pos] with
    the substitution string [templ] when given, [itempl] otherwise. Uses
    [flags] when given, the precompiled [iflags] otherwise. Callouts
    are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param itempl default = empty string
    @param templ default = ignored
    @param callout default = ignore callouts

    @raise Failure if there are backreferences to nonexistent subpatterns. *)

val qreplace :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?templ : string ->
  ?callout : callout ->
  string -> string
(** [qreplace ?iflags ?flags ?rex ?pat ?pos ?templ ?callout subj]
    replaces all substrings of [subj] matching pattern [pat] when given,
    regular expression [rex] otherwise, starting at position [pos]
    with the string [templ]. Uses [flags] when given, the precompiled
    [iflags] otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param templ default = ignored
    @param callout default = ignore callouts *)

val substitute_substrings :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  subst : (substrings -> string) ->
  string -> string
(** [substitute_substrings ?iflags ?flags ?rex ?pat ?pos ?callout ~subst subj]
    replaces all substrings of [subj] matching pattern [pat] when given,
    regular expression [rex] otherwise, starting at position [pos]
    with the result of function [subst] applied to the substrings
    of the match. Uses [flags] when given, the precompiled [iflags]
    otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts *)

val substitute :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  subst : (string -> string) ->
  string -> string
(** [substitute ?iflags ?flags ?rex ?pat ?pos ?callout ~subst subj]
    replaces all substrings of [subj] matching pattern [pat] when given,
    regular expression [rex] otherwise, starting at position [pos] with
    the result of function [subst] applied to the match. Uses [flags]
    when given, the precompiled [iflags] otherwise. Callouts are handled
    by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts *)

val replace_first :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?itempl : substitution ->
  ?templ : string ->
  ?callout : callout ->
  string -> string
(** [replace_first ?iflags ?flags ?rex ?pat ?pos ?itempl ?templ ?callout subj]
    replaces the first substring of [subj] matching pattern [pat] when
    given, regular expression [rex] otherwise, starting at position
    [pos] with the substitution string [templ] when given, [itempl]
    otherwise. Uses [flags] when given, the precompiled [iflags]
    otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param itempl default = empty string
    @param templ default = ignored
    @param callout default = ignore callouts

    @raise Failure if there are backreferences to nonexistent subpatterns. *)

val qreplace_first :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?templ : string ->
  ?callout : callout ->
  string -> string
(** [qreplace_first ?iflags ?flags ?rex ?pat ?pos ?templ ?callout subj]
    replaces the first substring of [subj] matching pattern [pat] when
    given, regular expression [rex] otherwise, starting at position [pos]
    with the string [templ]. Uses [flags] when given, the precompiled
    [iflags] otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param templ default = ignored
    @param callout default = ignore callouts *)

val substitute_substrings_first :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  subst : (substrings -> string) ->
  string -> string
(** [substitute_substrings_first
       ?iflags ?flags ?rex ?pat ?pos ?callout ~subst subj]
    replaces the first substring of [subj] matching pattern [pat] when
    given, regular expression [rex] otherwise, starting at position
    [pos] with the result of function [subst] applied to the substrings
    of the match. Uses [flags] when given, the precompiled [iflags]
    otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts *)

val substitute_first :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  subst : (string -> string) ->
  string -> string
(** [substitute_first ?iflags ?flags ?rex ?pat ?pos ?callout ~subst subj]
    replaces the first substring of [subj] matching pattern [pat] when
    given, regular expression [rex] otherwise, starting at position
    [pos] with the result of function [subst] applied to the match. Uses
    [flags] when given, the precompiled [iflags] otherwise. Callouts
    are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts *)


(** {6 Splitting} *)

val split :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?max : int ->
  ?callout : callout ->
  string -> string list
(** [split ?iflags ?flags ?rex ?pat ?pos ?max ?callout subj] splits [subj]
    into a list of at most [max] strings, using as delimiter pattern
    [pat] when given, regular expression [rex] otherwise, starting at
    position [pos]. Uses [flags] when given, the precompiled [iflags]
    otherwise. If [max] is zero, trailing empty fields are stripped. If
    it is negative, it is treated as arbitrarily large. If neither [pat]
    nor [rex] are specified, leading whitespace will be stripped! Should
    behave exactly as in PERL. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param max default = 0
    @param callout default = ignore callouts *)

val asplit :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?max : int ->
  ?callout : callout ->
  string -> string array
(** [asplit ?iflags ?flags ?rex ?pat ?pos ?max ?callout subj] same as
    {!Pcre2.split} but @return an array instead of a list. *)

(** Result of a {!Pcre2.full_split} *)
type split_result = Text of string        (** Text part of split string *)
                  | Delim of string       (** Delimiter part of split string *)
                  | Group of int * string (** Subgroup of matched delimiter
                                              (subgroup_nr, subgroup_str) *)
                  | NoGroup               (** Unmatched subgroup *)

val full_split :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?max : int ->
  ?callout : callout ->
  string -> split_result list
(** [full_split ?iflags ?flags ?rex ?pat ?pos ?max ?callout subj] splits
    [subj] into a list of at most [max] elements of type "split_result",
    using as delimiter pattern [pat] when given, regular expression
    [rex] otherwise, starting at position [pos]. Uses [flags] when given,
    the precompiled [iflags] otherwise. If [max] is zero, trailing empty
    fields are stripped. If it is negative, it is treated as arbitrarily
    large. Should behave exactly as in PERL. Callouts are handled by
    [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param max default = 0
    @param callout default = ignore callouts *)


(** {6 Additional convenience functions} *)

val foreach_line :
  ?ic : in_channel ->
  (string -> unit) -> unit
(** [foreach_line ?ic f] applies [f] to each line in inchannel [ic] until
    the end-of-file is reached.

    @param ic default = stdin *)

val foreach_file : string list -> (string -> in_channel -> unit) -> unit
(** [foreach_file filenames f] opens each file in the list [filenames]
    for input and applies [f] to each filename and the corresponding
    channel. Channels are closed after each operation (even when
    exceptions occur - they get reraised afterwards!). *)


(** {6 {b UNSAFE STUFF - USE WITH CAUTION!}} *)

val unsafe_pcre2_match :
  irflag ->
  regexp ->
  pos : int ->
  subj_start : int ->
  subj : string ->
  int array ->
  callout option ->
  unit
(** [unsafe_pcre2_match flags rex ~pos ~subj_start ~subj offset_vector callout].
    You should read the C-source to know what happens.
    If you do not understand it - {b don't use this function!} *)

val make_ovector : regexp -> int * int array
(** [make_ovector regexp] calculates the tuple (subgroups2, ovector)
    which is the number of subgroup offsets and the offset array. *)

val unsafe_pcre2_dfa_match :
  irflag ->
  regexp ->
  pos : int ->
  subj_start : int ->
  subj : string ->
  int array ->
  callout option ->
  workspace : int array ->
  unit
(** [unsafe_pcre2_dfa_match flags rex ~pos ~subj_start ~subj offset_vector callout
    ~workpace].
    You should read the C-source to know what happens.
    If you do not understand it - {b don't use this function!} *)
