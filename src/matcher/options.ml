(* Moved verbatim from src/pcre2.ml (Matcher-API factoring, fast-engine
   milestone chunk A). Bit values are the frozen PCRE2 option encodings. *)

module Jit = struct
  type matching_mode = JIT_COMPLETE | JIT_PARTIAL_SOFT | JIT_PARTIAL_HARD
  [@@deriving show, eq]

  let int32_of_matching_mode : matching_mode -> int32 = function
    | JIT_COMPLETE     -> 0x00000001l
    | JIT_PARTIAL_SOFT -> 0x00000002l
    | JIT_PARTIAL_HARD -> 0x00000004l
  [@@ocamlformat "disable"]

  type jit_only_compile_option = [ `JIT_INVALID_UTF ]
  (* [@deprecated "MATCH_INVALID_UTF should be used instead"] *)
  (* deprecated (not sure when v10.34ish?) - use MATCH_INVALID_UTF *)
  [@@deriving show, eq]

  let int32_of_compile_option : jit_only_compile_option -> int32 = function
    | (`JIT_INVALID_UTF [@alert "-deprecated"]) -> 0x00000100l

  let bitvector_of_compile_options (opts : jit_only_compile_option list) :
      int32 =
    opts |> List.map int32_of_compile_option |> List.fold_left Int32.logor 0l

  type match_option =
    [ `NOTBOL
    | `NOTEOL
    | `NOTEMPTY
    | `NOTEMPTY_ATSTART
    | `PARTIAL_SOFT
    | `PARTIAL_HARD ]
  [@@deriving show, eq]

  let int32_of_match_option : match_option -> int32 = function
    | `NOTBOL           -> 0x00000001l
    | `NOTEOL           -> 0x00000002l
    | `NOTEMPTY         -> 0x00000004l
    | `NOTEMPTY_ATSTART -> 0x00000008l
    | `PARTIAL_HARD     -> 0x00000010l
    | `PARTIAL_SOFT     -> 0x00000020l
  [@@ocamlformat "disable"]

  let bitvector_of_match_options (opts : match_option list) : int32 =
    opts |> List.map int32_of_match_option |> List.fold_left Int32.logor 0l
end

module Interp = struct
  type match_option =
    (* shared *)
    [ Jit.match_option
    | `COPY_MATCHED_SUBJECT
    | `DISABLE_RECURSELOOP_CHECK
    | `NO_JIT ]
  [@@deriving show, eq]

  let int32_of_match_option : match_option -> int32 = function
    | #Jit.match_option as jit_opt -> Jit.int32_of_match_option jit_opt
    | `COPY_MATCHED_SUBJECT -> 0x00004000l
    | `DISABLE_RECURSELOOP_CHECK -> 0x00040000l
    | `NO_JIT -> 0x00002000l

  let bitvector_of_match_options (opts : match_option list) : int32 =
    opts |> List.map int32_of_match_option |> List.fold_left Int32.logor 0l

  type compile_match_options = [ `ANCHORED | `NO_UTF_CHECK | `ENDANCHORED ]
  [@@deriving show, eq]

  let int32_of_compile_match_option : compile_match_options -> int32 = function
    | `ANCHORED     -> 0x80000000l
    | `NO_UTF_CHECK -> 0x40000000l
    | `ENDANCHORED  -> 0x20000000l
  [@@ocamlformat "disable"]

  type compile_option =
    [ compile_match_options
    | `ALLOW_EMPTY_CLASS
    | `ALT_BSUX
    | `AUTO_CALLOUT
    | `CASELESS
    | `DOLLAR_ENDONLY
    | `DOTALL
    | `DUPNAMES
    | `EXTENDED
    | `FIRSTLINE
    | `MATCH_UNSET_BACKREF
    | `MULTILINE
    | `NEVER_UCP
    | `NEVER_UTF
    | `NO_AUTO_CAPTURE
    | `NO_AUTO_POSSESS
    | `NO_DOTSTAR_ANCHOR
    | `NO_START_OPTIMIZE
    | `UCP
    | `UNGREEDY
    | `UTF
    | `NEVER_BACKSLASH_C
    | `ALT_CIRCUMFLEX
    | `ALT_VERBNAMES
    | `USE_OFFSET_LIMIT
    | `EXTENDED_MORE
    | `LITERAL
    | `MATCH_INVALID_UTF ]
  [@@deriving show, eq]

  let int32_of_compile_option : compile_option -> int32 = function
    | #compile_match_options as opt -> int32_of_compile_match_option opt
    | `ALLOW_EMPTY_CLASS            -> 0x00000001l
    | `ALT_BSUX                     -> 0x00000002l
    | `AUTO_CALLOUT                 -> 0x00000004l
    | `CASELESS                     -> 0x00000008l
    | `DOLLAR_ENDONLY               -> 0x00000010l
    | `DOTALL                       -> 0x00000020l
    | `DUPNAMES                     -> 0x00000040l
    | `EXTENDED                     -> 0x00000080l
    | `FIRSTLINE                    -> 0x00000100l
    | `MATCH_UNSET_BACKREF          -> 0x00000200l
    | `MULTILINE                    -> 0x00000400l
    | `NEVER_UCP                    -> 0x00000800l
    | `NEVER_UTF                    -> 0x00001000l
    | `NO_AUTO_CAPTURE              -> 0x00002000l
    | `NO_AUTO_POSSESS              -> 0x00004000l
    | `NO_DOTSTAR_ANCHOR            -> 0x00008000l
    | `NO_START_OPTIMIZE            -> 0x00010000l
    | `UCP                          -> 0x00020000l
    | `UNGREEDY                     -> 0x00040000l
    | `UTF                          -> 0x00080000l
    | `NEVER_BACKSLASH_C            -> 0x00100000l
    | `ALT_CIRCUMFLEX               -> 0x00200000l
    | `ALT_VERBNAMES                -> 0x00400000l
    | `USE_OFFSET_LIMIT             -> 0x00800000l
    | `EXTENDED_MORE                -> 0x01000000l
    | `LITERAL                      -> 0x02000000l
    | `MATCH_INVALID_UTF            -> 0x04000000l
  [@@ocamlformat "disable"]

  let bitvector_of_compile_options (opts : compile_option list) : int32 =
    opts |> List.map int32_of_compile_option |> List.fold_left Int32.logor 0l

  (* for compile ctx - can combine and just split back as needed in bindings? *)
  type compile_ctx =
    [ `EXTRA_ALLOW_SURROGATE_ESCAPES
    | `EXTRA_BAD_ESCAPE_IS_LITERAL
    | `EXTRA_MATCH_WORD
    | `EXTRA_MATCH_LINE
    | `EXTRA_ESCAPED_CR_IS_LF
    | `EXTRA_ALT_BSUX
    | `EXTRA_ALLOW_LOOKAROUND_BSK
    | (* These since 10.43 *)
      (* TODO: verify? what should we do about versioning?? *)
      `EXTRA_CASELESS_RESTRICT
    | `EXTRA_ASCII_BSD
    | `EXTRA_ASCII_BSS
    | `EXTRA_ASCII_BSW
    | `EXTRA_ASCII_POSIX
    | `EXTRA_ASCII_DIGIT ]
    (* TODO: impl these for compile *)
  [@@deriving show, eq]

  let int32_of_compile_ctx_option : compile_ctx -> int32 = function
    | `EXTRA_ALLOW_SURROGATE_ESCAPES -> 0x00000001l
    | `EXTRA_BAD_ESCAPE_IS_LITERAL   -> 0x00000002l
    | `EXTRA_MATCH_WORD              -> 0x00000004l
    | `EXTRA_MATCH_LINE              -> 0x00000008l
    | `EXTRA_ESCAPED_CR_IS_LF        -> 0x00000010l
    | `EXTRA_ALT_BSUX                -> 0x00000020l
    | `EXTRA_ALLOW_LOOKAROUND_BSK    -> 0x00000040l
    (* Assumed values. TODO: verify *)
    | `EXTRA_CASELESS_RESTRICT       -> 0x00000080l
    | `EXTRA_ASCII_BSD               -> 0x00000100l
    | `EXTRA_ASCII_BSS               -> 0x00000200l
    | `EXTRA_ASCII_BSW               -> 0x00000400l
    | `EXTRA_ASCII_POSIX             -> 0x00000800l
    | `EXTRA_ASCII_DIGIT             -> 0x00001000l
  [@@ocamlformat "disable"]

  type subst_options =
    (* shared *)
    [ Jit.match_option
    | compile_match_options
    | `NO_JIT
    | (* exclusive *)
      `SUBSTITUTE_GLOBAL
    | `SUBSTITUTE_EXTENDED
    | `SUBSTITUTE_UNSET_EMPTY
    | `SUBSTITUTE_UNKNOWN_UNSET
    | `SUBSTITUTE_OVERFLOW_LENGTH
    | `SUBSTITUTE_LITERAL
    | `SUBSTITUTE_MATCHED
    | `SUBSTITUTE_REPLACEMENT_ONLY ]
  [@@deriving show, eq]

  type newline_compile_ctx_option =
    | NEWLINE_CR
    | NEWLINE_LF
    | NEWLINE_CRLF
    | NEWLINE_ANY
    | NEWLINE_ANYCRLF
    | NEWLINE_NUL
  [@@deriving show, eq]

  type bsr = BSR_UNICODE | ANYCRLF [@@deriving show, eq]
end
