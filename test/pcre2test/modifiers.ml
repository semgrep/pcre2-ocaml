(* Modifier decoding, ported from pcre2test.c decode_modifiers()
   (pcre2test.c:3769-4136), check_modifier() (pcre2test.c:3680-3743) and the
   modlist table (pcre2test.c:643-784).

   Every pcre2test modifier is RECOGNIZED (so unknown-modifier errors match
   the C exactly); modifiers whose behavior is out of scope for the harness
   carry the [Skip] action: their value is parsed and discarded and the
   enclosing test unit is reported as skipped with reason "modifier:<name>". *)

(* pcre2test.c:450-459 *)
type which = CTC | CTM | PAT | PATP | DAT | DATP | PD | PDP | PND | PNDP

(* pcre2test.c:460-472 — modifier value types *)
type mtype =
  | OptT of int (* option bit *)
  | CtlT of int (* control bit, first word *)
  | Ctl2T of int (* control bit, second word *)
  | BsrT
  | ChrT
  | ConT
  | In2T
  | InsT
  | IntT
  | IndT of int (* unsigned integer, no value => default *)
  | NlT
  | NnT
  | SizT
  | StrT of int (* max size including terminating NUL *)

(* What the harness does with a successfully parsed modifier. *)
type act =
  | Apply (* option/control bit applied to the resolved block *)
  | Extra of int (* compile-context extra option (CTC) *)
  | Set_newline
  | Set_bsr
  | Set_offset
  | Set_oveccount
  | Nn_copy
  | Nn_get
  | Noop (* recognized; intentionally no effect (behavior already matches) *)
  | Skip (* recognized; out of harness scope -> unit skipped *)

type entry = { name : string; which : which; mtype : mtype; act : act }

let e name which mtype act = { name; which; mtype; act }

(* pcre2test.c:643-784 modlist, in the same collating order. *)
let modlist : entry array =
  [|
    e "aftertext" PNDP (CtlT Ctl.ctl_aftertext) Apply;
    e "allaftertext" PNDP (CtlT Ctl.ctl_allaftertext) Apply;
    e "allcaptures" PND (CtlT Ctl.ctl_allcaptures) Apply;
    e "allow_empty_class" PAT (OptT Flags.allow_empty_class) Apply;
    e "allow_lookaround_bsk" CTC (OptT Flags.extra_allow_lookaround_bsk)
      (Extra Flags.extra_allow_lookaround_bsk);
    e "allow_surrogate_escapes" CTC (OptT Flags.extra_allow_surrogate_escapes)
      (Extra Flags.extra_allow_surrogate_escapes);
    e "allusedtext" PNDP (CtlT Ctl.ctl_allusedtext) Skip;
    e "allvector" PND (Ctl2T 0x00000800) Skip;
    e "alt_bsux" PAT (OptT Flags.alt_bsux) Apply;
    e "alt_circumflex" PAT (OptT Flags.alt_circumflex) Apply;
    e "alt_verbnames" PAT (OptT Flags.alt_verbnames) Apply;
    e "altglobal" PND (CtlT Ctl.ctl_altglobal) Apply;
    e "anchored" PD (OptT Flags.anchored) Apply;
    e "ascii_all" CTC (OptT Flags.extra_ascii_all) (Extra Flags.extra_ascii_all);
    e "ascii_bsd" CTC (OptT Flags.extra_ascii_bsd) (Extra Flags.extra_ascii_bsd);
    e "ascii_bss" CTC (OptT Flags.extra_ascii_bss) (Extra Flags.extra_ascii_bss);
    e "ascii_bsw" CTC (OptT Flags.extra_ascii_bsw) (Extra Flags.extra_ascii_bsw);
    e "ascii_digit" CTC (OptT Flags.extra_ascii_digit)
      (Extra Flags.extra_ascii_digit);
    e "ascii_posix" CTC (OptT Flags.extra_ascii_posix)
      (Extra Flags.extra_ascii_posix);
    e "auto_callout" PAT (OptT Flags.auto_callout) Skip;
    e "bad_escape_is_literal" CTC (OptT Flags.extra_bad_escape_is_literal)
      (Extra Flags.extra_bad_escape_is_literal);
    e "bincode" PAT (CtlT 0x00000020) Skip;
    e "bsr" CTC BsrT Set_bsr;
    e "callout_capture" DAT (CtlT 0x00000040) Skip;
    e "callout_data" DAT InsT Skip;
    e "callout_error" DAT In2T Skip;
    e "callout_extra" DAT (Ctl2T 0x00000400) Skip;
    e "callout_fail" DAT In2T Skip;
    e "callout_info" PAT (CtlT 0x00000080) Skip;
    e "callout_no_where" DAT (Ctl2T 0x00000200) Skip;
    e "callout_none" DAT (CtlT 0x00000100) Noop;
    e "caseless" PATP (OptT Flags.caseless) Apply;
    e "caseless_restrict" CTC (OptT Flags.extra_caseless_restrict)
      (Extra Flags.extra_caseless_restrict);
    e "convert" PAT ConT Skip;
    e "convert_glob_escape" PAT ChrT Skip;
    e "convert_glob_separator" PAT ChrT Skip;
    e "convert_length" PAT IntT Skip;
    e "copy" DAT NnT Nn_copy;
    e "copy_matched_subject" DAT (OptT Flags.copy_matched_subject) Apply;
    e "debug" PAT (CtlT 0x00022000) Skip;
    e "depth_limit" CTM IntT Skip;
    e "dfa" DAT (CtlT 0x00000200) Skip;
    e "dfa_restart" DAT (OptT Flags.dfa_restart) Skip;
    e "dfa_shortest" DAT (OptT Flags.dfa_shortest) Skip;
    e "disable_recurseloop_check" DAT (OptT Flags.disable_recurseloop_check)
      Apply;
    e "dollar_endonly" PAT (OptT Flags.dollar_endonly) Apply;
    e "dotall" PATP (OptT Flags.dotall) Apply;
    e "dupnames" PATP (OptT Flags.dupnames) Apply;
    e "endanchored" PD (OptT Flags.endanchored) Apply;
    e "escaped_cr_is_lf" CTC (OptT Flags.extra_escaped_cr_is_lf)
      (Extra Flags.extra_escaped_cr_is_lf);
    e "expand" PAT (CtlT Ctl.ctl_expand) Apply;
    e "extended" PATP (OptT Flags.extended) Apply;
    e "extended_more" PATP (OptT Flags.extended_more) Apply;
    e "extra_alt_bsux" CTC (OptT Flags.extra_alt_bsux)
      (Extra Flags.extra_alt_bsux);
    e "find_limits" DAT (CtlT 0x00000800) Skip;
    e "find_limits_noheap" DAT (CtlT 0x00001000) Skip;
    e "firstline" PAT (OptT Flags.firstline) Apply;
    e "framesize" PAT (Ctl2T 0x00008000) Skip;
    e "fullbincode" PAT (CtlT 0x00002000) Skip;
    e "get" DAT NnT Nn_get;
    e "getall" DAT (CtlT Ctl.ctl_getall) Apply;
    e "global" PNDP (CtlT Ctl.ctl_global) Apply;
    e "heap_limit" CTM IntT Skip;
    e "heapframes_size" PND (Ctl2T 0x20000000) Skip;
    e "hex" PAT (CtlT Ctl.ctl_hexpat) Apply;
    e "info" PAT (CtlT 0x00020000) Skip;
    e "jit" PAT (IndT 7) Skip;
    e "jitfast" PAT (CtlT 0x00040000) Skip;
    (* jitstack only sizes the JIT stack; with interpreter matching it
       produces no output (pcre2test.c:7481-7502) *)
    e "jitstack" PNDP IntT Noop;
    e "jitverify" PAT (CtlT 0x00080000) Skip;
    e "literal" PAT (OptT Flags.literal) Apply;
    e "locale" PAT (StrT 32) Skip;
    e "mark" PNDP (CtlT Ctl.ctl_mark) Apply;
    e "match_invalid_utf" PAT (OptT Flags.match_invalid_utf) Apply;
    e "match_limit" CTM IntT Skip;
    e "match_line" CTC (OptT Flags.extra_match_line)
      (Extra Flags.extra_match_line);
    e "match_unset_backref" PAT (OptT Flags.match_unset_backref) Apply;
    e "match_word" CTC (OptT Flags.extra_match_word)
      (Extra Flags.extra_match_word);
    e "max_pattern_compiled_length" CTC SizT Skip;
    e "max_pattern_length" CTC SizT Skip;
    e "max_varlookbehind" CTC IntT Skip;
    e "memory" PD (CtlT 0x00200000) Skip;
    e "multiline" PATP (OptT Flags.multiline) Apply;
    e "never_backslash_c" PAT (OptT Flags.never_backslash_c) Apply;
    e "never_ucp" PAT (OptT Flags.never_ucp) Apply;
    e "never_utf" PAT (OptT Flags.never_utf) Apply;
    e "newline" CTC NlT Set_newline;
    e "no_auto_capture" PAT (OptT Flags.no_auto_capture) Apply;
    e "no_auto_possess" PATP (OptT Flags.no_auto_possess) Apply;
    e "no_dotstar_anchor" PAT (OptT Flags.no_dotstar_anchor) Apply;
    e "no_jit" DATP (OptT Flags.no_jit) Apply;
    e "no_start_optimize" PATP (OptT Flags.no_start_optimize) Apply;
    e "no_utf_check" PD (OptT Flags.no_utf_check) Apply;
    e "notbol" DAT (OptT Flags.notbol) Apply;
    e "notempty" DAT (OptT Flags.notempty) Apply;
    e "notempty_atstart" DAT (OptT Flags.notempty_atstart) Apply;
    e "noteol" DAT (OptT Flags.noteol) Apply;
    e "null_context" PD (CtlT 0x00400000) Noop;
    e "null_pattern" PAT (Ctl2T 0x00001000) Skip;
    e "null_replacement" DAT (Ctl2T 0x00004000) Skip;
    e "null_subject" DAT (Ctl2T 0x00002000) Skip;
    e "offset" DAT IntT Set_offset;
    e "offset_limit" CTM SizT Skip;
    e "ovector" DAT IntT Set_oveccount;
    e "parens_nest_limit" CTC IntT Skip;
    e "partial_hard" DAT (OptT Flags.partial_hard) Apply;
    e "partial_soft" DAT (OptT Flags.partial_soft) Apply;
    e "ph" DAT (OptT Flags.partial_hard) Apply;
    e "posix" PAT (CtlT 0x00800000) Skip;
    e "posix_nosub" PAT (CtlT 0x01800000) Skip;
    e "posix_startend" DAT In2T Skip;
    e "ps" DAT (OptT Flags.partial_soft) Apply;
    e "push" PAT (CtlT 0x02000000) Skip;
    e "pushcopy" PAT (CtlT 0x04000000) Skip;
    e "pushtablescopy" PAT (CtlT 0x08000000) Skip;
    e "recursion_limit" CTM IntT Skip;
    e "regerror_buffsize" PAT IntT Skip;
    e "replace" PND (StrT 100) Skip;
    e "stackguard" PAT IntT Skip;
    e "startchar" PND (CtlT Ctl.ctl_startchar) Apply;
    e "startoffset" DAT IntT Set_offset;
    e "subject_literal" PATP (Ctl2T Ctl.ctl2_subject_literal) Apply;
    e "substitute_callout" PND (Ctl2T 0x00000001) Skip;
    e "substitute_extended" PND (Ctl2T 0x00000002) Skip;
    e "substitute_literal" PND (Ctl2T 0x00000004) Skip;
    e "substitute_matched" PND (Ctl2T 0x00000008) Skip;
    e "substitute_overflow_length" PND (Ctl2T 0x00000010) Skip;
    e "substitute_replacement_only" PND (Ctl2T 0x00000020) Skip;
    e "substitute_skip" PND IntT Skip;
    e "substitute_stop" PND IntT Skip;
    e "substitute_unknown_unset" PND (Ctl2T 0x00000040) Skip;
    e "substitute_unset_empty" PND (Ctl2T 0x00000080) Skip;
    e "tables" PAT IntT Skip;
    e "ucp" PATP (OptT Flags.ucp) Apply;
    e "ungreedy" PAT (OptT Flags.ungreedy) Apply;
    e "use_length" PAT (CtlT 0x20000000) Noop;
    e "use_offset_limit" PAT (OptT Flags.use_offset_limit) Apply;
    e "utf" PATP (OptT Flags.utf) Apply;
    e "utf8_input" PAT (CtlT Ctl.ctl_utf8_input) Apply;
    e "zero_terminate" DAT (CtlT Ctl.ctl_zero_terminate) Noop;
  |]

(* pcre2test.c:861-872 c1modlist — single-character abbreviations. *)
let c1modlist =
  [
    ('B', "bincode");
    ('I', "info");
    ('a', "ascii_all");
    ('g', "global");
    ('i', "caseless");
    ('m', "multiline");
    ('n', "no_auto_capture");
    ('r', "caseless_restrict");
    ('s', "dotall");
    ('x', "extended");
  ]

(* pcre2test.c:3630-3654 scan_modifiers: exact-name lookup. *)
let scan_modifiers name =
  let rec go bot top =
    if top <= bot then None
    else
      let mid = (bot + top) / 2 in
      let c = String.compare name modlist.(mid).name in
      if c = 0 then Some modlist.(mid)
      else if c > 0 then go (mid + 1) top
      else go bot mid
  in
  go 0 (Array.length modlist)

type ctx = CtxPat | CtxDefpat | CtxDat | CtxDefdat

(* Resolution result of check_modifier: which block receives the value. *)
type target =
  | Tpat of Ctl.patctl (* pattern control block (options/control words) *)
  | Tdat of Ctl.datctl
  | Tpctx of Ctl.patctl (* compile-context fields inside patctl *)

(* pcre2test.c:3680-3743 check_modifier *)
let check_modifier (m : entry) ~ctx ~(pctl : Ctl.patctl option)
    ~(dctl : Ctl.datctl option) ~restrict_perl ~(emit : string -> unit)
    ~(charmod : char option) : target option =
  let fail () =
    (match charmod with
    | None -> emit (Printf.sprintf "** '%s' is not valid here" m.name)
    | Some c -> emit (Printf.sprintf "** /%c is not valid here" c));
    None
  in
  if
    restrict_perl
    && not (match m.which with PNDP | PATP | DATP | PDP -> true | _ -> false)
  then begin
    emit
      (Printf.sprintf "** '%s' is not allowed in a Perl-compatible test" m.name);
    None
  end
  else
    match m.which with
    | CTC -> (
        match (ctx, pctl) with
        | (CtxDefpat | CtxPat), Some p -> Some (Tpctx p)
        | _ -> fail ())
    | CTM -> (
        (* match-context fields are out of scope; validity still matters *)
        match (ctx, dctl) with
        | (CtxDefdat | CtxDat), Some d -> Some (Tdat d)
        | _ -> fail ())
    | DAT | DATP -> (
        match dctl with Some d -> Some (Tdat d) | None -> fail ())
    | PAT | PATP -> (
        match pctl with Some p -> Some (Tpat p) | None -> fail ())
    | PD | PDP -> (
        match (dctl, pctl) with
        | Some d, _ -> Some (Tdat d)
        | None, Some p -> Some (Tpat p)
        | None, None -> fail ())
    | PND | PNDP -> (
        match (dctl, pctl) with
        | Some d, _ -> Some (Tdat d)
        | None, Some p when ctx <> CtxDefpat -> Some (Tpat p)
        | _ -> fail ())

(* Apply an option/control bit to the resolved block (MOD_CTL / MOD_OPT). *)
let apply_bit target mtype ~off =
  let set cur v = if off then cur land lnot v else cur lor v in
  match (target, mtype) with
  | Tpat p, OptT v -> p.Ctl.options <- set p.Ctl.options v
  | Tdat d, OptT v -> d.Ctl.d_options <- set d.Ctl.d_options v
  | Tpat p, CtlT v -> p.Ctl.control <- set p.Ctl.control v
  | Tdat d, CtlT v -> d.Ctl.d_control <- set d.Ctl.d_control v
  | Tpat p, Ctl2T v -> p.Ctl.control2 <- set p.Ctl.control2 v
  | Tdat d, Ctl2T v -> d.Ctl.d_control2 <- set d.Ctl.d_control2 v
  | _ -> ()

(* Unsigned decimal parse like strtoul: consumes digits from [i]. Returns
   (value, next-index), or None on 32-bit overflow. Caller has verified the
   first character. *)
let parse_u32 at i =
  let v = ref 0 in
  let j = ref i in
  let ov = ref false in
  while Cstr.isdigit (at !j) do
    v := (!v * 10) + (Char.code (at !j) - Char.code '0');
    if !v > 0xFFFFFFFF then ov := true;
    incr j
  done;
  if !ov then None else Some (!v, !j)

let parse_s32 at i =
  let neg = at i = '-' in
  let i' = if neg || at i = '+' then i + 1 else i in
  match parse_u32 at i' with
  | None -> None
  | Some (v, j) ->
      let v = if neg then -v else v in
      if v > 0x7FFFFFFF || v < -0x80000000 then None else Some (v, j)

exception Fail

(* pcre2test.c:3769-4136 decode_modifiers.

   [s] is the raw modifier text (may include the trailing newline).
   [skip] records an out-of-scope modifier. Returns false on error, with the
   pcre2test error lines already emitted. *)
let decode ~ctx ~(pctl : Ctl.patctl option) ~(dctl : Ctl.datctl option)
    ~restrict_perl ~(emit : string -> unit) ~(skip : string -> unit)
    (s : string) : bool =
  (* The C trims trailing whitespace by writing a NUL; emulate with a
     shrinkable limit. *)
  let limit = ref (String.length s) in
  let at i = Cstr.at ~limit:!limit s i in
  let sub i j = if j <= i then "" else String.sub s i (j - i) in
  let p = ref 0 in
  let first = ref true in
  try
    let continue_scan = ref true in
    while !continue_scan do
      (* Skip white space and commas. *)
      while Cstr.isspace (at !p) || at !p = ',' do
        incr p
      done;
      if at !p = '\000' then continue_scan := false
      else begin
        (* Find the end of the item; lose trailing whitespace at end of line. *)
        let ep = ref !p in
        while at !ep <> '\000' && at !ep <> ',' do
          incr ep
        done;
        if at !ep = '\000' then begin
          while !ep > !p && Cstr.isspace (Cstr.at s (!ep - 1)) do
            decr ep
          done;
          limit := !ep (* the C writes *ep = 0 *)
        end;
        let off = at !p = '-' in
        if off then incr p;
        (* Length of a full-length modifier name. *)
        let pp = ref !p in
        while !pp < !ep && at !pp <> '=' do
          incr pp
        done;
        match scan_modifiers (sub !p !pp) with
        | None ->
            (* Single-character abbreviated modifiers (first item only);
               pcre2test.c:3813-3878. *)
            if not !first then begin
              emit (Printf.sprintf "** Unrecognized modifier '%s'" (sub !p !ep));
              if !ep - !p = 1 then
                emit "** Single-character modifiers must come first";
              raise Fail
            end;
            let mp = !p in
            while at !p <> ',' && at !p <> '\n' && at !p <> '\000' do
              let cc = at !p in
              match List.assoc_opt cc c1modlist with
              | None ->
                  emit
                    (Printf.sprintf "** Unrecognized modifier '%c' in '%s'" cc
                       (sub mp !ep));
                  raise Fail
              | Some fullname -> (
                  let m = Option.get (scan_modifiers fullname) in
                  match
                    check_modifier m ~ctx ~pctl ~dctl ~restrict_perl ~emit
                      ~charmod:(Some cc)
                  with
                  | None -> raise Fail
                  | Some target ->
                      (* /x special case: a second appearance changes
                         PCRE2_EXTENDED to PCRE2_EXTENDED_MORE
                         (pcre2test.c:3857-3866). *)
                      let cur_opts =
                        match target with
                        | Tpat pc -> Some pc.Ctl.options
                        | Tdat dc -> Some dc.Ctl.d_options
                        | Tpctx _ -> None
                      in
                      (match (cc, cur_opts) with
                      | 'x', Some o when o land Flags.extended <> 0 -> (
                          let o' =
                            o land lnot Flags.extended lor Flags.extended_more
                          in
                          match target with
                          | Tpat pc -> pc.Ctl.options <- o'
                          | Tdat dc -> dc.Ctl.d_options <- o'
                          | Tpctx _ -> ())
                      | _ -> (
                          match m.act with
                          | Skip -> skip ("modifier:" ^ m.name)
                          | Extra v -> (
                              match target with
                              | Tpctx pc ->
                                  pc.Ctl.ctx_extra <- pc.Ctl.ctx_extra lor v
                              | _ -> ())
                          | _ -> apply_bit target m.mtype ~off:false));
                      incr p)
            done
            (* first stays true, as in the C (single-char path continues). *)
        | Some m -> (
            (* Data-presence checks; pcre2test.c:3884-3906. *)
            let needs_data =
              match m.mtype with
              | CtlT _ | Ctl2T _ | OptT _ -> false
              | IndT _ -> at !pp = '='
              | _ -> true
            in
            if needs_data then begin
              if at !pp <> '=' then begin
                emit (Printf.sprintf "** '=' expected after '%s'" m.name);
                raise Fail
              end;
              incr pp;
              if off then begin
                emit (Printf.sprintf "** '-' is not valid for '%s'" m.name);
                raise Fail
              end
            end
            else if
              at !pp <> ',' && at !pp <> '\n' && at !pp <> ' '
              && at !pp <> '\000'
            then begin
              emit (Printf.sprintf "** Unrecognized modifier '%s'" (sub !p !ep));
              raise Fail
            end;
            let len = !ep - !pp in
            match
              check_modifier m ~ctx ~pctl ~dctl ~restrict_perl ~emit
                ~charmod:None
            with
            | None -> raise Fail
            | Some target ->
                let invalid_value () =
                  emit (Printf.sprintf "** Invalid value in '%s'" (sub !p !ep));
                  raise Fail
                in
                let note_skip () =
                  match m.act with
                  | Skip -> skip ("modifier:" ^ m.name)
                  | _ -> ()
                in
                (* Process according to data type; each arm advances pp past
                   the consumed value, as in the C. *)
                (match m.mtype with
                | CtlT _ | Ctl2T _ | OptT _ -> (
                    match m.act with
                    | Skip -> skip ("modifier:" ^ m.name)
                    | Noop -> ()
                    | Extra v -> (
                        match target with
                        | Tpctx pc ->
                            pc.Ctl.ctx_extra <-
                              (if off then pc.Ctl.ctx_extra land lnot v
                               else pc.Ctl.ctx_extra lor v)
                        | _ -> ())
                    | _ -> apply_bit target m.mtype ~off)
                | BsrT ->
                    (* pcre2test.c:3936-3962 *)
                    (if len = 7 && Cstr.strncmpic s !pp "default" 7 then (
                       match target with
                       | Tpctx pc ->
                           pc.Ctl.ctx_bsr <- 0;
                           pc.Ctl.control2 <-
                             pc.Ctl.control2 land lnot Ctl.ctl2_bsr_set
                       | _ -> ())
                     else
                       let v =
                         if len = 7 && Cstr.strncmpic s !pp "anycrlf" 7 then
                           Some Flags.bsr_anycrlf
                         else if len = 7 && Cstr.strncmpic s !pp "unicode" 7
                         then Some Flags.bsr_unicode
                         else None
                       in
                       match v with
                       | None -> invalid_value ()
                       | Some v -> (
                           match target with
                           | Tpctx pc ->
                               pc.Ctl.ctx_bsr <- v;
                               pc.Ctl.control2 <-
                                 pc.Ctl.control2 lor Ctl.ctl2_bsr_set
                           | _ -> ()));
                    note_skip ();
                    pp := !ep
                | NlT ->
                    (* pcre2test.c:4024-4045 *)
                    let n = Array.length Flags.newline_names in
                    let found = ref (-1) in
                    for i = 0 to n - 1 do
                      if
                        !found < 0
                        && len = String.length Flags.newline_names.(i)
                        && Cstr.strncmpic s !pp Flags.newline_names.(i) len
                      then found := i
                    done;
                    if !found < 0 then invalid_value ();
                    (match target with
                    | Tpctx pc ->
                        if !found = 0 then begin
                          pc.Ctl.ctx_newline <- 0;
                          pc.Ctl.control2 <-
                            pc.Ctl.control2 land lnot Ctl.ctl2_nl_set
                        end
                        else begin
                          pc.Ctl.ctx_newline <- !found;
                          pc.Ctl.control2 <- pc.Ctl.control2 lor Ctl.ctl2_nl_set
                        end
                    | _ -> ());
                    pp := !ep
                | ChrT ->
                    note_skip ();
                    incr pp
                | ConT ->
                    (* convert type list — recognized, out of scope. The C
                       validates each colon-separated name; accept and skip. *)
                    note_skip ();
                    pp := !ep
                | In2T -> (
                    if not (Cstr.isdigit (at !pp)) then invalid_value ();
                    match parse_u32 at !pp with
                    | None -> invalid_value ()
                    | Some (_, j) -> (
                        note_skip ();
                        if at j = ':' then
                          match parse_u32 at (j + 1) with
                          | None -> invalid_value ()
                          | Some (_, j2) -> pp := j2
                        else pp := j))
                | InsT -> (
                    if not (Cstr.isdigit (at !pp)) && at !pp <> '-' then
                      invalid_value ();
                    match parse_s32 at !pp with
                    | None -> invalid_value ()
                    | Some (_, j) ->
                        note_skip ();
                        pp := j)
                | SizT -> (
                    if not (Cstr.isdigit (at !pp)) then invalid_value ();
                    match parse_u32 at !pp with
                    | None -> invalid_value ()
                    | Some (_, j) ->
                        note_skip ();
                        pp := j)
                | IndT _ when len = 0 -> note_skip ()
                | IndT _ | IntT -> (
                    if not (Cstr.isdigit (at !pp)) then invalid_value ();
                    match parse_u32 at !pp with
                    | None -> invalid_value ()
                    | Some (v, j) ->
                        (match (m.act, target) with
                        | Set_offset, Tdat d -> d.Ctl.offset <- v
                        | Set_oveccount, Tdat d -> d.Ctl.oveccount <- v
                        | Skip, _ -> skip ("modifier:" ^ m.name)
                        | _ -> ());
                        pp := j)
                | NnT -> (
                    (* pcre2test.c:4048-4098 — number or name, several may
                       occur. *)
                    let too_many () =
                      emit
                        (Printf.sprintf "** Too many numeric '%s' modifiers"
                           m.name);
                      raise Fail
                    in
                    if Cstr.isdigit (at !pp) || at !pp = '-' then
                      match parse_s32 at !pp with
                      | None -> invalid_value ()
                      | Some (v, j) ->
                          (match (m.act, target) with
                          | Nn_copy, Tdat d ->
                              if v < 0 then d.Ctl.copy_numbers <- []
                              else if List.length d.Ctl.copy_numbers >= 9 then
                                too_many ()
                              else
                                d.Ctl.copy_numbers <- d.Ctl.copy_numbers @ [ v ]
                          | Nn_get, Tdat d ->
                              if v < 0 then d.Ctl.get_numbers <- []
                              else if List.length d.Ctl.get_numbers >= 9 then
                                too_many ()
                              else d.Ctl.get_numbers <- d.Ctl.get_numbers @ [ v ]
                          | _ -> ());
                          pp := j
                    else begin
                      let nm = sub !pp !ep in
                      if String.length nm > 128 then begin
                        emit
                          (Printf.sprintf "** Group name in '%s' is too long"
                             m.name);
                        raise Fail
                      end;
                      let check_room names =
                        let used =
                          List.fold_left
                            (fun a x -> a + String.length x + 1)
                            0 names
                        in
                        if used + String.length nm + 2 > 64 then begin
                          emit
                            (Printf.sprintf
                               "** Too many characters in named '%s' modifiers"
                               m.name);
                          raise Fail
                        end
                      in
                      (match (m.act, target) with
                      | Nn_copy, Tdat d ->
                          check_room d.Ctl.copy_names;
                          d.Ctl.copy_names <- d.Ctl.copy_names @ [ nm ]
                      | Nn_get, Tdat d ->
                          check_room d.Ctl.get_names;
                          d.Ctl.get_names <- d.Ctl.get_names @ [ nm ]
                      | _ -> ());
                      pp := !ep
                    end)
                | StrT maxsz ->
                    if len + 1 > maxsz then begin
                      emit
                        (Printf.sprintf
                           "** Overlong value for '%s' (max %d code units)"
                           m.name (maxsz - 1));
                      raise Fail
                    end;
                    note_skip ();
                    pp := !ep);
                (* pcre2test.c:4101-4109 *)
                if
                  at !pp <> ',' && at !pp <> '\n' && at !pp <> ' '
                  && at !pp <> '\000'
                then begin
                  emit
                    (Printf.sprintf "** Comma expected after modifier item '%s'"
                       m.name);
                  raise Fail
                end;
                p := !pp;
                first := false)
      end
    done;
    true
  with Fail -> false
