(* pcre2test-compatible line-driven test machine, a functor over the driver
   seam. Ported from pcre2test.c main() input loop (pcre2test.c:9520-9615),
   process_command() (pcre2test.c:4997-5245), process_pattern()
   (pcre2test.c:5271-5957) and process_data() (pcre2test.c:6860-8449),
   8-bit mode, interpreter matching only.

   Out-of-scope pcre2test features (JIT, DFA, POSIX, callouts, substitute,
   info/debug output, limits, serialization, locales/tables, ...) are
   RECOGNIZED and reported as unit skip reasons; see modifiers.ml. *)

module Make (D : Driver.S) = struct
  type compiled = {
    code : D.code;
    pat : Ctl.patctl;
    utf : bool; (* overall options & PCRE2_UTF *)
    nl : int; (* newline convention of the compiled pattern *)
    maxcapcount : int;
  }

  type t = {
    mutable def_pat : Ctl.patctl;
    mutable def_dat : Ctl.datctl;
    mutable forbid_utf : int;
    mutable local_newline_default : int;
    mutable restrict_perl : bool;
    mutable compiled : compiled option;
    mutable skipping : bool;
    mutable pending : (char * Buffer.t) option; (* multi-line pattern *)
    mutable aborted : bool;
    mutable unit_skips : string list; (* current unit, reversed *)
    mutable default_skips : string list; (* from #pattern/#subject/#commands *)
    mutable out : string list; (* current call, reversed *)
    mutable build_nl : int option; (* lazily probed build-default newline *)
  }

  let create () =
    {
      def_pat = Ctl.new_patctl ();
      def_dat = Ctl.new_datctl ();
      forbid_utf = 0;
      local_newline_default = 0;
      restrict_perl = false;
      compiled = None;
      skipping = false;
      pending = None;
      aborted = false;
      unit_skips = [];
      default_skips = [];
      out = [];
      build_nl = None;
    }

  let emit t s = t.out <- s :: t.out

  let note_skip t r =
    if not (List.mem r t.unit_skips) then t.unit_skips <- r :: t.unit_skips

  let note_default_skip t r =
    if not (List.mem r t.default_skips) then
      t.default_skips <- r :: t.default_skips

  (* Unit-skip accessors used by the conformance runner. *)
  let reset_unit_skips t = t.unit_skips <- []

  let unit_skips t = List.rev t.default_skips @ List.rev t.unit_skips

  let build_default_newline t =
    match t.build_nl with
    | Some v -> v
    | None ->
        let v =
          match D.compile "" with
          | Ok c -> (D.info c).D.newline
          | Error _ -> Flags.newline_lf
        in
        t.build_nl <- Some v;
        v

  let chomp line =
    let n = String.length line in
    if n > 0 && line.[n - 1] = '\n' then String.sub line 0 (n - 1) else line

  (* ---------------- #commands (pcre2test.c:4997-5245) ---------------- *)

  let cmdlist =
    [
      "forbid_utf";
      "load";
      "loadtables";
      "newline_default";
      "pattern";
      "perltest";
      "pop";
      "popcopy";
      "save";
      "subject";
    ]

  let process_command t line =
    (* pcre2test.c:5012-5024 — case-sensitive prefix, then a space *)
    let matches name =
      let len = String.length name in
      let rec eq k =
        k >= len || (Cstr.at line (1 + k) = name.[k] && eq (k + 1))
      in
      eq 0 && Cstr.isspace (Cstr.at line (len + 1))
    in
    let cmd = List.find_opt matches cmdlist in
    (* pcre2test.c:5025-5029: the perltest restriction fires before the
       switch; for unknown commands the C's cmdname is the last list entry. *)
    if
      t.restrict_perl
      && not (match cmd with Some ("pattern" | "subject") -> true | _ -> false)
    then begin
      emit t
        (Printf.sprintf "** #%s is not allowed after #perltest"
           (match cmd with Some c -> c | None -> "subject"));
      emit t "** pcre2test run abandoned";
      t.aborted <- true
    end
    else
      match cmd with
      | None -> emit t ("** Unknown command: " ^ chomp line)
      | Some "forbid_utf" ->
          t.forbid_utf <- Flags.never_utf lor Flags.never_ucp
      | Some "perltest" -> t.restrict_perl <- true
      | Some "pattern" ->
          (* argptr = buffer + cmdlen + 1 (pcre2test.c:5024) *)
          let from = min (String.length line) (1 + String.length "pattern") in
          let args = String.sub line from (String.length line - from) in
          ignore
            (Modifiers.decode ~ctx:Modifiers.CtxDefpat ~pctl:(Some t.def_pat)
               ~dctl:None ~restrict_perl:t.restrict_perl ~emit:(emit t)
               ~skip:(note_default_skip t) args)
      | Some "subject" ->
          let from = min (String.length line) (1 + String.length "subject") in
          let args = String.sub line from (String.length line - from) in
          ignore
            (Modifiers.decode ~ctx:Modifiers.CtxDefdat ~pctl:None
               ~dctl:(Some t.def_dat) ~restrict_perl:t.restrict_perl
               ~emit:(emit t) ~skip:(note_default_skip t) args)
      | Some "newline_default" ->
          (* pcre2test.c:5062-5083 *)
          t.local_newline_default <- 0;
          let first_listed = ref 0 in
          let argp = ref (String.length "newline_default" + 1) in
          let default_ok = ref false in
          let fin = ref false in
          while not !fin do
            argp := Cstr.skip_space line !argp;
            if Cstr.at line !argp = '\000' then fin := true
            else begin
              for i = 1 to Array.length Flags.newline_names - 1 do
                let nm = Flags.newline_names.(i) in
                let nlen = String.length nm in
                if
                  Cstr.strncmpic line !argp nm nlen
                  && Cstr.isspace (Cstr.at line (!argp + nlen))
                then begin
                  if i = build_default_newline t then default_ok := true;
                  if !first_listed = 0 then first_listed := i
                end
              done;
              while
                Cstr.at line !argp <> '\000'
                && not (Cstr.isspace (Cstr.at line !argp))
              do
                incr argp
              done
            end
          done;
          if not !default_ok then t.local_newline_default <- !first_listed
      | Some other ->
          (* #pop/#popcopy/#save/#load/#loadtables — serialization stack is
             out of scope for the harness *)
          note_skip t ("command:" ^ other)

  (* ------------- pattern lines (pcre2test.c:5271-5957) ------------- *)

  (* Detect \P, \p, \X escapes for the forbid_utf check. The C tests the
     compiled pattern's PCRE2_HASBKPORX flag (pcre2test.c:6094-6104), which
     the driver does not expose; scan the pattern text the way the lexer
     would see it. *)
  let has_bkporx s =
    let n = String.length s in
    let rec skip_q i =
      (* inside \Q ... \E *)
      if i >= n - 1 then n
      else if s.[i] = '\\' && s.[i + 1] = 'E' then i + 2
      else skip_q (i + 1)
    in
    let rec go i =
      if i >= n - 1 then false
      else if s.[i] <> '\\' then go (i + 1)
      else
        match s.[i + 1] with
        | 'P' | 'p' | 'X' -> true
        | 'c' -> go (i + 3) (* \c consumes the following character *)
        | 'Q' -> go (skip_q (i + 2))
        | _ -> go (i + 2)
    in
    go 0

  (* pcre2test.c:5389-5451 — hex pattern decoding *)
  let hex_decode t src =
    let n = String.length src in
    let at i = Cstr.at ~limit:n src i in
    let out = Buffer.create n in
    let pp = ref 0 in
    let ok = ref true in
    while !ok && at !pp <> '\000' do
      if Cstr.isspace (at !pp) then incr pp
      else begin
        let c = at !pp in
        incr pp;
        if c = '\'' || c = '"' then begin
          (* literal substring *)
          let pq = !pp in
          let fin = ref false in
          while not !fin do
            let d = at !pp in
            if d = '\000' then begin
              emit t
                (Printf.sprintf
                   "** Missing closing quote in hex pattern: opening quote is \
                    at offset %d."
                   (pq - 1));
              ok := false;
              fin := true
            end
            else if d = c then begin
              incr pp;
              fin := true
            end
            else begin
              Buffer.add_char out d;
              incr pp
            end
          done
        end
        else if not (Cstr.isxdigit c) then begin
          emit t
            (Printf.sprintf
               "** Unexpected non-hex-digit '%c' at offset %d in hex pattern: \
                quote missing?"
               c (!pp - 1));
          ok := false
        end
        else if at !pp = '\000' then begin
          emit t "** Odd number of digits in hex pattern";
          ok := false
        end
        else begin
          let d = at !pp in
          if not (Cstr.isxdigit d) then begin
            emit t
              (Printf.sprintf
                 "** Unexpected non-hex-digit '%c' at offset %d in hex \
                  pattern: quote missing?"
                 d !pp);
            ok := false
          end
          else begin
            Buffer.add_char out
              (Char.chr ((Cstr.hexval c * 16) + Cstr.hexval d));
            incr pp
          end
        end
      end
    done;
    if !ok then Some (Buffer.contents out) else None

  (* pcre2test.c:5455-5517 — \[...]{n} pattern expansion *)
  let expand_pattern t src =
    let n = String.length src in
    let at i = Cstr.at ~limit:n src i in
    let out = Buffer.create n in
    let pp = ref 0 in
    let ok = ref true in
    while !ok && at !pp <> '\000' do
      let pc = ref !pp in
      let count = ref 1 in
      let length = ref 1 in
      if at !pp = '\\' && at (!pp + 1) = '[' then begin
        let pe = ref (!pp + 2) in
        (try
           while at !pe <> '\000' do
             if at !pe = ']' && at (!pe + 1) = '{' then begin
               let clen = !pe - !pc - 2 in
               let pe2 = !pe + 2 in
               match Modifiers.parse_u32 at pe2 with
               | None ->
                   emit t "** Pattern repeat count too large";
                   ok := false;
                   raise Exit
               | Some (i, j) ->
                   if at j = '}' then begin
                     if i = 0 then begin
                       emit t "** Zero repeat not allowed";
                       ok := false;
                       raise Exit
                     end;
                     pc := !pc + 2;
                     count := i;
                     length := clen;
                     pp := j;
                     raise Exit
                   end
                   else pe := j + 1 (* C continues scanning from endptr *)
             end
             else incr pe
           done
         with Exit -> ())
      end;
      if !ok then begin
        let chunk =
          if !pc + !length <= n then String.sub src !pc !length else ""
        in
        for _k = 1 to !count do
          Buffer.add_string out chunk
        done;
        incr pp
      end
    done;
    if !ok then Some (Buffer.contents out) else None

  let finish_pattern t buf d =
    let pat = Ctl.copy_patctl t.def_pat in
    let n = String.length buf in
    let raw, mods_start =
      (* pcre2test.c:5318-5326 — a backslash straight after the closing
         delimiter makes the pattern end with a backslash *)
      if Cstr.at buf (d + 1) = '\\' then (String.sub buf 1 (d - 1) ^ "\\", d + 2)
      else (String.sub buf 1 (d - 1), d + 1)
    in
    let mods = String.sub buf mods_start (n - mods_start) in
    if
      not
        (Modifiers.decode ~ctx:Modifiers.CtxPat ~pctl:(Some pat) ~dctl:None
           ~restrict_perl:t.restrict_perl ~emit:(emit t) ~skip:(note_skip t)
           mods)
    then t.skipping <- true
    else if pat.Ctl.control land Ctl.ctl_utf8_input <> 0 then begin
      emit t "** The utf8_input modifier is not allowed in 8-bit mode";
      t.skipping <- true
    end
    else begin
      (* Mutually exclusive pattern controls we track
         (pcre2test.c:832-839, 5366-5376): expand vs hex. *)
      let excl = pat.Ctl.control land (Ctl.ctl_expand lor Ctl.ctl_hexpat) in
      if excl <> 0 && excl <> excl land (-excl) then begin
        emit t (Ctl.show_controls ~control:excl ~control2:0 "** Not allowed together:");
        t.skipping <- true
      end
      else
        let text =
          if pat.Ctl.control land Ctl.ctl_hexpat <> 0 then hex_decode t raw
          else if pat.Ctl.control land Ctl.ctl_expand <> 0 then
            expand_pattern t raw
          else Some raw
        in
        match text with
        | None -> t.skipping <- true
        | Some text -> (
            (* pcre2test.c:5949-5956 — newline default interaction *)
            let nl_ctx =
              if pat.Ctl.control2 land Ctl.ctl2_nl_set <> 0 then
                pat.Ctl.ctx_newline
              else if t.local_newline_default <> 0 then
                t.local_newline_default
              else 0
            in
            (* pcre2test.c:5967-5970 — PCRE2_LITERAL disables forbid_utf *)
            let use_forbid_utf =
              if pat.Ctl.options land Flags.literal <> 0 then 0
              else t.forbid_utf
            in
            match
              D.compile
                ~options:(pat.Ctl.options lor use_forbid_utf)
                ~newline:nl_ctx ~bsr:pat.Ctl.ctx_bsr ~extra:pat.Ctl.ctx_extra
                text
            with
            | Error { D.errcode; erroroffset } ->
                (* pcre2test.c:6084-6090 *)
                emit t
                  (Printf.sprintf "Failed: error %d at offset %d: %s" errcode
                     erroroffset (D.error_message errcode));
                t.skipping <- true
            | Ok code ->
                let inf = D.info code in
                t.compiled <-
                  Some
                    {
                      code;
                      pat;
                      utf = inf.D.alloptions land Flags.utf <> 0;
                      nl = inf.D.newline;
                      maxcapcount = inf.D.capture_count;
                    };
                (* pcre2test.c:6094-6104 *)
                if t.forbid_utf <> 0 && has_bkporx text then begin
                  emit t
                    "** \\P, \\p, and \\X are not allowed after the \
                     #forbid_utf command";
                  t.skipping <- true
                end)
    end

  let start_pattern t line =
    if t.restrict_perl && line.[0] <> '/' then begin
      (* pcre2test.c:5287-5292 *)
      emit t "** The only allowed delimiter after #perltest is '/'";
      emit t "** pcre2test run abandoned";
      t.aborted <- true
    end
    else begin
      let delim = line.[0] in
      match Scan.find_close ~delim line with
      | Some d -> finish_pattern t line d
      | None ->
          let buf = Buffer.create 128 in
          Buffer.add_string buf line;
          t.pending <- Some (delim, buf)
    end

  let continue_pattern t delim buf line =
    Buffer.add_string buf line;
    let s = Buffer.contents buf in
    match Scan.find_close ~delim s with
    | Some d ->
        t.pending <- None;
        finish_pattern t s d
    | None -> ()

  (* ------------- data lines (pcre2test.c:6860-8449) ------------- *)

  (* Substring extraction semantics, ported from pcre2_substring.c for use by
     the copy/get modifiers. [md_rc] is the emulated match-data rc. *)
  let substring_length_bynumber ~md_rc ~oveccount ~top_bracket ~(ov : int array)
      ~ulen n =
    (* pcre2_substring.c:316-357 *)
    if md_rc = Flags.error_partial && n > 0 then Error Flags.error_partial
    else if md_rc < 0 && md_rc <> Flags.error_partial then Error md_rc
    else if n > top_bracket then Error Flags.error_nosubstring
    else if n >= oveccount then Error Flags.error_unavailable
    else if 2 * n >= Array.length ov || ov.(2 * n) = -1 then
      Error Flags.error_unset
    else
      let left = ov.(2 * n) and right = ov.((2 * n) + 1) in
      if left > ulen || right > ulen then Error (-35) (* INVALIDOFFSET *)
      else Ok (if left > right then 0 else right - left)

  let nametable_matches nt name =
    Array.to_list nt |> List.filter (fun (nm, _) -> String.equal nm name)

  (* pcre2_substring.c:544-560 / 434-487 *)
  let number_from_name nt name =
    match nametable_matches nt name with
    | [] -> Flags.error_nosubstring
    | [ (_, n) ] -> n
    | _ -> Flags.error_nouniquesubstring

  (* pcre2test.c:6612-6820 copy_and_get *)
  let copy_and_get t ~(dat : Ctl.datctl) ~utf ~md_rc ~oveccount ~top_bracket
      ~(ov : int array) ~subj ~ulen ~nt ~capcount =
    let errmsg code = D.error_message code in
    let text_of n len = Pchars.pchars_str ~utf subj ov.(2 * n) len in
    (* copy by number *)
    List.iter
      (fun n ->
        match
          substring_length_bynumber ~md_rc ~oveccount ~top_bracket ~ov ~ulen n
        with
        | Error rc ->
            emit t
              (Printf.sprintf "Copy substring %d failed (%d): %s" n rc
                 (errmsg rc))
        | Ok len ->
            if len + 1 > 256 then
              (* copybuffer[256] in pcre2test.c:6621 *)
              emit t
                (Printf.sprintf "Copy substring %d failed (%d): %s" n
                   Flags.error_nomemory
                   (errmsg Flags.error_nomemory))
            else
              emit t (Printf.sprintf "%2dC %s (%d)" n (text_of n len) len))
      dat.Ctl.copy_numbers;
    (* copy by name *)
    List.iter
      (fun name ->
        let groupnumber = number_from_name nt name in
        if groupnumber < 0 && groupnumber <> Flags.error_nouniquesubstring then
          emit t (Printf.sprintf "Number not found for group '%s'" name);
        (* pcre2_substring.c:73-95 copy_byname *)
        let result =
          match nametable_matches nt name with
          | [] -> Error Flags.error_nosubstring
          | entries ->
              let rec try_entries failrc = function
                | [] -> Error failrc
                | (_, n) :: rest ->
                    if n < oveccount then
                      if 2 * n < Array.length ov && ov.(2 * n) <> -1 then
                        match
                          substring_length_bynumber ~md_rc ~oveccount
                            ~top_bracket ~ov ~ulen n
                        with
                        | Error rc -> Error rc
                        | Ok len -> Ok (n, len)
                      else try_entries Flags.error_unset rest
                    else try_entries failrc rest
              in
              try_entries Flags.error_unavailable entries
        in
        match result with
        | Error rc ->
            emit t
              (Printf.sprintf "Copy substring '%s' failed (%d): %s" name rc
                 (errmsg rc))
        | Ok (n, len) ->
            let suffix =
              if groupnumber >= 0 then Printf.sprintf " (group %d)" groupnumber
              else " (non-unique)"
            in
            emit t
              (Printf.sprintf "  C %s (%d) %s%s" (text_of n len) len name
                 suffix))
      dat.Ctl.copy_names;
    (* get by number *)
    List.iter
      (fun n ->
        match
          substring_length_bynumber ~md_rc ~oveccount ~top_bracket ~ov ~ulen n
        with
        | Error rc ->
            emit t
              (Printf.sprintf "Get substring %d failed (%d): %s" n rc
                 (errmsg rc))
        | Ok len -> emit t (Printf.sprintf "%2dG %s (%d)" n (text_of n len) len))
      dat.Ctl.get_numbers;
    (* get by name *)
    List.iter
      (fun name ->
        let groupnumber = number_from_name nt name in
        if groupnumber < 0 && groupnumber <> Flags.error_nouniquesubstring then
          emit t (Printf.sprintf "Number not found for group '%s'" name);
        let result =
          match nametable_matches nt name with
          | [] -> Error Flags.error_nosubstring
          | entries ->
              let rec try_entries failrc = function
                | [] -> Error failrc
                | (_, n) :: rest ->
                    if n < oveccount then
                      if 2 * n < Array.length ov && ov.(2 * n) <> -1 then
                        match
                          substring_length_bynumber ~md_rc ~oveccount
                            ~top_bracket ~ov ~ulen n
                        with
                        | Error rc -> Error rc
                        | Ok len -> Ok (n, len)
                      else try_entries Flags.error_unset rest
                    else try_entries failrc rest
              in
              try_entries Flags.error_unavailable entries
        in
        match result with
        | Error rc ->
            emit t
              (Printf.sprintf "Get substring '%s' failed (%d): %s" name rc
                 (errmsg rc))
        | Ok (n, len) ->
            let suffix =
              if groupnumber >= 0 then Printf.sprintf " (group %d)" groupnumber
              else " (non-unique)"
            in
            emit t
              (Printf.sprintf "  G %s (%d) %s%s" (text_of n len) len name
                 suffix))
      dat.Ctl.get_names;
    (* getall — pcre2test.c:6787-6812 with pcre2_substring_list_get *)
    if dat.Ctl.d_control land Ctl.ctl_getall <> 0 then begin
      if md_rc < 0 then
        emit t
          (Printf.sprintf "get substring list failed (%d): %s" md_rc
             (errmsg md_rc))
      else begin
        let count = if md_rc = 0 then oveccount else md_rc in
        let show = min capcount count in
        for i = 0 to show - 1 do
          let txt =
            if 2 * i < Array.length ov && ov.(2 * i) <> -1 then
              let left = ov.(2 * i) and right = ov.((2 * i) + 1) in
              let len = if left > right then 0 else right - left in
              Pchars.pchars_str ~utf subj left len
            else ""
          in
          emit t (Printf.sprintf "%2dL %s" i txt)
        done
      end
    end

  (* The global matching loop — pcre2test.c:7761-8443. *)
  let run_match t (cp : compiled) (dat : Ctl.datctl) subject0 =
    let utf = cp.utf in
    let nt = lazy (D.name_table cp.code) in
    let emu_ovec =
      if dat.Ctl.oveccount = 0 then cp.maxcapcount + 1 else dat.Ctl.oveccount
    in
    let subj = ref subject0 in
    let ulen = ref (String.length subject0) in
    let offset = ref dat.Ctl.offset in
    let g_notempty = ref 0 in
    (* ovecsave — pcre2test.c:7764 (PCRE2_UNSET ~ -1 at the driver seam) *)
    let ovecsave = [| -1; -1; -1 |] in
    let gmatched = ref 0 in
    let loop = ref true in
    while !loop do
      let res =
        D.exec
          ~options:(dat.Ctl.d_options lor !g_notempty)
          ~subject:!subj ~offset:!offset cp.code
      in
      let ov = res.D.ovector in
      let dpairs = Array.length ov / 2 in
      (* Emulate pcre2test's finite match-data ovector (default 15 pairs):
         pcre2_match returns 0 when there were too many substrings. *)
      let rc = if res.D.rc > emu_ovec then 0 else res.D.rc in
      (* advance decision carried to the loop bottom: None = break *)
      let advance = ref None in
      if rc >= 0 then begin
        let capcount = ref (if rc = 0 then emu_ovec else rc) in
        if rc = 0 then emit t "Matched, but too many substrings";
        (* pcre2test.c:7958-7980 — repeat-detection for global loops *)
        let skip_print = ref false in
        if !gmatched > 0 && ovecsave.(0) = ov.(0) && ovecsave.(1) = ov.(1)
        then begin
          if ov.(0) = ov.(1) && ovecsave.(2) <> !offset then begin
            g_notempty := Flags.notempty_atstart lor Flags.anchored;
            ovecsave.(2) <- !offset;
            skip_print := true (* continue: back to the top of the loop *)
          end
          else begin
            emit t
              "** PCRE2 error: global repeat returned the same string as \
               previous";
            emit t "** Global loop abandoned";
            dat.Ctl.d_control <- dat.Ctl.d_control land lnot Ctl.ctl_anyglob
          end
        end;
        if !skip_print then advance := Some `Continue
        else begin
          (* pcre2test.c:7986-7991 allcaptures *)
          if dat.Ctl.d_control land Ctl.ctl_allcaptures <> 0 then begin
            capcount := cp.maxcapcount + 1;
            if !capcount > emu_ovec then capcount := emu_ovec
          end;
          (* pcre2test.c:7999-8126 — output captured substrings *)
          for i = 0 to !capcount - 1 do
            let st0 = if 2 * i < 2 * dpairs then ov.(2 * i) else -1 in
            let en0 = if 2 * i < 2 * dpairs then ov.((2 * i) + 1) else -1 in
            let st, en =
              if st0 <> -1 && en0 <> -1 && st0 > en0 then begin
                emit t
                  "Start of matched string is beyond its end - displaying \
                   from end to start.";
                (en0, st0)
              end
              else (st0, en0)
            in
            let prefix = Printf.sprintf "%2d: " i in
            if st = -1 && en = -1 then emit t (prefix ^ "<unset>")
            else begin
              let line = Buffer.create 64 in
              Buffer.add_string line prefix;
              let caret = ref 0 in
              (if i = 0 && dat.Ctl.d_control land Ctl.ctl_startchar <> 0 then begin
                 (* pcre2test.c:8079-8094 startchar display *)
                 let sc = res.D.startchar in
                 let lstr, lcnt = Pchars.pchars ~utf !subj sc (st - sc) in
                 Buffer.add_string line lstr;
                 Buffer.add_string line
                   (Pchars.pchars_str ~utf !subj st (en - st));
                 if sc <> st then caret := lcnt
               end
               else
                 Buffer.add_string line
                   (Pchars.pchars_str ~utf !subj st (en - st)));
              emit t (Buffer.contents line);
              if !caret > 0 then
                emit t ("    " ^ String.make !caret '^');
              (* pcre2test.c:8117-8125 aftertext *)
              if
                dat.Ctl.d_control land Ctl.ctl_allaftertext <> 0
                || (i = 0 && dat.Ctl.d_control land Ctl.ctl_aftertext <> 0)
              then
                emit t
                  (Printf.sprintf "%2d+ %s" i
                     (Pchars.pchars_str ~utf !subj en0 (!ulen - en0)))
            end
          done;
          (* pcre2test.c:8128-8136 MK: *)
          (if dat.Ctl.d_control land Ctl.ctl_mark <> 0 then
             match res.D.mark with
             | Some m -> emit t ("MK: " ^ Pchars.mark_str ~utf m)
             | None -> ());
          (* copy/get *)
          copy_and_get t ~dat ~utf ~md_rc:rc ~oveccount:emu_ovec
            ~top_bracket:cp.maxcapcount ~ov ~subj:!subj ~ulen:!ulen
            ~nt:(Lazy.force nt) ~capcount:!capcount;
          advance := Some (`Match (ov.(0), ov.(1), res.D.startchar))
        end
      end
      else if rc = Flags.error_partial then begin
        (* pcre2test.c:8143-8198 *)
        let line = Buffer.create 64 in
        Buffer.add_string line "Partial match";
        (if dat.Ctl.d_control land Ctl.ctl_mark <> 0 then
           match res.D.mark with
           | Some m ->
               Buffer.add_string line ", mark=";
               Buffer.add_string line (Pchars.mark_str ~utf m)
           | None -> ());
        Buffer.add_string line ": ";
        (* leftchar = ovector[0] without allusedtext -> no back rubric *)
        Buffer.add_string line
          (Pchars.pchars_str ~utf !subj ov.(0) (ov.(1) - ov.(0)));
        emit t (Buffer.contents line);
        if !ulen <> ov.(1) then
          emit t
            (Printf.sprintf
               "** ovector[1] is not equal to the subject length: %d != %d"
               ov.(1) !ulen);
        copy_and_get t ~dat ~utf ~md_rc:Flags.error_partial ~oveccount:emu_ovec
          ~top_bracket:cp.maxcapcount ~ov ~subj:!subj ~ulen:!ulen
          ~nt:(Lazy.force nt) ~capcount:1;
        advance := None
      end
      else if !g_notempty <> 0 then begin
        (* pcre2test.c:8200-8241 — failed retry after a null match: fake a
           one-character match, CRLF- and UTF-aware *)
        let start_offset = !offset in
        let end_offset = ref (start_offset + 1) in
        let nl = cp.nl in
        if
          (nl = Flags.newline_crlf || nl = Flags.newline_any
          || nl = Flags.newline_anycrlf)
          && start_offset < !ulen - 1
          && Cstr.at !subj start_offset = '\r'
          && Cstr.at !subj !end_offset = '\n'
        then incr end_offset
        else if utf then
          while
            !end_offset < !ulen
            && Char.code !subj.[!end_offset] land 0xc0 = 0x80
          do
            incr end_offset
          done;
        advance := Some (`Match (start_offset, !end_offset, res.D.startchar))
      end
      else begin
        (* pcre2test.c:8246-8291 — normal match failure *)
        (if rc = Flags.error_nomatch then begin
           if !gmatched = 0 then begin
             let line = Buffer.create 32 in
             Buffer.add_string line "No match";
             (if dat.Ctl.d_control land Ctl.ctl_mark <> 0 then
                match res.D.mark with
                | Some m ->
                    Buffer.add_string line ", mark = ";
                    Buffer.add_string line (Pchars.mark_str ~utf m)
                | None -> ());
             emit t (Buffer.contents line)
           end
         end
         else if rc = Flags.error_badutfoffset then
           emit t (Printf.sprintf "Error %d (bad UTF-8 offset)" rc)
         else begin
           let line = Buffer.create 64 in
           Buffer.add_string line
             (Printf.sprintf "Failed: error %d: %s" rc (D.error_message rc));
           if rc <= Flags.error_utf8_err1 && rc >= Flags.error_utf32_err2 then
             Buffer.add_string line
               (Printf.sprintf " at offset %d" res.D.startchar);
           emit t (Buffer.contents line)
         end);
        advance := None
      end;
      (* pcre2test.c:8293-8443 — bottom of the global loop *)
      (match !advance with
      | None -> loop := false
      | Some `Continue -> incr gmatched
      | Some (`Match (mo, eo_orig, startchar)) ->
          if dat.Ctl.d_control land Ctl.ctl_anyglob = 0 then loop := false
          else begin
            let eo = ref eo_orig in
            let brk = ref false in
            if mo = !eo then begin
              if !eo = !ulen then brk := true
              else if mo <= !offset then
                g_notempty := Flags.notempty_atstart lor Flags.anchored
            end
            else begin
              g_notempty := 0;
              if dat.Ctl.d_control land Ctl.ctl_global <> 0 then
                if !eo <= startchar then begin
                  if startchar >= !ulen then brk := true
                  else begin
                    eo := startchar + 1;
                    if utf then
                      while
                        !eo < !ulen
                        && Char.code !subj.[!eo] land 0xc0 = 0x80
                      do
                        incr eo
                      done
                  end
                end
            end;
            if !brk then loop := false
            else begin
              if dat.Ctl.d_control land Ctl.ctl_global <> 0 then begin
                ovecsave.(0) <- mo;
                ovecsave.(1) <- eo_orig;
                ovecsave.(2) <- !offset;
                offset := !eo
              end
              else begin
                (* altglobal: advance the subject pointer *)
                subj := String.sub !subj !eo (!ulen - !eo);
                ulen := !ulen - !eo
              end;
              incr gmatched
            end
          end)
    done

  let process_data t line =
    match t.compiled with
    | None -> ()
    | Some cp ->
        (* pcre2test.c:6893-6905 — data controls inherit from the pattern *)
        let dat = Ctl.copy_datctl t.def_dat in
        dat.Ctl.d_control <-
          dat.Ctl.d_control lor (cp.pat.Ctl.control land Ctl.ctl_allpd);
        dat.Ctl.d_control2 <-
          dat.Ctl.d_control2 lor (cp.pat.Ctl.control2 land Ctl.ctl2_allpd);
        let utf = cp.utf in
        let subject_literal =
          cp.pat.Ctl.control2 land Ctl.ctl2_subject_literal <> 0
        in
        let len = Cstr.rstrip_len line in
        let start = Cstr.skip_space ~limit:len line 0 in
        (* pcre2test.c:6925-6940 — UTF-8 validity pre-check *)
        let valid =
          if not utf then true
          else begin
            let ok = ref true in
            let i = ref start in
            while !ok && !i < len do
              let rc, _ = Pchars.utf82ord ~limit:len line !i in
              if rc <= 0 then ok := false else i := !i + rc
            done;
            if not !ok then
              emit t
                "** Failed: invalid UTF-8 string cannot be used as input in \
                 UTF mode";
            !ok
          end
        in
        if valid then
          match
            Subject.decode ~utf ~subject_literal ~emit:(emit t) line ~start
              ~len
          with
          | None -> ()
          | Some { Subject.subject; mods_from } ->
              let mods_ok =
                match mods_from with
                | None -> true
                | Some idx ->
                    Modifiers.decode ~ctx:Modifiers.CtxDat ~pctl:None
                      ~dctl:(Some dat) ~restrict_perl:t.restrict_perl
                      ~emit:(emit t) ~skip:(note_skip t)
                      (String.sub line idx (len - idx))
              in
              if mods_ok then run_match t cp dat subject

  (* ---------------- top-level line dispatch ---------------- *)

  (* Feed one raw input line (including its newline, when present). Returns
     the harness output for that line: the echoed line first, then any
     result lines. *)
  let process_line t raw =
    t.out <- [];
    if t.aborted then []
    else begin
      emit t (chomp raw);
      (match t.pending with
      | Some (delim, buf) -> continue_pattern t delim buf raw
      | None ->
          let p = Cstr.skip_space raw 0 in
          let expectdata = t.compiled <> None in
          if expectdata || t.skipping then begin
            if Cstr.at raw p = '\000' then begin
              (* blank line terminates the test (pcre2test.c:9549-9568) *)
              t.compiled <- None;
              t.skipping <- false
            end
            else if (not t.skipping) && not (Scan.is_data_comment raw) then
              process_data t raw
          end
          else if Cstr.at raw 0 = '#' then begin
            if
              Cstr.isspace (Cstr.at raw 1)
              || Cstr.at raw 1 = '!'
              || Cstr.at raw 1 = '\000'
            then () (* comment *)
            else process_command t raw
          end
          else if Scan.is_delimiter (Cstr.at raw 0) then start_pattern t raw
          else if Cstr.at raw p <> '\000' then begin
            (* pcre2test.c:9598-9605 *)
            emit t
              (Printf.sprintf "** Invalid pattern delimiter '%c' (x%x)."
                 raw.[0] (Char.code raw.[0]));
            t.skipping <- true
          end);
      List.rev t.out
    end

  (* Signal end of input; reports an unterminated pattern like the C. *)
  let finish t =
    t.out <- [];
    if t.aborted then []
    else begin
      (match t.pending with
      | Some _ ->
          t.pending <- None;
          emit t "** Unexpected EOF";
          emit t "** pcre2test run abandoned";
          t.aborted <- true
      | None -> ());
      List.rev t.out
    end
end
