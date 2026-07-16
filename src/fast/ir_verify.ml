(* Fast-engine IR static verifier (M11 chunk C1, fast-design.md §2). Since
   the fast engine has NO runtime fallback (port-conventions.md §9), this is
   the guarantee that accepted IR cannot reach an unhandled instruction
   mid-match. [check] proves, for an [Ir.t]:

   - every instruction head carries a known tag (0..Ir.max_tag) and its
     operands fit inside [code] (operand counts / bounds consistent);
   - the program ends with exactly one trailing [t_end];
   - every jump target ([t_alt] handler, [t_jmp] target) is in range AND
     lands on an instruction head (never mid-instruction);
   - every [t_char_run] literal-pool reference is in-bounds (0 <= off,
     off + len <= |lit|, len >= 1);
   - ALT save-record metadata is consistent (the handler is a valid head;
     the record width is the fixed [Ir.arity.(t_alt)]).

   Runs after every compile in tests; a cheap subset is always-on in
   Fast.compile once the runner (chunk C2) exists. Pure walk, stdlib only,
   no polymorphic compare (port-conventions §7). *)

let check (ir : Ir.t) : (unit, string) result =
  let code = ir.Ir.code in
  let len = Array.length code in
  let litlen = String.length ir.Ir.lit in
  let top_bracket = ir.Ir.re.Pcre2_engine.Compile.top_bracket in
  let codelen = Bytes.length ir.Ir.re.Pcre2_engine.Compile.code in
  let n_groups = ir.Ir.n_groups in
  let nt_len = Bytes.length ir.Ir.re.Pcre2_engine.Compile.name_table in
  let name_entry_size = ir.Ir.re.Pcre2_engine.Compile.name_entry_size in
  (* An ovbase names a capture pair 2N (fast public convention), 1 <= N <=
     top_bracket, so it indexes the ovector / cap_start (sized 2*(top+1)). *)
  let ovbase_ok (ovb : int) : bool =
    ovb >= 2 && Int.equal (ovb land 1) 0 && ovb / 2 <= top_bracket
  in
  (* A class bitmap reference (t_class / t_class_rep) must name a 32-byte map
     wholly inside re.code. A character-type operand must be a supported
     single-type opcode (not \p/\P/\X — those decline at compile). *)
  let map_off_ok (off : int) : bool = off >= 0 && off + 32 <= codelen in
  let type_op_ok (op : int) : bool =
    (op >= Pcre2_engine.Opcodes.op_not_digit
    && op <= Pcre2_engine.Opcodes.op_anybyte)
    || (op >= Pcre2_engine.Opcodes.op_anynl
       && op <= Pcre2_engine.Opcodes.op_vspace)
  in
  if Int.equal len 0 then Error "fast-verify: empty IR (no END)"
  else
    (* Pass 1: linear head walk — validate tags + operand widths, record
       instruction heads. Returns the last head reached, or an error. *)
    let is_head = Array.make len false in
    let rec walk (pc : int) (last : int) : (int, string) result =
      if Int.equal pc len then Ok last
      else
        let t = code.(pc) in
        if t < 0 || t > Ir.max_tag then
          Error (Printf.sprintf "fast-verify: unknown tag %d at pc %d" t pc)
        else
          let w = Ir.arity.(t) in
          if pc + w > len then
            Error
              (Printf.sprintf
                 "fast-verify: instruction %s at pc %d overruns code array"
                 Ir.tag_name.(t) pc)
          else if Int.equal t Ir.t_end && not (Int.equal (pc + w) len) then
            (* "Exactly one trailing END" (fast-design.md §2): only the
               final head may be t_end. *)
            Error (Printf.sprintf "fast-verify: interior END at pc %d" pc)
          else (
            is_head.(pc) <- true;
            (walk [@tailcall]) (pc + w) pc)
    in
    match walk 0 (-1) with
    | Error _ as e -> e
    | Ok last ->
        if last < 0 || not (Int.equal code.(last) Ir.t_end) then
          Error "fast-verify: program does not end with END"
        else
          (* Pass 2: jump targets, literal-pool refs, ALT metadata. *)
          let rec check_ops (pc : int) : (unit, string) result =
            if pc >= len then Ok ()
            else
              let t = code.(pc) in
              (* Chunk D2: BRAZERO/BRAMINZERO carry a skip target (operand at
                 pc+1); KET_RMAX/KET_RMIN carry a loop-back [entry] (pc+1) —
                 all must be valid instruction heads, like ALT/JMP targets. *)
              let has_head_target =
                Int.equal t Ir.t_alt || Int.equal t Ir.t_jmp
                || Int.equal t Ir.t_brazero || Int.equal t Ir.t_braminzero
                || Int.equal t Ir.t_ket_rmax || Int.equal t Ir.t_ket_rmin
              in
              let step =
                if has_head_target then (
                  let tgt = code.(pc + 1) in
                  if tgt < 0 || tgt >= len then
                    Error
                      (Printf.sprintf
                         "fast-verify: %s at pc %d target %d out of range"
                         Ir.tag_name.(t) pc tgt)
                  else if not is_head.(tgt) then
                    Error
                      (Printf.sprintf
                         "fast-verify: %s at pc %d target %d not an \
                          instruction head"
                         Ir.tag_name.(t) pc tgt)
                  else if
                    (* KET_RMAX/KET_RMIN also carry a group id (pc+2): -1 (no
                       empty check) or a valid [0, n_groups) slot. *)
                    (Int.equal t Ir.t_ket_rmax || Int.equal t Ir.t_ket_rmin)
                    &&
                    let g = code.(pc + 2) in
                    g < Ir.no_group || g >= n_groups
                  then
                    Error
                      (Printf.sprintf
                         "fast-verify: %s at pc %d group id %d out of range \
                          (n_groups %d)"
                         Ir.tag_name.(t) pc code.(pc + 2) n_groups)
                  else Ok ())
                else if Int.equal t Ir.t_group_start then (
                  let g = code.(pc + 1) in
                  if g < 0 || g >= n_groups then
                    Error
                      (Printf.sprintf
                         "fast-verify: GROUP_START at pc %d group id %d out of \
                          range (n_groups %d)"
                         pc g n_groups)
                  else Ok ())
                else if Int.equal t Ir.t_char_run then (
                  let off = code.(pc + 1) and l = code.(pc + 2) in
                  if l < 1 || off < 0 || off + l > litlen then
                    Error
                      (Printf.sprintf
                         "fast-verify: CHAR_RUN at pc %d references lit \
                          [%d,%d) outside pool of %d"
                         pc off (off + l) litlen)
                  else Ok ())
                else if
                  Int.equal t Ir.t_cap_start || Int.equal t Ir.t_cap_end
                  || Int.equal t Ir.t_cap_start_ref
                  || Int.equal t Ir.t_cap_end_ref
                then (
                  (* ovbase = 2N, 1 <= N <= top_bracket. *)
                  let ovb = code.(pc + 1) in
                  if not (ovbase_ok ovb) then
                    Error
                      (Printf.sprintf
                         "fast-verify: %s at pc %d ovbase %d out of range (top \
                          bracket %d)"
                         Ir.tag_name.(t) pc ovb top_bracket)
                  else Ok ())
                else if Int.equal t Ir.t_ref then (
                  (* [ovbase; caseless]. *)
                  let ovb = code.(pc + 1) and ci = code.(pc + 2) in
                  if not (ovbase_ok ovb) then
                    Error
                      (Printf.sprintf
                         "fast-verify: REF at pc %d ovbase %d out of range (top \
                          bracket %d)"
                         pc ovb top_bracket)
                  else if not (Int.equal ci 0 || Int.equal ci 1) then
                    Error
                      (Printf.sprintf "fast-verify: REF at pc %d bad caseless %d"
                         pc ci)
                  else Ok ())
                else if Int.equal t Ir.t_ref_rep then (
                  (* [reptype; lmin; lmax; ovbase; caseless]. *)
                  let reptype = code.(pc + 1) in
                  let lmin = code.(pc + 2) and lmax = code.(pc + 3) in
                  let ovb = code.(pc + 4) and ci = code.(pc + 5) in
                  if reptype < 0 || reptype > 2 || lmin < 0 || lmax < lmin then
                    Error
                      (Printf.sprintf
                         "fast-verify: REF_REP at pc %d bad operands \
                          (reptype=%d min=%d max=%d)"
                         pc reptype lmin lmax)
                  else if not (ovbase_ok ovb) then
                    Error
                      (Printf.sprintf
                         "fast-verify: REF_REP at pc %d ovbase %d out of range"
                         pc ovb)
                  else if not (Int.equal ci 0 || Int.equal ci 1) then
                    Error
                      (Printf.sprintf
                         "fast-verify: REF_REP at pc %d bad caseless %d" pc ci)
                  else Ok ())
                else if Int.equal t Ir.t_dnref || Int.equal t Ir.t_dnref_rep then (
                  (* DNREF [slot_base; count; caseless]; DNREF_REP prefixes the
                     reptype triple. The run-time dnref_scan reads GET2 (2 safe
                     bytes) at slot_base + k*name_entry_size for k in
                     [0, count) — validate the widest access is inside the name
                     table so no read raises across the runner loop. *)
                  let rep = Int.equal t Ir.t_dnref_rep in
                  let base = if rep then pc + 4 else pc + 1 in
                  let slot_base = code.(base) in
                  let count = code.(base + 1) in
                  let ci = code.(base + 2) in
                  let rep_ok =
                    (not rep)
                    ||
                    let reptype = code.(pc + 1) in
                    let lmin = code.(pc + 2) and lmax = code.(pc + 3) in
                    reptype >= 0 && reptype <= 2 && lmin >= 0 && lmax >= lmin
                  in
                  let last_byte =
                    slot_base + ((count - 1) * name_entry_size) + 1
                  in
                  if not rep_ok then
                    Error
                      (Printf.sprintf
                         "fast-verify: DNREF_REP at pc %d bad repeat operands" pc)
                  else if count < 1 || slot_base < 0 || last_byte >= nt_len then
                    Error
                      (Printf.sprintf
                         "fast-verify: %s at pc %d name-table span [%d..%d) \
                          outside table of %d bytes (entry_size %d)"
                         Ir.tag_name.(t) pc slot_base (last_byte + 1) nt_len
                         name_entry_size)
                  else if not (Int.equal ci 0 || Int.equal ci 1) then
                    Error
                      (Printf.sprintf "fast-verify: %s at pc %d bad caseless %d"
                         Ir.tag_name.(t) pc ci)
                  else Ok ())
                else if
                  Int.equal t Ir.t_rep || Int.equal t Ir.t_repi
                  || Int.equal t Ir.t_notrep || Int.equal t Ir.t_notrepi
                then (
                  let reptype = code.(pc + 1) in
                  let lmin = code.(pc + 2) and lmax = code.(pc + 3) in
                  let c1 = code.(pc + 4) in
                  let two = Int.equal t Ir.t_repi || Int.equal t Ir.t_notrepi in
                  let c2ok = (not two) || (code.(pc + 5) >= 0 && code.(pc + 5) < 256) in
                  if reptype < 0 || reptype > 2 || lmin < 0 || lmax < lmin
                     || c1 < 0 || c1 > 255 || not c2ok
                  then
                    Error
                      (Printf.sprintf
                         "fast-verify: %s at pc %d has inconsistent operands \
                          (reptype=%d min=%d max=%d)"
                         Ir.tag_name.(t) pc reptype lmin lmax)
                  else Ok ())
                else if Int.equal t Ir.t_type then (
                  (* single character type: a supported type opcode. *)
                  let op = code.(pc + 1) in
                  if not (type_op_ok op) then
                    Error
                      (Printf.sprintf
                         "fast-verify: TYPE at pc %d has unsupported type \
                          opcode %d"
                         pc op)
                  else Ok ())
                else if Int.equal t Ir.t_wordbound then (
                  let w = code.(pc + 1) in
                  if not (Int.equal w 0 || Int.equal w 1) then
                    Error
                      (Printf.sprintf
                         "fast-verify: WORDBOUND at pc %d has bad want %d" pc w)
                  else Ok ())
                else if Int.equal t Ir.t_class then (
                  let off = code.(pc + 1) in
                  if not (map_off_ok off) then
                    Error
                      (Printf.sprintf
                         "fast-verify: CLASS at pc %d bitmap offset %d outside \
                          re.code (%d bytes)"
                         pc off codelen)
                  else Ok ())
                else if Int.equal t Ir.t_type_rep || Int.equal t Ir.t_class_rep
                then (
                  let reptype = code.(pc + 1) in
                  let lmin = code.(pc + 2) and lmax = code.(pc + 3) in
                  let payload = code.(pc + 4) in
                  let payload_ok =
                    if Int.equal t Ir.t_type_rep then type_op_ok payload
                    else map_off_ok payload
                  in
                  if reptype < 0 || reptype > 2 || lmin < 0 || lmax < lmin
                     || not payload_ok
                  then
                    Error
                      (Printf.sprintf
                         "fast-verify: %s at pc %d has inconsistent operands \
                          (reptype=%d min=%d max=%d payload=%d)"
                         Ir.tag_name.(t) pc reptype lmin lmax payload)
                  else Ok ())
                else Ok ()
              in
              match step with
              | Error _ as e -> e
              | Ok () -> (check_ops [@tailcall]) (pc + Ir.arity.(t))
          in
          check_ops 0
