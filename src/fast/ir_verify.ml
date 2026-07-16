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
              let step =
                if Int.equal t Ir.t_alt || Int.equal t Ir.t_jmp then (
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
                else if Int.equal t Ir.t_cap_start || Int.equal t Ir.t_cap_end
                then (
                  (* ovbase = 2N, 1 <= N <= top_bracket. *)
                  let ovb = code.(pc + 1) in
                  if ovb < 2 || not (Int.equal (ovb land 1) 0)
                     || ovb / 2 > top_bracket
                  then
                    Error
                      (Printf.sprintf
                         "fast-verify: %s at pc %d ovbase %d out of range (top \
                          bracket %d)"
                         Ir.tag_name.(t) pc ovb top_bracket)
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
                else Ok ()
              in
              match step with
              | Error _ as e -> e
              | Ok () -> (check_ops [@tailcall]) (pc + Ir.arity.(t))
          in
          check_ops 0
