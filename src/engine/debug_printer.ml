(* Debug printer for compiled patterns — pcre2_printint.c (PCRE2 10.44),
   8-bit code-unit width. pcre2test #includes that file (pcre2test.c:320)
   and calls pcre2_printint_8 for the bincode/fullbincode modifiers
   (pcre2test.c:4560-4564), printing the opening rule of dashes itself;
   everything from the first opcode line through the trailing rule is
   printed here. Matching that output byte-for-byte is the point: our
   dumps diff cleanly against `pcre2test -d` (fullbincode) listings.
   Printf in this module is sanctioned by port-conventions §7. *)

(* pcre2test.c:243-252 — PRINTABLE(c), non-EBCDIC branch: whether to print
   an output code unit as-is or as a hex value when showing compiled
   patterns (deliberately not isprint(), for locale-independent output). *)
let printable (c : int) : bool = c >= 32 && c < 127

(* pcre2_printint.c:91-184 — print_char: print one character from a
   string; in UTF mode the character may occupy more than one code unit.
   [ptr] indexes [bytes]; returns the number of ADDITIONAL code units
   used. Only the PCRE2_CODE_UNIT_WIDTH == 8 arms exist in this port. *)
let print_char (buf : Buffer.t) (bytes : Bytes.t) (ptr : int) (utf : bool) : int
    =
  let c = Char.code (Bytes.get bytes ptr) in
  (* pcre2_printint.c:95-109 — a valid single code unit at width 8 is
     c < 0x80 when UTF is requested. *)
  let one_code_unit = (not utf) || c < 0x80 in
  if one_code_unit then (
    (* pcre2_printint.c:114-120 *)
    if printable c then Buffer.add_char buf (Char.chr c)
    else if c < 0x80 then Printf.bprintf buf "\\x%02x" c
    else Printf.bprintf buf "\\x{%02x}" c;
    0)
  else if not (Int.equal (c land 0xc0) 0xc0) then (
    (* pcre2_printint.c:130-139 — malformed UTF-8 (sanity check turned
       off): don't swallow random bytes, print the invalid starting byte
       with \X instead of \x as an indication and stop. *)
    Printf.bprintf buf "\\X{%x}" c;
    0)
  else
    (* pcre2_printint.c:140-158 — accumulate a multi-byte UTF-8
       character; stop at an invalid secondary byte. *)
    let a = Tables.utf8_table4.(c land 0x3f) in
    let s = ref (6 * a) in
    let cval = ref ((c land Tables.utf8_table3.(a)) lsl !s) in
    let rec go (i : int) : int =
      if i > a then (
        Printf.bprintf buf "\\x{%x}" !cval;
        a)
      else
        let ci = Char.code (Bytes.get bytes (ptr + i)) in
        if not (Int.equal (ci land 0xc0) 0x80) then (
          Printf.bprintf buf "\\X{%x}" !cval (* invalid secondary byte *);
          i - 1)
        else (
          s := !s - 6;
          cval := !cval lor ((ci land 0x3f) lsl !s);
          (go [@tailcall]) (i + 1))
    in
    go 1

(* pcre2_printint.c:204-212 — print_custring: print a zero-terminated
   string as a list of code units (no account of UTF). *)
let print_custring (buf : Buffer.t) (bytes : Bytes.t) (ptr : int) : unit =
  let p = ref ptr in
  while not (Char.equal (Bytes.get bytes !p) '\000') do
    let c = Char.code (Bytes.get bytes !p) in
    incr p;
    if printable c then Buffer.add_char buf (Char.chr c)
    else Printf.bprintf buf "\\x{%x}" c
  done

(* pcre2_printint.c:214-222 — print_custring_bylen: same, length given. *)
let print_custring_bylen (buf : Buffer.t) (bytes : Bytes.t) (ptr : int)
    (len : int) : unit =
  for i = ptr to ptr + len - 1 do
    let c = Char.code (Bytes.get bytes i) in
    if printable c then Buffer.add_char buf (Char.chr c)
    else Printf.bprintf buf "\\x{%x}" c
  done

(* pcre2_printint.c:239-280 — get_ucpname: find a Unicode property name.
   The table contains both full names and their abbreviations, so this
   fiddles to get the full name: either the longer of (up to) two found
   names, or a 3-character script name. Ucptables.utt entries are
   (name, type, value) with the name string already resolved (the C
   indexes PRIV(utt_names) by offset). *)
let get_ucpname (ptype : int) (pvalue : int) : string =
  let yield = ref "??" in
  let len = ref 0 in
  let count = ref 0 in
  let ptypex =
    if Int.equal ptype Opcodes.pt_sc then Opcodes.pt_scx else ptype
  in
  (* The C's `break`s become a local exception (fine outside the
     interpreter frame loop, port-conventions §2/§6). *)
  (try
     for i = Ucptables.utt_size - 1 downto 0 do
       let name, utype, uvalue = Ucptables.utt.(i) in
       if
         (Int.equal ptype utype || Int.equal ptypex utype)
         && Int.equal pvalue uvalue
       then (
         let sl = String.length name in
         if
           Int.equal sl 3
           && (Int.equal utype Opcodes.pt_sc || Int.equal utype Opcodes.pt_scx)
         then (
           yield := name;
           raise Exit);
         if sl > !len then (
           yield := name;
           len := sl);
         incr count;
         if !count >= 2 then raise Exit)
     done
   with Exit -> ());
  !yield

(* pcre2_printint.c:301-317 — print_prop: print a Unicode property value.
   "Normal" properties come from the tables; PT_CLIST is a pseudo-property
   indexing PRIV(ucd_caseless_sets) (a NOTACHAR-terminated list of
   case-equivalent characters). [ptr] points at the OP_PROP/OP_NOTPROP
   opcode (or the type opcode copy after a TYPE repeat). *)
let print_prop (buf : Buffer.t) (bytes : Bytes.t) (ptr : int) (before : string)
    (after : string) : unit =
  let op = Char.code (Bytes.get bytes ptr) in
  let ptype = Char.code (Bytes.get bytes (ptr + 1)) in
  let pvalue = Char.code (Bytes.get bytes (ptr + 2)) in
  if not (Int.equal ptype Opcodes.pt_clist) then
    let sc = if Int.equal ptype Opcodes.pt_sc then "script:" else "" in
    let s = get_ucpname ptype pvalue in
    Printf.bprintf buf "%s%s %s%c%s%s" before Opcodes.op_names.(op) sc
      (Char.uppercase_ascii s.[0])
      (String.sub s 1 (String.length s - 1))
      after
  else
    let p = ref pvalue in
    Printf.bprintf buf "%s%sclist" before
      (if Int.equal op Opcodes.op_prop then "" else "not ");
    while Ucd_tables.ucd_caseless_sets.(!p) < Tables.notachar do
      Printf.bprintf buf " %04x" Ucd_tables.ucd_caseless_sets.(!p);
      incr p
    done;
    Buffer.add_string buf after

(* pcre2_printint.c:817-852 — the CLASS_REF_REPEAT label: handle repeats
   after a class or a back reference. [ccode] points at the possible
   repeat opcode; returns the amount added to `extra` (OP_lengths of the
   repeat, or 0 when it is not a repeat). *)
let class_ref_repeat (buf : Buffer.t) (bytes : Bytes.t) (ccode : int) : int =
  let rop = Char.code (Bytes.get bytes ccode) in
  match rop with
  | 98 (* OP_CRSTAR *)
  | 99 (* OP_CRMINSTAR *)
  | 100 (* OP_CRPLUS *)
  | 101 (* OP_CRMINPLUS *)
  | 102 (* OP_CRQUERY *)
  | 103 (* OP_CRMINQUERY *)
  | 106 (* OP_CRPOSSTAR *)
  | 107 (* OP_CRPOSPLUS *)
  | 108 (* OP_CRPOSQUERY *) ->
      (* pcre2_printint.c:822-833 *)
      Buffer.add_string buf Opcodes.op_names.(rop);
      Opcodes.op_lengths.(rop)
  | 104 (* OP_CRRANGE *) | 105 (* OP_CRMINRANGE *) | 109 (* OP_CRPOSRANGE *) ->
      (* pcre2_printint.c:835-845 *)
      let min = Compile.get2 bytes (ccode + 1) in
      let max = Compile.get2 bytes (ccode + 1 + Limits.imm2_size) in
      if Int.equal max 0 then Printf.bprintf buf "{%d,}" min
      else Printf.bprintf buf "{%d,%d}" min max;
      if Int.equal rop Opcodes.op_crminrange then Buffer.add_char buf '?'
      else if Int.equal rop Opcodes.op_crposrange then Buffer.add_char buf '+';
      Opcodes.op_lengths.(rop)
  | _ ->
      (* pcre2_printint.c:847-851 — do nothing if it's not a repeat. *)
      0

(* pcre2_printint.c:337-884 — pcre2_printint: print a compiled pattern.
   [print_lengths] controls whether offsets and lengths of items are
   printed (pcre2test: fullbincode vs bincode). Compile.re stores the
   program with offset 0 = the C's codestart (the name table lives in its
   own Bytes), so an int offset here IS the C's (code - codestart). *)
let pcre2_printint (buf : Buffer.t) (re : Compile.re) ~(print_lengths : bool) :
    unit =
  let codestart = re.Compile.code in
  let nametable = re.Compile.name_table in
  let nesize = re.Compile.name_entry_size in
  (* pcre2_printint.c:342 — BOOL utf = (re->overall_options & PCRE2_UTF). *)
  let utf = not (Int.equal (re.Compile.overall_options land Options.utf) 0) in
  let b (i : int) : int = Char.code (Bytes.get codestart i) in
  (* pcre2_printint.c:347-883 — the for(;;) scan loop. Arm order follows
     the C switch (port-conventions §2); the int literals are pinned
     against Opcodes constants by the module-initialization asserts below
     (interpreter.ml precedent). The C's OP_TABLE_LENGTH cases (369-374)
     are a compile-time table-size check, never obeyed — opcodes.ml
     carries the equivalent asserts. Opcode values above the tables cannot
     occur in a valid program (the C would index OP_names out of bounds;
     here Array.get would raise). *)
  let rec loop (code : int) : unit =
    (* pcre2_printint.c:355-358 *)
    if print_lengths then Printf.bprintf buf "%3d " code
    else Buffer.add_string buf "    ";
    let op = b code in
    match op with
    | 0 (* OP_END *) ->
        (* pcre2_printint.c:376-379 *)
        Printf.bprintf buf "    %s\n" Opcodes.op_names.(op);
        Buffer.add_string buf
          "------------------------------------------------------------------\n"
    | 29 (* OP_CHAR *) ->
        (* pcre2_printint.c:381-390 *)
        Buffer.add_string buf "    ";
        let rec chars (code : int) : int =
          let code = code + 1 in
          let code = code + 1 + print_char buf codestart code utf in
          if Int.equal (b code) Opcodes.op_char then (chars [@tailcall]) code
          else code
        in
        let code = chars code in
        Buffer.add_char buf '\n';
        (loop [@tailcall]) code
    | 30 (* OP_CHARI *) ->
        (* pcre2_printint.c:392-401 *)
        Buffer.add_string buf " /i ";
        let rec chars (code : int) : int =
          let code = code + 1 in
          let code = code + 1 + print_char buf codestart code utf in
          if Int.equal (b code) Opcodes.op_chari then (chars [@tailcall]) code
          else code
        in
        let code = chars code in
        Buffer.add_char buf '\n';
        (loop [@tailcall]) code
    | _ ->
        (* Arms that `break` in the C fall out with their `extra` value to
           the shared advance at pcre2_printint.c:881-882. *)
        let extra =
          match op with
          | 137 (* OP_CBRA *)
          | 138 (* OP_CBRAPOS *)
          | 142 (* OP_SCBRA *)
          | 143 (* OP_SCBRAPOS *) ->
              (* pcre2_printint.c:403-410 *)
              if print_lengths then
                Printf.bprintf buf "%3d " (Compile.get codestart (code + 1))
              else Buffer.add_string buf "    ";
              Printf.bprintf buf "%s %d" Opcodes.op_names.(op)
                (Compile.get2 codestart (code + 1 + Limits.link_size));
              0
          | 135 (* OP_BRA *)
          | 136 (* OP_BRAPOS *)
          | 140 (* OP_SBRA *)
          | 141 (* OP_SBRAPOS *)
          | 122 (* OP_KETRMAX *)
          | 123 (* OP_KETRMIN *)
          | 124 (* OP_KETRPOS *)
          | 120 (* OP_ALT *)
          | 121 (* OP_KET *)
          | 127 (* OP_ASSERT *)
          | 128 (* OP_ASSERT_NOT *)
          | 129 (* OP_ASSERTBACK *)
          | 130 (* OP_ASSERTBACK_NOT *)
          | 131 (* OP_ASSERT_NA *)
          | 132 (* OP_ASSERTBACK_NA *)
          | 133 (* OP_ONCE *)
          | 134 (* OP_SCRIPT_RUN *)
          | 139 (* OP_COND *)
          | 144 (* OP_SCOND *) ->
              (* pcre2_printint.c:412-434 *)
              if print_lengths then
                Printf.bprintf buf "%3d " (Compile.get codestart (code + 1))
              else Buffer.add_string buf "    ";
              Buffer.add_string buf Opcodes.op_names.(op);
              0
          | 125 (* OP_REVERSE *) ->
              (* pcre2_printint.c:436-440 *)
              if print_lengths then
                Printf.bprintf buf "%3d " (Compile.get2 codestart (code + 1))
              else Buffer.add_string buf "    ";
              Buffer.add_string buf Opcodes.op_names.(op);
              0
          | 126 (* OP_VREVERSE *) ->
              (* pcre2_printint.c:442-447 *)
              if print_lengths then
                Printf.bprintf buf "%3d %d "
                  (Compile.get2 codestart (code + 1))
                  (Compile.get2 codestart (code + 1 + Limits.imm2_size))
              else Buffer.add_string buf "    ";
              Buffer.add_string buf Opcodes.op_names.(op);
              0
          | 166 (* OP_CLOSE *) ->
              (* pcre2_printint.c:449-451 *)
              Printf.bprintf buf "    %s %d" Opcodes.op_names.(op)
                (Compile.get2 codestart (code + 1));
              0
          | 145 (* OP_CREF *) ->
              (* pcre2_printint.c:453-455 *)
              Printf.bprintf buf "%3d %s"
                (Compile.get2 codestart (code + 1))
                Opcodes.op_names.(op);
              0
          | 146 (* OP_DNCREF *) ->
              (* pcre2_printint.c:457-464 — flag is still "  " here. *)
              let entry =
                (Compile.get2 codestart (code + 1) * nesize) + Limits.imm2_size
              in
              Printf.bprintf buf " %s Cond ref <" "  ";
              print_custring buf nametable entry;
              Printf.bprintf buf ">%d"
                (Compile.get2 codestart (code + 1 + Limits.imm2_size));
              0
          | 147 (* OP_RREF *) ->
              (* pcre2_printint.c:466-472 *)
              let c = Compile.get2 codestart (code + 1) in
              if Int.equal c Opcodes.rref_any then
                Buffer.add_string buf "    Cond recurse any"
              else Printf.bprintf buf "    Cond recurse %d" c;
              0
          | 148 (* OP_DNRREF *) ->
              (* pcre2_printint.c:474-481 — flag is still "  " here. *)
              let entry =
                (Compile.get2 codestart (code + 1) * nesize) + Limits.imm2_size
              in
              Printf.bprintf buf " %s Cond recurse <" "  ";
              print_custring buf nametable entry;
              Printf.bprintf buf ">%d"
                (Compile.get2 codestart (code + 1 + Limits.imm2_size));
              0
          | 149 (* OP_FALSE *) ->
              (* pcre2_printint.c:483-485 *)
              Buffer.add_string buf "    Cond false";
              0
          | 150 (* OP_TRUE *) ->
              (* pcre2_printint.c:487-489 *)
              Buffer.add_string buf "    Cond true";
              0
          | 46 (* OP_STARI *)
          | 47 (* OP_MINSTARI *)
          | 55 (* OP_POSSTARI *)
          | 48 (* OP_PLUSI *)
          | 49 (* OP_MINPLUSI *)
          | 56 (* OP_POSPLUSI *)
          | 50 (* OP_QUERYI *)
          | 51 (* OP_MINQUERYI *)
          | 57 (* OP_POSQUERYI *)
          (* fallthrough from the I opcodes (flag = "/i") in C *)
          | 33 (* OP_STAR *)
          | 34 (* OP_MINSTAR *)
          | 42 (* OP_POSSTAR *)
          | 35 (* OP_PLUS *)
          | 36 (* OP_MINPLUS *)
          | 43 (* OP_POSPLUS *)
          | 37 (* OP_QUERY *)
          | 38 (* OP_MINQUERY *)
          | 44 (* OP_POSQUERY *)
          | 85 (* OP_TYPESTAR *)
          | 86 (* OP_TYPEMINSTAR *)
          | 94 (* OP_TYPEPOSSTAR *)
          | 87 (* OP_TYPEPLUS *)
          | 88 (* OP_TYPEMINPLUS *)
          | 95 (* OP_TYPEPOSPLUS *)
          | 89 (* OP_TYPEQUERY *)
          | 90 (* OP_TYPEMINQUERY *)
          | 96 (* OP_TYPEPOSQUERY *) ->
              (* pcre2_printint.c:491-533. Of this arm's members, exactly
                 OP_STARI..OP_POSQUERYI (46..57 minus the UPTOI/EXACTI
                 trio, which belong to the next arm) took the C's flag =
                 "/i" fallthrough. *)
              let flag =
                if op >= Opcodes.op_stari && op <= Opcodes.op_posqueryi then
                  "/i"
                else "  "
              in
              Printf.bprintf buf " %s " flag;
              let extra =
                if op >= Opcodes.op_typestar then
                  let c1 = b (code + 1) in
                  if
                    Int.equal c1 Opcodes.op_prop
                    || Int.equal c1 Opcodes.op_notprop
                  then (
                    print_prop buf codestart (code + 1) "" " ";
                    2)
                  else (
                    Buffer.add_string buf Opcodes.op_names.(c1);
                    0)
                else print_char buf codestart (code + 1) utf
              in
              Buffer.add_string buf Opcodes.op_names.(op);
              extra
          | 54 (* OP_EXACTI *)
          | 52 (* OP_UPTOI *)
          | 53 (* OP_MINUPTOI *)
          | 58 (* OP_POSUPTOI *)
          (* fallthrough from the I opcodes (flag = "/i") in C *)
          | 41 (* OP_EXACT *)
          | 39 (* OP_UPTO *)
          | 40 (* OP_MINUPTO *)
          | 45 (* OP_POSUPTO *) ->
              (* pcre2_printint.c:535-552. The caseless members are all
                 >= OP_UPTOI (52). *)
              let flag = if op >= Opcodes.op_uptoi then "/i" else "  " in
              Printf.bprintf buf " %s " flag;
              let extra =
                print_char buf codestart (code + 1 + Limits.imm2_size) utf
              in
              Buffer.add_char buf '{';
              if
                (not (Int.equal op Opcodes.op_exact))
                && not (Int.equal op Opcodes.op_exacti)
              then Buffer.add_string buf "0,";
              Printf.bprintf buf "%d}" (Compile.get2 codestart (code + 1));
              if
                Int.equal op Opcodes.op_minupto
                || Int.equal op Opcodes.op_minuptoi
              then Buffer.add_char buf '?'
              else if
                Int.equal op Opcodes.op_posupto
                || Int.equal op Opcodes.op_posuptoi
              then Buffer.add_char buf '+';
              extra
          | 93 (* OP_TYPEEXACT *)
          | 91 (* OP_TYPEUPTO *)
          | 92 (* OP_TYPEMINUPTO *)
          | 97 (* OP_TYPEPOSUPTO *) ->
              (* pcre2_printint.c:554-569 *)
              let c1 = b (code + 1 + Limits.imm2_size) in
              let extra =
                if
                  Int.equal c1 Opcodes.op_prop
                  || Int.equal c1 Opcodes.op_notprop
                then (
                  print_prop buf codestart
                    (code + Limits.imm2_size + 1)
                    "    " " ";
                  2)
                else (
                  Printf.bprintf buf "    %s" Opcodes.op_names.(c1);
                  0)
              in
              Buffer.add_char buf '{';
              if not (Int.equal op Opcodes.op_typeexact) then
                Buffer.add_string buf "0,";
              Printf.bprintf buf "%d}" (Compile.get2 codestart (code + 1));
              if Int.equal op Opcodes.op_typeminupto then
                Buffer.add_char buf '?'
              else if Int.equal op Opcodes.op_typeposupto then
                Buffer.add_char buf '+';
              extra
          | 32 (* OP_NOTI — flag = "/i", fallthrough in C *) | 31 (* OP_NOT *)
            ->
              (* pcre2_printint.c:571-578 *)
              let flag = if Int.equal op Opcodes.op_noti then "/i" else "  " in
              Printf.bprintf buf " %s [^" flag;
              let extra = print_char buf codestart (code + 1) utf in
              Buffer.add_char buf ']';
              extra
          | 72 (* OP_NOTSTARI *)
          | 73 (* OP_NOTMINSTARI *)
          | 81 (* OP_NOTPOSSTARI *)
          | 74 (* OP_NOTPLUSI *)
          | 75 (* OP_NOTMINPLUSI *)
          | 82 (* OP_NOTPOSPLUSI *)
          | 76 (* OP_NOTQUERYI *)
          | 77 (* OP_NOTMINQUERYI *)
          | 83 (* OP_NOTPOSQUERYI *)
          (* fallthrough from the I opcodes (flag = "/i") in C *)
          | 59 (* OP_NOTSTAR *)
          | 60 (* OP_NOTMINSTAR *)
          | 68 (* OP_NOTPOSSTAR *)
          | 61 (* OP_NOTPLUS *)
          | 62 (* OP_NOTMINPLUS *)
          | 69 (* OP_NOTPOSPLUS *)
          | 63 (* OP_NOTQUERY *)
          | 64 (* OP_NOTMINQUERY *)
          | 70 (* OP_NOTPOSQUERY *) ->
              (* pcre2_printint.c:580-604. The caseless members are all
                 >= OP_NOTSTARI (72). *)
              let flag = if op >= Opcodes.op_notstari then "/i" else "  " in
              Printf.bprintf buf " %s [^" flag;
              let extra = print_char buf codestart (code + 1) utf in
              Printf.bprintf buf "]%s" Opcodes.op_names.(op);
              extra
          | 80 (* OP_NOTEXACTI *)
          | 78 (* OP_NOTUPTOI *)
          | 79 (* OP_NOTMINUPTOI *)
          | 84 (* OP_NOTPOSUPTOI *)
          (* fallthrough from the I opcodes (flag = "/i") in C *)
          | 67 (* OP_NOTEXACT *)
          | 65 (* OP_NOTUPTO *)
          | 66 (* OP_NOTMINUPTO *)
          | 71 (* OP_NOTPOSUPTO *) ->
              (* pcre2_printint.c:606-625. The caseless members are all
                 >= OP_NOTUPTOI (78). *)
              let flag = if op >= Opcodes.op_notuptoi then "/i" else "  " in
              Printf.bprintf buf " %s [^" flag;
              let extra =
                print_char buf codestart (code + 1 + Limits.imm2_size) utf
              in
              Buffer.add_string buf "]{";
              if
                (not (Int.equal op Opcodes.op_notexact))
                && not (Int.equal op Opcodes.op_notexacti)
              then Buffer.add_string buf "0,";
              Printf.bprintf buf "%d}" (Compile.get2 codestart (code + 1));
              if
                Int.equal op Opcodes.op_notminupto
                || Int.equal op Opcodes.op_notminuptoi
              then Buffer.add_char buf '?'
              else if
                Int.equal op Opcodes.op_notposupto
                || Int.equal op Opcodes.op_notposuptoi
              then Buffer.add_char buf '+';
              extra
          | 117 (* OP_RECURSE *) ->
              (* pcre2_printint.c:627-631 *)
              if print_lengths then
                Printf.bprintf buf "%3d " (Compile.get codestart (code + 1))
              else Buffer.add_string buf "    ";
              Buffer.add_string buf Opcodes.op_names.(op);
              0
          | 114 (* OP_REFI — flag = "/i", fallthrough in C *) | 113 (* OP_REF *)
            ->
              (* pcre2_printint.c:633-639 — goto CLASS_REF_REPEAT *)
              let flag = if Int.equal op Opcodes.op_refi then "/i" else "  " in
              Printf.bprintf buf " %s \\%d" flag
                (Compile.get2 codestart (code + 1));
              let ccode = code + Opcodes.op_lengths.(op) in
              class_ref_repeat buf codestart ccode
          | 116 (* OP_DNREFI — flag = "/i", fallthrough in C *)
          | 115 (* OP_DNREF *) ->
              (* pcre2_printint.c:641-652 — goto CLASS_REF_REPEAT *)
              let flag =
                if Int.equal op Opcodes.op_dnrefi then "/i" else "  "
              in
              let entry =
                (Compile.get2 codestart (code + 1) * nesize) + Limits.imm2_size
              in
              Printf.bprintf buf " %s \\k<" flag;
              print_custring buf nametable entry;
              Printf.bprintf buf ">%d"
                (Compile.get2 codestart (code + 1 + Limits.imm2_size));
              let ccode = code + Opcodes.op_lengths.(op) in
              class_ref_repeat buf codestart ccode
          | 118 (* OP_CALLOUT *) ->
              (* pcre2_printint.c:654-657 *)
              Printf.bprintf buf "    %s %d %d %d" Opcodes.op_names.(op)
                (b (code + 1 + (2 * Limits.link_size)))
                (Compile.get codestart (code + 1))
                (Compile.get codestart (code + 1 + Limits.link_size));
              0
          | 119 (* OP_CALLOUT_STR *) ->
              (* pcre2_printint.c:659-672 *)
              let c = ref (b (code + 1 + (4 * Limits.link_size))) in
              Printf.bprintf buf "    %s %c" Opcodes.op_names.(op) (Char.chr !c);
              let extra =
                Compile.get codestart (code + 1 + (2 * Limits.link_size))
              in
              print_custring_bylen buf codestart
                (code + 2 + (4 * Limits.link_size))
                (extra - 3 - (4 * Limits.link_size));
              (let i = ref 0 in
               let searching = ref true in
               while
                 !searching
                 && not (Int.equal Tables.callout_start_delims.(!i) 0)
               do
                 if Int.equal !c Tables.callout_start_delims.(!i) then (
                   c := Tables.callout_end_delims.(!i);
                   searching := false)
                 else incr i
               done);
              Printf.bprintf buf "%c %d %d %d" (Char.chr !c)
                (Compile.get codestart (code + 1 + (3 * Limits.link_size)))
                (Compile.get codestart (code + 1))
                (Compile.get codestart (code + 1 + Limits.link_size));
              extra
          | 16 (* OP_PROP *) | 15 (* OP_NOTPROP *) ->
              (* pcre2_printint.c:674-677 *)
              print_prop buf codestart code "    " "";
              0
          | 110 (* OP_CLASS *) | 111 (* OP_NCLASS *) | 112 (* OP_XCLASS *) ->
              (* pcre2_printint.c:679-813 — OP_XCLASS cannot occur in
                 8-bit non-UTF mode, but as in the C there is no harm in
                 always having the code here. *)
              Buffer.add_string buf "    [";
              let extra = ref 0 in
              let invertmap = ref false in
              let printmap = ref false in
              let ccode = ref 0 in
              (* pcre2_printint.c:691-711 — negative XCLASS has an
                 inverted map whereas CLASS/NCLASS have already done the
                 inversion. *)
              if Int.equal op Opcodes.op_xclass then (
                extra := Compile.get codestart (code + 1);
                ccode := code + Limits.link_size + 1;
                let fl = b !ccode in
                printmap := not (Int.equal (fl land Opcodes.xcl_map) 0);
                if not (Int.equal (fl land Opcodes.xcl_not) 0) then (
                  invertmap := Int.equal (fl land Opcodes.xcl_hasprop) 0;
                  Buffer.add_char buf '^');
                incr ccode)
              else (
                (* CLASS or NCLASS *)
                printmap := true;
                ccode := code + 1);
              (* pcre2_printint.c:713-748 — print a bit map as ranges of
                 set bits. The C aliases the in-place map and copies only
                 to invert; Bytes.sub copies unconditionally (same
                 bytes read either way). *)
              if !printmap then (
                let map = Bytes.sub codestart !ccode 32 in
                if !invertmap then
                  for k = 0 to 31 do
                    (* 255 ^ map[k], as in the C *)
                    Bytes.set map k
                      (Char.chr (255 lxor Char.code (Bytes.get map k)))
                  done;
                let bit_set (k : int) : bool =
                  not
                    (Int.equal
                       (Char.code (Bytes.get map (k / 8))
                       land (1 lsl (k land 7)))
                       0)
                in
                let i = ref 0 in
                while !i < 256 do
                  if bit_set !i then (
                    let j = ref (!i + 1) in
                    while !j < 256 && bit_set !j do
                      incr j
                    done;
                    if
                      Int.equal !i (Char.code '-')
                      || Int.equal !i (Char.code ']')
                    then Buffer.add_char buf '\\';
                    if printable !i then Buffer.add_char buf (Char.chr !i)
                    else Printf.bprintf buf "\\x%02x" !i;
                    decr j;
                    if !j > !i then (
                      if not (Int.equal !j (!i + 1)) then
                        Buffer.add_char buf '-';
                      if
                        Int.equal !j (Char.code '-')
                        || Int.equal !j (Char.code ']')
                      then Buffer.add_char buf '\\';
                      if printable !j then Buffer.add_char buf (Char.chr !j)
                      else Printf.bprintf buf "\\x%02x" !j);
                    i := !j);
                  incr i
                done;
                ccode := !ccode + 32);
              (* pcre2_printint.c:751-809 — for an XCLASS there is always
                 some additional data. *)
              (if Int.equal op Opcodes.op_xclass then
                 let scanning = ref true in
                 while !scanning do
                   let ch = b !ccode in
                   incr ccode;
                   if Int.equal ch Opcodes.xcl_end then scanning := false
                   else if
                     Int.equal ch Opcodes.xcl_notprop
                     || Int.equal ch Opcodes.xcl_prop
                   then (
                     (* fallthrough from XCL_NOTPROP (notch = "^") in C *)
                     let notch =
                       if Int.equal ch Opcodes.xcl_notprop then "^" else ""
                     in
                     let ptype = b !ccode in
                     incr ccode;
                     let pvalue = b !ccode in
                     incr ccode;
                     if Int.equal ptype Opcodes.pt_pxgraph then
                       Printf.bprintf buf "[:%sgraph:]" notch
                     else if Int.equal ptype Opcodes.pt_pxprint then
                       Printf.bprintf buf "[:%sprint:]" notch
                     else if Int.equal ptype Opcodes.pt_pxpunct then
                       Printf.bprintf buf "[:%spunct:]" notch
                     else if Int.equal ptype Opcodes.pt_pxxdigit then
                       Printf.bprintf buf "[:%sxdigit:]" notch
                     else
                       let s = get_ucpname ptype pvalue in
                       Printf.bprintf buf "\\%c{%c%s}"
                         (if String.equal notch "^" then 'P' else 'p')
                         (Char.uppercase_ascii s.[0])
                         (String.sub s 1 (String.length s - 1)))
                   else (
                     (* default: XCL_SINGLE / XCL_RANGE character data *)
                     ccode := !ccode + 1 + print_char buf codestart !ccode utf;
                     if Int.equal ch Opcodes.xcl_range then (
                       Buffer.add_char buf '-';
                       ccode := !ccode + 1 + print_char buf codestart !ccode utf))
                 done);
              (* pcre2_printint.c:811-813 — indicate a non-UTF class
                 created by negation. *)
              Printf.bprintf buf "]%s"
                (if Int.equal op Opcodes.op_nclass then " (neg)" else "");
              (* pcre2_printint.c:817-852 — CLASS_REF_REPEAT falls here. *)
              extra := !extra + class_ref_repeat buf codestart !ccode;
              !extra
          | 154 (* OP_MARK *)
          | 162 (* OP_COMMIT_ARG *)
          | 156 (* OP_PRUNE_ARG *)
          | 158 (* OP_SKIP_ARG *)
          | 160 (* OP_THEN_ARG *) ->
              (* pcre2_printint.c:855-863 *)
              Printf.bprintf buf "    %s " Opcodes.op_names.(op);
              print_custring_bylen buf codestart (code + 2) (b (code + 1));
              b (code + 1)
          | 159 (* OP_THEN *) ->
              (* pcre2_printint.c:865-867 *)
              Printf.bprintf buf "    %s" Opcodes.op_names.(op);
              0
          | _ ->
              (* pcre2_printint.c:869-878 — OP_CIRCM/OP_DOLLM set flag =
                 "/m" and fall through; anything else is just an item with
                 no data, but possibly a flag. *)
              let flag =
                if
                  Int.equal op Opcodes.op_circm || Int.equal op Opcodes.op_dollm
                then "/m"
                else "  "
              in
              Printf.bprintf buf " %s %s" flag Opcodes.op_names.(op);
              0
        in
        (* pcre2_printint.c:881-882 *)
        Buffer.add_char buf '\n';
        (loop [@tailcall]) (code + Opcodes.op_lengths.(op) + extra)
  in
  loop 0
