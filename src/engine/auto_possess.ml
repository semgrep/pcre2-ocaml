(* Auto-possessification pass for the pure-OCaml PCRE2 10.44 port (8-bit
   library).

   Ported from vendor/pcre2/src/pcre2_auto_possess.c (whole file, 1-1371):
   scans a compiled pattern and changes repeats into possessive repeats
   where backtracking provably cannot help. The pass is behavior-neutral —
   it only rewrites quantifier opcodes in place.

   Pointer mapping: the C's PCRE2_SPTR/PCRE2_UCHAR* pointers into the
   compiled program become int offsets into the code [Bytes.t]; NULL
   returns become -1. The uint32_t list[8] property lists become int
   arrays. All reads use [Bytes.get]: every offset touched lies inside a
   complete compiled program (the complete-program invariant the
   interpreter and printer already rely on), and a corrupt opcode stops
   the scan with -1 → ERR80 exactly as the C (pcre2_auto_possess.c:1152).

   DEVIATION: the C passes `const compile_block *cb` and reads exactly
   cb->external_options, cb->had_recurse, cb->fcc, cb->cbits and
   cb->ctypes. The compile_block record lives in Compile, which calls this
   module, so cb is reduced here to the [~external_options]/[~had_recurse]
   arguments; the character tables are the Chartables module, per the
   project-wide custom-tables deviation (compile.ml compile_block note). *)

(* pcre2_intmodedep.h:104-109 — GET: fetch a LINK_SIZE = 2 big-endian
   offset. DEVIATION (structural only): duplicated from Compile.get to
   avoid a dependency cycle (Compile calls this module). *)
let get (code : Bytes.t) (n : int) : int =
  (Char.code (Bytes.get code n) lsl 8) lor Char.code (Bytes.get code (n + 1))

(* pcre2_intmodedep.h:192-195 — GET2: a 16-bit quantity (repeat counts),
   IMM2_SIZE = 2, same layout as GET (duplicated from Compile.get2, see
   the note on [get]). *)
let get2 (code : Bytes.t) (n : int) : int =
  (Char.code (Bytes.get code n) lsl 8) lor Char.code (Bytes.get code (n + 1))

(* GETCHARINCTEST (pcre2_intmodedep.h:319-324) over the compiled program:
   get the next character, testing for UTF-8 mode, advancing the position.
   Pattern literals are complete ord2utf encodings, so the continuation
   reads are in bounds (see Utf.getutf8_bytes and the xclass.ml
   precedent). *)
let getcharinctest_bytes ~(utf : bool) (code : Bytes.t) (pos : int ref) : int =
  let c = Char.code (Bytes.get code !pos) in
  incr pos;
  if utf && c >= 0xc0 then (
    let v = Utf.getutf8_bytes c code (!pos - 1) in
    pos := !pos + Utf.get_extralen c;
    v)
  else c

(* pcre2_auto_possess.c:70-71 — table dimensions: rows are the left
   (repeated) opcodes OP_NOT_DIGIT..OP_EXTUNI, columns the right opcodes
   OP_NOT_DIGIT..OP_DOLLM. *)

(* pcre2_auto_possess.c:73-92 — autoposstab: whether auto-possessification
   is possible between adjacent character-type opcodes. A value of 1 means
   OK; \P and \p rows/columns are always 0 (handled separately in the
   code). OP_DIGIT etc. are generated only when PCRE2_UCP is not set. *)
let autoposstab =
  [|
    (*  \D \d \S \s \W \w  . .+ \C \P \p \R \H \h \V \v \X \Z \z  $ $M *)
    [| 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0 |];
    (* \D *)
    [| 1; 0; 0; 1; 1; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 1; 1; 1; 1 |];
    (* \d *)
    [| 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 1; 1; 1; 1 |];
    (* \S *)
    [| 0; 1; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0 |];
    (* \s *)
    [| 0; 1; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0 |];
    (* \W *)
    [| 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 1; 1; 1; 1 |];
    (* \w *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 1; 0; 0 |];
    (* .  *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0 |];
    (* .+ *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0 |];
    (* \C *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
    (* \P *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
    (* \p *)
    [| 0; 1; 0; 1; 0; 1; 1; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 0; 0 |];
    (* \R *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 0; 0 |];
    (* \H *)
    [| 0; 1; 1; 0; 0; 1; 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 0; 1; 0; 0 |];
    (* \h *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0; 0; 1; 0; 0 |];
    (* \V *)
    [| 0; 1; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 1; 0; 0 |];
    (* \v *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0 |];
    (* \X *)
  |]

(* pcre2_auto_possess.c:94-141 — propposstab: whether auto-
   possessification is possible between adjacent Unicode property opcodes
   (OP_PROP and OP_NOTPROP), indexed [left PT_*][right PT_*]. Value key:
   0 never, 1 both-OP_PROP distinct groups, 2 same-group categories,
   3 opcodes differ, 4/5 general vs particular category, 6-17 the special
   ALNUM/SPACE/WORD combinations (see the case comments in
   compare_opcodes). *)
let propposstab =
  [|
    (* ANY LAMP GC PC SC SCX ALNUM SPACE PXSPACE WORD CLIST UCNC BIDICL
       BOOL *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
    (* PT_ANY *)
    [| 0; 3; 0; 0; 0; 0; 3; 1; 1; 0; 0; 0; 0; 0 |];
    (* PT_LAMP *)
    [| 0; 0; 2; 4; 0; 0; 9; 10; 10; 11; 0; 0; 0; 0 |];
    (* PT_GC *)
    [| 0; 0; 5; 2; 0; 0; 15; 16; 16; 17; 0; 0; 0; 0 |];
    (* PT_PC *)
    [| 0; 0; 0; 0; 2; 2; 0; 0; 0; 0; 0; 0; 0; 0 |];
    (* PT_SC *)
    [| 0; 0; 0; 0; 2; 2; 0; 0; 0; 0; 0; 0; 0; 0 |];
    (* PT_SCX *)
    [| 0; 3; 6; 12; 0; 0; 3; 1; 1; 0; 0; 0; 0; 0 |];
    (* PT_ALNUM *)
    [| 0; 1; 7; 13; 0; 0; 1; 3; 3; 1; 0; 0; 0; 0 |];
    (* PT_SPACE *)
    [| 0; 1; 7; 13; 0; 0; 1; 3; 3; 1; 0; 0; 0; 0 |];
    (* PT_PXSPACE *)
    [| 0; 0; 8; 14; 0; 0; 0; 1; 1; 3; 0; 0; 0; 0 |];
    (* PT_WORD *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
    (* PT_CLIST *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 3; 0; 0 |];
    (* PT_UCNC *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
    (* PT_BIDICL *)
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
    (* PT_BOOL *)
  |]

(* pcre2_auto_possess.c:143-159 — catposstab: general category (row,
   ucp_C..ucp_Z = 0..6) vs particular category (column, ucp_Cc..ucp_Zs =
   0..29); 1 if the particular category is not part of the general
   category. *)
let catposstab =
  [|
    (* Cc Cf Cn Co Cs Ll Lm Lo Lt Lu Mc Me Mn Nd Nl No Pc Pd Pe Pf Pi Po Ps
       Sc Sk Sm So Zl Zp Zs *)
    [|
      0;
      0;
      0;
      0;
      0;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
    |];
    (* C *)
    [|
      1;
      1;
      1;
      1;
      1;
      0;
      0;
      0;
      0;
      0;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
    |];
    (* L *)
    [|
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      0;
      0;
      0;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
    |];
    (* M *)
    [|
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      0;
      0;
      0;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
    |];
    (* N *)
    [|
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      0;
      0;
      0;
      0;
      0;
      0;
      0;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
    |];
    (* P *)
    [|
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      0;
      0;
      0;
      0;
      1;
      1;
      1;
    |];
    (* S *)
    [|
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      1;
      0;
      0;
      0;
    |];
    (* Z *)
  |]

(* pcre2_auto_possess.c:161-176 — posspropstab: for checking ALNUM,
   (PX)SPACE, and WORD against a general or particular category. The first
   two entries per row are general categories that always apply; the third
   and fourth are a general and a particular category (respectively) that
   include one or more relevant characters. *)
let posspropstab =
  [|
    [| Ucp.ucp_l; Ucp.ucp_n; Ucp.ucp_n; Ucp.ucp_nl |];
    (* ALNUM, 3rd and 4th values redundant *)
    [| Ucp.ucp_z; Ucp.ucp_z; Ucp.ucp_c; Ucp.ucp_cc |];
    (* SPACE and PXSPACE, 2nd value redundant *)
    [| Ucp.ucp_l; Ucp.ucp_n; Ucp.ucp_p; Ucp.ucp_po |];
    (* WORD *)
  |]

(* pcre2_auto_possess.c:181-275 — check_char_prop: called by
   compare_opcodes() when a property item is adjacent to a fixed
   character. Returns TRUE if auto-possessifying is OK. The C's
   `(cond) == negated` BOOL comparisons become Bool.equal. *)
let check_char_prop (c : int) (ptype : int) (pdata : int) ~(negated : bool) :
    bool =
  (* pcre2_auto_possess.c:204 — const ucd_record *prop = GET_UCD(c). *)
  let ri = Ucd.record_index c in
  if Int.equal ptype Opcodes.pt_lamp then
    (* pcre2_auto_possess.c:208-211 *)
    let chartype = Ucd_tables.chartype ri in
    Bool.equal
      (Int.equal chartype Ucp.ucp_lu
      || Int.equal chartype Ucp.ucp_ll
      || Int.equal chartype Ucp.ucp_lt)
      negated
  else if Int.equal ptype Opcodes.pt_gc then
    (* pcre2_auto_possess.c:213-214 *)
    Bool.equal
      (Int.equal pdata Tables.ucp_gentype.(Ucd_tables.chartype ri))
      negated
  else if Int.equal ptype Opcodes.pt_pc then
    (* pcre2_auto_possess.c:216-217 *)
    Bool.equal (Int.equal pdata (Ucd_tables.chartype ri)) negated
  else if Int.equal ptype Opcodes.pt_sc then
    (* pcre2_auto_possess.c:219-220 *)
    Bool.equal (Int.equal pdata (Ucd_tables.script ri)) negated
  else if Int.equal ptype Opcodes.pt_scx then
    (* pcre2_auto_possess.c:222-225 — script match or Script Extensions
       set bit (MAPBIT bound proof: Ucd.script_set_contains). *)
    let ok =
      Int.equal pdata (Ucd_tables.script ri) || Ucd.script_set_contains ri pdata
    in
    Bool.equal ok negated
  else if Int.equal ptype Opcodes.pt_alnum then
    (* pcre2_auto_possess.c:229-231 *)
    let gentype = Tables.ucp_gentype.(Ucd_tables.chartype ri) in
    Bool.equal
      (Int.equal gentype Ucp.ucp_l || Int.equal gentype Ucp.ucp_n)
      negated
  else if Int.equal ptype Opcodes.pt_space || Int.equal ptype Opcodes.pt_pxspace
  then
    (* pcre2_auto_possess.c:237-248 — Perl space and POSIX space are
       identical since Perl 5.18 / PCRE 8.34: HSPACE_CASES/VSPACE_CASES,
       else general category Z. *)
    if Xclass.hspace_char c || Xclass.vspace_char c then negated
    else
      Bool.equal
        (Int.equal Tables.ucp_gentype.(Ucd_tables.chartype ri) Ucp.ucp_z)
        negated
  else if Int.equal ptype Opcodes.pt_word then
    (* pcre2_auto_possess.c:250-253 — CHAR_UNDERSCORE = 0x5f. *)
    let gentype = Tables.ucp_gentype.(Ucd_tables.chartype ri) in
    Bool.equal
      (Int.equal gentype Ucp.ucp_l
      || Int.equal gentype Ucp.ucp_n
      || Int.equal c 0x5f)
      negated
  else if Int.equal ptype Opcodes.pt_clist then
    (* pcre2_auto_possess.c:255-261 — scan the caseless set: it is sorted
       ascending and NOTACHAR-terminated, so `c < *p` terminates at the
       latest on the terminator (c <= 0x7fffffff < NOTACHAR). *)
    let rec scan (p : int) : bool =
      if c < Ucd_tables.ucd_caseless_sets.(p) then not negated
      else if Int.equal c Ucd_tables.ucd_caseless_sets.(p) then negated
      else (scan [@tailcall]) (p + 1)
    in
    scan (Ucd_tables.caseset ri)
  else if Int.equal ptype Opcodes.pt_bidicl then
    (* pcre2_auto_possess.c:266-267 — haven't yet thought these through. *)
    false
  else if Int.equal ptype Opcodes.pt_bool then
    (* pcre2_auto_possess.c:269-270 *)
    false
  else false (* pcre2_auto_possess.c:273 — no case for PT_ANY etc. *)

(* pcre2_auto_possess.c:279-299 — get_repeat_base: the base opcode for
   repeated single character type opcodes; the original value if the
   opcode is not a repeated character type. *)
let get_repeat_base (c : int) : int =
  if c > Opcodes.op_typeposupto then c
  else if c >= Opcodes.op_typestar then Opcodes.op_typestar
  else if c >= Opcodes.op_notstari then Opcodes.op_notstari
  else if c >= Opcodes.op_notstar then Opcodes.op_notstar
  else if c >= Opcodes.op_stari then Opcodes.op_stari
  else Opcodes.op_star

(* pcre2_auto_possess.c:302-512 — get_chr_property_list: checks whether
   [pos] points to an opcode that can take part in auto-possessification,
   and if so fills [list] with its properties: list.(0) the (normalized)
   opcode, list.(1) non-zero if it can match an empty string, list.(2..7)
   opcode-dependent. Returns the offset of the next opcode, or -1 for the
   C's NULL (opcode not accepted). The C's fcc argument is the Chartables
   module (see the module DEVIATION note). *)
let get_chr_property_list (code : Bytes.t) (start : int) ~(utf : bool)
    ~(ucp : bool) (list : int array) : int =
  let c = ref (Char.code (Bytes.get code start)) in
  let pos = ref (start + 1) in
  list.(0) <- !c;
  list.(1) <- 0 (* FALSE *);
  (* pcre2_auto_possess.c:345-380 — normalize a repeat to its base
     single-character opcode. *)
  if !c >= Opcodes.op_star && !c <= Opcodes.op_typeposupto then (
    let base = get_repeat_base !c in
    c := !c - (base - Opcodes.op_star);
    if
      Int.equal !c Opcodes.op_upto
      || Int.equal !c Opcodes.op_minupto
      || Int.equal !c Opcodes.op_exact
      || Int.equal !c Opcodes.op_posupto
    then pos := !pos + Limits.imm2_size;
    list.(1) <-
      (if
         (not (Int.equal !c Opcodes.op_plus))
         && (not (Int.equal !c Opcodes.op_minplus))
         && (not (Int.equal !c Opcodes.op_exact))
         && not (Int.equal !c Opcodes.op_posplus)
       then 1
       else 0);
    (* pcre2_auto_possess.c:356-378 — switch (base). *)
    if Int.equal base Opcodes.op_star then list.(0) <- Opcodes.op_char
    else if Int.equal base Opcodes.op_stari then list.(0) <- Opcodes.op_chari
    else if Int.equal base Opcodes.op_notstar then list.(0) <- Opcodes.op_not
    else if Int.equal base Opcodes.op_notstari then list.(0) <- Opcodes.op_noti
    else if Int.equal base Opcodes.op_typestar then (
      list.(0) <- Char.code (Bytes.get code !pos);
      incr pos);
    c := list.(0));
  let c = !c in
  (* pcre2_auto_possess.c:382-402 — the opcodes accepted with no extra
     data. *)
  if
    Int.equal c Opcodes.op_not_digit
    || Int.equal c Opcodes.op_digit
    || Int.equal c Opcodes.op_not_whitespace
    || Int.equal c Opcodes.op_whitespace
    || Int.equal c Opcodes.op_not_wordchar
    || Int.equal c Opcodes.op_wordchar
    || Int.equal c Opcodes.op_any
    || Int.equal c Opcodes.op_allany
    || Int.equal c Opcodes.op_anynl
    || Int.equal c Opcodes.op_not_hspace
    || Int.equal c Opcodes.op_hspace
    || Int.equal c Opcodes.op_not_vspace
    || Int.equal c Opcodes.op_vspace
    || Int.equal c Opcodes.op_extuni
    || Int.equal c Opcodes.op_eodn
    || Int.equal c Opcodes.op_eod
    || Int.equal c Opcodes.op_doll
    || Int.equal c Opcodes.op_dollm
  then !pos
  else if Int.equal c Opcodes.op_char || Int.equal c Opcodes.op_not then (
    (* pcre2_auto_possess.c:404-409 *)
    let chr = getcharinctest_bytes ~utf code pos in
    list.(2) <- chr;
    list.(3) <- Tables.notachar;
    !pos)
  else if Int.equal c Opcodes.op_chari || Int.equal c Opcodes.op_noti then (
    (* pcre2_auto_possess.c:411-434 *)
    list.(0) <-
      (if Int.equal c Opcodes.op_chari then Opcodes.op_char else Opcodes.op_not);
    let chr = getcharinctest_bytes ~utf code pos in
    list.(2) <- chr;
    (* pcre2_auto_possess.c:417-421 — the SUPPORT_UNICODE arm. *)
    if chr < 128 || (chr < 256 && (not utf) && not ucp) then
      list.(3) <- Chartables.fcc chr
    else list.(3) <- Ucd.othercase chr;
    (* pcre2_auto_possess.c:428-433 — the othercase might be the same
       value. *)
    if Int.equal chr list.(3) then list.(3) <- Tables.notachar
    else list.(4) <- Tables.notachar;
    !pos)
  else if Int.equal c Opcodes.op_prop || Int.equal c Opcodes.op_notprop then (
    if not (Int.equal (Char.code (Bytes.get code !pos)) Opcodes.pt_clist) then (
      (* pcre2_auto_possess.c:439-443 *)
      list.(2) <- Char.code (Bytes.get code !pos);
      list.(3) <- Char.code (Bytes.get code (!pos + 1));
      !pos + 2)
    else
      (* pcre2_auto_possess.c:446-469 — convert a PT_CLIST property into
         the character list, if there is enough space. *)
      let clist_src = ref (Char.code (Bytes.get code (!pos + 1))) in
      let clist_dest = ref 2 in
      pos := !pos + 2;
      let result = ref (-2) (* sentinel: no early return taken *) in
      let scanning = ref true in
      while !scanning do
        if !clist_dest >= 8 then (
          (* pcre2_auto_possess.c:453-460 — early return if there is not
             enough space; should never happen, since all clists are
             shorter than 5 characters. *)
          list.(2) <- Char.code (Bytes.get code !pos);
          list.(3) <- Char.code (Bytes.get code (!pos + 1));
          result := !pos;
          scanning := false)
        else
          let v = Ucd_tables.ucd_caseless_sets.(!clist_src) in
          list.(!clist_dest) <- v;
          incr clist_dest;
          incr clist_src;
          if Int.equal v Tables.notachar then scanning := false
      done;
      if not (Int.equal !result (-2)) then !result
      else (
        (* pcre2_auto_possess.c:465-469 — all characters (including the
           terminating NOTACHAR) are stored. *)
        list.(0) <-
          (if Int.equal c Opcodes.op_prop then Opcodes.op_char
           else Opcodes.op_not);
        !pos))
  else if
    Int.equal c Opcodes.op_nclass
    || Int.equal c Opcodes.op_class
    || Int.equal c Opcodes.op_xclass
  then (
    (* pcre2_auto_possess.c:472-508 — classes: find the end of the class
       data (GET(code,0) is read at the post-opcode position, hence the
       -1), then look at any repeat that follows. *)
    let e =
      ref
        (if Int.equal c Opcodes.op_xclass then !pos + get code !pos - 1
         else !pos + 32)
    in
    let t = Char.code (Bytes.get code !e) in
    if
      Int.equal t Opcodes.op_crstar
      || Int.equal t Opcodes.op_crminstar
      || Int.equal t Opcodes.op_crquery
      || Int.equal t Opcodes.op_crminquery
      || Int.equal t Opcodes.op_crposstar
      || Int.equal t Opcodes.op_crposquery
    then (
      (* pcre2_auto_possess.c:484-492 *)
      list.(1) <- 1;
      incr e)
    else if
      Int.equal t Opcodes.op_crplus
      || Int.equal t Opcodes.op_crminplus
      || Int.equal t Opcodes.op_crposplus
    then (* pcre2_auto_possess.c:494-498 *)
      incr e
    else if
      Int.equal t Opcodes.op_crrange
      || Int.equal t Opcodes.op_crminrange
      || Int.equal t Opcodes.op_crposrange
    then (
      (* pcre2_auto_possess.c:500-504 *)
      list.(1) <- (if Int.equal (get2 code (!e + 1)) 0 then 1 else 0);
      e := !e + 1 + (2 * Limits.imm2_size));
    list.(2) <- !e - !pos;
    !e)
  else -1 (* pcre2_auto_possess.c:511 — opcode not accepted *)

(* pcre2_auto_possess.c:516-1114 — compare_opcodes: checks whether the
   base and the current opcode have a common character, in which case the
   base cannot be possessified. Returns TRUE if auto-possessification is
   possible. base_list.(1) holds whether the base has a GREEDY quantifier
   (unlike other lists, where slot 1 means "matches empty"). The C's
   `cb` is the [~had_recurse] flag plus the Chartables module (module
   DEVIATION note). *)
let rec compare_opcodes (code : Bytes.t) (start : int) ~(utf : bool)
    ~(ucp : bool) ~(had_recurse : bool) (base_list : int array) (base_end : int)
    (rec_limit : int ref) : bool =
  (* pcre2_auto_possess.c:554 — recursion has gone too deep. *)
  decr rec_limit;
  if !rec_limit <= 0 then false
  else
    let list = Array.make 8 0 in
    (* pcre2_auto_possess.c:561 — for(;;): all operations move the code
       pointer forward, so infinite recursions are not possible. *)
    let rec walk (pos : int) (entered_a_group : bool) : bool =
      let c = Char.code (Bytes.get code pos) in
      (* pcre2_auto_possess.c:570-582 — skip over callouts. *)
      if Int.equal c Opcodes.op_callout then
        (walk [@tailcall]) (pos + Opcodes.op_lengths.(c)) entered_a_group
      else if Int.equal c Opcodes.op_callout_str then
        (walk [@tailcall])
          (pos + get code (pos + 1 + (2 * Limits.link_size)))
          entered_a_group
      else
        (* pcre2_auto_possess.c:584-590 — at the end of a branch, skip to
           the end of the group. *)
        let pos =
          if Int.equal c Opcodes.op_alt then (
            let p = ref pos in
            let scanning = ref true in
            while !scanning do
              p := !p + get code (!p + 1);
              if not (Int.equal (Char.code (Bytes.get code !p)) Opcodes.op_alt)
              then scanning := false
            done;
            !p)
          else pos
        in
        let c = Char.code (Bytes.get code pos) in
        (* pcre2_auto_possess.c:594-714 — switch (c). *)
        if Int.equal c Opcodes.op_end then
          (* pcre2_auto_possess.c:600-601 — a greedy iterator at the end
             of the pattern can always be possessified; a non-greedy one
             never. *)
          not (Int.equal base_list.(1) 0)
        else if Int.equal c Opcodes.op_ket || Int.equal c Opcodes.op_ketrpos
        then
          (* pcre2_auto_possess.c:611-666 — inspect what follows certain
             kinds of group. (OP_KETRMAX/OP_KETRMIN are not handled here:
             they reach the property-list lookup below and fail as
             unsupported.) *)
          if Int.equal base_list.(1) 0 then
            (* the non-greedy case cannot be converted *)
            false
          else
            let bracode = pos - get code (pos + 1) in
            let b = Char.code (Bytes.get code bracode) in
            if
              (Int.equal b Opcodes.op_cbra
              || Int.equal b Opcodes.op_scbra
              || Int.equal b Opcodes.op_cbrapos
              || Int.equal b Opcodes.op_scbrapos)
              && had_recurse
            then
              (* pcre2_auto_possess.c:625-629 — a capturing bracket might
                 be referenced by an OP_RECURSE. *)
              false
            else if
              Int.equal b Opcodes.op_script_run
              && (not (Int.equal base_list.(0) Opcodes.op_char))
              && not (Int.equal base_list.(0) Opcodes.op_chari)
            then
              (* pcre2_auto_possess.c:636-639 — a script run might have
                 to backtrack unless repeating an explicit character. *)
              false
            else if
              Int.equal b Opcodes.op_assert
              || Int.equal b Opcodes.op_assert_not
              || Int.equal b Opcodes.op_once
            then
              (* pcre2_auto_possess.c:646-649 — atomic sub-patterns and
                 assertions can possessify their last iterator, unless
                 the group was entered from a previous iterator. *)
              not entered_a_group
            else if
              Int.equal b Opcodes.op_assertback
              || Int.equal b Opcodes.op_assertback_not
            then
              (* pcre2_auto_possess.c:651-653 — except variable length
                 lookbehinds. *)
              if
                Int.equal
                  (Char.code (Bytes.get code (bracode + 1 + Limits.link_size)))
                  Opcodes.op_vreverse
              then false
              else not entered_a_group
            else if
              Int.equal b Opcodes.op_assert_na
              || Int.equal b Opcodes.op_assertback_na
            then
              (* pcre2_auto_possess.c:658-660 — non-atomic assertions:
                 don't possessify the last iterator. *)
              false
            else
              (* pcre2_auto_possess.c:663-666 — skip over the bracket and
                 inspect what comes next. *)
              (walk [@tailcall]) (pos + Opcodes.op_lengths.(c)) entered_a_group
        else if
          Int.equal c Opcodes.op_once
          || Int.equal c Opcodes.op_bra
          || Int.equal c Opcodes.op_cbra
        then (
          (* pcre2_auto_possess.c:670-687 — the next item is a group:
             check each branch, recursing a level for all but the last. *)
          let next_code = ref (pos + get code (pos + 1)) in
          let cur = ref (pos + Opcodes.op_lengths.(c)) in
          let ok = ref true in
          while
            !ok
            && Int.equal (Char.code (Bytes.get code !next_code)) Opcodes.op_alt
          do
            if
              not
                (compare_opcodes code !cur ~utf ~ucp ~had_recurse base_list
                   base_end rec_limit)
            then ok := false
            else (
              cur := !next_code + 1 + Limits.link_size;
              next_code := !next_code + get code (!next_code + 1))
          done;
          if not !ok then false else (walk [@tailcall]) !cur true)
        else if
          Int.equal c Opcodes.op_brazero || Int.equal c Opcodes.op_braminzero
        then (
          (* pcre2_auto_possess.c:690-707 *)
          let next_code = pos + 1 in
          let nb = Char.code (Bytes.get code next_code) in
          if
            (not (Int.equal nb Opcodes.op_bra))
            && (not (Int.equal nb Opcodes.op_cbra))
            && not (Int.equal nb Opcodes.op_once)
          then false
          else
            let nc = ref next_code in
            let scanning = ref true in
            while !scanning do
              nc := !nc + get code (!nc + 1);
              if not (Int.equal (Char.code (Bytes.get code !nc)) Opcodes.op_alt)
              then scanning := false
            done;
            (* pcre2_auto_possess.c:699-704 — the bracket content will be
               checked by the OP_BRA/OP_CBRA case above. *)
            let nc = !nc + 1 + Limits.link_size in
            if
              not
                (compare_opcodes code nc ~utf ~ucp ~had_recurse base_list
                   base_end rec_limit)
            then false
            else
              (walk [@tailcall]) (pos + Opcodes.op_lengths.(c)) entered_a_group)
        else
          (* pcre2_auto_possess.c:716-720 — load the next opcode's
             properties; fail if unsupported. *)
          let new_pos = get_chr_property_list code pos ~utf ~ucp list in
          if Int.equal new_pos (-1) then false
          else if
            (* pcre2_auto_possess.c:722-734 — if either opcode is a small
               character list, set up for comparing characters from that
               list with the other side. *)
            Int.equal base_list.(0) Opcodes.op_char
            || Int.equal list.(0) Opcodes.op_char
          then
            let chr_arr, list_ptr, list_ptr_is_list =
              if Int.equal base_list.(0) Opcodes.op_char then
                (base_list, list, true)
              else (list, base_list, false)
            in
            (* the C's `list_ptr == list ? code : base_end` idiom: the
               code end matching list_ptr's opcode. *)
            let list_ptr_end = if list_ptr_is_list then new_pos else base_end in
            (* pcre2_auto_possess.c:961-1105 — all characters of the small
               list are checked against the other side. Each arm sets
               "conflict" where the C returns FALSE. *)
            let rec chr_loop (ci : int) : bool =
              let chr = chr_arr.(ci) in
              let lp0 = list_ptr.(0) in
              let conflict =
                if Int.equal lp0 Opcodes.op_char then
                  (* pcre2_auto_possess.c:970-977 *)
                  let rec scan (i : int) : bool =
                    if Int.equal chr list_ptr.(i) then true
                    else if Int.equal list_ptr.(i + 1) Tables.notachar then
                      false
                    else (scan [@tailcall]) (i + 1)
                  in
                  scan 2
                else if Int.equal lp0 Opcodes.op_not then
                  (* pcre2_auto_possess.c:980-989 — conflict when chr is
                     NOT in the list. *)
                  let rec scan (i : int) : bool =
                    if Int.equal chr list_ptr.(i) then false
                    else if Int.equal list_ptr.(i + 1) Tables.notachar then true
                    else (scan [@tailcall]) (i + 1)
                  in
                  scan 2
                else if Int.equal lp0 Opcodes.op_digit then
                  (* pcre2_auto_possess.c:995-996 *)
                  chr < 256
                  && not
                       (Int.equal
                          (Chartables.ctypes chr land Chartables.ctype_digit)
                          0)
                else if Int.equal lp0 Opcodes.op_not_digit then
                  (* pcre2_auto_possess.c:999-1000 *)
                  chr > 255
                  || Int.equal
                       (Chartables.ctypes chr land Chartables.ctype_digit)
                       0
                else if Int.equal lp0 Opcodes.op_whitespace then
                  (* pcre2_auto_possess.c:1003-1004 *)
                  chr < 256
                  && not
                       (Int.equal
                          (Chartables.ctypes chr land Chartables.ctype_space)
                          0)
                else if Int.equal lp0 Opcodes.op_not_whitespace then
                  (* pcre2_auto_possess.c:1007-1008 *)
                  chr > 255
                  || Int.equal
                       (Chartables.ctypes chr land Chartables.ctype_space)
                       0
                else if Int.equal lp0 Opcodes.op_wordchar then
                  (* pcre2_auto_possess.c:1011-1012 — the C tests
                     chr < 255 here (not 256); copied literally. *)
                  chr < 255
                  && not
                       (Int.equal
                          (Chartables.ctypes chr land Chartables.ctype_word)
                          0)
                else if Int.equal lp0 Opcodes.op_not_wordchar then
                  (* pcre2_auto_possess.c:1015-1016 *)
                  chr > 255
                  || Int.equal
                       (Chartables.ctypes chr land Chartables.ctype_word)
                       0
                else if Int.equal lp0 Opcodes.op_hspace then
                  (* pcre2_auto_possess.c:1019-1025 *)
                  Xclass.hspace_char chr
                else if Int.equal lp0 Opcodes.op_not_hspace then
                  (* pcre2_auto_possess.c:1027-1033 *)
                  not (Xclass.hspace_char chr)
                else if
                  Int.equal lp0 Opcodes.op_anynl
                  || Int.equal lp0 Opcodes.op_vspace
                then (* pcre2_auto_possess.c:1035-1042 *)
                  Xclass.vspace_char chr
                else if Int.equal lp0 Opcodes.op_not_vspace then
                  (* pcre2_auto_possess.c:1044-1050 *)
                  not (Xclass.vspace_char chr)
                else if
                  Int.equal lp0 Opcodes.op_doll || Int.equal lp0 Opcodes.op_eodn
                then
                  (* pcre2_auto_possess.c:1052-1067 — CR LF VT FF NEL and
                     the Unicode line separators. *)
                  Int.equal chr 0x0d || Int.equal chr 0x0a || Int.equal chr 0x0b
                  || Int.equal chr 0x0c || Int.equal chr 0x85
                  || Int.equal chr 0x2028 || Int.equal chr 0x2029
                else if Int.equal lp0 Opcodes.op_eod then
                  (* pcre2_auto_possess.c:1069-1070 — can always
                     possessify before \z. *)
                  false
                else if
                  Int.equal lp0 Opcodes.op_prop
                  || Int.equal lp0 Opcodes.op_notprop
                then
                  (* pcre2_auto_possess.c:1073-1078 *)
                  not
                    (check_char_prop chr list_ptr.(2) list_ptr.(3)
                       ~negated:(Int.equal lp0 Opcodes.op_notprop))
                else if Int.equal lp0 Opcodes.op_nclass && chr > 255 then
                  (* pcre2_auto_possess.c:1081-1082 *)
                  true
                else if
                  Int.equal lp0 Opcodes.op_nclass
                  || Int.equal lp0 Opcodes.op_class
                then
                  (* pcre2_auto_possess.c:1085-1090 — fallthrough from
                     OP_NCLASS in C (chr <= 255 here for NCLASS). *)
                  if chr > 255 then false
                  else
                    let class_bitset = list_ptr_end - list_ptr.(2) in
                    not
                      (Int.equal
                         (Char.code
                            (Bytes.get code (class_bitset + (chr lsr 3)))
                         land (1 lsl (chr land 7)))
                         0)
                else if Int.equal lp0 Opcodes.op_xclass then
                  (* pcre2_auto_possess.c:1093-1096 *)
                  Xclass.xclass chr code
                    (list_ptr_end - list_ptr.(2) + Limits.link_size)
                    utf
                else (* pcre2_auto_possess.c:1099-1100 — default *)
                  true
              in
              if conflict then false
              else
                (* pcre2_auto_possess.c:1103-1105 — chr_ptr++; loop while
                   *chr_ptr != NOTACHAR. *)
                let ci = ci + 1 in
                if not (Int.equal chr_arr.(ci) Tables.notachar) then
                  (chr_loop [@tailcall]) ci
                else if
                  (* pcre2_auto_possess.c:1107-1109 — at least one
                     character must be matched from this opcode. *)
                  Int.equal list.(1) 0
                then true
                else (walk [@tailcall]) new_pos entered_a_group
            in
            chr_loop 2
          else if
            (* pcre2_auto_possess.c:736-743 — character bitsets can also
               be compared to certain opcodes; in the 8-bit non-UTF case
               OP_CLASS and OP_NCLASS are the same. *)
            Int.equal base_list.(0) Opcodes.op_class
            || Int.equal list.(0) Opcodes.op_class
            || (not utf)
               && (Int.equal base_list.(0) Opcodes.op_nclass
                  || Int.equal list.(0) Opcodes.op_nclass)
          then
            (* pcre2_auto_possess.c:745-758 *)
            let set1, list_ptr, list_ptr_is_list =
              if
                Int.equal base_list.(0) Opcodes.op_class
                || ((not utf) && Int.equal base_list.(0) Opcodes.op_nclass)
              then (base_end - base_list.(2), list, true)
              else (new_pos - list.(2), base_list, false)
            in
            let list_ptr_end = if list_ptr_is_list then new_pos else base_end in
            (* pcre2_auto_possess.c:809-832 — the byte-by-byte overlap
               scan shared by every set2 source; set2 lives either in the
               compiled code or in the cbits table. *)
            let bitset_overlap (set2 : int) ~(set2_in_code : bool)
                ~(invert_bits : bool) : bool =
              let conflict = ref false in
              for i = 0 to 31 do
                if not !conflict then
                  let b1 = Char.code (Bytes.get code (set1 + i)) in
                  let b2 =
                    if set2_in_code then Char.code (Bytes.get code (set2 + i))
                    else Chartables.cbits (set2 + i)
                  in
                  let b2 = if invert_bits then lnot b2 land 0xff else b2 in
                  if not (Int.equal (b1 land b2) 0) then conflict := true
              done;
              !conflict
            in
            let finish (set2 : int) ~(set2_in_code : bool) ~(invert_bits : bool)
                : bool =
              if bitset_overlap set2 ~set2_in_code ~invert_bits then false
              else if
                (* pcre2_auto_possess.c:830-832 — might be an empty
                   repeat. *)
                Int.equal list.(1) 0
              then true
              else (walk [@tailcall]) new_pos entered_a_group
            in
            (* pcre2_auto_possess.c:760-807 — switch (list_ptr[0]). *)
            let lp0 = list_ptr.(0) in
            if Int.equal lp0 Opcodes.op_class || Int.equal lp0 Opcodes.op_nclass
            then
              finish
                (list_ptr_end - list_ptr.(2))
                ~set2_in_code:true ~invert_bits:false
            else if Int.equal lp0 Opcodes.op_xclass then
              (* pcre2_auto_possess.c:770-781 *)
              let xclass_flags =
                list_ptr_end - list_ptr.(2) + Limits.link_size
              in
              let flags = Char.code (Bytes.get code xclass_flags) in
              if not (Int.equal (flags land Opcodes.xcl_hasprop) 0) then false
              else if Int.equal (flags land Opcodes.xcl_map) 0 then
                if
                  (* no bits are set for characters < 256 *)
                  Int.equal list.(1) 0
                then Int.equal (flags land Opcodes.xcl_not) 0
                else
                  (* might be an empty repeat *)
                  (walk [@tailcall]) new_pos entered_a_group
              else
                finish (xclass_flags + 1) ~set2_in_code:true ~invert_bits:false
            else if Int.equal lp0 Opcodes.op_not_digit then
              (* pcre2_auto_possess.c:784-789 — fall through to OP_DIGIT
                 with inverted bits. *)
              finish Chartables.cbit_digit ~set2_in_code:false ~invert_bits:true
            else if Int.equal lp0 Opcodes.op_digit then
              finish Chartables.cbit_digit ~set2_in_code:false
                ~invert_bits:false
            else if Int.equal lp0 Opcodes.op_not_whitespace then
              (* pcre2_auto_possess.c:791-796 *)
              finish Chartables.cbit_space ~set2_in_code:false ~invert_bits:true
            else if Int.equal lp0 Opcodes.op_whitespace then
              finish Chartables.cbit_space ~set2_in_code:false
                ~invert_bits:false
            else if Int.equal lp0 Opcodes.op_not_wordchar then
              (* pcre2_auto_possess.c:798-803 *)
              finish Chartables.cbit_word ~set2_in_code:false ~invert_bits:true
            else if Int.equal lp0 Opcodes.op_wordchar then
              finish Chartables.cbit_word ~set2_in_code:false ~invert_bits:false
            else (* pcre2_auto_possess.c:805-806 — default *)
              false
          else
            (* pcre2_auto_possess.c:835-959 — some property combinations
               are also acceptable: Unicode property opcodes specially,
               the rest via autoposstab. *)
            let leftop = base_list.(0) and rightop = list.(0) in
            let accepted =
              if
                Int.equal leftop Opcodes.op_prop
                || Int.equal leftop Opcodes.op_notprop
              then
                if Int.equal rightop Opcodes.op_eod then true
                else if
                  Int.equal rightop Opcodes.op_prop
                  || Int.equal rightop Opcodes.op_notprop
                then
                  let same = Int.equal leftop rightop in
                  let lisprop = Int.equal leftop Opcodes.op_prop in
                  let risprop = Int.equal rightop Opcodes.op_prop in
                  let bothprop = lisprop && risprop in
                  (* pcre2_auto_possess.c:869 — the combination table. *)
                  let n = propposstab.(base_list.(2)).(list.(2)) in
                  match n with
                  | 0 -> false
                  | 1 -> bothprop
                  | 2 ->
                      (* (base_list[3] == list[3]) != same *)
                      not (Bool.equal (Int.equal base_list.(3) list.(3)) same)
                  | 3 -> not same
                  | 4 ->
                      (* left general category, right particular
                         category *)
                      risprop
                      && Int.equal
                           catposstab.(base_list.(3)).(list.(3))
                           (Bool.to_int same)
                  | 5 ->
                      (* right general category, left particular
                         category *)
                      lisprop
                      && Int.equal
                           catposstab.(list.(3)).(base_list.(3))
                           (Bool.to_int same)
                  | 6 | 7 | 8 ->
                      (* pcre2_auto_possess.c:885-912 — left
                         alphanum/space/word vs right general category.
                         This code is logically tricky: the first two
                         posspropstab entries always apply; the third is
                         a general category that includes one or more
                         relevant characters and cannot be used in a
                         NOTPROP case. *)
                      let p = posspropstab.(n - 6) in
                      risprop
                      && Bool.equal lisprop
                           ((not (Int.equal list.(3) p.(0)))
                           && (not (Int.equal list.(3) p.(1)))
                           && ((not (Int.equal list.(3) p.(2))) || not lisprop)
                           )
                  | 9 | 10 | 11 ->
                      (* right alphanum/space/word vs left general
                         category *)
                      let p = posspropstab.(n - 9) in
                      lisprop
                      && Bool.equal risprop
                           ((not (Int.equal base_list.(3) p.(0)))
                           && (not (Int.equal base_list.(3) p.(1)))
                           && ((not (Int.equal base_list.(3) p.(2)))
                              || not risprop))
                  | 12 | 13 | 14 ->
                      (* left alphanum/space/word vs right particular
                         category *)
                      let p = posspropstab.(n - 12) in
                      risprop
                      && Bool.equal lisprop
                           ((not (Int.equal catposstab.(p.(0)).(list.(3)) 0))
                           && (not (Int.equal catposstab.(p.(1)).(list.(3)) 0))
                           && ((not (Int.equal list.(3) p.(3))) || not lisprop)
                           )
                  | 15 | 16 | 17 ->
                      (* right alphanum/space/word vs left particular
                         category *)
                      let p = posspropstab.(n - 15) in
                      lisprop
                      && Bool.equal risprop
                           ((not
                               (Int.equal catposstab.(p.(0)).(base_list.(3)) 0))
                           && (not
                                 (Int.equal
                                    catposstab.(p.(1)).(base_list.(3))
                                    0))
                           && ((not (Int.equal base_list.(3) p.(3)))
                              || not risprop))
                  | _ -> false
                else false
              else
                (* pcre2_auto_possess.c:950-952 *)
                leftop >= Opcodes.first_autotab_op
                && leftop <= Opcodes.last_autotab_left_op
                && rightop >= Opcodes.first_autotab_op
                && rightop <= Opcodes.last_autotab_right_op
                && not
                     (Int.equal
                        autoposstab.(leftop - Opcodes.first_autotab_op).(rightop
                                                                         - Opcodes
                                                                           .first_autotab_op)
                        0)
            in
            if not accepted then false
            else if Int.equal list.(1) 0 then true
            else
              (* might be an empty repeat *)
              (walk [@tailcall]) new_pos entered_a_group
    in
    (* pcre2_auto_possess.c:552 — BOOL entered_a_group = FALSE. *)
    walk start false

(* pcre2_auto_possess.c:1118-1369 — PRIV(auto_possessify): replaces single
   character iterations with their possessive alternatives if appropriate,
   modifying the compiled code in place. Hitting a non-existent opcode
   (which can be caused by a bad UTF string compiled with
   PCRE2_NO_UTF_CHECK) returns -1; the rec_limit stops overly complicated
   patterns, leaving the remainder unpossessified. Returns 0 for
   success. *)
let auto_possessify (code : Bytes.t) ~(external_options : int)
    ~(had_recurse : bool) : int =
  let list = Array.make 8 0 in
  (* pcre2_auto_possess.c:1144 — was 10,000 but clang+ASAN uses a lot of
     stack. *)
  let rec_limit = ref 1000 in
  let utf = not (Int.equal (external_options land Options.utf) 0) in
  let ucp = not (Int.equal (external_options land Options.ucp) 0) in
  let result = ref 0 in
  let finished = ref false in
  let pos = ref 0 in
  while not !finished do
    let c = ref (Char.code (Bytes.get code !pos)) in
    if !c >= Opcodes.op_table_length then (
      (* pcre2_auto_possess.c:1152 — something gone wrong. *)
      result := -1;
      finished := true)
    else (
      if !c >= Opcodes.op_star && !c <= Opcodes.op_typeposupto then (
        (* pcre2_auto_possess.c:1154-1200 — a single-character repeat:
           possessify it when what follows cannot match. *)
        let cn = !c - (get_repeat_base !c - Opcodes.op_star) in
        let e =
          if cn <= Opcodes.op_minupto then
            get_chr_property_list code !pos ~utf ~ucp list
          else -1
        in
        list.(1) <-
          (if
             Int.equal cn Opcodes.op_star
             || Int.equal cn Opcodes.op_plus
             || Int.equal cn Opcodes.op_query
             || Int.equal cn Opcodes.op_upto
           then 1
           else 0);
        (if
           (not (Int.equal e (-1)))
           && compare_opcodes code e ~utf ~ucp ~had_recurse list e rec_limit
         then
           (* pcre2_auto_possess.c:1164-1197 — *code += OP_POS... - OP_...
              (the delta maps within the opcode's own family). *)
           let delta =
             if Int.equal cn Opcodes.op_star then
               Opcodes.op_posstar - Opcodes.op_star
             else if Int.equal cn Opcodes.op_minstar then
               Opcodes.op_posstar - Opcodes.op_minstar
             else if Int.equal cn Opcodes.op_plus then
               Opcodes.op_posplus - Opcodes.op_plus
             else if Int.equal cn Opcodes.op_minplus then
               Opcodes.op_posplus - Opcodes.op_minplus
             else if Int.equal cn Opcodes.op_query then
               Opcodes.op_posquery - Opcodes.op_query
             else if Int.equal cn Opcodes.op_minquery then
               Opcodes.op_posquery - Opcodes.op_minquery
             else if Int.equal cn Opcodes.op_upto then
               Opcodes.op_posupto - Opcodes.op_upto
             else if Int.equal cn Opcodes.op_minupto then
               Opcodes.op_posupto - Opcodes.op_minupto
             else 0
           in
           Bytes.set code !pos
             (Char.chr (Char.code (Bytes.get code !pos) + delta)));
        c := Char.code (Bytes.get code !pos))
      else if
        Int.equal !c Opcodes.op_class
        || Int.equal !c Opcodes.op_nclass
        || Int.equal !c Opcodes.op_xclass
      then (
        (* pcre2_auto_possess.c:1201-1249 — a class followed by a CR*
           repeat opcode. *)
        let repeat_opcode =
          if Int.equal !c Opcodes.op_xclass then !pos + get code (!pos + 1)
          else !pos + 1 + 32
        in
        let rc = Char.code (Bytes.get code repeat_opcode) in
        if rc >= Opcodes.op_crstar && rc <= Opcodes.op_crminrange then (
          (* pcre2_auto_possess.c:1213-1218 — the return will never be
             NULL for the three class opcodes, but the C keeps a check
             (for gcc -fanalyzer); kept here too. *)
          let e = get_chr_property_list code !pos ~utf ~ucp list in
          (* even CR opcodes are the greedy ones *)
          list.(1) <- (if Int.equal (rc land 1) 0 then 1 else 0);
          if
            (not (Int.equal e (-1)))
            && compare_opcodes code e ~utf ~ucp ~had_recurse list e rec_limit
          then
            (* pcre2_auto_possess.c:1224-1245 *)
            let np =
              if
                Int.equal rc Opcodes.op_crstar
                || Int.equal rc Opcodes.op_crminstar
              then Opcodes.op_crposstar
              else if
                Int.equal rc Opcodes.op_crplus
                || Int.equal rc Opcodes.op_crminplus
              then Opcodes.op_crposplus
              else if
                Int.equal rc Opcodes.op_crquery
                || Int.equal rc Opcodes.op_crminquery
              then Opcodes.op_crposquery
              else Opcodes.op_crposrange
            in
            Bytes.set code repeat_opcode (Char.chr np));
        c := Char.code (Bytes.get code !pos));
      (* pcre2_auto_possess.c:1251-1293 — switch (c): variable-length
         skips before the fixed length is added. *)
      if Int.equal !c Opcodes.op_end then (
        result := 0;
        finished := true)
      else (
        if
          (!c >= Opcodes.op_typestar && !c <= Opcodes.op_typeminquery)
          || (!c >= Opcodes.op_typeposstar && !c <= Opcodes.op_typeposquery)
        then (
          if
            (* pcre2_auto_possess.c:1256-1266 *)
            Int.equal (Char.code (Bytes.get code (!pos + 1))) Opcodes.op_prop
            || Int.equal
                 (Char.code (Bytes.get code (!pos + 1)))
                 Opcodes.op_notprop
          then pos := !pos + 2)
        else if
          (!c >= Opcodes.op_typeupto && !c <= Opcodes.op_typeexact)
          || Int.equal !c Opcodes.op_typeposupto
        then (
          if
            (* pcre2_auto_possess.c:1268-1274 *)
            Int.equal
              (Char.code (Bytes.get code (!pos + 1 + Limits.imm2_size)))
              Opcodes.op_prop
            || Int.equal
                 (Char.code (Bytes.get code (!pos + 1 + Limits.imm2_size)))
                 Opcodes.op_notprop
          then pos := !pos + 2)
        else if Int.equal !c Opcodes.op_callout_str then
          (* pcre2_auto_possess.c:1276-1278 *)
          pos := !pos + get code (!pos + 1 + (2 * Limits.link_size))
        else if Int.equal !c Opcodes.op_xclass then
          (* pcre2_auto_possess.c:1281-1283 *)
          pos := !pos + get code (!pos + 1)
        else if
          Int.equal !c Opcodes.op_mark
          || Int.equal !c Opcodes.op_commit_arg
          || Int.equal !c Opcodes.op_prune_arg
          || Int.equal !c Opcodes.op_skip_arg
          || Int.equal !c Opcodes.op_then_arg
        then
          (* pcre2_auto_possess.c:1286-1292 *)
          pos := !pos + Char.code (Bytes.get code (!pos + 1));
        (* pcre2_auto_possess.c:1295-1297 — add in the fixed length from
           the table. *)
        pos := !pos + Opcodes.op_lengths.(!c);
        (* pcre2_auto_possess.c:1299-1364 — MAYBE_UTF_MULTI: opcodes
           followed by a character may be followed by a multi-byte
           character; the table length is a minimum. The C's 56 listed
           cases are exactly OP_CHAR..OP_NOTPOSUPTOI (29..84): the four
           single chars plus all four 13-opcode repeat families. *)
        if utf && !c >= Opcodes.op_char && !c <= Opcodes.op_notposuptoi then
          let last = Char.code (Bytes.get code (!pos - 1)) in
          if Utf.has_extralen last then pos := !pos + Utf.get_extralen last))
  done;
  !result
