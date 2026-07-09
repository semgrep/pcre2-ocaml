(* Study pass for the pure-OCaml PCRE2 10.44 port (8-bit library).

   Ported from vendor/pcre2/src/pcre2_study.c (whole file, 1-1915):
   scanning a compiled pattern to collect data that speeds up matching —
   the bitmap of possible starting code units (set_start_bits and its
   helpers) and the minimum subject length (find_minlength) — plus the
   PRIV(study) driver that records the results in the compiled pattern.

   Pointer mapping: the C's PCRE2_SPTR/PCRE2_UCHAR* pointers into the
   compiled program become int offsets into the code [Bytes.t]; NULL
   returns become -1. codestart is offset 0 of [code] (the C computes it
   past the in-block name table). All reads use [Bytes.get]: every offset
   touched lies inside a complete compiled program (the complete-program
   invariant the interpreter, auto-possessifier and printer already rely
   on).

   DEVIATION (module seam): the C mutates pcre2_real_code fields in
   place. The full compiled-pattern record lives in Compile, which CALLS
   this module, so [re] here is a record holding exactly the fields
   pcre2_study.c reads and writes (same names); Compile builds it around
   the same code/name_table/start_bitmap Bytes.t values and copies the
   scalar results (flags, first_codeunit, minlength) back after the
   call. PRIV(find_bracket) also lives in Compile (pcre2_find_bracket.c
   was ported there); it is passed in as [~find_bracket], per the
   Auto_possess precedent for the Compile dependency cycle. The
   character tables are the Chartables module, per the project-wide
   custom-tables deviation (compile.ml compile_block note). *)

(* pcre2_intmodedep.h:620-644 — the pcre2_real_code fields that
   pcre2_study.c uses (see the DEVIATION note above). Mutable fields are
   the ones PRIV(study) assigns. *)
type re = {
  code : Bytes.t; (* compiled program; offset 0 = the C's codestart *)
  name_table : Bytes.t; (* the name/number table (in-block in the C) *)
  name_entry_size : int; (* size (code units) of name table entries *)
  overall_options : int; (* options after processing the pattern *)
  mutable flags : int; (* various state flags *)
  mutable first_codeunit : int; (* starting code unit *)
  last_codeunit : int; (* this codeunit must be seen *)
  mutable minlength : int; (* minimum length of match *)
  top_backref : int; (* highest numbered back reference *)
  start_bitmap : Bytes.t; (* 32 bytes, shared with Compile.re *)
}

(* pcre2_intmodedep.h:108-109 — GET: fetch a LINK_SIZE = 2 big-endian
   offset. DEVIATION (structural only): duplicated from Compile.get to
   avoid a dependency cycle (Compile calls this module), per the
   Auto_possess precedent. *)
let get (code : Bytes.t) (n : int) : int =
  (Char.code (Bytes.get code n) lsl 8) lor Char.code (Bytes.get code (n + 1))

(* pcre2_intmodedep.h:192-195 — GET2: a 16-bit quantity (repeat counts,
   capture numbers), IMM2_SIZE = 2, same layout as GET (duplicated from
   Compile.get2, see the note on [get]). *)
let get2 (code : Bytes.t) (n : int) : int =
  (Char.code (Bytes.get code n) lsl 8) lor Char.code (Bytes.get code (n + 1))

(* pcre2_internal.h:526-548 — the re->flags bits this file reads/writes
   (names drop the PCRE2_ prefix). DEVIATION (structural only): values
   duplicated from Compile, which owns the full set, to avoid the
   dependency cycle; PCRE2_DUPCAPUSED is Parse.dupcapused as in
   compile.ml. *)
let firstset = 0x0000_0010 (* PCRE2_FIRSTSET: first_codeunit is set *)
let firstcaseless = 0x0000_0020 (* PCRE2_FIRSTCASELESS *)
let firstmapset = 0x0000_0040 (* PCRE2_FIRSTMAPSET: start bitmap is set *)
let lastset = 0x0000_0080 (* PCRE2_LASTSET: last_codeunit is set *)
let startline = 0x0000_0200 (* PCRE2_STARTLINE: start after \n multiline *)
let match_empty = 0x0000_2000 (* PCRE2_MATCH_EMPTY: can match empty *)
let hasaccept = 0x0080_0000 (* PCRE2_HASACCEPT: contains ( *ACCEPT) *)

(* limits.h values the C leans on: INT_MAX (32-bit int) and UINT16_MAX. *)
let int_max = 0x7fffffff
let uint16_max = 0xffff

(* pcre2_study.c:51-53 — the maximum remembered capturing brackets
   minimum. *)
let max_cache_backref = 128

(* pcre2_study.c:55-57 — SET_BIT: set a bit in the starting code unit bit
   map. *)
let set_bit (re : re) (c : int) : unit =
  let i = c lsr 3 in
  Bytes.set re.start_bitmap i
    (Char.chr (Char.code (Bytes.get re.start_bitmap i) lor (1 lsl (c land 7))))

(* re->start_bitmap[i] |= v — the byte-wide OR used by the class-map and
   type-table loops (pcre2_study.c:872-873, 913-914, 1687, 1703). *)
let or_bitmap (re : re) (i : int) (v : int) : unit =
  Bytes.set re.start_bitmap i
    (Char.chr ((Char.code (Bytes.get re.start_bitmap i) lor v) land 0xff))

(* pcre2_study.c:59-61 — returns from set_start_bits(). *)
let ssb_fail = 0
let ssb_done = 1
let ssb_continue = 2
let ssb_unknown = 3
let ssb_toodeep = 4

(* pcre2_intmodedep.h:684-687 — recurse_check, the chain of active groups
   used to catch mutual recursion in find_minlength. *)
type recurse_check = { prev : recurse_check option; group : int }

(* The `for (r = recurses; r != NULL; r = r->prev) if (r->group == cs)
   break;` probe (pcre2_study.c:513-516, 573-574, 659-660): true when
   [cs] is already on the chain. *)
let rec recurse_seen (r : recurse_check option) (cs : int) : bool =
  match r with
  | None -> false
  | Some rc ->
      if Int.equal rc.group cs then true
      else (recurse_seen [@tailcall]) rc.prev cs

(* The pervasive `do cc += GET(cc, 1); while ( *cc == OP_ALT)` idiom
   (e.g. pcre2_study.c:190, 212, 253, 293): advance an offset
   over a group's branches to its final KET. *)
let skip_alts (re : re) (r : int ref) : unit =
  let scanning = ref true in
  while !scanning do
    r := !r + get re.code (!r + 1);
    if not (Int.equal (Char.code (Bytes.get re.code !r)) Opcodes.op_alt) then
      scanning := false
  done

(* ---------- Find the minimum subject length for a group ---------- *)

(* pcre2_study.c:64-762 — find_minlength: scan a parenthesized group and
   compute the minimum length of subject needed to match it. This is a
   lower bound; it does not mean there is a string of that length that
   matches. In UTF mode the result is in characters rather than code
   units. The stored field is 16 bits, so we give up at UINT16_MAX.

   Backreference minimum lengths are cached in [backref_cache] (element 0
   holds the number of the highest set value); this function is called
   only when the highest back reference is <= MAX_CACHE_BACKREF.

   Returns:  the minimum length
             -1 \C in UTF-8 mode, or ( *ACCEPT), or too complicated
             -2 internal error (missing capturing bracket)
             -3 internal error (opcode not listed) *)
let rec find_minlength (re : re)
    ~(find_bracket : Bytes.t -> int -> utf:bool -> int -> int) (code : int)
    (startcode : int) ~(utf : bool) (recurses : recurse_check option)
    (countptr : int ref) (backref_cache : int array) : int =
  let byte (i : int) : int = Char.code (Bytes.get re.code i) in
  (* pcre2_study.c:107-118 — the locals. *)
  let length = ref (-1) in
  let branchlength = ref 0 in
  let prev_cap_recno = ref (-1) in
  let prev_cap_d = ref 0 in
  let prev_recurse_recno = ref (-1) in
  let prev_recurse_d = ref 0 in
  let once_fudge = ref 0 in
  let had_recurse = ref false in
  let dupcapused = not (Int.equal (re.flags land Parse.dupcapused) 0) in
  let nextbranch = ref (code + get re.code (code + 1)) in
  let cc = ref (code + 1 + Limits.link_size) in
  let op0 = byte code in
  (* pcre2_study.c:120-122 — a "could be empty" group has minimum length
     0. *)
  if op0 >= Opcodes.op_sbra && op0 <= Opcodes.op_scond then 0
  else begin
    (* pcre2_study.c:124-126 — skip over capturing bracket number. *)
    if Int.equal op0 Opcodes.op_cbra || Int.equal op0 Opcodes.op_cbrapos then
      cc := !cc + Limits.imm2_size;
    (* pcre2_study.c:128-130 — a large and/or complex regex can take too
       long to process: if (( *countptr)++ > 1000) return -1. *)
    let count = !countptr in
    incr countptr;
    if count > 1000 then -1
    else begin
      (* pcre2_study.c:132-760 — scan along the opcodes for this branch.
         The C's for(;;)+switch becomes [loop]; the PROCESS_NON_CAPTURE
         and REPEAT_BACK_REFERENCE forward gotos become the mutually
         recursive functions below (port-conventions §2). *)
      let rec loop () : int =
        (* pcre2_study.c:141-145 — if the accumulated length passes
           16 bits, reset to that value and skip the rest of the
           branch. *)
        if !branchlength >= uint16_max then begin
          branchlength := uint16_max;
          cc := !nextbranch
        end;
        let op = byte !cc in
        if Int.equal op Opcodes.op_cond || Int.equal op Opcodes.op_scond then begin
          (* pcre2_study.c:150-164 — if there is only one branch in a
             condition, the implied branch has zero length, so we don't
             add anything (this covers DEFINE automatically). With two
             branches, treat it as any other non-capturing subpattern. *)
          let cs = !cc + get re.code (!cc + 1) in
          if not (Int.equal (byte cs) Opcodes.op_alt) then begin
            cc := cs + 1 + Limits.link_size;
            (loop [@tailcall]) ()
          end
          else (process_non_capture [@tailcall]) ()
        end
        else if Int.equal op Opcodes.op_bra then begin
          (* pcre2_study.c:166-178 — special case of OP_BRA wrapped round
             a repeated OP_RECURSE: process the latter at this level so
             that remembering the value works for repeated cases; the
             fudge value skips over the OP_KET after the recurse. *)
          if
            Int.equal (byte (!cc + 1 + Limits.link_size)) Opcodes.op_recurse
            && Int.equal
                 (byte (!cc + (2 * (1 + Limits.link_size))))
                 Opcodes.op_ket
          then begin
            once_fudge := 1 + Limits.link_size;
            cc := !cc + 1 + Limits.link_size;
            (loop [@tailcall]) ()
          end
          else (* fallthrough from OP_BRA in C *)
            (process_non_capture [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_once
          || Int.equal op Opcodes.op_script_run
          || Int.equal op Opcodes.op_sbra
          || Int.equal op Opcodes.op_brapos
          || Int.equal op Opcodes.op_sbrapos
        then (process_non_capture [@tailcall]) ()
        else if
          Int.equal op Opcodes.op_cbra
          || Int.equal op Opcodes.op_scbra
          || Int.equal op Opcodes.op_cbrapos
          || Int.equal op Opcodes.op_scbrapos
        then begin
          (* pcre2_study.c:194-214 — to save time for repeated capturing
             subpatterns, remember the length of the previous one. Not
             possible if (?| is present because captures with the same
             number are not then identical. *)
          let recno = get2 re.code (!cc + 1 + Limits.link_size) in
          let bail =
            if dupcapused || not (Int.equal recno !prev_cap_recno) then begin
              prev_cap_recno := recno;
              prev_cap_d :=
                find_minlength re ~find_bracket !cc startcode ~utf recurses
                  countptr backref_cache;
              !prev_cap_d < 0
            end
            else false
          in
          if bail then !prev_cap_d
          else begin
            branchlength := !branchlength + !prev_cap_d;
            skip_alts re cc;
            cc := !cc + 1 + Limits.link_size;
            (loop [@tailcall]) ()
          end
        end
        else if
          (* pcre2_study.c:216-222 — ACCEPT makes things far too
             complicated; give up (retained just in case: from 10.34 this
             function is not used when the pattern contains ( *ACCEPT)). *)
          Int.equal op Opcodes.op_accept
          || Int.equal op Opcodes.op_assert_accept
        then -1
        else if
          Int.equal op Opcodes.op_alt
          || Int.equal op Opcodes.op_ket
          || Int.equal op Opcodes.op_ketrmax
          || Int.equal op Opcodes.op_ketrmin
          || Int.equal op Opcodes.op_ketrpos
          || Int.equal op Opcodes.op_end
        then begin
          (* pcre2_study.c:224-243 — end of a branch: check the length
             against that of the other branches. If the length of any
             branch is zero, there is no need to scan any subsequent
             branches. *)
          if !length < 0 || ((not !had_recurse) && !branchlength < !length)
          then length := !branchlength;
          if (not (Int.equal op Opcodes.op_alt)) || Int.equal !length 0 then
            !length
          else begin
            nextbranch := !cc + get re.code (!cc + 1);
            cc := !cc + 1 + Limits.link_size;
            branchlength := 0;
            had_recurse := false;
            (loop [@tailcall]) ()
          end
        end
        else if
          (* pcre2_study.c:245-254 — skip over assertive subpatterns. *)
          op >= Opcodes.op_assert && op <= Opcodes.op_assertback_na
        then begin
          skip_alts re cc;
          (* fallthrough from the assertions in C: cc +=
             PRIV(OP_lengths)[*cc] reads the KET now under cc. *)
          cc := !cc + Opcodes.op_lengths.(byte !cc);
          (loop [@tailcall]) ()
        end
        else if
          (* pcre2_study.c:256-280 — skip over things that don't match
             chars. *)
          Int.equal op Opcodes.op_reverse
          || Int.equal op Opcodes.op_vreverse
          || Int.equal op Opcodes.op_cref
          || Int.equal op Opcodes.op_dncref
          || Int.equal op Opcodes.op_rref
          || Int.equal op Opcodes.op_dnrref
          || Int.equal op Opcodes.op_false
          || Int.equal op Opcodes.op_true
          || Int.equal op Opcodes.op_callout
          || Int.equal op Opcodes.op_sod
          || Int.equal op Opcodes.op_som
          || Int.equal op Opcodes.op_eod
          || Int.equal op Opcodes.op_eodn
          || Int.equal op Opcodes.op_circ
          || Int.equal op Opcodes.op_circm
          || Int.equal op Opcodes.op_doll
          || Int.equal op Opcodes.op_dollm
          || Int.equal op Opcodes.op_not_word_boundary
          || Int.equal op Opcodes.op_word_boundary
          || Int.equal op Opcodes.op_not_ucp_word_boundary
          || Int.equal op Opcodes.op_ucp_word_boundary
        then begin
          cc := !cc + Opcodes.op_lengths.(op);
          (loop [@tailcall]) ()
        end
        else if Int.equal op Opcodes.op_callout_str then begin
          (* pcre2_study.c:282-284 *)
          cc := !cc + get re.code (!cc + 1 + (2 * Limits.link_size));
          (loop [@tailcall]) ()
        end
        else if
          (* pcre2_study.c:286-295 — skip over a subpattern that has a {0}
             or {0,x} quantifier. *)
          Int.equal op Opcodes.op_brazero
          || Int.equal op Opcodes.op_braminzero
          || Int.equal op Opcodes.op_braposzero
          || Int.equal op Opcodes.op_skipzero
        then begin
          cc := !cc + Opcodes.op_lengths.(op);
          skip_alts re cc;
          cc := !cc + 1 + Limits.link_size;
          (loop [@tailcall]) ()
        end
        else if
          (* pcre2_study.c:297-320 — handle literal characters and +
             repetitions. *)
          (op >= Opcodes.op_char && op <= Opcodes.op_noti)
          || Int.equal op Opcodes.op_plus
          || Int.equal op Opcodes.op_plusi
          || Int.equal op Opcodes.op_minplus
          || Int.equal op Opcodes.op_minplusi
          || Int.equal op Opcodes.op_posplus
          || Int.equal op Opcodes.op_posplusi
          || Int.equal op Opcodes.op_notplus
          || Int.equal op Opcodes.op_notplusi
          || Int.equal op Opcodes.op_notminplus
          || Int.equal op Opcodes.op_notminplusi
          || Int.equal op Opcodes.op_notposplus
          || Int.equal op Opcodes.op_notposplusi
        then begin
          incr branchlength;
          cc := !cc + 2;
          if utf && Utf.has_extralen (byte (!cc - 1)) then
            cc := !cc + Utf.get_extralen (byte (!cc - 1));
          (loop [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_typeplus
          || Int.equal op Opcodes.op_typeminplus
          || Int.equal op Opcodes.op_typeposplus
        then begin
          (* pcre2_study.c:322-327 *)
          incr branchlength;
          let t = byte (!cc + 1) in
          cc :=
            !cc
            + (if Int.equal t Opcodes.op_prop || Int.equal t Opcodes.op_notprop
               then 4
               else 2);
          (loop [@tailcall]) ()
        end
        else if
          (* pcre2_study.c:329-341 — handle exact repetitions. The count
             is already in characters, but we may need to skip over a
             multibyte character in UTF mode. *)
          Int.equal op Opcodes.op_exact
          || Int.equal op Opcodes.op_exacti
          || Int.equal op Opcodes.op_notexact
          || Int.equal op Opcodes.op_notexacti
        then begin
          branchlength := !branchlength + get2 re.code (!cc + 1);
          cc := !cc + 2 + Limits.imm2_size;
          if utf && Utf.has_extralen (byte (!cc - 1)) then
            cc := !cc + Utf.get_extralen (byte (!cc - 1));
          (loop [@tailcall]) ()
        end
        else if Int.equal op Opcodes.op_typeexact then begin
          (* pcre2_study.c:343-347 *)
          branchlength := !branchlength + get2 re.code (!cc + 1);
          let t = byte (!cc + 1 + Limits.imm2_size) in
          cc :=
            !cc + 2 + Limits.imm2_size
            + (if Int.equal t Opcodes.op_prop || Int.equal t Opcodes.op_notprop
               then 2
               else 0);
          (loop [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_prop || Int.equal op Opcodes.op_notprop
        then begin
          (* pcre2_study.c:349-354 — handle single-char non-literal
             matchers; falls through to the single-char group below. *)
          cc := !cc + 2;
          (* fallthrough from OP_PROP/OP_NOTPROP in C *)
          incr branchlength;
          incr cc;
          (loop [@tailcall]) ()
        end
        else if
          (op >= Opcodes.op_not_digit && op <= Opcodes.op_wordchar)
          || Int.equal op Opcodes.op_any
          || Int.equal op Opcodes.op_allany
          || Int.equal op Opcodes.op_extuni
          || (op >= Opcodes.op_not_hspace && op <= Opcodes.op_vspace)
        then begin
          (* pcre2_study.c:356-371 *)
          incr branchlength;
          incr cc;
          (loop [@tailcall]) ()
        end
        else if Int.equal op Opcodes.op_anynl then begin
          (* pcre2_study.c:373-379 — "any newline" might match two
             characters, but it also might match just one. *)
          branchlength := !branchlength + 1;
          incr cc;
          (loop [@tailcall]) ()
        end
        else if Int.equal op Opcodes.op_anybyte then begin
          (* pcre2_study.c:381-391 — the single-byte matcher means we
             can't proceed in UTF mode. *)
          if utf then -1
          else begin
            incr branchlength;
            incr cc;
            (loop [@tailcall]) ()
          end
        end
        else if
          (* pcre2_study.c:393-404 — for repeated character types, test
             for \p and \P, which have an extra two bytes of
             parameters. *)
          Int.equal op Opcodes.op_typestar
          || Int.equal op Opcodes.op_typeminstar
          || Int.equal op Opcodes.op_typequery
          || Int.equal op Opcodes.op_typeminquery
          || Int.equal op Opcodes.op_typeposstar
          || Int.equal op Opcodes.op_typeposquery
        then begin
          let t = byte (!cc + 1) in
          if Int.equal t Opcodes.op_prop || Int.equal t Opcodes.op_notprop
          then cc := !cc + 2;
          cc := !cc + Opcodes.op_lengths.(op);
          (loop [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_typeupto
          || Int.equal op Opcodes.op_typeminupto
          || Int.equal op Opcodes.op_typeposupto
        then begin
          (* pcre2_study.c:406-412 *)
          let t = byte (!cc + 1 + Limits.imm2_size) in
          if Int.equal t Opcodes.op_prop || Int.equal t Opcodes.op_notprop
          then cc := !cc + 2;
          cc := !cc + Opcodes.op_lengths.(op);
          (loop [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_class
          || Int.equal op Opcodes.op_nclass
          || Int.equal op Opcodes.op_xclass
        then begin
          (* pcre2_study.c:414-458 — check a class for variable
             quantification. *)
          if Int.equal op Opcodes.op_xclass then
            cc := !cc + get re.code (!cc + 1)
          else cc := !cc + Opcodes.op_lengths.(Opcodes.op_class);
          let t = byte !cc in
          if
            Int.equal t Opcodes.op_crplus
            || Int.equal t Opcodes.op_crminplus
            || Int.equal t Opcodes.op_crposplus
          then begin
            incr branchlength;
            (* fallthrough from OP_CRPLUS in C *)
            incr cc
          end
          else if
            Int.equal t Opcodes.op_crstar
            || Int.equal t Opcodes.op_crminstar
            || Int.equal t Opcodes.op_crquery
            || Int.equal t Opcodes.op_crminquery
            || Int.equal t Opcodes.op_crposstar
            || Int.equal t Opcodes.op_crposquery
          then incr cc
          else if
            Int.equal t Opcodes.op_crrange
            || Int.equal t Opcodes.op_crminrange
            || Int.equal t Opcodes.op_crposrange
          then begin
            branchlength := !branchlength + get2 re.code (!cc + 1);
            cc := !cc + 1 + (2 * Limits.imm2_size)
          end
          else incr branchlength;
          (loop [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_dnref || Int.equal op Opcodes.op_dnrefi
        then begin
          (* pcre2_study.c:460-543 — duplicate named pattern back
             reference: scan all groups with the same name and find the
             shortest. A recursion (simple or mutual) sets had_recurse so
             the length of this branch is ignored (another alternative
             must stop the recursing and provides the minimum). If
             PCRE2_MATCH_UNSET_BACKREF is set, an unset backreference
             matches an empty string, so the minimum is zero; likewise
             when duplicate group numbers may make the reference
             ambiguous. *)
          let ret = ref 0 in
          let d = ref 0 in
          if
            (not dupcapused)
            && Int.equal
                 (re.overall_options land Options.match_unset_backref)
                 0
          then begin
            let count = ref (get2 re.code (!cc + 1 + Limits.imm2_size)) in
            let slot = ref (get2 re.code (!cc + 1) * re.name_entry_size) in
            d := int_max;
            (* pcre2_study.c:488-539 — while (count-- > 0). *)
            let brk = ref false in
            while (not !brk) && Int.equal !ret 0 && !count > 0 do
              decr count;
              let recno = get2 re.name_table !slot in
              let dd = ref 0 in
              if recno <= backref_cache.(0) && backref_cache.(recno) >= 0
              then dd := backref_cache.(recno)
              else begin
                let cs = find_bracket re.code startcode ~utf recno in
                if cs < 0 then ret := -2
                else begin
                  let ce = ref cs in
                  skip_alts re ce;
                  dd := 0;
                  if
                    (not dupcapused)
                    || find_bracket re.code !ce ~utf recno < 0
                  then begin
                    if !cc > cs && !cc < !ce then
                      (* simple recursion *)
                      had_recurse := true
                    else if recurse_seen recurses cs then
                      (* mutual recursion *)
                      had_recurse := true
                    else begin
                      (* no recursion *)
                      let this_recurse = { prev = recurses; group = cs } in
                      dd :=
                        find_minlength re ~find_bracket cs startcode ~utf
                          (Some this_recurse) countptr backref_cache;
                      if !dd < 0 then ret := !dd
                    end
                  end;
                  if Int.equal !ret 0 then begin
                    (* pcre2_study.c:531-533 *)
                    backref_cache.(recno) <- !dd;
                    for i = backref_cache.(0) + 1 to recno - 1 do
                      backref_cache.(i) <- -1
                    done;
                    backref_cache.(0) <- recno
                  end
                end
              end;
              if Int.equal !ret 0 then begin
                if !dd < !d then d := !dd;
                if !d <= 0 then brk := true
                  (* no point looking at any more *)
                else slot := !slot + re.name_entry_size
              end
            done
          end
          else d := 0;
          if not (Int.equal !ret 0) then !ret
          else begin
            cc := !cc + 1 + (2 * Limits.imm2_size);
            (repeat_back_reference [@tailcall]) !d
          end
        end
        else if Int.equal op Opcodes.op_ref || Int.equal op Opcodes.op_refi
        then begin
          (* pcre2_study.c:545-596 — single back reference by number
             (references by name are converted to by number when there is
             no duplication). *)
          let recno = get2 re.code (!cc + 1) in
          let ret = ref 0 in
          let d = ref 0 in
          if recno <= backref_cache.(0) && backref_cache.(recno) >= 0 then
            d := backref_cache.(recno)
          else begin
            d := 0;
            if
              Int.equal
                (re.overall_options land Options.match_unset_backref)
                0
            then begin
              let cs = find_bracket re.code startcode ~utf recno in
              if cs < 0 then ret := -2
              else begin
                let ce = ref cs in
                skip_alts re ce;
                if
                  (not dupcapused)
                  || find_bracket re.code !ce ~utf recno < 0
                then begin
                  if !cc > cs && !cc < !ce then
                    (* simple recursion *)
                    had_recurse := true
                  else if recurse_seen recurses cs then
                    (* mutual recursion *)
                    had_recurse := true
                  else begin
                    (* no recursion *)
                    let this_recurse = { prev = recurses; group = cs } in
                    d :=
                      find_minlength re ~find_bracket cs startcode ~utf
                        (Some this_recurse) countptr backref_cache;
                    if !d < 0 then ret := !d
                  end
                end
              end
            end;
            if Int.equal !ret 0 then begin
              (* pcre2_study.c:591-593 *)
              backref_cache.(recno) <- !d;
              for i = backref_cache.(0) + 1 to recno - 1 do
                backref_cache.(i) <- -1
              done;
              backref_cache.(0) <- recno
            end
          end;
          if not (Int.equal !ret 0) then !ret
          else begin
            cc := !cc + 1 + Limits.imm2_size;
            (repeat_back_reference [@tailcall]) !d
          end
        end
        else if Int.equal op Opcodes.op_recurse then begin
          (* pcre2_study.c:641-677 — recursion always refers to the first
             occurrence of a subpattern with a given number, so caching
             can always be used. *)
          let cs = startcode + get re.code (!cc + 1) in
          let recno = get2 re.code (cs + 1 + Limits.link_size) in
          let ret = ref 0 in
          if Int.equal recno !prev_recurse_recno then
            branchlength := !branchlength + !prev_recurse_d
          else begin
            let ce = ref cs in
            skip_alts re ce;
            if !cc > cs && !cc < !ce then (* simple recursion *)
              had_recurse := true
            else if recurse_seen recurses cs then (* mutual recursion *)
              had_recurse := true
            else begin
              let this_recurse = { prev = recurses; group = cs } in
              prev_recurse_d :=
                find_minlength re ~find_bracket cs startcode ~utf
                  (Some this_recurse) countptr backref_cache;
              if !prev_recurse_d < 0 then ret := !prev_recurse_d
              else begin
                prev_recurse_recno := recno;
                branchlength := !branchlength + !prev_recurse_d
              end
            end
          end;
          if not (Int.equal !ret 0) then !ret
          else begin
            cc := !cc + 1 + Limits.link_size + !once_fudge;
            once_fudge := 0;
            (loop [@tailcall]) ()
          end
        end
        else if
          (* pcre2_study.c:679-730 — anything else does not or need not
             match a character: get the length from the table; for those
             that can match zero occurrences of a character, take special
             action for UTF-8 characters. *)
          Int.equal op Opcodes.op_upto
          || Int.equal op Opcodes.op_uptoi
          || Int.equal op Opcodes.op_notupto
          || Int.equal op Opcodes.op_notuptoi
          || Int.equal op Opcodes.op_minupto
          || Int.equal op Opcodes.op_minuptoi
          || Int.equal op Opcodes.op_notminupto
          || Int.equal op Opcodes.op_notminuptoi
          || Int.equal op Opcodes.op_posupto
          || Int.equal op Opcodes.op_posuptoi
          || Int.equal op Opcodes.op_notposupto
          || Int.equal op Opcodes.op_notposuptoi
          || Int.equal op Opcodes.op_star
          || Int.equal op Opcodes.op_stari
          || Int.equal op Opcodes.op_notstar
          || Int.equal op Opcodes.op_notstari
          || Int.equal op Opcodes.op_minstar
          || Int.equal op Opcodes.op_minstari
          || Int.equal op Opcodes.op_notminstar
          || Int.equal op Opcodes.op_notminstari
          || Int.equal op Opcodes.op_posstar
          || Int.equal op Opcodes.op_posstari
          || Int.equal op Opcodes.op_notposstar
          || Int.equal op Opcodes.op_notposstari
          || Int.equal op Opcodes.op_query
          || Int.equal op Opcodes.op_queryi
          || Int.equal op Opcodes.op_notquery
          || Int.equal op Opcodes.op_notqueryi
          || Int.equal op Opcodes.op_minquery
          || Int.equal op Opcodes.op_minqueryi
          || Int.equal op Opcodes.op_notminquery
          || Int.equal op Opcodes.op_notminqueryi
          || Int.equal op Opcodes.op_posquery
          || Int.equal op Opcodes.op_posqueryi
          || Int.equal op Opcodes.op_notposquery
          || Int.equal op Opcodes.op_notposqueryi
        then begin
          cc := !cc + Opcodes.op_lengths.(op);
          if utf && Utf.has_extralen (byte (!cc - 1)) then
            cc := !cc + Utf.get_extralen (byte (!cc - 1));
          (loop [@tailcall]) ()
        end
        else if
          (* pcre2_study.c:732-740 — skip these, but add in the name
             length. *)
          Int.equal op Opcodes.op_mark
          || Int.equal op Opcodes.op_commit_arg
          || Int.equal op Opcodes.op_prune_arg
          || Int.equal op Opcodes.op_skip_arg
          || Int.equal op Opcodes.op_then_arg
        then begin
          cc := !cc + Opcodes.op_lengths.(op) + byte (!cc + 1);
          (loop [@tailcall]) ()
        end
        else if
          (* pcre2_study.c:742-752 — the remaining opcodes are just
             skipped over. *)
          Int.equal op Opcodes.op_close
          || Int.equal op Opcodes.op_commit
          || Int.equal op Opcodes.op_fail
          || Int.equal op Opcodes.op_prune
          || Int.equal op Opcodes.op_set_som
          || Int.equal op Opcodes.op_skip
          || Int.equal op Opcodes.op_then
        then begin
          cc := !cc + Opcodes.op_lengths.(op);
          (loop [@tailcall]) ()
        end
        else
          (* pcre2_study.c:754-758 — this should not occur: all opcodes
             are listed explicitly so that new ones get properly
             considered. *)
          -3
      and process_non_capture () : int =
        (* pcre2_study.c:185-192 — PROCESS_NON_CAPTURE: recurse into the
           group, then step past it. *)
        let d =
          find_minlength re ~find_bracket !cc startcode ~utf recurses countptr
            backref_cache
        in
        if d < 0 then d
        else begin
          branchlength := !branchlength + d;
          skip_alts re cc;
          cc := !cc + 1 + Limits.link_size;
          (loop [@tailcall]) ()
        end
      and repeat_back_reference (d : int) : int =
        (* pcre2_study.c:598-630 — REPEAT_BACK_REFERENCE: handle repeated
           back references. *)
        let t = byte !cc in
        let min =
          if
            Int.equal t Opcodes.op_crstar
            || Int.equal t Opcodes.op_crminstar
            || Int.equal t Opcodes.op_crquery
            || Int.equal t Opcodes.op_crminquery
            || Int.equal t Opcodes.op_crposstar
            || Int.equal t Opcodes.op_crposquery
          then begin
            incr cc;
            0
          end
          else if
            Int.equal t Opcodes.op_crplus
            || Int.equal t Opcodes.op_crminplus
            || Int.equal t Opcodes.op_crposplus
          then begin
            incr cc;
            1
          end
          else if
            Int.equal t Opcodes.op_crrange
            || Int.equal t Opcodes.op_crminrange
            || Int.equal t Opcodes.op_crposrange
          then begin
            let m = get2 re.code (!cc + 1) in
            cc := !cc + 1 + (2 * Limits.imm2_size);
            m
          end
          else 1
        in
        (* pcre2_study.c:632-638 — take care not to overflow: (1) min and
           d are (32-bit C) ints, so their product must not exceed
           INT_MAX; (2) branchlength is limited to UINT16_MAX. *)
        if (d > 0 && int_max / d < min) || uint16_max - !branchlength < min * d
        then branchlength := uint16_max
        else branchlength := !branchlength + (min * d);
        (loop [@tailcall]) ()
      in
      loop ()
    end
  end

(* ---------- Set a bit and maybe its alternate case ---------- *)

(* pcre2_study.c:766-845 — set_table_bit: given a character whose first
   code unit is at offset [p] in the compiled code, set that code unit's
   bit in the table, and also the corresponding bit for the other version
   of a letter if we are caseless. Returns the offset after the
   character. *)
let set_table_bit (re : re) (p : int) ~(caseless : bool) ~(utf : bool)
    ~(ucp : bool) : int =
  let c = ref (Char.code (Bytes.get re.code p)) (* first code unit *) in
  let p = ref (p + 1) in
  set_bit re !c;
  (* pcre2_study.c:801-813 — in UTF-8 mode, pick up the remaining code
     units in order to find the end of the character, even when caseless:
     if (c >= 0xc0) GETUTF8INC(c, p). *)
  if utf && !c >= 0xc0 then begin
    let extra = Utf.get_extralen !c in
    c := Utf.getutf8_bytes !c re.code (!p - 1);
    p := !p + extra
  end;
  (* pcre2_study.c:815-842 — if caseless, handle the other case of the
     character. *)
  if caseless then
    if utf || ucp then begin
      c := Ucd.othercase !c;
      if utf then begin
        let buff = Bytes.create 6 in
        ignore (Utf.ord2utf !c buff 0);
        set_bit re (Char.code (Bytes.get buff 0))
      end
      else if !c < 256 then set_bit re !c
    end
    else
      (* Not UTF or UCP; MAX_255(c) is always TRUE in the 8-bit
         library. *)
      set_bit re (Chartables.fcc !c);
  !p

(* ---------- Set bits for a positive character type ---------- *)

(* pcre2_study.c:849-886 — set_type_bits: set starting bits for a
   character type. In UTF-8 mode a direct setting is only possible for
   bytes less than 128 (table_limit = 16); bits from the high half of the
   table are transferred to the bits for their UTF-8 leading bytes. *)
let set_type_bits (re : re) (cbit_type : int) (table_limit : int) : unit =
  for c = 0 to table_limit - 1 do
    or_bitmap re c (Chartables.cbits (c + cbit_type))
  done;
  if not (Int.equal table_limit 32) then
    for c = 128 to 255 do
      (* pcre2_study.c:878 — NOTE: the C reads re->tables[cbits_offset +
         c/8], i.e. WITHOUT adding cbit_type (the row read is always the
         cbit_space one); copied faithfully. *)
      if
        not
          (Int.equal (Chartables.cbits (c lsr 3) land (1 lsl (c land 7))) 0)
      then begin
        let buff = Bytes.create 6 in
        ignore (Utf.ord2utf c buff 0);
        set_bit re (Char.code (Bytes.get buff 0))
      end
    done

(* ---------- Set bits for a negative character type ---------- *)

(* pcre2_study.c:889-918 — set_nottype_bits: set starting bits for a
   negative character type such as \D. Unlike the positive case, the bits
   for all high-valued characters have to be set (starting at 0xc0 for
   simplicity, overkilling below the lowest first byte 0xc2). *)
let set_nottype_bits (re : re) (cbit_type : int) (table_limit : int) : unit =
  for c = 0 to table_limit - 1 do
    or_bitmap re c (lnot (Chartables.cbits (c + cbit_type)) land 0xff)
  done;
  if not (Int.equal table_limit 32) then
    for c = 24 to 31 do
      Bytes.set re.start_bitmap c '\xff'
    done

(* pcre2_study.c:1348-1382 (and the identical 1487-1519) — the OP_HSPACE
   starting bits: HT, SPACE, and either the UTF-8 first bytes of the
   high-valued horizontal space characters or 0xA0 (not EBCDIC). *)
let set_hspace_bits (re : re) ~(utf : bool) : unit =
  set_bit re 0x09 (* CHAR_HT *);
  set_bit re 0x20 (* CHAR_SPACE *);
  if utf then begin
    set_bit re 0xc2 (* for U+00A0 *);
    set_bit re 0xe1 (* for U+1680, U+180E *);
    set_bit re 0xe2 (* for U+2000 - U+200A, U+202F, U+205F *);
    set_bit re 0xe3 (* for U+3000 *)
  end
  else set_bit re 0xa0

(* pcre2_study.c:1384-1416 (and the identical 1521-1551) — the OP_ANYNL /
   OP_VSPACE starting bits: LF, VT, FF, CR, and either the UTF-8 first
   bytes of NEL/U+2028/U+2029 or NEL itself. ANYNL can match the
   two-character CRLF sequence, but that is not relevant for finding the
   first character, so its bits are identical to VSPACE's. *)
let set_vspace_bits (re : re) ~(utf : bool) : unit =
  set_bit re Newline.char_lf;
  set_bit re Newline.char_vt;
  set_bit re Newline.char_ff;
  set_bit re Newline.char_cr;
  if utf then begin
    set_bit re 0xc2 (* for U+0085 (NEL) *);
    set_bit re 0xe2 (* for U+2028, U+2029 *)
  end
  else set_bit re Newline.char_nel

(* pcre2_study.c:997-1071 — the valid opcodes in set_start_bits that
   imply no starting bits (the switch's SSB_FAIL block), listed in the
   C's case order. Entries left false fall to the recognized-opcode
   chain, whose final else is the C's `default: return SSB_UNKNOWN`. *)
let ssb_fail_ops : bool array =
  let t = Array.make Opcodes.op_table_length false in
  List.iter
    (fun op -> t.(op) <- true)
    [
      Opcodes.op_accept;
      Opcodes.op_assert_accept;
      Opcodes.op_allany;
      Opcodes.op_any;
      Opcodes.op_anybyte;
      Opcodes.op_circm;
      Opcodes.op_close;
      Opcodes.op_commit;
      Opcodes.op_commit_arg;
      Opcodes.op_cond;
      Opcodes.op_cref;
      Opcodes.op_false;
      Opcodes.op_true;
      Opcodes.op_dncref;
      Opcodes.op_dnref;
      Opcodes.op_dnrefi;
      Opcodes.op_dnrref;
      Opcodes.op_doll;
      Opcodes.op_dollm;
      Opcodes.op_end;
      Opcodes.op_eod;
      Opcodes.op_eodn;
      Opcodes.op_extuni;
      Opcodes.op_fail;
      Opcodes.op_mark;
      Opcodes.op_not;
      Opcodes.op_notexact;
      Opcodes.op_notexacti;
      Opcodes.op_noti;
      Opcodes.op_notminplus;
      Opcodes.op_notminplusi;
      Opcodes.op_notminquery;
      Opcodes.op_notminqueryi;
      Opcodes.op_notminstar;
      Opcodes.op_notminstari;
      Opcodes.op_notminupto;
      Opcodes.op_notminuptoi;
      Opcodes.op_notplus;
      Opcodes.op_notplusi;
      Opcodes.op_notposplus;
      Opcodes.op_notposplusi;
      Opcodes.op_notposquery;
      Opcodes.op_notposqueryi;
      Opcodes.op_notposstar;
      Opcodes.op_notposstari;
      Opcodes.op_notposupto;
      Opcodes.op_notposuptoi;
      Opcodes.op_notprop;
      Opcodes.op_notquery;
      Opcodes.op_notqueryi;
      Opcodes.op_notstar;
      Opcodes.op_notstari;
      Opcodes.op_notupto;
      Opcodes.op_notuptoi;
      Opcodes.op_not_hspace;
      Opcodes.op_not_vspace;
      Opcodes.op_prune;
      Opcodes.op_prune_arg;
      Opcodes.op_recurse;
      Opcodes.op_ref;
      Opcodes.op_refi;
      Opcodes.op_reverse;
      Opcodes.op_vreverse;
      Opcodes.op_rref;
      Opcodes.op_scond;
      Opcodes.op_set_som;
      Opcodes.op_skip;
      Opcodes.op_skip_arg;
      Opcodes.op_sod;
      Opcodes.op_som;
      Opcodes.op_then;
      Opcodes.op_then_arg;
    ];
  t

(* ---------- Create bitmap of starting code units ---------- *)

(* pcre2_study.c:922-1741 — set_start_bits: scan a compiled unanchored
   expression recursively and attempt to build a bitmap of the set of
   possible starting code units whose values are less than 256. In UTF-8
   mode, set[_not]_type_bits() is given a table limit of 16 rather than
   32 (direct settings only below 128). SSB_CONTINUE is returned for
   parenthesized groups such as (a* )b that provide optional starting
   code units, so that scanning continues at the outer level; at the
   outermost level the caller requires SSB_DONE. Recursion is restricted
   to a depth of 1000.

   Returns:  SSB_FAIL     => Failed to find any starting code units
             SSB_DONE     => Found mandatory starting code units
             SSB_CONTINUE => Found optional starting code units
             SSB_UNKNOWN  => Hit an unrecognized opcode
             SSB_TOODEEP  => Recursion is too deep *)
let rec set_start_bits (re : re) (code : int) ~(utf : bool) ~(ucp : bool)
    (depthptr : int ref) : int =
  let byte (i : int) : int = Char.code (Bytes.get re.code i) in
  let yield = ref ssb_done in
  (* pcre2_study.c:962-966 — table_limit: 16 for UTF-8, else 32. *)
  let table_limit = if utf then 16 else 32 in
  incr depthptr;
  if !depthptr > 1000 then ssb_toodeep
  else begin
    (* pcre2_study.c:971-1738 — the do { ... } while ( *code == OP_ALT)
       over the branches, with the while (try_next) item loop inside. The
       item loop is [item_loop]: it returns an SSB code for the C's early
       returns, or -1 when try_next becomes FALSE (fall to the next
       branch). *)
    let rec branch_loop (code : int) : int =
      let tcode = ref (code + 1 + Limits.link_size) in
      let cop = byte code in
      if
        Int.equal cop Opcodes.op_cbra
        || Int.equal cop Opcodes.op_scbra
        || Int.equal cop Opcodes.op_cbrapos
        || Int.equal cop Opcodes.op_scbrapos
      then tcode := !tcode + Limits.imm2_size;
      let rec item_loop () : int =
        let op = byte !tcode in
        if ssb_fail_ops.(op) then
          (* pcre2_study.c:997-1071 — fail for a valid opcode that
             implies no starting bits. *)
          ssb_fail
        else if Int.equal op Opcodes.op_circ then begin
          (* pcre2_study.c:1073-1078 — OP_CIRC happens only at the start
             of an anchored branch: skip over it. *)
          tcode := !tcode + Opcodes.op_lengths.(Opcodes.op_circ);
          (item_loop [@tailcall]) ()
        end
        else if Int.equal op Opcodes.op_prop then begin
          (* pcre2_study.c:1080-1103 — a "real" property test implies no
             starting bits, but the fake property PT_CLIST identifies a
             list of characters. *)
          if not (Int.equal (byte (!tcode + 1)) Opcodes.pt_clist) then
            ssb_fail
          else begin
            let p = ref (byte (!tcode + 2)) in
            let scanning = ref true in
            while !scanning do
              let c = Ucd_tables.ucd_caseless_sets.(!p) in
              incr p;
              if c < Tables.notachar then begin
                let c =
                  if utf then begin
                    let buff = Bytes.create 6 in
                    ignore (Utf.ord2utf c buff 0);
                    Char.code (Bytes.get buff 0)
                  end
                  else c
                in
                if c > 0xff then set_bit re 0xff else set_bit re c
              end
              else scanning := false
            done;
            (* try_next = FALSE *)
            -1
          end
        end
        else if
          (* pcre2_study.c:1105-1112 — we can ignore word boundary
             tests. *)
          Int.equal op Opcodes.op_word_boundary
          || Int.equal op Opcodes.op_not_word_boundary
          || Int.equal op Opcodes.op_ucp_word_boundary
          || Int.equal op Opcodes.op_not_ucp_word_boundary
        then begin
          incr tcode;
          (item_loop [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_assert || Int.equal op Opcodes.op_assert_na
        then begin
          (* pcre2_study.c:1114-1196 — for a positive lookahead
             assertion, inspect what immediately follows, ignoring
             intermediate assertions and callouts. If the next item sets
             a mandatory character, skip this assertion; otherwise treat
             it the same as other bracket groups. *)
          let ncode = ref (!tcode + get re.code (!tcode + 1)) in
          while Int.equal (byte !ncode) Opcodes.op_alt do
            ncode := !ncode + get re.code (!ncode + 1)
          done;
          ncode := !ncode + 1 + Limits.link_size;
          (* pcre2_study.c:1125-1161 — skip irrelevant items. *)
          let done_ = ref false in
          while not !done_ do
            let nop = byte !ncode in
            if nop >= Opcodes.op_assert && nop <= Opcodes.op_assertback_na
            then begin
              ncode := !ncode + get re.code (!ncode + 1);
              while Int.equal (byte !ncode) Opcodes.op_alt do
                ncode := !ncode + get re.code (!ncode + 1)
              done;
              ncode := !ncode + 1 + Limits.link_size
            end
            else if
              Int.equal nop Opcodes.op_word_boundary
              || Int.equal nop Opcodes.op_not_word_boundary
              || Int.equal nop Opcodes.op_ucp_word_boundary
              || Int.equal nop Opcodes.op_not_ucp_word_boundary
            then incr ncode
            else if Int.equal nop Opcodes.op_callout then
              ncode := !ncode + Opcodes.op_lengths.(Opcodes.op_callout)
            else if Int.equal nop Opcodes.op_callout_str then
              ncode :=
                !ncode + get re.code (!ncode + 1 + (2 * Limits.link_size))
            else done_ := true
          done;
          (* pcre2_study.c:1163-1195 — now check the next significant
             item. *)
          let nop = byte !ncode in
          let significant =
            (Int.equal nop Opcodes.op_prop
            && Int.equal (byte (!ncode + 1)) Opcodes.pt_clist)
            || Int.equal nop Opcodes.op_anynl
            || Int.equal nop Opcodes.op_char
            || Int.equal nop Opcodes.op_chari
            || Int.equal nop Opcodes.op_exact
            || Int.equal nop Opcodes.op_exacti
            || Int.equal nop Opcodes.op_hspace
            || Int.equal nop Opcodes.op_minplus
            || Int.equal nop Opcodes.op_minplusi
            || Int.equal nop Opcodes.op_plus
            || Int.equal nop Opcodes.op_plusi
            || Int.equal nop Opcodes.op_posplus
            || Int.equal nop Opcodes.op_posplusi
            || Int.equal nop Opcodes.op_vspace
            (* these types are only present in non-UCP mode *)
            || Int.equal nop Opcodes.op_digit
            || Int.equal nop Opcodes.op_not_digit
            || Int.equal nop Opcodes.op_wordchar
            || Int.equal nop Opcodes.op_not_wordchar
            || Int.equal nop Opcodes.op_whitespace
            || Int.equal nop Opcodes.op_not_whitespace
          in
          if significant then begin
            tcode := !ncode;
            (* continue with the following significant opcode *)
            (item_loop [@tailcall]) ()
          end
          else (* fallthrough to the bracket group case in C *)
            (bracket_case [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_bra
          || Int.equal op Opcodes.op_sbra
          || Int.equal op Opcodes.op_cbra
          || Int.equal op Opcodes.op_scbra
          || Int.equal op Opcodes.op_brapos
          || Int.equal op Opcodes.op_sbrapos
          || Int.equal op Opcodes.op_cbrapos
          || Int.equal op Opcodes.op_scbrapos
          || Int.equal op Opcodes.op_once
          || Int.equal op Opcodes.op_script_run
        then (bracket_case [@tailcall]) ()
        else if Int.equal op Opcodes.op_alt then begin
          (* pcre2_study.c:1227-1237 — ALT means nothing mandatory was
             found in this branch (though maybe something optional):
             continue with the next alternative, arranging that the final
             result is SSB_CONTINUE rather than SSB_DONE. *)
          yield := ssb_continue;
          (* try_next = FALSE *)
          -1
        end
        else if
          Int.equal op Opcodes.op_ket
          || Int.equal op Opcodes.op_ketrmax
          || Int.equal op Opcodes.op_ketrmin
          || Int.equal op Opcodes.op_ketrpos
        then
          (* pcre2_study.c:1239-1243 — at the top level SSB_CONTINUE
             indicates failure; after a nested subpattern it causes
             scanning to continue. *)
          ssb_continue
        else if Int.equal op Opcodes.op_callout then begin
          (* pcre2_study.c:1245-1249 — skip over callout. *)
          tcode := !tcode + Opcodes.op_lengths.(Opcodes.op_callout);
          (item_loop [@tailcall]) ()
        end
        else if Int.equal op Opcodes.op_callout_str then begin
          (* pcre2_study.c:1251-1253 *)
          tcode := !tcode + get re.code (!tcode + 1 + (2 * Limits.link_size));
          (item_loop [@tailcall]) ()
        end
        else if
          (* pcre2_study.c:1255-1263 — skip over lookbehind and negative
             lookahead assertions. *)
          Int.equal op Opcodes.op_assert_not
          || Int.equal op Opcodes.op_assertback
          || Int.equal op Opcodes.op_assertback_not
          || Int.equal op Opcodes.op_assertback_na
        then begin
          skip_alts re tcode;
          tcode := !tcode + 1 + Limits.link_size;
          (item_loop [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_brazero
          || Int.equal op Opcodes.op_braminzero
          || Int.equal op Opcodes.op_braposzero
        then begin
          (* pcre2_study.c:1265-1274 — BRAZERO does the bracket, but
             carries on. *)
          incr tcode (* ++tcode in the set_start_bits call *);
          let rc = set_start_bits re !tcode ~utf ~ucp depthptr in
          if
            Int.equal rc ssb_fail
            || Int.equal rc ssb_unknown
            || Int.equal rc ssb_toodeep
          then rc
          else begin
            skip_alts re tcode;
            tcode := !tcode + 1 + Limits.link_size;
            (item_loop [@tailcall]) ()
          end
        end
        else if Int.equal op Opcodes.op_skipzero then begin
          (* pcre2_study.c:1276-1282 — SKIPZERO skips the bracket. *)
          incr tcode;
          skip_alts re tcode;
          tcode := !tcode + 1 + Limits.link_size;
          (item_loop [@tailcall]) ()
        end
        else if
          (* pcre2_study.c:1284-1293 — single-char * or ? sets the bit
             and tries the next item. *)
          Int.equal op Opcodes.op_star
          || Int.equal op Opcodes.op_minstar
          || Int.equal op Opcodes.op_posstar
          || Int.equal op Opcodes.op_query
          || Int.equal op Opcodes.op_minquery
          || Int.equal op Opcodes.op_posquery
        then begin
          tcode := set_table_bit re (!tcode + 1) ~caseless:false ~utf ~ucp;
          (item_loop [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_stari
          || Int.equal op Opcodes.op_minstari
          || Int.equal op Opcodes.op_posstari
          || Int.equal op Opcodes.op_queryi
          || Int.equal op Opcodes.op_minqueryi
          || Int.equal op Opcodes.op_posqueryi
        then begin
          (* pcre2_study.c:1295-1302 *)
          tcode := set_table_bit re (!tcode + 1) ~caseless:true ~utf ~ucp;
          (item_loop [@tailcall]) ()
        end
        else if
          (* pcre2_study.c:1304-1310 — single-char upto sets the bit and
             tries the next. *)
          Int.equal op Opcodes.op_upto
          || Int.equal op Opcodes.op_minupto
          || Int.equal op Opcodes.op_posupto
        then begin
          tcode :=
            set_table_bit re
              (!tcode + 1 + Limits.imm2_size)
              ~caseless:false ~utf ~ucp;
          (item_loop [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_uptoi
          || Int.equal op Opcodes.op_minuptoi
          || Int.equal op Opcodes.op_posuptoi
        then begin
          (* pcre2_study.c:1312-1316 *)
          tcode :=
            set_table_bit re
              (!tcode + 1 + Limits.imm2_size)
              ~caseless:true ~utf ~ucp;
          (item_loop [@tailcall]) ()
        end
        else if
          (* pcre2_study.c:1318-1329 — at least one single char sets the
             bit and stops. *)
          Int.equal op Opcodes.op_exact
          || Int.equal op Opcodes.op_char
          || Int.equal op Opcodes.op_plus
          || Int.equal op Opcodes.op_minplus
          || Int.equal op Opcodes.op_posplus
        then begin
          if Int.equal op Opcodes.op_exact then
            tcode := !tcode + Limits.imm2_size;
          (* fallthrough from OP_EXACT in C *)
          ignore (set_table_bit re (!tcode + 1) ~caseless:false ~utf ~ucp);
          (* try_next = FALSE *)
          -1
        end
        else if
          Int.equal op Opcodes.op_exacti
          || Int.equal op Opcodes.op_chari
          || Int.equal op Opcodes.op_plusi
          || Int.equal op Opcodes.op_minplusi
          || Int.equal op Opcodes.op_posplusi
        then begin
          (* pcre2_study.c:1331-1340 *)
          if Int.equal op Opcodes.op_exacti then
            tcode := !tcode + Limits.imm2_size;
          (* fallthrough from OP_EXACTI in C *)
          ignore (set_table_bit re (!tcode + 1) ~caseless:true ~utf ~ucp);
          (* try_next = FALSE *)
          -1
        end
        else if Int.equal op Opcodes.op_hspace then begin
          (* pcre2_study.c:1342-1382 — special spacing items recognizing
             specific lists of characters. *)
          set_hspace_bits re ~utf;
          (* try_next = FALSE *)
          -1
        end
        else if
          Int.equal op Opcodes.op_anynl || Int.equal op Opcodes.op_vspace
        then begin
          (* pcre2_study.c:1384-1416 *)
          set_vspace_bits re ~utf;
          (* try_next = FALSE *)
          -1
        end
        else if Int.equal op Opcodes.op_not_digit then begin
          (* pcre2_study.c:1418-1426 — single character types set the
             bits and stop. These opcodes are not seen when PCRE2_UCP is
             set, so they apply when only characters less than 256 are
             recognized to match the types. *)
          set_nottype_bits re Chartables.cbit_digit table_limit;
          -1
        end
        else if Int.equal op Opcodes.op_digit then begin
          (* pcre2_study.c:1428-1431 *)
          set_type_bits re Chartables.cbit_digit table_limit;
          -1
        end
        else if Int.equal op Opcodes.op_not_whitespace then begin
          (* pcre2_study.c:1433-1436 *)
          set_nottype_bits re Chartables.cbit_space table_limit;
          -1
        end
        else if Int.equal op Opcodes.op_whitespace then begin
          (* pcre2_study.c:1438-1441 *)
          set_type_bits re Chartables.cbit_space table_limit;
          -1
        end
        else if Int.equal op Opcodes.op_not_wordchar then begin
          (* pcre2_study.c:1443-1446 *)
          set_nottype_bits re Chartables.cbit_word table_limit;
          -1
        end
        else if Int.equal op Opcodes.op_wordchar then begin
          (* pcre2_study.c:1448-1451 *)
          set_type_bits re Chartables.cbit_word table_limit;
          -1
        end
        else if
          (* pcre2_study.c:1453-1460 — one or more character type fudges
             the pointer and restarts, knowing it will hit a single
             character type and stop there. *)
          Int.equal op Opcodes.op_typeplus
          || Int.equal op Opcodes.op_typeminplus
          || Int.equal op Opcodes.op_typeposplus
        then begin
          incr tcode;
          (item_loop [@tailcall]) ()
        end
        else if Int.equal op Opcodes.op_typeexact then begin
          (* pcre2_study.c:1462-1464 *)
          tcode := !tcode + 1 + Limits.imm2_size;
          (item_loop [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_typeupto
          || Int.equal op Opcodes.op_typeminupto
          || Int.equal op Opcodes.op_typeposupto
        then begin
          (* pcre2_study.c:1466-1472 — zero or more repeats of character
             types set the bits and then try again. *)
          tcode := !tcode + Limits.imm2_size;
          (* fallthrough from OP_TYPEUPTO group in C *)
          (type_repeat_tail [@tailcall]) ()
        end
        else if
          Int.equal op Opcodes.op_typestar
          || Int.equal op Opcodes.op_typeminstar
          || Int.equal op Opcodes.op_typeposstar
          || Int.equal op Opcodes.op_typequery
          || Int.equal op Opcodes.op_typeminquery
          || Int.equal op Opcodes.op_typeposquery
        then (type_repeat_tail [@tailcall]) ()
        else if Int.equal op Opcodes.op_xclass then begin
          (* pcre2_study.c:1581-1636 — extended class: if there are any
             property checks, or if this is a negative XCLASS without a
             map, give up. Otherwise code points >= 255 are potential
             starters; in the UTF-8 case the character list can be
             scanned to set bits for the relevant leading bytes. *)
          let xclassflags = byte (!tcode + 1 + Limits.link_size) in
          if
            (not (Int.equal (xclassflags land Opcodes.xcl_hasprop) 0))
            || Int.equal
                 (xclassflags land (Opcodes.xcl_map lor Opcodes.xcl_not))
                 Opcodes.xcl_not
          then ssb_fail
          else begin
            (* pcre2_study.c:1595-1599 — set up the map pointer if there
               is one. *)
            let classmap =
              if Int.equal (xclassflags land Opcodes.xcl_map) 0 then -1
              else !tcode + 1 + Limits.link_size + 1
            in
            if utf && Int.equal (xclassflags land Opcodes.xcl_not) 0 then begin
              (* pcre2_study.c:1601-1634 — in UTF-8 mode, scan the
                 character list and set bits for leading bytes, then jump
                 to handle the map. *)
              let p =
                ref
                  (!tcode + 1 + Limits.link_size + 1
                  + if classmap < 0 then 0 else 32)
              in
              tcode := !tcode + get re.code (!tcode + 1);
              let rec scan () : int =
                let t = byte !p in
                incr p;
                if Int.equal t Opcodes.xcl_single then begin
                  let b = byte !p in
                  incr p;
                  while Int.equal (byte !p land 0xc0) 0x80 do
                    incr p
                  done;
                  set_bit re b;
                  (scan [@tailcall]) ()
                end
                else if Int.equal t Opcodes.xcl_range then begin
                  let b = byte !p in
                  incr p;
                  while Int.equal (byte !p land 0xc0) 0x80 do
                    incr p
                  done;
                  let e = byte !p in
                  incr p;
                  while Int.equal (byte !p land 0xc0) 0x80 do
                    incr p
                  done;
                  for x = b to e do
                    set_bit re x
                  done;
                  (scan [@tailcall]) ()
                end
                else if Int.equal t Opcodes.xcl_end then
                  -2 (* goto HANDLE_CLASSMAP *)
                else ssb_unknown (* internal error, should not occur *)
              in
              let rc = scan () in
              if Int.equal rc (-2) then begin
                (* HANDLE_CLASSMAP (1680) *)
                handle_classmap classmap;
                (class_tail [@tailcall]) ()
              end
              else rc
            end
            else
              (* fallthrough from OP_XCLASS to OP_NCLASS in C *)
              (nclass_class_case [@tailcall]) ~nclass_bits:true classmap
          end
        end
        else if Int.equal op Opcodes.op_nclass then
          (nclass_class_case [@tailcall]) ~nclass_bits:true (-1)
        else if Int.equal op Opcodes.op_class then
          (nclass_class_case [@tailcall]) ~nclass_bits:false (-1)
        else
          (* pcre2_study.c:990-995 — default: an opcode not added to this
             function. *)
          ssb_unknown
      and bracket_case () : int =
        (* pcre2_study.c:1198-1225 — for a group bracket or a positive
           assertion without an immediately following mandatory setting,
           recurse to set bits from within the subpattern. If it can't
           find anything, give up. If it finds some mandatory
           character(s), we are done for this branch. Otherwise, carry on
           scanning after the subpattern. *)
        let rc = set_start_bits re !tcode ~utf ~ucp depthptr in
        if Int.equal rc ssb_done then (* try_next = FALSE *) -1
        else if Int.equal rc ssb_continue then begin
          skip_alts re tcode;
          tcode := !tcode + 1 + Limits.link_size;
          (item_loop [@tailcall]) ()
        end
        else rc (* FAIL, UNKNOWN, or TOODEEP *)
      and type_repeat_tail () : int =
        (* pcre2_study.c:1474-1579 — the OP_TYPESTAR..OP_TYPEPOSQUERY
           switch on tcode[1], then tcode += 2 and try again
           ([type_repeat_advance]). *)
        let t = byte (!tcode + 1) in
        if Int.equal t Opcodes.op_hspace then begin
          set_hspace_bits re ~utf;
          (type_repeat_advance [@tailcall]) ()
        end
        else if Int.equal t Opcodes.op_anynl || Int.equal t Opcodes.op_vspace
        then begin
          set_vspace_bits re ~utf;
          (type_repeat_advance [@tailcall]) ()
        end
        else if Int.equal t Opcodes.op_not_digit then begin
          set_nottype_bits re Chartables.cbit_digit table_limit;
          (type_repeat_advance [@tailcall]) ()
        end
        else if Int.equal t Opcodes.op_digit then begin
          set_type_bits re Chartables.cbit_digit table_limit;
          (type_repeat_advance [@tailcall]) ()
        end
        else if Int.equal t Opcodes.op_not_whitespace then begin
          set_nottype_bits re Chartables.cbit_space table_limit;
          (type_repeat_advance [@tailcall]) ()
        end
        else if Int.equal t Opcodes.op_whitespace then begin
          set_type_bits re Chartables.cbit_space table_limit;
          (type_repeat_advance [@tailcall]) ()
        end
        else if Int.equal t Opcodes.op_not_wordchar then begin
          set_nottype_bits re Chartables.cbit_word table_limit;
          (type_repeat_advance [@tailcall]) ()
        end
        else if Int.equal t Opcodes.op_wordchar then begin
          set_type_bits re Chartables.cbit_word table_limit;
          (type_repeat_advance [@tailcall]) ()
        end
        else
          (* default, OP_ANY, OP_ALLANY (1482-1485) *)
          ssb_fail
      and type_repeat_advance () : int =
        (* pcre2_study.c:1578 — tcode += 2 after the type switch. *)
        tcode := !tcode + 2;
        (item_loop [@tailcall]) ()
      and handle_classmap (classmap : int) : unit =
        (* pcre2_study.c:1672-1705 — HANDLE_CLASSMAP: when wide
           characters are supported, classmap may be NULL (-1). In UTF-8
           mode the bits in a class bit map correspond to character
           values, not to byte values, so a conversion is done for
           characters whose code point is greater than 127 (there are
           only two possible starting bytes for characters in the range
           128-255). *)
        if classmap >= 0 then
          if utf then begin
            for c = 0 to 15 do
              or_bitmap re c (byte (classmap + c))
            done;
            let c = ref 128 in
            while !c < 256 do
              if
                not
                  (Int.equal
                     (byte (classmap + (!c lsr 3)) land (1 lsl (!c land 7)))
                     0)
              then begin
                (* set the bit for this starter, then skip on to the
                   next relevant character *)
                let d = (!c lsr 6) lor 0xc0 in
                set_bit re d;
                c := (!c land 0xc0) + 0x40 - 1
              end;
              incr c
            done
          end
          else
            (* in all modes except UTF-8, the two bit maps are
               compatible *)
            for c = 0 to 31 do
              or_bitmap re c (byte (classmap + c))
            done
      and class_tail () : int =
        (* pcre2_study.c:1707-1731 — act on what follows the class. For a
           zero minimum repeat, continue; otherwise stop processing. *)
        let t = byte !tcode in
        if
          Int.equal t Opcodes.op_crstar
          || Int.equal t Opcodes.op_crminstar
          || Int.equal t Opcodes.op_crquery
          || Int.equal t Opcodes.op_crminquery
          || Int.equal t Opcodes.op_crposstar
          || Int.equal t Opcodes.op_crposquery
        then begin
          incr tcode;
          (item_loop [@tailcall]) ()
        end
        else if
          Int.equal t Opcodes.op_crrange
          || Int.equal t Opcodes.op_crminrange
          || Int.equal t Opcodes.op_crposrange
        then
          if Int.equal (get2 re.code (!tcode + 1)) 0 then begin
            tcode := !tcode + 1 + (2 * Limits.imm2_size);
            (item_loop [@tailcall]) ()
          end
          else (* try_next = FALSE *) -1
        else (* try_next = FALSE *) -1
      and nclass_class_case ~(nclass_bits : bool) (classmap0 : int) : int =
        (* pcre2_study.c:1643-1705 — the OP_NCLASS/OP_CLASS shared code,
           entered directly for those opcodes (classmap0 = -1 = NULL) or
           by fall through from OP_XCLASS. For a negative class in UTF-8
           mode, any byte with a value >= 0xc4 is a potentially valid
           starter (it starts a character with a value > 255); in 8-bit
           non-UTF mode there is no difference between CLASS and
           NCLASS. *)
        if nclass_bits && utf then begin
          or_bitmap re 24 0xf0 (* bits for 0xc4 - 0xc8 *);
          for i = 25 to 31 do
            Bytes.set re.start_bitmap i '\xff' (* bits for 0xc9 - 0xff *)
          done
        end;
        (* pcre2_study.c:1661-1670 — if we have fallen through from an
           XCLASS, classmap is already set; just advance the code
           pointer. Otherwise set up classmap for a non-XCLASS and
           advance past it. *)
        let classmap =
          if Int.equal (byte !tcode) Opcodes.op_xclass then begin
            tcode := !tcode + get re.code (!tcode + 1);
            classmap0
          end
          else begin
            let cm = !tcode + 1 in
            tcode := cm + 32;
            cm
          end
        in
        handle_classmap classmap;
        (class_tail [@tailcall]) ()
      in
      let rc = item_loop () in
      if rc >= 0 then rc (* an early return out of the C's branch loop *)
      else begin
        (* pcre2_study.c:1736-1738 — advance to the next branch. *)
        let code = code + get re.code (code + 1) in
        if Int.equal (byte code) Opcodes.op_alt then
          (branch_loop [@tailcall]) code
        else !yield
      end
    in
    branch_loop code
  end

(* ---------- Study a compiled expression ---------- *)

(* pcre2_study.c:1745-1913 — PRIV(study): study a compiled expression to
   produce information that will speed up matching.

   Returns:  0 normally
             1 unknown opcode in set_start_bits
             2 missing capturing bracket
             3 unknown opcode in find_minlength *)
let study (re : re)
    ~(find_bracket : Bytes.t -> int -> utf:bool -> int -> int) : int =
  let count = ref 0 in
  let utf = not (Int.equal (re.overall_options land Options.utf) 0) in
  let ucp = not (Int.equal (re.overall_options land Options.ucp) 0) in
  (* pcre2_study.c:1769-1772 — find start of compiled code: offset 0 of
     re.code in this port (the name table is a separate Bytes.t). *)
  let code = 0 in
  (* pcre2_study.c:1774-1878 — for a pattern that has a first code unit,
     or a multiline pattern that matches only at "line start", there is
     no point in seeking a list of starting code units. *)
  let rc_first =
    if Int.equal (re.flags land (firstset lor startline)) 0 then begin
      let depth = ref 0 in
      let rc = set_start_bits re code ~utf ~ucp depth in
      if Int.equal rc ssb_unknown then 1
      else begin
        (* pcre2_study.c:1784-1877 — if a list of starting code units was
           set up, scan the list to see if only one or two were listed.
           If two are listed and they are caseless versions of the same
           character, the list can be replaced with a caseless first code
           unit (better performance for patterns such as [Ww]ord or
           (word|WORD)). *)
        if Int.equal rc ssb_done then begin
          let a = ref (-1) in
          let b = ref (-1) in
          let flags = ref firstmapset in
          (* the C's goto DONE becomes the [donef] flag: it skips the
             replacement block below and joins at re.flags |= flags. *)
          let donef = ref false in
          let i = ref 0 in
          (* for (i = 0; i < 256; p++, i += 8) *)
          while (not !donef) && !i < 256 do
            let x = Char.code (Bytes.get re.start_bitmap (!i lsr 3)) in
            if not (Int.equal x 0) then begin
              let y = x land (lnot x + 1) (* least significant bit *) in
              if not (Int.equal y x) then donef := true
                (* more than one bit set *)
              else begin
                (* pcre2_study.c:1816-1826 — compute the character
                   value. *)
                let c =
                  !i
                  +
                  match x with
                  | 1 -> 0
                  | 2 -> 1
                  | 4 -> 2
                  | 8 -> 3
                  | 16 -> 4
                  | 32 -> 5
                  | 64 -> 6
                  | 128 -> 7
                  | _ -> 0 (* unreachable: y = x means x is a power of 2 *)
                in
                (* pcre2_study.c:1828-1834 — in 8-bit UTF mode, only
                   values < 128 can be used. *)
                if utf && c > 127 then donef := true
                else if !a < 0 then a := c (* first one found *)
                else if !b < 0 then begin
                  (* pcre2_study.c:1836-1850 — second one found. *)
                  let d = Chartables.fcc c (* TABLE_GET; 8-bit *) in
                  if utf || ucp then begin
                    if not (Int.equal (Ucd.caseset c) 0) then
                      donef := true (* multiple case set *)
                    else begin
                      let d = if c > 127 then Ucd.othercase c else d in
                      if not (Int.equal d !a) then donef := true
                        (* not the other case of a *)
                      else b := c (* save second in b *)
                    end
                  end
                  else if not (Int.equal d !a) then donef := true
                  else b := c
                end
                else donef := true (* more than two characters found *)
              end
            end;
            i := !i + 8
          done;
          (* pcre2_study.c:1855-1873 — replace the start code unit bits
             with a first code unit, but only if it is not the same as a
             required later code unit (patterns such as /a*a/ don't work
             if both the start unit and required unit are the same). *)
          if not !donef then
            if
              !a >= 0
              && (Int.equal (re.flags land lastset) 0
                 || (not (Int.equal re.last_codeunit !a))
                    && (!b < 0 || not (Int.equal re.last_codeunit !b)))
            then begin
              re.first_codeunit <- !a;
              flags := firstset;
              if !b >= 0 then flags := !flags lor firstcaseless
            end;
          (* DONE: (1875-1876) *)
          re.flags <- re.flags lor !flags
        end;
        0
      end
    end
    else 0
  in
  if not (Int.equal rc_first 0) then rc_first
  else if
    (* pcre2_study.c:1880-1910 — find the minimum length of subject
       string. If the pattern can match an empty string, the minimum
       length is already known; if it contains ( *ACCEPT) we don't even
       try; if there are more back references than the cache vector, do
       nothing (such a pattern would take too long to analyze anyway). *)
    Int.equal (re.flags land (match_empty lor hasaccept)) 0
    && re.top_backref <= max_cache_backref
  then begin
    let backref_cache = Array.make (max_cache_backref + 1) 0 in
    backref_cache.(0) <- 0 (* highest one that is set *);
    let min =
      find_minlength re ~find_bracket code code ~utf None count backref_cache
    in
    if Int.equal min (-1) then
      0 (* \C in UTF mode or over-complex: leave minlength unchanged *)
    else if Int.equal min (-2) then 2 (* missing capturing bracket *)
    else if Int.equal min (-3) then 3 (* unrecognized opcode *)
    else begin
      re.minlength <- (if min > uint16_max then uint16_max else min);
      0
    end
  end
  else 0
