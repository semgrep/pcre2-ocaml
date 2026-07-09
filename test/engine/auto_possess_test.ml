(* Engine module-initialization asserts for [Pcre2_engine.Auto_possess], migrated
   verbatim from src/engine/auto_possess.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Auto_possess

(* Hand-assembled compiled programs (the interpreter test precedent:
   opcode bodies terminated by OP_END; groups carry real KET links),
   checked against `pcre2test -d` on the real PCRE2 10.44 8-bit library
   ("PCRE2 version 10.44 2024-06-07"): each case below is annotated with
   the pattern whose oracle dump pins the expected rewrite. *)
let test_0 () =
  let mk (units : int list) : Bytes.t =
    let b = Bytes.create (List.length units) in
    List.iteri (fun i u -> Bytes.set b i (Char.chr u)) units;
    b
  in
  let run ?(external_options = 0) ?(had_recurse = false) (units : int list) :
      Bytes.t =
    let b = mk units in
    assert (Int.equal (auto_possessify b ~external_options ~had_recurse) 0);
    b
  in
  let byte (b : Bytes.t) (i : int) : int = Char.code (Bytes.get b i) in
  let a = Char.code 'a' and bc = Char.code 'b' in
  (* /a*b/ → `a*+ b` and /a*?b/ → `a*+ b` (both oracle-pinned: MINSTAR is
     possessified through the direct character comparison, which never
     consults the greediness flag). *)
  let p = run [ Opcodes.op_star; a; Opcodes.op_char; bc; Opcodes.op_end ] in
  assert (Int.equal (byte p 0) Opcodes.op_posstar);
  let p = run [ Opcodes.op_minstar; a; Opcodes.op_char; bc; Opcodes.op_end ] in
  assert (Int.equal (byte p 0) Opcodes.op_posstar);
  (* /a*a/: same character — no rewrite. *)
  let p = run [ Opcodes.op_star; a; Opcodes.op_char; a; Opcodes.op_end ] in
  assert (Int.equal (byte p 0) Opcodes.op_star);
  (* /a{0,2}b/ shape → POSUPTO (the oracle's /é{2,4}/ shows {0,2}+); EXACT
     is never rewritten. *)
  let p =
    run [ Opcodes.op_upto; 0; 2; a; Opcodes.op_char; bc; Opcodes.op_end ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_posupto);
  let p =
    run [ Opcodes.op_exact; 0; 2; a; Opcodes.op_char; bc; Opcodes.op_end ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_exact);
  (* /a*b/i → `/i a*+` (STARI + CHARI via othercase lists); /a*A/i stays
     (A is a's othercase) — both oracle-pinned. *)
  let p = run [ Opcodes.op_stari; a; Opcodes.op_chari; bc; Opcodes.op_end ] in
  assert (Int.equal (byte p 0) Opcodes.op_posstari);
  let p =
    run [ Opcodes.op_stari; a; Opcodes.op_chari; Char.code 'A'; Opcodes.op_end ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_stari);
  (* /\d+x/ → `\d++ x`; /\d+5/ stays (autoposstab is not consulted — the
     char loop tests ctype_digit). *)
  let p =
    run
      [
        Opcodes.op_typeplus;
        Opcodes.op_digit;
        Opcodes.op_char;
        Char.code 'x';
        Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_typeposplus);
  let p =
    run
      [
        Opcodes.op_typeplus;
        Opcodes.op_digit;
        Opcodes.op_char;
        Char.code '5';
        Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_typeplus);
  (* /\D+\d/ → `\D++ \d` (autoposstab[\D][\d] = 1); /\d+\z/ → `\d++ \z`
     (autoposstab[\d][\z] = 1, both oracle-pinned); /a*$/ → `a*+ $` (the
     char loop's OP_DOLL arm). *)
  let p =
    run
      [
        Opcodes.op_typeplus;
        Opcodes.op_not_digit;
        Opcodes.op_digit;
        Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_typeposplus);
  let p =
    run
      [ Opcodes.op_typeplus; Opcodes.op_digit; Opcodes.op_eod; Opcodes.op_end ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_typeposplus);
  let p = run [ Opcodes.op_star; a; Opcodes.op_doll; Opcodes.op_end ] in
  assert (Int.equal (byte p 0) Opcodes.op_posstar);
  (* /\X+/ → `extuni++`; /\X+\d/ stays (autoposstab row \X, both
     oracle-pinned) — the OP_EXTUNI acceptance in get_chr_property_list. *)
  let p = run [ Opcodes.op_typeplus; Opcodes.op_extuni; Opcodes.op_end ] in
  assert (Int.equal (byte p 0) Opcodes.op_typeposplus);
  let p =
    run
      [
        Opcodes.op_typeplus; Opcodes.op_extuni; Opcodes.op_digit; Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_typeplus);
  (* Class repeats: [a]* before 'b' → CRPOSSTAR; before 'a' unchanged
     (/[a-d]*\d/ vs /[a-d]*[e-f]/ pin the possessified forms). *)
  let class_a = List.init 32 (fun i -> if Int.equal i 12 then 2 else 0) in
  (* bit for 'a' = 0x61: byte 12, bit 1 *)
  let p =
    run
      ([ Opcodes.op_class ] @ class_a
      @ [ Opcodes.op_crstar; Opcodes.op_char; bc; Opcodes.op_end ])
  in
  assert (Int.equal (byte p 33) Opcodes.op_crposstar);
  let p =
    run
      ([ Opcodes.op_class ] @ class_a
      @ [ Opcodes.op_crstar; Opcodes.op_char; a; Opcodes.op_end ])
  in
  assert (Int.equal (byte p 33) Opcodes.op_crstar);
  (* Class vs type: [a]* before \d → CRPOSSTAR (bitset against
     cbits/cbit_digit). *)
  let p =
    run
      ([ Opcodes.op_class ] @ class_a
      @ [ Opcodes.op_crstar; Opcodes.op_digit; Opcodes.op_end ])
  in
  assert (Int.equal (byte p 33) Opcodes.op_crposstar);
  (* Callout skips (both the compare walk and the main scan): /a*(?C1)b/ →
     `a*+ Callout b` (oracle-pinned); the string flavour uses the length
     word at 1+2*LINK_SIZE. *)
  let p =
    run
      [
        Opcodes.op_star;
        a;
        Opcodes.op_callout;
        0;
        0;
        0;
        0;
        0;
        Opcodes.op_char;
        bc;
        Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_posstar);
  let p =
    run
      [
        Opcodes.op_star;
        a;
        Opcodes.op_callout_str;
        0;
        0;
        0;
        0;
        0;
        8;
        0;
        Opcodes.op_char;
        bc;
        Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_posstar);
  (* Iterator at the end of a capturing group: possessified across the
     KET unless the pattern contains recursion (cb->had_recurse). *)
  let cap_star =
    [
      Opcodes.op_cbra;
      0;
      7;
      0;
      1;
      Opcodes.op_star;
      a;
      Opcodes.op_ket;
      0;
      7;
      Opcodes.op_char;
      bc;
      Opcodes.op_end;
    ]
  in
  let p = run cap_star in
  assert (Int.equal (byte p 5) Opcodes.op_posstar);
  let p = run ~had_recurse:true cap_star in
  assert (Int.equal (byte p 5) Opcodes.op_star);
  (* Script runs: /(*sr:a*)b/ → `a*+` (explicit character allowed) but
     /(*sr:\d*)x/ keeps `\d*` (both oracle-pinned). *)
  let p =
    run
      [
        Opcodes.op_script_run;
        0;
        5;
        Opcodes.op_star;
        a;
        Opcodes.op_ket;
        0;
        5;
        Opcodes.op_char;
        bc;
        Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 3) Opcodes.op_posstar);
  let p =
    run
      [
        Opcodes.op_script_run;
        0;
        5;
        Opcodes.op_typestar;
        Opcodes.op_digit;
        Opcodes.op_ket;
        0;
        5;
        Opcodes.op_char;
        Char.code 'x';
        Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 3) Opcodes.op_typestar);
  (* /(?>a+)b/ → `a++` inside the atomic group (oracle-pinned; also the
     compile.ml end-to-end assert). *)
  let p =
    run
      [
        Opcodes.op_once;
        0;
        5;
        Opcodes.op_plus;
        a;
        Opcodes.op_ket;
        0;
        5;
        Opcodes.op_char;
        bc;
        Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 3) Opcodes.op_posplus);
  (* A group after the iterator: /a*(b|c)d/ → `a*+` (every branch and the
     continuation are checked); with an 'a' branch it must stay. *)
  let group_after mid =
    [
      Opcodes.op_star;
      a;
      Opcodes.op_bra;
      0;
      5;
      Opcodes.op_char;
      bc;
      Opcodes.op_alt;
      0;
      5;
      Opcodes.op_char;
      mid;
      Opcodes.op_ket;
      0;
      10;
      Opcodes.op_char;
      Char.code 'd';
      Opcodes.op_end;
    ]
  in
  let p = run (group_after (Char.code 'c')) in
  assert (Int.equal (byte p 0) Opcodes.op_posstar);
  let p = run (group_after a) in
  assert (Int.equal (byte p 0) Opcodes.op_star);
  (* BRAZERO: /a*(?:b|c)?d/ → `a*+` (oracle-pinned; group content and the
     item after the optional group are both checked). *)
  let p =
    run
      [
        Opcodes.op_star;
        a;
        Opcodes.op_brazero;
        Opcodes.op_bra;
        0;
        5;
        Opcodes.op_char;
        bc;
        Opcodes.op_alt;
        0;
        5;
        Opcodes.op_char;
        Char.code 'c';
        Opcodes.op_ket;
        0;
        10;
        Opcodes.op_char;
        Char.code 'd';
        Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_posstar);
  (* Unicode property pairs (propposstab): /\p{Nd}+\p{Lu}/ → `prop Nd ++`
     (PT_PC vs PT_PC, distinct values, both OP_PROP → case 2);
     \p{Nd}+\p{Nd} must stay. Oracle-pinned under /utf. *)
  let prop_pair v =
    [
      Opcodes.op_typeplus;
      Opcodes.op_prop;
      Opcodes.pt_pc;
      Ucp.ucp_nd;
      Opcodes.op_prop;
      Opcodes.pt_pc;
      v;
      Opcodes.op_end;
    ]
  in
  let p = run (prop_pair Ucp.ucp_lu) in
  assert (Int.equal (byte p 0) Opcodes.op_typeposplus);
  let p = run (prop_pair Ucp.ucp_nd) in
  assert (Int.equal (byte p 0) Opcodes.op_typeplus);
  (* UTF walking: a multi-byte literal before the iterator must be
     stepped over (MAYBE_UTF_MULTI), and a multi-byte UPTO is
     possessified exactly like the oracle's /é{2,4}/ tail. *)
  let p =
    run ~external_options:Options.utf
      [
        Opcodes.op_char;
        0xc3;
        0xa9;
        Opcodes.op_star;
        Char.code 'x';
        Opcodes.op_char;
        Char.code 'y';
        Opcodes.op_end;
      ]
  in
  assert (Int.equal (byte p 3) Opcodes.op_posstar);
  let p =
    run ~external_options:Options.utf
      [ Opcodes.op_upto; 0; 2; 0xc3; 0xa9; Opcodes.op_char; bc; Opcodes.op_end ]
  in
  assert (Int.equal (byte p 0) Opcodes.op_posupto);
  (* A corrupt opcode returns -1 (→ ERR80 at the driver). *)
  let bad = mk [ 200; Opcodes.op_end ] in
  assert (
    Int.equal (auto_possessify bad ~external_options:0 ~had_recurse:false) (-1))

let tests = [ Alcotest.test_case "auto_possess 0" `Quick test_0 ]
