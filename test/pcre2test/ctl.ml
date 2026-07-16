(* Pattern / data control blocks, mirroring pcre2test's patctl and datctl.

   Control bit values are copied from vendor/pcre2/src/pcre2test.c with
   citations; only bits the harness acts on (or must diagnose) are defined. *)

(* pcre2test.c:478-509 — first control word *)
let ctl_aftertext = 0x00000001
let ctl_allaftertext = 0x00000002
let ctl_allcaptures = 0x00000004
let ctl_allusedtext = 0x00000008
let ctl_altglobal = 0x00000010
let ctl_expand = 0x00000400
let ctl_getall = 0x00004000
let ctl_global = 0x00008000
let ctl_hexpat = 0x00010000
let ctl_mark = 0x00100000
let ctl_startchar = 0x10000000
let ctl_utf8_input = 0x40000000
let ctl_zero_terminate = 0x80000000

(* pcre2test.c:519-538 — second control word *)
let ctl2_subject_literal = 0x00000100
let ctl2_nl_set = 0x40000000
let ctl2_bsr_set = 0x80000000

(* pcre2test.c:515 *)
let ctl_anyglob = ctl_altglobal lor ctl_global

(* pcre2test.c:545-553 — pattern controls copied to each data line *)
let ctl_allpd =
  ctl_aftertext lor ctl_allaftertext lor ctl_allcaptures lor ctl_allusedtext
  lor ctl_altglobal lor ctl_global lor ctl_mark lor ctl_startchar
  lor ctl_utf8_input

(* pcre2test.c:555-564 — CTL2_ALLPD; only the bit we track is subject-irrelevant,
   but keep the mask shape for fidelity (substitute bits are out of scope). *)
let ctl2_allpd = 0

(* pcre2test.c:212 *)
let default_oveccount = 15

(* pcre2test.c:571-588 patctl (subset the harness models). The compile-context
   fields (newline/bsr/extra) correspond to pcre2test's pat_context. *)
type patctl = {
  mutable options : int;
  mutable control : int;
  mutable control2 : int;
  mutable ctx_newline : int; (* 0 = build default *)
  mutable ctx_bsr : int; (* 0 = build default *)
  mutable ctx_extra : int;
}

(* pcre2test.c:593-611 datctl (subset). *)
type datctl = {
  mutable d_options : int;
  mutable d_control : int;
  mutable d_control2 : int;
  mutable offset : int;
  mutable oveccount : int;
  (* Match-context limits (MOD_CTM, pcre2test.c:684/706/718/759). pcre2test
     stores these directly in a pcre2_match_context; the harness has no
     match-context object, so it keeps them here and passes them as the
     per-call ?match_limit/?depth_limit/?heap_limit engine-seam args. -1 =
     unset (the build default is used at the seam). *)
  mutable match_limit : int;
  mutable depth_limit : int;
  mutable heap_limit : int;
  (* copy/get requests in order of appearance *)
  mutable copy_numbers : int list;
  mutable copy_names : string list;
  mutable get_numbers : int list;
  mutable get_names : string list;
}

let new_patctl () =
  {
    options = 0;
    control = 0;
    control2 = 0;
    ctx_newline = 0;
    ctx_bsr = 0;
    ctx_extra = 0;
  }

let copy_patctl (p : patctl) = { p with options = p.options }

let new_datctl () =
  {
    d_options = 0;
    d_control = 0;
    d_control2 = 0;
    offset = 0;
    oveccount = default_oveccount;
    match_limit = -1;
    depth_limit = -1;
    heap_limit = -1;
    copy_numbers = [];
    copy_names = [];
    get_numbers = [];
    get_names = [];
  }

let copy_datctl (d : datctl) = { d with d_options = d.d_options }

(* pcre2test.c:4213-4264 show_controls — names for the control bits we track,
   emitted in the same collating order as the C. *)
let control_names : (int * int * string) list =
  (* (ctl bit, ctl2 bit, name) — exactly the rows relevant to tracked bits *)
  [
    (ctl_aftertext, 0, "aftertext");
    (ctl_allaftertext, 0, "allaftertext");
    (ctl_allcaptures, 0, "allcaptures");
    (ctl_allusedtext, 0, "allusedtext");
    (ctl_altglobal, 0, "altglobal");
    (0, ctl2_bsr_set, "bsr");
    (ctl_expand, 0, "expand");
    (ctl_getall, 0, "getall");
    (ctl_global, 0, "global");
    (ctl_hexpat, 0, "hex");
    (ctl_mark, 0, "mark");
    (0, ctl2_nl_set, "newline");
    (ctl_startchar, 0, "startchar");
    (ctl_utf8_input, 0, "utf8_input");
    (ctl_zero_terminate, 0, "zero_terminate");
  ]

(* pcre2test.c:4213-4264 *)
let show_controls ~control ~control2 before =
  let b = Buffer.create 64 in
  Buffer.add_string b before;
  List.iter
    (fun (c1, c2, name) ->
      if control land c1 lor (control2 land c2) <> 0 then (
        Buffer.add_char b ' ';
        Buffer.add_string b name))
    control_names;
  Buffer.contents b
