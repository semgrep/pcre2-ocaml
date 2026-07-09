(* Extended grapheme cluster stepping for the pure-OCaml PCRE2 10.44 port
   (8-bit library).

   Ported from vendor/pcre2/src/pcre2_extuni.c: PRIV(extuni) (95-158),
   the internal function used to match a Unicode extended grapheme
   sequence. The grapheme-break pair table PRIV(ucp_gbtable) lives in
   Tables (pcre2_tables.c:141-208); UCD_GRAPHBREAK is Ucd.gbprop.

   Pointer mapping (the frames.ml/interpreter.ml convention): the C's
   PCRE2_SPTR arguments become int offsets into [subject], which is passed
   alongside (DEVIATION: the C embeds the base in its pointers).

   DEVIATION: the C's sixth argument `int *xcount` (a count of additional
   characters, pcre2_extuni.c:89-91) is dropped — every pcre2_match.c call
   site passes NULL (2630-2631, 2991-2992, 3822-3823, 4416-4417); the only
   non-NULL caller is pcre2_dfa_match.c, which is out of this port's
   scope.

   All helpers are top-level and fully applied at every call site: no
   closures, no allocation (port-conventions §8 — extuni runs inside the
   interpreter's frame loop). *)

(* pcre2_intmodedep.h:341-345 — BACKCHAR(eptr) over the subject: if the
   position is not at the start of a character, move it back until it is.
   DEVIATION (defined behavior where the C is undefined, the
   Interpreter.backchar_subject / Utf.peek precedent): the walk is clamped
   at position 0 and out-of-range positions read as 0 (not a continuation
   byte, so the walk stops). On a valid UTF subject — every path except
   PCRE2_NO_UTF_CHECK with garbage — the C's unclamped walk stops at the
   character's lead byte all the same. *)
let backchar (s : string) (pos : int) : int =
  let len = String.length s in
  let p = ref pos in
  while
    !p > 0 && !p < len
    (* safe: 0 < !p < len checked in the conjuncts above *)
    && Int.equal (Char.code (String.unsafe_get s !p) land 0xc0) 0x80
  do
    decr p
  done;
  !p

(* GETCHAR (pcre2_intmodedep.h:298-303) over the subject with the lead
   byte read through Utf.peek (clamped; in bounds on every valid-UTF
   path — the Interpreter.getchar_subject precedent). *)
let getchar (s : string) (pos : int) : int =
  let c = Utf.peek s pos in
  if c >= 0xc0 then Utf.getutf8 c s pos else c

(* pcre2_extuni.c:119-140 — not breaking between Regional Indicators is
   allowed only if there are an even number of preceding RIs: the while
   loop counting them, walking backwards from [bptr] (already pointing to
   the left-hand character). Returns the C's ricount. Reads are in
   bounds: 0 <= bptr on entry (caller adjusted with [backchar], clamped)
   and the walk only moves left, stopping at start_subject >= 0. *)
let rec ri_count (subject : string) (start_subject : int) (utf : bool)
    (bptr : int) (ricount : int) : int =
  if bptr <= start_subject then ricount (* while (bptr > start_subject) *)
  else
    (* pcre2_extuni.c:129-136 — bptr--; if (utf) { BACKCHAR(bptr);
       GETCHAR(c, bptr); } else c = *bptr. *)
    let bptr = bptr - 1 in
    let bptr = if utf then backchar subject bptr else bptr in
    let c =
      if utf then getchar subject bptr
      else
        (* safe: 0 <= start_subject <= bptr < entry bptr < String.length
           subject (entry bptr is left of a previously read position) *)
        Char.code (String.unsafe_get subject bptr)
    in
    (* pcre2_extuni.c:137-138 *)
    if not (Int.equal (Ucd.gbprop c) Ucp.ucp_gb_regional_indicator) then ricount
    else (ri_count [@tailcall]) subject start_subject utf bptr (ricount + 1)

(* pcre2_extuni.c:102-155 — the main while (eptr < end_subject) loop.
   [lgb]/[was_ep_zwj] are the C's locals of the same names; [eptr] is the
   loop-carried position. Caller contract (the C's argument contract,
   pcre2_match.c call sites): 0 <= start_subject < eptr and
   end_subject <= String.length subject, so every read below is in
   bounds. *)
let rec step (subject : string) (start_subject : int) (end_subject : int)
    (utf : bool) (lgb : int) (was_ep_zwj : bool) (eptr : int) : int =
  if eptr >= end_subject then eptr
  else
    (* pcre2_extuni.c:105-107 — int len = 1; if (!utf) c = *eptr; else
       { GETCHARLEN(c, eptr, len); } rgb = UCD_GRAPHBREAK(c). *)
    (* safe: 0 <= eptr < end_subject <= String.length subject (checked
       above; caller contract) *)
    let c0 = Char.code (String.unsafe_get subject eptr) in
    let c = if utf && c0 >= 0xc0 then Utf.getutf8 c0 subject eptr else c0 in
    let len = if utf && c0 >= 0xc0 then 1 + Utf.get_extralen c0 else 1 in
    let rgb = Ucd.gbprop c in
    (* pcre2_extuni.c:108 — break if the pair table permits a break. *)
    if Int.equal (Tables.ucp_gbtable.(lgb) land (1 lsl rgb)) 0 then eptr
    else if
      (* pcre2_extuni.c:110-114 — ZWJ followed by Extended Pictographic is
         allowed only if the ZWJ was preceded by Extended Pictographic. *)
      Int.equal lgb Ucp.ucp_gb_zwj
      && Int.equal rgb Ucp.ucp_gb_extended_pictographic
      && not was_ep_zwj
    then eptr
    else if
      (* pcre2_extuni.c:116-141 — not breaking between Regional Indicators
         is allowed only if there are an even number of preceding RIs:
         bptr = eptr - 1; if (utf) BACKCHAR(bptr) — bptr then points to
         the left-hand character — count backwards from there; an odd
         count means a grapheme break is required. *)
      Int.equal lgb Ucp.ucp_gb_regional_indicator
      && Int.equal rgb Ucp.ucp_gb_regional_indicator
      &&
      let bptr = eptr - 1 in
      let bptr = if utf then backchar subject bptr else bptr in
      not (Int.equal (ri_count subject start_subject utf bptr 0 land 1) 0)
    then eptr
    else
      (* pcre2_extuni.c:143-146 — set a flag when ZWJ follows Extended
         Pictographic (with optional Extend in between; see next
         statement). *)
      let was_ep_zwj =
        Int.equal lgb Ucp.ucp_gb_extended_pictographic
        && Int.equal rgb Ucp.ucp_gb_zwj
      in
      (* pcre2_extuni.c:148-151 — if Extend follows Extended_Pictographic,
         do not update lgb; this allows any number of them before a
         following ZWJ. *)
      let lgb =
        if
          (not (Int.equal rgb Ucp.ucp_gb_extend))
          || not (Int.equal lgb Ucp.ucp_gb_extended_pictographic)
        then rgb
        else lgb
      in
      (* pcre2_extuni.c:153-154 — eptr += len (xcount dropped, see the
         module DEVIATION note). *)
      (step [@tailcall]) subject start_subject end_subject utf lgb was_ep_zwj
        (eptr + len)

(* pcre2_extuni.c:95-158 — PRIV(extuni): match an extended grapheme
   sequence.

   Arguments (pcre2_extuni.c:83-90):
     c              the first character
     subject        the subject string (DEVIATION note above)
     eptr           offset of the next character
     start_subject  offset of the start of the subject
     end_subject    offset past the end of the subject
     utf            true if in UTF mode

   Returns (92): the offset after the end of the sequence. *)
let extuni (c : int) (subject : string) (eptr : int) (start_subject : int)
    (end_subject : int) (utf : bool) : int =
  (* pcre2_extuni.c:99-100 — BOOL was_ep_ZWJ = FALSE; int lgb =
     UCD_GRAPHBREAK(c). *)
  step subject start_subject end_subject utf (Ucd.gbprop c) false eptr

(* ---------- Inline sanity checks (module-initialization asserts) ---------- *)

(* Cluster boundaries pinned against pcre2test 10.44 `/\X/g,aftertext`
   oracle runs (and UAX #29 rules GB3-GB13). [cluster_end_at] fetches the
   first character exactly like the interpreter's GETCHARINCTEST + extuni
   call sites (pcre2_match.c:2629-2631). *)
let () =
  let buf = Bytes.make 64 '\000' in
  let enc (cs : int list) : string =
    let p = ref 0 in
    List.iter (fun c -> p := !p + Utf.ord2utf c buf !p) cs;
    Bytes.sub_string buf 0 !p
  in
  let cluster_end_at (s : string) (pos : int) (utf : bool) : int =
    let c0 = Char.code s.[pos] in
    let c = if utf && c0 >= 0xc0 then Utf.getutf8 c0 s pos else c0 in
    let ep =
      if utf && c0 >= 0xc0 then pos + 1 + Utf.get_extralen c0 else pos + 1
    in
    extuni c s ep 0 (String.length s) utf
  in
  let cluster_end (cs : int list) : int = cluster_end_at (enc cs) 0 true in
  (* Property pins for the code points used below (UCD 15.0.0, the
     vendored tables' version). *)
  assert (Int.equal (Ucd.gbprop 0x301) Ucp.ucp_gb_extend);
  assert (Int.equal (Ucd.gbprop 0x1100) Ucp.ucp_gb_l);
  assert (Int.equal (Ucd.gbprop 0x1161) Ucp.ucp_gb_v);
  assert (Int.equal (Ucd.gbprop 0x11a8) Ucp.ucp_gb_t);
  assert (Int.equal (Ucd.gbprop 0x1f600) Ucp.ucp_gb_extended_pictographic);
  (* GB9: e + COMBINING ACUTE (U+0301) is one cluster ("é"); a following
     base character starts a new one. *)
  assert (Int.equal (cluster_end [ 0x65; 0x301 ]) 3);
  assert (Int.equal (cluster_end [ 0x65; 0x301; 0x7a ]) 3);
  (* GB3/GB4: CRLF is one cluster; LF CR is two. *)
  assert (Int.equal (cluster_end [ 0x0d; 0x0a; 0x61 ]) 2);
  assert (Int.equal (cluster_end [ 0x0a; 0x0d ]) 1);
  (* GB6-GB8: Hangul jamo L V T form one cluster. *)
  assert (Int.equal (cluster_end [ 0x1100; 0x1161; 0x11a8 ]) 9);
  (* GB12/GB13: two Regional Indicators pair into one cluster; a third
     starts a new cluster (2 RIs = 1 cluster, 3 RIs = 2 clusters); an RI
     at an odd position in a run pairs with the one after it, not the one
     before. *)
  assert (Int.equal (cluster_end [ 0x1f1e6; 0x1f1e7 ]) 8);
  assert (Int.equal (cluster_end [ 0x1f1e6; 0x1f1e7; 0x1f1e8 ]) 8);
  assert (
    Int.equal (cluster_end_at (enc [ 0x1f1e6; 0x1f1e7; 0x1f1e8 ]) 8 true) 12);
  (* RI#2 of a run: one preceding RI (odd) forces a break after it. *)
  assert (
    Int.equal (cluster_end_at (enc [ 0x1f1e6; 0x1f1e7; 0x1f1e8 ]) 4 true) 8);
  (* GB11: EP + ZWJ + EP is one cluster, also with Extends after the first
     EP; a ZWJ + EP without a preceding EP breaks before the EP. *)
  assert (Int.equal (cluster_end [ 0x1f600; 0x200d; 0x1f600 ]) 11);
  assert (Int.equal (cluster_end [ 0x1f600; 0x301; 0x200d; 0x1f600 ]) 13);
  assert (Int.equal (cluster_end [ 0x200d; 0x1f600 ]) 3);
  (* Non-UTF mode steps one code unit at a time: CRLF still clusters;
     'a' + 0xcc (in Latin-1, a lead byte here) does not. *)
  assert (Int.equal (cluster_end_at "\x0d\x0a" 0 false) 2);
  assert (Int.equal (cluster_end_at "a\xcc" 0 false) 1)
