(* Script-run checking for the pure-OCaml PCRE2 10.44 port (8-bit
   library).

   Ported from vendor/pcre2/src/pcre2_script_run.c: PRIV(script_run)
   (84-342), the function for checking a script run. This port always has
   Unicode support, so only the SUPPORT_UNICODE side exists (the C's
   no-Unicode fallback at 336-341 is unreachable: pcre2_compile gives an
   error for script runs without Unicode support, pcre2_script_run.c:
   58-61).

   Pointer mapping (the frames.ml/interpreter.ml convention): the C's
   PCRE2_SPTR arguments become int offsets into [subject], which is
   passed alongside (DEVIATION: the C embeds the base in its pointers).

   Helpers are top-level and fully applied at every call site (no
   closures); the only allocations are the two per-call map arrays,
   mirroring the C's stack arrays (89-90). *)

(* pcre2_script_run.c:73-79 — states in the checking process. *)
let script_unset = 0 (* Requirement as yet unknown *)
let script_map = 1 (* Bitmap contains acceptable scripts *)
let script_hanpending = 2 (* Have had only Han characters *)
let script_hanhirakata = 3 (* Expect Han or Hirikata *)
let script_hanbopomofo = 4 (* Expect Han or Bopomofo *)
let script_hanhangul = 5 (* Expect Han or Hangul *)

(* pcre2_script_run.c:81-82 — the ucd_script_sets items only have bits
   for scripts less than ucp_Unknown (those that appear in script
   extension lists); the full-size maps have a bit for every script. *)
let ucd_mapsize = (Ucp.ucp_unknown / 32) + 1
let full_mapsize = (Ucp.ucp_script_count / 32) + 1

(* pcre2_script_run.c:199-202 — the SCRIPT_HANPENDING accumulator bits. *)
let found_bopomofo = 1
let found_hiragana = 2
let found_katakana = 4
let found_hangul = 8

(* pcre2_internal.h:1896-1898 — MAPBIT(map,n) = map[n/32] & (1u<<(n%32))
   and MAPSET(map,n) = map[n/32] |= (1u<<(n%32)) over a full-size local
   map. In bounds: every [n] passed below is a script number
   < ucp_Script_Count, so n lsr 5 <= full_mapsize - 1. *)
let mapbit (map : int array) (n : int) : int =
  map.(n lsr 5) land (1 lsl (n land 31))

let mapset (map : int array) (n : int) : unit =
  map.(n lsr 5) <- map.(n lsr 5) lor (1 lsl (n land 31))

(* pcre2_script_run.c:246-255 — the SCRIPT_MAP OK scan: any word where
   require_map & map is nonzero. *)
let rec maps_intersect (require_map : int array) (map : int array) (i : int) :
    bool =
  i < full_mapsize
  && ((not (Int.equal (require_map.(i) land map.(i)) 0))
     || (maps_intersect [@tailcall]) require_map map (i + 1))

(* pcre2_script_run.c:153-290 — "Handle the different checking states":
   the switch(require_state), factored out of the loop. [map] is the
   character's full-size script map (already set up); [require_map] is
   updated in place where the C does. Returns the next require_state, or
   -1 for the C's `return FALSE` sites. *)
let handle_state (require_state : int) (script : int) (map : int array)
    (require_map : int array) : int =
  if Int.equal require_state script_unset then
    (* pcre2_script_run.c:157-185 — first significant character — it
       might follow Common or Inherited characters that do not have any
       script extensions. *)
    if Int.equal script Ucp.ucp_han then script_hanpending
    else if
      Int.equal script Ucp.ucp_hiragana || Int.equal script Ucp.ucp_katakana
    then script_hanhirakata
    else if Int.equal script Ucp.ucp_bopomofo then script_hanbopomofo
    else if Int.equal script Ucp.ucp_hangul then script_hanhangul
    else (
      Array.blit map 0 require_map 0 full_mapsize;
      script_map)
  else if Int.equal require_state script_hanpending then
    (* pcre2_script_run.c:187-224 — the first significant character was
       Han. Another Han does nothing; otherwise classify the character's
       extension list against the four "with Han" scripts. *)
    if not (Int.equal script Ucp.ucp_han) then
      let chspecial =
        (if not (Int.equal (mapbit map Ucp.ucp_bopomofo) 0) then found_bopomofo
         else 0)
        lor (if not (Int.equal (mapbit map Ucp.ucp_hiragana) 0) then
               found_hiragana
             else 0)
        lor (if not (Int.equal (mapbit map Ucp.ucp_katakana) 0) then
               found_katakana
             else 0)
        lor
        if not (Int.equal (mapbit map Ucp.ucp_hangul) 0) then found_hangul
        else 0
      in
      if Int.equal chspecial 0 then -1 (* Not allowed with Han *)
      else if Int.equal chspecial found_bopomofo then script_hanbopomofo
      else if Int.equal chspecial (found_hiragana lor found_katakana) then
        script_hanhirakata
      else
        (* pcre2_script_run.c:221-222 — otherwise this character must be
           allowed with all of them, so remain in the pending state. *)
        script_hanpending
    else script_hanpending
  else if Int.equal require_state script_hanhirakata then
    (* pcre2_script_run.c:226-231 — previously encountered one of the
       "with Han" scripts: check that this character is appropriate. *)
    if
      Int.equal
        (mapbit map Ucp.ucp_han
        + mapbit map Ucp.ucp_hiragana
        + mapbit map Ucp.ucp_katakana)
        0
    then -1
    else require_state
  else if Int.equal require_state script_hanbopomofo then
    (* pcre2_script_run.c:234-235 *)
    if Int.equal (mapbit map Ucp.ucp_han + mapbit map Ucp.ucp_bopomofo) 0 then
      -1
    else require_state
  else if Int.equal require_state script_hanhangul then
    (* pcre2_script_run.c:238-239 *)
    if Int.equal (mapbit map Ucp.ucp_han + mapbit map Ucp.ucp_hangul) 0 then -1
    else require_state
  else if
    (* pcre2_script_run.c:242-289 — SCRIPT_MAP: previously encountered
       one or more characters that are allowed with a list of scripts. *)
    not (maps_intersect require_map map 0)
  then -1
  else if
    (* pcre2_script_run.c:259-287 — the rest of the string must be in
       this script, but we have to allow for the Han complications. *)
    Int.equal script Ucp.ucp_han
  then script_hanpending
  else if Int.equal script Ucp.ucp_hiragana || Int.equal script Ucp.ucp_katakana
  then script_hanhirakata
  else if Int.equal script Ucp.ucp_bopomofo then script_hanbopomofo
  else if Int.equal script Ucp.ucp_hangul then script_hanhangul
  else (
    (* pcre2_script_run.c:281-286 — compute the intersection of the
       required list of scripts and the allowed scripts for this
       character. *)
    for i = 0 to full_mapsize - 1 do
      require_map.(i) <- require_map.(i) land map.(i)
    done;
    script_map)

(* pcre2_script_run.c:307-322 — identify a decimal digit's set by the
   offset of its '9' character in PRIV(ucd_digit_sets): the binary chop
   (the caller has excluded the initial <= ucd_digit_sets[1] fast
   case). *)
let rec digit_chop (c : int) (bot : int) (top : int) : int =
  if top <= bot + 1 then top (* <= rather than == is paranoia *)
  else
    let mid = (top + bot) / 2 in
    if c <= Ucd_tables.ucd_digit_sets.(mid) then
      (digit_chop [@tailcall]) c bot mid
    else (digit_chop [@tailcall]) c mid top

(* pcre2_script_run.c:305-322 — uint32_t digitset for character [c]
   (known to be chartype Nd): an initial check of the first value picks
   up ASCII digits quickly, otherwise the binary chop. *)
let digitset_of (c : int) : int =
  if c <= Ucd_tables.ucd_digit_sets.(1) then 1
  else digit_chop c 1 Ucd_tables.ucd_digit_sets.(0)

(* pcre2_script_run.c:125-334 — the main checking for(;;) loop, one call
   per character. [c] is the current character, [ptr] the offset after
   it; [require_state]/[require_digitset] are the C locals of those
   names, loop-carried; [require_map] persists across iterations and
   [map] is per-character scratch, both caller-allocated. Bounds: reads
   at ptr happen only when ptr < endptr <= String.length subject, and
   ptr >= 0 (the entry advanced it from the caller's ptr >= 0). *)
let rec check_loop (subject : string) (endptr : int) (utf : bool)
    (require_map : int array) (map : int array) (c : int) (ptr : int)
    (require_state : int) (require_digitset : int) : bool =
  (* pcre2_script_run.c:127-128 — const ucd_record *ucd = GET_UCD(c);
     uint32_t script = ucd->script. *)
  let ri = Ucd.record_index c in
  let script = Ucd_tables.script ri in
  (* pcre2_script_run.c:130-133 — if the script is Unknown, the string is
     not a valid script run. Such characters can only form script runs of
     length one (see test in [script_run]). *)
  if Int.equal script Ucp.ucp_unknown then false
  else
    (* pcre2_script_run.c:135-139 — a character without any script
       extensions whose script is Inherited or Common is always accepted
       with any script. If there are extensions, the following processing
       happens for all scripts. *)
    let scriptx = Ucd.scriptx_prop ri in
    let require_state =
      if
        (not (Int.equal scriptx 0))
        || (not (Int.equal script Ucp.ucp_inherited))
           && not (Int.equal script Ucp.ucp_common)
      then (
        (* pcre2_script_run.c:143-151 — set up a full-sized map for this
           character: copy the scriptx map (in bounds: scriptx is a word
           offset to a 3-word = ucd_mapsize item, the
           Ucd.script_set_contains proof), zero the rest, and, except for
           Common or Inherited, add this script's bit. *)
        Array.blit Ucd_tables.ucd_script_sets scriptx map 0 ucd_mapsize;
        Array.fill map ucd_mapsize (full_mapsize - ucd_mapsize) 0;
        if
          (not (Int.equal script Ucp.ucp_common))
          && not (Int.equal script Ucp.ucp_inherited)
        then mapset map script;
        handle_state require_state script map require_map)
      else require_state
    in
    if Int.equal require_state (-1) then false (* a `return FALSE` above *)
    else
      (* pcre2_script_run.c:293-328 — the character is in an acceptable
         script. We must now ensure that all decimal digits in the string
         come from the same set; a required value of 0 means "unset". *)
      let require_digitset =
        if Int.equal (Ucd_tables.chartype ri) Ucp.ucp_nd then
          let digitset = digitset_of c in
          if Int.equal require_digitset 0 then digitset
          else if not (Int.equal digitset require_digitset) then -1
          else require_digitset
        else require_digitset
      in
      if Int.equal require_digitset (-1) then false (* 327 — return FALSE *)
      else if
        (* pcre2_script_run.c:330-333 — if we haven't yet got to the end,
           pick up the next character. *)
        ptr >= endptr
      then true
      else
        (* GETCHARINCTEST(c, ptr) (333). *)
        (* safe: 0 <= ptr < endptr <= String.length subject (checked
           above; caller contract) *)
        let c0 = Char.code (String.unsafe_get subject ptr) in
        let c = if utf && c0 >= 0xc0 then Utf.getutf8 c0 subject ptr else c0 in
        let ptr =
          if utf && c0 >= 0xc0 then ptr + 1 + Utf.get_extralen c0 else ptr + 1
        in
        (check_loop [@tailcall]) subject endptr utf require_map map c ptr
          require_state require_digitset

(* pcre2_script_run.c:84-342 — PRIV(script_run): check a script run.

   Arguments (63-68):
     subject  the subject string (DEVIATION note above)
     ptr      offset of the first character
     endptr   offset after the last character
     utf      true if in UTF mode

   Returns: true if this is a valid script run. *)
let script_run (subject : string) (ptr : int) (endptr : int) (utf : bool) : bool
    =
  (* pcre2_script_run.c:98-102 — any string containing fewer than 2
     characters is a valid script run. *)
  if ptr >= endptr then true
  else
    (* GETCHARINCTEST(c, ptr) (101). *)
    (* safe: 0 <= ptr < endptr <= String.length subject (checked above;
       caller passes frame eptr values — mb invariant) *)
    let c0 = Char.code (String.unsafe_get subject ptr) in
    let c = if utf && c0 >= 0xc0 then Utf.getutf8 c0 subject ptr else c0 in
    let ptr =
      if utf && c0 >= 0xc0 then ptr + 1 + Utf.get_extralen c0 else ptr + 1
    in
    if ptr >= endptr then true
    else
      (* pcre2_script_run.c:88-91 + 104-109 — the checking state and the
         two full-size maps; the require map starts zeroed (Array.make;
         the C's explicit loop at 109). *)
      let require_map = Array.make full_mapsize 0 in
      let map = Array.make full_mapsize 0 in
      check_loop subject endptr utf require_map map c ptr script_unset 0
