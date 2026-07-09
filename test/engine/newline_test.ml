(* Engine module-initialization asserts for [Pcre2_engine.Newline], migrated
   verbatim from src/engine/newline.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Newline

(* PRIV(is_newline) over whole subjects (endptr = length, startptr = 0),
   checked against pcre2_newline.c:78-145 by hand: NLTYPE_ANY matches LF,
   VT, FF, CR (length 2 when an LF follows before endptr, else 1), NEL and
   (in UTF mode) LS/PS; NLTYPE_ANYCRLF matches only CR, LF, CRLF. *)
let test_0 () =
  let probe_is type_ subj pos utf =
    let len = ref 0 in
    let hit = is_newline subj type_ pos (String.length subj) len utf in
    (hit, !len)
  in
  (* ANY, non-UTF: LF / VT / FF are newlines of length 1. *)
  (match probe_is nltype_any "\n" 0 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\x0b" 0 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\x0c" 0 false with
  | true, 1 -> ()
  | _ -> assert false);
  (* ANY: CRLF has length 2; CR before non-LF and CR at the last position
     (the endptr boundary in pcre2_newline.c:119) have length 1. *)
  (match probe_is nltype_any "\r\na" 0 false with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\rx" 0 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "a\r" 1 false with
  | true, 1 -> ()
  | _ -> assert false);
  (* endptr short of a following LF: same CR arm, length 1 not 2. *)
  (let len = ref 0 in
   assert (is_newline "\r\n" nltype_any 0 1 len false);
   assert (Int.equal !len 1));
  (* ANY, non-UTF: NEL is the single code unit 0x85, length 1; the 0xC2
     lead byte of its UTF-8 encoding is NOT a newline without utf. *)
  (match probe_is nltype_any "a\x85b" 1 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\xc2\x85" 0 false with
  | false, _ -> ()
  | _ -> assert false);
  (* ANY, utf: NEL decodes from 0xC2 0x85, length 2; LS U+2028 and PS
     U+2029 decode from three code units, length 3. *)
  (match probe_is nltype_any "\xc2\x85" 0 true with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\xe2\x80\xa8" 0 true with
  | true, 3 -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\xe2\x80\xa9" 0 true with
  | true, 3 -> ()
  | _ -> assert false);
  (* ANY: an ordinary character is not a newline; another multi-byte
     character (U+00E9) is not a newline in UTF mode. *)
  (match probe_is nltype_any "a" 0 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_is nltype_any "\xc3\xa9" 0 true with
  | false, _ -> ()
  | _ -> assert false);
  (* ANYCRLF: LF 1, CR 1, CRLF 2; VT / FF / NEL (either encoding) / LS are
     NOT newlines. *)
  (match probe_is nltype_anycrlf "\n" 0 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "a\r" 1 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "\r\n" 0 false with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "\x0b" 0 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "\x0c" 0 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "a\x85b" 1 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "\xc2\x85" 0 true with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_is nltype_anycrlf "\xe2\x80\xa8" 0 true with
  | false, _ -> ()
  | _ -> assert false);
  (* FALSE leaves *lenptr untouched, like the C. *)
  let len = ref 42 in
  assert (not (is_newline "abc" nltype_any 0 3 len false));
  assert (Int.equal !len 42)

(* PRIV(was_newline), checked against pcre2_newline.c:168-241: [pos] points
   just past the candidate newline; CRLF is recognized from its LF with the
   CR looked up behind, guarded by startptr. *)
let test_1 () =
  let probe_was type_ subj pos utf =
    let len = ref 0 in
    let hit = was_newline subj type_ pos 0 len utf in
    (hit, !len)
  in
  (* ANY, non-UTF: LF preceded by CR gives length 2; plain LF, VT, FF, CR
     give length 1. *)
  (match probe_was nltype_any "a\r\n" 3 false with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "a\n" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "a\x0b" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "a\x0c" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "a\r" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (* startptr boundary (pcre2_newline.c:206): the LF is at the start of the
     string, so no CR can be looked up before it — length 1. *)
  (match probe_was nltype_any "\nx" 1 false with
  | true, 1 -> ()
  | _ -> assert false);
  (* Same boundary via a non-zero startptr: the CR exists in the string but
     lies before startptr, so it is not consulted. *)
  (let len = ref 0 in
   assert (was_newline "\r\n" nltype_any 2 1 len false);
   assert (Int.equal !len 1));
  (* ANY, non-UTF: NEL single byte, length 1. *)
  (match probe_was nltype_any "a\x85" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (* ANY, utf: BACKCHAR walks back over the continuation byte(s); NEL is
     length 2, LS/PS length 3. *)
  (match probe_was nltype_any "a\xc2\x85" 3 true with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "\xe2\x80\xa8" 3 true with
  | true, 3 -> ()
  | _ -> assert false);
  (match probe_was nltype_any "\xe2\x80\xa9" 3 true with
  | true, 3 -> ()
  | _ -> assert false);
  (* ANY: not a newline (also multi-byte non-newline in UTF mode). *)
  (match probe_was nltype_any "ab" 2 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_was nltype_any "\xc3\xa9" 2 true with
  | false, _ -> ()
  | _ -> assert false);
  (* ANYCRLF: CRLF 2, LF 1, CR 1; VT and NEL are NOT newlines. *)
  (match probe_was nltype_anycrlf "\r\n" 2 false with
  | true, 2 -> ()
  | _ -> assert false);
  (match probe_was nltype_anycrlf "a\n" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_was nltype_anycrlf "a\r" 2 false with
  | true, 1 -> ()
  | _ -> assert false);
  (match probe_was nltype_anycrlf "a\x0b" 2 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_was nltype_anycrlf "a\x85" 2 false with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_was nltype_anycrlf "a\xc2\x85" 3 true with
  | false, _ -> ()
  | _ -> assert false);
  (* Every byte before [pos] a continuation byte (reachable only under
     PCRE2_MATCH_INVALID_UTF): the pinned defined behavior for C 10.44's
     out-of-bounds BACKCHAR walk (see the DEVIATION note in was_newline)
     reads 0 at position -1 — never a newline. The "\x85t" case
     distinguishes it from clamping the walk at position 0, which would
     misread the NEL continuation byte as a newline under NLTYPE_ANY. *)
  (match probe_was nltype_anycrlf "\xb9t" 1 true with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_was nltype_any "\x85t" 1 true with
  | false, _ -> ()
  | _ -> assert false);
  (match probe_was nltype_any "\xb9\x85t" 2 true with
  | false, _ -> ()
  | _ -> assert false);
  (* FALSE leaves *lenptr untouched, like the C. *)
  let len = ref 42 in
  assert (not (was_newline "ab" nltype_any 2 0 len false));
  assert (Int.equal !len 42)

let tests =
  [
    Alcotest.test_case "newline 0" `Quick test_0;
    Alcotest.test_case "newline 1" `Quick test_1;
  ]
