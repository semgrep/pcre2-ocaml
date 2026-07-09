(* Engine module-initialization asserts for [Pcre2_engine.Errors], migrated
   verbatim from src/engine/errors.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Errors

(* Table-length checks: compile texts cover codes 100..201 (ERR0..ERR101),
   match texts cover codes 0..-67 (through PCRE2_ERROR_INVALIDOFFSET,
   pcre2.h.generic:409). *)
let test_0 () = assert (Int.equal (Array.length compile_error_texts) 102)
let test_1 () = assert (Int.equal (Array.length match_error_texts) 68)

let tests =
  [
    Alcotest.test_case "errors 0" `Quick test_0;
    Alcotest.test_case "errors 1" `Quick test_1;
  ]
