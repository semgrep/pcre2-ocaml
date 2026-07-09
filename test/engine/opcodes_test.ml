(* Engine module-initialization asserts for [Pcre2_engine.Opcodes], migrated
   verbatim from src/engine/opcodes.ml into Alcotest test cases (test
   infrastructure migration; assertion bodies are unchanged). *)

open Pcre2_engine
open Pcre2_engine.Opcodes

(* Consistency checks mirroring the C's use of OP_TABLE_LENGTH to catch
   updating errors in tables indexed by opcode. *)
let test_0 () = assert (Int.equal op_ucp_word_boundary (op_table_length - 1))
let test_1 () = assert (Int.equal (Array.length op_names) op_table_length)
let test_2 () = assert (Int.equal (Array.length op_lengths) op_table_length)

let tests =
  [
    Alcotest.test_case "opcodes 0" `Quick test_0;
    Alcotest.test_case "opcodes 1" `Quick test_1;
    Alcotest.test_case "opcodes 2" `Quick test_2;
  ]
