(* Aggregates the per-module engine test suites (migrated from the former
   src/engine/*.ml module-initialization asserts) under one Alcotest runner.
   Linking these modules also replaces the old engine_init_tests.ml
   force-linking hack: each *_test module references its engine module
   directly. *)

let () =
  Alcotest.run "pcre2-engine"
    [
      (* [frames] runs FIRST: its last test pins the module-level scratch-arena
         slot as pristine (empty, not busy) and restores it — exactly as it saw
         things at module-init time, when Frames initialized before
         Engine/Compile/Interpreter. Any group that performs real matching
         (compile/engine/interpreter) acquires that shared slot, so ordering
         frames ahead of them reproduces the original load-order precondition. *)
      ("frames", Frames_test.tests);
      ("auto_possess", Auto_possess_test.tests);
      ("compile", Compile_test.tests);
      ("debug_printer", Debug_printer_test.tests);
      ("engine", Engine_test.tests);
      ("errors", Errors_test.tests);
      ("extuni", Extuni_test.tests);
      ("interpreter", Interpreter_test.tests);
      ("newline", Newline_test.tests);
      ("opcodes", Opcodes_test.tests);
      ("parse", Parse_test.tests);
      ("script_run", Script_run_test.tests);
      ("study", Study_test.tests);
      ("ucd", Ucd_test.tests);
      ("utf", Utf_test.tests);
      ("valid_utf", Valid_utf_test.tests);
      ("xclass", Xclass_test.tests);
    ]
