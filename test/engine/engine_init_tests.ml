(* Referencing one value per assert-carrying module forces the linker to
   include it, which runs its module-init assert suite. Extend when a new
   engine module gains init asserts. *)
let () =
  ignore (Sys.opaque_identity Pcre2_engine.Parse.meta_end);
  ignore (Sys.opaque_identity Pcre2_engine.Compile.compile_work_size);
  ignore (Sys.opaque_identity Pcre2_engine.Opcodes.op_table_length);
  ignore (Sys.opaque_identity (Pcre2_engine.Errors.message 114));
  ignore (Sys.opaque_identity Pcre2_engine.Engine.version);
  ignore (Sys.opaque_identity (Pcre2_engine.Chartables.lcc (Char.code 'A')));
  ignore (Sys.opaque_identity Pcre2_engine.Ucd_tables.ucd_block_size);
  ignore (Sys.opaque_identity Pcre2_engine.Frames.frame_header_ints);
  ignore (Sys.opaque_identity Pcre2_engine.Interpreter.match_match);
  print_endline "engine module-init asserts: OK"
