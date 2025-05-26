open OUnit2

module SYM = C64MC.Symbol_table
module AST_SRC = C64MC.Ast_src
module AST_TGT = C64MC.Ast_tgt
module PP_TGT = C64MC.Pprint_tgt
module PP_SRC = C64MC.Pprint_src

(* _ctx is a OUnit2 optional parameter that passes some testing context *)

(* Setup and teardown mostly used for symbol table tests. Takes a function as a parameter *)
let setup_and_teardown test_fn _ctx =

  (* Clear the symbol table before the test *)
  let symbol_table = SYM.get_symbol_table () in
  Hashtbl.clear symbol_table;
  
  let id = "new_seq" in (* Mock new seq id *)
  let seq : AST_SRC.seq = [ (* Create mock sequence *)
    Sound (C, Nat, Whole, Defined 4);
    Rest Whole;
    Sound (D, Nat, Whole, Defined 5)
  ] in
  (* Run the passed test *)
  test_fn id seq;
  
  (* Clear the symbol table after the test *)
  Hashtbl.clear symbol_table

(*---------------- TEST SUITE SETUP ----------------*)

(*Setup of the test suite. The suite consist of sub suites*)
let suite =
  "All Tests" >:::[
    "Symbol Table Tests" >::: [
      "test_add_sequence1" >:: setup_and_teardown Test_symbol_table.test_add_sequence1;
      "test_add_sequence2" >:: setup_and_teardown Test_symbol_table.test_add_sequence2;
      "test_check_sequence" >:: Test_symbol_table.test_check_sequence;
      "test_get_sequenc1" >:: setup_and_teardown Test_symbol_table.test_get_sequence1;
      "test_get_sequenc2" >:: setup_and_teardown Test_symbol_table.test_get_sequence2;
      "test_update_sequence1" >:: setup_and_teardown Test_symbol_table.test_update_sequence1;
      ];
      "Codegen Tests" >::: [
        "test_construct_instruction1" >:: Test_codegen.test_construct_instruction1;
        "test_construct_instruction2" >:: Test_codegen.test_construct_instruction2;
        "test_construct_instruction3" >:: Test_codegen.test_construct_instruction3;
        "test_construct_instruction4" >:: Test_codegen.test_construct_instruction4;
        "test_int_to_hex" >:: Test_codegen.test_int_to_hex;
        "test_int_to_hex_negative" >:: Test_codegen.test_int_to_hex_negative;
       ];
      "AST Translate Tests" >::: [
        "test_base_offset" >:: Test_ast_translate.test_base_offset;
        "test_acc_offset" >:: Test_ast_translate.test_acc_offset;
        "test_oct_offset" >:: Test_ast_translate.test_oct_offset;
        "test_get_note_duration" >:: Test_ast_translate.test_get_note_duration;
        "test_get_duration_ref" >:: Test_ast_translate.test_get_duration_ref;
        "test_note_translate_sound" >:: Test_ast_translate.test_note_translate_sound;
        "test_note_translate_rest" >:: Test_ast_translate.test_note_translate_rest;
        "test_seq_translate" >:: Test_ast_translate.test_seq_translate;
        "test_waveform_translate" >:: Test_ast_translate.test_waveform_translate;
        "test_ident_translate" >:: Test_ast_translate.test_ident_translate;
        "test_voice_translate" >:: Test_ast_translate.test_voice_translate;
      ];
  ]


(* Runs the actual tests *)  
let () = run_test_tt_main suite
