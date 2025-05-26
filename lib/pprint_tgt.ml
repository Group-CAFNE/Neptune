open Ast_tgt

(* Prints the data structure of Ast_tgt *)
let pp_tgt (ast : Ast_tgt.file) =
  (* Helper function for printing waveform *)
  let string_of_waveform = function
    | Ast_tgt.Noise -> "Noise" 
    | Ast_tgt.Vpulse -> "Vpulse"
    | Ast_tgt.Sawtooth -> "Sawtooth" 
    | Ast_tgt.Triangle -> "Triangle" 
  in
  
  (* Helper function to print a voice *)
  let print_voice name voice =
    Printf.printf "\nVoice %s:\n" name;
    List.iter (fun (id, wf) -> (*Iterate over the voice list*)
      (* Print the id and waveform *)
      Printf.printf "  %s: %s\n" id (string_of_waveform wf)
    ) voice
  in
  
  (* Print all voices *)
  print_voice "1" ast.vc1;
  print_voice "2" ast.vc2;
  print_voice "3" ast.vc3

  