open Ast_src


(* Prints the data structure of Ast_src as is*)
let pp_src (ast : Ast_src.file) =
  (* Helper functions for printing components *)
  let string_of_tone = function
    | A -> "A" | B -> "B" | C -> "C" | D -> "D"
    | E -> "E" | F -> "F" | G -> "G" in
  
  let string_of_acc = function
    | Nat -> "" | Sharp -> "#" | Flat -> "b" in
  
  let string_of_frac = function
    | Whole -> "1" | Half -> "1/2" | Quarter -> "1/4"
    | Eighth -> "1/8" | Sixteenth -> "1/16" in
  
  let string_of_oct = function
    | None -> "" | Defined n -> string_of_int n in
  
  let string_of_note = function 
  (* Turns the note into a string by concatenating the components *)
    | Sound (tone, acc, frac, oct) ->
        Printf.sprintf "%s%s %s %s"
          (string_of_tone tone) (string_of_acc acc)
          (string_of_frac frac) (string_of_oct oct)
    | Rest frac -> Printf.sprintf "Rest %s" (string_of_frac frac) in
  
  let string_of_waveform = function
    | Noise -> "Noise" | Vpulse -> "Vpulse"
    | Sawtooth -> "Sawtooth" | Triangle -> "Triangle" in
  
  (* Print parameters *)
  Printf.printf "Parameters:\n";
  (match ast.parameters.tempo with
   | Some t -> Printf.printf "  Tempo: %d\n" t
   | None -> Printf.printf "  Tempo: None\n");
  (match ast.parameters.timesig with
   | Some (n, d) -> Printf.printf "  Time Signature: %d/%d\n" n d
   | None -> Printf.printf "  Time Signature: None\n");
  (match ast.parameters.stdpitch with
   | Some p -> Printf.printf "  Standard Pitch: %d\n" p
   | None -> Printf.printf "  Standard Pitch: None\n");
  
  (* Print sequences *)
  Printf.printf "\nSequences:\n";
  List.iter (fun seqdef ->
    Printf.printf "  %s: [" seqdef.name;
    List.iter (fun note ->
      Printf.printf "%s; " (string_of_note note)
    ) seqdef.seq;
    Printf.printf "]\n\n"
  ) ast.sequences;
  
  (* Print voices *)
  let print_voice name voice =
    Printf.printf "\nVoice %s:\n" name;
    List.iter (fun (id, wf) ->
      Printf.printf "  %s: %s\n" id (string_of_waveform wf)
    ) voice
  in
  print_voice "1" ast.voice1;
  print_voice "2" ast.voice2;
  print_voice "3" ast.voice3



