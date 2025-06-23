open InstructionSet
open Exceptions
open List
open C64MC.Ast_tgt

module Target_Ast = C64MC.Ast_tgt
module Sym = C64MC.Symbol_table
module Runtime_options = C64MC.Runtime_options


(* This module generates assembly code for the C64 music compiler.
   It constructs instructions and writes them to a file. *)

(* Global file_name for the output file *)
let file_name = ref "output.asm"

(* Clean the output file*)
let clean_build ()=
  let oc = open_out !file_name in
  close_out oc


(* Write a line to a file without overwriting existing lines *)
let write_line_tf (line : string) =
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 !file_name in
  Printf.fprintf oc "%s\n" line;
  close_out oc

(* Write the standard library to a file *)
(* This function appends snippets from the stdlib to the output file *)
let write_stdlib (stdlib_string : string) =
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 !file_name in
  Printf.fprintf oc "%s\n" stdlib_string;
  close_out oc

(* Construct an instruction from its mnemonic and arguments *)
let construct_instruction (mnemonic : string) (args : string list) =
  let instruction = List.find_opt (fun instr -> instr.mnemonic = mnemonic) instructions in
  
  match instruction with (* find the instruction in the instruction set *)
    | None -> raise (InstructionNotFoundException (Printf.sprintf "Instruction '%s' not found" mnemonic))
    | Some instr -> (*Check if instruction has correct arguments/amount of arguments*)
      let arg_count = List.length args in
      (* Print debug information if the debug option is enabled *)
      Runtime_options.conditional_option [Runtime_options.get_debug] (fun () ->
        Printf.printf "Constructing instruction: %s with arguments: %s\n" mnemonic (String.concat ", " args));
      
      if arg_count < instr.min_args then
        raise (InsufficientInstructionArgumentsException (mnemonic, instr.min_args, arg_count))
      
      else if arg_count > instr.max_args then
        raise (TooManyInstructionArgumentsException (mnemonic, instr.max_args, arg_count))
      
      else
        let constructed_instruction = Printf.sprintf "%s %s" instr.mnemonic (String.concat ", " args) in
        constructed_instruction (* Return full instruction as string*)



(* Construct a labelled instruction block/indented instruction block *)
let write_instr_group (instructions : string list) =
  let indentation = String.make (4 * 4) ' ' in  (* 4 tabs, 4 spaces each = 16 spaces *)
  List.iter (fun instruction ->
    write_line_tf (indentation ^ instruction)
  ) instructions


(*Converts the waveform into the hex byte for assembly*)
let waveform_to_byte = function
  | Noise -> "$F9"
  | Vpulse -> "$FA"
  | Sawtooth -> "$FB"
  | Triangle -> "$FC"

  
  (* Function to write code in the output.asm file
    General structure:
    Write voice label to file;
    Iterate through voice (ident * waveform) list;
    Write waveform, enter-sequence instruction and sequence id to file;
    Write repeat voice instruction;
    repeat for the two other voices
  *)
let gen_voice (file : Target_Ast.file) =
  write_line_tf (";; Voice Data");
  let write_voice label voice =
    write_line_tf (label ^ ":");
    if (voice = []) then 
      write_instr_group [
        construct_instruction "dc.b" ["$00, $00, $00"];
      ] 
    else
      List.iter (fun (id, waveform) ->
        write_instr_group [
          construct_instruction "dc.b" [waveform_to_byte waveform];
          construct_instruction "dc.b" ["$FE"];
          construct_instruction "dc.w" [id]
        ]
      ) voice;

    write_instr_group ["dc.b $FD"; "dc.w " ^ label];
  in
  write_voice "voice1" file.vc1;
  write_voice "voice2" file.vc2;
  write_voice "voice3" file.vc3
  
  (* Converts an integer to a hexadecimal string *)
  let int_to_hex (n : int) : string =
    if n < 0 then
      raise (InvalidArgumentException "Negative integers cannot be converted to hexadecimal")
    else
      Printf.sprintf "%02X" n (* 2 character, automatically converted hexidecimal number, add zero in front if only one character *)


(*Function to generate code for the sequences in assembly
  Gets the symbol_table and iterates through each sequence.
  For each sequence it writes the identifier
  Then go through the list of notes and write "dc.b $lofreq, $hifreq, $duration"
  Ends by writing dc.b $FF to signal the end of the sequence.  
*)
let gen_sequence () =
  write_line_tf (";; Sequence Data");
  let symbol_table = Sym.get_symbol_table () in
  Hashtbl.iter (fun id value -> 
      match value with
        | Sym.SequenceSymbol {seq} -> (*The note lists are here somewhere*)
          match seq with
            | Sym.FinalSequence seq -> (* The definition of the note lists from the target AST *)
              write_line_tf (id ^ ":"); (*Write the label*)

              let instruction_list = ref [] in (* Create a mutable and appendable list *)

              List.iter (fun note ->  (*Iterate through the sequences in the symbol table*)
                let next_instruction = (*Create the new instruction*)
                  construct_instruction "dc.b" [ 
                      "$" ^ int_to_hex note.lowfreq; 
                      "$" ^ int_to_hex note.highfreq; 
                      "$" ^ int_to_hex note.duration;
                  ] in
                instruction_list := next_instruction :: !instruction_list; (*Append the new instruction to the front of the list of instructions*)
              ) seq;
              
              instruction_list := construct_instruction "dc.b" ["$FF"] :: !instruction_list;    (*Appends instruction_list with required dc.b $FF*)
              instruction_list := List.rev !instruction_list;   (* Reverses the instruction_list list*)
              write_instr_group !instruction_list     (*Write the instructions in the output.asm file*)
            | Sym.RawSequence _ -> assert false; (*Do nothing, this is for the source AST*)             

        (* Print the last $FF instruction after processing each symbol *)
  ) symbol_table
  
  

let run_example () =
  write_stdlib Stdlib.init;
  write_stdlib Stdlib.playinit;
  write_stdlib Stdlib.note_initialisation;
  write_stdlib Stdlib.play_loop;
  write_stdlib Stdlib.fetches;
  write_stdlib Stdlib.voice_data; 
  write_stdlib Stdlib.instrument_data

