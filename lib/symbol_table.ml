
open Exceptions

(* Variant types, so the seq can hold both the sequence of the source and target AST concurrently *)
type sequence_type = 
  | RawSequence of Ast_src.seq
  | FinalSequence of Ast_tgt.note list

type symbol_info = (* a record data structure used for grouping related information *)
  | SequenceSymbol of { (* the key is the sequence id, the value is sequence *)
    seq : sequence_type; (* list of notes *)
  }


(* Creating symbol table *)
let symbol_table : (string, symbol_info) Hashtbl.t = Hashtbl.create 10


(*------------------------------Helper Functions------------------------------*)


(* This function adds a sequence to the symbol table. If a sequence with the specified id already exists
an error will be thrown. *)
let add_sequence id seq = 
  if Hashtbl.mem symbol_table id then 
    raise (SyntaxErrorException "Sequences id's cannot be duplicated. Each sequence must have a unique id.");

  let symbol = SequenceSymbol {seq = RawSequence seq} in 
  Hashtbl.add symbol_table id symbol;
  if Runtime_options.get_sym_tab () then
    Printf.printf "Added sequence: %s \n" id


(* This function checks if the sequence id exists. If not, an error will be thrown. *)
let check_sequence id =
  if not (Hashtbl.mem symbol_table id) then
    raise (SyntaxErrorException "Sequences must be defined before adding to a voice")


(* This function is used in the translator when translating from the source AST to the target AST.
 If a sequence with the specified id exists in the symbol table, it will be replaced with 
 with the correlated updated sequence from the target AST. If no such sequence exist in the
 symbol table, an error will be thrown. *)
let update_sequence id seq = 
  if not (Hashtbl.mem symbol_table id) then
    raise (SyntaxErrorException "This sequence could not be updated as it does not exist");

  let symbol = SequenceSymbol {seq = FinalSequence seq} in 
  Hashtbl.replace symbol_table id symbol;
  
  if Runtime_options.get_sym_tab () then (
    (* Print the updated sequence id *)
    Printf.printf "Updated sequence: %s \n" id;

    (* We check if the sequence has been successfully updated using pretty print *)
    match symbol with
      | SequenceSymbol {seq = FinalSequence final_seq} ->
        Printf.printf "  Sequence contains %d notes\n" (List.length final_seq);
        List.iteri (fun i _ -> 
          Printf.printf "Note %d\n" (i + 1) 
        ) final_seq
      | _ -> raise (SyntaxErrorException "Updated sequence not found")
  )

(* This function retrieves a sequence (value) from the symbol table by the id (key). 
  If no sequence matching the specified id is found in the symbol table, an error is thrown. *)
let get_sequence id =
  match Hashtbl.find_opt symbol_table id with 
    | Some (SequenceSymbol {seq}) -> seq
    | None -> raise (SyntaxErrorException ("Sequence not found for id: " ^ id))


(* This function retrieves the whole symbol table containing sequence ID's and symbol info *)
let get_symbol_table () = symbol_table;