open Argparse
open Ephemeral
open Common
open Parser
open Vector_table

let () =
  let args = Argparse.parse_arguments () in
  Printf.printf "Vector Tables: %s\n" (String.concat "," args.vector_tables) ;
  let vtables = List.map parse args.vector_tables in
  List.iter (fun s -> string_of_vtable s |> print_endline) vtables
