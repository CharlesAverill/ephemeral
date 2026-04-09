open Argparse
open Ephemeral
open Common
open Parser
open Vector_table
open Render

let () =
  let args = Argparse.parse_arguments () in
  Printf.printf "Vector Tables: %s\n" (String.concat "," args.vector_tables) ;
  (* Parse tables from files *)
  let vtables = List.map parse args.vector_tables in
  (* Align table time periods *)
  let vtables = align vtables in
  (* List.iter (fun s -> string_of_vtable s |> print_endline) vtables *)
  init vtables ; render ()
