open Ephemeral.Common

type arguments = {vector_tables: path list}

let parse_arguments () =
  let vector_tables = ref [] in
  let speclist = [] in
  let usage_msg = "Usage: ephemeral [VECTOR_TABLE]..." in
  Arg.parse speclist (fun vt -> vector_tables := vt :: !vector_tables) usage_msg ;
  {vector_tables= !vector_tables}
