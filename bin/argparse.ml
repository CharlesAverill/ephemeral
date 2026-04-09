open Ephemeral
open Common
open Render

type arguments =
  { vector_tables: path list
  ; render_video: string option
  ; speed: int
  ; title: string }

let parse_arguments () =
  let vector_tables = ref [] in
  let render_video = ref None in
  let min_speed, max_speed = (0, Array.length speeds - 1) in
  let speed = ref !default_speed_idx in
  let title = ref "" in
  let speclist =
    [ ( "--speed"
      , Arg.Int (fun n -> speed := clamp n min_speed max_speed)
      , Printf.sprintf
          {|   <int>   Sets the starting speed of the simulation
                     (%d-%d, default is %d)|}
          min_speed max_speed !speed )
    ; ( "--record"
      , Arg.String (fun s -> render_video := Some s)
      , {|  <path>  Runs one loop of the ephemerides and calls
                     ffmpeg to render a video from the frames|}
      )
    ; ( "--title"
      , Arg.Set_string title
      , {|  <str>   Draws text to the top of the screen|} ) ]
  in
  let usage_msg = "Usage: ephemeral [OPTION]... [VECTOR_TABLE]..." in
  Arg.parse speclist (fun vt -> vector_tables := vt :: !vector_tables) usage_msg ;
  { vector_tables= !vector_tables
  ; render_video= !render_video
  ; speed= !speed
  ; title= !title }
