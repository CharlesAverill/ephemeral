(** Rendering vector tables *)

open Graphics
open Vector_table
open Unix

(** Body shapes *)
type shape = Circle | Square | Triangle | Point | Cross

(** Screen-space body *)
type body =
  { shape: shape  (** What shape the body will show as *)
  ; filled: bool  (** Whether the shape will be filled *)
  ; size: int
        (** Radius, side length, side length, N/A, width depending on [shape] *)
  ; color: color  (** Color of body *)
  ; table: vtable option  (** Corresponding vector table *) }

(** System's reference body *)
let reference_body =
  ref {shape= Circle; filled= false; size= 10; color= blue; table= None}

(** Target bodies in system *)
let targets = ref []

(** Bounds on the plane in terms of system coordinates *)
let min_x, max_x, min_y, max_y = (ref 0., ref 0., ref 0., ref 0.)

(** Initialize the rendering environment *)
let init (vts : vtable list) =
  open_graph "" ;
  set_window_title
    ( List.map (fun vt -> vt.target_body.name) vts
    |> String.concat ", "
    |> Printf.sprintf "ephemeral | %s" ) ;
  (* Initialize target bodies *)
  targets :=
    List.map
      (fun vt ->
        let is_spacecraft =
          String.ends_with ~suffix:"(spacecraft)" vt.target_body.name
        in
        { shape=
            ( if is_spacecraft then
                Triangle
              else
                Circle )
        ; filled= false
        ; size=
            ( if is_spacecraft then
                2
              else
                10 )
        ; color= white
        ; table= Some vt } )
      vts ;
  (* Initialize bounds *)
  List.iter
    (fun vt ->
      List.iter
        (fun entry ->
          if entry.pos.x < !min_x then min_x := entry.pos.x ;
          if entry.pos.y < !min_y then min_y := entry.pos.y ;
          if entry.pos.x > !max_x then max_x := entry.pos.x ;
          if entry.pos.y > !max_y then max_y := entry.pos.y )
        vt.entries )
    vts

let draw_shape (s : shape) (filled : bool) ((x, y) : int * int) (size : int)
    (color : color) =
  set_color color ;
  match s with
  | Circle ->
      let draw =
        if filled then
          fill_circle
        else
          draw_circle
      in
      draw x y size
  | _ ->
      ()

let screen_coords_of_body (entry_idx : int) (b : body) : int * int =
  let sx, sy = (size_x (), size_y ()) in
  let cx, cy = (sx / 2, sy / 2) in
  match b.table with
  | None ->
      (cx, cy)
  | Some table ->
      let entry =
        List.nth table.entries (entry_idx mod List.length table.entries)
      in
      let x, y = (entry.pos.x, entry.pos.y) in
      let x_range =
        if !max_x = !min_x then
          1.
        else
          !max_x -. !min_x
      in
      let y_range =
        if !max_y = !min_y then
          1.
        else
          !max_y -. !min_y
      in
      let px = int_of_float ((x -. !min_x) /. x_range *. float_of_int sx) in
      let py = int_of_float ((y -. !min_y) /. y_range *. float_of_int sy) in
      (px, py)

let render_frame (st : status) (entry_idx : int) =
  (* Clear screen *)
  set_color black ;
  fill_rect 0 0 (size_x ()) (size_y ()) ;
  (* Draw reference body *)
  draw_shape !reference_body.shape !reference_body.filled
    (size_x () / 2, size_y () / 2)
    !reference_body.size !reference_body.color ;
  (* Draw target bodies *)
  List.iter
    (fun body ->
      let x, y = screen_coords_of_body entry_idx body in
      draw_shape body.shape body.filled (x, y) body.size body.color )
    !targets

let render () =
  let frame_time = 1.0 /. 60.0 in
  try
    let entry_idx = ref 0 in
    while true do
      let t_start = gettimeofday () in
      (* Poll events *)
      let status = wait_next_event [Key_pressed; Poll] in
      (* ESC to quit *)
      if status.keypressed && status.key = '\027' then
        raise Exit
      else
        render_frame status !entry_idx ;
      entry_idx := !entry_idx + 50 ;
      (* 60fps cap *)
      let t_end = gettimeofday () in
      let elapsed = t_end -. t_start in
      let sleep_time = frame_time -. elapsed in
      if sleep_time > 0.0 then ignore (Unix.sleepf sleep_time)
    done
  with Exit -> close_graph ()
