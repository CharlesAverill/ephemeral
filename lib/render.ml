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
  ref {shape= Circle; filled= false; size= 20; color= blue; table= None}

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
                8 )
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
    vts ;
  let pad v =
    if v >= 0. then
      v *. 1.1
    else
      v *. 1.1
  in
  min_x := pad !min_x ;
  max_x := pad !max_x ;
  min_y := pad !min_y ;
  max_y := pad !max_y

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
  | Triangle ->
      let half = size in
      let pts = [|(x, y + half); (x - half, y - half); (x + half, y - half)|] in
      if filled then
        fill_poly pts
      else
        draw_poly pts
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
      let max_extent =
        Float.max
          (Float.max (Float.abs !min_x) (Float.abs !max_x))
          (Float.max (Float.abs !min_y) (Float.abs !max_y))
      in
      let max_extent =
        if max_extent = 0. then
          1.
        else
          max_extent
      in
      let scale =
        Float.min (float_of_int sx) (float_of_int sy) /. (2. *. max_extent)
      in
      let px = int_of_float ((x *. scale) +. float_of_int cx) in
      let py = int_of_float ((y *. scale) +. float_of_int cy) in
      (* Offset outward from center by the reference body's pixel radius *)
      let dx = px - cx in
      let dy = py - cy in
      let dist = Float.sqrt (float_of_int ((dx * dx) + (dy * dy))) in
      if dist = 0. then
        (px, py)
      else
        let r = float_of_int !reference_body.size in
        let factor = (dist +. r) /. dist in
        ( cx + int_of_float (float_of_int dx *. factor)
        , cy + int_of_float (float_of_int dy *. factor) )

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

let speeds = [1; 2; 5; 10; 25; 50]

let render () =
  let frame_time = 1.0 /. 60.0 in
  try
    let entry_idx = ref 0 in
    let speed_idx = ref 0 in
    while true do
      let t_start = gettimeofday () in
      (* Poll events *)
      let status = wait_next_event [Key_pressed; Poll] in
      (* ESC to quit *)
      if status.keypressed then
        match Graphics.read_key () with
        | '\027' ->
            raise Exit
        | '.' ->
            if !speed_idx < List.length speeds - 1 then
              speed_idx := !speed_idx + 1
        | ',' ->
            if 0 < !speed_idx then speed_idx := !speed_idx - 1
        | _ ->
            ()
      else
        render_frame status !entry_idx ;
      entry_idx := !entry_idx + List.nth speeds !speed_idx ;
      (* 60fps cap *)
      let t_end = gettimeofday () in
      let elapsed = t_end -. t_start in
      let sleep_time = frame_time -. elapsed in
      if sleep_time > 0.0 then ignore (Unix.sleepf sleep_time)
    done
  with Exit -> close_graph ()
