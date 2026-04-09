(** Rendering vector tables *)

open Graphics
open Ephemeral
open Vector_table
open Common
open Unix
open Logging
open Settings

(** Guess a body's shape from its name *)
let shape_of_name (s : string) : shape =
  if
    List.exists (( = ) s)
      [ "Sun"
      ; "Mercury"
      ; "Venus"
      ; "Earth"
      ; "Moon"
      ; "Mars"
      ; "Jupiter"
      ; "Saturn"
      ; "Uranus"
      ; "Neptune"
      ; "Pluto" ]
  then
    Circle
  else if contains s "Station" then
    H
  else if contains s "(spacecraft)" then
    Triangle
  else
    Point

(** Guess a body's color from its name *)
let color_of_name : string -> color = function
  | "Mercury" ->
      rgb 137 131 131
  | "Venus" ->
      rgb 228 216 202
  | "Earth" ->
      blue
  | "Mars" ->
      rgb 190 113 85
  | "Jupiter" ->
      rgb 194 131 88
  | "Saturn" ->
      rgb 251 250 220
  | "Uranus" ->
      rgb 208 233 242
  | "Neptune" ->
      rgb 159 186 195
  | _ ->
      white

(** Guess a body's size from its shape *)
let size_of_shape (s : shape) : int =
  match s with
  | Circle ->
      10
  | Triangle ->
      3
  | H ->
      6
  | Point ->
      0
  | Cross ->
      6
  | Square ->
      5

(** Guess a body's size from its name *)
let size_of_name (s : string) : int =
  match s with
  | "Sun" ->
      20
  | "Jupiter" ->
      16
  | "Saturn" ->
      14
  | "Uranus" ->
      12
  | "Neptune" ->
      11
  | "Earth" ->
      10
  | "Venus" ->
      10
  | "Mars" ->
      7
  | "Mercury" ->
      6
  | "Moon" ->
      5
  | "Pluto" ->
      4
  | _ ->
      size_of_shape (shape_of_name s)

(** Mouse drag state: Some (last_x, last_y) when button held *)
let drag_origin = ref None

(** Apply the current view rotation to a 3D point, returning projected (x, y) *)
let rotate_point (x : float) (y : float) (z : float) : float * float =
  let ct = Float.cos !view_theta and st = Float.sin !view_theta in
  let cp = Float.cos !view_phi and sp = Float.sin !view_phi in
  (* Rotate around Z by theta (azimuth) *)
  let x' = (x *. ct) -. (y *. st) in
  let y' = (x *. st) +. (y *. ct) in
  let z' = z in
  (* Rotate around X by phi (elevation) *)
  let y'' = (y' *. cp) -. (z' *. sp) in
  (* x'' = x' unchanged, z'' unused - orthographic projection *)
  (x', y'')

let draw_axes (entry_idx : int) =
  let sx, sy = (size_x (), size_y ()) in
  let cx, cy = (sx / 2, sy / 2) in
  (* Compute scale exactly like screen_coords_of_body *)
  let max_extent =
    if !dynamic_scale then
      List.fold_left
        (fun acc t ->
          match t.table with
          | None ->
              acc
          | Some tbl ->
              let e =
                List.nth tbl.entries (entry_idx mod List.length tbl.entries)
              in
              let ex, ey = rotate_point e.pos.x e.pos.y e.pos.z in
              Float.max acc (Float.max (Float.abs ex) (Float.abs ey)) )
        1. !targets
    else
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
  let scale = float !reference_body.size in
  (* Basis vectors *)
  let axes =
    [(1., 0., 0.) (* X *); (0., 1., 0.) (* Y *); (0., 0., 1.) (* Z *)]
  in
  set_color (rgb 180 180 180) ;
  (* light gray *)
  List.iter
    (fun (x, y, z) ->
      let rx, ry = rotate_point x y z in
      let px = int_of_float (rx *. scale) in
      let py = int_of_float (ry *. scale) in
      moveto cx cy ;
      lineto (cx + px) (cy + py) )
    axes

(** Initialize the rendering environment *)
let init (vts : vtable list) (speed : int) (title : string) =
  open_graph "" ;
  auto_synchronize false ;
  set_window_title
    ( List.map (fun vt -> vt.target_body.name) vts
    |> String.concat ", "
    |> Printf.sprintf "ephemeral | %s" ) ;
  (* Initialize parameters *)
  default_speed_idx := speed ;
  title_text :=
    if title = "" then
      None
    else
      Some title ;
  (* Initialize bodies *)
  let center = common_center vts in
  reference_body :=
    { shape= shape_of_name center.name
    ; filled= false
    ; size= size_of_name center.name
    ; color= color_of_name center.name
    ; table= None } ;
  targets :=
    List.map
      (fun vt ->
        let is_spacecraft =
          String.ends_with ~suffix:"(spacecraft)" vt.target_body.name
        in
        { shape= shape_of_name vt.target_body.name
        ; filled= false
        ; size= size_of_name vt.target_body.name
        ; color= color_of_name vt.target_body.name
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

(** Draw a body's shape on the screen *)
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
      let draw =
        if filled then
          fill_poly
        else
          draw_poly
      in
      let half = size in
      let pts = [|(x, y + half); (x - half, y - half); (x + half, y - half)|] in
      draw pts
  | H ->
      let pts =
        [| (x - size, y, x + size, y)
         ; (x - size, y - size, x - size, y + size)
         ; (x + size, y - size, x + size, y + size) |]
      in
      draw_segments pts
  | Square ->
      let draw =
        if filled then
          fill_poly
        else
          draw_poly
      in
      let half = size / 2 in
      let pts =
        [| (x - half, y - half)
         ; (x - half, y + half)
         ; (x + half, y + half)
         ; (x + half, y - half) |]
      in
      draw pts
  | Point ->
      draw_circle x y 1
  | Cross ->
      let half = size / 2 in
      draw_segments [|(x - half, y, x + half, y); (x, y - half, x, y + half)|]

(** Compute a body's screen coordinates from its real coordinates *)
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
      let rx, ry = rotate_point entry.pos.x entry.pos.y entry.pos.z in
      let max_extent =
        if !dynamic_scale then
          List.fold_left
            (fun acc t ->
              match t.table with
              | None ->
                  acc
              | Some tbl ->
                  let e =
                    List.nth tbl.entries (entry_idx mod List.length tbl.entries)
                  in
                  let ex, ey = rotate_point e.pos.x e.pos.y e.pos.z in
                  Float.max acc (Float.max (Float.abs ex) (Float.abs ey)) )
            1. !targets
        else
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
        Float.min (float_of_int sx) (float_of_int sy)
        /. (2. *. max_extent *. 1.1)
      in
      let px = int_of_float ((rx *. scale) +. float_of_int cx) in
      let py = int_of_float ((ry *. scale) +. float_of_int cy) in
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

(** Convert an epoch timestamp to text *)
let format_time (t : float) : string =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

(** Render an individual frame *)
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
    !targets ;
  (* Draw title *)
  ( match !title_text with
  | None ->
      ()
  | Some tt ->
      set_color white ;
      let dimx, dimy = text_size tt in
      moveto ((size_x () / 2) - (dimx / 2)) (size_y () - 8 - dimy) ;
      draw_string tt ) ;
  (* Draw timestamp *)
  match !targets with
  | [] ->
      ()
  | body :: _ ->
      ( match body.table with
      | None ->
          ()
      | Some table ->
          let idx = entry_idx mod List.length table.entries in
          let entry = List.nth table.entries idx in
          set_color white ;
          moveto 8 8 ;
          draw_string (format_time entry.time) ) ;
      draw_axes entry_idx

(** Main render loop *)
let render () =
  let frame_time = 1.0 /. 60.0 in
  try
    let entry_idx = ref 0 in
    let speed_idx = ref !default_speed_idx in
    let frame_count = ref 0 in
    let paused = ref false in
    while true do
      let t_start = gettimeofday () in
      let status =
        wait_next_event [Key_pressed; Button_down; Button_up; Mouse_motion; Poll]
      in
      (* Mouse drag to rotate *)
      if status.button then (
        match !drag_origin with
        | None ->
            drag_origin := Some (status.mouse_x, status.mouse_y)
        | Some (lx, ly) ->
            let dx = status.mouse_x - lx in
            let dy = status.mouse_y - ly in
            let sensitivity = 0.005 in
            view_theta := !view_theta +. (float_of_int dx *. sensitivity) ;
            view_phi := !view_phi -. (float_of_int dy *. sensitivity) ;
            (* Clamp phi to avoid flipping past poles *)
            view_phi :=
              Float.max (-.Float.pi /. 2.)
                (Float.min (Float.pi /. 2.) !view_phi) ;
            drag_origin := Some (status.mouse_x, status.mouse_y)
      ) else
        drag_origin := None ;
      ( if status.keypressed then
          match Graphics.read_key () with
          | '\027' ->
              raise Exit
          | '.' ->
              if !speed_idx < Array.length speeds - 1 then incr speed_idx
          | ',' ->
              if !speed_idx > 0 then decr speed_idx
          | '/' ->
              speed_idx := 4
          | ' ' ->
              paused := not !paused
          | 'z' ->
              dynamic_scale := not !dynamic_scale
          | 'r' ->
              view_theta := 0. ;
              view_phi := 0.
          | _ ->
              () ) ;
      render_frame status !entry_idx ;
      ( if not !paused then
          match speeds.(!speed_idx) with
          | Slow n ->
              incr frame_count ;
              if !frame_count >= n then begin
                frame_count := 0 ;
                incr entry_idx
              end
          | Fast n ->
              frame_count := 0 ;
              entry_idx := !entry_idx + n ) ;
      let elapsed = gettimeofday () -. t_start in
      let sleep_time = frame_time -. elapsed in
      if sleep_time > 0.0 then ignore (Unix.sleepf sleep_time) ;
      synchronize ()
    done
  with Exit -> close_graph ()

(** Render to a video file *)
let record (filename : string) =
  let entries_len =
    match !targets with
    | [] ->
        fatal rc_Error "No targets to record"
    | body :: _ -> (
      match body.table with
      | None ->
          fatal rc_Error "No table on target"
      | Some table ->
          List.length table.entries )
  in
  let sx, sy = (size_x (), size_y ()) in
  let cmd =
    Printf.sprintf
      "ffmpeg -y -framerate 60 -f rawvideo -pixel_format rgb24 -video_size \
       %dx%d -i pipe:0 -pix_fmt yuv420p '%s'"
      sx sy filename
  in
  let pipe = Unix.open_process_out cmd in
  let entry_idx = ref 0 in
  let frame_count = ref 0 in
  let speed = speeds.(!default_speed_idx) in
  let total_frames =
    match speed with
    | Slow n ->
        entries_len * n
    | Fast n ->
        (entries_len + n - 1) / n
  in
  Printf.printf "Recording %d frames...\n%!" total_frames ;
  let rec loop frames_remaining =
    if frames_remaining <= 0 then
      ()
    else (
      render_frame
        {keypressed= false; key= ' '; mouse_x= 0; mouse_y= 0; button= false}
        !entry_idx ;
      synchronize () ;
      let img = Graphics.get_image 0 0 sx sy in
      let mat = Graphics.dump_image img in
      for row = 0 to sy - 1 do
        for col = 0 to sx - 1 do
          let c = mat.(row).(col) in
          output_byte pipe ((c lsr 16) land 0xff) ;
          output_byte pipe ((c lsr 8) land 0xff) ;
          output_byte pipe (c land 0xff)
        done
      done ;
      (* Advance according to speed *)
      ( match speed with
      | Slow n ->
          incr frame_count ;
          if !frame_count >= n then (
            frame_count := 0 ;
            incr entry_idx
          )
      | Fast n ->
          entry_idx := !entry_idx + n ) ;
      loop (frames_remaining - 1)
    )
  in
  loop total_frames ;
  match Unix.close_process_out pipe with
  | Unix.WEXITED 0 ->
      Printf.printf "Saved to %s\n%!" filename
  | Unix.WEXITED n ->
      Printf.printf "ffmpeg exited with code %d\n%!" n
  | _ ->
      Printf.printf "ffmpeg terminated abnormally\n%!"
