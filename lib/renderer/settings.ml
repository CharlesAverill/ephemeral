(** Global settings for the renderer *)

open Ephemeral.Vector_table
open Graphics

(** Body shapes *)
type shape = Circle | Square | Triangle | Point | Cross | H

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
let targets : body list ref = ref []

(** Bounds on the plane in terms of system coordinates *)
let min_x, max_x, min_y, max_y = (ref 0., ref 0., ref 0., ref 0.)

(** Frame advancement speeds *)
type speed =
  | Slow of int  (** Advance 1 entry every n frames *)
  | Fast of int  (** Advance n entries every frame *)

(** Pre-selected speeds *)
let speeds =
  [| Slow 16
   ; Slow 8
   ; Slow 4
   ; Slow 2
   ; Fast 1
   ; Fast 2
   ; Fast 5
   ; Fast 10
   ; Fast 25
   ; Fast 50 |]

let find_index (f : 'a -> bool) (arr : 'a array) : int option =
  fst
    (Array.fold_right
       (fun i (found, idx) ->
         ( ( match found with
           | Some _ ->
               found
           | None ->
               if f i then
                 Some idx
               else
                 None )
         , idx + 1 ) )
       arr (None, 0) )

(** Index of initial speed setting *)
let default_speed_idx =
  ref
    ( match find_index (( = ) (Fast 1)) speeds with
    | None ->
        [%unreachable]
    | Some idx ->
        idx )

(** Title text to be drawn at the top of the screen *)
let title_text : string option ref = ref None

(** Whether to use per-frame dynamic scaling *)
let dynamic_scale = ref false

(** Azimuth *)
let view_theta = ref 0.

(** Elevation *)
let view_phi = ref 0.
