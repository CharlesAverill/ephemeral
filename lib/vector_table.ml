(** Specifying and parsing JPL vector tables *)

open Common
open Logging

(** Vector Table Entry *)
type vtable_entry =
  { time: float  (** Time of entry w.r.t. epoch *)
  ; pos: vec3  (** Position vector *)
  ; vel: vec3  (** Velocity vector *)
  ; lt: float (* Newtonian light-time *)
  ; rt: float  (** Distance from coordinate center *)
  ; rr: float  (** Radial velocity *) }

(** String representation of [vtable_entry] *)
let string_of_vtable_entry (v : vtable_entry) : string =
  Printf.sprintf
    "@%f\n  X =%f Y =%f Z =%f\n  VX=%f VY=%f VZ=%f\n  LT=%f RT=%f RR=%f" v.time
    v.pos.x v.pos.y v.pos.z v.vel.x v.vel.y v.vel.z v.lt v.rt v.rr

(** Celestial body *)
type body = {name: string; id: int}

(** String representation of [body] *)
let string_of_body (b : body) : string = Printf.sprintf "%s (%d)" b.name b.id

(** Vector Table *)
type vtable =
  { center_body: body  (** Body at center of coordinate system *)
  ; target_body: body  (** Tracked body *)
  ; entries: vtable_entry list  (** Position, velocity data over time *) }

(** String representation of [vtable] *)
let string_of_vtable (v : vtable) : string =
  Printf.sprintf
    "===Center Body===\n%s\n===Target Body===\n%s\n===Entries===\n%s"
    (string_of_body v.center_body)
    (string_of_body v.target_body)
    (String.concat "\n" (List.map string_of_vtable_entry v.entries))

(** Min and max time tracked *)
let time_span (vt : vtable) : float * float =
  let open List in
  try ((nth vt.entries 0).time, (nth vt.entries (length vt.entries - 1)).time)
  with _ -> fatal rc_Error "Expected non-empty table in time_span"

(** Align a list of vtables to their common overlapping time window.
    Entries outside the window are dropped.
    Returns an empty list if there is no overlap. *)
let align (vts : vtable list) : vtable list =
  match vts with
  | [] | [_] ->
      vts
  | _ ->
      let spans = List.map time_span vts in
      let overlap_start =
        List.fold_left (fun acc (s, _) -> Float.max acc s) neg_infinity spans
      in
      let overlap_end =
        List.fold_left (fun acc (_, e) -> Float.min acc e) infinity spans
      in
      if overlap_start >= overlap_end then
        fatal rc_Error "Vector tables have no overlapping time window"
      else
        List.map
          (fun vt ->
            let entries =
              List.filter
                (fun e -> e.time >= overlap_start && e.time <= overlap_end)
                vt.entries
            in
            {vt with entries} )
          vts

(** Get the common center body from a list of [vtable]s, or throw an error *)
let common_center (vts : vtable list) : body =
  match vts with
  | [] ->
      fatal rc_Error "Expected non-empty list of vector tables"
  | h :: t ->
      let center = h.center_body in
      if List.exists (fun vt -> vt.center_body <> center) t then
        fatal rc_Error
          "Expected all bodies to be centered around %s, but found multiple \
           centers"
          center.name
      else
        center
