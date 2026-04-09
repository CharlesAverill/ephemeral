(** Common components *)

(** File paths *)
type path = string

(** XYZ vector *)
type vec3 = {x: float; y: float; z: float}

(** String representation of a vector *)
let string_of_vec3 (v : vec3) : string =
  Printf.sprintf "<%f, %f, %f>" v.x v.y v.z

(** Compute the magnitude of a vector *)
let magnitude (v : vec3) : float =
  Float.sqrt ((v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z))

(** Clamp a value between two boundaries, inclusive *)
let clamp (x : 'a) (min : 'a) (max : 'a) : 'a =
  if x < min then
    min
  else if x > max then
    max
  else
    x
