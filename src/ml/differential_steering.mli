(** Localization routines for a differential steering robot *)

type t 
(** Position *) 

type robot_geometry = {
  counts_per_revolution : int; 
  wheel_diameter: float;
  b: float; (** distance between wheels *) 
}
(** robot geometry - static information *)

val create: robot_geometry:robot_geometry -> right_encoder:int -> left_encoder:int -> unit -> t 
(** [create ~robot_geometry ~right_encoder ~left_encoder] create a new robot
    position at (0, 0)
 *)

val x : t -> float 
(** [x t] returns the position on the X-axis *)

val y : t -> float 
(** [y t] returns the position on the Y-axis *)

val theta : t -> float 
(** [theta t] returns the orientation of the robot *)

val update : t -> int -> int -> unit 
(** [update t right_counter left_counter] updates the position of the robot
    based on new [right_counter] and [left_counter] values 
 *)
