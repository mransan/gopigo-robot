(** GoPiGo interface *) 

type t 
(** GoPiGo handle *)

val create: unit -> t 
(** [create ()] creates a handle to the GoPiGo. Only one such handle should be
    done per task. 
 *)

val fwd : t -> unit Lwt.t 
(** [fwd t] send the forward command to the GoPiGo *)

val stop : t -> unit Lwt.t 
(** [stop t] send the stop command to the GoPiGo *)

val decrease_speed : t -> unit Lwt.t 
(** [decrease_speed t] decreases the speed of the GoPiGo *)

val increase_speed : t -> unit Lwt.t 
(** [increase_speed t] increases the speed of the GoPiGo *)

val set_speed : t -> [`Left | `Right ] -> int -> unit Lwt.t 
(** [set_speed t `Left speed] sets the [`Left] motor speed to [speed] *) 

val enable_encoder : t -> unit Lwt.t 
(** [enable_encoder t] sends the enable_encoder command to the GoPiGo *)

val move_servo : t -> int -> unit Lwt.t 
(** [move_servo t angle] moves the servo to the [angle] direction *)

val read_encoder : t -> [`Left | `Right ] -> int Lwt.t 
(** [read_encoder t `Left] returns the [`Left] encoder counter *)

val read_speed : t -> (int * int) Lwt.t 
(** [read_speed t] returns the speed [(left, right)] of the motors *)

val read_us_distance : t -> int Lwt.t 
(** [read_us_distance t] reads the ultra sonic distance in cm *)

val led : t -> [`On | `Off] -> [`Left | `Right] -> unit Lwt.t
(** [lef t `On `Left] turn the [`Left] led [`On] *)
