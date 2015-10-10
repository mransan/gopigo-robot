


type t 

val create: unit -> t 

val fwd : t -> unit Lwt.t 

val stop : t -> unit Lwt.t 

val decrease_speed : t -> unit Lwt.t 

val increase_speed : t -> unit Lwt.t 

val set_speed : t -> [`Left | `Right ] -> int -> unit Lwt.t 

val enable_encoder : t -> unit Lwt.t 

val move_servo : t -> int -> unit Lwt.t 

val read_encoder : t -> [`Left | `Right ] -> int Lwt.t 

val read_speed : t -> (int * int) Lwt.t 

val read_us_distance : t -> int Lwt.t 

val led : t -> [`On | `Off] -> [`Left | `Right] -> unit Lwt.t
