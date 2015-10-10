(** Simple coordination primitive used to interupt multiple recursive 
    threads. 
  *) 


type t 
(** abstract type - mutable *) 

val create : unit -> t  
(** [create ()] creates a new [t] *)

val interupt : t -> unit 
(** [interupt t] mark [t] as done *)

val exec: ?delay:float -> t -> (unit -> unit Lwt.t) -> unit Lwt.t 
(** [exec ~delay t f] will run [f] once after [delay] unless [t] has been
    interupted. 
  *) 

val iter: ?delay:float -> t -> (unit -> unit Lwt.t) -> unit Lwt.t 
(** [iter ~delay t f] executes [f] every [delay] until [t] is interupted. 
 *)  
