
type t 

val create : ?dampening:int -> unit -> t 

val speed : t -> float 

val time : t -> float 

val counter : t -> int 

val update_counter : t -> int -> unit  
