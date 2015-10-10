

type t 

val create: float -> float -> int -> int -> unit -> t 

val x : t -> float 
val y : t -> float 
val theta : t -> float 

val update : t -> int -> int -> unit 
