(** Collection of small independent robot behaviors *) 

val led_blink : Done.t -> Gopigo.t -> float -> unit Lwt.t 
(** [led_blink done_ delay] starts blinking the led with [delay] seconds between
    left and right led. 
 *)

val stop_if_too_close : Done.t -> Gopigo.t -> delay:float -> distance:float -> unit Lwt.t 
(** [stop_if_too_close done_ gopigo ~delay ~distance] will interupt [done_] if
    the ultra sonic distance is less than [distance] (in meters). 
  *)

val stop_later : Done.t -> Gopigo.t -> float -> unit Lwt.t 
(** [stop_later done_ gopigo delay] will stop the robot after [delay] seconds *)
