(** Frequency calculator 
   
   This module provides functionality for calculating the live frequency
   of a particular event. 

   In order to calculate the frequency this module relies on the 
   client capacity to observe a counter. This counter value 
   is typically measured by a sensor like an encoder. 
*)  
  
type t 
(** Frequency type *) 

val create : ?dampening:int -> unit -> t 
(** [create ~dampening ()] create a new frequency value. The [dampening] value
    indicates how much 'smoothing' should be done over time by averaging
    previously measured frequency with the latest sampling. A value of 0 means
    that no dampening is done. A value of 1 indicates that the updated frequency
    value will be the average of the latest measurement and the previous one. 
  *)

val value : t -> float 
(** [value frequency] returns the value in Hz of the frequncy *)

val last_observation_time : t -> float 
(** [last_observation_time frequncy] returns the time since the last counter
    value was measured. 
 *)

val counter : t -> int 
(** [counter frequncy] returns the current counter value. *)

val update_counter : t -> int -> unit  
(** [update_counter frequency counter] update the frequency counter *)
