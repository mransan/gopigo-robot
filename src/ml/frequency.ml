
type t = {
  total_weight : float; 
  prev_weight : float; 
  mutable time_to_next_update : float;
  mutable last_counter_increment : float; 
  mutable counter : int; 
  mutable time : float; 
  mutable frequency : float option; 
}

let create ?dampening:(dampening = 0) () = {
  time_to_next_update = 1000000.; 
  last_counter_increment = 0.; 
  prev_weight = float_of_int dampening;
  total_weight = float_of_int @@ dampening+1;
  counter = 0; 
  time    = Unix.gettimeofday ();
  frequency   = None; 
}

let value {frequency; _ } = match frequency with 
  | None -> 0.
  | Some x -> x 

let counter {counter; _ } = counter 

let last_observation_time {time; _ } = time 

let update_counter t new_counter = 
  let new_time = Unix.gettimeofday () in 
  match t.frequency with 
  | None -> ( 
    (* Initialization *) 
    t.frequency   <- Some 0.; 
    t.counter <- new_counter; 
    t.time    <- new_time; 
  )
  | Some previous_frequency -> (
    let delta_t = new_time -. t.time in 
    let delta_counter = new_counter  - t.counter in 
    if delta_counter = 0
    then ( 
      (* When the counter is the same it means our sampling frequency 
         is much greater than the actual measured frequency. 

         To improve the smoothness of the computed frequency if the time
         is greater than the expected 'next measure' only then we start 
         reducing the frequency, anticipating a slowness before the next 
         count happens. 
      *)
      if delta_t > t.time_to_next_update 
      then ( 
        t.time_to_next_update <- 1.1 *. delta_t; 
        t.frequency <-  Some (t.last_counter_increment /. t.time_to_next_update);
      )
    )
    else (
      let new_frequency = float_of_int (new_counter - t.counter) /. (new_time -.  t.time) in 
      t.frequency <- Some ( (new_frequency +. t.prev_weight *. previous_frequency) /. t.total_weight); 
      t.last_counter_increment <- float_of_int delta_counter; 
      t.time_to_next_update <- t.last_counter_increment /. new_frequency; 
      t.time    <- new_time; 
      t.counter <- new_counter; 
    )
  )
