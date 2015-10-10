
type t = {
  total_weight : float; 
  prev_weight  : float; 
  mutable counter : int; 
  mutable time    : float; 
  mutable speed   : float option; 
}

let create ?dampening:(dampening = 0) () = {
  prev_weight = float_of_int dampening;
  total_weight = float_of_int @@ dampening+1;
  counter = 0; 
  time    = 0.;
  speed   = None; 
}

let speed {speed; _ } = match speed with 
  | None -> 0.
  | Some x -> x 

let counter {counter; _ } = counter 

let time {time; _ } = time 

let update_counter ({counter; time; speed; prev_weight; total_weight} as speed_measure) new_counter = 
  if new_counter - counter >= 0 
  then (
    let new_time = Unix.gettimeofday () in 
    begin 
      match speed with 
      | None   -> speed_measure.speed  <- Some 0. 
      | Some prev  -> 
        let new_speed = float_of_int (new_counter - counter) /. (new_time -. time) in 
        speed_measure.speed <- Some ( (new_speed +. prev_weight *. prev ) /.
        total_weight) 
    end; 
    speed_measure.counter <- new_counter; 
    speed_measure.time    <- new_time; 
    ()
  )
  else () 
