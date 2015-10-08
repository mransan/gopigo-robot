
type t = {
  mutable counter : int; 
  mutable time    : float; 
  mutable speed   : float option; 
}

let create () = {
  counter = 0; 
  time    = 0.;
  speed   = None; 
}

let speed {speed; _ } = match speed with 
  | None -> 0.
  | Some x -> x 

let update_counter ({counter; time; speed } as speed_measure) new_counter = 
  if new_counter - counter > 5 
  then (
    let new_time = Unix.gettimeofday () in 
    begin 
      match speed with 
      | None   -> speed_measure.speed  <- Some 0. 
      | Some _ -> 
        let new_speed = float_of_int (new_counter - counter) /. (new_time -. time) in 
        speed_measure.speed <- Some new_speed
    end; 
    speed_measure.counter <- new_counter; 
    speed_measure.time    <- new_time; 
    ()
  )
  else () 
