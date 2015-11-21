open Lwt.Infix 

let led_blink done_ gopigo delay = 
  Done.iter done_ (fun () -> 
    Gopigo.led gopigo `On `Right 
    >>=(fun () -> Lwt_unix.sleep delay)  
    >>=(fun () -> Gopigo.led gopigo `Off `Right) 
    >>=(fun () -> Gopigo.led gopigo `On  `Left) 
    >>=(fun () -> Lwt_unix.sleep delay)  
    >>=(fun () -> Gopigo.led gopigo `Off `Left) 
  )

let stop_if_too_close done_ gopigo ~delay ~distance = 
  Done.iter ~delay done_ (fun () -> 
    Gopigo.read_us_distance gopigo
    >>= (fun distance ->
      Lwt_io.printf "distance: %f\n" distance 
      >>= (fun () -> Lwt.return distance)
    ) 
    >>=(fun us_distance -> 
      if (us_distance > 0.01 && us_distance < distance )
      then (
        Done.interupt done_;
        Gopigo.stop gopigo
      ) 
      else Lwt.return_unit 
    )
  )

let stop_later done_ gopigo delay =
  let t0 = Unix.gettimeofday () in  
  Done.iter done_ ~delay:1. (fun () -> 
    if Unix.gettimeofday () -. t0 > delay
    then (
      Printf.eprintf ">>> done\n";
      Done.interupt done_; 
      Gopigo.stop gopigo
    )
    else Lwt.return_unit
  )  
