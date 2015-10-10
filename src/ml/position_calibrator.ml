
open Lwt.Infix 

let pi = 4. *. atan 1.0 

let total_time      = 3.
let pos_update_time = 0.5  
let speed           = 55


let () = 

  let fd = Gopigo.create () in 

  
  let right_encoder, left_encoder = 
    Lwt_main.run (
      Gopigo.read_encoder fd `Right 
      >>=(fun x -> 
        Gopigo.read_encoder fd `Left 
        >|= (fun y -> (x, y))
      )
    ) in 

  let conversion_ratio = pi *. 0.063 /. 18. in  
  let b = 0.111 in 
  let robot = Differential_steering.create 
    conversion_ratio b right_encoder left_encoder () in 

  let done_ = Done.create () in 
  
  let set_speed i () = 
    Gopigo.set_speed fd `Left i 
    >>=(fun () -> Gopigo.set_speed fd `Right (i -5 ))
  in 

  let main_t = 
    set_speed speed () 
    >>= (fun () -> Gopigo.fwd fd)
    >>= (fun () -> Lwt_unix.sleep total_time)
    >>= (fun () -> 
      Done.interupt done_;
      Gopigo.stop fd 
    ) 
  in 

  let update_t = 
    Done.iter ~delay:pos_update_time done_ (fun () ->
      Gopigo.read_encoder fd `Right 
      >>=(fun x -> 
        Gopigo.read_encoder fd `Left 
        >|= (fun y -> (x, y))
      )
      >|=(fun (right_counter, left_counter) -> 
        Differential_steering.update robot right_counter left_counter
      )
    )
  in 

  let print_t = 
    Done.iter ~delay:pos_update_time done_ (fun () -> 
      Lwt_io.printf "x: %10.3f | y: %10.3f | theta: %10.3f | \n"
        (Differential_steering.x robot)
        (Differential_steering.y robot)
        (Differential_steering.theta robot)
    )
  in 

  Lwt_main.run (Lwt.join [
    main_t; update_t; print_t;
  ])
