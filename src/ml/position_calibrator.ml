
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


  let done_ = ref false in 
  
  let until_done ?delay f = 
    if !done_ 
    then Lwt.return_unit 
    else (
      match delay with 
      | None -> f () 
      | Some delay -> Lwt_unix.sleep delay >>= f 
    ) 
  in 
  
  let set_speed i () = 
    Gopigo.set_speed fd `Left i 
    >>=(fun () -> Gopigo.set_speed fd `Right (i -5 ))
  in 

  let main_t = 
    set_speed speed () 
    >>= (fun () -> Gopigo.fwd fd)
    >>= (fun () -> Lwt_unix.sleep total_time)
    >>= (fun () -> 
      done_ := true; 
      Gopigo.stop fd 
    ) 
  in 

  let update_t = 
    let rec loop () = 
      Gopigo.read_encoder fd `Right 
      >>=(fun x -> 
        Gopigo.read_encoder fd `Left 
        >|= (fun y -> (x, y))
      )
      >|=(fun (right_counter, left_counter) -> 
        Differential_steering.update robot right_counter left_counter
      )
      >>= (fun () -> until_done ~delay:pos_update_time loop)
    in 
    loop () 
  in 

  let print_t = 
    let rec loop () = 
      Lwt_io.printf "x: %10.3f | y: %10.3f | theta: %10.3f | \n"
        (Differential_steering.x robot)
        (Differential_steering.y robot)
        (Differential_steering.theta robot)
      >>=(fun () -> until_done ~delay:pos_update_time loop)
    in
    loop () 
  in 

  Lwt_main.run (Lwt.join [
    main_t; update_t; print_t;
  ])
