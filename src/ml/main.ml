open Lwt.Infix 

let lwt_ignore _ = Lwt.return_unit 


let () = 
  let fd = Control.create () in  

  let t0 = Unix.gettimeofday () in 

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
  
  let us_distance = ref 100 in 

  let main_t = 
    Control.fwd fd
  in 

  let rec stop_if_too_closed () = 
    let now = Unix.gettimeofday () in 
    if !us_distance < 10 || (now -.  t0 > 20.)  
    then (
      done_ := true; 
      Control.stop fd 
    ) 
    else until_done ~delay:0.1 stop_if_too_closed
  in 

  let rec blink_loop () = 
    let delay = 0.05 in
    Control.led fd `On `Right 
    >>=(fun () -> Lwt_unix.sleep delay)  
    >>=(fun () -> Control.led fd `Off `Right) 
    >>=(fun () -> Control.led fd `On  `Left) 
    >>=(fun () -> Lwt_unix.sleep delay)  
    >>=(fun () -> Control.led fd `Off `Left) 
    >>=(fun () -> until_done blink_loop) 
  in 
  
  let m1_speed = Speed.create () in 
  let m2_speed = Speed.create () in 

  let rec update_counter_loop ()  = 
    Control.read_counter fd 0  
    >|= Speed.update_counter m1_speed 
    >>= (fun () -> 
      Control.read_counter fd 1 
      >|= Speed.update_counter m2_speed 
    )
    >>= (fun () -> Control.read_us_distance fd) 
    >|= (fun x  -> us_distance := x) 
    >>= (fun () -> until_done update_counter_loop)
  in   

  let rec rotate_servo increasing = function 
    | i when i < 20   -> rotate_servo true 20
    | i when i > 160  -> rotate_servo false 160 
    | i -> (
      Control.move_servo fd i 
      >>= (fun () -> 
        until_done ~delay:1.0 (fun () -> 
          if increasing 
          then rotate_servo increasing (i + 5) 
          else rotate_servo increasing (i - 5) 
        )
      )
    )
  in 

  let rec print_speed_loop () = 
    Lwt_io.printf " %5f |  %5f | %3i cm \n" 
      (Speed.speed m1_speed) (Speed.speed m2_speed) !us_distance  
    >>=(fun () -> until_done ~delay:0.1 print_speed_loop)
  in 

  let read_encoder () = 
    Control.enable_encoder fd 
    >>= update_counter_loop 
  in 

  Lwt_main.run (
    Lwt.join [
      main_t;
      stop_if_too_closed (); 
      read_encoder (); 
      print_speed_loop (); 
      blink_loop (); 
      (* rotate_servo true 90; *)  
    ] 
  )
