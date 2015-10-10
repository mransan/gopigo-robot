open Lwt.Infix 

let lwt_ignore _ = Lwt.return_unit 

let () = 
  let fd = Gopigo.create () in  

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

  let m1_speed = Speed.create () in 
  let m2_speed = Speed.create () in 

  let m1_set_speed = ref 45 in 
  let m2_set_speed = ref 57 in 
  
  let calculate_speed measured set = 
    let diff = 12. -. measured in 
    if diff > 0. 
    then min 90 (set + 1)  
    else max 0  (set - 1)  
  in 

  let main_t = 
    let rec loop () = 
      m1_set_speed := calculate_speed (Speed.speed m1_speed) !m1_set_speed;
      m2_set_speed := calculate_speed (Speed.speed m2_speed) !m2_set_speed;
      Gopigo.set_speed fd `Left !m1_set_speed 
      >>= (fun () -> Gopigo.set_speed fd `Right !m2_set_speed) 
      >>= (fun () -> Lwt_unix.sleep 0.2)
      >>= (fun () -> until_done loop )
    in 
    Gopigo.set_speed fd `Left 35
    >>= (fun () -> Gopigo.set_speed fd `Right 47) 
    >>= (fun () -> Gopigo.fwd fd) 
    >>= (fun () -> loop ())
  in 

  let rec stop_if_too_closed () = 
    let now = Unix.gettimeofday () in 
    if (!us_distance > 0 && !us_distance < 20 )|| (now -.  t0 > 20.)  
    then (
      done_ := true; 
      Gopigo.stop fd 
    ) 
    else until_done ~delay:0.5 stop_if_too_closed
  in 

  let rec blink_loop () = 
    let delay = 0.5 in
    Gopigo.led fd `On `Right 
    >>=(fun () -> Lwt_unix.sleep delay)  
    >>=(fun () -> Gopigo.led fd `Off `Right) 
    >>=(fun () -> Gopigo.led fd `On  `Left) 
    >>=(fun () -> Lwt_unix.sleep delay)  
    >>=(fun () -> Gopigo.led fd `Off `Left) 
    >>=(fun () -> until_done blink_loop) 
  in 
  

  let motor_speed = ref (0, 0) in 

  let rec update_counter_loop ()  = 
    Gopigo.read_encoder fd `Left 
    >|= Speed.update_counter m1_speed 
    >>= (fun () -> 
      Gopigo.read_encoder fd `Right 
      >|= Speed.update_counter m2_speed 
    )
    >>= (fun () -> Gopigo.read_us_distance fd) 
    >|= (fun x  -> us_distance := x) 
    >>= (fun () -> until_done ~delay:0.25 update_counter_loop)
  in   

  let rec print_speed_loop () = 
    Lwt_io.printf " %5f (%i)|  %5f | (%3i , %3i) | %3i cm \n" 
      (Speed.speed m1_speed) (Speed.counter m1_speed) (Speed.speed m2_speed) 
      (!m1_set_speed) (!m2_set_speed ) !us_distance  
    >>=(fun () -> until_done ~delay:0.1 print_speed_loop)
  in 

  let read_encoder () = 
    Gopigo.enable_encoder fd 
    >>= update_counter_loop 
  in 

  let catch t = 
    Lwt.catch (fun () -> t) (fun exn -> 
      Lwt_io.printl @@ Printexc.to_string exn
    )
  in 

  Lwt_main.run (
    Lwt.join [
      catch @@ main_t;
      catch @@ stop_if_too_closed (); 
      catch @@ read_encoder (); 
      catch @@ print_speed_loop (); 
      (*catch @@ blink_loop (); *) 
      (* rotate_servo true 90; *)  
    ] 
  )
