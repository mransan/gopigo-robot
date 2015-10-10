open Lwt.Infix 

let lwt_ignore _ = Lwt.return_unit 

let () = 
  let fd = Gopigo.create () in  

  let t0 = Unix.gettimeofday () in 

  let done_ = Done.create () in 

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
    let loop () = 
      Done.iter ~delay:0.2 done_ (fun () ->  
        m1_set_speed := calculate_speed (Speed.speed m1_speed) !m1_set_speed;
        m2_set_speed := calculate_speed (Speed.speed m2_speed) !m2_set_speed;
        Gopigo.set_speed fd `Left !m1_set_speed 
        >>= (fun () -> Gopigo.set_speed fd `Right !m2_set_speed) 
      )
    in 
    Gopigo.set_speed fd `Left 35
    >>= (fun () -> Gopigo.set_speed fd `Right 47) 
    >>= (fun () -> Gopigo.fwd fd) 
    >>= (fun () -> loop ())
  in 

  let stop_if_too_closed () = 
    Done.iter ~delay:0.5 done_ (fun () -> 
      let now = Unix.gettimeofday () in 
      if (!us_distance > 0 && !us_distance < 20 )|| (now -.  t0 > 20.)  
      then (
        Done.interupt done_;
        Gopigo.stop fd 
      ) 
      else Lwt.return_unit 
    )
  in 

  let blink_loop () = 
    Done.iter done_ (fun () -> 
      let delay = 0.5 in
      Gopigo.led fd `On `Right 
      >>=(fun () -> Lwt_unix.sleep delay)  
      >>=(fun () -> Gopigo.led fd `Off `Right) 
      >>=(fun () -> Gopigo.led fd `On  `Left) 
      >>=(fun () -> Lwt_unix.sleep delay)  
      >>=(fun () -> Gopigo.led fd `Off `Left) 
    )
  in 
  
  let motor_speed = ref (0, 0) in 

  let update_counter_loop ()  = 
    Done.iter ~delay:0.5 done_ (fun () -> 
      Gopigo.read_encoder fd `Left 
      >|= Speed.update_counter m1_speed 
      >>= (fun () -> 
        Gopigo.read_encoder fd `Right 
        >|= Speed.update_counter m2_speed 
      )
      >>= (fun () -> Gopigo.read_us_distance fd) 
      >|= (fun x  -> us_distance := x) 
    )
  in   

  let print_speed_loop () = 
    Done.iter done_ ~delay:0.1 (fun () -> 
      Lwt_io.printf " %5f (%i)|  %5f | (%3i , %3i) | %3i cm \n" 
        (Speed.speed m1_speed) (Speed.counter m1_speed) (Speed.speed m2_speed) 
        (!m1_set_speed) (!m2_set_speed ) !us_distance  
    )
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
      catch @@ blink_loop (); 
      (* rotate_servo true 90; *)  
    ] 
  )
