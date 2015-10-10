open Lwt.Infix 

let lwt_ignore _ = Lwt.return_unit 

let () = 
  let fd = Gopigo.create () in  

  let t0 = Unix.gettimeofday () in 

  let done_ = Done.create () in 

  let us_distance = ref 100 in 

  let m1_speed = Frequency.create () in 
  let m2_speed = Frequency.create () in 

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
        m1_set_speed := calculate_speed (Frequency.value m1_speed) !m1_set_speed;
        m2_set_speed := calculate_speed (Frequency.value m2_speed) !m2_set_speed;
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
    Small_behavior.stop_if_too_close done_ fd ~delay:0.5 ~distance:0.2
  in 

  let stop_later () = 
    Small_behavior.stop_later done_ fd 20. 
  in 

  let blink_loop () = 
    Small_behavior.led_blink done_ fd 0.5 
  in 
  
  let motor_speed = ref (0, 0) in 

  let update_counter_loop ()  = 
    Done.iter ~delay:0.5 done_ (fun () -> 
      Gopigo.read_encoder fd `Left 
      >|= Frequency.update_counter m1_speed 
      >>= (fun () -> 
        Gopigo.read_encoder fd `Right 
        >|= Frequency.update_counter m2_speed 
      )
    )
  in   

  let print_speed_loop () = 
    Done.iter done_ ~delay:0.1 (fun () -> 
      Lwt_io.printf " %5f (%i)|  %5f | (%3i , %3i) | cm \n" 
        (Frequency.value m1_speed) (Frequency.counter m1_speed) (Frequency.value m2_speed) 
        (!m1_set_speed) (!m2_set_speed ) 
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
      catch @@ stop_later (); 
      (* rotate_servo true 90; *)  
    ] 
  )
