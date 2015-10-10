
open Lwt.Infix 

let () = 

  let fd = Gopigo.create () in 
  let t0 = Unix.gettimeofday () in 

  let done_ = Done.create () in 

  let speeds = 
    let rec loop l = function 
      | -1  -> l 
      | i  -> loop ((Frequency.create ~dampening:i ())::l) (i - 1)
    in 
    loop [] 4 
  in 

  let string_of_speeds () = 
    String.concat ", " @@ List.map (fun s -> string_of_float @@ Frequency.value s) speeds 
  in 

  let set_speed i () = 
    Gopigo.set_speed fd `Left i 
    >>=(fun () -> Gopigo.set_speed fd `Right (i + 12))
  in 

  let main_t = 

    let rec slow_down = function 
      | 0 -> (Done.interupt done_ ; Gopigo.stop fd )
      | i -> (
        set_speed i ()
        >>= (fun () -> Lwt_unix.sleep 0.05)
        >>= (fun () -> slow_down (i - 1))
      )
    in 

    set_speed 35 () 
    >>= (fun () -> Gopigo.fwd fd) 
    >>= (fun () -> Lwt_unix.sleep 5.)
    >>= (fun () -> slow_down 35) 
  in 

  let read_t = 
    Done.iter ~delay:0.25 done_ (fun () ->
      Gopigo.read_encoder fd `Right 
      >|=(fun i -> 
        List.iter (fun s -> Frequency.update_counter s i) speeds
      )  
    )
  in 

  let print_t = 
    Lwt_io.open_file Lwt_io.Output "calibrator.csv"
    >>= (fun channel -> 
      Done.iter ~delay:0.01 done_ (fun () -> 
        Lwt_io.fprintf channel "%i, %f, %s \n"
          (Frequency.counter (List.hd speeds))
          (Frequency.last_observation_time (List.hd speeds) -. t0 )
          (string_of_speeds () )
      )
    )
      
  in 
      
  Lwt_main.run (Lwt.join [
    main_t ; read_t; print_t; 
  ]) 



