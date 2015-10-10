
open Lwt.Infix 

let () = 

  let fd = Gopigo.create () in 
  let t0 = Unix.gettimeofday () in 

  let speeds = 
    let rec loop l = function 
      | 2 -> l 
      | i  -> loop ((Speed.create ~dampening:i ())::l) (i - 1)
    in 
    loop [] 4 
  in 

  let string_of_speeds () = 
    String.concat ", " @@ List.map (fun s -> string_of_float @@ Speed.speed s) speeds 
  in 

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
    >>=(fun () -> Gopigo.set_speed fd `Right (i + 12))
  in 

  let main_t = 

    let rec slow_down = function 
      | 0 -> (done_ :=true ; Gopigo.stop fd )
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
    let rec loop () = 
      Gopigo.read_encoder fd `Right 
      >|=(fun i -> 
        List.iter (fun s -> Speed.update_counter s i) speeds
      )  
      >>= (fun () -> until_done ~delay:0.25 loop)
    in  
    loop () 
  in 

  let print_t = 
    let rec loop channel () = 
      Lwt_io.fprintf channel "%f, %s \n"
        (Speed.time (List.hd speeds) -. t0 )
        (string_of_speeds () )
      >>=(fun () -> until_done ~delay:0.05 (loop channel))
    in 
    Lwt_io.open_file Lwt_io.Output "calibrator.csv"
    >>= (fun channel -> loop channel ())  
  in 
      
  Lwt_main.run (Lwt.join [
    main_t ; read_t; print_t; 
  ]) 



