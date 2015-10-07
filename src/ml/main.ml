open Lwt.Infix 

external gopigo_init_device : unit -> Unix.file_descr = "gopigo_init_device"

let lwt_ignore _ = Lwt.return_unit 

let if_some v f = 
  match v with 
  | None -> () 
  | Some x -> f x 

let cmd ?d1 ?d2 ?d3 id = 
  let cmd = Bytes.make 5 (Char.chr 0) in 
  Bytes.set cmd 0 (Char.chr 1); 
  Bytes.set cmd 1 (Char.chr id); 
  if_some d1 (fun i -> Bytes.set cmd 2 (Char.chr i)); 
  if_some d2 (fun i -> Bytes.set cmd 3 (Char.chr i)); 
  if_some d3 (fun i -> Bytes.set cmd 4 (Char.chr i)); 
  cmd

let write fd cmd : unit Lwt.t = 
  Lwt_unix.write fd cmd 0 5 
  >>= (fun written  -> 
    Lwt_unix.sleep 0.005
    >>= (fun () -> 
      if written <> 5
      then 
        failwith (Printf.sprintf "Invalid number of bytes written for cmd") 
      else Lwt.return_unit 
    )
  )

let read fd n : bytes Lwt.t = 
  (* check n < 5 *) 
  let b = Bytes.make 5 (Char.chr 0) in 
  let rec loop = function 
    | i when i = n -> Lwt.return b 
    | i -> (
      Lwt_unix.read fd b i 1 
      >>= (fun _ -> 
        Lwt_unix.sleep 0.005
      )
      >>= (fun () -> 
        loop (i + 1)
      )
    ) 
  in 
  loop 0 

let device_mtx = Lwt_mutex.create ()   

let single_cmd id fd = 
  Lwt_mutex.with_lock device_mtx (fun () -> 
    write fd @@ cmd id
  ) 

let read_cmd ?d1 ?d2 ?d3 id n fd = 
  Lwt_mutex.with_lock device_mtx (fun () -> 
    write fd @@ cmd ?d1 ?d2 ?d3 id 
    >>= (fun () -> 
      read fd n
    )
  ) 

let fwd            = single_cmd 119
let stop           = single_cmd 120
let enable_encoder = single_cmd 51

let read_counter fd i = 
  read_cmd ~d1:i 53 2 fd 
  >>=(fun b -> 
    let b0 = Char.code @@ Bytes.get b 0  in 
    let b1 = Char.code @@ Bytes.get b 1  in 
    Lwt.return @@ b0 * 256 + b1 
  ) 

let read_speed fd = 
  read_cmd 114 2 fd 
  >>=(fun b -> 
    let b0 = Char.code @@ Bytes.get b 0  in 
    let b1 = Char.code @@ Bytes.get b 1  in 
    Lwt.return (b0, b1) 
  ) 

let led fd mode which = 
  let v = match mode with 
    | `On -> 1
    | `Off -> 0 
  in 
  let pin = match which with 
    | `Left  -> 10
    | `Right -> 5 in 
  Lwt_mutex.with_lock device_mtx (fun () -> 
    write fd @@ cmd ~d1:pin ~d2:1 16 
    >>= (fun () ->
      write fd @@ cmd ~d1:pin ~d2:v 12
    )
  )

module Speed = struct 
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
end 

let () = 
  let fd = Lwt_unix.of_unix_file_descr ~blocking:false @@ gopigo_init_device () in 

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


  let main_t = 
    fwd fd 
    >>= (fun () -> Lwt_unix.sleep 2.0) 
    >>= (fun () -> stop fd)
    >>= (fun () -> done_ := true; Lwt.return_unit) in 

  let rec blink_loop () = 
    let delay = 0.05 in
    led fd `On `Right 
    >>=(fun () -> Lwt_unix.sleep delay)  
    >>=(fun () -> led fd `Off `Right) 
    >>=(fun () -> led fd `On  `Left) 
    >>=(fun () -> Lwt_unix.sleep delay)  
    >>=(fun () -> led fd `Off `Left) 
    >>=(fun () -> until_done blink_loop) 
  in 
  
  let m1_speed = Speed.create () in 
  let m2_speed = Speed.create () in 

  let rec update_counter_loop ()  = 
    read_counter fd 0  
    >|= Speed.update_counter m1_speed 
    >>= (fun () -> 
      read_counter fd 1 
      >|= Speed.update_counter m2_speed 
    )
    >>= (fun () -> until_done update_counter_loop)
  in   

  let rec print_speed_loop () = 
    Lwt_io.printf " %5f |  %5f \n" (Speed.speed m1_speed) (Speed.speed m2_speed) 
    >>=(fun () -> until_done ~delay:0.1 print_speed_loop)
  in 

  let read_encoder () = 
    enable_encoder fd 
    >>= update_counter_loop 
  in 

  Lwt_main.run (
    Lwt_unix.blocking fd 
    >>=(function
      | true  -> Lwt_io.printl "Blocking mode"
      | false -> Lwt_io.printl "Non-Blocking mode"
    )
    >>=(fun () ->
      Lwt.join [main_t;read_encoder (); print_speed_loop (); blink_loop () ] 
    )
  )
