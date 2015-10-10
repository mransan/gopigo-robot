open Lwt.Infix 

external gopigo_init_device : unit -> Unix.file_descr = "gopigo_init_device"

type t = Lwt_unix.file_descr 

let create () = 
  Lwt_unix.of_unix_file_descr ~blocking:false @@ gopigo_init_device () 

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

let read_cmd ?delay ?d1 ?d2 ?d3 id n fd = 
  Lwt_mutex.with_lock device_mtx (fun () -> 
    write fd @@ cmd ?d1 ?d2 ?d3 id 
    >>= (fun () -> 
      match delay with 
      | None -> Lwt.return_unit 
      | Some x -> Lwt_unix.sleep x 
    )
    >>= (fun () -> 
      read fd n
    )
  ) 

let fwd            = single_cmd 105

let stop           = single_cmd 120

let enable_encoder = single_cmd 51

let decrease_speed = single_cmd 103 

let increase_speed  = single_cmd 116

let set_speed fd which speed = 
  let id = match which with
    | `Left -> 70 
    | `Right -> 71
  in  
  Lwt_mutex.with_lock device_mtx (fun () -> 
    write fd @@ cmd ~d1:speed id 
  ) 

let move_servo fd angle = 
  Lwt_mutex.with_lock device_mtx (fun () -> 
    write fd @@ cmd ~d1:angle 101 
  )  

let read_encoder fd which = 
  let d1 = match which with 
    | `Left -> 0 
    | `Right -> 1 
  in 
  read_cmd ~d1 53 2 fd 
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

let read_us_distance fd = 
  read_cmd ~delay:0.08 ~d1:15 117 2 fd 
  >>=(fun b -> 
    let b0 = Char.code @@ Bytes.get b 0  in 
    let b1 = Char.code @@ Bytes.get b 1  in 
    Lwt.return @@ b0 * 256 + b1 
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
