
open Lwt.Infix 

module Msg = Gopigo_pb 

module Command_lwt = Pblwtrt.Make(struct 
  type t = Msg.command 
  let encode = Msg.encode_command
  let decode = Msg.decode_command 
end)

module Output_lwt = Pblwtrt.Make(struct 
  type t = Msg.output 
  let encode = Msg.encode_output
  let decode = Msg.decode_output
end)

let gopigo_handle = Gopigo.create () 

let ok_output = Lwt.return {
  Msg.encoder_value = None; 
  us_distance = None; 
  type_ = Msg.Ok
}

type robot = {
  mutable left_encoder : int; 
  mutable right_encoder : int; 
  mutable us_distance : float; 
}

let robot = {
  left_encoder = 0;
  right_encoder = 0;
  us_distance = 0.; 
}

let convert_side = function 
  | Msg.Left -> `Left 
  | Msg.Right -> `Right

let handle_command {Msg.type_ ; side; speed_value} =  

  match type_, side, speed_value with
  | Msg.Fwd, None, None-> 
      Gopigo.fwd gopigo_handle
      >>= (fun () -> ok_output) 
  | Msg.Stop, None, None -> 
     Gopigo.stop gopigo_handle 
     >>= (fun () -> ok_output)  
  | Msg.Set_speed, Some side, Some speed_value -> 
     Gopigo.set_speed gopigo_handle (convert_side side) speed_value
     >>= (fun () -> ok_output)
  | _ -> Lwt.fail_with "Unrecognize command" 

let rec main_loop gopigo cmd_queue = 
  let t0 = Unix.gettimeofday () in 
  
  Gopigo.read_encoder gopigo `Left 
  >>=(fun left_encoder_value -> 
    Gopigo.read_encoder gopigo `Right 
    >>=(fun right_encoder_value -> 
      Lwt.return (left_encoder_value, right_encoder_value)
    )
  )
  >>=(fun encoder_values -> 
    Gopigo.read_us_distance gopigo 
    >>= (fun us_distance -> 
      Lwt.return (encoder_values, us_distance)
    )
  )
  >>=(fun ((left, right), us_distance) -> 
    robot.left_encoder  <- left;
    robot.right_encoder <- right;
    robot.us_distance   <- us_distance;

    let t1 = Unix.gettimeofday () in 
    let elapsed_ms = (t1 -. t0) *. 1_000. in
    Lwt.ignore_result @@ Lwt_io.printf "%10.5f | (%i, %i), %f\n" elapsed_ms left right us_distance;
    Lwt.return_unit
  )
  >>= (fun () -> 
    if Queue.length cmd_queue > 0 
    then (
      let cmd = Queue.pop cmd_queue in 
      handle_command cmd >>= (fun _ -> Lwt.return_unit) 
    )
    else Lwt.return_unit 
  ) 
  >>= (fun () -> 
    let t1 = Unix.gettimeofday () in 
    let sleep_s = 0.250 -. (t1 -. t0) in 
    if sleep_s >= 0. 
    then Lwt_unix.sleep sleep_s
    else (
      Lwt.ignore_result @@ Lwt_io.printf "Could not keep up with time interval %f\n" sleep_s;
      Lwt.return_unit
    )
  )
  >>=(fun () -> main_loop gopigo cmd_queue) 

let init gopigo = 
  Gopigo.set_speed gopigo `Left 65
  >>=(fun () -> Gopigo.set_speed gopigo `Right 75)

let pipe_name = "/tmp/gopigo_server" 

let () = 

  ignore @@ Unix.umask 0o000; 
  if not (Sys.file_exists pipe_name)
  then Unix.mkfifo pipe_name 0o666;  
    
  let cmd_queue = Queue.create () in 

  let t = 
    Lwt_unix.openfile pipe_name [Unix.O_RDWR;Unix.O_CREAT; Unix.O_TRUNC] 0o666
    >>= (fun fd -> 

      let rec loop () = 
        Command_lwt.read fd 
        >>= (fun cmd -> 
          match cmd with
          | {Msg.type_ = Msg.Read_us_distance; side = None; speed_value = None; } -> (
            
            let output = {
              Msg.type_ = Us_distance;  
              Msg.encoder_value = None;
              Msg.us_distance = Some (robot.us_distance);
            } in 
            Output_lwt.write fd output 
          )
          | _ -> Lwt.return @@ Queue.push cmd cmd_queue 
        ) 
        >>= loop
      in
      loop () 
    )
  in 

  Lwt_main.run (Lwt.join [
    t;  
    init gopigo_handle >>= (fun () -> main_loop gopigo_handle cmd_queue) 
  ])  
