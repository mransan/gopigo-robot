
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


let ok_output = Lwt.return {
  Msg.sensors      = None; 
  type_ = Msg.Ok
}

type robot_sensors = {
  mutable left_encoder_reference : int; 
  mutable right_encoder_reference : int; 
  mutable left_encoder : int; 
  mutable right_encoder : int; 
  mutable us_distance : float; 
}

type robot = {
  sensors : robot_sensors;
  mutable left_led : bool;
  mutable right_led : bool;
  mutable movement : [`Fwd  | `Bwd | `Stop];
  mutable left_speed : int;
  mutable right_speed : int;
}

let robot = {
  sensors = {
    left_encoder_reference = 0;
    right_encoder_reference = 0;
    left_encoder = 0;
    right_encoder = 0;
    us_distance = 0.; 
  };
  left_led = false; 
  right_led = false; 
  movement = `Stop; 
  left_speed = 35; 
  right_speed = 45; 
}

let convert_side = function 
  | Msg.Left -> `Left 
  | Msg.Right -> `Right

let handle_command gopigo_handle {Msg.type_ ; side; speed_value} =  

  match type_, side, speed_value with
  | Msg.Fwd, None, None-> 
    Gopigo.fwd gopigo_handle
    >>= (fun () -> 
      robot.movement <-  `Fwd; 
      ok_output
    ) 
  | Msg.Stop, None, None -> 
    Gopigo.stop gopigo_handle 
     >>= (fun () -> 
       robot.movement <-  `Stop; 
       ok_output
     )  
  | Msg.Set_speed, Some side, Some speed_value -> 
    Gopigo.set_speed gopigo_handle (convert_side side) speed_value
    >>= (fun () -> 
      begin
        match side with 
        | Msg.Left -> robot.left_speed <- speed_value
        | Msg.Right-> robot.right_speed <- speed_value
      end;
      ok_output
    )  
  | Msg.Led_on, Some side, None -> 
    Gopigo.led gopigo_handle `On (convert_side side) 
    >>= (fun () -> 
      begin
        match side with 
        | Msg.Left -> robot.left_led <- true 
        | Msg.Right-> robot.right_led <- true
      end;
      ok_output
    )
  | Msg.Led_off, Some side, None -> 
    Gopigo.led gopigo_handle `Off (convert_side side) 
    >>= (fun () -> 
      begin
        match side with 
        | Msg.Left -> robot.left_led <- false 
        | Msg.Right-> robot.right_led <- false
      end;
      ok_output
    )
  | _ -> Lwt.fail_with "Unrecognize command" 

let rec main_loop gopigo cmd_queue next = 
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
    robot.sensors.left_encoder  <- left;
    robot.sensors.right_encoder <- right;
    robot.sensors.us_distance   <- us_distance;
    let t1 = Unix.gettimeofday () in 
    let elapsed_ms = (t1 -. t0) *. 1_000. in
    (*
    Lwt.ignore_result @@ Lwt_io.printf "%10.5f | (%i, %i), %f\n" elapsed_ms left right us_distance;
    *)
    Lwt.return_unit
  )
  >>= (fun () -> 
    if Queue.length cmd_queue > 0 
    then (
      let cmd = Queue.pop cmd_queue in 
      (*
      Printf.printf "executing cmd %s \n%!" (Msg.string_of_command cmd); 
      *)
      handle_command gopigo cmd >>= (fun _ -> Lwt.return_unit) 
    )
    else Lwt.return_unit 
  ) 
  >>= (fun () -> 
    next := (!next +. 0.300); 
    let t1 = Unix.gettimeofday () in 
    let sleep_s = !next  -. t1 in
    if sleep_s >= 0. 
    then Lwt_unix.sleep sleep_s
    else (
      Lwt.ignore_result @@ Lwt_io.printf "Could not keep up with time interval %f\n" sleep_s;
      Lwt.return_unit
    )
  )
  >>=(fun () -> main_loop gopigo cmd_queue next) 

let init gopigo = 
  Gopigo.set_speed gopigo `Left robot.left_speed 
  >>=(fun () -> Gopigo.set_speed gopigo `Right robot.right_speed)
  >>=(fun () -> Gopigo.read_encoder gopigo `Left) 
  >|=(fun i  -> robot.sensors.left_encoder_reference <- i)
  >>=(fun () -> Gopigo.read_encoder gopigo `Right) 
  >|=(fun i  -> robot.sensors.right_encoder_reference <- i)
  >>=(fun () ->
    match robot.movement with
    | `Fwd -> Gopigo.fwd gopigo
    | `Stop -> Gopigo.stop gopigo
  )

let pipe_name_in  = "/tmp/gopigo_server.in" 
let pipe_name_out = "/tmp/gopigo_server.out" 

let open_pipes () = 
  let create_if_not_exist file_name =  
    if not (Sys.file_exists file_name)
    then Unix.mkfifo file_name 0o666
  in

  create_if_not_exist pipe_name_in;
  create_if_not_exist pipe_name_out;

  Lwt_unix.openfile pipe_name_in [Unix.O_RDWR; Unix.O_TRUNC] 0o666
  >>= (fun read_fd -> 
    Lwt_unix.openfile pipe_name_out [Unix.O_RDWR; Unix.O_TRUNC] 0o666
    >>=(fun write_fd -> 
      Lwt.return (read_fd, write_fd)
    )
  )

let () = 

  ignore @@ Unix.umask 0o000; 

  let cmd_queue = Queue.create () in 

  let t = 
    open_pipes () 
    >>=(fun (read_fd, write_fd) ->   
      let rec loop () = 
        Command_lwt.read read_fd 
        >>= (fun cmd -> 
          match cmd with
          | {Msg.type_ = Msg.Sensors; side = None; speed_value = None; } -> (
            let sensors = robot.sensors in 
            let output = {
              Msg.type_ = Msg.Sensors;  
              Msg.sensors = Some {
                Msg.left_encoder = sensors.left_encoder - sensors.left_encoder_reference;
                Msg.right_encoder = sensors.right_encoder - sensors.right_encoder_reference;
                Msg.us_distance = sensors.us_distance;
              };
            } in 
            Output_lwt.write write_fd output 
          )
          | _ -> Lwt.return @@ Queue.push cmd cmd_queue 
        ) 
        >>= loop
      in
      loop () 
    )
  in 

  let main gopigo_handle () = 
    init gopigo_handle
    >>= (fun () ->
      main_loop gopigo_handle cmd_queue (ref @@ Unix.gettimeofday ())
    )
  in 

  let rec f () = 
    let gopigo_handle = Gopigo.create () in 
    Lwt_io.printl "C'est reparti"
    >>= (fun () -> 
      Lwt.catch (main gopigo_handle) (fun exn ->
        Gopigo.close gopigo_handle 
        >>=(fun () -> 
          Lwt_unix.sleep 0.5 >>= f 
        )
      )
    )
  in 


  Lwt_main.run (Lwt.join [
    t;  
    f ();
  ])  
