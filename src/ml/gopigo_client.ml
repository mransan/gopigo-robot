
open Lwt.Infix 

module Msg = Gopigo_pb 

module Output_lwt = Pblwtrt.Make(struct 
  type t = Msg.output 
  let encode = Msg.encode_output
  let decode = Msg.decode_output
end)

module Cmd_lwt = Pblwtrt.Make(struct 
  type t = Msg.command 
  let encode = Msg.encode_command
  let decode = Msg.decode_command 
end) 


let default_cmd = {
  Msg.type_ = Msg.Stop; 
  Msg.side  = None; 
  Msg.speed_value = None; 
}

let pipe_name_in  = "/tmp/gopigo_server.in" 
let pipe_name_out = "/tmp/gopigo_server.out" 

let open_pipes () = 
  Lwt_unix.openfile pipe_name_in [Unix.O_RDWR; Unix.O_TRUNC] 0o666
  >>= (fun write_fd -> 
    Lwt_unix.openfile pipe_name_out [Unix.O_RDWR; Unix.O_TRUNC] 0o666
    >>=(fun read_fd -> 
      Lwt.return (read_fd, write_fd)
    )
  )


let string_of_float_option = function
  | Some x -> string_of_float x 
  | None   -> "nan"

let send_cmd = Cmd_lwt.write 

let print_sensors read_fd write_fd = 
  send_cmd write_fd {default_cmd with 
    Msg.type_ = Msg.Sensors;
  }
  >>= (fun () -> Output_lwt.read read_fd)
  >>= (function 
    | {Msg.type_ = Msg.Sensors; sensors = Some sensors } -> 
      Lwt_io.printf "%s\n" (Msg.string_of_sensors sensors)
      >>= (fun () -> Lwt_unix.sleep 0.1) 
    | _ -> Lwt.fail_with "Unexpected response"
  )

let get_int b i = Char.code @@ Bytes.get b i 

let current_speed = ref 35 
let diff_speed    = ref 0

let change_speed what write_fd = 
  begin 
    match what with
    | `Increase -> current_speed := !current_speed + 2
    | `Decrease -> current_speed := !current_speed - 2
    | `Left     -> ( 
      diff_speed    := !diff_speed + 4;
      current_speed := !current_speed - 2
    )
    | `Right    -> (
      diff_speed    := !diff_speed - 4;
      current_speed := !current_speed + 2
    )
  end;
  let cmd = {
    Msg.type_ = Msg.Set_speed; 
    Msg.side  = Some Msg.Left; 
    Msg.speed_value = (Some !current_speed);
  } in 
  send_cmd write_fd cmd
  >>=(fun _ -> 
    let cmd = {cmd with 
      Msg.side = (Some Msg.Right);
      Msg.speed_value = Some (!current_speed + 10 + !diff_speed); 
    } in 
    send_cmd write_fd cmd 
  ) 

let () = 
  ignore @@ Sys.command "stty raw -echo" ;
  let fwd = {default_cmd with Msg.type_ = Msg.Fwd } in 
  let stop= {default_cmd with Msg.type_ = Msg.Stop} in 

  let rec loop write_fd () = 
    let b = Bytes.create 2 in 
    Lwt_unix.read Lwt_unix.stdin b 0 1 
    >>=(fun x ->
      (match x with 
      | 1 -> (
        match get_int b 0  with 
        | 27 -> 
          Lwt_unix.read Lwt_unix.stdin b 0 2
          >>=(fun _ -> 
            match get_int b 0, get_int b 1 with
            | 91, 65 -> change_speed `Increase write_fd 
            | 91, 66 -> change_speed `Decrease write_fd 
            | 91, 67 -> change_speed `Right write_fd 
            | 91, 68 -> change_speed `Left write_fd 
            | _ -> Lwt.return_unit 
          )
        | 32  -> Lwt_io.printl "space"
        | 113 -> (
          send_cmd write_fd stop 
          >>= (fun () ->
            Sys.command "stty sane"; 
            exit 0
          )
        )
        | _ -> Lwt.return_unit
      )
      | _ -> Lwt.fail_with "Error reading stdin"
      ) 
      >>= loop write_fd  
    )
  in 

  let t = 
    open_pipes ()
    >>= (fun (read_fd, write_fd) -> 
      send_cmd write_fd fwd
      >>= loop write_fd 
    )
  in 
  Lwt_main.run t 
