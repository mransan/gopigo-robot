
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

let () = 
  let fwd = {default_cmd with Msg.type_ = Msg.Fwd } in 
  let stop= {default_cmd with Msg.type_ = Msg.Stop} in 

  let t = 
    open_pipes ()
    >>= (fun (read_fd, write_fd) -> 
      send_cmd write_fd fwd
      >>= (fun () -> Lwt_unix.sleep 2.)
      >>= (fun () -> send_cmd write_fd stop) 
      >>= (fun () -> 
        let rec loop () = 
          print_sensors read_fd write_fd 
          >>= (fun () -> Lwt_unix.sleep 0.1) 
          >>= loop 
        in
        loop ()
      )
    )
  in 
  Lwt_main.run t 
