
open Lwt.Infix 

module Msg = Gopigo_pb 

module Int_16_codec = struct 

  let size = 2 
  
  let encode (x:int) (buffer:bytes) (start:int) = 

    let f mask shift_size = 
       Char.unsafe_chr @@  ((x land mask) lsr shift_size) 
    in 
    Bytes.set buffer (start + 0) @@ f 0x0000FF00 8;  
    Bytes.set buffer (start + 1) @@ f 0x000000FF 0;
    ()

  let decode buffer start = 
    let f i shift_size = 
      (Bytes.get buffer i |> Char.code )lsl shift_size
    in 
    f 0 8 
    lor f 1 0
end 

let default_cmd = {
  Msg.type_ = Msg.Stop; 
  Msg.side  = None; 
  Msg.speed_value = None; 
}

let pipe_name = "/tmp/gopigo_server" 

module Cmd_lwt = Pblwtrt.Make(struct 
  type t = Msg.command 
  let encode = Msg.encode_command
  let decode = Msg.decode_command 
end) 

let send_cmd = Cmd_lwt.write 

(*
let read_output fd =   
  let buffer = Bytes.create 4 in  
  Lwt_unix.read fd buffer 0 2
  >>= (function 
    | i when i = Int_16_codec.size  -> 
      let msg_length = Int_16_codec.decode buffer 0 in 
      let buffer = Bytes.create msg_length in  
      Lwt_unix.read fd buffer 0 msg_length 
      >|= (fun x -> (x, buffer))  
    | x -> Lwt.fail_with (Printf.sprintf "Error reading from pipe: %i" x) 
  )
  >>= (function
    | 0, _      -> Lwt.fail_with "0 bytes read... failure"
    | _, buffer -> 
        let decoder = Protobuf_codec.Decoder.of_bytes buffer in 
        let cmd = Msg.decode_output decoder in 
        Printf.printf "Output received: %s \n%!" (Msg.string_of_output cmd); 
        Lwt.return_unit
  ) 
*)

let () = 
  let fwd = {default_cmd with Msg.type_ = Msg.Fwd } in 
  let stop= {default_cmd with Msg.type_ = Msg.Stop} in 

  let t = 
    Lwt_unix.openfile pipe_name [Unix.O_RDWR;Unix.O_TRUNC] 0o666
    >>= (fun fd -> 
      send_cmd fd fwd
      >>= (fun () -> Lwt_unix.sleep 2.)
      >>= (fun () -> send_cmd fd stop) 
      (*
      >>= (fun () -> send_cmd fd {default_cmd with 
        Msg.type_ = Read_us_distance;
      })
      >>= (fun () -> read_output fd)
    *)
    )
  in 

  Lwt_main.run t 
