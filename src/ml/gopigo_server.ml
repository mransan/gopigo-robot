
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


let gopigo_handle = Gopigo.create () 

let ok_output = Lwt.return {
  Msg.encoder_value = None; 
  us_distance = None; 
  type_ = Msg.Ok
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


let () = 

  ignore @@ Unix.umask 0o000; 

  let pipe_name = "/tmp/gopigo_server" in 
  Unix.mkfifo pipe_name 0o666;  
  let t = 
    Lwt_unix.openfile pipe_name [Unix.O_RDWR;Unix.O_CREAT; Unix.O_TRUNC] 0o666
    >>= (fun fd -> 

      let buffer = Bytes.create 4 in  
      let rec loop () = 
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
              let cmd = Msg.decode_command decoder in 
              Printf.printf "Command received: %s \n%!" (Msg.string_of_command cmd); 
              Lwt.return cmd
        ) 
        >>= handle_command
        >|= (fun _ -> ()) 
        >>= loop
      in
      loop () 
    )
  in 
  Lwt_main.run t 
