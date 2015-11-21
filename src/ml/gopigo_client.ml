
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

let send_cmd fd cmd = 
  let encoder = Protobuf_codec.Encoder.create () in 
  Msg.encode_command cmd encoder; 
  let b = Protobuf_codec.Encoder.to_bytes encoder in 
  let len = Bytes.length b in 
  let t = 
    let msg = Bytes.create Int_16_codec.size in 
    Int_16_codec.encode len msg 0; 
    Lwt_unix.write fd msg 0 Int_16_codec.size 
  in 
  t >>= (fun _ -> 
     Lwt_unix.write fd b 0 len 
  )  
  >>= (fun _ -> Lwt.return_unit)  

let () = 
  let fwd = {default_cmd with Msg.type_ = Msg.Fwd } in 
  let stop= {default_cmd with Msg.type_ = Msg.Stop} in 

  let t = 
    Lwt_unix.openfile pipe_name [Unix.O_RDWR;Unix.O_TRUNC] 0o666
    >>= (fun fd -> 
      send_cmd fd fwd
      >>= (fun () -> Lwt_unix.sleep 2.)
      >>= (fun () -> send_cmd fd stop) 
    )
  in 

  Lwt_main.run t 
