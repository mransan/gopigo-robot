open Lwt.Infix 

module type Sig = sig 
  type t 
  val decode : Protobuf_codec.Decoder.t -> t 
  val encode : t -> Protobuf_codec.Encoder.t -> unit
end 

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

module Make(S:Sig) = struct 
  let write fd v = 
    let encoder = Protobuf_codec.Encoder.create () in 
    S.encode v encoder; 
    let v_b   = Protobuf_codec.Encoder.to_bytes encoder in 
    let v_len = Bytes.length v_b in 
    let size_b= Bytes.create Int_16_codec.size in 
    Int_16_codec.encode v_len size_b 0; 
    Lwt_unix.write fd size_b 0 Int_16_codec.size 
    >>=(function
      | nb_written when nb_written = Int_16_codec.size -> 
        Lwt_unix.write fd v_b 0 v_len 
        >>= (function
          | nb_written when nb_written = v_len -> 
            Lwt.return_unit
          | _ -> Lwt.fail_with "Error writting protobuf message"
        )
      | _ -> Lwt.fail_with "Error writting protobu message"
    )

  let read fd =   
    let buffer = Bytes.create Int_16_codec.size in  
    Lwt_unix.read fd buffer 0 Int_16_codec.size
    >>= (function 
      | nb_read when nb_read = Int_16_codec.size  -> 
        let msg_length = Int_16_codec.decode buffer 0 in 
        let buffer = Bytes.create msg_length in  
        Lwt_unix.read fd buffer 0 msg_length 
        >>= (function
          | nb_read when nb_read = msg_length -> 
            Lwt.return buffer
          | _ -> Lwt.fail_with "Error reading protobuf message"
        )
      | _ -> Lwt.fail_with "Error reading protobuf message"
    )
    >>= (function
      | buffer -> 
        let decoder = Protobuf_codec.Decoder.of_bytes buffer in 
        Lwt.return @@ S.decode decoder 
    ) 
end 


