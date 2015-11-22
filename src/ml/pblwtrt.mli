
module type Sig = sig 
  type t 
  val decode : Protobuf_codec.Decoder.t -> t 
  val encode : t -> Protobuf_codec.Encoder.t -> unit
end 


module Make(S:Sig) : sig 
  val write : Lwt_unix.file_descr -> S.t -> unit Lwt.t 

  val read : Lwt_unix.file_descr -> S.t Lwt.t 
end 
