type command_type =
  | Sensors 
  | Fwd 
  | Stop 
  | Set_speed 
  | Led_on 
  | Led_off 

val decode_command_type : Protobuf_codec.Decoder.t -> command_type
(** [decode_command_type decoder] decodes a [command_type] value from [decoder] *)

val encode_command_type : command_type -> Protobuf_codec.Encoder.t -> unit
(** [encode_command_type v encoder] encodes [v] with the given [encoder] *)

val string_of_command_type : command_type -> string 
(** [string_of_command_type v] returns a debugging string for [v] *)

type command_side =
  | Left 
  | Right 

val decode_command_side : Protobuf_codec.Decoder.t -> command_side
(** [decode_command_side decoder] decodes a [command_side] value from [decoder] *)

val encode_command_side : command_side -> Protobuf_codec.Encoder.t -> unit
(** [encode_command_side v encoder] encodes [v] with the given [encoder] *)

val string_of_command_side : command_side -> string 
(** [string_of_command_side v] returns a debugging string for [v] *)

type command = {
  type_ : command_type;
  side : command_side option;
  speed_value : int option;
}

val decode_command : Protobuf_codec.Decoder.t -> command
(** [decode_command decoder] decodes a [command] value from [decoder] *)

val encode_command : command -> Protobuf_codec.Encoder.t -> unit
(** [encode_command v encoder] encodes [v] with the given [encoder] *)

val string_of_command : command -> string 
(** [string_of_command v] returns a debugging string for [v] *)

type sensors = {
  left_encoder : int;
  right_encoder : int;
  us_distance : float;
}

val decode_sensors : Protobuf_codec.Decoder.t -> sensors
(** [decode_sensors decoder] decodes a [sensors] value from [decoder] *)

val encode_sensors : sensors -> Protobuf_codec.Encoder.t -> unit
(** [encode_sensors v encoder] encodes [v] with the given [encoder] *)

val string_of_sensors : sensors -> string 
(** [string_of_sensors v] returns a debugging string for [v] *)

type output_type =
  | Ok 
  | Sensors 

val decode_output_type : Protobuf_codec.Decoder.t -> output_type
(** [decode_output_type decoder] decodes a [output_type] value from [decoder] *)

val encode_output_type : output_type -> Protobuf_codec.Encoder.t -> unit
(** [encode_output_type v encoder] encodes [v] with the given [encoder] *)

val string_of_output_type : output_type -> string 
(** [string_of_output_type v] returns a debugging string for [v] *)

type output = {
  type_ : output_type;
  sensors : sensors option;
}

val decode_output : Protobuf_codec.Decoder.t -> output
(** [decode_output decoder] decodes a [output] value from [decoder] *)

val encode_output : output -> Protobuf_codec.Encoder.t -> unit
(** [encode_output v encoder] encodes [v] with the given [encoder] *)

val string_of_output : output -> string 
(** [string_of_output v] returns a debugging string for [v] *)

