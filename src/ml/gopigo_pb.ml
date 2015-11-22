

module P  = Printf
  
let add_indentation n s = 
  Str.global_replace (Str.regexp "^" ) (String.make (n * 2) ' ') s  

module Pc = Protobuf_codec 

let decode_varint_as_int decoder = 
  (*Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.varint decoder*)
  Int64.to_int @@ Pc.Decoder.varint decoder

let decode_varint_zigzag_as_int decoder = 
  (* Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.varint decoder *) 
  Int64.to_int @@ Pc.Decoder.zigzag decoder

let encode_int_as_varint v encoder = 
  Pc.Encoder.varint (Int64.of_int v) encoder  

let encode_int_as_varint_zigzag v encoder = 
  Pc.Encoder.zigzag (Int64.of_int v) encoder  

let decode_bits32_as_int decoder = 
  (* Pc.Decoder.int_of_int32 "" @@ Pc.Decoder.bits32 decoder *) 
  Int32.to_int @@ Pc.Decoder.bits32 decoder

let encode_int_as_bits32 v encoder = 
  Pc.Encoder.bits32 (Pc.Encoder.int32_of_int "" v) encoder

let decode_bits64_as_int decoder = 
  (* Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.bits64 decoder *)
  Int64.to_int @@ Pc.Decoder.bits64 decoder 

let encode_int_as_bits64 v encoder = 
  Pc.Encoder.bits64 (Int64.of_int v) encoder

let decode_varint_as_bool decoder = 
  Pc.Decoder.bool_of_int64 "" @@ Pc.Decoder.varint decoder

let encode_bool_as_varint v encoder = 
  Pc.Encoder.varint (Int64.of_int @@ if v  then 1 else 0) encoder 

let decode_bits32_as_float decoder = 
  Int32.float_of_bits @@ Pc.Decoder.bits32 decoder 

let encode_float_as_bits32 v encoder =
  Pc.Encoder.bits32 (Int32.bits_of_float v) encoder 
  
let decode_bits64_as_float decoder = 
  Int64.float_of_bits @@ Pc.Decoder.bits64 decoder 

let encode_float_as_bits64 v encoder =
  Pc.Encoder.bits64 (Int64.bits_of_float v) encoder

let decode_bytes_as_string decoder = 
  Bytes.to_string @@ Pc.Decoder.bytes decoder 

let encode_string_as_bytes v encoder = 
  Pc.Encoder.bytes (Bytes.of_string v) encoder 

let decode_bytes_as_bytes  = Pc.Decoder.bytes 

let encode_bytes_as_bytes  = Pc.Encoder.bytes



let decode decoder mappings values = 
  let insert number v = 
    let v' = Array.unsafe_get values number in 
    Array.unsafe_set values number (v::v')
  in 

  let continue = ref true in 
  while !continue do 
    match Pc.Decoder.key decoder with 
    | None -> continue := false
    | Some (number, payload_kind) -> (
      try 
        let mapping = List.assoc number mappings in 
        insert number (mapping decoder);
      with 
      | Not_found ->  (
        Pc.Decoder.skip decoder payload_kind; 
      )
    )
  done;
  values

let required number a f = 
  match Array.unsafe_get a number with 
  | []     -> failwith (P.sprintf "field %i missing" number)
  | hd::_  -> f hd
  (* TODO Improve *) 

let optional number a f = 
  match Array.unsafe_get a number with 
  | []     -> None
  | hd::_ -> Some (f hd)
  (* TODO Improve *) 

let list_ number a f = 
  List.rev_map f @@ Array.unsafe_get a number

external identity: 'a -> 'a = "%identity"

let oneof numbers a = 
  let ret = List.fold_left (fun x number -> 
    match x with 
    | Some _ -> x 
    | None   -> optional number a identity  
 ) None numbers in 
 match ret with 
 | Some x -> x 
 | None -> failwith "None of oneof value could be found." 



let e () = failwith "programmatic error" 

type command_type =
  | Sensors 
  | Fwd 
  | Stop 
  | Set_speed 
  | Led_on 
  | Led_off 

let rec decode_command_type d = 
  match decode_varint_as_int d with
  | 1 -> Sensors
  | 2 -> Fwd
  | 3 -> Stop
  | 4 -> Set_speed
  | 5 -> Led_on
  | 6 -> Led_off
  | _ -> failwith "Unknown value for enum command_type"

let rec encode_command_type v encoder =
  match v with
  | Sensors -> encode_int_as_varint 1 encoder
  | Fwd -> encode_int_as_varint 2 encoder
  | Stop -> encode_int_as_varint 3 encoder
  | Set_speed -> encode_int_as_varint 4 encoder
  | Led_on -> encode_int_as_varint 5 encoder
  | Led_off -> encode_int_as_varint 6 encoder

let rec string_of_command_type v =
  match v with
  | Sensors -> "Sensors"
  | Fwd -> "Fwd"
  | Stop -> "Stop"
  | Set_speed -> "Set_speed"
  | Led_on -> "Led_on"
  | Led_off -> "Led_off"

type command_side =
  | Left 
  | Right 

let rec decode_command_side d = 
  match decode_varint_as_int d with
  | 1 -> Left
  | 2 -> Right
  | _ -> failwith "Unknown value for enum command_side"

let rec encode_command_side v encoder =
  match v with
  | Left -> encode_int_as_varint 1 encoder
  | Right -> encode_int_as_varint 2 encoder

let rec string_of_command_side v =
  match v with
  | Left -> "Left"
  | Right -> "Right"

type command = {
  type_ : command_type;
  side : command_side option;
  speed_value : int option;
}

let rec decode_command =
  let command_mappings = [
    (1, (fun d -> `Command_type (decode_command_type d)));
    (2, (fun d -> `Command_side (decode_command_side d)));
    (3, (fun d -> `Int (decode_varint_as_int d)));
  ]
  in
  (fun d ->
    let a = decode d command_mappings (Array.make 4 []) in {      
      type_ = required 1 a (function | `Command_type __v -> __v | _ -> e());
      side = optional 2 a (function | `Command_side __v -> __v | _ -> e());
      speed_value = optional 3 a (function | `Int __v -> __v | _ -> e());
    }
  )

let rec encode_command v encoder =   
  Pc.Encoder.key (1, Pc.Varint) encoder; 
  encode_command_type v.type_ encoder;
  (match v.side with 
  | Some x -> (  
    Pc.Encoder.key (2, Pc.Varint) encoder; 
    encode_command_side x encoder;)
  | None -> ());
  (match v.speed_value with 
  | Some x -> (  
    Pc.Encoder.key (3, Pc.Varint) encoder; 
    encode_int_as_varint x encoder;)
  | None -> ());
  ()

let rec string_of_command v = 
  add_indentation 1 @@ String.concat "" [    
    (let x = v.type_ in P.sprintf "\ntype_: %s" @@ string_of_command_type x);
    (match v.side with 
    | Some x -> (P.sprintf "\nside: %s" @@ string_of_command_side x)
    | None -> "\nside: None");
    (match v.speed_value with 
    | Some x -> (P.sprintf "\nspeed_value: %i" x)
    | None -> "\nspeed_value: None");
  ]

type sensors = {
  left_encoder : int;
  right_encoder : int;
  us_distance : float;
}

let rec decode_sensors =
  let sensors_mappings = [
    (1, (fun d -> `Int (decode_varint_as_int d)));
    (2, (fun d -> `Int (decode_varint_as_int d)));
    (3, (fun d -> `Float (decode_bits64_as_float d)));
  ]
  in
  (fun d ->
    let a = decode d sensors_mappings (Array.make 4 []) in {      
      left_encoder = required 1 a (function | `Int __v -> __v | _ -> e());
      right_encoder = required 2 a (function | `Int __v -> __v | _ -> e());
      us_distance = required 3 a (function | `Float __v -> __v | _ -> e());
    }
  )

let rec encode_sensors v encoder =   
  Pc.Encoder.key (1, Pc.Varint) encoder; 
  encode_int_as_varint v.left_encoder encoder;
  Pc.Encoder.key (2, Pc.Varint) encoder; 
  encode_int_as_varint v.right_encoder encoder;
  Pc.Encoder.key (3, Pc.Bits64) encoder; 
  encode_float_as_bits64 v.us_distance encoder;
  ()

let rec string_of_sensors v = 
  add_indentation 1 @@ String.concat "" [    
    (let x = v.left_encoder in P.sprintf "\nleft_encoder: %i" x);
    (let x = v.right_encoder in P.sprintf "\nright_encoder: %i" x);
    (let x = v.us_distance in P.sprintf "\nus_distance: %f" x);
  ]

type output_type =
  | Ok 
  | Sensors 

let rec decode_output_type d = 
  match decode_varint_as_int d with
  | 1 -> Ok
  | 2 -> Sensors
  | _ -> failwith "Unknown value for enum output_type"

let rec encode_output_type v encoder =
  match v with
  | Ok -> encode_int_as_varint 1 encoder
  | Sensors -> encode_int_as_varint 2 encoder

let rec string_of_output_type v =
  match v with
  | Ok -> "Ok"
  | Sensors -> "Sensors"

type output = {
  type_ : output_type;
  sensors : sensors option;
}

let rec decode_output =
  let output_mappings = [
    (1, (fun d -> `Output_type (decode_output_type d)));
    (2, (fun d -> `Sensors (decode_sensors (Pc.Decoder.nested d))));
  ]
  in
  (fun d ->
    let a = decode d output_mappings (Array.make 3 []) in {      
      type_ = required 1 a (function | `Output_type __v -> __v | _ -> e());
      sensors = optional 2 a (function | `Sensors __v -> __v | _ -> e());
    }
  )

let rec encode_output v encoder =   
  Pc.Encoder.key (1, Pc.Varint) encoder; 
  encode_output_type v.type_ encoder;
  (match v.sensors with 
  | Some x -> (  
    Pc.Encoder.key (2, Pc.Bytes) encoder; 
    Pc.Encoder.nested (encode_sensors x) encoder;)
  | None -> ());
  ()

let rec string_of_output v = 
  add_indentation 1 @@ String.concat "" [    
    (let x = v.type_ in P.sprintf "\ntype_: %s" @@ string_of_output_type x);
    (match v.sensors with 
    | Some x -> (P.sprintf "\nsensors: %s" @@ string_of_sensors x)
    | None -> "\nsensors: None");
  ]

