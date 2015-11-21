open Lwt.Infix 

type t = bool ref 

let create () = ref false 

let interupt done_  = 
  done_ := true 

let exec ?delay done_ f = 
  if !done_
  then Lwt.return_unit 
  else 
   match delay with 
   | None -> f () 
   | Some delay -> Lwt_unix.sleep delay >>= f 

let iter ?delay done_ f = 
  let next = match delay with 
    | None -> f 
    | Some delay -> (fun () -> 
      f () >>= (fun () -> Lwt_unix.sleep delay) 
    ) 
  in 

  let rec loop () = 
    if !done_ 
    then Lwt.return_unit 
    else next () >>= loop 
  in 

  loop () 

