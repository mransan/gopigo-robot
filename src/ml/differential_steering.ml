

type t = {
  conversion_ratio : float;
  b: float; 
  mutable previous_right_counter: int; 
  mutable previous_left_counter: int; 
  mutable x : float; 
  mutable y: float; 
  mutable theta: float; 
}

let x {x; _ } = x 
let y {y; _ } = y 
let theta {theta; _ } = theta 

let create conversion_ratio b previous_right_counter previous_left_counter () =  {
  conversion_ratio; 
  b;
  previous_right_counter; 
  previous_left_counter; 
  x = 0.0; 
  y = 0.0; 
  theta = 0.0;
}

let update t right_counter left_counter = 
  let sr = float_of_int (right_counter - t.previous_right_counter) in 
  let sl = float_of_int (left_counter  - t.previous_left_counter) in 
  let s  = (sr +. sl) *. t.conversion_ratio /. 2. in 
  t.theta <- ((sr -. sl) *. t.conversion_ratio /.  (2. *. t.b)) +. t.theta;  
  t.x <- t.x +. s *. (cos t.theta); 
  t.y <- t.y +. s *. (sin t.theta); 
  t.previous_right_counter <- right_counter; 
  t.previous_left_counter <- left_counter; 
  ()
