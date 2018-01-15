type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t 
  | Tuple of t list
  | Array of t
  | Var of string

let ctr = ref 0

let gentyp () = let newType= "t"^(Printf.sprintf "%d" !ctr)in incr ctr; 

	Var(newType) 


