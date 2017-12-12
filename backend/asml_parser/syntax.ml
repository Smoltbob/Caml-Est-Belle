open Printf

type t = 
  | Int of int
  | Float of float
  | Add of Id.t * t
  | Sub of Id.t * t
  | Let of Id.t * t * t
  | Var of Id.t
  | Eq of Id.t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string = 
    match l with 
    | [] -> ""
    | [x] -> to_s x
    | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

let rec to_string exp =
    match exp with
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Add (e1, e2) -> sprintf "(%s + %s)" (Id.to_string e1) (to_string e2)
  | Sub (e1, e2) -> sprintf "(%s - %s)" (Id.to_string e1) (to_string e2) 
  | Let (id, e1, e2) -> 
          sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1) (to_string e2)   
  | Var id -> Id.to_string id 
  | Eq (e1, e2) -> sprintf "(%s = %s)" (Id.to_string e1) (to_string e2) 
