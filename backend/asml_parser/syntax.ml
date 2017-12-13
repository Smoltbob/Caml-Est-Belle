open Printf

type t = 
  | Int of int
  | Float of float
  | Neg of Id.t
  | Fneg of Id.t
  | Fsub of Id.t * Id.t
  | Fadd of Id.t * Id.t
  | Fmul of Id.t * Id.t
  | Fdiv of Id.t * Id.t
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
  | Neg id -> sprintf "(neg %s)" (Id.to_string id)
  | Fneg id -> sprintf "(fneg %s)" (Id.to_string id)
  | Fadd (id1, id2) -> sprintf "(fadd %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Fsub (id1, id2) -> sprintf "(fsub %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Fmul (id1, id2) -> sprintf "(fmul %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Fdiv (id1, id2) -> sprintf "(fdiv %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Add (e1, e2) -> sprintf "(add %s %s)" (Id.to_string e1) (to_string e2)
  | Sub (e1, e2) -> sprintf "(sub %s %s)" (Id.to_string e1) (to_string e2) 
  | Let (id, e1, e2) -> 
          sprintf "(let %s = %s in %s)" (Id.to_string id) (to_string e1) (to_string e2)   
  | Var id -> Id.to_string id 
  | Eq (e1, e2) -> sprintf "(%s = %s)" (Id.to_string e1) (to_string e2) 
