
type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | IfEq of t * t * t
  | IfLE of t * t * t
  (*| IfBool of t * t * t*)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of Syntax.fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t

val knormal : Syntax.t -> t

val k_to_string : t -> string  
