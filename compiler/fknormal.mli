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
  | Let of (Fid.t * Ftype.t) * t * t
  | Var of Fid.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Fid.t * Ftype.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Fid.t * Ftype.t; args : (Fid.t * Ftype.t) list; body : t }   

val knormal : Fsyntax.t -> t

val k_to_string : t -> string
