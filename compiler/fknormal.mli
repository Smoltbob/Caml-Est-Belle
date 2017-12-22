(** This module encapslates the K-normalization step.*)

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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  (*| IfBool of t * t * t*)
  | Let of (Id.t * Ftype.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Ftype.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Ftype.t; args : (Id.t * Ftype.t) list; body : t }

(** K-normalization. Applied to ast, return a flatter version of it: aside from let and letrec, all constructs will have a bounded depth.
@param ast Abstract syntax Tree of a general mincaml program
@return New K-normalized AST*)
val knormal : Fsyntax.t -> t
(** Produces a string out of a K-normalized ast
@param exp t
@return string*)
val k_to_string : t -> string
