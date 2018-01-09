(** This module will unnest the letrecs in the next versions. For now it only transforms a Fknormal.t into a Fclosure.t
The function returning a string is just a dbeugging function for now *)
open Fknormal;;

type t =
(* uncomment those lines when ready to create closures *)
    (*| LetFclosure of (Id.t * Ftype.t) * (Id.l * Ftype.t) * t list*)
    (*| AppC of (Id.l * t list)*)
    | AppD of (Id.t * t list)
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
    | IfBool of t * t * t
    | Let of (Id.t * Ftype.t) * t * t
    | Var of Id.t
    | LetRec of fundef * t
    | Tuple of t list
    | LetTuple of (Id.t * Ftype.t) list * t * t
    | Array of t * t
    | Get of t * t
    | Put of t * t * t
and fundef = {
                name : Id.t * Ftype.t;
                args : (Id.t * Ftype.t) list;
                formal_fv : (Id.t * Ftype.t) list;
                body : t
            }

(*type prog = Prog of fundef list * t*)

(* type toplevel = fundef list *)

val clos_out : Fknormal.t -> t
(* val clos : Fknormal.t -> Fknormal.t *)
val clos_exp : Fknormal.t -> t
(** This function is for debugging purpose only, it returns its argument as a string *)
val clos_to_string : t -> string

(* val clos_toplevel : Fknormal.t -> toplevel *)
(* val fv = t -> S.t *)

(* val toplevel : fundef list ref *)
(* val g = Ftype.t M.t -> S.t -> Fsyntax.t -> t *)
