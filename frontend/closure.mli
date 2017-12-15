open Knormal;;

type t =
(* uncomment those lines when ready to create closures *)
    (*| LetClosure of (Id.t * Type.t) * (Id.l * Type.t) * t list*)
    (*| AppD of (Id.t * t list)*)
    (*| AppC of (Id.l * t list)*)
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
    | IfBool of t * t * t
    | Let of (Id.t * Type.t) * t * t
    | Var of Id.t
    | LetRec of Syntax.fundef * t
    | Tuple of t list
    | LetTuple of (Id.t * Type.t) list * t * t
    | Array of t * t
    | Get of t * t
    | Put of t * t * t
and fundef = {
                name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t
            }

(*type prog = Prog of fundef list * t*)

type toplevel = fundef list

val clos = Knormal.t -> Closure.t
(* val clos_first = Knormal.t -> Closure.toplevel *)
(* val fv = t -> S.t *)

val toplevel = fundef list ref
(* val g = Type.t M.t -> S.t -> Syntax.t -> t *)
