open Fknormal;;

type t =
(* uncomment those lines when ready to create closures *)
    (*| LetFclosure of (Fid.t * Ftype.t) * (Fid.l * Ftype.t) * t list*)
    (* | AppD of (Fid.t * t list) *)
    (*| AppC of (Fid.l * t list)*)
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
    | Let of (Fid.t * Ftype.t) * t * t
    | Var of Fid.t
    | LetRec of Fsyntax.fundef * t
    | Tuple of t list
    | LetTuple of (Fid.t * Ftype.t) list * t * t
    | Array of t * t
    | Get of t * t
    | Put of t * t * t
and fundef = {
                name : Fid.l * Ftype.t;
                args : (Fid.t * Ftype.t) list;
                formal_fv : (Fid.t * Ftype.t) list;
                body : t
            }

(*type prog = Prog of fundef list * t*)

(* type toplevel = fundef list *)

val clos : Fknormal.t -> t
(* val clos_toplevel : Fknormal.t -> toplevel *)
(* val fv = t -> S.t *)

(* val toplevel : fundef list ref *)
(* val g = Ftype.t M.t -> S.t -> Fsyntax.t -> t *)
