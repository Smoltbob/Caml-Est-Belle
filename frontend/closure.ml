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
    | If of t * t * t
    | Let of (Id.t * Type.t) * t * t
    | Var of Id.t
    | LetRec of fundef * t
    | App of t * t list
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
and prog = Prog of fundef list * t

(* Not sure what S.t and M.t are yet *)
val f = Syntax.t -> prog
val fv = t -> S.t

val toplevel = fundef list ref
val g = Type.t M.t -> S.t -> Syntax.t -> t
