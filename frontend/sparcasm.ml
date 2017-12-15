type t = string
type exp =
    |
    |
and fundef = {
                name : Id.l;
                args : Id.t list; (* int arguments *)
                fargs : Id.t list; (* float arguments *)
                body : SparcAsm.t;
                ret : Type.t (* return type *)
            }
and prog = Prog of (Id.l * float) list * SparcAsm.fundef list * SparcAsm.t

val fv : SparcAsm.t -> Id.t list (* use order *)
val f : Closure.prog -> prog

(* private *)
val data : (Id.l * float) list ref (* float table *)
val h : Closure.fundef -> fundef
val g : Type.t M.t -> Closure.t -> t
