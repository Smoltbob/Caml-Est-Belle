(** This module contains only one function unnesting the lets and returning a Fknormal.t *)
open Fknormal;;

(** This function unnests the Lets (and the Apps) (even those inside the bodies of the LetRecs) recursively
@param k is a Fknormal.t
@return an unnested Fknormal.t*)
let rec reduc k = match k with
    | LetRec (f, a) ->
        LetRec ({ name = f.name; args = f.args ; body = (reduc f.body) },
                reduc a)
    | Let (x, a, b) -> (match a with
        | Let (y, a2, b2) -> reduc (Let (y, a2, (reduc (Let (x, b2, b)))))
        | _ -> Let (x, a, reduc b))
    (*(*args are Vars*)
    | App (f, l) -> (* f cannot be a Var so it's not an App nor a Let (see previous part knorm) *)
        let rec reduc_args l = match l with
            | [] -> []
            | t::q -> (reduc t)::(reduc_args q)
        in App (f, reduc_args l)
    *)
    | IfEq(x, y, a, b) -> IfEq(x, y, reduc a, reduc b)
    | IfLE(x, y, a, b) -> IfLE(x, y, reduc a, reduc b)
    | _ -> k
