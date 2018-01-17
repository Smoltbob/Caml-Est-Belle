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
        | _ -> Let (x, reduc a, reduc b))
    | IfEq(x, y, a, b) -> IfEq(x, y, reduc a, reduc b)
    | IfLE(x, y, a, b) -> IfLE(x, y, reduc a, reduc b)
    | _ -> k
