open Syntax;;

let rec reduc k = match k with
    | Let (x, a, b) -> (match a with
        | Let (y, a2, b2) ->
            let aux = (reduc (Let (x, b2, b))) in
                Let (y, (reduc a2), aux)
        | _ -> Let (x, a, reduc b))
    | _ -> k
