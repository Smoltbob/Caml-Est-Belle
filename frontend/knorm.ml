let rec K a = match a with
    | Let (a, b, c) ->
    | LetRec (a, b) ->
    | Add (a, b) ->
    | Var x ->
    | Unit ->
    | Bool a ->
    | Int a ->
    | Float a ->
    | Not a ->
    | Neg a ->
    | Sub (a, b) ->
    | FNeg a ->
    | FAdd (a, b) ->
    | FSub (a, b) ->
    | FMul (a, b) ->
    | FDiv (a, b) ->
    | Eq (a, b) ->
    | LE (a, b) ->
    | If (a, b, c) ->
    | App (a, lst) ->
        let rec f = fun lst' -> match lst' with
            | [] -> ()
            | t::q -> set_of_vars t; f q
        in f lst
    | Tuple lst ->
        let rec f = fun lst' -> match lst' with
            | [] -> ()
            | t::q -> set_of_vars t; f q
        in f lst
    | LetTuple (a, b, c) ->
    | Array (a, b) ->
    | Get (a, b) ->
    | Put (a, b, c) ->
    | _ ->
