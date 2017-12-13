let rec reduc k = match k with
    | Let(x, a, b) -> match a with
        | Let(y, a', b') -> Let(y, reduc a', reduc Let(x, b', b))
        | _ -> Let(x, a, reduc b)
    | _ -> k
