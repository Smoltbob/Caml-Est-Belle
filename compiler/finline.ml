open Fknormal

let threshold = ref 10

let rec find x env =
    match env with
    |[] -> None
    |((h,_),m)::q -> if h=x then Some m else (find x q)

let rec size x =
    match x with
    |LetRec(a,b) -> 1 + size b (*questionable, to be revisited*)
    |Let(a,b,c) -> 1 + (size b) + (size c)
    |IfEq(x, y, a, b) -> 1 + (size a) + (size b)
    |IfLE(x, y, a, b) -> 1 + (size a) + (size b)
    |_ -> 1

let rec g env (k:Fknormal.t) : Fknormal.t =
    match k with
    |LetRec(a,b) -> let body' = g env a.body in (*not adding a.name to env in case of recursion*)
            if (size body') < !threshold then
                LetRec({name=a.name; args=a.args; body=body'}, g ((a.name, (a.args, body'))::env) b)
            else
                LetRec({name=a.name; args=a.args; body=body'}, g env b)
    |Let(a,b,c) -> Let(a, g env b, g env c) 
    |IfEq(x, y, a, b) -> IfEq(x, y, g env a, g env b) 
    |IfLE(x, y, a, b) -> IfLE(x, y, g env a, g env b) 
    |Let(a,b,c) -> Let(a, g env b, g env c) 
    |App((Var x), ys) -> (match (find x env) with
                    |None -> App((Var x), ys)
                    |Some (zs, e) -> let env' = List.map2 (fun (z,_) (Var y) -> (z,y)) zs ys in
                                     Falphaconversion.alpha_g env' e
                   )
    |_ -> k

let f k = g [] k

