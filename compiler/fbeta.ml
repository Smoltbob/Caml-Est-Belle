open Fknormal

let find m x =
    if Hashtbl.mem m x then Hashtbl.find m x else Var(x) (*Hashtbl uses = not ==*) 

let rec g (m: (Id.t, Fknormal.t) Hashtbl.t) (k:Fknormal.t) : Fknormal.t  =
    match k with
    |Let ((a,t), b, c) -> let b' = g m b in 
                        let _ = match b' with |Var(x) -> Hashtbl.add m a b' |_->() in
                        Let((a,t), b', g m c)
    |LetRec (a, b) -> 
            (
            let newbody = g m a.body in
            LetRec({name=a.name; args=a.args; body=newbody}, g m b)
            )
    |Unit -> Unit
    |Bool a -> Bool a
    |Int a ->  Int a
    |Float a -> Float a
    |Var a -> find m a 
    |Not b -> Not (g m b) 
    |Neg a -> Neg (g m a) 
    |Sub (a, b) -> Sub (g m a, g m b) 
    |Add (a, b) -> Add (g m a, g m b) 

    |Land (a, b) -> Land (g m a, g m b) 

    |FAdd (a, b) -> FAdd (g m a, g m b) 
    |FNeg a -> FNeg (g m a) 
    |FSub (a, b) -> FSub (g m a, g m b)  
    |FMul (a, b) -> FMul (g m a, g m b) 
    |FDiv (a, b) -> FDiv (g m a, g m b) 
    |Eq (a, b) -> Eq (g m a, g m b)  
    |LE (a, b) -> LE (g m a, g m b) 
    |App (Var(a),b) ->  let a' = find m a in App (a', List.map (g m) b)
    (*for now assume both sides on the comparisons of If's are IDENT*)
    |IfEq (x, y, b, c) -> (let x' = find m x in
                          let y' = find m y in
                          match x',y' with
                          |Var(u), Var(v) -> IfEq(u, v, g m b, g m c)
                          |_ -> failwith "Beta.g: match failure IfEq")
    |IfLE (x, y, b, c) -> (let x' = find m x in
                          let y' = find m y in
                          match x',y' with
                          |Var(u), Var(v) -> IfLE(u, v, g m b, g m c)
                          |_ -> failwith "Beta.g: match failure IfLE")

    |Array (a, b) -> Array (g m a, g m b)
    |Get (a, b) -> Get (g m a, g m b)
    |Put (a, b, c) -> Put (g m a, g m b, g m c)
    |_ -> failwith "Beta.g: NotYetImplemented"
    (* 
    |Tuple a -> Tuple(List.map alpha a)
    |LetTuple (a, b, c) -> LetTuple ( a, alpha b,  alpha c )
    *)

let f (k:Fknormal.t) = 
    let expected_number = 100 in
    let m = Hashtbl.create expected_number in
    g m k
