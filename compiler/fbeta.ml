open Fknormal

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
    |Var a -> if Hashtbl.mem m a then Hashtbl.find m a else Var a (*Hashtbl uses = not ==*) 
    |Not b -> Not (g m b) 
    |Neg a -> Neg (g m a) 
    |Sub (a, b) -> Sub (g m a, g m b) 
    |Add (a, b) -> Add (g m a, g m b) 
    |FAdd (a, b) -> FAdd (g m a, g m b) 
    |FNeg a -> FNeg (g m a) 
    |FSub (a, b) -> FSub (g m a, g m b)  
    |FMul (a, b) -> FMul (g m a, g m b) 
    |FDiv (a, b) -> FDiv (g m a, g m b) 
    |Eq (a, b) -> Eq (g m a, g m b)  
    |LE (a, b) -> LE (g m a, g m b) 
    |App (Var(a),b) ->  let a' = if Hashtbl.mem m a then Hashtbl.find m a else Var(a) in App (a', List.map (g m) b)
    |_ -> failwith "Beta.g: NotYetImplemented"
    (* 
    |IfEq (x, y, b, c) -> IfEq (convert !alphaMap x, convert !alphaMap y, alpha b,  alpha c )
    |IfLE (x, y, b, c) -> IfLE (convert !alphaMap x, convert !alphaMap y, alpha b,  alpha c )
    |Tuple a -> Tuple(List.map alpha a)
    |LetTuple (a, b, c) -> LetTuple ( a, alpha b,  alpha c )
    |Array (a, b) -> Array (alpha a, alpha b)
    |Get (a, b) -> Get (alpha a, alpha b)
    |Put (a, b, c) -> Put (alpha a, alpha b,  alpha c)
    *)

let f (k:Fknormal.t) = 
    let expected_number = 100 in
    let m = Hashtbl.create expected_number in
    g m k
