open Fknormal
let rec effect ast =
    match ast with
    |Let(a,b,c) -> (effect b) || (effect c)
    |LetRec(a,b) -> effect b
    |IfEq(u, v, a, b) -> (effect a) || (effect b)
    |IfLE(u, v, a, b) -> (effect a) || (effect b)
    |LetTuple (a, b, c) ->  (effect b) || (effect c)
    |Put (a, b, c) -> true 
    |App (a, b) -> true (*improvement: access the body of*)
    |_ -> false


let rec fv x ast =
    match ast with
    |Let(a,b,c) -> (fv x b) || (fv x c)
    |LetRec(a,b) -> (fv x a.body) || (fv x b)
    |Var a -> a = x
    |Unit -> false 
    |Bool a -> false 
    |Int a -> false
    |Float a -> false
    |Not b -> fv x b 
    |Neg b -> fv x b 
    |Sub (a, b) -> (fv x a) || (fv x b)
    |Add (a, b) -> (fv x a) || (fv x b)

    |Land (a, b) -> (fv x a) || (fv x b)

    |FAdd (a, b) -> (fv x a) || (fv x b)
    |FNeg b ->  fv x b 
    |FSub (a, b) -> (fv x a) || (fv x b) 
    |FMul (a, b) -> (fv x a) || (fv x b) 
    |FDiv (a, b) -> (fv x a) || (fv x b)
    |Eq (a, b) -> (fv x a) || (fv x b) 
    |LE (a, b) -> (fv x a) || (fv x b)
    |IfEq(u, v, a, b) -> x=u || x=v || (fv x a) || (fv x b)
    |IfLE(u, v, a, b) -> x=u || x=v || (fv x a) || (fv x b)
    |Tuple a -> List.fold_left (fun b->(fun y->(b||(fv x y)))) false a
    |LetTuple (a, b, c) ->  (fv x b) || (fv x c)
    |Array (a, b) -> (fv x a) || (fv x b) 
    |Get (a, b) -> (fv x a) || (fv x b) 
    |Put (a, b, c) -> (fv x a) || (fv x b) 
    |App (a, b) -> (fv x a) || (List.fold_left (fun b->(fun y->(b||(fv x y)))) false b)
 

let rec f ast =
    match ast with
    |Let(a,b,c) -> if (effect b) || (fv (fst a) c) then Let(a, f b, f c) else (f c)
    |LetRec(a, b) -> if (fv (fst a.name) b) then LetRec({name=a.name; args=a.args; body=(f a.body)}, f b) else (f b)
    |IfEq(x, y, a, b) -> IfEq(x, y, f a, f b)
    |IfLE(x, y, a, b) -> IfLE(x, y, f a, f b)
    |_ -> ast
