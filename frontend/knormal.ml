open Syntax;;
(*open Parser;;*)

(*
let addtyp x = (x, Type.gentyp ()) 
let newid () = addtyp (Id.genid ()) 
*)
let last = ref 97 
let newvar () = let res = ((String.make 1  (char_of_int !last)), Type.gentyp ()) in incr last; res  
let newfct args body = let res = {name = newvar (); args = args; body = body } in res  

let rec knormal ast =
    match ast with
    |Unit -> Unit 
    |Bool a -> Bool a
    |Int a ->  Int a
    |Float a -> Float a
    |Not b -> let (b',t) = newvar () in Let((b',t), knormal b, Not (Var b'))
    |Neg b -> let (b',t) = newvar () in Let((b',t), knormal b, Neg (Var b'))
    |Sub (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b, 
                        Sub(Var a', Var b')) 
                   )
    |Add (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b, 
                        Add(Var a', Var b')) 
                   ) 
    |FAdd (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b, 
                        FAdd(Var a', Var b')) 
                   )
    |FNeg b -> let (b',t) = newvar () in Let((b',t), knormal b, FNeg (Var b'))
    |FSub (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b, 
                        FSub(Var a', Var b')) 
                   )
    |FMul (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b, 
                        FMul(Var a', Var b')) 
                   )
    |FDiv (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b, 
                        FDiv(Var a', Var b')) 
                   )
    |Eq (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b, 
                        Eq (Var a', Var b')) 
                   )
    |LE (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b, 
                        LE (Var a', Var b')) 
                   )
    
    |Var a -> Var a
    |App (a, b) -> App(a, b)
    
    |If (a, b, c) -> If (a, b, c)
    |Tuple a -> Tuple a
    |LetTuple (a, b, c) -> LetTuple (a, b, c)
    |Array (a, b) -> Array (a, b)
    |Get (a, b) -> Get (a, b)
    |Put (a, b, c) -> Put (a, b, c)
    
    |Let (a, b, c) -> Let (a, knormal b, knormal c)
    
    |LetRec (a, b) ->  LetRec (a, b) 

