open Syntax;;
(*open Parser;;*)

(*
let addtyp x = (x, Type.gentyp ()) 
let newid () = addtyp (Id.genid ()) 
*)
let last = ref 97 
let newvar () = let res = ((String.make 1  (char_of_int !last)), Type.gentyp ()) in incr last; res  

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
    |Add (a, b) -> print_string "HERE ";print_newline ();
                    let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b, 
                        Add(Var a', Var b')) 
                   ) 
    |FNeg b -> let (b',t) = newvar () in Let((b',t), knormal b, FNeg (Var b'))
    |FAdd (a, b) -> FAdd (knormal a, knormal b)
    |FSub (a, b) -> FSub (knormal a, knormal b)
    |FMul (a, b) -> FMul (knormal a, knormal b)
    |FDiv (a, b) -> FDiv (knormal a, knormal b)
    |Eq (a, b) -> Eq (a, b)
    |LE (a, b) -> LE (a, b)
    |If (a, b, c) -> If (a, b, c)
    |Var a -> Var a
    |App (a, b) -> App (a, b)
    |Tuple a -> Tuple a
    |LetTuple (a, b, c) -> LetTuple (a, b, c)
    |Array (a, b) -> Array (a, b)
    |Get (a, b) -> Get (a, b)
    |Put (a, b, c) -> Put (a, b, c)
    |Let (a, b, c) -> Let (a, knormal b, knormal c)
    |LetRec (a, b) ->  LetRec (a, b) 

