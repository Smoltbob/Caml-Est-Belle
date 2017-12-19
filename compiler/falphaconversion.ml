open Fknormal;;


let ctr = ref 1 
let alphaMap = ref []

let pop ()  = 
  match !alphaMap with
  | h::tl -> alphaMap:= tl 
  |_ ->()

let rec push   x =
  ctr:=!ctr+1 ; alphaMap:= ((fst x), (fst x) ^ string_of_int !ctr)::(!alphaMap)


let rec convert lst x =
 match lst with
            |(v, v1)::tl -> if v =  x then  v1
                            else convert tl  x
            |_ ->  x


let rec alpha (k_t:Fknormal.t) : Fknormal.t  =
    match k_t with
    |Let (a, b, c) -> (push  a); 
                      let l=Let ((convert !alphaMap  (fst a), (snd a)), alpha b , alpha c ) 
                      in pop ();l
                      
    |LetRec (a, b) -> List.map push a.args; 
                     let l= LetRec ({name=(a.name); args= List.map (fun x -> (convert !alphaMap (fst x)), snd x) a.args; body=(alpha a.body)}, alpha b) 
                     in pop ();l


    |Var a -> Var (convert !alphaMap a)

    |Unit -> Unit   
    |Bool a -> Bool a
    |Int a ->  Int a
    |Float a -> Float a
    |Not b -> Not (alpha b)
    |Neg b -> Neg (alpha  b)
    |Sub (a, b) -> Sub(alpha  a, alpha  b)                  
    |Add (a, b) -> Add(alpha  a, alpha b)                  
    |FAdd (a, b) -> FAdd(alpha  a, alpha b)
    |FNeg b -> FNeg (alpha  b)
    |FSub (a, b) -> FSub(alpha a, alpha b)
    |FMul (a, b) -> FMul(alpha a, alpha b)                 
    |FDiv (a, b) -> FDiv (alpha a, alpha b)
    |Eq (a, b) -> Eq (alpha a, alpha b)
    |LE (a, b) -> LE (alpha a, alpha b)
    |IfEq (x, y, b, c) -> IfEq (alpha x, alpha y, alpha b,  alpha c )
    |IfLE (x, y, b, c) -> IfLE (alpha x, alpha y, alpha b,  alpha c )
    |Tuple a -> Tuple(List.map alpha a)
    |LetTuple (a, b, c) -> LetTuple ( a, alpha b,  alpha c )
    |Array (a, b) -> Array (alpha a, alpha b)
    |Get (a, b) -> Get (alpha a, alpha b)
    |Put (a, b, c) -> Put (alpha a, alpha b,  alpha c)  
    |App (a,b) ->  App (alpha a,List.map alpha b)
    

