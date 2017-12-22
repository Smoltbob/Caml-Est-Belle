(** This module make sure that all the bound variables
  (variables defined by a let or functions parameters) are different.
*)
open Fknormal;;

(**
  a counter to create a unique variable names
*)
let ctr = ref 1
(**
  a map between old varibale names and new varible names
*)
let alphaMap = ref []

(**
  this function is to remove last tuple from alphaMap
*)
let pop ()  =
  match !alphaMap with
  | h::tl -> alphaMap:= tl
  |_ ->()

(**
  this function is to add new pair (old name, new name) to alphaMap
  @param x tuple(old name, new name)
*)
let rec push   x =
  ctr:=!ctr+1 ; alphaMap:= ((fst x), (fst x) ^ string_of_int !ctr)::(!alphaMap)

(**
  this function is to find the new name of the variable in alphaMap list
  @param lst the alphaMap list
  @param x the old variable name
  @return the new variable name
*)
let rec convert lst x =
 match lst with
            |(v, v1)::tl -> if v =  x then  v1
                            else convert tl  x
            |_ ->  x

(**
  this fuction is to parse the AST and create a new and unique name for each variable
  @param k_t:Fknormal.t the knormalized AST
  @return  AST with new names
*)
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
    |IfEq (x, y, b, c) -> IfEq (convert !alphaMap x, convert !alphaMap y, alpha b,  alpha c )
    |IfLE (x, y, b, c) -> IfLE (convert !alphaMap x, convert !alphaMap y, alpha b,  alpha c )
    |Tuple a -> Tuple(List.map alpha a)
    |LetTuple (a, b, c) -> LetTuple ( a, alpha b,  alpha c )
    |Array (a, b) -> Array (alpha a, alpha b)
    |Get (a, b) -> Get (alpha a, alpha b)
    |Put (a, b, c) -> Put (alpha a, alpha b,  alpha c)
    |App (a,b) ->  App (alpha a,List.map alpha b)
