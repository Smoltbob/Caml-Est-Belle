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

let rename x =
    let c = !ctr in incr ctr;
    x ^ string_of_int c


    
(**
  this fuction is to parse the AST and create a new and unique name for each variable
  @param k_t:Fknormal.t the knormalized AST
  @return  AST with new names
*)
let rec alpha_g env (k_t:Fknormal.t) : Fknormal.t  =
    match k_t with
    |Let ((a,t), b, c) -> let a' = rename a in
            Let ((a', t), alpha_g env b, alpha_g ((a,a')::env) c) 
    |LetRec (a, b) -> let name' = (rename (fst a.name), snd a.name) in
                      let args' = List.map (fun (x,t)->(rename x,t)) a.args in
                      let inenv = (fst a.name, fst name')::env in
                      let funenv = (List.map2 (fun (x,_) (y,_) -> (x,y)) a.args args') @ inenv in
                      LetRec({name=name'; args=args'; body=(alpha_g funenv a.body)}, alpha_g inenv b)
    |Var a -> Var (convert env a)
    |Unit -> Unit
    |Bool a -> Bool a
    |Int a ->  Int a
    |Float a -> Float a
    |Not b -> Not (alpha_g env b)
    |Neg b -> Neg (alpha_g env  b)
    |Sub (a, b) -> Sub(alpha_g env  a, alpha_g env  b)
    |Add (a, b) -> Add(alpha_g env  a, alpha_g env b)

    |Land (a, b) -> Land(alpha_g env  a, alpha_g env b)

    |FAdd (a, b) -> FAdd(alpha_g env  a, alpha_g env b)
    |FNeg b -> FNeg (alpha_g env  b)
    |FSub (a, b) -> FSub(alpha_g env a, alpha_g env b)
    |FMul (a, b) -> FMul(alpha_g env a, alpha_g env b)
    |FDiv (a, b) -> FDiv (alpha_g env a, alpha_g env b)
    |Eq (a, b) -> Eq (alpha_g env a, alpha_g env b)
    |LE (a, b) -> LE (alpha_g env a, alpha_g env b)
    |IfEq (x, y, b, c) -> let x' = convert env x in
                          let y' = convert env y in
                          IfEq(x', y', alpha_g env b, alpha_g env c) 
    |IfLE (x, y, b, c) -> let x' = convert env x in
                          let y' = convert env y in
                          IfLE(x', y', alpha_g env b, alpha_g env c) 
    |Array (a, b) -> Array (alpha_g env a, alpha_g env b)
    |Get (a, b) -> Get (alpha_g env a, alpha_g env b)
    |Put (a, b, c) -> Put (alpha_g env a, alpha_g env b,  alpha_g env c)
    |App (a,b) -> App (alpha_g env a, List.map (alpha_g env) b)
    |_ -> failwith "AlphaConv:g Match failure"

let alpha k = alpha_g [] k

