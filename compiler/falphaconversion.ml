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
            Let ((a', t), alpha_g ((a,a')::env) b, alpha_g ((a,a')::env) c) 
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
    (*
    |Tuple a -> Tuple(List.map (alpha_g env) a)
    |LetTuple (a, b, c) -> LetTuple ( a, alpha_g env b,  alpha_g env c )
    *)

let alpha k = alpha_g [] k

(* old version
let rec alpha (k_t:Fknormal.t) : Fknormal.t  =
    match k_t with
    |Let (a, b, c) -> (push a);
                      let l=Let ((convert !alphaMap  (fst a), (snd a)), alpha b , alpha c )
                      in pop ();l

    |LetRec (a, b) ->( push a.name;
                       List.iter push a.args;
                       let newname = ((convert !alphaMap (fst a.name)), (snd a.name)) in
                       let newargs = List.map (fun x -> (convert !alphaMap (fst x)), snd x) a.args in
                       let newbody = alpha a.body in
                       List.iter (fun x ->pop ()) a.args;
                       LetRec ({name=newname; args=newargs ; body=newbody}, alpha b)
                     )
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
    |IfEq (x, y, b, c) -> (let n = List.length !alphaMap in
                            let x' = convert !alphaMap x in
                            let y' = convert !alphaMap y in
                            let b' = alpha b in
                            for i = n+1 to List.length !alphaMap do pop () done;
                            let c' = alpha c in
                            for i = n+1 to List.length !alphaMap do pop () done;
                            IfEq(x', y', b', c') 
                          )
    |IfLE (x, y, b, c) -> (let n = List.length !alphaMap in
                            let x' = convert !alphaMap x in
                            let y' = convert !alphaMap y in
                            let b' = alpha b in
                            for i = n+1 to List.length !alphaMap do pop () done;
                            let c' = alpha c in
                            for i = n+1 to List.length !alphaMap do pop () done;
                            IfLE(x', y', b', c') 
                          )
    |Tuple a -> Tuple(List.map alpha a)
    |LetTuple (a, b, c) -> LetTuple ( a, alpha b,  alpha c )
    |Array (a, b) -> Array (alpha a, alpha b)
    |Get (a, b) -> Get (alpha a, alpha b)
    |Put (a, b, c) -> Put (alpha a, alpha b,  alpha c)
    |App (Var(a),b) -> let a' =  Var(convert !alphaMap a) in App (a', List.map alpha b)
    |App (_, b) -> failwith "Falphaconversion.alpha: wrong App"
     *)
