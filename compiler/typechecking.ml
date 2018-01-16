(** This module is for program type checking*)
open Fsyntax;;
open Ftype;;


(** list of equations , it has a type of (Ftype.t* Ftype.t) list *)
let  eq = ref []


(**This fuction is to search for a variable in the environment
  @param lst the environment list ((Fid.t* Ftype.t) list)
  @param x the variable we want to find (Fid.t)
  @return if the variable is found in the environment return its type 
          else return a failwith message "Unbound value x"
*)
let rec findVar lst x =
  match lst with
    |(id, t)::tl -> if id =  x then  t
                    else  findVar tl  x
    |_ ->   failwith (Printf.sprintf "Unbound value: %s" x)
 


(**This function is to generate a new type strings for a list of arguments by using Ftype.gentyp()
@param args list of arguments 
@return list of generated types
*)
let rec generateTypes args = 
  match args with
  | h::tl ->  Ftype.gentyp() :: generateTypes tl
  |_->[]
  


(** This function is to append two lists
  @param l1 first list
  @param l2 second list
  @return list contains l1 then l2 content
*)
let rec append l1 l2 =
  match l1 with
  | h :: t -> h :: append t l2
  | [] -> l2


(**This is just a helpful function to display function equation content
  @param a function arguments list
  @param b function output
  @return the equivalent string 
*)
let rec getfunType a b =  
  match a with
  | h::t -> h^","^getfunType t b
  | _ -> "RETURN " ^b
  

let rec getTupleType a  =  
  match a with
  | h::t -> h^","^getTupleType t 
  | _ -> "" 
  

(**This is just a helpful function that display the equivalent string of Ftype.t 
  @param t a variable of type Ftype.t
  @return the equivalent string of any type
*)
let rec getTypeString t=
  match t with
    | Unit ->  "unit"
    | Bool->  "bool"
    | Int->  "int"
    | Float->  "float"
    | Fun (a,b)-> let out= getTypeString b in 
    let l = List.map getTypeString a in 
                 "FUN " ^getfunType l out
    | Tuple  a-> let l = List.map getTypeString a in 
                 "Tuple " ^getTupleType l 
    | Array a-> "Array " ^getTypeString a
    | Var a->   a


(**This is just a helpful function that  display the equations list  content
  @param lst equations list
  @return the equivalent string 
*)
let rec printEq lst =
  match lst with
    |(t1, t2)::tl -> print_string(getTypeString t1) ;
                     print_string "->";
                     print_string(getTypeString t2);
                     print_string "\n";
                     printEq tl  
    |_ -> print_string "!!!done\n"  

let rec toArray eq = 
   match eq with
   | (a,b)::tl -> (Array(a),b)::tl
   | _ -> []

(** This function is  generate the the equation list 
  @param env  the environment in which the program is type checked (Fid.t* Ftype.t) list
  @param expr the parsed program that we want to check
  @param tp  the type that the program must have (unit)
  @return list of equations [(Ftype.t* Ftype.t)] 
*)
let rec genEquations  env (expr:Fsyntax.t)  tp  =
  match expr with
    | Unit ->   [(Unit, tp)]
    | Bool b -> [(Bool, tp)] 
    | Int i ->  [(Int, tp) ]
    | Float f -> [(Float, tp)]
    | Not e ->  eq:=genEquations env e Bool ;
                eq:=(Bool, tp) ::!eq;!eq

    | Add (e1, e2) -> let eq1= genEquations env e1 Int in  
                      let eq2 = genEquations env e2 Int in 
                      append ((Int, tp)::eq1) eq2

    | Sub (e1, e2) ->  let eq1= genEquations env e1 Int in  
                    let eq2 = genEquations env e2 Int in 
                    append ((Int, tp)::eq1) eq2

    | Neg e ->  let eq1= genEquations env e tp in eq1

    | FNeg e -> let eq1= genEquations env e Bool in  
                (Bool, tp) ::eq1 

    | FAdd (e1, e2) ->  let eq1= genEquations env e1 Float in  
                        let eq2 = genEquations env e2 Float in 
                        append ((Float, tp)::eq1) eq2 

    | FSub (e1, e2) ->  let eq1= genEquations env e1 Float in  
                      let eq2 = genEquations env e2 Float in 
                      append ((Float, tp)::eq1) eq2


    | FMul (e1, e2) ->  let eq1= genEquations env e1 Float in  
                        let eq2 = genEquations env e2 Float in 
                        append ((Float, tp)::eq1) eq2

    | FDiv (e1, e2) ->  let eq1= genEquations env e1 Float in  
                        let eq2 = genEquations env e2 Float in 
                        append ((Float, tp)::eq1) eq2 



    | Let ((id,t), e1, e2) -> 
                              let eq1= genEquations  env e1 t in
                              let eq2 = genEquations ((id, t)::env) e2 tp in 
                              append eq1 eq2                         

    | Var x -> 
    let t= findVar env x in
               [(t, tp)] 


    | LetRec (fd, e) ->  let (a,b)= fd.name in
                         let eq1=  genEquations ((a, Fun( List.map snd fd.args , b ))::env) e tp in 
                         let eq2 = genEquations ((a, Fun( List.map snd fd.args , b ))::(append fd.args env)) fd.body b in 
                         append eq1 eq2

    | App (e1, le2) -> 
        let ts = generateTypes le2 in (*create special case for predef??*)
       
        let eq1 = genEquations env e1 (Fun( ts , tp )) in   
      
        let eq2 = 
          let rec gen l1 l2 =
            match l1, l2 with
            |[h1],[h2]->genEquations env h1 h2
            | h1::t1, h2::t2 -> append (genEquations env h1 h2) (gen t1 t2)
            |_->[]
          in  gen le2 ts
        in append eq1 eq2

    | If (e1, e2, e3) ->  let eq1=  genEquations env e1 Bool in 
                          let eq2=  genEquations env e2 tp in 
                          let eq3= genEquations env e3 tp in 
                          append(append eq1 eq2) eq3

    | Eq (e1, e2) -> let ts = Ftype.gentyp() in
                     let eq1= genEquations env e1 ts in 
                     let eq2= genEquations env e2 ts in 
                     append eq1 eq2

    | LE (e1, e2) -> let ts = Ftype.gentyp() in
                     let eq1= genEquations env e1 ts in 
                     let eq2= genEquations env e2 ts in  
                     append eq1 eq2          

   | LetTuple (l, e1, e2)-> 
                            let eq1= genEquations  env e1 (Tuple(List.map snd l)) in
                            let eq2 = genEquations (append l env) e2 tp in 
                            append eq1 eq2    
            
    | Tuple(l) ->  
        let ts = generateTypes l in (*create special case for predef*)
        let eq1= (Tuple(ts), tp )::!eq in 
        let eq2 = 
          let rec gen l1 l2 =
            match l1, l2 with
            |[h1],[h2]->genEquations env h1 h2
            | h1::t1, h2::t2 -> append (genEquations env h1 h2) (gen t1 t2)
            |_->[]
          in gen l ts
        in append eq1 eq2

  | Get(e1, e2) ->  
                    let eq1 = genEquations env e1 (Array(tp)) in 
                    let eq2 = genEquations env e2 Int in 
                    append eq1 eq2

  | Put(e1, e2, e3) -> 
  let eq1= genEquations env e2 Int in    
                        let eq2= genEquations env e3 tp in
                        let eq3= genEquations env e1 tp in
                    
                        append eq3 (append eq1 (toArray eq2)) 

  | Array(e1,e2) -> let eq2 =genEquations env e2 (tp) in
                    let eq1= genEquations env e1 Int in
                    append eq1 (toArray eq2)




(**This function is to check if x is of type  Var
  @param x type to be checked
  @return true if the type is var 
          false everything else
*)
let isVar x=
   match x with
   | Var a -> true
   | _ -> false


(**This function is to check if x is of type Fun
  @param x type to be checked
  @return true if the type is a function 
          false everything else
*)
let isFun x=
   match x with
   |Fun (a,b)-> true
   | _ -> false


let isTup x=
   match x with
   |Tuple a-> true
   | _ -> false

let isArray x=
   match x with
   |Array a-> true
   | _ -> false


(**This function is to check if x is of type int, float, unit or bool
  @param x type to be checked
  @return true if the type is int, float, unit or bool 
          false everything else
*)
let isSimple x=
   match x with
   | Var a -> false
   | Fun (a,b)-> false
   |Tuple a-> false
   |Array a-> false
   | _ -> true


(**This function is to map between types (Fun( [args1] , out1 ) -> Fun( [args2] , out2 ))
  @param lst list of equations 
  @param tp1 first function type Fun( t*list , t )
  @param tp2 second function type Fun( t*list , t )
  @return list of equations with the new generated pairs
*)
let rec checkFun lst tp1 tp2 = 
  match tp1 ,tp2 with
                  | Fun (a1,b1) ,Fun (a2,b2)-> 
                   if not ((List.length a1)=(List.length a2)) then
                     failwith "number of function arguments is incorrect\n" 
                   else begin               
                    let args = let rec gen l1 l2 =
                        match l1, l2 with
                          |[h1],[h2]->[(h1,h2)]
                          | h1::t1, h2::t2 -> (h1,h2)::(gen t1 t2)
                          |_->failwith("function type mismatch")
                      in gen a1 a2
                  in (b1,b2)::(append args lst)  
                    end     
                  | _ -> []

let rec checkTuple lst tp1 tp2 = 
  match tp1 ,tp2 with
                  | Tuple a1 ,Tuple a2-> 
                   if not ((List.length a1)=(List.length a2)) then
                     failwith "number of Tuple arguments is incorrect\n" 
                   else begin               
                    let args = let rec gen l1 l2 =
                        match l1, l2 with
                          |[h1],[h2]->[(h1,h2)]
                          | h1::t1, h2::t2 -> (h1,h2)::(gen t1 t2)
                          |_->failwith("Tuple type mismatch")
                      in gen a1 a2
                  in append args lst
                    end     
                  | _ -> []

let rec checkArray lst tp1 tp2 = 
  match tp1 ,tp2 with
                  | Array a1 ,Array a2->  (a1,a2)::lst   
                  | _ -> []


 let rec replcaeinList ls tp1 tp2 = 
     match ls with
                    | h::t -> if h= tp2 then 
                                tp1::replcaeinList t tp1 tp2
                              else
                                h::replcaeinList t tp1 tp2
                    | _ -> []  


let rec advReplace t tp1 tp2= 
    match t with
      |Fun (a,b) -> if b=tp2 then Fun((replcaeinList a tp1 tp2),tp1)
                    else Fun((replcaeinList a tp1 tp2),b)
      |Tuple (a) -> Tuple(replcaeinList a tp1 tp2)
      |Array a -> if a= tp2 then Array tp1 else Array a
      | _ -> t



     

(** this function is to replace the occurrences of type in all equations list 
  @param lst list of equations 
  @param tp1 first type t
  @param tp2 second type t
  @return  equations list with occurrences of var replaced by t
*)
let rec replace lst tp1 tp2 = 
    match lst with
      | [] -> []
      | (t1,t2)::tl-> if t1= tp2 then  
                        (tp1,t2)::replace tl tp1 tp2
                      else if   t2= tp2 then   
                        (t1,tp1)::replace tl tp1 tp2

                      else if isFun t1 && isFun t2 then 
                          (advReplace t1 tp1 tp2,advReplace t2 tp1 tp2)::replace tl tp1 tp2
                      else if isFun t1 then
                         (advReplace t1 tp1 tp2,t2)::replace tl tp1 tp2
                      else if isFun t2 then 
                          (t1,advReplace t2 tp1 tp2)::replace tl tp1 tp2
                

                      else if isTup t1 && isTup t2 then 
                          (advReplace t1 tp1 tp2,advReplace t2 tp1 tp2)::replace tl tp1 tp2
                      else if isTup t1 then
                          (advReplace t1 tp1 tp2,t2)::replace tl tp1 tp2
                      else if isTup t2 then 
                        (t1,advReplace t2 tp1 tp2)::replace tl tp1 tp2

                      else if isArray t1 && isArray t2 then 
                          (advReplace t1 tp1 tp2,advReplace t2 tp1 tp2)::replace tl tp1 tp2  
                      else if isArray t1 then 
                      begin
                      

                          (advReplace t1 tp1 tp2,t2)::replace tl tp1 tp2
                      end
                      else if isArray t2 then  begin
                    

                        (t1,advReplace t2 tp1 tp2)::replace tl tp1 tp2
                      end

                      else
                        (t1,t2)::replace tl tp1 tp2



(** this function is to solve the tree of equations
  @param lst list of equations to be solved
  @return  well typed message if everything is correct or failwith 
*)
let rec unification lst = 
match lst with
| [] ->   print_string "well typed\n";()
| (t1,t2)::tl-> (
                if t1= t2 then 
                begin
            
                
                   unification tl
                 end
                else      
                if  ((isSimple t1)&&(isSimple t2) )then
                    begin
                     
                     let s1= getTypeString t1 in let s2= getTypeString t2 in 
                    failwith (Printf.sprintf "Expression has type %s but an expression was 
                    expected of type  %s" s1 s2)
                   end
                else if ( (isFun t1) &&  (isFun t2))then 
                  begin
                     let lst2 = checkFun tl t1 t2 in
                 
                     unification lst2
                  end
                else if ( (isTup t1) &&  (isTup t2))then 
                  begin
                     let lst2 = checkTuple tl t1 t2 in
                      
                     unification lst2
                  end  
                else if ( (isArray t1) &&  (isArray t2))then 
                  begin
                    
                     let lst2 = checkArray tl t1 t2 in
                   
                     unification lst2
                  end      
                else if (not (isVar t1) && not (isVar t2))then
                   begin
                    
                     let s1= getTypeString t1 in let s2= getTypeString t2 in 
                    failwith (Printf.sprintf "Expression has type %s but an expression was 
                    expected of type  %s" s1 s2)
                   end
              else if (isVar t2) then      
                begin        
                  let lst2= replace tl t1 t2 in 
                    
                     unification lst2
                end
              else if (isVar t1) then      
                begin                    
                  let lst2= replace tl t2 t1 in 
             
                  unification lst2
                end
)

