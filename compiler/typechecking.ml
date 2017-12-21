open Fsyntax;;
open Ftype;;





(* env has a type of (Fid.t* Ftype.t) list *)
let  env = ref []
(*for later : after defining string type*)
(*let  env = ref [( "print_string",(string,unit )); ("print_int" ,( [Int] , Unit ))]*)


(* eq has a type of (Ftype.t* Ftype.t) list *)
let  eq = ref []

let  printSize lst =  
       let i= List.length lst in print_int i  




let rec print_Type t=
  match t with
    | Unit -> print_string "unit"
    | Bool-> print_string "bool"
    | Int-> print_string "int"
    | Float-> print_string "float"
    | Fun (a,b)->print_string "fun"
    | Tuple  a->print_string "tuple"
    | Array a->print_string "array"
    | Var a-> print_string "var"; (match !a with 
                                            |None -> print_string "none"
                                            |Some x -> print_Type x
                                          )
    | _-> print_string "undef"

let rec findVar lst x =
  let i= List.length lst in print_int i;
  print_string "\n";
  print_string "find in environment the variable " ;
  print_string x ;print_string "\n";
  match lst with
    |(id, t)::tl ->  print_string id ; print_string "\n";print_Type t;print_string "\n";
    if id =  x then  t
                    else findVar tl  x
    |_ ->  failwith " Undefined Varible" (*print x : the name of the variable*)




let rec print_Eq lst =
  match lst with
    |(t1, t2)::tl -> print_Type t1 ;
                     print_string "->";
                     print_Type t2;
                    print_string "\n";
                     print_Eq tl  
    |_ -> print_string "!!!\n"          
                                  
let  getType () =
  match !eq with
  |(t1, t2)::tl -> t1
  |_->failwith " Undefined Type"

let checkForVar x=
  print_Type x;print_string "\n";
   match x with
   | Var a -> true

   | _ -> false

let updateType t1 t2=
    match t1 with
    | Var a -> t2
    | _ -> t1


let  updateEq x =
  (* eq:=x::!eq;!eq *)
  if (fst x) = (snd x) || checkForVar (snd x)  || checkForVar (fst x)then  
    begin
          eq:=x::!eq;!eq
    end
    else 
      failwith "Type mismatch"
    
let rec append l1 l2 =
  match l1 with
  | h :: t -> h :: append t l2
  | [] -> l2


(*TODO enhance the return of eq*)
(*we could retutn the well typed AST*)

let rec genEquations  (expr:Fsyntax.t) tp :(Ftype.t* Ftype.t) list =
  match expr with
    | Unit ->   updateEq (Unit, tp)
    | Bool b -> updateEq(Bool, tp) 
    | Int i -> print_string "eq int \n"; updateEq(Int, tp) 
    | Float f -> print_string "eq float \n"; updateEq(Float, tp) 

    | Not e ->  genEquations e Bool ;
                updateEq(Bool, tp) 

    | Add (e1, e2) ->  print_string "eq add1 \n";
                        genEquations e1 Int ;
                        print_string "eq add2 \n";
                        genEquations e2 Int ;
                        print_string "eq add3 \n";
                        updateEq(Int, tp)  

   | Let ((id,t), e1, e2) -> genEquations  e1 t ;
                             (* t:=getType ();*)
                              print_Type t;  print_string "\n";
                              env:=(id, getType ())::!env;
                              genEquations e2 tp
                                  
                             (*eq1 :: eq2*) 

    | Neg e ->  genEquations e Bool ;
               updateEq(Bool, tp) 
    | Sub (e1, e2) -> genEquations e1 Int;
                      genEquations e2 Int ;
                      updateEq(Int, tp) 


    | Var x ->  print_string "eq var \n";updateEq(findVar !env x, tp) 


    | LetRec (fd, e) -> 
                        env:= append fd.args !env;
                        print_string "eq letrec1 \n";
                        genEquations fd.body (Var(ref None));
                        print_string "eq letrec2 \n";
                      
                        print_string "eq letrec3 \n";
                        genEquations e tp;
                        print_string "eq letrec4 \n";
                        !eq
           
    |_ ->failwith "Not implemented yet"

      (*    

           | App (e1, le2) ->

    | FNeg e -> let eq = genEquations e Bool;  (Bool, tp) :: eq
    | FAdd (e1, e2) -> let eq1 = genEquations e1 Float;
                       let eq2 = genEquations e2 Float;
                       (Float, tp) :: eq1 :: eq2 
    | FSub (e1, e2) -> let eq1 = genEquations e1 Float;
                       let eq2 = genEquations e2 Float;
                       (Float, tp) :: eq1 :: eq2 
    | FMul (e1, e2) -> let eq1 = genEquations e1 Float;
                       let eq2 = genEquations e2 Float;
                       (Float, tp) :: eq1 :: eq2 
    | FDiv (e1, e2) -> let eq1 = genEquations e1 Float;
                       let eq2 = genEquations e2 Float;
                       (Float, tp) :: eq1 :: eq2 

  | Eq (e1, e2) -> 
  | LE (e1, e2) -> 
  | If (e1, e2, e3) ->          
  | App (e1, le2) -> 

  | LetTuple (l, e1, e2)-> 
  | Get(e1, e2) -> 
  | Put(e1, e2, e3) ->               
  | Tuple(l) -> 
  | Array(e1,e2) ->
*)
(*
genEquations(env, expr, type) =
  case on expr:
    // only one equation "unit = type"
    UNIT -> return [ (unit, type) ]  
    // similarly "Int = type", "Float = type"
    Int -> return [ (Int, type) ]
    Float -> return  [ (Float, type) ]
    // "not expr2" is a Boolean, so we must have "Bool = type"
    // but expr2 must be a Boolean, so we generate the additional
    // equations "GenEquation(env, expr2, Bool)"
    Not expr2 -> 
       eq = genEquations(env, expr2, Bool)
       return (Bool, type) :: eq
    Add (expr1, expr2) ->
       eq1 = genEquations(env, expr1, Int)
       eq2 = genEquations(env, expr2, Int)
       return (Int, type) :: eq1 :: eq2
    //  This case corresponds to a program of the form
    // Let (x : id_type) = exrp1 in expr2
    // expr1 must be of the type id_type, so we generate 
    // genEquations(env, expr1, id_type) 
    // expr2 must be of type "type", but in a new environment where
    // x is of type id_type, so we generate
    // genEquations(env + (id -> id_type), expr2, type)
    Let (id, id_type, expr1, expr2) ->
       eq1 = genEquations(env, expr1, id_type)  
       eq2 = genEquations(env + (id -> id_type), expr2, type)
       return e1 :: eq2
    // a variable x has a type t1 that must be known from the environment 
    // (possibly a type variable)
    // then, we have only one equation "ty = type" 
    Var (x) ->
       t1 = env(x)  (* error if not defined *)
       return [(t1, type)]
    ... to be completed!
    Letrec ...  ->  
    App ... ->*)
