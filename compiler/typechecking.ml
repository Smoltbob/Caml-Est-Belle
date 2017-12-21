open Fsyntax;;
open Ftype;;


(* eq has a type of (Ftype.t* Ftype.t) list *)
let  eq = ref []

(* env has a type of (Fid.t* Ftype.t) list *)
let  env = ref []

(*let  env = ref [("print_int" ,Fun( [Int] , Unit ))]*)
(*for later : after defining string type*)
(*let  env = ref [( "print_string",Fun([string],Unit )); ("print_int" ,Fun( [Int] , Unit ))]*)



let  printSize lst =  
  let i= List.length lst in print_int i  


let rec getTypeString t=
  match t with
    | Unit ->  "unit"
    | Bool->  "bool"
    | Int->  "int"
    | Float->  "float"
    | Fun (a,b)-> "fun"
    | Tuple  a-> "tuple"
    | Array a-> "array"
    | Var a->  "var"; (match !a with 
                                            |None ->  " none"
                                            |Some x -> getTypeString x
                                  )
    | _->  "undef"


let rec print_Type t=
  let str=  getTypeString t in 
    print_string str


let rec findVar lst x =
 (* let i= List.length lst in print_int i;
  print_string "\n";
  print_string "find in environment the variable " ;
  print_string x ;print_string "\n";*)
  match lst with
    |(id, t)::tl -> (*print_string id ; print_string "\n";print_Type t;print_string "\n";*)
                    if id =  x then  t
                    else findVar tl  x
    |_ ->   failwith (Printf.sprintf "Unbound value: %s" x)
   (*print x : the name of the variable*)


let rec print_Eq lst =
  match lst with
    |(t1, t2)::tl -> print_Type t1 ;
                     print_string "->";
                     print_Type t2;
                     print_string "\n";
                     print_Eq tl  
    |_ -> print_string "done\n"          
                  

let  getType () =
  match !eq with
  |(t1, t2)::tl -> t1
  |_->failwith " Undefined Type"


let checkForVar x=
  (*print_Type x;print_string "\n";*)
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
    else   let s1= getTypeString(fst x) in let s2= getTypeString(snd x) in 
      failwith (Printf.sprintf "Expression has type %s but an expression was 
        expected of type  %s" s1 s2)
     
 

let rec append l1 l2 =
  match l1 with
  | h :: t -> h :: append t l2
  | [] -> l2


(*TODO enhance the return of eq*)
(*we could return the well typed AST*)
let rec genEquations  (expr:Fsyntax.t) tp :(Ftype.t* Ftype.t) list =
  match expr with
    | Unit ->   updateEq (Unit, tp)
    | Bool b -> updateEq(Bool, tp) 
    | Int i -> (*print_string "eq int \n";*) updateEq(Int, tp) 
    | Float f -> (*print_string "eq float \n";*) updateEq(Float, tp) 

    | Not e ->  genEquations e Bool ;
                updateEq(Bool, tp) 

    | Add (e1, e2) ->  (*print_string "eq add1 \n"; *)
                        genEquations e1 Int ;
                        (*print_string "eq add2 \n";*)
                        genEquations e2 Int ;
                       (* print_string "eq add3 \n";*)
                        updateEq(Int, tp)  

    | Sub (e1, e2) -> genEquations e1 Int;
                      genEquations e2 Int ;
                      updateEq(Int, tp) 

    | Neg e ->  genEquations e Bool ;
                updateEq(Bool, tp) 

    | FNeg e -> genEquations e Bool ;
                updateEq(Bool, tp) 

    | FAdd (e1, e2) ->  genEquations e1 Float ;
                        genEquations e2 Float ;
                        updateEq(Float, tp)  

    | FSub (e1, e2) ->  genEquations e1 Float;
                        genEquations e2 Float ;
                        updateEq(Float, tp) 

    | FMul (e1, e2) ->  genEquations e1 Float;
                        genEquations e2 Float ;
                        updateEq(Float, tp) 

    | FDiv (e1, e2) ->  genEquations e1 Float;
                        genEquations e2 Float ;
                        updateEq(Float, tp)                      

   | Let ((id,t), e1, e2) -> genEquations  e1 t ;
                             (* TODO maybewe should update the AST types t:=getType ();*)
                          (*   print_Type t;  print_string "\n";*)
                             env:=(id, getType ())::!env;
                             genEquations e2 tp 


    | Var x -> (* print_string "eq var \n";*)updateEq(findVar !env x, tp) 


    | LetRec (fd, e) -> 
                        env:= append fd.args !env;
                       (* print_string "eq letrec1 \n";*)
                        genEquations fd.body (Var(ref None));
                       (* print_string "eq letrec2 \n";*)
                        let (a,b)= fd.name in
                        env:=(a, getType ())::!env;
                       (* print_string "eq letrec3 \n";*)
                        genEquations e tp;
                      (*  print_string "eq letrec4 \n";*)
                        !eq
  

    |_ ->failwith "Not implemented yet"

(*   
  | App (e1, le2) ->
  | Eq (e1, e2) -> 
  | LE (e1, e2) -> 
  | If (e1, e2, e3) ->          
  | LetTuple (l, e1, e2)-> 
  | Get(e1, e2) -> 
  | Put(e1, e2, e3) ->               
  | Tuple(l) -> 
  | Array(e1,e2) ->
*)