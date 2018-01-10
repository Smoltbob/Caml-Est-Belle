(** This module is for program type checking*)
open Fsyntax;;
open Ftype;;


(** list of equations , it has a type of (Ftype.t* Ftype.t) list *)
let  eq = ref []

(** the environment in which the program is typechecked 
    it has a type of (Fid.t* Ftype.t) list *)
let  env = ref [("print_int" ,Fun( [Int] , Unit ));("print_float" ,Fun( [Float] , Unit ))]
(*let  env = ref [] *)
(*for later : after defining string type*)
(*let  env = ref [( "print_string",Fun([string],Unit )); ("print_int" ,Fun( [Int] , Unit ))]*)


(**This is just a helpful function that  get the string of Ftype.t 
to display the equation or the environment content
@param t a variable of type Ftype.t
@return the equivalent string of any type
*)
let rec getTypeString t=
  match t with
    | Unit ->  "unit"
    | Bool->  "bool"
    | Int->  "int"
    | Float->  "float"
    | Fun (a,b)-> "fun"
    | Tuple  a-> "tuple"
    | Array a-> "array"
    | Var a->   (match !a with 
                                |None ->  "var none"
                                |Some x -> getTypeString x)


(**This is just a helpful function to print the content of equation list
  @param equation list
  @return print_string the equatuon content (type->type)
*)
let rec print_Eq lst =
  match lst with
    |(t1, t2)::tl -> print_string(getTypeString t1) ;
                     print_string "->";
                     print_string(getTypeString t2);
                     print_string "\n";
                     print_Eq tl  
    |_ -> print_string "done\n"          
                  


(*let  printSize lst =  
  let i= List.length lst in print_int i *) 

(**This fuction is to search for a variable in the environment
  @param lst the environment list ((Fid.t* Ftype.t) list)
  @param x the variable we want to find (Fid.t)
  @return if the variable is found in the environment return its type 
          else return a failwith message "Unbound value x"
*)
let rec findVar lst x =
(* let i= List.length lst in print_int i;
  print_string "\n";
  print_string "find in environment the variable " ;
  print_string x ;print_string "\n";*)
  match lst with
    |(id, t)::tl ->(* print_string id ; print_string "\n";print_string(getTypeString t);print_string "\n";*)
                    if id =  x then  t
                    else findVar tl  x
    |_ ->   failwith (Printf.sprintf "Unbound value: %s" x)
   (*print x : the name of the variable*)


(**This function is to get the head of equation list and return first type
  @return first type in equation head pair (Ftype.t)
*)
let  getType () =
  match !eq with
  |(t1, t2)::tl -> t1
  |_->failwith " Undefined Type"


(** This function is to check if the variable type is equal to Var
  @param x type to be checked (Ftype.t)
  @return if the type is Var return true else false
*)
let checkForVar x=
  (*print_Type x;print_string "\n";*)
   match x with
   | Var a -> true
   | _ -> false

(*
let updateType t1 t2=
    match t1 with
    | Var a -> t2
    | _ -> t1
*)

(** This function is to check if the new calculated equation pair is valid and add it to the equation list 
@param x pair of (Ftype.t* Ftype.t)
@return if it's valid return updated equation list 
        else failwith message "Expression has type %s but an expression was expected of type %s"
*)
let  updateEq x =
  (* eq:=x::!eq;!eq *)
  if (fst x) = (snd x) || checkForVar (snd x)  || checkForVar (fst x)then  
    begin
          eq:=x::!eq;
    end
    else   let s1= getTypeString(fst x) in let s2= getTypeString(snd x) in 
      failwith (Printf.sprintf "Expression has type %s but an expression was 
        expected of type  %s" s1 s2)
     
 
(** This is function is to append two lists
  @param l1 first list
  @param l2 second list
  @return list contains l1 then l2 content
*)
let rec append l1 l2 =
  match l1 with
  | h :: t -> h :: append t l2
  | [] -> l2




(*TODO enhance the return of eq*)
(*we could return the well typed AST*)
(** This function is to caculate the types for the AST 
    and generate the the equation list and the environment
  @param expr the parsed program that we want to check
  @param tp  the type that the program must have (unit)
  @return unit if everything is correct or failwith 
*)
let rec genEquations  (expr:Fsyntax.t) tp  =
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

    | Neg e ->  genEquations e Bool;
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
                             (* TODO maybe we should update the AST types t:=getType ();*)
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

    (*just check print_int and print_float.TODO enhance later*)
    | App (e1, le2) -> (match e1 with
                        | Var id -> (
                            if id="print_int" || id= "print_float" then
                                 begin  
                                    let x=findVar !env id in  (* Fun of t list * t *)
                                   ( match x with
                                    | Fun (a,b)-> ( 
                                      if not ((List.length le2)=(List.length a)) then
                                          failwith "number of function arguments is incorrect\n" (*for later enhance this error msg*)
                                     else
                                       begin
                                         genEquations (List.hd le2) (List.hd a) ; 
                                        updateEq(Unit, tp)
                                       end
                                      )
                                    | _ -> failwith "Fun type was expected\n"
                                   )
                                  
                              end
                            else
                              print_string "calling functions not implemented yet\n"
                        )

                        | _ -> print_string "calling functions not implemented yet\n")

    | If (e1, e2, e3) ->  genEquations e1 Bool ;
                          genEquations e2 tp;
                          genEquations e3 tp

    | Eq (e1, e2) -> genEquations e1 (Var(ref None));
                     genEquations e2 (getType())

    | LE (e1, e2) -> genEquations e1 (Var(ref None));
                     genEquations e2 (getType ())

    |_ ->print_string "there is a type not implemented yet\n"

(*   
  | App (e1, le2) ->         
  | LetTuple (l, e1, e2)-> 
  | Get(e1, e2) -> 
  | Put(e1, e2, e3) ->               
  | Tuple(l) -> 
  | Array(e1,e2) ->
*)