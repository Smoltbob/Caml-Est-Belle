(** This module contains functions which outputs a string from a Fclosure.t or outputs a Bsyntax.toplevel used as an input by the backend *)

open Fclosure;;
open Printf;;
open Bsyntax;;


(** This function takes care of the base cases such as sums and variables.
@param t is a Fclosure.t
@return a Bsyntax.t *)
let rec asml_t_triv t = match t with
    | Unit -> Nop
    | Int a -> Int a
    | Float a -> Float a
    | Neg x -> (match x with
                        | (Var y) -> Neg y
                        | _ -> failwith "matchfailure Neg")
    | FNeg x -> (match x with
                        | (Var y) -> Fneg y
                        | _ -> failwith "matchfailure FNeg")
    | FSub (x, y) -> (match x, y with
                        | (Var x2, Var y2) -> Fsub (x2, y2)
                        | _ -> failwith "matchfailure FSub")
    | FAdd (x, y) -> (match x, y with
                        | (Var x2, Var y2) -> Fadd (x2, y2)
                        | _ -> failwith "matchfailure FAdd")
    | FMul (x, y) -> (match x, y with
                        | (Var x2, Var y2) -> Fmul (x2, y2)
                        | _ -> failwith "matchfailure FMul")
    | FDiv (x, y) -> (match x, y with
                        | (Var x2, Var y2) -> Fdiv (x2, y2)
                        | _ -> failwith "matchfailure FDiv")
    | Add (x, y) -> (match x, y with
                        | (Var x2, Var y2) -> Add (x2, y2)
                        | _ -> failwith "matchfailure Add")
    | Sub (x, y) -> (match x, y with
                        | (Var x2, Var y2) -> Sub (x2, y2)
                        | _ -> failwith "matchfailure Sub")
    | AppD (f, l) ->
        (let rec trans (l:Fclosure.t list) :Bsyntax.formal_args = match l with
            | [] -> []
            | (Var x)::q -> (x:Id.t)::(trans q)
            | _ -> failwith "not a list of variables. Maybe the argument is of type unit ?"
        in
        Call (f, trans l))
    | Var x -> Var x
    | _ -> failwith "asml_t_triv matchfailure"

(** This function this is a recursive function on Let, AppD and (LetRec TBA). It calls asml_t_triv when it encounters a simple case that ends the recursion like a sum.
@param c is an Fclosure.t
@return an Bsyntax.asmt*)
let rec asml_exp (c:Fclosure.t) :asmt = match c with
    | Let (x, a, b) -> Let (fst x, asml_t_triv a, asml_exp b)
    (* | LetRec (fundef, a) -> LetRec ({name = }) *)
    | _ -> Expression (asml_t_triv c)

    (* | _ -> failwith "asml_exp matchfailure" *)


let create_main c = {name = "_"; args = []; body = asml_exp c}

let rec asml_list c = match c with
    | LetRec (f,a) -> ({
                        name = fst f.name;
                        args = List.map fst f.args;
                        body = (asml_exp f.body)
                      })
                      ::(asml_list a)
    | _ -> [create_main c]

(* let asml_fundefs c = Fundefs (asml_list c) *)

let asml_head c = Fundefs (asml_list c)

(** This function is used to output the string to generate the asml file.
@param exp is an Fclosure.t*)
let rec closure_to_asmlstring (exp:Fclosure.t) : string = match exp with
    | Unit -> "nop"
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Var id -> Id.to_string id
    (* | Let (x, a, b) -> sprintf *)
    | Add (e1, e2) -> sprintf "add %s %s \n" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
    | Sub (e1, e2) -> sprintf "sub %s %s \n" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
    | Let ((id,t), e1, e2) -> sprintf "let %s = %s in\n %s"
        (Id.to_string id)
        (closure_to_asmlstring e1)
        (closure_to_asmlstring e2)
    | AppD (f, args) -> sprintf "call %s %s\n"
        (f)
        (infix_to_string closure_to_asmlstring args " ")
    | LetRec (fd, e) -> sprintf "let rec %s %s =\n %s in\n %s"
        (let (x, _) = fd.name in (Id.to_string x))
        (infix_to_string (fun (x,_) -> (Id.to_string x)) fd.args " ")
        (closure_to_asmlstring fd.body)   (*CHANGE LATER*)
        (closure_to_asmlstring e)
    | _ -> "\n[[ match not found in asml gen ]]\n"

(* Do not delete this *)
(*
  | Not e -> sprintf "(not %s)" (closure_to_asmlstring e)
  | Neg e -> sprintf "(- %s)" (closure_to_asmlstring e)
  | Float f -> sprintf "%.2f" f
  | FNeg e -> sprintf "(-. %s)" (closure_to_asmlstring e)
  | FAdd (e1, e2) -> sprintf "(%s +. %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | FSub (e1, e2) -> sprintf "(%s -. %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | FMul (e1, e2) -> sprintf "(%s *. %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | FDiv (e1, e2) -> sprintf "(%s /. %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | Eq (e1, e2) -> sprintf "(%s = %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | LE (e1, e2) -> sprintf "(%s <= %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2) *)
  (* | IfEq (e1, e2, e3) ->
          sprintf "(if %s then %s else %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2) (closure_to_asmlstring e3)
  | IfLE (e1, e2, e3) ->
          sprintf "(if %s then %s else %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2) (closure_to_asmlstring e3)
  | LetTuple (l, e1, e2)->
          sprintf "(let (%s) = %s in %s)"
          (infix_to_string (fun (x, _) -> Id.to_string x) l ", ")
          (closure_to_asmlstring e1)
          (closure_to_asmlstring e2)
  | Get(e1, e2) -> sprintf "%s.(%s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | Put(e1, e2, e3) -> sprintf "(%s.(%s) <- %s)"
                 (closure_to_asmlstring e1) (closure_to_asmlstring e2) (closure_to_asmlstring e3)
  | Tuple(l) -> sprintf "(%s)" (infix_to_string closure_to_asmlstring l ", ")
  | Array(e1,e2) -> sprintf "(Array.create %s %s)"
       (closure_to_asmlstring e1) (closure_to_asmlstring e2) *)

(** Temporary function to print the starting let _ = at the beginning of the asml file *)
let closure_to_asmlstring_main (exp:Fclosure.t) : string =
   sprintf "let _ = \n %s" (closure_to_asmlstring exp)
