(** This module contains functions which outputs a string from a Fclosure.t or outputs a Bsyntax.toplevel used as an input by the backend *)

open Fclosure;;
open Printf;;
open Bsyntax;;


(*TODO unnest toplevel that are inside lets and inside letrecs and put them before the function call of the function they are defined in.*)
(* let rec unnest_letcls toplvl = match toplvl with
    | _ -> ()

let rec addmem_to_letrecs l = match l with
    | t::q -> ()

let rec addmem_to_letrecs_with_fv toplvl = match toplvl with
    | Fundefs l -> addmem_to_letrecs l *)

(*TODO use this function to alloc the pointer of the function as well as the pointers to the fv*)
let rec mem_fv_closure addr fv count call = match fv with
    | t::q -> Let (
                "tu"^(string_of_int count),
                MemAff (addr, 4*count, t),
                mem_fv_closure addr q (count+1)) (*TODO change the name of tu+id*)
    | [] -> call

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
    (*TODO : unnest the letcls if it is inside another let  and replace appc with callc*)
    | AppC (c, l) -> LetCls (c, New (c, 1 + List.length fv),
                        Let ("tu0", MemAff (addr, 0, c (*TODO retrieve the addr of c*)),
                        mem_fv_closure addr fv 4 (Expression (CallC (c, l)))))
    | Var x -> Var x
    | _ -> failwith "asml_t_triv matchfailure not implemented"

(** This function this is a recursive function on Let, AppD and (LetRec TBA). It calls asml_t_triv when it encounters a simple case that ends the recursion like a sum.
@param c is an Fclosure.t
@return an Bsyntax.asmt*)
let rec asml_exp (c:Fclosure.t) :asmt = match c with
    (*TODO unnest the letcls from the lets*)
    | Let (x, AppC (c,l), b) -> LetCls (c, New (c, 1 + List.length fv),
                        Let ("tu0", MemAff (addr, 0, c (*TODO retrieve the addr of c*)),
                        mem_fv_closure addr fv 4 (Let (x, CallC (c, l), asml_exp b))))
    | Let (x, a, b) -> Let (fst x, asml_t_triv a, asml_exp b)
    | _ -> Expression (asml_t_triv c)

let create_main c = {name = "_"; args = []; body = asml_exp c}

let rec asml_list c = match c with
    | LetRec (f,a) -> ({
                        name = fst f.name;
                        args = List.map fst f.args;
                        body = (asml_exp f.body)
                      })
                      ::(asml_list a)
    | _ -> [create_main c]

let asml_head c = Fundefs (asml_list c)


let rec expression_to_string exp = match exp with
    | Nop -> "nop"
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Var id -> Id.to_string id
    | Add (e1, e2) -> sprintf "add %s %s" e1 e2
    | Sub (e1, e2) -> sprintf "sub %s %s" e1 e2
    | Call (f, args) -> sprintf "call %s %s"
        f
        (infix_to_string (fun x->x) args " ")
    | New i -> sprintf "new %i" i
    | MemAcc (id, i) -> sprintf "mem(%s+%i)"
        (Id.to_string id)
        i
    | MemAff (id1, i, id2) -> sprintf "mem(%s+%i) <- %s"
        (Id.to_string id1)
        i
        (Id.to_string id2)
    | _ -> "\n[[ match not found in asml gen ]]\n"

let rec asmt_to_string (body:Bsyntax.asmt) = match body with
    | Let (id, e1, e2) -> sprintf "let %s = %s in\n\t%s"
        (Id.to_string id)
        (expression_to_string e1)
        (asmt_to_string e2)
    | LeCls (id, e1, e2) ->  sprintf "let %s = %s in\n\t%s"
        (Id.to_string id)
        (expression_to_string e1)
        (asmt_to_string e2)
    | Expression t -> expression_to_string t

let fundef_to_string (fund:fundef) =
    sprintf "let %s %s =\n\t%s in\n"
        (Id.to_string fund.name)
        (infix_to_string (fun x -> (Id.to_string x)) fund.args " ")
        (asmt_to_string fund.body)

let rec list_to_string (l) = match l with
    | t::q -> sprintf "%s\n\n%s" (fundef_to_string t) (list_to_string q)
    | [] -> ""

let toplevel_to_string (toplvl:toplevel) = match toplvl with
    | Fundefs l -> list_to_string l
