(** This module contains functions which outputs a string from a Fclosure.t or outputs a Bsyntax.toplevel used as an input by the backend *)

open Fclosure;;
open Printf;;
open Bsyntax;;

let rec to_fargs (l:Fclosure.t list) = match l with
    | t::q -> (* we can only have Var in the t list *)
                (match t with
                | Var x -> x::(to_fargs q)
                | _ -> failwith "argument not Var")
    | [] -> []


let rec mem_fv_letrec (name:Id.t) fv count call = match fv with
    | t::q -> Let (
                t,
                MemAcc ("%"^name, Int (4*count)),
                mem_fv_letrec name q (count+1) call)
    | [] -> call

(*TODO use this function to alloc the pointer of the function as well as the pointers to the fv*)
let rec mem_fv_closure addr fv count call = match fv with
    | t::q -> Let (
                "tu"^(string_of_int count),
                MemAff (addr, Int (4*count), t),
                mem_fv_closure addr q (count+1) call)
    | [] -> call

(** This function takes care of the base cases such as sums and variables.
@param t is a Fclosure.t
@return a Bsyntax.t *)
let rec asml_t_triv t = match t with
    | Unit -> Nop
    | Int a -> Int a
    | Float a -> Float a
    | Var x -> Var x
    | Neg x -> (match x with
                        | (Var y) -> Neg y
                        | _ -> failwith "matchfailure Neg")
    (* | FNeg x -> (match x with
                        | (Var y) -> Fneg y
                        | _ -> failwith "matchfailure FNeg") *)
    (* | FSub (x, y) -> (match x, y with
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
                        | _ -> failwith "matchfailure FDiv") *)
    | Add (x, y) -> (match x, y with
                        | (Var x2, Var y2) -> Add (x2, Var y2)
                        | _ -> failwith "matchfailure Add")
    | Sub (x, y) -> (match x, y with
                        | (Var x2, Var y2) -> Sub (x2, Var y2)
                        | _ -> failwith "matchfailure Sub")
    | AppD (f, l) -> (*if f.[0] = '_' then
                        Call (f, to_fargs l)
                     else
                        Let (f^"aux", MemAcc (f, Int 0),*)
                        Call (f, to_fargs l)
    | AppC (c, l) -> CallClo (c, to_fargs l)
    (*TODO check the requierments on types for get and put*)
    | Get (a, b) -> (match a, b with
                        | (Var a2, Var b2) -> MemAcc (a2, Var b2)
                        | _ -> failwith "matchfailure Get")
    | Put (a, b, c) -> (match a, c with
                        | (Var a2, Var c2) -> MemAff (a2, asml_t_triv b, c2)
                        | _ -> failwith "matchfailure Put")
    | Array (a, b) -> (match a, b with
                        | Var a2, Var b2 -> Call ("_min_caml_create_array", [a2; b2])
                        | _ -> failwith "matchfailure Array")
    | IfEq (id1, id2, t1, t2) -> If (id1, Var id2, asml_exp t1, asml_exp t2, "beq")
    | IfLE (id1, id2, t1, t2) -> If (id1, Var id2, asml_exp t1, asml_exp t2, "ble")
    (* | IfBool (t1, t2, t3) -> If (id1, t, asmt, asmt, string) *)
    | _ -> failwith "asml_t_triv matchfailure not implemented"

(** This function this is a recursive function on Let, AppD and (LetRec TBA). It calls asml_t_triv when it encounters a simple case that ends the recursion like a sum.
@param c is an Fclosure.t
@return an Bsyntax.asmt*)
and asml_exp (c:Fclosure.t) :asmt = match c with
    | Let (x, a, b) -> Let (fst x, asml_t_triv a, asml_exp b)
    | LetCls (clo, f, l, t) ->
                        LetCls (clo, New (Int (1 + List.length l)),
                        Let ("tu0", MemAff (clo, Int 0, clo (*TODO retrieve the addr of c*)),
                        mem_fv_closure f l 1 (asml_exp t)))
    | _ -> Expression (asml_t_triv c)

let create_main c = {name = "_"; args = []; body = asml_exp c}

let rec asml_list c = match c with
    | LetRec (f,a) -> ({
                        name = fst f.name;
                        args = List.map fst f.args;
                        body = mem_fv_letrec (fst f.name) (List.map fst f.formal_fv) 1 (asml_exp f.body)
                      })
                      ::(asml_list a)
    | _ -> [create_main c]

let asml_head c = Fundefs (asml_list c)


let rec expression_to_string exp = match exp with
    | Nop -> "nop"
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Var id -> Id.to_string id
    | Add (e1, e2) -> sprintf "add %s %s" e1 (expression_to_string e2)
    | Sub (e1, e2) -> sprintf "sub %s %s" e1 (expression_to_string e2)
    | Call (f, args) -> sprintf "call %s %s"
        f
        (infix_to_string (fun x->x) args " ")
    | CallClo (c, args) -> sprintf "apply_closure %s %s"
        c
        (infix_to_string (fun x->x) args " ")
    | New i -> sprintf "new %s" (expression_to_string i)
    | MemAcc (id, i) -> sprintf "mem(%s+%s)"
        (Id.to_string id)
        (expression_to_string i)
    | MemAff (id1, i, id2) -> sprintf "mem(%s+%s) <- %s"
        (Id.to_string id1)
        (expression_to_string i)
        (Id.to_string id2)
    | If (id, t, a, b, s) -> sprintf "if %s %s %s\n\tthen\n\t%s\n\telse\n\t%s\n"
        (Id.to_string id)
        (if s = "beq" then "=" else "<=")
        (expression_to_string t)
        (asmt_to_string a)
        (asmt_to_string b)
    | _ -> "\n[[ match not found in asml gen ]]\n"

and asmt_to_string (body:Bsyntax.asmt) = match body with
    | Let (id, e1, e2) -> sprintf "let %s = %s in\n\t%s"
        (Id.to_string id)
        (expression_to_string e1)
        (asmt_to_string e2)
    | LetCls (id, e1, e2) ->  sprintf "let %s = %s in\n\t%s"
        (Id.to_string id)
        (expression_to_string e1)
        (asmt_to_string e2)
    | Expression t -> expression_to_string t

let fundef_to_string (fund:fundef) =
    sprintf "let %s %s =\n\t%s\n"
        (Id.to_string fund.name)
        (infix_to_string (fun x -> (Id.to_string x)) fund.args " ")
        (asmt_to_string fund.body)

let rec list_to_string (l) = match l with
    | t::q -> sprintf "%s\n\n%s" (fundef_to_string t) (list_to_string q)
    | [] -> ""

let toplevel_to_string (toplvl:toplevel) = match toplvl with
    | Fundefs l -> list_to_string l
