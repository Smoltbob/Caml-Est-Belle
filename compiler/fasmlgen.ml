(** This module contains functions which outputs a string from a Fclosure.t or outputs a Bsyntax.toplevel used as an input by the backend *)

open Fclosure;;
open Printf;;
open Bsyntax;;

(** Transforms a list of args from fclosure into a list of args of bsyntax
    @param l is a Fclosure.t list
    @return formal_args *)
let rec to_fargs (l:Fclosure.t list) = match l with
    | t::q -> (* we can only have Var in the t list *)
                (match t with
                | Var x -> x::(to_fargs q)
                | _ -> failwith "argument not Var")
    | [] -> []

(** Creates the memory access lines to retrieve the free variables from memory and appends call at the end.
    @param name is an Id.t
    @param fv is the list of free variables
    @param count is used to count the offset for the variables
    @param call is an asmt
    @return asmt *)
let rec mem_fv_letrec (name:Id.t) fv count call = match fv with
    | t::q -> Let (
                t,
                MemAcc ("%"^name, Int count),
                mem_fv_letrec name q (count+1) call)
    | [] -> call

(** Creates the memory affectation lines to put the free variables in memory and appends call at the end of the instructions.
    @param name is an Id.t
    @param fv is the list of free variables
    @param count is used to count the offset for the variables
    @param call is an asmt
    @return asmt *)
let rec mem_fv_closure name fv count call = match fv with
    | t::q -> Let (
                "tu"^(string_of_int count)^"a",
                MemAff (name, Int count, t),
                mem_fv_closure name q (count+1) call)
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
    | Get (a, b) -> (match a with
                        | Var a2 -> MemAcc (a2, asml_t_triv b)
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

(** This function is a recursive function on Let and LetCls. It calls asml_t_triv when it encounters a simple case that ends the recursion (like a sum for instance)
    @param c is an Fclosure.t
    @return a Bsyntax.asmt*)
and asml_exp (c:Fclosure.t) :asmt = match c with
    | Let (x, a, b) -> Let (fst x, asml_t_triv a, asml_exp b)
    | LetCls (clo, f, l, t) ->
                        (* LetCls (clo, New (Int (1 + List.length l)), *)
                        Let (clo, New (Int (1 + List.length l)),
                        Let ("addr"^f, Var f,
                        Let ("tu0a", MemAff (clo, Int 0, clo),
                        mem_fv_closure f l 1 (asml_exp t))))
    | _ -> Expression (asml_t_triv c)

(** Creates the "let _" main and add asml_exp c to it's body to use the types defined in bsyntax.
    @param c is an Fclosure.t
    @return a fundef*)
let create_main c = {name = "_"; args = []; body = asml_exp c}

(** Executes asml_exp on the body of the function definitions and adds the memory access to the free variable if any.
The closure starts with letrecs thanks to the previous unnesting. Anything that isn't a letrec is added to the main let using create_main.
    @param c is a Fclosure.t
    @return a fundef list*)
let rec asml_list c = match c with
    | LetRec (f,a) -> ({
                        name = fst f.name;
                        args = List.map fst f.args;
                        body = mem_fv_letrec (fst f.name) (List.map fst f.formal_fv) 1 (asml_exp f.body)
                      })
                      ::(asml_list a)
    | _ -> [create_main c]

(** Create a toplevel containing asml_list to c
    @param c Fclosure.t
    @return toplevel*)
let asml_head c = Fundefs (asml_list c)

(** Creates a string from a simple expressions of type Bsyntax.t recursively and applies asmt_to_string to asmts
    @param exp is a Bsyntax.t
    @return a string*)
let rec expression_to_string exp = match exp with
    | Nop -> "nop"
    | Neg id -> "-"^(Id.to_string id)
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
    | If (id, t, a, b, s) -> sprintf "if %s %s %s\n\tthen\n\t%s\n\telse\n\t%s"
        (Id.to_string id)
        (if s = "beq" then "=" else "<=")
        (expression_to_string t)
        (asmt_to_string a)
        (asmt_to_string b)
    | _ -> "\n[[ match not found in asml gen ]]\n"

(** Creates a string from an asmt of type Bsyntax.asmt recursively and applies expression_to_string to simple expressions of types Bsyntax.t
    @param a is an asmt
    @return a string*)
and asmt_to_string (a:Bsyntax.asmt) = match a with
    | Let (id, e1, e2) -> sprintf "let %s = %s in\n\t%s"
        (Id.to_string id)
        (expression_to_string e1)
        (asmt_to_string e2)
    (* | LetCls (id, e1, e2) ->  sprintf "let %s = %s in\n\t%s"
        (Id.to_string id)
        (expression_to_string e1)
        (asmt_to_string e2) *)
    | Expression t -> expression_to_string t

(** Creates a string from a fundef and applies asmt_to_string to asmts
    @param fund is a fundef
    @return a string*)
let fundef_to_string (fund:fundef) =
    sprintf "let %s %s =\n\t%s\n"
        (Id.to_string fund.name)
        (infix_to_string (fun x -> (Id.to_string x)) fund.args " ")
        (asmt_to_string fund.body)

(** Creates a string from a fundef list
    @param l is a fundef list
    @return a string*)
let rec list_to_string l = match l with
    | t::q -> sprintf "%s\n\n%s" (fundef_to_string t) (list_to_string q)
    | [] -> ""

(** Matches a toplevel to return the string of a fundef list
    @param toplvl is a toplevel
    @return a string*)
let toplevel_to_string (toplvl:toplevel) = match toplvl with
    | Fundefs l -> list_to_string l
