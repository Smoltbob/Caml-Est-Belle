open Printf;;
open List;;

(** This module defines the type of the AST as well as functions
  to display it.*)
type t =
  | Int of int
  | Float of float
  | Neg of Id.t
  | Fneg of Id.t
  | Fsub of Id.t * Id.t
  | Fadd of Id.t * Id.t
  | Fmul of Id.t * Id.t
  | Fdiv of Id.t * Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Var of Id.t
  | Eq of Id.t * t
  | If of Id.t * Id.t * asmt * asmt
  | Call of Id.t * formal_args
  | Nop

and formal_args = Id.t list

and asmt =
    | Let of Id.t * t * asmt
    | Expression of t
    (* | Additional case for parenthesis ? Don't think so ? *)

and fundef = {
                name : Id.t;
                args : Id.t list;
                body : asmt (* We will need the name, arguments and return type for functions *)
                (* ret : Type.t *)
             }

type toplevel =
    | Fundefs of (fundef list) (* Once we implement functions we will have a list *)


(** Prints the functions arguments. They are stored in a list.
   @param argu the list of arguments
*)
let rec to_string_args argu =
    match argu with
    | [] -> ""
    | [x] -> Id.to_string x
    | t::q -> sprintf "%s %s" t (to_string_args q)

(* Useless? *)
let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string =
    match l with
    | [] -> ""
    | [x] -> to_s x
    | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

(** Prints expressions occuring in the program.
    @param exp The expression to print. *)
let rec exp_to_string exp =
    match exp with
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Neg id -> sprintf "(neg %s)" (Id.to_string id)
  | Fneg id -> sprintf "(fneg %s)" (Id.to_string id)
  | Fadd (id1, id2) -> sprintf "(fadd %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Fsub (id1, id2) -> sprintf "(fsub %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Fmul (id1, id2) -> sprintf "(fmul %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Fdiv (id1, id2) -> sprintf "(fdiv %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Add (e1, e2) -> sprintf "(add %s %s)" (Id.to_string e1) (Id.to_string e2)
  | Sub (e1, e2) -> sprintf "(sub %s %s)" (Id.to_string e1) (Id.to_string e2)
  | Var id -> Id.to_string id
  | Eq (e1, e2) -> sprintf "(%s = %s)" (Id.to_string e1) (exp_to_string e2)
  | If (id1, e1, asmt1, asmt2) -> sprintf "(if %s = %s then %s else %s)" (Id.to_string id1) (Id.to_string e1) (to_string_asm asmt1) (to_string_asm asmt2)
  | Call (l1, a1) -> sprintf "(call %s %s)" (Id.to_string l1) (to_string_args a1)
  | Nop -> sprintf "nop"

(*(** Prints an asmt. It can be an assignement (with a let) or an expression alone.
    @param asm The asmt to print.
*)
and to_string_asm asm =
    match asm with
 | Let (id, e1, a) -> sprintf "(Let %s = %s in %s)" (Id.to_string id) (exp_to_string e1) (to_string_asm a)
 | Expression e -> sprintf "(%s)" (exp_to_string e)

(*(** Prints the functions in the list of fundefs/
    @param fund the list of function definitions.
*)
let rec to_string_fundef fund =
    match fund with
 | Body b -> sprintf "(%s)" (to_string_asm b) *)

(*(** Prints the root of the ast of an asml program. This is the function to call to print the whole tree.
    @param top The ast as provided by the parser.
    *)
let rec to_string_top top =
    match top with
  | Fundefs f -> sprintf "(%s)" (to_string_fundef (hd f))
*)

let rec print_list_idx l i =
   match i with
    | i when i = 0 -> sprintf "%s" (Id.to_string (hd l))
    | _ -> print_list_idx (tl l) (i - 1) *)
