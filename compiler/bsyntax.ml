open Printf;;
open List;;

(** This module defines the type of the AST as well as functions
  to display it.*)
type t =
    | Int of int
    | Float of float
    | Neg of Id.t
    | Fneg of Id.t
    | Add of Id.t * t
    | Sub of Id.t * t
    | Land of Id.t * t
    | Var of Id.t
    | Eq of Id.t * t
    | Call of Id.t * formal_args
    | CallC of Id.t * formal_args
    | If of Id.t * t * asmt * asmt * string
    | MemAcc of Id.t * t
    | MemAff of Id.t * t * Id.t
    | New of t
    | Nop

and formal_args = Id.t list

and asmt =
    | LetCls of Id.t * t * asmt
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

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string =
    match l with
    | [] -> ""
    | [x] -> to_s x
    | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

(** Transforms comparison instructions into strings. Ex : "beq" into "=" *)
let rec comp_to_string comp =
    match comp with
    | "beq" -> sprintf "="
    | "ble" -> sprintf "<="
    | "bge" -> sprintf ">="
    | _ -> failwith "Empty comparator"
(** Prints expressions occuring in the program.
    @param exp The expression to print. *)
let rec exp_to_string exp =
    match exp with
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Neg id -> sprintf "(neg %s)" (Id.to_string id)
  | MemAcc (id1, id2) -> sprintf "(mem(%s + %s))" (Id.to_string id1) (exp_to_string id2)
  | MemAff (id1, id2, id3) -> sprintf "(mem(%s + %s)<-%s)" (Id.to_string id1) (exp_to_string id2) (Id.to_string id3)
  | Add (e1, e2) -> sprintf "(add %s %s)" (Id.to_string e1) (exp_to_string e2)
  | Sub (e1, e2) -> sprintf "(sub %s %s)" (Id.to_string e1) (exp_to_string e2)
  | Land (e1, e2) -> sprintf "(land %s %s)" (Id.to_string e1) (exp_to_string e2)
  | Var id -> Id.to_string id
  | Eq (e1, e2) -> sprintf "(%s = %s)" (Id.to_string e1) (exp_to_string e2)
  | If (id1, e1, asmt1, asmt2, comp) -> sprintf "(if %s %s %s then %s else %s)" (Id.to_string id1) (comp_to_string comp) (exp_to_string e1) (to_string_asm asmt1) (to_string_asm asmt2)
  | Call (l1, a1) -> sprintf "(call %s %s)" (Id.to_string l1) (to_string_args a1)
  | New (e1) -> sprintf "(new %s)" (exp_to_string e1)
  | Nop -> sprintf "nop"

(** Prints an asmt. It can be an assignement (with a let) or an expression alone.
    @param asm The asmt to print.
*)
and to_string_asm asm =
    match asm with
 | Let (id, e1, a) -> sprintf "(Let %s = %s in %s)" (Id.to_string id) (exp_to_string e1) (to_string_asm a)
 | Expression e -> sprintf "(%s)" (exp_to_string e)

(** Prints the functions in the list of fundefs/
    @param fund the list of function definitions.
*)
let rec to_string_fundef fund =
     sprintf "(%s)" (to_string_asm fund.body)

(** Prints the root of the ast of an asml program. This is the function to call to print the whole tree.
    @param top The ast as provided by the parser.
    *)
let rec to_string_top top =
    match top with
  | Fundefs f -> sprintf "(%s)" (to_string_fundef (hd f))

let rec print_list_idx l i =
   match i with
    | i when i = 0 -> sprintf "%s" (Id.to_string (hd l))
    | _ -> print_list_idx (tl l) (i - 1)
