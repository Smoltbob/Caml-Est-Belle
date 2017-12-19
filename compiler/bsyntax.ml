open Printf;;
open List;;

type t =
  | Int of int
  | Float of float
  | Neg of Bid.t
  | Fneg of Bid.t
  | Fsub of Bid.t * Bid.t
  | Fadd of Bid.t * Bid.t
  | Fmul of Bid.t * Bid.t
  | Fdiv of Bid.t * Bid.t
  | Add of Bid.t * t
  | Sub of Bid.t * t
  | Var of Bid.t
  | Eq of Bid.t * t
  | Call of Bid.t * formal_args
  | Nop

and formal_args = Bid.t list

and asmt =
    | Let of Bid.t * t * asmt
    | Expression of t
    (* | Additional case for parenthesis ? Don't think so ? *)

and fundef =
    | Body of asmt (* We will need the name, arguments and return type for functions *)

type toplevel =
    | Fundefs of (fundef list) (* Once we implement functions we will have a list *)


let rec to_string_args argu =
    match argu with
    | [] -> ""
    | [x] -> Bid.to_string x
    | t::q -> sprintf "%s %s" t (to_string_args q) 

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string =
    match l with
    | [] -> ""
    | [x] -> to_s x
    | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

let rec to_string exp =
    match exp with
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Neg id -> sprintf "(neg %s)" (Bid.to_string id)
  | Fneg id -> sprintf "(fneg %s)" (Bid.to_string id)
  | Fadd (id1, id2) -> sprintf "(fadd %s %s)" (Bid.to_string id1) (Bid.to_string id2)
  | Fsub (id1, id2) -> sprintf "(fsub %s %s)" (Bid.to_string id1) (Bid.to_string id2)
  | Fmul (id1, id2) -> sprintf "(fmul %s %s)" (Bid.to_string id1) (Bid.to_string id2)
  | Fdiv (id1, id2) -> sprintf "(fdiv %s %s)" (Bid.to_string id1) (Bid.to_string id2)
  | Add (e1, e2) -> sprintf "(add %s %s)" (Bid.to_string e1) (to_string e2)
  | Sub (e1, e2) -> sprintf "(sub %s %s)" (Bid.to_string e1) (to_string e2)
  | Var id -> Bid.to_string id
  | Eq (e1, e2) -> sprintf "(%s = %s)" (Bid.to_string e1) (to_string e2)
  | Call (l1, a1) -> sprintf "(call %s %s)" (Bid.to_string l1) (to_string_args a1)
  | Nop -> sprintf "nop"

let rec to_string_asm asm =
    match asm with
 | Let (id, e1, a) -> sprintf "(Let %s = %s in %s)" (Bid.to_string id) (to_string e1) (to_string_asm a)
 | Expression e -> sprintf "(%s)" (to_string e)

let rec to_string_fundef fund =
    match fund with
 | Body b -> sprintf "(%s)" (to_string_asm b)

let rec to_string_top top =
    match top with
  | Fundefs f -> sprintf "(%s)" (to_string_fundef (hd f))

let rec print_list_idx l i =
   match i with
    | i when i = 0 -> sprintf "%s" (Bid.to_string (hd l))
    | _ -> print_list_idx (tl l) (i - 1) 

(* Bellow : WIP ARM generation *)
(* Put this in a new file ? *)
(* Handle return values ? *)

let rec movegen l i =
    match l with
        | [] -> sprintf ""
        | t::q -> sprintf "MOV r%s, %s\n%s" (string_of_int i) (Bid.to_register t) (movegen q (i + 1))

let rec to_arm_formal_args args =
    
    (* if len(args) <= 4:
        * for i in range(len(args)):
            * print mov ri to_string(args[i])*)
    match args with
    | [] -> sprintf ""
    | l when (List.length l <= 4) -> movegen l 0
    | t::q -> sprintf "%s %s" t (to_string_args q) 

let rec to_arm exp =
    match exp with
  | Int i -> sprintf "#%s" (string_of_int i)
  | Add (e1, e2) -> sprintf "ADD %s, %s" (Bid.to_register e1) (to_arm e2)
  | Sub (e1, e2) -> sprintf "SUB %s, %s" (Bid.to_register e1) (to_arm e2)
  | Var id -> sprintf "%s" (Bid.to_register id)
  (* We want to remove the underscore in front of the function label.
   * To to so we print the sub string starting at index 1 and finishing at the label length *)
  | Call (l1, a1) -> let l = (Bid.to_string l1) in sprintf "%sBL %s" (to_arm_formal_args a1) (String.sub l 1 ((String.length l) - 1))
  | Nop -> sprintf "nop"


let rec to_arm_asm asm =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP Bid Bid Bid/Imm" *)
    | Let (id, e, a) -> (match e with
                            | Add (e1, e2) -> sprintf "ADD %s, %s, %s\n%s" (Bid.to_register id) (Bid.to_register e1) (to_arm e2) (to_arm_asm a)
                            | Sub (e1, e2) -> sprintf "SUB %s, %s, %s\n%s" (Bid.to_register id) (Bid.to_register e1) (to_arm e2) (to_arm_asm a)
                            | Int i -> sprintf "MOV %s, #%s\n%s" (Bid.to_register id) (string_of_int i) (to_arm_asm a) (* Good traduction ? *)
                            | Var id2 -> sprintf "MOV %s, #%s\n%s" (Bid.to_register id) (Bid.to_register id2) (to_arm_asm a)
    )
    | Expression e -> sprintf "%s" (to_arm e)

let rec to_arm_fundef fund =
    match fund with
    | Body b -> to_arm_asm b

let rec to_arm_top top =
    print_string ".text\n.global _start\n";
    print_string "_start:\n";
    match top with
    | Fundefs f -> sprintf "%s %s" (to_arm_fundef (hd f)) "\nBL min_caml_exit\n"