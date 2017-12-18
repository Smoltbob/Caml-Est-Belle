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
  (* NOP ? *)
  (* Too allowing: should be ADD of Bid.t * imm (that can be float or int) *)
and formal_args =
    | Arg of Bid.t
    | Args of Bid.t * formal_args

and asmt =
    | Let of Bid.t * t * asmt
    | Expression of t
    (* | Additional case for parenthesis ? Don't think so ? *)

and fundef =
    | Body of asmt (* We will need the name, arguments and return type for functions *)

type toplevel =
    | Fundefs of (fundef list) (* Once we implement functions we will have a list *)



let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string =
    match l with
    | [] -> ""
    | [x] -> to_s x
    | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

let rec to_string_args argu =
    match argu with
    | Arg a1 -> sprintf "(%s)" (Bid.to_string a1)
    | Args (a1, a2) -> sprintf "(%s %s)" (Bid.to_string a1) (to_string_args a2)

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

(* Bellow : WIP ARM generation *)
(* Put this in a new file ? *)
(* Handle return values ? *)
let rec exp_to_arm exp dest =
    match exp with
  | Int i -> sprintf "#%s" (string_of_int i)
  | Float f -> sprintf "%.2f" f  (* Not implem *)
  | Neg id -> sprintf "(neg %s)" (Bid.to_register id) (* Not implem *)
  | Fneg id -> sprintf "(fneg %s)" (Bid.to_register id)(* Not implem *)
  | Fadd (id1, id2) -> sprintf "(fadd %s %s)" (Bid.to_register id1) (Bid.to_register id2)(* Not implem *)
  | Fsub (id1, id2) -> sprintf "(fsub %s %s)" (Bid.to_register id1) (Bid.to_register id2)(* Not implem *)
  | Fmul (id1, id2) -> sprintf "(fmul %s %s)" (Bid.to_register id1) (Bid.to_register id2)(* Not implem *)
  | Fdiv (id1, id2) -> sprintf "(fdiv %s %s)" (Bid.to_register id1) (Bid.to_register id2)(* Not implem *)
  | Add (e1, e2) -> sprintf "ADD %s, %s, %s" (Bid.to_register dest) (Bid.to_register e1) (exp_to_arm e2 "")
  | Sub (e1, e2) -> sprintf "SUB %s, %s, %s" (Bid.to_register dest) (Bid.to_register e1) (exp_to_arm e2 "")
  | Var id -> sprintf "%s" (Bid.to_register id)
  | Eq (e1, e2) -> sprintf "(%s = %s)" (Bid.to_register e1) (exp_to_arm e2 "")
  | Call (l1, a1) -> sprintf ("TODO")
  | Nop -> sprintf "nop"

let rec to_arm_formal_args args =
    match args with
    | Arg a1 -> sprintf "%s" (Bid.to_string a1)
    | Args (a1, a2) -> sprintf "(%s %s)" (Bid.to_string a1) (to_string_args a2)

let rec to_arm_asm asm =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP Bid Bid Bid/Imm" *)
    | Let (id, e, a) -> sprintf "%s %s" (exp_to_arm e id) (to_arm_asm a)
    
    | Expression e -> sprintf "%s" (exp_to_arm e "")

let rec to_arm_fundef fund =
    match fund with
    | Body b -> to_arm_asm b

let rec to_arm_top top =
    print_string ".text\n.global _start\n";
    print_string "_start:\n";
    match top with
    | Fundefs f -> print_string (sprintf "%s %s" (to_arm_fundef (hd f)) "\nBL min_caml_exit\n")
