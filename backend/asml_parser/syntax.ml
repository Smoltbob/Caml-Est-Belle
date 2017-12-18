open Printf
open List

type t = 
  | Int of int
  | Float of float
  | Neg of Id.t
  | Fneg of Id.t
  | Fsub of Id.t * Id.t
  | Fadd of Id.t * Id.t
  | Fmul of Id.t * Id.t
  | Fdiv of Id.t * Id.t
  | Add of Id.t * t
  | Sub of Id.t * t
  | Var of Id.t
  | Eq of Id.t * t
  | Call of Id.t * formal_args
  | Nop
  (* NOP ? *)
  (* Too allowing: should be ADD of Id.t * imm (that can be float or int) *)
and formal_args = Id.t list

and asmt = 
    | Let of Id.t * t * asmt
    | Expression of t
    (* | Additional case for parenthesis ? Don't think so ? *)

and fundef = 
    | Body of asmt (* We will need the name, arguments and return type for functions *)

type toplevel =
    | Fundefs of (fundef list) 



let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string = 
    match l with 
    | [] -> ""
    | [x] -> to_s x
    | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

let rec to_string_args argu =
    match argu with
    | [] -> sprintf ""
    | t::q -> sprintf "(%s %s)" t (to_string_args q) 
    
let rec to_string exp =
    match exp with
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Neg id -> sprintf "(neg %s)" (Id.to_string id)
  | Fneg id -> sprintf "(fneg %s)" (Id.to_string id)
  | Fadd (id1, id2) -> sprintf "(fadd %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Fsub (id1, id2) -> sprintf "(fsub %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Fmul (id1, id2) -> sprintf "(fmul %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Fdiv (id1, id2) -> sprintf "(fdiv %s %s)" (Id.to_string id1) (Id.to_string id2)
  | Add (e1, e2) -> sprintf "(add %s %s)" (Id.to_string e1) (to_string e2)
  | Sub (e1, e2) -> sprintf "(sub %s %s)" (Id.to_string e1) (to_string e2) 
  | Var id -> Id.to_string id 
  | Eq (e1, e2) -> sprintf "(%s = %s)" (Id.to_string e1) (to_string e2) 
  | Call (l1, a1) -> sprintf "(call %s %s)" (Id.to_string l1) (to_string_args a1)
  | Nop -> sprintf "nop"

let rec to_string_asm asm =
    match asm with
 | Let (id, e1, a) -> sprintf "(Let %s = %s in %s)" (Id.to_string id) (to_string e1) (to_string_asm a)
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
let rec to_arm exp =
    match exp with
  | Int i -> sprintf "#%s" (string_of_int i)
  | Float f -> sprintf "%.2f" f  (* Not implem *)
  | Neg id -> sprintf "(neg %s)" (Id.to_register id) (* Not implem *)
  | Fneg id -> sprintf "(fneg %s)" (Id.to_register id)(* Not implem *)
  | Fadd (id1, id2) -> sprintf "(fadd %s %s)" (Id.to_register id1) (Id.to_register id2)(* Not implem *)
  | Fsub (id1, id2) -> sprintf "(fsub %s %s)" (Id.to_register id1) (Id.to_register id2)(* Not implem *)
  | Fmul (id1, id2) -> sprintf "(fmul %s %s)" (Id.to_register id1) (Id.to_register id2)(* Not implem *)
  | Fdiv (id1, id2) -> sprintf "(fdiv %s %s)" (Id.to_register id1) (Id.to_register id2)(* Not implem *)
  | Add (e1, e2) -> sprintf "ADD %s, %s" (Id.to_register e1) (to_arm e2)
  | Sub (e1, e2) -> sprintf "SUB %s, %s" (Id.to_register e1) (to_arm e2)
  | Var id -> sprintf "%s" (Id.to_register id)
  | Eq (e1, e2) -> sprintf "(%s = %s)" (Id.to_register e1) (to_arm e2) 
  | Call (l1, a1) -> sprintf ("TODO")
  | Nop -> sprintf "nop"

let rec to_arm_formal_args args =
    match args with
    | [] -> sprintf ""
    | t::q -> sprintf "(%s %s)" t (to_string_args q) 

let rec to_arm_asm asm =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP Id Id Id/Imm" *)
    | Let (id, e, a) -> (match e with 
                            | Add (e1, e2) -> sprintf "ADD %s, %s, %s\n%s" (Id.to_register id) (Id.to_register e1) (to_arm e2) (to_arm_asm a)
                            | Sub (e1, e2) -> sprintf "SUB %s, %s, %s\n%s" (Id.to_register id) (Id.to_register e1) (to_arm e2) (to_arm_asm a)
                            | Int i -> sprintf "ADD %s, %s, #0\n%s" (Id.to_register id) (string_of_int i) (to_arm_asm a) (* Good traduction ? *)
                            | Var id2 -> sprintf "ADD %s, %s, #0\n%s" (Id.to_register id) (Id.to_register id2) (to_arm_asm a)
    )
    | Expression e -> sprintf "%s" (to_arm e)

let rec to_arm_fundef fund =
    match fund with
    | Body b -> to_arm_asm b

let rec to_arm_top top =
    print_string ".text\n.global _start\n";
    print_string "_start:\n";
    match top with
    | Fundefs f -> 
            (* match f with t::q in  *)
            sprintf "%s %s" (to_arm_fundef (hd f)) "\nBL min_caml_exit"
