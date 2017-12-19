open Bsyntax;;
open Printf;;

(* WIP ARM generation *)
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
    
(* Useless ? *)
let rec ident_or_imm_expression_to_arm ident_or_imm =
    match ident_or_imm with
    | Int i -> sprintf "#%s" (string_of_int i)
    | Var id -> sprintf "%s" (Bid.to_register id)
    | _ -> failwith "Not a valid identifiant or immediate"

(* OK *)
let rec exp_to_arm exp dest =
    match exp with
    | Int i -> sprintf "MOV %s, #%s\n" (Bid.to_register dest) (string_of_int i)
    | Var id -> sprintf "MOV %s, %s\n" (Bid.to_register dest) (Bid.to_register id)
    | Add (e1, e2) -> sprintf "ADD %s, %s, %s\n" (Bid.to_register dest) (Bid.to_register e1) (ident_or_imm_expression_to_arm e2)
    | Sub (e1, e2) -> sprintf "SUB %s, %s, %s\n" (Bid.to_register dest) (Bid.to_register e1) (ident_or_imm_expression_to_arm e2)
    | Call (l1, a1) -> let l = (Bid.to_string l1) in sprintf "%sBL %s" (to_arm_formal_args a1) (String.sub l 1 ((String.length l) - 1))
    | Nop -> sprintf "NOP"

(* OK *)
let rec asmt_to_arm asm =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP Bid Bid Bid/Imm" *)
    | Let (id, e, a) -> sprintf "%s %s" (exp_to_arm e id) (asmt_to_arm a)
    | Expression e -> sprintf "%s" (exp_to_arm e "")

(* OK *)
let rec fundef_to_arm fundef =
    match fundef with
    | Body b -> asmt_to_arm b

(* OK *)
let rec toplevel_to_arm toplevel =
    match toplevel with
    | Fundefs f -> sprintf ".text\n.global _start\n_start:\n%s\nBL min_caml_exit\n" (fundef_to_arm (List.hd f))
