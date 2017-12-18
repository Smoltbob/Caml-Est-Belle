open Bsyntax;;
open Printf;;

(* WIP ARM generation *)
let rec ident_or_imm_expression_to_arm ident_or_imm =
    match ident_or_imm with
    | Int i -> sprintf "#%s" (string_of_int i)
    | Var id -> sprintf "%s" (Bid.to_register id)
    | _ -> failwith "Not a valid identifiant or immediate"

let rec exp_to_arm exp dest =
    match exp with
    | Int i -> sprintf "\tMOV %s, #%s\n" (Bid.to_register dest) (string_of_int i)
    | Var id -> sprintf "\tMOV %s, %s\n" (Bid.to_register dest) (Bid.to_register id)
    | Add (e1, e2) -> sprintf "\tADD %s, %s, %s\n" (Bid.to_register dest) (Bid.to_register e1) (ident_or_imm_expression_to_arm e2)
    | Sub (e1, e2) -> sprintf "\tSUB %s, %s, %s\n" (Bid.to_register dest) (Bid.to_register e1) (ident_or_imm_expression_to_arm e2)
    | Call (l1, a1) -> sprintf ("TODO")
    | Nop -> sprintf "\tNOP"

let rec asmt_to_arm asm =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP Bid Bid Bid/Imm" *)
    | Let (id, e, a) -> sprintf "%s %s" (exp_to_arm e id) (asmt_to_arm a)
    | Expression e -> sprintf "%s" (exp_to_arm e "")

let rec fundef_to_arm fundef =
    match fundef with
    | Body b -> asmt_to_arm b

let rec toplevel_to_arm toplevel =
    print_string ".text\n.global _start\n";
    print_string "_start:\n";
    match toplevel with
    | Fundefs f -> print_string (sprintf "%s %s" (fundef_to_arm (List.hd f))  "\n\tBL min_caml_exit\n")
