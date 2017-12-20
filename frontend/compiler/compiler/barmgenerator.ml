open Bsyntax;;
open Printf;;

let register_nb = 12
let vartbl_r = Hashtbl.create register_nb
let register_i = ref 3

let registVar x =
	if (not (Hashtbl.mem vartbl_r x)) then
		register_i := !register_i + 1;
		Hashtbl.add vartbl_r x (true, !register_i)
(* WIP ARM generation *)

let to_register variable_name =
    registVar variable_name;
    let is_in_register, register_id = Hashtbl.find vartbl_r variable_name in
    if is_in_register then
        sprintf "R%i" register_id
    else
        failwith "Error while allocating a register"

let rec movegen l i =
    match l with
        | [] -> sprintf ""
        | t::q -> sprintf "MOV r%s, %s\n%s" (string_of_int i) (to_register t) (movegen q (i + 1))

let recto_arm_formal_args args =

    (* if len(args) <= 4:
        * for i in range(len(args)):
            * print mov rito_string(args[i])*)
    match args with
    | [] -> sprintf ""
    | l when (List.length l <= 4) -> movegen l 0
    | t::q -> sprintf "%s %s" t (to_string_args q)

(* Useless ? *)
let rec ident_or_imm_expressionto_arm ident_or_imm =
    match ident_or_imm with
    | Int i -> sprintf "#%s" (string_of_int i)
    | Var id -> sprintf "%s" (to_register id)
    | _ -> failwith "Not a valid identifiant or immediate"

(* OK *)
let rec exp_to_arm exp dest =
    match exp with
    | Int i -> sprintf "\tMOV %s, #%s\n" (to_register dest) (string_of_int i)
    | Var id -> sprintf "\tMOV %s, %s\n" (to_register dest) (to_register id)
    | Add (e1, e2) -> sprintf "\tADD %s, %s, %s\n" (to_register dest) (to_register e1) (ident_or_imm_expressionto_arm e2)
    | Sub (e1, e2) -> sprintf "\tSUB %s, %s, %s\n" (to_register dest) (to_register e1) (ident_or_imm_expressionto_arm e2)
    (*| Call (l1, a1) -> let l = (to_string l1) in sprintf "%sBL %s" (to_arm_formal_args a1) (String.sub l 1 ((String.length l) - 1))*)
    | Nop -> sprintf "\tNOP"

(* OK *)
let rec asmt_to_arm asm =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP ...Imm" *)
    | Let (id, e, a) -> sprintf "%s %s" (exp_to_arm e id) (asmt_to_arm a)
    | Expression e -> sprintf "%s" (exp_to_arm e "")

(* OK *)
let rec fundef_to_arm fundef =
    match fundef with
    | Body b -> asmt_to_arm b

(* OK *)
let rec toplevel_to_arm toplevel =
    match toplevel with
    | Fundefs f -> sprintf ".text\n.global _start\n_start:\n%s\n\tBL min_caml_exit\n" (fundef_to_arm (List.hd f))
