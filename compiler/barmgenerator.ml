(** This file is to generate arm code from Bsyntax.toplevel stutructure by very basic method of variables registation*)

open Bsyntax;; 
open Printf;;


(** the number of free registers *)
let register_nb = 12

(** A hashtable: the keys are the name of variables and the contant of each key is a tuple (bool, int), if bool is equal to "true", then the variable is in the register and the int is the index of register, else the variable is in the memory, the int is the address . *)
let vartbl_r = Hashtbl.create register_nb

(** register counter *)
let register_i = ref 3

(** This function is to load a register for variable x and update the vartbl_r
@param x the variable name in type id.t
@return unit *)
let registVar x =
	if (not (Hashtbl.mem vartbl_r x)) then
		register_i := !register_i + 1;
		Hashtbl.add vartbl_r x (true, !register_i)
(* WIP ARM generation *)

(** This function is to get the register associated with variable_name according to the index of register in vartbl_r,
@param variable_name the variable name in type id.t
@return string "Ri" *)
let to_register variable_name =
    registVar variable_name;
    let is_in_register, register_id = Hashtbl.find vartbl_r variable_name in
    if is_in_register then
        sprintf "R%i" register_id
    else
        failwith "Error while allocating a register"

(** This function is to return the register for arguments in function call when the arguments are less than 4;
@param l the list of args, in type string;
@param i the conuter of arguments, int;
@return string "mov ri arg" *)
let rec movegen l i =
    match l with
        | [] -> sprintf ""
        | t::q -> sprintf "mov r%s, %s\n%s" (string_of_int i) (to_register t) (movegen q (i + 1))

(** This function is to call function movegen when the arguments are less than 4, to return empty string when there's no argument, to put arguments into stack when there're more than 4 arguments(TO BE DONE)
@param args the list of arguments, in type string
@return unit *)
let rec to_arm_formal_args args =
    (* if len(args) <= 4:
        * for i in range(len(args)):
            * print mov ri to_string(args[i])*)
    match args with
    | [] -> sprintf ""
    | l when (List.length l <= 4) -> movegen l 0
    | t::q -> sprintf "%s %s" t (to_string_args q) 

(**/**)
(* Useless ? *)
let rec ident_or_imm_expression_to_arm ident_or_imm =
    match ident_or_imm with
    | Int i -> sprintf "#%s" (string_of_int i)
    | Var id -> sprintf "%s" (to_register id)
    | _ -> failwith "Not a valid identifiant or immediate"
(**/**)

(** This function is to convert assignments into arm code 
@param exp expression in the assigment
@param dest the variable which is assigned in this assignment
@return unit*)
(* OK *)
let rec exp_to_arm exp dest =
    match exp with
    | Int i -> sprintf "mov %s, #%s\n" (to_register dest) (string_of_int i)
    | Var id -> sprintf "mov %s, %s\n" (to_register dest) (to_register id)
    | Add (e1, e2) -> (match dest with 
                    | "" -> sprintf "add r0, %s, %s\n" (to_register e1) (to_register e2)
                    | _ -> sprintf "add %s, %s, %s\n" (to_register dest) (to_register e1) (to_register e2))
    | Sub (e1, e2) -> (match dest with
                    | "" -> sprintf "add r0, %s, %s\n" (to_register e1) (to_register e2)
                    | _ -> sprintf "add %s, %s, %s\n" (to_register dest) (to_register e1) (to_register e2))
    | Call (l1, a1) -> (match dest with
                    | "" ->let l = (Id.to_string l1) in sprintf "%sBL %s\n" (to_arm_formal_args a1) (String.sub l 1 ((String.length l) - 1))
                    | _ ->let l = (Id.to_string l1) in sprintf "%sBL %s\nmov %s, r0\n" (to_arm_formal_args a1) (String.sub l 1 ((String.length l) - 1)) (to_register dest))
    | Nop -> sprintf "nop\n"
	| _ -> failwith "matchfailure in barmgenerator"
(** This function is a recursive function to convert type asmt into assignments
@param asm program in type asmt
@return unit*)
(* OK *)
let rec asmt_to_arm asm =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP ...Imm" *)
    | Let (id, e, a) -> sprintf "%s %s" (exp_to_arm e id) (asmt_to_arm a)
    | Expression e -> sprintf "%s" (exp_to_arm e "")

(** This function is a recursive function to conver tpye fundef into type asmt
@param fundef program in type fundef
@return unit*)
(* OK *)
let rec fundef_to_arm fundef =
    match fundef with
    | Body b -> asmt_to_arm b

(** This function is a recursive function to conver tpye toplevel into type fundef
@param toplevel program in type toplevel
@return unit*)
(* OK *)
let rec toplevel_to_arm toplevel =
    match toplevel with
    | Fundefs f -> sprintf ".text\n.global _start\n_start:\n%sBL min_caml_exit" (fundef_to_arm (List.hd f))
