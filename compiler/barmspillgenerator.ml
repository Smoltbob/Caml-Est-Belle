(** This file is to generate arm code from Bsyntax.toplevel stutructure by spill eveyrthing variables allocation method*)

open Bsyntax;;
open Printf;;
open List;;

(** the number of free registers *)
let register_nb = 12

(** A hashtable: the keys are the name of variables and the contant of each key is a tuple (bool, int), if bool is equal to "true", then the variable is in the register and the int is the index of register, else the variable is in the memory, the int is the address . *)
let vartbl_s = Hashtbl.create register_nb

(** address in virtual memory stack refer to current fp*)
let frame_index = ref 0

(* WIP ARM generation *)
(** This function is to allocate 4 bits for variable x and update the vartbl_s, and return the address
@param variable_name the variable name in type id.t
@return the relative address of x in type int *)
let frame_position variable_name =
	if (not (Hashtbl.mem vartbl_s variable_name)) then
        begin
		    frame_index := !frame_index + 4;
            Hashtbl.add vartbl_s variable_name !frame_index
        end;
    Hashtbl.find vartbl_s variable_name

let genif =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    sprintf "%d" !counter

(** This function is to return the address for arguments in function call when the arguments are less than 4;
@parustaam l the list of args, in type string;
@param i the conuter of arguments, int;
@return string "ldr ri, [fp,i]" *)
let rec ldrgen l i =
    match l with
        | [] -> sprintf ""
        | t::q -> sprintf "\tldr r%i, [fp, #%i]\n%s" i (frame_position t) (ldrgen q (i + 1))
        
(** This function is to call function movegen when the arguments are less than 4, to return empty string when there's no argument, to put arguments into stack when there're more than 4 arguments(TO BE DONE)
@param args the list of arguments, in type string
@return unit *)
let rec to_arm_formal_args args =
    (* if len(args) <= 4:
        * for i in range(len(args)):
            * print mov rito_string(args[i])*)
    match args with
    | [] -> sprintf ""
    | l when (List.length l <= 4) -> ldrgen l 0
    (*| t::q -> sprintf "%s %s" (Id.to_string t) (to_arm_formal_args q *) 

(** This function is to convert assignments into arm code 
@param exp expression in the assigment
@param dest the variable which is assigned in this assignment
@return unit*)
let rec exp_to_arm exp dest =
    match exp with
    | Int i -> sprintf "\tmov r4, #%s\n\tstr r4, [fp, #%i]\n" (string_of_int i) (frame_position dest)
    | Var id -> sprintf "\tldr r4, [fp, #%i]\n\tmov r5, r4\n\tstr r5, [fp, #%i]\n" (frame_position id) (frame_position dest)
    | Add (e1, e2)  -> sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n\tadd r6, r4, r5\n\tstr r6, [fp, #%i]\n\n" (frame_position e1) (frame_position e2) (frame_position dest)
    | Sub (e1, e2) -> sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n\tsub r6, r4, r5\n\tstr r6, [fp, #%i]\n\n" (frame_position e1) (frame_position e2) (frame_position dest)
    | Call (l1, a1) -> let l = (Id.to_string l1) in sprintf "%s\tbl %s\n" (to_arm_formal_args a1) (String.sub l 1 ((String.length l) - 1))
    | Ifeq (id1, e1, asmt1, asmt2) ->
            let counter = genif() in 
                let set_registers = sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n" (frame_position id1) (frame_position e1) in 
                let code = sprintf "\tcmp r4, r5\n\tbeq if%s\n %s\tb end%s\n\nif%s:\n%s\nend%s:\n" counter (asmt_to_arm asmt2) counter counter (asmt_to_arm asmt1) counter in
                sprintf "%s%s" set_registers code
    | Ifle (id1, e1, asmt1, asmt2) ->
            let counter = genif() in 
                let set_registers = sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n" (frame_position id1) (frame_position e1) in 
                let code = sprintf "\tcmp r4, r5\n\tble if%s\n %s\tb end%s\n\nif%s:\n%s\nend%s:\n" counter (asmt_to_arm asmt2) counter counter (asmt_to_arm asmt1) counter in
                sprintf "%s%s" set_registers code
    | Ifge (id1, e1, asmt1, asmt2) ->
            let counter = genif() in 
                let set_registers = sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n" (frame_position id1) (frame_position e1) in 
                let code = sprintf "\tcmp r4, r5\n\tbge if%s\n %s\tb end%s\n\nif%s:\n%s\nend%s:\n" counter (asmt_to_arm asmt2) counter counter (asmt_to_arm asmt1) counter in
                sprintf "%s%s" set_registers code
    | Nop -> sprintf "\tnop\n"
    | _ -> failwith "Error while generating ARM from ASML"

(** This function is a recursive function to convert type asmt into assignments
@param asm program in type asmt
@return unit*)
(* OK *)
and asmt_to_arm asm =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP ...Imm" *)
    | Let (id, e, a) -> sprintf "%s %s" (exp_to_arm e id) (asmt_to_arm a)
    | Expression e -> sprintf "%s" (exp_to_arm e "")

(* Helper functions for fundef *)
let rec movegen l i =
    match l with
        | [] -> sprintf ""
        | t::q -> sprintf "\tstr r%i, [fp, #%i]\n%s" i (frame_position t) (movegen q (i + 1))
        | _ -> failwith "Not handled movegen"

        
let rec prepare_arg args =
    match args with
    | [] -> sprintf ""
    | l when (List.length l <= 4) -> movegen l 0
    | _ -> failwith "Not handled yet"

(** This function is a recursive function to conver tpye fundef into type asmt
@param fundef program in type fundef
@return unit*)
(* OK *)
let rec fundef_to_arm fundef =
    match fundef with
    | Body b -> asmt_to_arm b
    | _ -> failwith "Matching error in fundef"

(** This function is a recursive function to conver tpye toplevel into type fundef
@param toplevel program in type toplevel
@return unit*)
(* OK *)
let rec toplevel_to_arm toplevel =
    match toplevel with
    | Fundefs f -> sprintf ".text\n.global _start\n_start:\n\tmov fp, sp\n%s\tbl min_caml_exit\n" (fundef_to_arm (List.hd f))
    | _ -> failwith "Matching error in toplevel"
