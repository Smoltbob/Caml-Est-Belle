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
(** This function is to allocate 4 bytes for variable x and update the vartbl_s, and return the address
@param variable_name the variable name in type id.t
@return the relative address of x in type int *)
let frame_position variable_name =
	if (not (Hashtbl.mem vartbl_s variable_name)) then
        begin
		    frame_index := !frame_index - 4;
            Hashtbl.add vartbl_s variable_name !frame_index
        end;
    Hashtbl.find vartbl_s variable_name
        
(** This function is to call function movegen when the arguments are less than 4, to return empty string when there's no argument, to put arguments into stack when there're more than 4 arguments(TO BE DONE)
@param args the list of arguments, in type string
@return unit *)
let rec to_arm_formal_args args =
    match args with
    | [] -> sprintf ""
    | l when (List.length l = 1) -> sprintf "\tpush {r0}\n"
    | l when (List.length l <= 4) -> sprintf "\tpush {r0-r%i}\n" ((List.length l)-1)
    | _ -> failwith "Not handled yet"

(** This function is to convert assignments into arm code 
@param exp expression in the assigment
@param dest the variable which is assigned in this assignment
@return unit*)
(* OK *)
let rec exp_to_arm exp dest =
    match exp with
    | Int i -> sprintf "\tmov r4, #%s\n\tstr r4, [fp, #%i]\n" (string_of_int i) (frame_position dest)
    | Var id -> sprintf "\tldr r4, [fp, #%i]\n\tmov r5, r4\n\tstr r5, [fp, #%i]\n" (frame_position id) (frame_position dest)
    | Add (e1, e2)  -> sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n\tadd r6, r4, r5\n\tstr r6, [fp, #%i]\n" (frame_position e1) (frame_position e2) (frame_position dest)
    | Sub (e1, e2) -> sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n\tsub r6, r4, r5\n\tstr r6, [fp, #%i]\n" (frame_position e1) (frame_position e2) (frame_position dest)
    | Call (l1, a1) -> let l = (Id.to_string l1) in sprintf "\tadd sp, sp, #%i\n%s\tbl %s\n" (!frame_index - 4) (to_arm_formal_args a1) (String.sub l 1 ((String.length l) - 1))
    | Nop -> sprintf "\tnop\n"
    | _ -> failwith "Error while generating ARM from ASML"

(** This function is a recursive function to convert type asmt into assignments
@param asm program in type asmt
@return unit*)
(* OK *)
let rec asmt_to_arm asm =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP ...Imm" *)
    | Let (id, e, a) -> sprintf "%s %s" (exp_to_arm e id) (asmt_to_arm a)
    | Expression e -> sprintf "%s" (exp_to_arm e "")

(* Helper functions for fundef *)
let rec get_args args =
    match args with
    | [] -> sprintf ""
    | l when (List.length l = 1) -> sprintf "\tldr r0, [sp]\n"
    | l when (List.length l <= 4) -> sprintf "\tldm sp, {r0-r%i}" ((List.length l)-1)
    | _ -> failwith "Not handled yet"

(** This function is a recursive function to conver tpye fundef into type asmt
@param fundef program in type fundef
@return unit*)
(* OK *)
let rec fundef_to_arm fundef =
    (* Write down the label *)
    sprintf ".global _%s\n_%s:\n\tmov fp, sp\n\tstmfd sp!, {lr}\n%s%s\tldmfd sp!, {lr}\n\tbx lr\n\n" fundef.name fundef.name (get_args fundef.args) (asmt_to_arm fundef.body)
    (* Prepare the arguments. If <= 4 arguments, put them in r0 -> r3.
     * Else put them in the stack *)
    (* TODO *)


let rec fundefs_to_arm fundefs =
    match fundefs with
    | [] -> sprintf ""
    | h::l -> sprintf "%s%s" (fundef_to_arm h) (fundefs_to_arm l)

(** This function is a recursive function to conver tpye toplevel into type fundef
@param toplevel program in type toplevel
@return unit*)
(* OK *)
let rec toplevel_to_arm toplevel =
    match toplevel with (* TODO match with h::l instead of doing tl and hd *)
    | Fundefs f -> sprintf ".text\n%s.global _start\n_start:\n\tmov fp, sp\n%s\tbl min_caml_exit\n" (fundefs_to_arm (List.tl f)) (fundef_to_arm (List.hd f))
