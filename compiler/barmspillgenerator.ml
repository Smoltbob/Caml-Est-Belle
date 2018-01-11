(** This file is to generate arm code from Bsyntax.toplevel stutructure by spill eveyrthing variables allocation method*)

open Bsyntax;;
open Printf;;
open List;;

(** A hashtable: the keys are the name of variables and the contant of each key is a tuple (bool, int), if bool is equal to "true", then the variable is in the register and the int is the index of register, else the variable is in the memory, the int is the address . *)
let vartbl_s = Hashtbl.create 10

(** address in virtual memory stack refer to current fp*)
let frame_index = ref 0

(* WIP ARM generation *)
(** This function is to allocate 4 bytes for variable x and update the vartbl_s, and return the address
@param variable_name the variable name in type id.t
@return the relative address of x in type int *)
let rec frame_position variable_name =
	if (not (Hashtbl.mem vartbl_s variable_name)) then
		    (frame_index := !frame_index - 4;
            Hashtbl.add vartbl_s variable_name !frame_index;
            (!frame_index, true))
    else
    (Hashtbl.find vartbl_s variable_name, false)

let rec register_args args =
    match args with
    | [] -> ()
    | arg::arg_list -> if (not (Hashtbl.mem vartbl_s arg)) then
		                    (frame_index := !frame_index - 4;
                            Hashtbl.add vartbl_s arg !frame_index);
                       register_args arg_list

let rec reset_frame_table () =
    Hashtbl.reset vartbl_s;
    frame_index := 0

let genif =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    sprintf "%d" !counter

(** This function is to call function movegen when the arguments are less than 4, to return empty string when there's no argument, to put arguments into stack when there're more than 4 arguments(TO BE DONE)
@param args the list of arguments, in type string
@return unit *)
let rec to_arm_formal_args args i =
    match args with
    | [] -> sprintf ""
    | l when (List.length l <= 4) -> sprintf "\tldr r%i, [fp, #%i]\n%s" i (fst (frame_position (List.hd l))) (to_arm_formal_args (List.tl l) (i-1))
    | _ -> failwith "Not handled yet"

(* Helpers for 3-addresses operation (like add or sub) *)
let rec store_in_stack register_id dest =
    let frame_offset, need_push = frame_position dest in
    let push_stack = if need_push then "\tadd sp, sp, #-4\n" else "" in
    sprintf "%s\tstr r%i, [fp, #%i]\n" push_stack register_id frame_offset

let rec operation_to_arm op e1 e2 dest =
    let store_string = store_in_stack 6 dest in
    sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n\t%s r6, r4, r5\n%s" (fst (frame_position e1)) (fst (frame_position e2)) op store_string

(** This function is to convert assignments into arm code 
@param exp expression in the assigment
@param dest the variable which is assigned in this assignment
@return unit*)
let rec exp_to_arm exp dest =
    match exp with
    | Int i -> let store_string = store_in_stack 4 dest in sprintf "\tmov r4, #%s\n%s" (string_of_int i) store_string
    | Var id -> let store_string = store_in_stack 4 dest in sprintf "\tldr r4, [fp, #%i]\n%s" (fst (frame_position id)) store_string
    | Add (e1, e2) -> operation_to_arm "add" e1 e2 dest
    | Sub (e1, e2) -> operation_to_arm "sub" e1 e2 dest
    | Call (l1, a1) -> let l = (Id.to_string l1) in sprintf "%s\tbl %s\n" (to_arm_formal_args a1 0) (String.sub l 1 ((String.length l) - 1))

    | If (id1, e1, asmt1, asmt2, comp) ->
            let counter = genif() in 
                let set_registers = sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n" (fst (frame_position id1)) (fst (frame_position e1)) in 
                let code = sprintf "\tcmp r4, r5\n\t%s if%s\n %s\tb end%s\n\nif%s:\n%s\nend%s:\n" comp counter (asmt_to_arm asmt2 dest) counter counter (asmt_to_arm asmt1 dest) counter in
                sprintf "%s%s" set_registers code
    | Nop -> sprintf "\tnop\n"
    | _ -> failwith "Error while generating ARM from ASML"

(** This function is a recursive function to convert type asmt into assignments
@param asm program in type asmt
@return unit*)
(* OK *)
and asmt_to_arm asm dest =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP ...Imm" *)
    | Let (id, e, a) -> let exp_string = exp_to_arm e id in sprintf "%s%s" exp_string (asmt_to_arm a "")
    | Expression e -> sprintf "%s" (exp_to_arm e dest)

(* Helper functions for fundef *)
let rec get_args args =
    register_args args;
    match args with
    | [] -> sprintf ""
    | l when (List.length l = 1) -> sprintf "\tstmfd sp!, {r0}\n\n"
    | l when (List.length l <= 4) -> sprintf "\tstmfd sp!, {r0-r%i}\n\n" ((List.length l)-1)
    | _ -> failwith "Not handled yet"

(** This function is a recursive function to conver tpye fundef into type asmt
@param fundef program in type fundef
@return unit*)
(* OK *)
let rec fundef_to_arm fundef =
    (* Write down the label *)
    reset_frame_table ();
    let get_args_string = get_args fundef.args in
    sprintf "\t.globl %s\n%s:\n\tstmfd sp!, {fp, lr}\n\tmov fp, sp\n\n%s%s\n\tmov sp, fp\n\tldmfd sp!, {fp, pc}\n\n\n" fundef.name fundef.name get_args_string (asmt_to_arm fundef.body "")

let rec fundefs_to_arm fundefs =
    match fundefs with
    | [start] -> reset_frame_table (); sprintf "\t.globl _start\n_start:\n\tmov fp, sp\n\n%s\n\tbl min_caml_exit\n" (asmt_to_arm start.body "") 
    | h::l -> sprintf "%s%s" (fundef_to_arm h) (fundefs_to_arm l)
    | _ -> failwith "No main function found"

(** This function is a recursive function to conver tpye toplevel into type fundef
@param toplevel program in type toplevel
@return unit*)
(* OK *)
let rec toplevel_to_arm toplevel =
    match toplevel with
    | Fundefs functions_list -> sprintf "\t.text\n%s" (fundefs_to_arm functions_list)
