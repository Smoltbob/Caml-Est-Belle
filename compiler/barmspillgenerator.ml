(** This file is to generate arm code from Bsyntax.toplevel stutructure by spill everything variables allocation method*)

open Bsyntax;;
open Printf;;
open List;;

(** A hashtable: the keys are the name of variables and the contant of each key is a tuple (bool, int), if bool is equal to "true", then the variable is in the register and the int is the index of register, else the variable is in the memory, the int is the address . *)

let frames_stack = Stack.create ()

(** This function is to allocate 4 bytes for variable x and update the (Stack.top frames_table), and return the address
@param variable_name the variable name in type id.t
@return the relative address of x in type int *)
let rec frame_position variable_name =
    let top_frame_table = Stack.top frames_stack in
	if (not (Hashtbl.mem top_frame_table variable_name)) then
            (let frame_index = -4 * (Hashtbl.length top_frame_table) - 4 in
            Hashtbl.add top_frame_table variable_name frame_index;
            (frame_index, true))
    else
    (Hashtbl.find top_frame_table variable_name, false)

let rec register_args args =
    match args with
    | [] -> ()
    | arg::arg_list -> let top_frame_table = Stack.top frames_stack in if (not (Hashtbl.mem top_frame_table arg)) then
                            (let frame_index = -4 * (Hashtbl.length top_frame_table) - 4 in
                            Hashtbl.add top_frame_table arg frame_index);
                       register_args arg_list

let rec push_frame_table () =
    let (new_frame_table:(string, int) Hashtbl.t) = Hashtbl.create 10 in
    Stack.push new_frame_table frames_stack

let rec pop_frame_table () =
    let _ = Stack.pop frames_stack in ()

let genif =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    sprintf "%d" !counter

let remove_underscore function_name =
    String.sub function_name 1 ((String.length function_name) - 1)

let rec stack_remaining_arguments args =
    match args with
    | [] -> ""
    | arg::arg_list -> sprintf "%s\tldr r4, [fp, #%i]\n\tstmfd sp!, {r4}\n" (stack_remaining_arguments arg_list) (fst (frame_position arg))

(** This function is to call function movegen when the arguments are less than 4, to return empty string when there's no argument, to put arguments into stack when there're more than 4 arguments(TO BE DONE)
@param args the list of arguments, in type string
@return unit *)
let rec to_arm_formal_args args i =
    match args with
    | [] -> sprintf ""
    | l when (List.length l <= 4) -> sprintf "\tldr r%i, [fp, #%i]\n%s" i (fst (frame_position (List.hd l))) (to_arm_formal_args (List.tl l) (i+1))
    | a1::a2::a3::a4::l -> sprintf "%s%s" (to_arm_formal_args (a1::a2::a3::a4::[]:string list)  0) (stack_remaining_arguments l)
    | _ -> failwith "Error while parsing arguments"

(* Helpers for 3-addresses operation (like add or sub) *)
let rec store_in_stack register_id dest =
    let frame_offset, need_push = frame_position dest in
    let push_stack = if need_push then "\tadd sp, sp, #-4\n" else "" in
    sprintf "%s\tstr r%i, [fp, #%i]\n" push_stack register_id frame_offset

let rec operation_to_arm op e1 e2 dest =
    let store_arg1 = sprintf "\tldr r4, [fp, #%i]\n" (fst (frame_position e1)) in
        (match e2 with
         | Var id -> let store_arg2 = sprintf "\tldr r5, [fp, #%i]\n" (fst (frame_position id)) in
                    let return_result = sprintf "\t%s r6, r4, r5\n%s" op (store_in_stack 6 dest) in
                    sprintf "%s%s%s" store_arg1 store_arg2 return_result
         | Int i -> let store_arg2 = sprintf "\tmov r5, #%i\n" i in
                    let return_result = sprintf "\t%s r6, r4, r5\n%s" op (store_in_stack 6 dest) in
                    sprintf "%s%s%s" store_arg1 store_arg2 return_result
         | _ -> failwith "Unauthorized type"
        )

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
    | Land (e1, e2) -> operation_to_arm "land" e1 e2 dest
    | Call (l1, a1) -> let l = (Id.to_string l1) in sprintf "%s\tbl %s\n%s" (to_arm_formal_args a1 0) (remove_underscore l) (store_in_stack 0 dest)
    
    | New (e1) -> (match e1 with
                (* We want to call min_caml_create_array on the id and return the adress *)
                | Var id -> let call = sprintf "%s\tbl talloc\n%s" (to_arm_formal_args [id] 0) (store_in_stack 0 dest)
                            in sprintf "%s" call
                | Int i -> let store_string = store_in_stack 0 dest in 
                           let prepare_arg = sprintf "\tmov r0, #%s\n" (string_of_int i) in
                           let call_alloc = sprintf "\tbl talloc\n%s" (store_in_stack 0 dest) in
                               sprintf "%s%s%s" prepare_arg store_string call_alloc
                | _ -> failwith "Unauthorized type"
    )

    | MemAcc (id1, id2) ->
            let store_arg1 = sprintf "\tldr r4, [fp, #%i]\n" (fst (frame_position id1)) in
            let load = sprintf "\tldr r4, [r4, r5, LSL #2]\n" in
            let mov = sprintf "%s" (store_in_stack 4 dest) in
                (match id2 with
                | Var id -> let store_arg2 = sprintf "\tldr r5, [fp, #%i]\n" (fst (frame_position id)) in
                            sprintf "%s%s%s%s" store_arg1 store_arg2 load mov 
                | Int i ->  let store_arg2 = sprintf "\tmov r5, #%i\n" i in
                            sprintf "%s%s%s%s" store_arg1 store_arg2 load mov 
                | _ -> failwith "Unauthorized type"
                );

                
    | MemAff (id1, id2, id3) ->
            let saver7 = sprintf "\tstmfd sp!, {r7}\n" in
            let store_arg1 = sprintf "\tldr r4, [fp, #%i]\n" (fst (frame_position id1)) in
            let prepstore = sprintf "\tldr r7, [fp, #%i]\n" (fst (frame_position id3)) in
            let store = sprintf "\tstr r7, [r4, r5, lsl #2]\n" in
            let restorer7 = sprintf "\tldmfd sp!, {r7}\n" in
            (match id2 with
            | Var id -> let store_arg2 = sprintf "\tldr r5, [fp, #%i]\n" (fst (frame_position id)) in
                        sprintf "%s%s%s%s%s%s" saver7 store_arg1 store_arg2 prepstore store restorer7
            | Int i ->  let store_arg2 = sprintf "\tmov r5, #%i\n" i in
                        sprintf "%s%s%s%s%s%s" saver7 store_arg1 store_arg2 prepstore store restorer7
            | _ -> failwith "Unauthorized type"
            )

    | If (id1, e1, asmt1, asmt2, comp) ->
            let counter = genif() in 
            let store_arg1 = sprintf "\tldr r4, [fp, #%i]\n" (fst (frame_position id1)) in
            let cmpop = sprintf "\tcmp r4, r5\n" in
            let branch1 = sprintf "\t%s if%s\n" comp counter in
            let codeelse = sprintf "%s" (asmt_to_arm asmt2 dest) in
            let branch2 = sprintf "\tb end%s\n\n" counter in
            let codeif = sprintf "if%s:\n%s\n" counter (asmt_to_arm asmt1 dest) in
            let endop = sprintf "end%s:\n" counter in
            (match e1 with
            | Var id -> let store_arg2 = sprintf "\tldr r5, [fp, #%i]\n" (fst (frame_position id)) in 
                        sprintf "%s%s%s%s%s%s%s%s" store_arg1 store_arg2 cmpop branch1 codeelse branch2 codeif endop
            | Int i  -> let store_arg2 = sprintf "\tmov r5,#%i\n" i in 
                        sprintf "%s%s%s%s%s%s%s%s" store_arg1 store_arg2 cmpop branch1 codeelse branch2 codeif endop
            | _ -> failwith "Unauthorized type"
            )
    | Nop -> sprintf "\tnop\n"
    | _ -> failwith "Error while generating ARM from ASML"

(** This function is a recursive function to print expressions contained in
  a statement
@param asm program in type asmt
@return unit*)
and asmt_to_arm asm dest =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP ...Imm" *)
    | Let (id, e, a) -> let exp_string = exp_to_arm e id in sprintf "%s%s" exp_string (asmt_to_arm a "")
    | Expression e -> sprintf "%s\tldr r0, [fp, #%i]\n" (exp_to_arm e dest) (fst (frame_position dest))

(* Helper functions for fundef *)
let rec pull_remaining_args l =
    match l with
    | [] -> ""
    | arg::args -> sprintf "\tldmfd r5!, {r4}\n\tstmfd sp!, {r4}\n%s" (pull_remaining_args args)

let rec get_args args =
    match args with
    | [] -> sprintf ""
    | l when (List.length l = 1) -> sprintf "\tstmfd sp!, {r0}\n\n"
    | l when (List.length l <= 4) -> sprintf "\tstmfd sp!, {r0-r%i}\n\n" ((List.length l)-1)
    | a1::a2::a3::a4::l -> sprintf "\tmov r5, fp\n\tadd r5, r5, #%i\n%s%s" (4*(List.length l) + 8) (pull_remaining_args l) (get_args (a1::a2::a3::a4::[]:string list))
    | _ -> failwith "Error while pushing arguments to the stack"

(** Puts the stack pointer where it was before a function call in order to free
 * space.
 * If we had less than 4 arguments then the stack was not used for them,
 * otherwise it was. *)
let rec epilogue args =
    if (List.length args <= 4) then
        "\tmov sp, fp
        \tldmfd sp!, {fp, pc}\n\n\n"
    else
        sprintf "\tmov sp, fp
                 \tldmfd sp!, {fp, lr}
                 \tadd sp, sp, #%i
                 \tbx lr\n\n\n" (4 * ((List.length args) - 4))

(** This function handles the printing of a given function
@param fundef program in type fundef
*)
let rec fundef_to_arm fundef =
    push_frame_table ();
    register_args (List.rev fundef.args);
    let arm_name = remove_underscore fundef.name in
    let label = sprintf "\t.globl %s\n%s:\n" arm_name arm_name in
    let prologue = sprintf "\t@prologue\n\tstmfd sp!, {fp, lr}\n" in
    let mov = sprintf "\tmov fp, sp\n\n" in
    let get_args = sprintf "\t@get arguments\n%s" (get_args fundef.args) in
    let functioncode = sprintf "\t@function code\n%s" (asmt_to_arm fundef.body "") in
    let epilogue = sprintf "\n\t@epilogue\n%s" (epilogue fundef.args) in
    pop_frame_table ();
    sprintf "%s%s%s%s%s%s" label prologue mov get_args functioncode epilogue
    
(** This function prints the function definitions contained in the list
@param fundefs the list of definitions
*)
(* The main function is at the top of the list. We give it the name _start and
 * print it first. Then we print the other functions one after the other by
 * going through the list *)
let rec fundefs_to_arm fundefs =
    match fundefs with
    | [start] -> push_frame_table (); 
    let start_string = sprintf "\t.globl _start\n_start:\n" in 
    let mov = sprintf "\tmov fp, sp\n\n" in
    let print_code = sprintf "%s\n" (asmt_to_arm start.body "") in 
    let print_exit = sprintf "\tbl min_caml_exit\n" in pop_frame_table (); 
    sprintf "%s%s%s%s" start_string mov print_code print_exit
    | h::l -> sprintf "%s%s" (fundef_to_arm h) (fundefs_to_arm l)
    | _ -> failwith "No main function found"

(** This function is a recursive function to conver tpye toplevel into type fundef
@param toplevel program in type toplevel
@return unit*)
let rec toplevel_to_arm toplevel =
    match toplevel with
    | Fundefs functions_list -> sprintf "\t.text\n%s" (fundefs_to_arm functions_list)
