(** This file is to generate arm code from Bsyntax.toplevel stutructure by spill everything variables allocation method*)

open Bsyntax;;
open Printf;;
open List;;

(** The frames are managed on a frame stack where we always access the top one. For an obscure reason having a simple frame and reseting it after the translation of each function wasn't working *)
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

(** Has to be called when entering a function, it adds the arguments in registers and in the stack to the frame *)
let rec register_args args =
    match args with
    | [] -> ()
    | arg::arg_list -> let top_frame_table = Stack.top frames_stack in if (not (Hashtbl.mem top_frame_table arg)) then
                            (let frame_index = -4 * (Hashtbl.length top_frame_table) - 4 in
                            Hashtbl.add top_frame_table arg frame_index);
                           register_args arg_list
(** Has to be called before everything when calling a function, it creates a fresh frame table for the current function*)
let rec push_frame_table () =
    let (new_frame_table:(string, int) Hashtbl.t) = Hashtbl.create 10 in
    Stack.push new_frame_table frames_stack
(** Is called at the end of a function, it shouldn't be needed but we use it for reseting the top frame *)
let rec pop_frame_table () =
    let _ = Stack.pop frames_stack in ()

(** Counter used to associate unique identifiers to "if" labels *)
let genif =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    sprintf "%d" !counter

(** Removes the first character of a string 
 @param String to process *)
let remove_underscore function_name =
    String.sub function_name 1 ((String.length function_name) - 1)

(** The caller adds the arguments that doesn't fit in registers to the stack to be pulled by callee function *)
let rec stack_remaining_arguments args =
    match args with
    | [] -> ""
    | arg::arg_list -> sprintf "\tldr r4, [fp, #%i]\n\tstmfd sp!, {r4}\n%s" (fst (frame_position arg)) (stack_remaining_arguments arg_list)

(** This function is to call function movegen when the arguments are less than 4, to return empty string when there's no argument, to put arguments into stack when there're more than 4 arguments
@param args the list of arguments, in type string
@return unit *)
let rec to_arm_formal_args args i =
    match args with
    | [] -> sprintf ""
    | l when (List.length l <= 4) -> let frame_offset, need_push = frame_position (List.hd l) in
                                     let push_stack = if need_push then "\tadd sp, sp, #-4\n" else "" in
                                     sprintf "%s\tldr r%i, [fp, #%i]\n%s" push_stack i frame_offset (to_arm_formal_args (List.tl l) (i+1))
    | a1::a2::a3::a4::l -> sprintf "%s%s" (to_arm_formal_args (a1::a2::a3::a4::[]:string list)  0) (stack_remaining_arguments l)
    | _ -> failwith "Error while parsing arguments"

(* Helpers for 3-addresses operation (like add or sub) *)
(** When storing in the stack, this function checks if we need to allocate some space or not *)
let rec store_in_stack register_id dest =
    let frame_offset, need_push = frame_position dest in
    let push_stack = if need_push then "\tadd sp, sp, #-4\n" else "" in
    sprintf "%s\tstr r%i, [fp, #%i] @%s\n" push_stack register_id frame_offset dest

(** Helper function for 3 address code operation
 @param op the assembly directive of the operation
 @param e1 the first operand (needs to be a register
 @param e2 the second operand (can be a register or a 16bits immediate
 @param dest the destination register
 @return a string representing the assembly code to perform the operation *)
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
    (* To create negative values we substract them to 0 *)
    | Neg id -> let store_string = store_in_stack 4 dest in
                    sprintf "\tldr r4, [fp, #%i]\nmov r5, #0\n\tsub r4, r5, r4\n%s" (fst (frame_position id)) store_string
    (* Storing immediates with support for big ints *)
    | Int i -> let store_string = store_in_stack 4 dest in
               let move_string = if i < 65536 then
                   sprintf "\tmov r4, #%i\n" i
               else
                   sprintf "\tmovw r4, #:lower16:%i\n\tmovt r4, #:upper16:%i\n" i i
               in move_string ^ store_string
    (* Loading variables from the frame table *)
    | Var id -> (match id with 
                (* Here we want to treat labels and variable names differently *)
                | _ -> let str = (Id.to_string id) in 
                       let store_string = store_in_stack 4 dest in
                       if str.[0] = '_' then 
                                sprintf "\tldr r4, =%s\n%s" (remove_underscore str) store_string
                            else 
                                sprintf "\tldr r4, [fp, #%i]\n%s" (fst (frame_position id)) store_string
                )
    (* Binary operations *)
    | Add (e1, e2) -> operation_to_arm "add" e1 e2 dest
    | Sub (e1, e2) -> operation_to_arm "sub" e1 e2 dest
    (* Function calls. We have to prepare the function arguments, then call the function and return the value into the destination
     * register. *)
    | Land (e1, e2) -> operation_to_arm "and" e1 e2 dest
    | Call (l1, a1) -> let l = (Id.to_string l1) in
                       let args_string = (to_arm_formal_args a1 0) in
                       let function_call_name = (remove_underscore l) in
                       let store_string = (store_in_stack 0 dest) in
                       sprintf "%s\tbl %s\n%s" args_string function_call_name store_string

    (* Closure application. We have to store the closure address into the word "_self", prepare the arguments and call the closure. *)
    | CallClo (l1, a1) -> let store_closure = sprintf "\tldr r6, [fp, #%i]\n\tldr r5, _self\n\tstr r6, [r5]\n" (fst(frame_position l1)) in
                          let prep_args = sprintf "%s" (to_arm_formal_args a1 0) in
                          let load_addr = sprintf "\tldr r4, [fp, #%i]\n\tldr r4, [r4]\n" (fst (frame_position l1)) in 
                          let branch = sprintf "\tblx r4\n" in 
                          sprintf "%s%s%s%s" store_closure prep_args load_addr branch 

    (* New allocates space on the heap. It's bascially a macro for min_caml_create_array.
     * If we call it with an int, we put it first in a register and then call the function. *)
    | New (e1) -> (match e1 with
                (* We want to call min_caml_create_array on the id and return the adress *)
                | Var id -> let call = sprintf "%s\tmov r1, #0\nbl min_caml_create_array\n%s" (to_arm_formal_args [id] 0) (store_in_stack 0 dest)
                            in sprintf "%s" call
                | Int i -> let store_string = store_in_stack 0 dest in 
                           let prepare_arg = sprintf "\tmov r0, #%s\n" (string_of_int i) in
                           let call_alloc = sprintf "\tmov r1, #0\n\tbl min_caml_create_array\n%s" (store_in_stack 0 dest) in
                               sprintf "%s%s%s" prepare_arg store_string call_alloc
                | _ -> failwith "Unauthorized type"
    )
    
    (* Memory access for an array in the heap. We want to make a special case for the base address%self, 
     * in which case we will need to load it from the word "_self".
     * Otherwise we take the base address id1 and add to it the offset id2 that we shift 2 bits to the left (LSL #2).
     * It makes the final address of the cell from which we can read. *)
    | MemAcc (id1, id2) ->
            let store_arg1 = 
                match id1 with
                (*| function_name when (function_name.[0] = '%') -> sprintf "\tldr r4, =%s\n" (remove_underscore function_name)*)
                | function_name when (function_name = "%self") -> sprintf "\tldr r4, _self\n\t ldr r4, [r4]\n"
                | _ -> sprintf "\tldr r4, [fp, #%i]\n" (fst (frame_position id1))
            in
            let load = sprintf "\tldr r4, [r4, r5, LSL #2]\n" in
            let mov = sprintf "%s" (store_in_stack 4 dest) in
                (* If we have an int, we want to first put it into a register. *)
                (match id2 with
                | Var id -> let store_arg2 = sprintf "\tldr r5, [fp, #%i]\n" (fst (frame_position id)) in
                            sprintf "%s%s%s%s" store_arg1 store_arg2 load mov 
                | Int i ->  let store_arg2 = sprintf "\tmov r5, #%i\n" i in
                            sprintf "%s%s%s%s" store_arg1 store_arg2 load mov 
                | _ -> failwith "Unauthorized type"
                );

    (* Memory affectation. Works with the same mechanisms as memory access. Only difference : we need a new register
     * r7 to store the value to put in the array. To make sure we don't perturb the rest of the code, we save it on
     * the stack with stmfd before using it, and when we are done we restore it with ldmfd. *) 
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
            
    (* If statement. We create one label called "if" followed by a unique number. It contains the first asmt. If we don't
     * branch into it we want to execute the second asmt.
     * Because we can have different comparison operators in an if, we put them in the variable comp. It can be "ble", "beq"
     * "beg". *)
    | If (id1, e1, asmt1, asmt2, comp) ->
            let counter = genif() in 
            let store_arg1 = sprintf "\tldr r4, [fp, #%i] @%s\n" (fst (frame_position id1)) id1 in
            let store_arg2 = match e1 with
                             | Var id -> sprintf "\tldr r5, [fp, #%i] @%s\n" (fst (frame_position id)) id
                             | Int i  -> sprintf "\tmov r5,#%i\n" i 
                             | _ -> failwith "Unauthorized type" 
            in
            let cmpop = sprintf "\tcmp r4, r5\n" in
            let branch1 = sprintf "\t%s if%s\n" comp counter in
            let codeelse = sprintf "%s" (asmt_to_arm asmt2 dest) in
            let branch2 = sprintf "\tb end%s\n\n" counter in
            let codeif = sprintf "if%s:\n%s\n" counter (asmt_to_arm asmt1 dest) in
            let endop = sprintf "end%s:\n" counter in
            sprintf "%s%s%s%s%s%s%s%s" store_arg1 store_arg2 cmpop branch1 codeelse branch2 codeif endop

    | Nop -> sprintf "\tnop\n"
    | _ -> failwith "Error while generating ARM from ASML"

(** This function is a recursive function to print expressions contained in
  a statement
@param asm program in type asmt
@return unit*)
and asmt_to_arm asm dest =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP ...Imm" *)
    | Let (id, e, a) -> let exp_string = exp_to_arm e id in
                        let next_asmt_string = asmt_to_arm a dest in
                        exp_string ^ next_asmt_string
    | Expression e -> (match e with
                    |CallClo(l, a) -> sprintf "%s" (exp_to_arm e dest)
                    | _ -> let exp_string = exp_to_arm e dest in
                      let return_value_string = sprintf "\tldr r0, [fp, #%i]\n" (fst (frame_position dest)) in
                      exp_string ^ return_value_string
    )

(** Pulls the arguments that doesn't fit in the first registers from the stack and copy them in the local frame (callee) *)
let rec pull_remaining_args l =
    match l with
    | [] -> ""
    | arg::args -> sprintf "\tldmfd r5!, {r4}\n\tstmfd sp!, {r4}\n%s" (pull_remaining_args args)

(** Loads the arguments from r0 to r3 (callee) *)
let rec get_args args =
    match args with
    | [] -> sprintf ""
    | l when (List.length l = 1) -> sprintf "\tstmfd sp!, {r0}\n\n"
    | l when (List.length l <= 4) -> sprintf "\tstmfd sp!, {r0-r%i}\n\n" ((List.length l)-1)
    | a1::a2::a3::a4::l -> sprintf "\tmov r5, fp\n\tadd r5, r5, #8\n%s%s" (pull_remaining_args l) (get_args (a1::a2::a3::a4::[]:string list))
    | _ -> failwith "Error while pushing arguments to the stack"

(** Puts the stack pointer where it was before a function call in order to free
 * space.
 * If we had less than 4 arguments then the stack was not used for them,
 * otherwise it was. *)
let rec epilogue args =
    if (List.length args <= 4) then
        "\tmov sp, fp\n\tldmfd sp!, {fp, pc}\n\n\n"
    else
        sprintf "\tmov sp, fp\n\tldmfd sp!, {fp, lr}\n\tadd sp, sp, #%i\n\tbx lr\n\n\n" (4 * ((List.length args) - 4))

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

(** This function is a function to conver type toplevel into type fundef
@param toplevel program in type toplevel
@return unit*)
let rec toplevel_to_arm toplevel =
    match toplevel with
    | Fundefs functions_list -> let data_section = sprintf "\t.data\n\t.balign 4\nself: .word 0" in 
                                let word_declaration = sprintf "_self: .word self" in 
                                sprintf "%s\n\n\t.text\n%s\n\n%s\n" data_section (fundefs_to_arm functions_list) word_declaration
