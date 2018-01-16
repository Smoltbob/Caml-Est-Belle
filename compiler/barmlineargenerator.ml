(** This file is to generate arm code from Bsyntax.toplevel stutructure by spill everything variables allocation method*)

open Bsyntax;;
open Printf;;
open List;;

let remove_underscore function_name =
    String.sub function_name 1 ((String.length function_name) - 1)

(* get a value between underscores in the string code *)
(* To be rewritten *)
let rec parse_string_code string_code index current_value =
    if index = String.length string_code then
        current_value
    else
        match string_code.[index] with
        | '_' -> current_value
        | current_char -> parse_string_code string_code (index+1) (current_value ^ (String.make 1 current_char))

(* To be rewritten *)
let rec resolve_linear_scan_register string_code =
    if string_code.[0] = 'R' then
        sprintf "r%s" (remove_underscore string_code)
    else
        sprintf "r%s" (parse_string_code string_code 3 "")

(* To be rewritten *)
let rec resolve_store_load string_code =
    if string_code.[0] = 'f' then
        let reg_number = parse_string_code string_code 3 "" in
        let load_address = parse_string_code string_code (4 + (String.length reg_number)) "" in
        let store_address = parse_string_code string_code (5 + (String.length reg_number) + (String.length load_address)) "" in
        let load_string = if load_address = "" then "" else sprintf "\tldr r%s, [fp, #-%s]\n" reg_number load_address in
        let store_string = if store_address = "" then "" else sprintf "\tstr r%s, [fp, #-%s]\n" reg_number store_address in
        sprintf "%s%s" store_string load_string
    else
        ""

let genif =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    sprintf "%d" !counter

let rec arg_to_arm arg_code i =
    if arg_code.[0] = 'R' then
        sprintf "\tmov r%i, %s\n" i (resolve_linear_scan_register arg_code)
    else
        resolve_store_load arg_code

(** This function is to call function movegen when the arguments are less than 4, to return empty string when there's no argument, to put arguments into stack when there're more than 4 arguments(TO BE DONE)
@param args the list of arguments, in type string
@return unit *)
let rec to_arm_formal_args args i =
    match args with
    | [] -> sprintf ""
    | l when (List.length l <= 4) -> sprintf "%s%s" (arg_to_arm (List.hd l) i) (to_arm_formal_args (List.tl l) (i+1))
    (*| a1::a2::a3::a4::l -> sprintf "%s%s" (to_arm_formal_args (a1::a2::a3::a4::[]:string list)  0) (stack_remaining_arguments l)*)
    | _ -> failwith "Error while parsing arguments"

let rec operation_to_arm op e1 e2 dest =
    let load_store_dest = resolve_store_load dest in
    let register_number_dest = resolve_linear_scan_register dest in
    let load_store_e1 = resolve_store_load e1 in
    let register_number_e1 = resolve_linear_scan_register e1 in
    match e2 with
    | Var id ->
        let load_store_id = resolve_store_load id in
        let register_number_id = resolve_linear_scan_register id in
        sprintf "%s%s%s\t%s %s, %s, %s\n" load_store_dest load_store_e1 load_store_id op register_number_dest register_number_e1 register_number_id
    | Int i -> sprintf "%s%s\t%s %s, %s, #%i\n" load_store_dest load_store_e1 op register_number_dest register_number_e1 i

(** This function is to convert assignments into arm code 
@param exp expression in the assigment
@param dest the variable which is assigned in this assignment
@return unit*)
let rec exp_to_arm exp dest =
    match exp with
    | Int i -> let load_store_string = resolve_store_load dest in
               let register_number_string = resolve_linear_scan_register dest in
               sprintf "%s\tmov %s, #%i\n" load_store_string register_number_string i
    | Var id -> let load_store_id = resolve_store_load id in
                let register_number_id = resolve_linear_scan_register id in
                let load_store_dest = resolve_store_load dest in
                let register_number_dest = resolve_linear_scan_register dest in
                sprintf "%s%s\tmov %s, %s\n" load_store_id load_store_dest register_number_dest register_number_id
    | Add (e1, e2) -> operation_to_arm "add" e1 e2 dest
    | Sub (e1, e2) -> operation_to_arm "sub" e1 e2 dest
    | Land (e1, e2) -> operation_to_arm "land" e1 e2 dest
    | Call (l1, a1) -> let l = (Id.to_string l1) in sprintf "%s\tbl %s @no return value for now\n" (to_arm_formal_args a1 0) (remove_underscore l)
    (*
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
            let store_arg1 = 
                match id1 with
                | function_name when (function_name.[0] = '%') -> sprintf "\tldr r4, =%s\n" (remove_underscore function_name)
                | _ -> sprintf "\tldr r4, [fp, #%i]\n" (fst (frame_position id1))
            in
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
            *)
    | Nop -> sprintf "\tnop\n"
    | _ -> failwith "Error while generating ARM from ASML"

(** This function is a recursive function to print expressions contained in
  a statement
@param asm program in type asmt
@return unit*)
and asmt_to_arm asm dest =
    match asm with
    (* We want ex "ADD R1 R2 #4" -> "OP ...Imm" *)
    | Let (id, e, a) -> let exp_string = exp_to_arm e id in sprintf "%s%s" exp_string (asmt_to_arm a "R0")
    | Expression e -> exp_to_arm e dest

(* Helper functions for fundef *)
let rec pull_remaining_args l =
    match l with
    | [] -> ""
    | arg::args -> sprintf "\tldmfd r5!, {r4}\n\tstmfd sp!, {r4}\n%s" (pull_remaining_args args)

let rec get_args args =
    if List.length args <= 4 then
        ""
    else
        failwith "Functions with more than 4 arguments not handled yet in linear scaning"

(** Puts the stack pointer where it was before a function call in order to free
 * space.
 * If we had less than 4 arguments then the stack was not used for them,
 * otherwise it was. *)
let rec epilogue args =
    if (List.length args <= 4) then
        "\tmov sp, fp\n\tldmfd sp!, {fp, pc}\n\n\n"
    else
        sprintf "\tmov sp, fp\t\nldmfd sp!, {fp, lr}\t\nadd sp, sp, #%i\t\nbx lr\n\n\n" (4 * ((List.length args) - 4))

(** This function handles the printing of a given function
@param fundef program in type fundef
*)
let rec fundef_to_arm fundef =
    let arm_name = remove_underscore fundef.name in
    let label = sprintf "\t.globl %s\n%s:\n" arm_name arm_name in
    let prologue = sprintf "\t@prologue\n\tstmfd sp!, {fp, lr}\n" in
    let mov = sprintf "\tmov fp, sp\n\n" in
    let get_args = sprintf "\t@get arguments\n\t@stack arguments not implemented yet\n%s" (get_args fundef.args) in
    let functioncode = sprintf "\t@function code\n%s" (asmt_to_arm fundef.body "R0") in
    let epilogue = sprintf "\n\t@epilogue\n%s" (epilogue fundef.args) in
    sprintf "%s%s%s%s%s%s" label prologue mov get_args functioncode epilogue
    
(** This function prints the function definitions contained in the list
@param fundefs the list of definitions
*)
(* The main function is at the top of the list. We give it the name _start and
 * print it first. Then we print the other functions one after the other by
 * going through the list *)
let rec fundefs_to_arm fundefs =
    match fundefs with
    | [start] -> let start_string = sprintf "\t.globl _start\n_start:\n" in 
    let mov = sprintf "\tmov fp, sp\n\n" in
    let print_code = sprintf "%s\n" (asmt_to_arm start.body "") in 
    let print_exit = sprintf "\tbl min_caml_exit\n" in 
    sprintf "%s%s%s%s" start_string mov print_code print_exit
    | h::l -> sprintf "%s%s" (fundef_to_arm h) (fundefs_to_arm l)
    | _ -> failwith "No main function found"

(** This function is a recursive function to conver tpye toplevel into type fundef
@param toplevel program in type toplevel
@return unit*)
let rec toplevel_to_arm toplevel =
    match toplevel with
    | Fundefs functions_list -> sprintf "\t.text\n%s" (fundefs_to_arm functions_list)
