open Bsyntax;;
open Printf;;

let register_nb = 12
let vartbl_s = Hashtbl.create register_nb
let frame_index = ref 0

(* WIP ARM generation *)

let frame_position variable_name =
	if (not (Hashtbl.mem vartbl_s variable_name)) then
        begin
		    frame_index := !frame_index + 4;
            Hashtbl.add vartbl_s variable_name !frame_index
        end;
    Hashtbl.find vartbl_s variable_name


let rec movegen l i =
    match l with
        | [] -> sprintf ""
        | t::q -> sprintf "\tldr r4, [fp, #%i]\n\tmov r%i, r4\n%s" (frame_position t) i (movegen q (i + 1))

let rec to_arm_formal_args args =
    (* if len(args) <= 4:
        * for i in range(len(args)):
            * print mov rito_string(args[i])*)
    match args with
    | [] -> sprintf ""
    | l when (List.length l <= 4) -> movegen l 0
    | _ -> failwith "Not handled yet"
    (*| t::q -> sprintf "%s %s" (Id.to_string t) (to_arm_formal_args q *) 

(* OK *)
let rec exp_to_arm exp dest =
    match exp with
    | Int i -> sprintf "\tmov r4, #%s\n\tstr r4, [fp, #%i]\n" (string_of_int i) (frame_position dest)
    | Var id -> sprintf "\tldr r4, [fp, #%i]\n\tmov r5, r4\n\tstr r5, [fp, #%i]\n" (frame_position id) (frame_position dest)
    | Add (e1, e2)  -> sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n\tadd r6, r4, r5\n\tstr r6, [fp, #%i]\n\n" (frame_position e1) (frame_position e2) (frame_position dest)
    | Sub (e1, e2) -> sprintf "\tldr r4, [fp, #%i]\n\tldr r5, [fp, #%i]\n\tsub r6, r4, r5\n\tstr r6, [fp, #%i]\n\n" (frame_position e1) (frame_position e2) (frame_position dest)
    | Call (l1, a1) -> let l = (Id.to_string l1) in sprintf "%s\tbl %s\n\n" (to_arm_formal_args a1) (String.sub l 1 ((String.length l) - 1))
    | Nop -> sprintf "\tnop\n"
    | _ -> failwith "Error while generating ARM from ASML"

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
    | Fundefs f -> sprintf ".text\n.global _start\n_start:\n\tmov fp, sp\n%s\tbl min_caml_exit\n" (fundef_to_arm (List.hd f))
