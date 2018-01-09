open Bsyntax.ml

let live_interval_s = ref []
let live_interval_e = ref []
let asm_counter = ref 0

let calcu_live_interval toplevel = 
	match toplevel with
		|Fundefs f -> calcu_live_interval_f f

let prepend_live_interval counter arg = 
	live_interval_s := !live_interval_s @ [(arg, counter)];
	live_interval_e := (arg, counter) :: !live_interval_e
	
let calcu_live_interval_f fundef = 
	match fundef with
		|Args args -> List.iter (prepend_live_interval asm_counter) args; 
		|Body b -> expire_fundef_body b

let rec delet l x =
	match l with
	| t::q where t = x -> q
	| t::q -> t :: (delet q x)
	| _ -> failwith "delet"
	
let modify_live_interval_e var counter =
	live_interval_e := delet !live_interval_e var;
	live_interval_e := (var, counter) :: !live_interval_e

let rec expire_asm_exp e counter =
	(*look for var in e*)
	match e with
	| Add (a, b) -> modify_live_interval_e a counter; modify_live_interval_e b counter
	| Sub (a, b) -> modify_live_interval_e a counter; modify_live_interval_e b counter
	| Var a -> modify_live_interval_e a counter
	| Eq (a, exp) -> modify_live_interval_e a counter; expire_expression exp counter
	| _ failwith ("match failure with blinearscan expression")

let rec expire_fundef_body asm =
	match asm with
	|Let (id e a) -> asm_counter = asm_counter + 1; prepend_live_interval asm_counter id; expire_asm_exp e asm_counter; expire_fundef_body a
	|Expression e -> asm_counter = asm_counter + 1; expire_asm_exp e asm_counter



	
	
	
