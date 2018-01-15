open Bsyntax;;

let live_interval_s = ref []
let live_interval_e = ref []
let asm_counter = ref 0

let prepend_live_interval counter arg = 
	live_interval_s := (!live_interval_s) @ [(arg, counter)];
	live_interval_e := (arg, counter) :: !live_interval_e
	
let rec delet l x =
	match l with
	| (a,b)::q when a = x -> q
	| (a,b)::q -> (a,b) :: (delet q x)
	| _ -> failwith ("failure with bliveinterval delete")
	
let modify_live_interval_e var counter =
	live_interval_e := delet !live_interval_e var;
	live_interval_e := (var, counter) :: !live_interval_e

let rec expire_asm_exp e counter =
	(*look for var in e*)
	match e with	
	| Int a -> ()
	| Add (a, b) -> modify_live_interval_e a counter; modify_live_interval_e b counter
	| Sub (a, b) -> modify_live_interval_e a counter; modify_live_interval_e b counter
	| Var a -> modify_live_interval_e a counter
	| Eq (a, exp) -> modify_live_interval_e a counter; expire_asm_exp exp counter
	| _ -> failwith ("match failure with bliveinterval expression")

let rec expire_fundef_body asm =
	match asm with
	|Let (id, e, a) -> asm_counter := !asm_counter + 1; prepend_live_interval !asm_counter id; expire_asm_exp e !asm_counter; expire_fundef_body a
	|Expression e -> asm_counter := !asm_counter + 1; expire_asm_exp e !asm_counter

let calcu_live_interval_f fundef = 
	List.iter (prepend_live_interval !asm_counter) fundef.args; 
	expire_fundef_body fundef.body
		
let calcu_live_interval toplevel = 
	match toplevel with
	|Fundefs f -> List.iter calcu_live_interval_f f

let rec print_live_interval live_interval =
	match live_interval with
	|t::q -> Printf.fprintf stdout "%s %i\n" (Id.to_string (fst t)) (snd t); print_live_interval q
	| [] -> ();
	Printf.fprintf stdout "end list\n"
	
let rec to_hashtbl l= 
	let l_ht = Hashtbl.create (List.length l) in
	(*(match l with
	|t::q -> Hashtbl.add l_ht (fst t) (snd t); to_hashtbl q
	|[] -> ());*)
	List.iter (fun (x,y) -> Hashtbl.add l_ht x y) l; l_ht






	
	
	
