open Bliveinterval;;
open Bsyntax;;

let active = ref []
let spill = ref []
let free_reg = ref ["R0";"R1";"R2";"R3";"R4";"R5";"R6";"R7";"R8";"R9";"R10";"R12";]
let args_counter = ref 0
let spill_counter = ref 0
let live_interval_e_ht = to_hashtbl live_interval_e

	
let rec add_to_active tpl l = 
	match l with
	|t::q -> if (trd t) > (trd tpl) then t :: (add_to_active tpl q) else tpl::t::q
	|[] -> [tpl;]
	
(** spill_active_var we will spill the first elememt in active list in to memory, 
first we spill_to_mem frt_active(check if frt_active is in spill list, if not, store and add, if so, only store), reg_id = reg_frt_active, delete the first element from active list
@param l spill list
@param id the var need to be alloc
@return "f_R%i__add"*)
let rec spill_active_var id l=
	let fst_id = (fst (List.hd !active)) in
	match l with
	|t::q -> if (fst t) = fst_id then 
				let reg_id:Id.t = sprintf "f_%s__%i" (to_string (snd (List.hd !active))) (snd t) in
				let active := List.tl !active in
			else spill_active_var id q
	|[] ->  let spill_counter := !spill_counter + 4 in
			let active := List.tl !active in
			let reg_id:Id.t = sprintf "f_%s__%i" (to_string (snd (List.hd !active))) (!spill_counter) in
			spill := (fst_id, spill_counter) :: !spill;	
	reg_id	

(** expire_active_list is to remove the first dead variable in the active list upto the startpoint if var id
@param id a variable, in type Id.t
@param l active list
@return () *)
let rec expire_active_list id l= 
	match l with
	|t::q -> if (trd t) < (Hashtbl.find live_interval_s_ht id) then free_reg := (snd t)::!free_reg; q else t:: (expire_active_list id q)
	|[] -> ()

(** register_alloc is to alloc a register to a new defined variable,
we will first expire_active_list, 
then check the length of active list, if it's equal to the number of registers we can use then we spill_active_var else we pick the first element in free_reg list, remove it from free_reg, add (id, reg_id, endpoint) to active list and return reg_id
@param id the variable newly defined which needs ti be assigned to a register
@return "R%i" or "f_R%i__add"(return value of spill_active_var)*)
let register_alloc id =
	expire_active_list id !active;
	if (List.length !free_reg) = 0 then
		spill_active_var id !spill
	else 
		let reg_id:Id.t = List.hd !free_reg in
		let free_reg := List.tl !free_reg in
		reg_id
(** 
@param l spill list*)
let load_spill_active_var id l addr =
	let fst_id = (fst (List.hd !active)) in
	match l with
	|t::q -> if (fst t) = fst_id then 
				let reg_id:Id.t = sprintf "f_%s_%i_%i" (to_string (snd (List.hd !active))) (addr) (snd t) in
				let active := List.tl !active in
				let active := add_to_active (id, (snd (List.hd !active)), (Hashtbl.find live_interval_e_ht id)) !active in
			else 
				load_spill_active_var id q addr
	|[] ->  let spill_counter := !spill_counter + 4 in
			let active := List.tl !active in
			let reg_id:Id.t = sprintf "f_%s_%i_%i" (to_string (snd (List.hd !active))) (addr) (!spill_counter) in
			let active := add_to_active (id, (snd (List.hd !active)), (Hashtbl.find live_interval_e_ht id)) !active in
			spill := (fst_id, spill_counter) :: !spill;	
	reg_id
	

(** load_alloc_reg is a to load a variable which is in memory into a new register and add it into active list. we will first alloc a new register, load it into the new register and add it into active variable list;
@param id the variable need to be load to a register, in type Id.t
@param l_spill the spill list
@return a register and the load address in model "f_R%i_addr_addr", in type Id.t*)
let load_alloc_reg id addr =
	expire_active_list id !active;
	if (List.length !free_reg) = 0 then
		load_spill_active_var id !spill addr
	else
		let spill_counter := !spill_counter + 4 in
		let active := add_to_active (id, (List.hd !free_reg), (Hashtbl.find live_interval_e_ht id) ) !active in
		let reg_id:Id.t = "f_%s_%i_" (List.hd !free_reg) (!spill_counter) in
		reg_id
	
(** alloc_id_spill is a to assign a register to a variable which is in memory. we will find it in spill list, alloc a new register, load it into the new register and add it into active variable list;
@param id the variable need to be foud in spill list and be assigned to a register, in type Id.t
@param l_spill the spill list
@return a register and the load address in model "f_R%i_addr_addr", in type Id.t*)
let rec alloc_id_spill id l_spill =
	match l_spill with
	|t::q -> if (fst t) = id then load_alloc_reg id (snd t)  else alloc_id_spill id q
	|[] -> failwith ("failure with find variable in spill list")

(** alloc_id is a to assign a register to a variable which is already defined before. if this variable is already in the active  variable list , we will return the register which is already assigned to it; else it's in memory, we will alloc a new register, find it in spill list, load it into the new register and (#######)add it into active variable list;
@param id the variable need to be assigned, in type Id.t
@param l the active variable list 
@return a register, in type Id.t*)
let rec alloc_id id l =
	if (not (Hashtbl.mem live_intercal_e_ht id)) then failwith ("failure with finding a used local variable in live_interval_e");
	match l with
	|t::q -> if (fst t) = id then (snd t) else alloc_id id q
	|[] -> alloc_id_spill id !spill	

(** alloc_id_def is a to alloc a register to a new defined variable and add the new variable into active variable list;
@param id a new defined variable, in type Id.t
@para a register, in type Id.t*)
let alloc_id_def id = 
	let reg_id = register_alloc id in
	if (not (Hashtbl.mem live_intercal_e_ht id)) then failwith ("failure with finding a defined local variable in live_interval_e");
	let active := add_to_active (id, ref_id, (Hashtbl.find live_interval_e_ht id)) in
	reg_id

let alloc_exp e = 
	match e with
	|Int i -> Int i
	|Var id -> let reg_id = alloc_id id !active in Var reg_id
	|Add (a, b) -> let reg_a =alloc_id a !active in let reg_b = alloc_id b !active in Add (reg_a, reg_b) 
	|Sub (a, b) -> let reg_a =alloc_id a !active in let reg_b = alloc_id b !active in Sub (reg_a, reg_b) 
	|Eq (a, exp) -> let reg_a =alloc_id a !active in let reg_exp = alloc_exp exp in Eq (reg_a, reg_exp)
	| _ -> failwith ("match failure with blinearscan expression: TODO")
	
let rec alloc_asm asm = 
	match asm with
	|Let (id, e, a) -> let reg_id = alloc_id_def id in let reg_e = alloc_exp e in let reg_asm = alloc_asm a in Let (reg_id, reg_e, reg_asm)
	|Expression e -> let reg_e = alloc_exp e in Expression reg_e 
	
let rec active_args args =
	match args with
	|t::q -> if (not (Hashtbl.mem live_interval_e_ht t)) then failwith ("failure with finding arg in live_interval_e"); 
			args_counter := !args_counter + 1; 
			let x:Id.t = sprintf "R%i" (!args_counter - 1) in 
			let free_reg := List.tl !free_reg in
			let active := add_to_active (t, x, (Hashtbl.find live_interval_e_ht t)) !active in  
			x :: (active_args q)
	|[] -> ()
	
let alloc_args args =
	if (List.length args) < 5 then
		active_args args
	else
		failwith ("function arguments more than 4: TODO")

let alloc_fund fund =
	let reg_args = (alloc_args fund.args) in
	let reg_body = (alloc_asm fund.body) in
	{name = fund.name; args = reg_fund.args; body = reg_fund.body}

let rec alloc_funds funds = 
	match funds with
	|t::q -> alloc_fund t; alloc_funds q
	|[] -> ()

let registeralloc topl =
	match topl with
	|Fundefs funds -> Fundefs (alloc_funds funds)
