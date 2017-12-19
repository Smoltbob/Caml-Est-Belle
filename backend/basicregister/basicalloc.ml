open Syntax;;

let max_var_nb = 10 in
let register_nb = 10 in
let vartbl_s = Hashtbl.create max_var_nb in
let vartbl_r = Hashtbl.create register_nb in
let address = ref 0 in
let register_i = ref 15 in

let int_of_bool b = if b then 1 else 0 in

(* array tuple structure*)
(*
let allocVar x = 
	while ((!i < (Array.length vartbl)) && ((fst vartbl.(!i)) <> "") && (x <> (fst vartbl.(!i)))) do
		i := !i + 1;
	done;
	if !i = (Array.length vartbl) then
		(failwith "vartbl is full");
	if ((fst vartbl.(!i)) = "") then
	    address := !address + 4;
		vartbl.(!i) <- (x, !address)
		(* TODO : for now, we just alloc type int*)		
in
*)

(* hashtbl_tuple_structure: fst element in tuple is a boolean representing the var spilled or not, snd element is the adress or register *)
let allocVar x =
	if (not (Hashtbl.mem vartbl_s x)) then
		address := !address + 4;
		Hashtbl.add vartbl_s x (false, !address)
in

let rec alloc prog table =
	match prog with
	
	|Neg(x) -> allocVar x
	|Fneg(x) -> allocVar x
	|Fsub(a,b) -> allocVar a; allocVar b
	|Fadd(a,b) -> allocVar a; allocVar b
	|Fmul(a,b) -> allocVar a; allocVar b
	|Fdiv(a,b) -> allocVar a; allocVar b
	|Add(a,b) -> allocVar a; alloc b table
	|Sub(a,b) -> allocVar a; alloc b table
	|Let(a,b,c) -> allocVar a; alloc b table; alloc c table
	|Eq(a,b) -> allocVar a; alloc b table
	|Var(x) -> allocVar x
in

let registVar x =
	if (not (Hashtbl.mem vartbl_r x)) then
		register_i := !register_i + 1;
		Hashtbl.add vartbl_r x (true, !register_i)
in

let rec regist prog table =
	match prog with
	
	|Neg(x) -> registVar x
	|Fneg(x) -> registVar x
	|Fsub(a,b) -> registVar a; registVar b
	|Fadd(a,b) -> registVar a; registVar b
	|Fmul(a,b) -> registVar a; registVar b
	|Fdiv(a,b) -> registVar a; registVar b
	|Add(a,b) -> registVar a; regist b table
	|Sub(a,b) -> registVar a; regist b table
	|Let(a,b,c) -> registVar a; regist b table; regist c table
	|Eq(a,b) -> registVar a; regist b table
	|Var(x) -> registVar x
in	 

let () =
	let prog = Sub("a", Var("b")) in(* Parser.toplevel is the input*)
	regist prog vartbl_r;
	alloc prog vartbl_s;
	print_string ("a");
	print_int (int_of_bool (fst (Hashtbl.find vartbl_s "a")));
	print_int (snd (Hashtbl.find vartbl_s "a"));
	print_string ("b");
	print_int (int_of_bool (fst (Hashtbl.find vartbl_s "b")));
	print_int (snd (Hashtbl.find vartbl_s "b"));
	print_newline;
	print_string ("a");
	print_int (int_of_bool (fst (Hashtbl.find vartbl_r "a")));
	print_int (snd (Hashtbl.find vartbl_r "a"));
	print_string ("b");
	print_int (int_of_bool (fst (Hashtbl.find vartbl_r "b")));
	print_int (snd (Hashtbl.find vartbl_r "b"));
	
	in ()
