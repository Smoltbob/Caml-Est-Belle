open Syntax;;

let max_var_nb = 10 in
let register_number = 10 in
let vartbl = Hashtbl.create max_var_nb in
let address = ref 0 in
let i = ref 0 in

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
	if (not (Hashtbl.mem vartbl x)) then
		address := !address + 4;
		Hashtbl.add vartbl x (true, !address)
in

let rec regist prog table =
	match prog with
	
	|Neg(x) -> allocVar x
	|Fneg(x) -> allocVar x
	|Fsub(a,b) -> allocVar a; allocVar b
	|Fadd(a,b) -> allocVar a; allocVar b
	|Fmul(a,b) -> allocVar a; allocVar b
	|Fdiv(a,b) -> allocVar a; allocVar b
	|Add(a,b) -> allocVar a; regist b table
	|Sub(a,b) -> allocVar a; regist b table
	|Let(a,b,c) -> allocVar a; regist b table; regist c table
	|Eq(a,b) -> allocVar a; regist b table
	|Var(x) -> allocVar x
in	 

let () =
	let prog = Sub("a", Var("b")) in(* Parser.toplevel is the input*)
	regist prog vartbl;
	print_string ("a");
	print_int (int_of_bool (fst (Hashtbl.find vartbl "a")));
	print_int (snd (Hashtbl.find vartbl "a"));
	print_string ("b");
	print_int (int_of_bool (fst (Hashtbl.find vartbl "b")));
	print_int (snd (Hashtbl.find vartbl "b"));
	in ()
