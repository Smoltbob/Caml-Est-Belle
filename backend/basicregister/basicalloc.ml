open Syntax;;

let max_var_nb = 10 in
let register_number = 10 in
let variabletable = Array.make max_var_nb ("", 0) in
let address = ref 0 in
let i = ref 0 in

let allocVar x = 
	while ((!i < (Array.length variabletable)) && ((fst variabletable.(!i)) <> "") && (x <> (fst variabletable.(!i)))) do
		i := !i + 1;
	done;
	if !i = (Array.length variabletable) then
		(failwith "variabletable is full");
	if ((fst variabletable.(!i)) = "") then
	    address := !address + 4;
		variabletable.(!i) <- (x, !address)
		(* TODO : for now, we just alloc type int*)		
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
	regist prog variabletable;
	let j = ref 0 in
	while (!j < (Array.length variabletable)) do
	print_string (fst variabletable.(!j));
	print_int (snd variabletable.(!j));
	j := !j + 1;
	done
	in ()
