open Syntax;;

let register_number = 10 in
let variabletable = Array.make register_number "" in

let registVar x = 
	let i = ref 0 in
	while (!i < (Array.length variabletable)) && (variabletable.(!i) <> "") && (x <> variabletable.(!i)) do
		i := !i + 1;
	done;
	if !i = Array.length variabletable then
		(failwith "variabletable is full");
	if (variabletable.(!i) = "") then
		(variabletable.(!i) <- x)
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
	regist prog variabletable;
	Array.iter print_string variabletable
	in ()
