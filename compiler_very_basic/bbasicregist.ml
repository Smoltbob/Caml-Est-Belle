open Bsyntax;;

let register_nb = 12
let vartbl_r = Hashtbl.create register_nb
let register_i = ref 3

let registVar x =
	if (not (Hashtbl.mem vartbl_r x)) then
		register_i := !register_i + 1;
		Hashtbl.add vartbl_r x (true, !register_i)

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
	|Eq(a,b) -> registVar a; regist b table
	|Var(x) -> registVar x
