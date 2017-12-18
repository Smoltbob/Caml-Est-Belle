(* The maximum number of variables we allow.
 * It will be used to create a table used to associate a unique index to
 * each variable *)
let register_number = 10 
let variabletable = Array.make register_number "" 

(* Returns the index of a variable by using the table.
 * If the variable is not in the table then we put it inside. *)
let registVar x = 
	let i = ref 0 in
	while (!i < register_number) && (variabletable.(!i) <> "") && (x <> variabletable.(!i)) do
		i := !i + 1;
	done;
	if !i = register_number then
        (failwith "variabletable is full")
    else
        (variabletable.(!i) <- x; !i)
(*
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
	let prog = Sub("a", Var("b")) in(* Fparser.toplevel is the input*)
	regist prog variabletable;
	Array.iter print_string variabletable
	in ()
*)
