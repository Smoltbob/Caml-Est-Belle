let x = Array.create 3 (1,3,4) in 
let t1 = x.(0) <- (1,2,4) in
let t2 = x.(1) <- (3,5,5) in
let rec f x = 
	let (a,b,c)= x in 
		a+b+c
in print_int (f (x.(1)))