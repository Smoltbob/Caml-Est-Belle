
let rec f x = 
	let y = 1 in  f y 
	
in 
let rec h x = f x in print_int (h 1)
