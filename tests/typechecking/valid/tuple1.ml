let rec f x = 
	let (a,b)= x in 
	if a<b then
		print_int a
	else
		print_int b
		
in f(1,2)