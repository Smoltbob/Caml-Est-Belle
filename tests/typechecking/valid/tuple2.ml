let rec f x = 
	let (a,b)= x in 
	if a<b then
		print_int a
	else
		print_int b
		
in let rec h x = 1+2 in 
f(1,(h 3))