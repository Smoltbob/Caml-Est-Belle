let rec g y = 
    let a = let rec f x = 1 in f 0 in a
in print_int (g 0)
