let rec f _ = 
   let rec g _ = print_int 1; f () 
   in print_int 2; g () 
in f ()
