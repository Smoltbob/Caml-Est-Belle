let rec f x = x+1 in 
let x = Array.create 3 f in 
let t1 = x.(0) <- f in
  print_int(x.(0)(2))