let x = Array.create 3 2.3 in 
let t1 = x.(0) <- 2.3 in
let t2 = x.(1) <- 3.3 in
print_int (x.(1))