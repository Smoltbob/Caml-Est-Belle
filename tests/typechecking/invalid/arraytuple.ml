let x = Array.create 3 (1,3,4) in 
let t1 = x.(0) <- (1,2) in
let t2 = x.(1) <- (3,5) in
print_int (x.(1))