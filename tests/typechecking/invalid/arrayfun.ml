let rec f x = 0 in 
let x = Array.create 3 f in 
let t1 = x.(0) <- 2.3 in
let t2 = x.(1) <- 3.3 in
 ()