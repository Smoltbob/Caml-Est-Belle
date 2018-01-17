let t = ((if 1=1 then 0 else 1), (let a = 2 in a)) in
let (a,b) = t in 
let t = print_int a in
print_int b
