let rec f x =
  1 + x
in
let t = (f, (if 1 = 1 then 1 else 2)) in
let (a,b) = t in
print_int (a b)
