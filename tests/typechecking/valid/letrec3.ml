let rec f g = g 1 2 in
let rec h a b = a + b in
print_int (f h)