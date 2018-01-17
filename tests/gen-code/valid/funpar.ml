let rec f g x =
    print_int (g x)
in
let rec g x =
    42 - x
in
f g 1
