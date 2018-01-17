let rec f x =
    f
in
let g = f 1 in
g 1
