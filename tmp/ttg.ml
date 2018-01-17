let x = 1 in
let rec g y =
    y + x
in let rec f z = g z in ()
