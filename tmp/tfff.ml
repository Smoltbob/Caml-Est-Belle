let rec f x =
    let y = 1 in
    let rec g z =
        y + z
    in
    g x
in f 2
