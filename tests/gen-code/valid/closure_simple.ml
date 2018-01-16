let rec f x =
  let a = 1 in
    let rec g y =
      y + a
   in g 1
in print_int (f 1)
