let rec f x =
  let rec g y =
    let rec h z =
      z + 1
    in h y
  in g x
in f 1
