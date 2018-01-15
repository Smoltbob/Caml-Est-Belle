let rec f x =
  let rec g y =
    y + 2
  in
  let rec h z =
    z + 1
  in
  g (h x)
in
f 1
