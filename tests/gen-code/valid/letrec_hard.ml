let rec f x =
  let rec g y =
    y + 2
  in
  g x
in
f 1
