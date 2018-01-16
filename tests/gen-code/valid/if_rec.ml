let rec f n =
  if n = 0 then
    let t = print_int 1 in
    print_newline ()
  else
    let t = print_int 0 in
    f (n-1)
in f 4
