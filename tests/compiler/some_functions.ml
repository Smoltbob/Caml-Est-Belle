let rec good_print x =
    let a = print_int x in
    let b = print_newline () in
    ()
in
let a = good_print 5 in
()
