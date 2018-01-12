let rec good_print x =
    let a = print_int x in
    let b = print_newline () in
    ()
in
let rec addition x y =
    let b = good_print x in
    let c = good_print y in
    let a = x + y in 
    let d = good_print a in
    a
in
let a = good_print 5 in
let x = 1 + 2 in
let b = good_print x in
let y = 3 - 4 in
let c = good_print y in
let z = addition x y in
let d = good_print z in
()
