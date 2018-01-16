let rec mul a b =
    if b = 1 then
        a
    else
        a + (mul a (b-1))
in

let rec fact n =
    if n = 1 then
        1
    else
        mul (fact (n-1)) n
in
let x = fact 4 in
let a = print_int x in
let b = print_newline () in
()
