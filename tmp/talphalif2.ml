let x = 1 in
let y = 0 in
let _ =
    if true then
        let x = 2 in
        let y = 3 in
        ()
    else
        let z = 4 in
        ()
in
let z = 5 in
print_int (x+y)

