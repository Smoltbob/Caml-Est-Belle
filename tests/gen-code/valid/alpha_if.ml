let x = 1 in
let _ =
    if true then
        let x = 2 in
        ()
    else
        ()
in
print_int x

