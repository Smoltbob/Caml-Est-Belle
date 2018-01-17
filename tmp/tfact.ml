let rec fact n =
    if n = 1 then
        1
    else
        n * (fact (n-1))
in
let _ = fact 4 in
 ()
