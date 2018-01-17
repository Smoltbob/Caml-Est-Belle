let rec f x = 
    if x=42 then
        1
    else

        let rec f x =
            2
        in
        f 42
in print_int (f 0)
