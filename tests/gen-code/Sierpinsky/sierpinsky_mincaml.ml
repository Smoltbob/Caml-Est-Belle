let rec for3 x y size =
    let c = land x y in
    let u = if x < size then
        if c = 0 then print_string "* " else print_string "  " 
    else () in
    let x = x + 1 in
    let test = x + y in
    if test < size then for3 x y size else () 

let rec for2 i y size =
    let u = 
        if y > 0 then print_string " " else () in
    let i = i + 1 in 
        if i < y then for2 i y size else ()

let rec for1 y size = 
    let i = 0 in 
    let u = for2 i y size in
    let x = 0  in 
    let u = for3 x y size in
    let u = print_string "\n" in
    let y = y - 1 in
    if y >= 0 then for1 y size else ()

let sierpinsky size =
    let y = size - 1 in 
    for1 y size

let () = 
    sierpinsky 16

