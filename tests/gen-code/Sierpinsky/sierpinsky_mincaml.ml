let rec for3 x y size =
    let c = x land y in
    let u = if x < size then
        if c = 0 then print_char "* " else print_char "  " 
    else nop in
    let x = x + 1 in
    let test = x + y in
    if test < size then for3 x y size else nop 

let rec for2 i y size =
    let u = 
        if y > 0 then print_char " " else nop in
    let i = i + 1 in 
        if i < y then for2 i y size else nop

let rec for1 y size = 
    let i = 0 in 
    let u = for2 i y size in
    let x = 0  in 
    let u = for3 x y size in
    let u = print_char "\n" in
    let y = y - 1 in
    if y >= 0 then for1 y size else nop

let sierpinsky size =
    let y = size - 1 in 
    for1 y size

let () = 
    sierpinsky 16

