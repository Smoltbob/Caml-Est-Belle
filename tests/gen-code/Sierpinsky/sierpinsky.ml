open Printf

let rec for3 x y size =
    let a = "* " in
    let b = "  " in
    let c = x land y in
    if x < size then
        if c = 0 then printf "%s" a else printf "%s" b 
    else ();
    let x = x + 1 in
    let test = x + y in
    if test < size then for3 x y size else () 

let rec for2 i y size =
    if y > 0 then printf " " else ();
    let i = i + 1 in 
    if i < y then for2 i y size else ()

let rec for1 y size = 
    let i = 0 in for2 i y size;
    let x = 0  in for3 x y size;
    printf "\n" ;
    let y = y - 1 in
    if y >= 0 then for1 y size else ()

let sierpinsky size =
    let y = size - 1 in 
    for1 y size

let () = 
    sierpinsky 32
