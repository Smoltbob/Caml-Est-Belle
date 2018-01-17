let rec for3 x y size =
    let c = x land y in
    let u = if x < size then
        if c = 0 then () else () 
    else () in
    let x = x + 1 in
    let test = x + y in
    if test < size then for3 x y size else () 
in

let rec for2 i y size =
    let u = 
        if y > 0 then () else () in
    let i = i + 1 in 
        if i < y then for2 i y size else ()
in

let rec for1 y size = 
    let i = 0 in 
    let u = for2 i y size in
    let x = 0  in 
    let u = for3 x y size in
    let u = () in
    let y = y - 1 in
    if y >= 0 then for1 y size else ()
in

let rec sierpinsky size =
    let y = size - 1 in 
    for1 y size
in

sierpinsky 20000
