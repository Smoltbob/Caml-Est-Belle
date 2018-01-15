let rec print_multiple a b c d =
    let q = print_int a in
    let s = print_int b in
    let f = print_int c in
    let g = print_int d in
    let h = print_newline ()
    in ()
in
let rec print_a_lot a b c d e f g h =
    let q = print_int a in
    let s = print_int b in
    let j = print_int c in
    let k = print_int d in
    let l = print_int e in
    let m = print_int f in
    let w = print_int g in
    let x = print_int h in
    let v = print_newline ()
    in ()
in
let a = print_multiple 1 2 3 4 in
let b = print_a_lot 8 7 6 5 4 3 2 1 in
let c = print_multiple 2 4 6 4 in
let d = print_a_lot 1 2 3 4 5 6 7 8 in
let e = print_a_lot 3 14 15 92 65 3 58 97 in
()
