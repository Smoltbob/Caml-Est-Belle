let dummy = Array.create 0 0. in
let rec make m n =
  let mat = Array.create m dummy in
  let rec init i =
    if i < 0 then () else
    (mat.(i) <- Array.create n 0.;
     init (i - 1)) in
  init (m - 1);
  mat in
let a = make 2 3 in
a.(0).(0) <- 1.; 
() 