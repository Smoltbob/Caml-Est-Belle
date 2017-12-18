open Printf
type t = string

let to_string x = x
let to_register x = sprintf "r%s" (string_of_int (Bbasicregist.registVar x))

let genid =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    Printf.sprintf "?v%d" !counter
