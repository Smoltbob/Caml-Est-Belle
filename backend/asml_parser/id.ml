open Printf
type t = string

let to_string x = sprintf "r%s" (string_of_int (Basicregist.registVar x))

let genid =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    Printf.sprintf "?v%d" !counter
