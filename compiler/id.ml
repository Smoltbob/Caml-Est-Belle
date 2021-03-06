open Printf
type t = string
type l = string

(** Function is a syntactic sugar *)
let to_string x = x
(*let to_register x = sprintf "r%s" (string_of_int (Bbasicregist.registVar x))*)

let genid =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    sprintf "?v%d" !counter
