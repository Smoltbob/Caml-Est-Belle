type t = string

(* type for labels *)
type l = string

let to_string x = x

let genid =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    Printf.sprintf "?v%d" !counter
