type t = string

(* type l = string (* type for label *)*)

let to_string x = x

let genid =
  let counter = ref (-1) in
  fun () ->
    incr counter;
    Printf.sprintf "?v%d" !counter

(* let genlabel =
    let counter = ref (-1) in
    fun () ->
        incr counter;
        Printf.sprintf "?v%d" !counter *)
