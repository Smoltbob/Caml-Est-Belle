open Knormal;;
open Closure;;

let last = ref 97
let newlabel () = let res = ((String.make 1  (char_of_int !last)), Type.gentyp ()) in incr last; res
let newfct args body = let res = {name = newlabel (); args = args; formal_fv = ; body = body} in res

(* Nested letrec have not been unnested yet (in reduction) *)
let rec clos k :Closure.t = match k with
    | LetRec (f, a, b) -> (match a with
        | LetRec (y, a2, b2) -> reduc (LetRec (y, a2, (reduc (LetRec (x, b2, b)))))
        | _ -> LetRec (x, a, reduc b))
    | App (f, l) -> (* This is not needed for the first version of closure we consider : there is no higher-order function*)
        let rec clos_args l = match l with
            | [] -> []
            | t::q -> (clos t)::(clos_args q)
        in App (f, clos_args l)
    | _ -> k

let rec clos_first k = match k with
    | -> Toplevel (l)
