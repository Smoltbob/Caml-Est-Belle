type t =
  | IDENT
  | LABEL of t option ref
  | Int 
  | Float
  | Var of t option ref
  | EOF
 
let gentyp () = Var(ref None) 
