open Printf;;
open Fclosure;;


val closure_to_asmlstring : Fclosure.t -> string

val closure_to_asmlstring_main : Fclosure.t -> string

(* val asml_t_triv : Fclosure.t -> Bsyntax.t *)
(* val asml_exp : Fclosure.t -> Bsyntax.asmt *)
val asml_head : Fclosure.t -> Bsyntax.toplevel
