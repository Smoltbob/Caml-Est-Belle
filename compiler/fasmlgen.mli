(** This module contains functions which outputs a string from a Fclosure.t or outputs a Bsyntax.toplevel used as an input by the backend *)
open Printf;;
open Fclosure;;


(** This function takes care of the base cases such as sums and variables.
@param t is a Fclosure.t
@return a Bsyntax.t *)
val asml_t_triv : Fclosure.t -> Bsyntax.t
(** This function this is a recursive function on Let, AppD and (LetRec TBA). It calls asml_t_triv when it encounters a simple case that ends the recursion like a sum.
@param c is an Fclosure.t
@return an Bsyntax.asmt*)
val asml_exp : Fclosure.t -> Bsyntax.asmt
(** Temporary function to correctly return a Bsyntax.toplevel *)
val asml_head : Fclosure.t -> Bsyntax.toplevel

(** Temporary function to print the starting let _ = at the beginning of the asml file *)
val closure_to_asmlstring_main : Fclosure.t -> string
(** This function is used to output the string to generate the asml file.
@param exp is an Fclosure.t*)
val closure_to_asmlstring : Fclosure.t -> string
