(** This module make sure that all the bound variables
  (variables defined by a let or functions parameters) are different.
*)

(**
  a counter to create a unique variable names
*)
val ctr : int ref

(**
  this function is to remove last tuple from alphaMap
*)
val pop : unit -> unit

(**
  this function is to add new pair (old name, new name) to alphaMap
  @param x tuple(old name, new name)
*)
val push : Id.t*Id.t -> unit


(**
  this fuction is to parse the AST and create a new and unique name for each variable
  @param env environment
  @param k_t:Fknormal.t the knormalized AST
  @return  AST with new names
*)
val alpha_g : (Id.t * Id.t) list -> Fknormal.t -> Fknormal.t
val alpha : Fknormal.t -> Fknormal.t
