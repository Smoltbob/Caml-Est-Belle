(** This module contains functions which outputs a string from a Fclosure.t or outputs a Bsyntax.toplevel used as an input by the backend *)
open Printf;;
open Fclosure;;
open Bsyntax;;

(** Transforms a list of args from fclosure into a list of args of bsyntax
    @param l is a Fclosure.t list
    @return formal_args *)
val to_fargs : Fclosure.t list -> Id.t list

(** Creates the memory access lines to retrieve the free variables from memory and appends call at the end.
    @param name is an Id.t
    @param fv is the list of free variables
    @param count is used to count the offset for the variables
    @param call is an asmt
    @return asmt *)
val mem_fv_letrec : Id.t -> Id.t list -> int -> Bsyntax.asmt -> Bsyntax.asmt

(** Creates the memory affectation lines to put the free variables in memory and appends call at the end of the instructions.
    @param name is an Id.t
    @param fv is the list of free variables
    @param count is used to count the offset for the variables
    @param call is an asmt
    @return asmt *)
val mem_fv_closure : Id.t -> Id.t list -> int -> Bsyntax.asmt -> Bsyntax.asmt

(** This function takes care of the base cases such as sums and variables.
@param t is a Fclosure.t
@return a Bsyntax.t *)
val asml_t_triv : Fclosure.t -> Bsyntax.t

(** This function is a recursive function on Let, AppD and (LetRec TBA). It calls asml_t_triv when it encounters a simple case that ends the recursion like a sum.
@param c is an Fclosure.t
@return an Bsyntax.asmt*)
val asml_exp : Fclosure.t -> Bsyntax.asmt

(** Creates the "let _" main and add asml_exp c to it's body to use the types defined in bsyntax.
    @param c is an Fclosure.t
    @return a fundef*)
val create_main : Fclosure.t -> Bsyntax.fundef

(** Executes asml_exp on the body of the function definitions and adds the memory access to the free variable if any.
The closure starts with letrecs thanks to the previous unnesting. Anything that isn't a letrec is added to the main let using create_main.
    @param c is a Fclosure.t
    @return a fundef list*)
val asml_list : Fclosure.t -> Bsyntax.fundef list

(** Create a toplevel containing asml_list to c
    @param c Fclosure.t
    @return toplevel*)
val asml_head : Fclosure.t -> Bsyntax.toplevel

(** Creates a string from a simple expressions of type Bsyntax.t recursively and applies asmt_to_string to asmts
    @param exp is a Bsyntax.t
    @return a string*)
val expression_to_string : Bsyntax.t -> string

(** Creates a string from an asmt of type Bsyntax.asmt recursively and applies expression_to_string to simple expressions of types Bsyntax.t
    @param a is an asmt
    @return a string*)
val asmt_to_string : Bsyntax.asmt -> string

(** Creates a string from a fundef and applies asmt_to_string to asmts
    @param fund is a fundef
    @return a string*)
val fundef_to_string : Bsyntax.fundef -> string

(** Creates a string from a fundef list
    @param l is a fundef list
    @return a string*)
val list_to_string : Bsyntax.fundef list -> string

(** Matches a toplevel to return the string of a fundef list
    @param toplvl is a toplevel
    @return a string*)
val toplevel_to_string : Bsyntax.toplevel -> string
