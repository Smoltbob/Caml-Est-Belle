open Printf;;
open Fclosure;;

(* type t =
  | Int of int
  | Float of float
  | Neg of Id.t
  | Fneg of Id.t
  | Fsub of Id.t * Id.t
  | Fadd of Id.t * Id.t
  | Fmul of Id.t * Id.t
  | Fdiv of Id.t * Id.t
  | Add of Id.t * t
  | Sub of Id.t * t
  | Var of Id.t
  | Eq of Id.t * t
  | Nop
  (* NOP ? *)

and asmt =
    | Let of Id.t * t * asmt
    | Expression of t
    (* | Additional case for parenthesis ? Don't think so ? *)

and fundef =
    | Body of asmt
    (* We will need the name, arguments and return type for functions *)

type toplevel =
    | Fundefs of (fundef list) *)

(* val gen : Fclosure.t -> asmt *)
val closure_to_asmlstring : Fclosure.t -> string

val closure_to_asmlstring_main : Fclosure.t -> string

val asml_t_triv : Fclosure.t -> Bsyntax.t
val asml_exp : Fclosure.t -> Bsyntax.asmt
val asml_head : Fclosure.t -> Bsyntax.toplevel
