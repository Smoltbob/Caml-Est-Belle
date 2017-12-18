open Printf;;
open Fclosure;;

(* type t =
  | Int of int
  | Float of float
  | Neg of Fid.t
  | Fneg of Fid.t
  | Fsub of Fid.t * Fid.t
  | Fadd of Fid.t * Fid.t
  | Fmul of Fid.t * Fid.t
  | Fdiv of Fid.t * Fid.t
  | Add of Fid.t * t
  | Sub of Fid.t * t
  | Var of Fid.t
  | Eq of Fid.t * t
  | Nop
  (* NOP ? *)

and asmt =
    | Let of Fid.t * t * asmt
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
