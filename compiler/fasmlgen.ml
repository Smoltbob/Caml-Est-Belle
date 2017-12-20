open Fclosure;;
open Printf;;
open Bsyntax;; (* Type for the asml file *)

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

and asmt =
    | Let of Fid.t * t * asmt
    | Expression of t
    (* | Additional case for parenthesis ? Don't think so ? *)

and fundef =
    | Body of asmt (* We will need the name, arguments and return type for functions *)

type toplevel =
    | Fundefs of (fundef list) (* Once we implement functions we will have a list *) *)


let rec asml_t_triv t = match t with
    | Unit -> Nop
    | Int a -> Int a
    | Float a -> Float a
    | Neg x -> (match x with (Var y) -> Neg y)
    | FNeg x -> (match x with (Var y) -> Fneg y)
    | FSub (x, y) -> (match x, y with (Var x2, Var y2) -> Fsub (x2, y2))
    | FAdd (x, y) -> (match x, y with (Var x2, Var y2) -> Fadd (x2, y2))
    | FMul (x, y) -> (match x, y with (Var x2, Var y2) -> Fmul (x2, y2))
    | FDiv (x, y) -> (match x, y with (Var x2, Var y2) -> Fdiv (x2, y2))
    | Add (x, a) -> (match x with (Var y) -> match a with (Var z) -> Add (y, z))
    | Sub (x, a) -> (match x with (Var y) -> match a with (Var z) -> Sub (y, z))
    | Var x -> Var x


let rec asml_exp (c:Fclosure.t) :asmt = match c with
    | Let (x, a, b) -> Let (fst x, asml_t_triv a, asml_exp b)
    (* | _ -> Expression asml_t_triv c *)
    | Unit -> Expression Nop
    | Int a -> Expression (Int a)
    | Float a -> Expression (Float a)
    | Neg x -> (match x with (Var y) -> Expression (Neg y))
    | FNeg x -> (match x with (Var y) -> Expression (Fneg y))
    | FSub (x, y) -> (match x, y with (Var x2, Var y2) -> Expression (Fsub (x2, y2)))
    | FAdd (x, y) -> (match x, y with (Var x2, Var y2) -> Expression (Fadd (x2, y2)))
    | FMul (x, y) -> (match x, y with (Var x2, Var y2) -> Expression (Fmul (x2, y2)))
    | FDiv (x, y) -> (match x, y with (Var x2, Var y2) -> Expression (Fdiv (x2, y2)))
    | Add (x, a) -> (match x with (Var y) -> match a with (Var z) -> Expression (Add (y, z)))
    | Sub (x, a) -> (match x with (Var y) -> match a with (Var z) -> Expression (Sub (y, z)))
    | Var x -> Expression (Var x)
    | Eq (x, a) -> (match x with (Var y) -> Expression (Eq (y, asml_t_triv a)))

(* let rec asml_asmt c = match c with
    | Let (x, a, asmt) -> Let (fst x, asml_exp a, asml_asmt asmt)
    | Expression exp -> Expression (asml_exp exp) *)

let asml_head c =
    Fundefs [Body (asml_exp c)]

let rec closure_to_asmlstring (exp:Fclosure.t) : string = match exp with
  | Unit -> "nop"
  (* | Bool b -> if b then "true" else "false" *)
  | Int i -> string_of_int i
  | Var id -> Fid.to_string id
  (* | Let (x, a, b) -> sprintf *)
  | Add (e1, e2) -> sprintf "add %s %s \n" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | Sub (e1, e2) -> sprintf "sub %s %s \n" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | Let (x, a, b) -> sprintf "let %s = %s in \n %s" (fst x) (closure_to_asmlstring a) (closure_to_asmlstring b)
  | _ -> ""
(*
  | Not e -> sprintf "(not %s)" (closure_to_asmlstring e)
  | Neg e -> sprintf "(- %s)" (closure_to_asmlstring e)
  | Float f -> sprintf "%.2f" f
  | FNeg e -> sprintf "(-. %s)" (closure_to_asmlstring e)
  | FAdd (e1, e2) -> sprintf "(%s +. %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | FSub (e1, e2) -> sprintf "(%s -. %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | FMul (e1, e2) -> sprintf "(%s *. %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | FDiv (e1, e2) -> sprintf "(%s /. %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | Eq (e1, e2) -> sprintf "(%s = %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | LE (e1, e2) -> sprintf "(%s <= %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2) *)
  (* | IfEq (e1, e2, e3) ->
          sprintf "(if %s then %s else %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2) (closure_to_asmlstring e3)
  | IfLE (e1, e2, e3) ->
          sprintf "(if %s then %s else %s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2) (closure_to_asmlstring e3)
  | Let ((id,t), e1, e2) ->
          sprintf "(let %s = %s in %s)" (Fid.to_string id) (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | App (e1, le2) -> sprintf "(%s %s)" (closure_to_asmlstring e1) (infix_to_string closure_to_asmlstring le2 " ")
  | LetRec (fd, e) ->
          sprintf "(let rec %s %s = %s in %s)"
          (let (x, _) = fd.name in (Fid.to_string x))
          (infix_to_string (fun (x,_) -> (Fid.to_string x)) fd.args " ")
          (closure_to_asmlstring fd.body)   (*CHANGE LATER*)
          (closure_to_asmlstring e)
  | LetTuple (l, e1, e2)->
          sprintf "(let (%s) = %s in %s)"
          (infix_to_string (fun (x, _) -> Fid.to_string x) l ", ")
          (closure_to_asmlstring e1)
          (closure_to_asmlstring e2)
  | Get(e1, e2) -> sprintf "%s.(%s)" (closure_to_asmlstring e1) (closure_to_asmlstring e2)
  | Put(e1, e2, e3) -> sprintf "(%s.(%s) <- %s)"
                 (closure_to_asmlstring e1) (closure_to_asmlstring e2) (closure_to_asmlstring e3)
  | Tuple(l) -> sprintf "(%s)" (infix_to_string closure_to_asmlstring l ", ")
  | Array(e1,e2) -> sprintf "(Array.create %s %s)"
       (closure_to_asmlstring e1) (closure_to_asmlstring e2) *)


let closure_to_asmlstring_main (exp:Fclosure.t) : string =
   sprintf "let _ = \n %s" (closure_to_asmlstring exp)
