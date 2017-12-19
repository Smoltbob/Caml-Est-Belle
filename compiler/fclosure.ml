type t =
(* uncomment those lines when ready to create closures *)
    (*| LetFclosure of (Fid.t * Ftype.t) * (Fid.l * Ftype.t) * t list*)
    (* | AppD of (Fid.t * t list) *)
    (*| AppC of (Fid.l * t list)*)
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | Not of t
    | Neg of t
    | Add of t * t
    | Sub of t * t
    | FNeg of t
    | FAdd of t * t
    | FSub of t * t
    | FMul of t * t
    | FDiv of t * t
    | Eq of t * t
    | LE of t * t
    | IfEq of Fid.t * Fid.t * t * t
    | IfLE of Fid.t * Fid.t * t * t
    | IfBool of t * t * t
    | Let of (Fid.t * Ftype.t) * t * t
    | Var of Fid.t
    | LetRec of fundef * t
    | Tuple of t list
    | LetTuple of (Fid.t * Ftype.t) list * t * t
    | Array of t * t
    | Get of t * t
    | Put of t * t * t
and fundef = {
                name : Fid.t * Ftype.t;
                args : (Fid.t * Ftype.t) list;
                formal_fv : (Fid.t * Ftype.t) list;
                body : t
            }

(*type prog = Prog of fundef list * t*)

(* type toplevel = fundef list *)

let rec clos_exp (k:Fknormal.t) :t = match k with
    | Unit -> Unit
    | Bool a -> Bool a
    | Int a ->  Int  a
    | Float a -> Float a
    | Not b -> Not (clos_exp b)
    | Neg b -> Neg (clos_exp b)
    | Sub (a, b) -> Sub (clos_exp a, clos_exp b)
    | Add (a, b) -> Add (clos_exp a, clos_exp b)
    | FAdd (a, b) -> FAdd (clos_exp a, clos_exp b)
    | FNeg b -> FNeg (clos_exp b)
    | FSub (a, b) -> FSub (clos_exp a, clos_exp b)
    | FMul (a, b) -> FMul (clos_exp a, clos_exp b)
    | FDiv (a, b) -> FDiv (clos_exp a, clos_exp b)
    | Eq (a, b) -> Eq (clos_exp a, clos_exp b)
    | LE (a, b) -> LE (clos_exp a, clos_exp b)
    | Var a -> Var a
    (* |App (a,b) -> AppD (clos_exp a, List.map clos_exp b) *)
    | IfEq (x, y, b, c) -> IfEq (x, y, clos_exp b, clos_exp c)
    | IfLE (x, y, b, c) -> IfLE (x, y, clos_exp b, clos_exp c)
    (* |IfBool (a, b, c) -> IfBool (clos_exp a, clos_exp b, clos_exp c) *)
    | Tuple a -> Tuple (List.map clos_exp a)
    (* |LetTuple (a, b, c) -> LetTuple (clos_exp a, clos_exp b, clos_exp c) *)
    | Array (a, b) -> Array (clos_exp a, clos_exp b)
    | Get (a, b) -> Get (clos_exp a, clos_exp b)
    | Put (a, b, c) -> Put (clos_exp a, clos_exp b, clos_exp c)
    (*/tmp*)

(* Nested letrec have not been unnested yet (in reduction) *)
let rec clos (k:Fknormal.t) :t = match k with
(* We now consider that there are no free variable inside our nested letrecs *)
    | LetRec (fundef, t) ->
        let (fname, fargs, fbody) = (fundef.name, fundef.args, fundef.body) in
            (match fbody with
            | LetRec (fundef2, t2) ->
                let (newfundef : Fknormal.fundef) = {name = fname; args = fargs; body = t2} in
                match (clos (LetRec (fundef2, Unit))) with
                LetRec (f, _) ->
                    let newfundef2 = {name = f.name; args = f.args; formal_fv = []; body = (clos_exp t2)} in
                    LetRec (newfundef2, clos (LetRec (newfundef, t)))
            (* | Let (x, a, b) -> *)
            )
    (* | Let (a, b, c) -> Let (a, clos b, clos c) *)
    (* | App (f, l) ->
        let rec clos_args l = match l with
            | [] -> []
            | t::q -> (clos t)::(clos_args q)
        in AppD (f, clos_args l) *)

(*
let rec clos_toplevel k = match k with
    | -> Toplevel (clos l) *)
