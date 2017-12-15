type t =
(* uncomment those lines when ready to create closures *)
    (*| LetClosure of (Id.t * Type.t) * (Id.l * Type.t) * t list*)
    (* | AppD of (Id.t * t list) *)
    (*| AppC of (Id.l * t list)*)
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
    | IfEq of t * t * t
    | IfLE of t * t * t
    | IfBool of t * t * t
    | Let of (Id.t * Type.t) * t * t
    | Var of Id.t
    | LetRec of Syntax.fundef * t
    | Tuple of t list
    | LetTuple of (Id.t * Type.t) list * t * t
    | Array of t * t
    | Get of t * t
    | Put of t * t * t
and fundef = {
                name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t
            }

(*type prog = Prog of fundef list * t*)

(* type toplevel = fundef list *)


(* Nested letrec have not been unnested yet (in reduction) *)
let rec clos (k:Knormal.t) :t = match k with
(*This is not needed for the first version of closure we consider : there is no higher-order function*)
    (* | LetRec (f, a, b) -> (match a with
        | LetRec  (y, a2, b2) -> clos (LetRec (y, clos a2, (clos (LetRec (x, b2, b)))))
        | _ -> LetRec (x, clos a, clos b)) *)
    (* | App (f, l) ->
        let rec clos_args l = match l with
            | [] -> []
            | t::q -> (clos t)::(clos_args q)
        in AppD (f, clos_args l) *)
    |Unit -> Unit
    |Bool a -> Bool a
    |Int a ->  Int  a
    |Float a -> Float a
    |Not b -> Not (clos b)
    |Neg b -> Neg (clos b)
    |Sub (a, b) -> Sub (clos a, clos b)
    |Add (a, b) -> Add (clos a, clos b)
    |FAdd (a, b) -> FAdd (clos a, clos b)
    |FNeg b -> FNeg (clos b)
    |FSub (a, b) -> FSub (clos a, clos b)
    |FMul (a, b) -> FMul (clos a, clos b)
    |FDiv (a, b) -> FDiv (clos a, clos b)
    |Eq (a, b) -> Eq (clos a, clos b)
    |LE (a, b) -> LE (clos a, clos b)
    |Var a -> Var a
    (* |App (a,b) -> AppD (clos a, List.map clos b) *)
    |IfEq (a, b, c) -> IfEq (clos a, clos b, clos c)
    |IfLE (a, b, c) -> IfLE (clos a, clos b, clos c)
    (* |IfBool (a, b, c) -> IfBool (clos a, clos b, clos c) *)
    |Tuple a -> Tuple (List.map clos a)
    (* |LetTuple (a, b, c) -> LetTuple (clos a, clos b, clos c) *)
    |Array (a, b) -> Array (clos a, clos b)
    |Get (a, b) -> Get (clos a, clos b)
    |Put (a, b, c) -> Put (clos a, clos b, clos c)
    (*/tmp*)
    |Let (a, b, c) -> Let (a, clos b, clos c) (*OK*)
    (*tmp*)
    (* |LetRec (a, b) ->  LetClosure ({name=a.name; args=a.args; body=(knormal a.body)}, knormal b) *)
    (*/tmp*)

(* let rec clos_first k = match k with
    | -> Toplevel (clos l) *)
