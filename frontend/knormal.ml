open Syntax;;
open Printf;;
(*open Parser;;*)

(*
let addtyp x = (x, Type.gentyp ())
let newid () = addtyp (Id.genid ())
*)
type t =
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
  (*| IfBool of t * t * t*)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }


let last = ref 97
let newvar () = let res = ((String.make 1  (char_of_int !last)), Type.gentyp ()) in incr last; res
let newfct args body = let res = {name = newvar (); args = args; body = body } in res



let rec knormal (ast:Syntax.t) : t =
    match ast with
    |Unit -> Unit
    |Bool a -> Bool a
    |Int a ->  Int a
    |Float a -> Float a

    |Not b -> let (b',t) = newvar () in Let((b',t), knormal b, Not (Var b'))
    |Neg b -> let (b',t) = newvar () in Let((b',t), knormal b, Neg (Var b'))
    |Sub (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b,
                        Sub(Var a', Var b'))
                   )
    |Add (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b,
                        Add(Var a', Var b'))
                   )
    |FAdd (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b,
                        FAdd(Var a', Var b'))
                   )
    |FNeg b -> let (b',t) = newvar () in Let((b',t), knormal b, FNeg (Var b'))
    |FSub (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b,
                        FSub(Var a', Var b'))
                   )
    |FMul (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b,
                        FMul(Var a', Var b'))
                   )
    |FDiv (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b,
                        FDiv(Var a', Var b'))
                   )
    |Eq (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b,
                        Eq (Var a', Var b'))
                   )
    |LE (a, b) -> let (a',t) = newvar () in
                   let (b',t) = newvar () in
                   Let((a',t), knormal a,
                    Let((b',t), knormal b,
                        LE (Var a', Var b'))
                   )

    |Var a -> Var a
    |App (a,b) ->  let (f,t) = newvar () in
                    let rec aux vars_rem k_vars =
                        match vars_rem with
                        |[] -> App(Var(f), List.rev k_vars)
                        |h::q -> let (x,t) = newvar () in Let((x,t), knormal h, aux q ((Var x)::k_vars))
                    in
                    Let((f, t), knormal a, aux b [])

    (*tmp*)
    |If (a, b, c) -> IfEq (knormal a, knormal b, knormal c) (*TODO*)
    |Tuple a -> Tuple(List.map knormal a)
    |LetTuple (a, b, c) -> LetTuple (a, knormal b, knormal c)
    |Array (a, b) -> Array (knormal a, knormal b)
    |Get (a, b) -> Get (knormal a, knormal b)
    |Put (a, b, c) -> Put (knormal a, knormal b, knormal c)
    (*/tmp*)
    |Let (a, b, c) -> Let (a, knormal b, knormal c) (*OK*)
    (*tmp*)
    |LetRec (a, b) ->  LetRec ({name=a.name; args=a.args; body=(knormal a.body)}, knormal b)
    (*/tmp*)

let rec k_to_string (exp:t) : string =
    match exp with
  | Unit -> "()"
  | Bool b -> if b then "true" else "false"
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f

  | Not e -> sprintf "(not %s)" (k_to_string e)
  | Neg e -> sprintf "(- %s)" (k_to_string e)
  | Add (e1, e2) -> sprintf "(%s + %s)" (k_to_string e1) (k_to_string e2)
  | Sub (e1, e2) -> sprintf "(%s - %s)" (k_to_string e1) (k_to_string e2)
  | FNeg e -> sprintf "(-. %s)" (k_to_string e)
  | FAdd (e1, e2) -> sprintf "(%s +. %s)" (k_to_string e1) (k_to_string e2)
  | FSub (e1, e2) -> sprintf "(%s -. %s)" (k_to_string e1) (k_to_string e2)
  | FMul (e1, e2) -> sprintf "(%s *. %s)" (k_to_string e1) (k_to_string e2)
  | FDiv (e1, e2) -> sprintf "(%s /. %s)" (k_to_string e1) (k_to_string e2)
  | Eq (e1, e2) -> sprintf "(%s = %s)" (k_to_string e1) (k_to_string e2)
  | LE (e1, e2) -> sprintf "(%s <= %s)" (k_to_string e1) (k_to_string e2)
  | IfEq (e1, e2, e3) ->
          sprintf "(if %s then %s else %s)" (k_to_string e1) (k_to_string e2) (k_to_string e3)
  | IfLE (e1, e2, e3) ->
          sprintf "(if %s then %s else %s)" (k_to_string e1) (k_to_string e2) (k_to_string e3)
  | Let ((id,t), e1, e2) ->
          sprintf "(let %s = %s in %s)" (Id.to_string id) (k_to_string e1) (k_to_string e2)
  | Var id -> Id.to_string id
  | App (e1, le2) -> sprintf "(%s %s)" (k_to_string e1) (infix_to_string k_to_string le2 " ")
  | LetRec (fd, e) ->
          sprintf "(let rec %s %s = %s in %s)"
          (let (x, _) = fd.name in (Id.to_string x))
          (infix_to_string (fun (x,_) -> (Id.to_string x)) fd.args " ")
          (k_to_string fd.body)   (*CHANGE LATER*)
          (k_to_string e)
  | LetTuple (l, e1, e2)->
          sprintf "(let (%s) = %s in %s)"
          (infix_to_string (fun (x, _) -> Id.to_string x) l ", ")
          (k_to_string e1)
          (k_to_string e2)
  | Get(e1, e2) -> sprintf "%s.(%s)" (k_to_string e1) (k_to_string e2)
  | Put(e1, e2, e3) -> sprintf "(%s.(%s) <- %s)"
                 (k_to_string e1) (k_to_string e2) (k_to_string e3)
  | Tuple(l) -> sprintf "(%s)" (infix_to_string k_to_string l ", ")
  | Array(e1,e2) -> sprintf "(Array.create %s %s)"
       (k_to_string e1) (k_to_string e2)
