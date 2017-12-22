(** This module encapslates the K-normalization step.*)
open Fsyntax;;
open Printf;;
(*open Fparser;;*)

(*
let addtyp x = (x, Ftype.gentyp ())
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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  (*| IfBool of t * t * t*)
  | Let of (Id.t * Ftype.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Ftype.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Ftype.t; args : (Id.t * Ftype.t) list; body : t }

(** Temporary solution to generate new labels. Revise later.*)
let last = ref 0
(*let newvar () = let res = ((String.make 1  (char_of_int !last)), Ftype.gentyp ()) in incr last; res*)
let newvar () = let res = ("v"^(Printf.sprintf "%d" !last), Ftype.gentyp ()) in incr last; res
(**Unused.*)
let newfct args body = let res = {name = newvar (); args = args; body = body } in res


(*K-normalization. Applied to ast, return a flatter version of it: aside from let and letrec, all constructs will have a bounded depth.
@param ast Abstract syntax Tree of a general mincaml program
@return New K-normalized AST*)
let rec knormal (ast:Fsyntax.t) : t =
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
    |App (a,b) ->  ( match a with
                    |Var(fct) -> (  (*a temporary solution to prevent functions from being renamed*)
                        let rec aux vars_rem k_vars =
                            match vars_rem with
                            |[] -> App(Var("min_caml_"^fct), List.rev k_vars)
                            |h::q -> let (x,t) = newvar () in Let((x,t), knormal h, aux q ((Var x)::k_vars))
                        in
                        aux b []
                       )
                        

                    |_ -> ( (*When constant propgation is implemnted, only this mechanism should remain*)
                        let (f,t) = newvar () in
                        let rec aux vars_rem k_vars =
                            match vars_rem with
                            |[] -> App(Var(f), List.rev k_vars)
                            |h::q -> let (x,t) = newvar () in Let((x,t), knormal h, aux q ((Var x)::k_vars))
                        in
                        Let((f, t), knormal a, aux b [])
                        )
                  )

    |If (a, b, c) ->(match a with
                     | LE(x, y) -> let (x',t) = newvar () in
                                    let (y',t) = newvar () in
                                    Let((x',t), knormal x,
                                    Let((y',t), knormal y,
                                    IfLE(x', y', knormal b, knormal c)))

                     | Eq(x, y) ->  let (x',t) = newvar () in
                                    let (y',t) = newvar () in
                                    Let((x',t), knormal x,
                                    Let((y',t), knormal y,
                                    IfEq(x', y', knormal b, knormal c)))
                     | Not(x) -> knormal (If(x, c, b))

                     | _ ->  let (x',t) = newvar () in
                             let (y',t) = newvar () in
                             Let((x',t), knormal a,
                              Let((y',t), Bool(true),
                               IfEq(x', y', knormal b, knormal c)))
                    )
    |Let (a, b, c) -> Let (a, knormal b, knormal c) (*OK*)
    |LetRec (a, b) ->  LetRec ({name=("min_caml_"^(fst a.name), snd a.name); args=a.args; body=(knormal a.body)}, knormal b)
    |_ -> failwith "knormal: NotImplementedYet"
    (*
    |Tuple a -> Tuple(List.map knormal a)
    |LetTuple (a, b, c) -> LetTuple (a, knormal b, knormal c)
    |Array (a, b) -> Array (knormal a, knormal b)
    |Get (a, b) -> Get (knormal a, knormal b)
    |Put (a, b, c) -> Put (knormal a, knormal b, knormal c)
    *)

(** Produces a string out of a K-normalized ast
@param exp t
@return string*)
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
  | IfEq (x, y, e2, e3) ->
          sprintf "(if %s=%s then %s else %s)" (Id.to_string x) (Id.to_string y) (k_to_string e2) (k_to_string e3)
  | IfLE (x, y, e2, e3) ->
          sprintf "(if %s <= %s then %s else %s)" (Id.to_string x) (Id.to_string y) (k_to_string e2) (k_to_string e3)
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
  (* | _ -> failwith "fknormal matchfailure" *)
