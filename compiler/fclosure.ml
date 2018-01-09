(** This module will unnest the letrecs in the next versions. For now it only transforms a Fknormal.t into a Fclosure.t
The function returning a string is just a dbeugging function for now *)
open Fknormal;;
open Fsyntax;;
open Printf;;


type t =
(* uncomment those lines when ready to create closures *)
    (*| LetFclosure of (Id.t * Ftype.t) * (Id.l * Ftype.t) * t list*)
    (*| AppC of (Id.l * t list)*)
    | AppD of (Id.t * t list)
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
    | IfBool of t * t * t
    | Let of (Id.t * Ftype.t) * t * t
    | Var of Id.t
    | LetRec of fundef * t
    | Tuple of t list
    | LetTuple of (Id.t * Ftype.t) list * t * t
    | Array of t * t
    | Get of t * t
    | Put of t * t * t
and fundef = {
                name : Id.t * Ftype.t;
                args : (Id.t * Ftype.t) list;
                formal_fv : (Id.t * Ftype.t) list;
                body : t
            }

(*type prog = Prog of fundef list * t*)

(* type toplevel = fundef list *)

(** This function transform unnested expressions into a Fclosure.t *)
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
    | IfEq (x, y, b, c) -> IfEq (x, y, clos_exp b, clos_exp c)
    | IfLE (x, y, b, c) -> IfLE (x, y, clos_exp b, clos_exp c)
    (* |IfBool (a, b, c) -> IfBool (clos_exp a, clos_exp b, clos_exp c) *)
    | Tuple a -> Tuple (List.map clos_exp a)
    (* |LetTuple (a, b, c) -> LetTuple (clos_exp a, clos_exp b, clos_exp c) *)
    | Array (a, b) -> Array (clos_exp a, clos_exp b)
    | Get (a, b) -> Get (clos_exp a, clos_exp b)
    | Put (a, b, c) -> Put (clos_exp a, clos_exp b, clos_exp c)
    (* TODO remove let later *)
    | Let (x, a, b) -> Let (x, clos_exp a, clos_exp b)
    | LetRec (fundef, a) -> LetRec ({name = fundef.name; args = fundef.args; formal_fv = []; body = (clos_exp fundef.body)}, (clos_exp a))
    | App (f, l) -> (match f with
                        | (Var id) -> AppD ("_"^id, List.map clos_exp l)
                        | _ -> failwith "matchfailure App")
    | _-> failwith "match not exhaustive in clos_exp fclosure.ml"

(* Ye Olde Buggy Version
let rec the_savage_phi (fb:Fknormal.t) : (Fknormal.fundef option * Fknormal.t)  =
    match fb with
    |Let(a,b,c) -> (
                    let recbody, newin = the_savage_phi c in
                    match recbody with
                    |None -> (None,  Let(a, b, newin))
                    |Some fbody -> (Some fbody, Let(a,b, newin))
                    )
    |LetRec(fbody, recin) -> (Some fbody, recin)
    |_ -> (None, fb)

and the_cunning_psi (ast : Fknormal.t) : Fknormal.t =
    match ast with
    |LetRec(fd, een) -> (
                        let recbody, newin = the_savage_phi fd.body in
                        match recbody with
                        |None -> LetRec(fd, een)
                        |Some fbody -> the_cunning_psi ( LetRec(fbody, the_cunning_psi (LetRec({name=fd.name; args=fd.args; body=newin}, een))))
                        )
    |Let(a,b,c) -> Let(a, the_cunning_psi b, the_cunning_psi c)
    |_ -> ast
*)

(*Ye new version*)
(*
let rec the_savage_phi (fb:Fknormal.t) : (Fknormal.fundef option * Fknormal.t)  =
    match fb with
    |Let(a,b,c) -> (
                    let recbody, newletbody = the_savage_phi b in
                    match recbody with
                    |None -> (let recbody', newin = the_savage_phi c in
                              match recbody' with
                              |None -> (None,  Let(a, b, c))
                              |Some fbody -> (Some fbody, Let(a,b, newin))
                             )
                    |Some fbody -> (Some fbody, Let(a, newletbody, c))
                   )
    |LetRec(fbody, recin) -> (Some fbody, recin)
    |IfEq(x, y, a, b) -> (let arecbody, anewbody = the_savage_phi a in
                        match arecbody with
                        |None ->  (let brecbody, bnewbody = the_savage_phi b in
                                    match brecbody with
                                    |None -> (None,  IfEq(x, y, a, b))
                                    |Some fbody -> (Some fbody, IfEq(x, y, a, bnewbody))
                                  )
                        |Some fbody -> (Some fbody, IfEq(x, y, anewbody, b))
                       )
    |IfLE(x, y, a, b) -> (let arecbody, anewbody = the_savage_phi a in
                        match arecbody with
                        |None ->  (let brecbody, bnewbody = the_savage_phi b in
                                    match brecbody with
                                    |None -> (None,  IfLE(x, y, a, b))
                                    |Some fbody -> (Some fbody, IfLE(x, y, a, bnewbody))
                                  )
                        |Some fbody -> (Some fbody, IfLE(x, y, anewbody, b))
                       )
    |_ -> (None, fb)

and the_cunning_psi (ast : Fknormal.t) : Fknormal.t =
    match ast with
    |LetRec(fd, een) -> (
                        let recbody, newin = the_savage_phi fd.body in
                        match recbody with
                        |None -> LetRec(fd, the_cunning_psi een)
                        |Some fbody -> the_cunning_psi ( LetRec(fbody, (LetRec({name=fd.name; args=fd.args; body=newin}, een))))
                        )
    |Let(a,b,c) -> ( 
                    let recbody, newin = the_savage_phi b in
                    match recbody with
                        |None -> Let(a, b, the_cunning_psi c)
                        |Some fbody -> the_cunning_psi ( LetRec(fbody, Let(a, newin, c)))
                   )
    |_ -> ast
*)

let rec subphi cat (y:Fknormal.t) (z:Fknormal.t) lr : Fknormal.t = 
    match (phi y) with
    |LetRec(a, b) -> phi (LetRec(a, cat b z))
    |_ -> (match (phi z) with
            |LetRec(a, b) when lr -> cat y (phi (LetRec(a, b)))
            |LetRec(a, b) -> phi (LetRec(a, cat y b))
            |_ -> cat y z)
and phi (ast:Fknormal.t) : Fknormal.t = 
    match ast with
    |Let(x,y,z) -> subphi (fun a->fun b->Let(x, a, b)) y z false
    |LetRec(y,z) -> subphi (fun a->fun b->LetRec({name=y.name; args=y.args; body=a}, b)) y.body z true
    |IfEq(u,v,y,z) -> subphi (fun a->fun b->IfEq(u,v,a,b)) y z false
    |IfLE(u,v,y,z) -> subphi (fun a->fun b->IfLE(u,v,a,b)) y z false
    |_ -> ast

(*not really needed anymore*)
let rec letrecs_at_top clos = match clos with
    | LetRec (f, een) -> LetRec (f, letrecs_at_top een)
    | Let (id, a, een) -> letrecs_at_top (een)
    | _ -> Unit

let rec lets_at_bot clos = match clos with
    | Let (id, a, een) -> Let (id, a, lets_at_bot een)
    | LetRec (f, een) -> lets_at_bot (een)
    | _ -> clos

let rec merge_letrecs_lets letrecs lets = match letrecs with
    | LetRec (f, een) -> LetRec (f, merge_letrecs_lets een lets)
    | Unit -> lets
    | _ -> failwith "merge_letrecs_lets: error matchfailure"

(*and chi fd = fd*)

let clos_out k =
    (*let clos = (clos_exp (the_cunning_psi k)) in*)
    let clos = (clos_exp (phi k)) in
    merge_letrecs_lets (letrecs_at_top clos) (lets_at_bot clos) 


(** This function is for debugging purpose only, it returns its argument as a string *)
let rec clos_to_string (c:t) : string =
    match c with
  | Unit -> "()"
  | Bool b -> if b then "true" else "false"
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Not e -> sprintf "(not %s)" (clos_to_string e)
  | Neg e -> sprintf "(- %s)" (clos_to_string e)
  | Add (e1, e2) -> sprintf "(%s + %s)" (clos_to_string e1) (clos_to_string e2)
  | Sub (e1, e2) -> sprintf "(%s - %s)" (clos_to_string e1) (clos_to_string e2)
  | FNeg e -> sprintf "(-. %s)" (clos_to_string e)
  | FAdd (e1, e2) -> sprintf "(%s +. %s)" (clos_to_string e1) (clos_to_string e2)
  | FSub (e1, e2) -> sprintf "(%s -. %s)" (clos_to_string e1) (clos_to_string e2)
  | FMul (e1, e2) -> sprintf "(%s *. %s)" (clos_to_string e1) (clos_to_string e2)
  | FDiv (e1, e2) -> sprintf "(%s /. %s)" (clos_to_string e1) (clos_to_string e2)
  | Eq (e1, e2) -> sprintf "(%s = %s)" (clos_to_string e1) (clos_to_string e2)
  | LE (e1, e2) -> sprintf "(%s <= %s)" (clos_to_string e1) (clos_to_string e2)
  | IfEq (x, y, e2, e3) ->
          sprintf "(if %s=%s then %s else %s)" (Id.to_string x) (Id.to_string y) (clos_to_string e2) (clos_to_string e3)
  | IfLE (x, y, e2, e3) ->
          sprintf "(if %s <= %s then %s else %s)" (Id.to_string x) (Id.to_string y) (clos_to_string e2) (clos_to_string e3)
  | Let ((id,t), e1, e2) ->
          sprintf "(let %s = %s in %s)" (Id.to_string id) (clos_to_string e1) (clos_to_string e2)
  | Var id -> Id.to_string id
  | AppD (e1, le2) -> sprintf "(%s %s)" (Id.to_string e1) (infix_to_string clos_to_string le2 " ")
  | LetRec (fd, e) ->
          sprintf "(let rec %s %s = %s in %s)"
          (let (x, _) = fd.name in (Id.to_string x))
          (infix_to_string (fun (x,_) -> (Id.to_string x)) fd.args " ")
          (clos_to_string fd.body)
          (clos_to_string e)
  | LetTuple (l, e1, e2)->
          sprintf "(let (%s) = %s in %s)"
          (infix_to_string (fun (x, _) -> Id.to_string x) l ", ")
          (clos_to_string e1)
          (clos_to_string e2)
  | Get(e1, e2) -> sprintf "%s.(%s)" (clos_to_string e1) (clos_to_string e2)
  | Put(e1, e2, e3) -> sprintf "(%s.(%s) <- %s)"
                 (clos_to_string e1) (clos_to_string e2) (clos_to_string e3)
  | Tuple(l) -> sprintf "(%s)" (infix_to_string clos_to_string l ", ")
  | Array(e1,e2) -> sprintf "(Array.create %s %s)"
       (clos_to_string e1) (clos_to_string e2)
   | _-> "NotYetImplemented"
