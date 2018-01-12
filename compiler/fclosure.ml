(** This module will unnest the letrecs in the next versions. For now it only transforms a Fknormal.t into a Fclosure.t
The function returning a string is just a dbeugging function for now *)
open Fknormal;;
open Fsyntax;;
open Printf;;



type t =
    | Let of (Id.t * Ftype.t) * t * t
    | LetRec of fundef * t
    | LetCls of Id.t * Id.l * (Id.t list) * t
                 (*name of clsr * lbl of fct * free vars of fct * in*)
    | AppC of (Id.t * t list)
    | AppD of (Id.l * t list)
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
    | Var of Id.t
    | Tuple of t list
    | LetTuple of (Id.t * Ftype.t) list * t * t
    | Array of t * t
    | Get of t * t
    | Put of t * t * t
and fundef = {
                name : Id.l * Ftype.t;
                args : (Id.t * Ftype.t) list;
                formal_fv : (Id.t * Ftype.t) list;
                body : t
            }

(*type prog = Prog of fundef list * t*)

(* type toplevel = fundef list *)

let hash_fundef = Hashtbl.create 10

let rec add_und (fund:fundef) = let (id,typ) = fund.name in
    {name = ("_"^id,typ); args = fund.args; formal_fv = fund.formal_fv; body = scan_fundef fund.body}
and scan_fundef clos :t = match clos with
    | LetRec (fund, een) -> Hashtbl.add hash_fundef (fst fund.name) ();
                                 LetRec (add_und fund, scan_fundef een)
    | Let (x, valu, een) -> Let (x, scan_fundef valu, scan_fundef een)
    | AppD (id, l) ->   if Hashtbl.mem hash_fundef id then
                            AppD ("_"^id, l)
                        else
                            AppD ("_min_caml_"^id, l)
    | AppC (id, l) -> AppC (id, l)
    | _ -> clos

(** This function transform unnested expressions into a"_" Fclosure.t *)
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
    (* TODO remove let later *)(*TODO: Don't remove let*)
    | Let (x, a, b) -> Let (x, clos_exp a, clos_exp b)
    | LetRec (fundef, a) -> LetRec ({name = fundef.name; args = fundef.args; formal_fv = []; body = (clos_exp fundef.body)}, (clos_exp a))
    | App (f, l) -> (match f with
                        | (Var id) -> AppD (id, List.map clos_exp l)
                        | _ -> failwith "matchfailure App")
    | _ -> failwith "match not exhaustive in clos_exp fclosure.ml"

(* Ye Olde Buggy Version
let rec the_savage_phi (fb:Fknormal.t) : (Fknormal.fundef option * Fknormal.t)  =
    match fb with
    |Let(a,b,c) -> (
                    let recbody, newin = the_savage_phi c in
                    match recbody with
                    |None -> (None,  Let(a, b, newin)) ()
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

(*--------THE-VERSION-BEFORE-TRUE-CLOSURE-----------------------------------*)
(*
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
*)
(*--------------------------------------------------------------------------*)


(*------THE-VERSION-AFTER-TRUE-CLOSURE--------------------------------------*)
let closures = Hashtbl.create 10
let known = ref ["print_int"] (*TODO: add the others*)
(* (a try at using sets, but couldn't be bothered to check if comparison works properly. Maybe come back later)
module SS = Set.Make(struct
                        let compare = fun (x,_)->fun (y,_)->Pervasives.compare x y
                        type t = Id.t * Ftype.t
                     end)

let rec make_set l =
    match l with
    |[] -> SS.empty
    |h::q -> SS.add h (make_set q)

let test_var (x:Id.t) bv =
    let t = Ftype.gentyp () in
    if SS.mem (x, t) bv then
        SS.singleton (x, t)
    else
        SS.empty

let test_list l bv =
    let rec test_list_aux y x =
        match x with
        |Var(a) -> SS.union (test_var a bv) y
        |_ -> failwith "find_fv: wrong App"
    in
    List.fold_left test_list_aux SS.empty l

let rec find_fv ast args =
    match ast with
    |Let (a,b,c) -> SS.union (find_fv b args) (find_fv c (SS.add a args))
    |LetRec (a, b) -> SS.union (find_fv a.body (SS.add a.name args)) (find_fv b (SS.add a.name args))
    |Unit -> SS.empty
    |Bool b -> SS.empty
    |Int i -> SS.empty
    |Not e -> SS.empty
    |Add (a, b) -> SS.union (find_fv a args) (find_fv b args)
    |Sub (a, b) -> SS.union (find_fv a args) (find_fv b args)
    |IfEq (x, y, a, b) -> SS.union (test_var x args) (SS.union (test_var y args) (SS.union (find_fv a args) (find_fv b args)))
    |IfLE (x, y, a, b) -> SS.union (test_var x args) (SS.union (test_var y args) (SS.union (find_fv a args) (find_fv b args)))
    |Var id -> test_var id args
    |AppD (a, b) -> SS.union (test_var a args) (test_list b args)
    | _-> failwith "Fclosure:find_fv NotYetImplemented"
*)


let rec memv x l =
    match l with
    |[] -> false
    |(y,_)::q -> if y=x then true else memv x q

let rec union l k =
    match l with
    |[] -> k
    |h::q -> if (memv (fst h) k) then (union q k) else (h::(union q k))

let test_var (x:Id.t) bv =
    let t = Ftype.gentyp () in
    if memv x bv then
        []
    else
        [(x, t)]

let test_list l bv =
    let rec test_list_aux y x =
        match x with
        |Var a -> union (test_var a bv) y
        |_ -> failwith "find_fv->test_list: wrong App"
    in
    List.fold_left test_list_aux [] l

let rec find_fv ast args =
    match ast with
    |Let (a,b,c) -> union (find_fv b args) (find_fv c (a::args))
    |LetRec (a, b) -> union (find_fv a.body (a.name::args)) (find_fv b (a.name::args))
    |Unit -> []
    |Bool b -> []
    |Int i -> []
    |Not e -> []
    |Add (a, b) -> union (find_fv a args) (find_fv b args)
    |Sub (a, b) -> union (find_fv a args) (find_fv b args)
    |IfEq (x, y, a, b) -> union (test_var x args) (union (test_var y args) (union (find_fv a args) (find_fv b args)))
    |IfLE (x, y, a, b) -> union (test_var x args) (union (test_var y args) (union (find_fv a args) (find_fv b args)))
    |Var id -> test_var id args
    |AppD (a, b) -> union (test_var a args) (test_list b args)
    | _-> failwith "Fclosure:find_fv NotYetImplemented"

(*In the following functions:
    * COND(ast) means that ast has LetRec only as root or right sons and that
    * if the right son rs of a node is not LetRec then rs contains no LetRec
    * *)

let rec chi cat ls rs =
    (*precond: COND(ls) && (ls contains no LetRec) && COND(rs)
     *output: COND(output)*)
    match rs with
    |LetRec(a,b) -> LetRec(a, chi cat ls b)
    |_ -> cat ls rs

and psi cat ls rs =
    (*precond: COND(ls)
     *output: COND(output) *)
    match ls with
    |LetRec(a,b) -> LetRec(a, (psi cat b rs)) (*it's a psi-cat, bros*)
    |_ -> chi cat ls (phi rs)
(*
and phi (ast:t) : t =
     (*output: COND(output) *)
    match ast with
    |Let(x,y,z) -> psi (fun ls->fun rs->Let(x, ls, rs)) (phi y) z
    |LetRec(y,z) ->(
                     let fv = SS.elements (find_fv y.body (make_set y.args)) in
                     match fv with
                     |[] -> (known:=(fst y.name)::(!known);
                             psi (fun ls->fun rs->
                             LetRec({name=y.name; args=y.args; formal_fv=[]; body=ls},rs))
                                 (phi y.body) z)
                     |_ -> ( let clos_name = (fst y.name)^"c" in (*Hashtbl.add closures y.name clos_name;*)
                             psi (fun ls->fun rs ->
                             LetRec({name=y.name; args=y.args; formal_fv=fv; body=ls},rs))
                                 (phi y.body) (LetCls(clos_name, (fst y.name), (List.map fst fv), z)))
                   )
    |IfEq(u,v,y,z) -> psi (fun ls->fun rs->IfEq(u,v,ls,rs)) (phi y) z
    |IfLE(u,v,y,z) -> psi (fun ls->fun rs->IfLE(u,v,ls,rs)) (phi y) z
    |LetCls(u,v,a,b) -> chi (fun ls->fun rs->LetCls(u,v,a,rs)) Unit (phi b)
    |AppD(a, b) when List.mem a !known -> AppD(a, b)
    |AppD(a, b) -> let clos_name = a^"c" (*Hashtbl.find closures a*) in
                   AppC(clos_name, b)
    |_ -> ast
*)
and phi (ast:t) : t =
     (*output: COND(output) *)
    match ast with
    |Let(x,y,z) -> psi (fun ls->fun rs->Let(x, ls, rs)) (phi y) z
    |LetRec(y,z) ->(
                     let fv = (find_fv y.body y.args) in
                     match fv with
                     |[] -> (known:=(fst y.name)::(!known);
                             psi (fun ls->fun rs->
                             LetRec({name=y.name; args=y.args; formal_fv=[]; body=ls},rs))
                                 (phi y.body) z)
                     |_ -> ( let clos_name = (fst y.name)^"c" in (*Hashtbl.add closures y.name clos_name;*)
                             psi (fun ls->fun rs ->
                             LetRec({name=y.name; args=y.args; formal_fv=fv; body=ls},rs))
                                 (phi y.body) (LetCls(clos_name, (fst y.name), (List.map fst fv), z)))
                   )
    |IfEq(u,v,y,z) -> psi (fun ls->fun rs->IfEq(u,v,ls,rs)) (phi y) z
    |IfLE(u,v,y,z) -> psi (fun ls->fun rs->IfLE(u,v,ls,rs)) (phi y) z
    |LetCls(u,v,a,b) -> chi (fun ls->fun rs->LetCls(u,v,a,rs)) Unit (phi b)
    |_ -> ast

(*--------------------------------------------------------------------------*)


(*not really needed anymore*)
(*
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
*)
(*and chi fd = fd*)

let clos_out k =
    (*let clos = (clos_exp (the_cunning_psi k)) in*)
    (*
    let clos = (clos_exp (phi k)) in
    merge_letrecs_lets (letrecs_at_top clos) (lets_at_bot clos)
    *)
    scan_fundef ((*phi*) (clos_exp k))


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
          sprintf "(let %s = %s in \n%s)" (Id.to_string id) (clos_to_string e1) (clos_to_string e2)
  | Var id -> Id.to_string id
  | AppD (e1, le2) -> sprintf "AppD(%s %s)" (Id.to_string e1) (infix_to_string clos_to_string le2 " ")
  | AppC (e1, le2) -> sprintf "AppC(%s %s)" (Id.to_string e1) (infix_to_string clos_to_string le2 " ")
  | LetCls (x,l,fv,z) ->
          sprintf "(make_closure %s = (%s, %s) in \n%s)" x l
          (infix_to_string (fun x -> x) fv " ")
          (clos_to_string z)
  | LetRec (fd, e) ->
          sprintf "(let rec %s [%s] [%s] =\n    %s in\n%s)"
          (let (x, _) = fd.name in (Id.to_string x))
          (infix_to_string (fun (x,_) -> (Id.to_string x)) fd.args " ")
          (infix_to_string (fun (x,_) -> (Id.to_string x)) fd.formal_fv " ")
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
   | _-> "clos_to_string: NotYetImplemented"
