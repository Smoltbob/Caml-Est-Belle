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
    | Land of t * t
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

    | Land (a, b) -> Land (clos_exp a, clos_exp b)

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
    | Tuple a -> Tuple (List.map clos_exp a)
    | Array (a, b) -> Array (clos_exp a, clos_exp b)
    | Get (a, b) -> Get (clos_exp a, clos_exp b)
    | Put (a, b, c) -> Put (clos_exp a, clos_exp b, clos_exp c)
    | Let (x, a, b) -> Let (x, clos_exp a, clos_exp b)
    | LetRec (fundef, a) -> LetRec ({name = fundef.name; args = fundef.args; formal_fv = []; body = (clos_exp fundef.body)}, (clos_exp a))
    | App (f, l) -> (match f with
                        | (Var id) -> AppD (id, List.map clos_exp l)
                        | _ -> failwith "matchfailure App")
    | _ -> failwith "match not exhaustive in clos_exp fclosure.ml"




let closures = Hashtbl.create 10
let funs = Hashtbl.create 10
let predef = ref ["print_int"; "print_newline"; "print_char"]
let known = ref []
let _ = known := !predef


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
    if (memv x bv) || (Hashtbl.mem funs x) then
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
    |LetRec (a, b) -> union (find_fv a.body ((a.name::a.args)@args)) (find_fv b ((a.name::a.args)@args))
    |Unit -> []
    |Bool b -> []
    |Int i -> []
    |Not e -> []
    |Neg a -> find_fv a args
    |Get (a,b) -> find_fv a args
    |Put (a,b,c) -> union (find_fv a args) (union (find_fv b args) (find_fv c args))
    |Array (a,b) -> union (find_fv a args) (find_fv b args)
    |Add (a, b) -> union (find_fv a args) (find_fv b args)

    |Land (a, b) -> union (find_fv a args) (find_fv b args)

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

and phi (ast:t) : t =
     (*output: COND(output) *)
    match ast with
    |Let(x,y,z) -> psi (fun ls->fun rs->Let(x, ls, rs)) (phi y) z
    |LetRec(y,z) ->(
            Hashtbl.add funs (fst y.name) ("_"^(fst y.name));
            let fv = (find_fv y.body ((y.name)::((List.map (fun x->(x, Ftype.gentyp ())) !predef)@y.args))) in
                     match fv with
                     |[] -> (known:=(fst y.name)::(!known);
                             psi (fun ls->fun rs->
                             LetRec({name=y.name; args=y.args; formal_fv=[]; body=ls},rs))
                                 (phi y.body) z)
                     |_ -> ( let clos_name = (fst y.name)^"c" in
                             Hashtbl.add closures (fst y.name) clos_name;
                             psi (fun ls->fun rs ->
                             LetRec({name=y.name; args=y.args; formal_fv=fv; body=ls},rs))
                                 (phi y.body) (LetCls(clos_name, (fst y.name), (List.map fst fv), z)))
                   )
    |IfEq(u,v,y,z) -> psi (fun ls->fun rs->IfEq(u,v,ls,rs)) (phi y) z
    |IfLE(u,v,y,z) -> psi (fun ls->fun rs->IfLE(u,v,ls,rs)) (phi y) z
    |LetCls(u,v,a,b) -> chi (fun ls->fun rs->LetCls(u,v,a,rs)) Unit (phi b)
    |AppD(a, b) when List.mem a !known -> AppD(a, b)
    |AppD(a, b) -> AppC(a, b)
    |_ -> ast


let substitute tbl a =
    if Hashtbl.mem tbl a then (Hashtbl.find tbl a) else a

let substitute_var tbl a =
    match a with
    |Var(x) -> Var(substitute tbl x)
    |_ -> failwith "fclosure:id_to_clos matchfailure"

let rec id_to_cls ast =
    match ast with
    |Let(x,y,z) -> Let(x,id_to_cls y, id_to_cls z)
    |LetCls(x,l,y,z) -> LetCls(x, l, y, id_to_cls z) 
    |LetRec(x,y) -> LetRec({name=x.name; args=x.args; formal_fv=x.formal_fv; body=id_to_cls x.body}, id_to_cls y)
    |IfEq(u, v, x, y) -> IfEq(u, v, id_to_cls x, id_to_cls y)
    |IfLE(u, v, x, y) -> IfLE(u, v, id_to_cls x, id_to_cls y)
    |AppD(a, b) -> AppD(substitute closures a, List.map (substitute_var closures) b)
    |AppC(a, b) -> AppC(substitute closures a, List.map (substitute_var closures) b)
    |Var(_) -> substitute_var closures ast
    |_ -> ast

let rec add_prefix ast =
    match ast with
    |Let(x,y,z) -> Let(x,add_prefix y, add_prefix z)
    |LetCls(x,l,y,z) -> LetCls(x, l, y, add_prefix z)
    |LetRec(x,y) -> LetRec({name=x.name; args=x.args; formal_fv=x.formal_fv; body=add_prefix x.body}, add_prefix y)
    |IfEq(u, v, x, y) -> IfEq(u, v, add_prefix x, add_prefix y)
    |IfLE(u, v, x, y) -> IfLE(u, v, add_prefix x, add_prefix y)
    |AppD(a, b) -> if List.mem a (!predef) then AppD("_min_caml_"^a, b) else AppD(a,b)
    |Var(a) -> if List.mem a (!predef) then Var("_min_caml_"^a) else Var(a)
    |_ -> ast

let add_undr_id a =
    if Hashtbl.mem funs a then Hashtbl.find funs a else a

let rec add_undr ast =
    match ast with
    |Let(x,y,z) -> Let(x,add_undr y, add_undr z)
    |LetCls(x,l,y,z) -> LetCls(x, add_undr_id l, List.map add_undr_id y, add_undr z)
    |LetRec(x,y) -> if Hashtbl.mem funs (fst x.name) then
        LetRec({name=(Hashtbl.find funs (fst x.name), snd x.name); args=x.args; formal_fv=x.formal_fv; body=add_undr x.body}, add_undr y)
                    else
                        failwith "fclosure: not found in add_undr"
    |IfEq(u, v, x, y) -> IfEq(u, v, add_undr x, add_undr y)
    |IfLE(u, v, x, y) -> IfLE(u, v, add_undr x, add_undr y)
    |AppD(a, b) -> AppD(substitute funs a, List.map (substitute_var funs) b)
    |Var(_) -> substitute_var funs ast 
    |_ -> ast



let rec reduc k = match k with
    | LetRec (f, a) ->
            LetRec ({ name = f.name; args = f.args ; formal_fv = f.formal_fv; body = (reduc f.body) },
                reduc a)
    | Let (x, a, b) -> (match a with
        | Let (y, a2, b2) -> reduc (Let (y, a2, (reduc (Let (x, b2, b)))))
        | _ -> Let (x, reduc a, reduc b))
    | IfEq(x, y, a, b) -> IfEq(x, y, reduc a, reduc b)
    | IfLE(x, y, a, b) -> IfLE(x, y, reduc a, reduc b)
    | _ -> k
 


(** Main function of the module. First we do trivial type conversion. Then we unnest the LetRecs. Then we redo the unnesting of Lets. Then we correct the names of closures. Then we add "_" to labels. Finally we add th "_min_caml" prefix to the library functions*)
let clos_out k =
     add_prefix (add_undr (id_to_cls (reduc (phi  (clos_exp k)))))


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

  | Land (e1, e2) -> sprintf "(%s && %s)" (clos_to_string e1) (clos_to_string e2)

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
