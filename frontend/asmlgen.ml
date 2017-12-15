open Closure;;
open Printf;;

(* let rec gen c = match c with
    | Expression t -> match t with
        | Unit -> Nop
        | Int a -> Int a
        | Add (a, b) -> Add (,)
        | Sub (a, b) -> Sub (,)
        | Eq (a, b) -> Eq (,)
        | Let (a, b, c) -> Let
        | Var a ->
    | Let (id, t, asmt) -> match *)


let rec asml_to_string (exp:Closure.t) : string = match exp with
  | Unit -> "nop"
  (* | Bool b -> if b then "true" else "false" *)
  | Int i -> string_of_int i
  | Var id -> Id.to_string id
  (* | Let (x, a, b) -> sprintf *)
  | Add (e1, e2) -> sprintf "add %s %s \n" (asml_to_string e1) (asml_to_string e2)
  | Sub (e1, e2) -> sprintf "sub %s %s \n" (asml_to_string e1) (asml_to_string e2)
  | Let (x, a, b) -> sprintf "let %s = %s in \n %s" (fst x) (asml_to_string a) (asml_to_string b)
  | _ -> ""
(*
  | Not e -> sprintf "(not %s)" (asml_to_string e)
  | Neg e -> sprintf "(- %s)" (asml_to_string e)
  | Float f -> sprintf "%.2f" f
  | FNeg e -> sprintf "(-. %s)" (asml_to_string e)
  | FAdd (e1, e2) -> sprintf "(%s +. %s)" (asml_to_string e1) (asml_to_string e2)
  | FSub (e1, e2) -> sprintf "(%s -. %s)" (asml_to_string e1) (asml_to_string e2)
  | FMul (e1, e2) -> sprintf "(%s *. %s)" (asml_to_string e1) (asml_to_string e2)
  | FDiv (e1, e2) -> sprintf "(%s /. %s)" (asml_to_string e1) (asml_to_string e2)
  | Eq (e1, e2) -> sprintf "(%s = %s)" (asml_to_string e1) (asml_to_string e2)
  | LE (e1, e2) -> sprintf "(%s <= %s)" (asml_to_string e1) (asml_to_string e2) *)
  (* | IfEq (e1, e2, e3) ->
          sprintf "(if %s then %s else %s)" (asml_to_string e1) (asml_to_string e2) (asml_to_string e3)
  | IfLE (e1, e2, e3) ->
          sprintf "(if %s then %s else %s)" (asml_to_string e1) (asml_to_string e2) (asml_to_string e3)
  | Let ((id,t), e1, e2) ->
          sprintf "(let %s = %s in %s)" (Id.to_string id) (asml_to_string e1) (asml_to_string e2)
  | App (e1, le2) -> sprintf "(%s %s)" (asml_to_string e1) (infix_to_string asml_to_string le2 " ")
  | LetRec (fd, e) ->
          sprintf "(let rec %s %s = %s in %s)"
          (let (x, _) = fd.name in (Id.to_string x))
          (infix_to_string (fun (x,_) -> (Id.to_string x)) fd.args " ")
          (asml_to_string fd.body)   (*CHANGE LATER*)
          (asml_to_string e)
  | LetTuple (l, e1, e2)->
          sprintf "(let (%s) = %s in %s)"
          (infix_to_string (fun (x, _) -> Id.to_string x) l ", ")
          (asml_to_string e1)
          (asml_to_string e2)
  | Get(e1, e2) -> sprintf "%s.(%s)" (asml_to_string e1) (asml_to_string e2)
  | Put(e1, e2, e3) -> sprintf "(%s.(%s) <- %s)"
                 (asml_to_string e1) (asml_to_string e2) (asml_to_string e3)
  | Tuple(l) -> sprintf "(%s)" (infix_to_string asml_to_string l ", ")
  | Array(e1,e2) -> sprintf "(Array.create %s %s)"
       (asml_to_string e1) (asml_to_string e2) *)


let asml_to_string_main (exp:Closure.t) : string =
   sprintf "let _ = \n %s" (asml_to_string exp)
