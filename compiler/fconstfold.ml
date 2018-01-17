(*type constant =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
*)

open Fknormal

let is_constant (k:Fknormal.t) =
    match k with
    |Unit -> true
    |Bool _ -> true
    |Int _ -> true
    |Float _ -> true
    |_ -> false

let get_bool k=
    match k with
    |Bool x -> x
    |_ -> failwith "ConstFold.get_bool: wrong type"

let get_int k=
    match k with
    |Int x -> x
    |_ -> failwith "ConstFold.get_int: wrong type"

let get_float k=
    match k with
    |Float x -> x
    |_ -> failwith "ConstFold.get_float: wrong type"

type comparison = {f : 'a. 'a -> 'a -> bool}

let apply_polymorph op a b =
    match a,b with
    |(Int a', Int b') -> op.f a' b'
    |(Float a', Float b') -> op.f a' b'
    |(Bool a', Bool b') -> op.f a' b'
    |_ -> failwith "ConstFold:apply_polymorph types do not match or operation is not polymorphic"

let order a b =
    if is_constant a then b, a else a, b

let rec g (m: (Id.t, Fknormal.t) Hashtbl.t) (k:Fknormal.t) : Fknormal.t  =
    match k with
    |Let ((a,t), b, c) -> let b' = g m b in
                        if is_constant b' then Hashtbl.add m a b';
                        Let((a,t), b', g m c)
    |LetRec (a, b) -> LetRec(a, g m b)
    |Unit -> Unit
    |Bool a -> Bool a
    |Int a ->  Int a
    |Float a -> Float a
    |Var a -> if Hashtbl.mem m a then Hashtbl.find m a else Var a
    |Not b -> let b' = g m b in if is_constant b' then Bool(not (get_bool b')) else Not b'
    |Neg a -> let a' = g m a in if is_constant a' then Int(- (get_int a')) else Neg a'
    |Sub (a, b) -> let a' = g m a in
                   let b' = g m b in
                   if (is_constant a') && (is_constant b') then
                       Int((get_int a') - (get_int b'))
                   else let a', b' = order a' b' in Sub(a',b')
    |Add (a, b) -> let a' = g m a in
                   let b' = g m b in
                   if (is_constant a') && (is_constant b') then
                       Int((get_int a') + (get_int b'))
                   else let a', b' = order a' b' in Add(a',b')

    |Land (a, b) -> Land(a,b) 

    |FNeg a -> let a' = g m a in if is_constant a' then Float(-. (get_float a')) else FNeg a'

    |FAdd (a, b) -> let a' = g m a in
                   let b' = g m b in
                   if (is_constant a') && (is_constant b') then
                       Float((get_float a') +. (get_float b'))
                   else FAdd(a,b)  (*either a=a' or b=b'(=Var(x)) => we must keep both in non-immediate form*)
    |FSub (a, b) -> let a' = g m a in
                   let b' = g m b in
                   if (is_constant a') && (is_constant b') then
                       Float((get_float a') -. (get_float b'))
                   else FSub(a,b)
    |FMul (a, b) -> let a' = g m a in
                   let b' = g m b in
                   if (is_constant a') && (is_constant b') then
                       Float((get_float a') *. (get_float b'))
                   else FMul(a,b)
    |FDiv (a, b) -> let a' = g m a in
                   let b' = g m b in
                   if (is_constant a') && (is_constant b') then
                       Float((get_float a') /. (get_float b'))
                   else FDiv(a,b)
    |App (a,b) ->  (match a with
                                |Var(a') -> let a = (if Hashtbl.mem m a' then Hashtbl.find m a' else a) in App (a, b)
                                |_->failwith "ConstFold.g: bad App")

    |IfEq (x, y, b, c) ->
                                    IfEq (x, y, g m b, g m c)

    |IfLE (x, y, b, c) ->
                                    IfLE (x, y, g m b, g m c)

    |Array (a, b) -> Array (a, b)
    |Get (a, b) -> Get (a, b)
    |Put (a, b, c) -> Put (a, b, c)
    |_ -> failwith "ConstFold.g: NotYetImplemented"

let f (k:Fknormal.t) =
    let expected_number = 100 in
    let m = Hashtbl.create expected_number in
    g m k
