type list_ = Nil of unit | Cons of int * list_
type idlist = Inil of unit | Icons of int * int * idlist

let rec length xs =
  match xs with
  | Inil _ -> 0
  | Icons (_, _, t) -> 1 + length t

let rec is_indexed xs =
  match xs with
  | Inil _ -> true
  | Icons (_, id, t) -> is_indexed t && id = length t

let rec repr m =
  match m with
  | Inil _ -> Nil ()
  | Icons (h, _, t) -> Cons (h, repr t)

let rec len xs =
  match xs with
  | Nil _ -> 0
  | Cons (_, t) -> 1 + len t

let rec spec xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> if h > len t then h + spec t else spec t

let rec target xs =
  match xs with
  | Inil _ -> xs
  | Icons (h, id, t) -> Icons (h, id, target t)

let program m =
  if is_indexed m then spec (repr (target m)) else 0