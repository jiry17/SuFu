type tree = Empty | Node of int * tree * tree
type mylist = Nil | Cons of int * mylist

let rec cat x y =
  match x with
  | Cons (h, t) -> Cons (h, cat t y)
  | Nil -> y

let rec dec res t =
  match t with
  | Empty -> res
  | Node (w, l, r) -> dec (Cons (w, dec res l)) r

let rec repr t =
  match t with
  | Empty -> Nil
  | Node (w, l, r) -> dec (Cons (w, repr l)) r

let rec target t =
  match t with
  | Empty -> Empty
  | Node (w, l, r) -> Node (w, target l, target r)

let x = 0

let rec spec xs =
  match xs with
  | Nil -> false
  | Cons (h, t) -> if h = x then true else spec t

let program xs = spec (repr (target xs))