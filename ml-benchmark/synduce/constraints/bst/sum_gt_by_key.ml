type lst = Elt of int * int | Cons of int * int * lst
type map = Value of int * int | Node of int * map * map

let min a b = if a < b then a else b
let max a b = if a > b then a else b

let rec min_key m =
  match m with
  | Value (k, v) -> k
  | Node (a, l, r) -> min (min_key l) (min_key r)

let rec max_key m =
  match m with
  | Value (k, v) -> k
  | Node (a, l, r) -> max (max_key l) (max_key r)

let rec is_map m =
  match m with
  | Value (k, v) -> true
  | Node (a, l, r) -> (max_key l) < a && a <= (min_key r) && is_map l && is_map r

let rec cat x y =
  match x with
  | Cons (k, v, t) -> Cons (k, v, cat t y)
  | Elt (k, v) -> Cons (k, v, y)

let rec repr m =
  match m with
  | Value (k, v) -> Elt (k, v)
  | Node (k, l, r) -> cat (repr l) (repr r)

let key = 0

let rec spec l =
  match l with
  | Elt (k, v) -> if k > key then v else 0
  | Cons (k, v, t) -> (if k > key then v else 0) + spec t

let rec target m =
  match m with
  | Value (k, v) -> Value (k, v)
  | Node (a, l, r) ->
      if a > key then Node (a, target l, target r)
      else Node (a, l, target r)

let program m =
  if is_map m then spec (repr (target m)) else 0