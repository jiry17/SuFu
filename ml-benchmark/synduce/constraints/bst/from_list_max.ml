type tree = Leaf of int | Node of int * tree * tree
type llist = Elt of int | Cons of int * llist

let min a b = if a < b then a else b
let max a b = if a < b then b else a

let rec tmin t =
  match t with
  | Leaf w -> w
  | Node (w, l, r) -> min w (min (tmin l) (tmin r))

let rec tmax t =
  match t with
  | Leaf w -> w
  | Node (w, l, r) -> max w (max (tmax l) (tmax r))

let rec is_bst t =
  match t with
  | Leaf w -> true
  | Node (w, l, r) ->
      (w >= tmax l) && (w <= tmin r) && is_bst l && is_bst r

let rec cat x y =
  match x with
  | Cons (h, t) -> Cons (h, cat t y)
  | Elt w -> Cons (w, y)

let rec repr t =
  match t with
  | Leaf x -> Elt x
  | Node (x, l, r) -> cat (repr l) (Cons (x, repr r))

let rec spec t =
  match t with
  | Elt x -> x
  | Cons (h, t) -> max h (spec t)

val target: tree -> tree compress
let rec target t =
  match t with
  | Leaf x -> Leaf x
  | Node (a, l, r) -> Node (a, l, target r)

let program t =
  if is_bst t then spec (repr (target t)) else 0