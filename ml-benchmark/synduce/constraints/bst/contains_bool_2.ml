@Input val w: int
type tree = Leaf of int | Node of int * tree * tree

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
  | Node (w, l, r) -> (w >= tmax l) && (w <= tmin r) && (is_bst l) && (is_bst r)

let rec spec t =
  match t with
  | Leaf x -> x == w
  | Node (a, l, r) -> (a == w) || (spec l) || (spec r)

val target: tree -> tree compress
let rec target t =
  match t with
  | Leaf x -> Leaf x
  | Node (a, l, r) ->
    if w < a then
      Node (a, target l, r)
    else
      Node (a, target l, target r)

let program t = if is_bst t then spec (target t) else false