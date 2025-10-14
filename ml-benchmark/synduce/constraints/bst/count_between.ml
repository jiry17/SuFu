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
  | Node (w, l, r) -> (w >= tmax l && w <= tmin r) && (is_bst l && is_bst r)

let lo = 0
let hi = 0

let rec spec t =
  match t with
  | Leaf x -> if x > lo && x < hi then 1 else 0
  | Node (a, l, r) -> (if a > lo && a < hi then 1 else 0) + (spec l + spec r)

let rec target t =
  match t with
  | Leaf x -> Leaf x
  | Node (a, l, r) ->
      if a <= lo then Node (a, l, target r)
      else if a >= hi then Node (a, target l, r)
      else Node (a, target l, target r)

let program t = if is_bst t then spec (target t) else 0