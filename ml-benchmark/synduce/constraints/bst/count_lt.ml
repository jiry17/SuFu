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
  | Leaf _ -> true
  | Node (a, l, r) -> (a >= tmax l) && (a <= tmin r) && (is_bst l) && (is_bst r)

let rec spec t =
  match t with
  | Leaf x -> if x < w then 1 else 0
  | Node (a, l, r) -> (if a < w then 1 else 0) + (spec l + spec r)

val target: tree -> tree compress
let rec target t =
  match t with
  | Leaf x -> Leaf x
  | Node (a, l, r) -> if a < w then Node (a, target l, target r) else Node (a, target l, r)

let program t = if is_bst t then spec (target t) else 0