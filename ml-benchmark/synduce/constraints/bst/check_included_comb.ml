@Input val lo: int
@Input val hi: int

type tree = Leaf of int * int | Node of int * int * tree * tree

let min a b = if a < b then a else b
let max a b = if a < b then b else a

let rec tmin t =
  match t with
  | Leaf (a, b) -> a
  | Node (a, b, l, r) -> min a (min (tmin l) (tmin r))

let rec tmax t =
  match t with
  | Leaf (a, b) -> a
  | Node (a, b, l, r) -> max a (max (tmax l) (tmax r))

let rec is_bst t =
  match t with
  | Leaf (a, b) -> a < b
  | Node (a, b, l, r) -> (a < b) && ((a > tmax l) && (a < tmin r) && (is_bst l) && (is_bst r))

let inside a b = (lo < a) && (b < hi)

let rec spec t =
  match t with
  | Leaf (a, b) -> inside a b
  | Node (a, b, l, r) -> (inside a b) || (spec l) || (spec r)

val target: tree -> tree compress
let rec target t =
  match t with
  | Leaf (a, b) -> Leaf (a, b)
  | Node (a, b, l, r) ->
    if (b > hi) && (a < lo) then
      Node (a, b, l, target r)
    else
      Node (a, b, target l, target r)

let program t = if is_bst t then spec (target t) else false