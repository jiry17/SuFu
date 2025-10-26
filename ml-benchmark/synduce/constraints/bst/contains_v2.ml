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
  | Node (w, l, r) ->
      (w >= tmax l) && (w <= tmin r) && (is_bst l) && (is_bst r)

let spec =
  let rec f t =
    match t with
    | Leaf x -> if x == w then 1 else 0
    | Node (a, l, r) ->
        if a == w then 1
        else if f l == 1 then 1
        else if f r == 1 then 1
        else 0
  in
  f

val target: tree -> tree compress
let target =
  let rec f t =
    match t with
    | Leaf x -> Leaf x
    | Node (a, l, r) ->
        if w == a then Node (a, l, r)
        else if w < a then Node (a, f l, r)
        else Node (a, l, f r)
  in
  f

let program t =
  if is_bst t then spec (target t) else 0