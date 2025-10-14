type tree = Nil of unit | Node of int * tree * tree

let max a b = if a < b then b else a

let rec height t =
  match t with
  | Nil _ -> 0
  | Node (w, l, r) -> 1 + max (height l) (height r)

let rec balanced t =
  match t with
  | Nil _ -> true
  | Node (w, l, r) -> (height l = height r) && balanced l && balanced r

let rec spec t =
  match t with
  | Nil _ -> 0
  | Node (w, l, r) -> 1 + (spec l + spec r)

let rec target t =
  match t with
  | Nil _ -> Nil ()
  | Node (w, l, r) -> Node (w, target l, r)

let program t =
  if balanced t then spec (target t) else 0