type tree = Leaf of int | Node of int * tree * tree

let max a b = if a < b then b else a
let min a b = if a > b then b else a

let rec g x t =
  match t with
  | Leaf a ->
      (match x with
       | (x1, x2) -> (min a x1, max a x2))
  | Node (a, l, r) ->
      (match x with
       | (x1, x2) -> g (g (min a x1, max a x2) l) r)

let rec spec t =
  match t with
  | Leaf x -> (x, x)
  | Node (a, l, r) ->
      match g (a, a) l with
      | (r1, r2) -> g (r1, r2) r

let rec repr t =
  match t with
  | Leaf a -> Leaf a
  | Node (a, l, r) -> Node (a, repr l, repr r)

let program t = spec (repr t)