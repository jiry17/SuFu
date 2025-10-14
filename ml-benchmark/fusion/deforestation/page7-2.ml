type tree = Leaf of int | Branch of tree * tree

let square x = x * x

let rec squaretr t =
  match t with
  | Leaf w -> Leaf (square w)
  | Branch (l, r) -> Branch (squaretr l, squaretr r)

let rec sumtr t =
  match t with
  | Leaf w -> w
  | Branch (l, r) -> sumtr l + sumtr r

let program t = sumtr (squaretr t)