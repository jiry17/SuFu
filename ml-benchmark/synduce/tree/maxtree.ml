type tree = Nil of unit | Node of int * tree * tree

let max a b = if a < b then b else a

let rec f w t =
  match t with
  | Nil _ -> w
  | Node (a, l, r) -> f (max (f w l) a) r

let spec = f 0

let rec repr t =
  match t with
  | Nil _ -> Nil ()
  | Node (a, l, r) -> Node (a, repr l, repr r)

let program t = spec (repr t)