type tree = Nil of unit | Node of int * tree * tree

let max a b = if a < b then b else a

let rec spec_impl w t =
  match t with
  | Nil _ -> w
  | Node (a, l, r) -> spec_impl (max (spec_impl w l) a) r

let spec t = spec_impl 0 t

let rec repr t =
  match t with
  | Nil _ -> Nil ()
  | Node (a, l, r) -> Node (a, repr l, repr r)

let program t = spec (repr t)