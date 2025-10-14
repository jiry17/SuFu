type tree = Nil of unit | Node of int * tree * tree

let max a b = if a < b then b else a

let rec f w t =
  match t with
  | Nil _ -> w
  | Node (a, l, r) ->
      let left = f (w + a) l in
      let right = f (w + a) r in
      max left right

let spec t = f 0 t

let rec repr t =
  match t with
  | Nil _ -> Nil ()
  | Node (a, l, r) ->
      let l1 = repr l in
      let r1 = repr r in
      Node (a, l1, r1)

let program t = spec (repr t)