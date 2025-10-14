type tree = Nil | Node of int * tree * tree

let rec f w t =
  match t with
  | Nil -> w
  | Node (a, l, r) ->
      let wl = f w l in
      let w2 = wl + a in
      f w2 r

let spec t =
  f 0 t

let rec repr t =
  match t with
  | Nil -> Nil
  | Node (a, l, r) ->
      let l2 = repr l in
      let r2 = repr r in
      Node (a, l2, r2)

let program t =
  spec (repr t)