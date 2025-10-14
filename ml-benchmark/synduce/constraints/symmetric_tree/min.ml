type tree = Leaf of int | Node of int * tree * tree

let rec is_sym_pair l r =
  match l with
  | Leaf x1 ->
      (match r with
       | Leaf x2 -> x1 = x2
       | _ -> false)
  | Node (x1, l1, r1) ->
      (match r with
       | Node (x2, l2, r2) -> x1 = x2 && is_sym_pair r1 l2 && is_sym_pair l1 r1
       | _ -> false)

let rec is_sym t =
  match t with
  | Leaf _ -> true
  | Node (_, l, r) -> is_sym_pair l r && is_sym l && is_sym r

let min a b = if a < b then a else b

let rec spec t =
  match t with
  | Leaf w -> w
  | Node (w, l, r) -> min w (min (spec l) (spec r))

let rec target t =
  match t with
  | Leaf w -> Leaf w
  | Node (w, l, r) -> Node (w, target l, r)

let program t =
  if is_sym t then spec (target t) else 0