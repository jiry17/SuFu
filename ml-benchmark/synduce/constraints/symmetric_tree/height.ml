type tree = Leaf of int | Node of int * tree * tree

let rec is_sym_pair l r =
  match (l, r) with
  | (Leaf x1, Leaf x2) -> x1 = x2
  | (Node (x1, l1, r1), Node (x2, l2, r2)) -> (x1 = x2) && (is_sym_pair r1 l2 && is_sym_pair l1 r1)
  | _ -> false

let rec is_sym t =
  match t with
  | Leaf _ -> true
  | Node (_, l, r) -> is_sym_pair l r && (is_sym l && is_sym r)

let max a b = if a < b then b else a

let rec spec t =
  match t with
  | Leaf _ -> 0
  | Node (_, l, r) -> 1 + (spec l + spec r)

let rec target t =
  match t with
  | Leaf w -> Leaf w
  | Node (w, l, r) -> Node (w, target l, r)

type list_ = Elt of int | Cons of int * list_

let depth_lim = 4

let rec gen_f depth xs =
  match xs with
  | Elt w -> Leaf w
  | Cons (h, t) ->
      if depth = 0 then Leaf h
      else
        let rem = depth - 1 in
        Node (h, gen_f rem t, gen_f rem t)

let gen xs = gen_f depth_lim xs

let program xs =
  let t = gen xs in
  if is_sym t then spec (target t) else 0