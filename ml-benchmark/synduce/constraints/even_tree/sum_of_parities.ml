type tree = Elt of int | Node of int * tree * tree

let mod2 x = x - ((x / 2) * 2)

let rec is_even t =
  match t with
  | Elt a -> mod2 a = 0
  | Node (a, l, r) -> (mod2 a = 0) && is_even l && is_even r

let rec spec t =
  match t with
  | Elt a -> mod2 a
  | Node (a, l, r) -> mod2 a + (spec l + spec r)

let rec target t =
  match t with
  | Elt a -> Elt a
  | Node (a, l, r) -> Node (a, l, r)

let program t =
  if is_even t then spec (target t) else 0