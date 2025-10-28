type tree = Nil of unit | Leaf of int | Node of int * tree * tree

let rec size t =
  match t with
  | Nil _ -> 0
  | Leaf _ -> 1
  | Node (_, l, r) -> 1 + (size l + size r)

let rec empty_right t =
  match t with
  | Node (_, l, r) -> (0 == size r) && empty_right l
  | _ -> true

let rec spec t =
  match t with
  | Nil _ -> 0
  | Leaf a -> a
  | Node (a, l, r) -> a + (spec l + spec r)

val target: tree -> tree compress
let rec target t =
  match t with
  | Nil _ -> Nil ()
  | Leaf a -> Leaf a
  | Node (a, l, r) -> Node (a, target l, r)

let program t =
  if empty_right t then spec (target t) else 0