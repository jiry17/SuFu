type tree =
  | Nil of unit
  | Leaf of int
  | Node of int * tree * tree

let rec size t =
  match t with
  | Nil _ -> 0
  | Leaf _ -> 1
  | Node (_, l, r) -> 1 + size l + size r

let rec empty_right t =
  match t with
  | Node (_, l, r) -> (size r = 0) && empty_right l
  | _ -> true

let w = 0

let rec spec t =
  match t with
  | Nil _ -> 0
  | Leaf a -> if a = w then 1 else 0
  | Node (a, l, r) ->
      if a = w then 1 else if spec l = 1 then 1 else spec r

let rec target t =
  match t with
  | Nil _ -> Nil ()
  | Leaf a -> Leaf a
  | Node (a, l, r) -> Node (a, target l, r)

let program t =
  if empty_right t then spec (target t) else 0