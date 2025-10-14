type tree = Leaf of int | Node of int * tree * tree

type treememo = Mleaf of int | Mnode of int * int * treememo * treememo

let memo t =
  match t with
  | Mleaf _ -> 1
  | Mnode (x, _, _, _) -> x

let rec is_memo t =
  match t with
  | Mleaf x -> true
  | Mnode (n, a, l, r) ->
      (n = 1 + memo l + memo r) && (is_memo l && is_memo r)

let rec repr t =
  match t with
  | Mleaf a -> Leaf a
  | Mnode (n, a, l, r) -> Node (a, repr l, repr r)

let target t =
  match t with
  | Mleaf a -> Mleaf a
  | Mnode (n, a, l, r) -> Mnode (n, a, l, r)

let rec spec t =
  match t with
  | Leaf a -> 1
  | Node (a, l, r) -> 1 + spec l + spec r

let rec gen t =
  match t with
  | Leaf a -> Mleaf a
  | Node (a, l, r) ->
      let res = (gen l, gen r) in
      match res with
      | (l1, r1) -> Mnode (1 + memo l1 + memo r1, a, l1, r1)

let program mt =
  if is_memo mt then spec (repr (target mt)) else 0