type tree =
  | Leaf of int
  | Node of int * tree * tree

type treememo =
  | Mleaf of int
  | Mnode of int * int * treememo * treememo

let memo t =
  match t with
  | Mleaf _ -> 1
  | Mnode (x, _, _, _) -> x

let rec is_memo t =
  match t with
  | Mleaf _ -> true
  | Mnode (n, _, l, r) ->
      (n = 1 + memo l + memo r) && (is_memo l) && (is_memo r)

let rec repr t =
  match t with
  | Mleaf a -> Leaf a
  | Mnode (_, a, l, r) -> Node (a, repr l, repr r)

let target t =
  match t with
  | Mleaf a -> Mleaf a
  | Mnode (n, a, l, r) -> Mnode (n, a, l, r)

let spec t =
  match t with
  | Leaf _ -> 1
  | Node (_, _, _) -> 1

let rec gen t =
  match t with
  | Leaf a -> Mleaf a
  | Node (a, l, r) ->
      let fl = gen l in
      let fr = gen r in
      Mnode (1 + memo fl + memo fr, a, fl, fr)

let program mt =
  if is_memo mt then spec (repr (target mt)) else 0