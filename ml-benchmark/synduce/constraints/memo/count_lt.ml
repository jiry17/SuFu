type tree = Leaf of int | Node of int * tree * tree
type treememo = MLeaf of int * int | MNode of int * int * treememo * treememo

let memo t =
  match t with
  | MLeaf (x, _) -> x
  | MNode (x, _, _, _) -> x

let rec is_memo t =
  match t with
  | MLeaf (n, x) ->
      (n >= 0) && (if x < 2 then n == 1 else n == 0)
  | MNode (n, a, l, r) ->
      let exp = (if a < 2 then 1 else 0) + (memo l + memo r) in
      ((n >= 0) && (n == exp)) && (is_memo l && is_memo r)

let rec repr t =
  match t with
  | MLeaf (_, a) -> Leaf a
  | MNode (_, a, l, r) -> Node (a, repr l, repr r)

let rec spec t =
  match t with
  | Leaf a -> if a < 2 then 1 else 0
  | Node (a, l, r) ->
      if a < 2 then 1 + spec l + spec r else spec l + spec r

val target: treememo -> treememo compress
let target t =
  match t with
  | MLeaf (_, a) -> if a < 2 then t else t
  | MNode (_, a, _, _) -> if a < 2 then t else t

let rec gen t =
  match t with
  | Leaf a -> MLeaf ((if a < 2 then 1 else 0), a)
  | Node (a, l, r) ->
      let res = (gen l, gen r) in
      match res with
      | (res1, res2) ->
          MNode (((if a < 2 then 1 else 0) + (memo res1 + memo res2)), a, res1, res2)

let program mt =
  if is_memo mt then spec (repr (target mt)) else 0