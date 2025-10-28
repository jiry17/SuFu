@Input val key: int
type tree = Leaf of int | Node of int * tree * tree
type treememo = MLeaf of int * int | MNode of int * int * treememo * treememo

let memo t =
  match t with
  | MLeaf (x, _) -> x
  | MNode (x, _, _, _) -> x

let max a b =
  if a < b then b else a

let rec tmax t =
  match t with
  | MLeaf (_, x) -> x
  | MNode (_, x, l, r) -> max x (max (tmax l) (tmax r))

let rec is_memo t =
  match t with
  | MLeaf (n, x) -> true
  | MNode (n, a, l, r) ->
    (n >= tmax l) && (n >= tmax r) && (n >= a) && (is_memo l) && (is_memo r)

let rec repr t =
  match t with
  | MLeaf (n, a) -> Leaf a
  | MNode (n, a, l, r) -> Node (a, repr l, repr r)

let rec spec t =
  match t with
  | Leaf a -> a == key
  | Node (a, l, r) -> (a == key) || (spec l) || (spec r)

val target: treememo -> treememo compress
let rec target t =
  match t with
  | MLeaf (n, a) -> t
  | MNode (n, a, l, r) ->
    if key > n then t else MNode (n, a, target l, target r)

let program mt =
  if is_memo mt then spec (repr (target mt)) else false