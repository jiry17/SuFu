@Input val key: int
type tree = Leaf of int | Node of int * tree * tree

type treememo = Mleaf of int * int | Mnode of int * int * treememo * treememo

let memo t =
  match t with
  | Mleaf (x, _) -> x
  | Mnode (x, _, _, _) -> x

let max a b = if a < b then b else a

let rec tmax t =
  match t with
  | Mleaf (_, x) -> x
  | Mnode (_, x, l, r) ->
      let ml = tmax l in
      let mr = tmax r in
      let m = max ml mr in
      max x m

let rec is_memo t =
  match t with
  | Mleaf (n, x) -> true
  | Mnode (n, a, l, r) ->
      let c1 = n >= tmax l in
      let c2 = n >= tmax r in
      let c3 = n >= a in
      let bl = is_memo l in
      let br = is_memo r in
      c1 && c2 && c3 && bl && br

let rec repr t =
  match t with
  | Mleaf (n, a) -> Leaf a
  | Mnode (n, a, l, r) ->
      let rl = repr l in
      let rr = repr r in
      Node (a, rl, rr)

let rec spec t =
  match t with
  | Leaf a ->
      if a > key then a else 0
  | Node (a, l, r) ->
      let v = if a > key then a else 0 in
      let sl = spec l in
      let sr = spec r in
      v + (sl + sr)

val target: treememo -> treememo compress
let rec target t =
  match t with
  | Mleaf (n, a) -> t
  | Mnode (n, a, l, r) ->
      if key > n then t
      else
        let tl = target l in
        let tr = target r in
        Mnode (n, a, tl, tr)

let program mt =
  if is_memo mt then
    let tm = target mt in
    let rt = repr tm in
    spec rt
  else 0