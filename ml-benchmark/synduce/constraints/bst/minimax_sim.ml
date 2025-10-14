type tree = Leaf of int | Node of int * tree * tree

let min a b = if a < b then a else b
let max a b = if a < b then b else a

let rec tmin t =
  match t with
  | Leaf w -> w
  | Node (w, l, r) -> min w (min (tmin l) (tmin r))

let rec tmax t =
  match t with
  | Leaf w -> w
  | Node (w, l, r) -> max w (max (tmax l) (tmax r))

let rec is_bst t =
  match t with
  | Leaf _ -> true
  | Node (w, l, r) -> (w >= tmax l) && (w <= tmin r) && (is_bst l) && (is_bst r)

let rec spec t =
  match t with
  | Leaf x -> (x, x)
  | Node (a, l, r) ->
      let lres = spec l in
      let rres = spec r in
      match lres with
      | (l1, l2) ->
          match rres with
          | (r1, r2) -> (max a (max l1 r1), min a (min l2 r2))

let rec target t =
  match t with
  | Leaf x -> Leaf x
  | Node (a, l, r) ->
      let _ = tmin l in
      let _ = tmax r in
      Node (a, l, r)

let program t = if is_bst t then spec (target t) else (0, 0)