type tree =
  | Leaf of int
  | Node of int * int * int * int * int * tree * tree

let min a b = if a < b then a else b
let max a b = if a < b then b else a

let rec tmin t =
  match t with
  | Leaf w -> w
  | Node (_, _, _, _, a, l, r) -> min a (min (tmin l) (tmin r))

let rec tmax t =
  match t with
  | Leaf w -> w
  | Node (_, _, _, _, a, l, r) -> max a (max (tmax l) (tmax r))

let rec is_memo t =
  match t with
  | Leaf _ -> true
  | Node (lmin, lmax, rmin, rmax, _, l, r) ->
      lmin = tmin l && lmax = tmax l && rmin = tmin r && rmax = tmax r && is_memo l && is_memo r

let spec t = (tmin t, tmax t)

let rec target t =
  match t with
  | Leaf _ -> t
  | Node (_, _, _, _, _, _, _) -> t

let rec gen t =
  match t with
  | Leaf x -> Leaf x
  | Node (_, _, _, _, v, l, r) ->
      Node (tmin l, tmax l, tmin r, tmax r, v, gen l, gen r)

let program t =
  let inp = gen t in
  if is_memo inp then spec (target inp) else (0, 0)