type tree =
  | Leaf of int
  | Node of int * tree * tree

type mtree =
  | MLeaf of int
  | MNode of int * int * mtree * mtree

let min a b = if a < b then a else b
let max a b = if a < b then b else a

let rec tmin t =
  match t with
  | MLeaf w -> w
  | MNode (w, _, l, r) ->
      let ml = tmin l in
      let mr = tmin r in
      min w (min ml mr)

let rec tmax t =
  match t with
  | MLeaf w -> w
  | MNode (w, _, l, r) ->
      let ml = tmax l in
      let mr = tmax r in
      max w (max ml mr)

let rec tsum t =
  match t with
  | MLeaf w -> w
  | MNode (w, _, l, r) ->
      w + (tsum l + tsum r)

let rec is_bst t =
  match t with
  | MLeaf w -> w > 0
  | MNode (w, s, l, r) ->
      (w > 0)
      && (s = (tsum l + tsum r))
      && (w >= tmax l)
      && (w <= tmin r)
      && is_bst l
      && is_bst r

let lim = 0

let spec t =
  let rec f t =
    match t with
    | Leaf x -> (x <= lim, x)
    | Node (a, l, r) ->
        let lres = f l in
        let rres = f r in
        match lres with
        | (lb, ls) ->
            match rres with
            | (rb, rs) ->
                let sum = a + (ls + rs) in
                ((sum <= lim) && lb && rb, sum)
  in
  match f t with
  | (b, _) -> b

let rec drop_tag t =
  match t with
  | MLeaf x -> Leaf x
  | MNode (a, _, l, r) ->
      let dl = drop_tag l in
      let dr = drop_tag r in
      Node (a, dl, dr)

let rec add_tag t =
  match t with
  | Leaf x -> MLeaf x
  | Node (a, l, r) ->
      let lres = add_tag l in
      let rres = add_tag r in
      let s = tsum lres + tsum rres in
      MNode (a, s, lres, rres)

let rec target t =
  match t with
  | MLeaf x -> MLeaf x
  | MNode (a, s, l, r) ->
      if a > lim then MNode (a, s, l, r)
      else
        let tl = target l in
        let tr = target r in
        MNode (a, s, tl, tr)

let program t =
  let inp = add_tag t in
  if is_bst inp then spec (drop_tag (target inp)) else false