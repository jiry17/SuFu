let sampleSize = 20

type tree =
  | Leaf of int
  | Node of tree * tree

type llist =
  | Nil
  | Cons of int * llist

let rec cat a b =
  match a with
  | Nil -> b
  | Cons (h, t) -> Cons (h, cat t b)

let max a b = if a < b then b else a

let rec depth t =
  match t with
  | Leaf w -> 0
  | Node (l, r) -> 1 + max (depth l) (depth r)

let rec deepest t =
  match t with
  | Leaf w -> (Cons (w, Nil), t)
  | Node (l, r) ->
      let lres = deepest l in
      let rres = deepest r in
      let ltree =
        match lres with
        | (x, y) -> y
      in
      let rtree =
        match rres with
        | (x, y) -> y
      in
      if depth ltree > depth rtree then
        match lres with
        | (x, y) -> (x, t)
      else if depth ltree = depth rtree then
        let llist =
          match lres with
          | (x, y) -> x
        in
        let rlist =
          match rres with
          | (x, y) -> x
        in
        (cat llist rlist, t)
      else
        match rres with
        | (x, y) -> (x, t)

let program t =
  match deepest t with
  | (x, y) -> x