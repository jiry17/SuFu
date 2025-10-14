type tree =
  | Leaf of int
  | Node of int * tree * tree

let rec spec y t =
  match t with
  | Leaf a -> y > a
  | Node (a, l, r) ->
      let r1 = spec a l in
      let r2 = spec a r in
      r1 && (r2 && (y > a))

let rec repr t =
  match t with
  | (pre, tt) ->
      match tt with
      | Leaf a -> (pre, Leaf a)
      | Node (a, l, r) ->
          let lres = repr (a, l) in
          let rres = repr (a, r) in
          let ltree =
            match lres with
            | (_, lt) -> lt
          in
          let rtree =
            match rres with
            | (_, rt) -> rt
          in
          (pre, Node (a, ltree, rtree))

let program key t =
  let res = repr (key, t) in
  match res with
  | (_, t2) -> spec key t2