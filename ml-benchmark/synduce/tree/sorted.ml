type tree = Leaf of int | Node of int * tree * tree

let spec t =
  let rec f t =
    match t with
    | Leaf a -> (a, true)
    | Node (a, l, r) ->
      let r1 = f l in
      let r2 = f r in
      match r1 with
      | r1a, r1b ->
        match r2 with
        | r2a, r2b ->
          (a, (r1a < a) && (a < r2a) && r1b && r2b)
  in
  match f t with
  | _, b -> b

let repr t =
  let rec f t =
    match t with
    | Leaf a -> Leaf a
    | Node (a, l, r) -> Node (a, f l, f r)
  in
  f t

let program t = spec (repr t)