type tree = Nil of unit | Node of int * tree * tree

let max a b = if a < b then b else a

let spec =
  let rec f w t =
    match t with
    | Nil _ -> w
    | Node (a, l, r) -> max a (f (f w r) l)
  in
  f 0

type 'a compress = 'a

let repr =
  let rec f t =
    match t with
    | Nil _ -> Nil ()
    | Node (a, l, r) -> Node (a, f l, f r)
  in
  f

let program t = spec (repr t)