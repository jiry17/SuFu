type tree = Nil of unit | Single of int | Node of int * tree * tree

let max a b = if a < b then b else a

let spec t =
  let rec f s t =
    match t with
    | Nil _ -> s
    | Single a ->
        (match s with
         | (s1, s2) -> (s1 + a, max s2 (s1 + a)))
    | Node (a, l, r) ->
        (match s with
         | (s1, s2) ->
             let s' = (s1 + a, max s2 (s1 + a)) in
             f (f s' l) r)
  in
  match f (0, 0) t with
  | (_, x) -> x

let rec repr t =
  match t with
  | Nil _ -> Nil ()
  | Single a -> Single a
  | Node (a, l, r) -> Node (a, repr l, repr r)

let program t = spec (repr t)