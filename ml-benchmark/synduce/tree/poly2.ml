type tree = Nil of unit | Node of int * tree * tree

let max a b = if a < b then b else a

let x = ref 0

let spec xs =
  let rec f s t =
    match t with
    | Nil _ -> s
    | Node (a, l, r) ->
      let result = f s l in
      let new_s =
        match result with
        | (r1, r2) -> (a + (!x) * r1, (!x) * r2)
      in
      f new_s r
  in
  match f (0, 1) xs with
  | (s1, _) -> s1

let rec repr t =
  match t with
  | Nil _ -> Nil ()
  | Node (a, l, r) ->
    let l2 = repr l in
    let r2 = repr r in
    Node (a, l2, r2)

let program t = spec (repr t)