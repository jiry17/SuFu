type tree = Nil of unit | Node of int * tree * tree

let max a b = if a < b then b else a

let spec t =
  let rec f s t =
    match t with
    | Nil _ -> s
    | Node (a, l, r) ->
        let result = f s l in
        match result with
        | (x, y) ->
            let sum = a + x in
            f (sum, max y sum) r
  in
  let res = f (0, 0) t in
  match res with
  | (_, y) -> y

let rec repr t =
  match t with
  | Nil _ -> Nil ()
  | Node (a, l, r) -> Node (a, repr l, repr r)

let program t = spec (repr t)