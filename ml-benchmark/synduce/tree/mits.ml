type tree = Nil of unit | Node of int * tree * tree
type 'a compress = 'a

let max a b = if a < b then b else a

let spec t =
  let rec f s t =
    match t with
    | Nil _ -> s
    | Node (a, l, r) ->
        let result = f s l in
        match result with
        | (r1, r2) ->
            let s' = (a + r1, max 0 (a + r2)) in
            f s' r
  in
  let p = f (0, 0) t in
  match p with
  | (_, x) -> x

let rec repr t =
  match t with
  | Nil _ -> Nil ()
  | Node (a, l, r) ->
      let l' = repr l in
      let r' = repr r in
      Node (a, l', r')

let program t = spec (repr t)