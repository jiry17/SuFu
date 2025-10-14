type tree = Nil of unit | Node of int * tree * tree

let rec spec t =
  let rec f s t =
    match t with
    | Nil _ -> s
    | Node (a, l, r) ->
      let result = f s l in
      match result with
      | (r1, r2) ->
        let new_s = (r1 + r2 * a, r2 * x) in
        f new_s r
  in
  let res = f (0, 1) t in
  match res with
  | (v, _) -> v

let rec repr t =
  match t with
  | Nil _ -> Nil ()
  | Node (a, l, r) -> Node (a, repr l, repr r)

let program t = spec (repr t)