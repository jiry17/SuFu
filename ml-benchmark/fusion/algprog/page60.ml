type tree = Tip of int | Bin of tree * tree

let rec tri op =
  let rec f t =
    match t with
    | Tip _ -> Tip 0
    | Bin (l, r) ->
        let rec g ys =
          match ys with
          | Tip w -> Tip (op w)
          | Bin (l2, r2) -> Bin (g l2, g r2)
        in
        Bin (g (f l), g (f r))
  in
  f

let op x = x + 1

let max a b = if a < b then b else a

let rec maximum t =
  match t with
  | Tip w -> w
  | Bin (l, r) -> max (maximum l) (maximum r)

let program t = maximum (tri op t)