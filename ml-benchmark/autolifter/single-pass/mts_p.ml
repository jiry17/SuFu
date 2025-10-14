type list_ = Nil of unit | Cons of int * list_

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let max a b = if a < b then b else a

let mts_p xs =
  let rec f xs i pos mts =
    match xs with
    | Nil _ -> pos
    | Cons (h, t) ->
        let pos = if (mts + h) < 0 then i else pos in
        let mts = max 0 (mts + h) in
        f t (i + 1) pos mts
  in
  f xs 0 (-1) 0

let program = single_pass mts_p