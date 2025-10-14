type list_ = Nil | Cons of int * list_

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let max a b = if a < b then b else a

let mss xs =
  let rec f xs =
    match xs with
    | Nil -> (0, 0)
    | Cons (h, t) ->
        let res = f t in
        let new_mps =
          match res with
          | (_, b) -> max 0 (h + b)
        in
        let mss_value =
          match res with
          | (a, _) -> max new_mps a
        in
        (mss_value, new_mps)
  in
  match f xs with
  | (a, _) -> a

let program = single_pass mss