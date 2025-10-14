type l = Nil of unit | Cons of int * l

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let max a b = if a < b then b else a

let mts xs =
  let rec f xs =
    match xs with
    | Nil _ -> (0, 0)
    | Cons (h, t) ->
        let res = f t in
        match res with
        | (a1, a2) -> (max (a2 + h) a1, a2 + h)
  in
  let res = f xs in
  match res with
  | (a1, _) -> a1

let program = single_pass mts