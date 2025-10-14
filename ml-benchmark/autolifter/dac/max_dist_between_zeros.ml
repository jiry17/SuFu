type lst = Cons of int * lst | Nil

let rec length x =
  match x with
  | Cons (h, t) -> length t + 1
  | Nil -> 0

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil -> y

let split xs =
  let rec f x n =
    if n < 1 then (Nil, x)
    else
      match x with
      | Cons (h, t) ->
          let res = f t (n - 1) in
          match res with
          | (l1, l2) -> (Cons (h, l1), l2)
      | _ -> (Nil, Nil)
  in
  f xs (length xs / 2)

let dac v xs =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) ->
        match t with
        | Nil -> xs
        | _ ->
            let sp = split xs in
            match sp with
            | (l, r) -> concat (run l) (run r)
  in
  v (run xs)

let inf = 100

let max a b =
  if a < b then b else a

let rec max_dist_between_zeros_impl cs xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) ->
      let cs = if h = 0 then 0 else cs + 1 in
      max cs (max_dist_between_zeros_impl cs t)

let max_dist_between_zeros xs =
  max_dist_between_zeros_impl 0 xs

let program = dac max_dist_between_zeros