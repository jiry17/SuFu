type l = Nil of unit | Cons of int * l

let rec length x =
  match x with
  | Cons (_, t) -> length t + 1
  | Nil _ -> 0

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil _ -> y

let split xs =
  let rec f x n =
    if n < 1 then (Nil (), x)
    else
      match x with
      | Cons (h, t) ->
        let res = f t (n - 1) in
        match res with
        | (a, b) -> (Cons (h, a), b)
      | _ -> (Nil (), Nil ())
  in
  f xs (length xs / 2)

let dac v xs =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (_, t) ->
      match t with
      | Nil _ -> xs
      | _ ->
        let sp = split xs in
        match sp with
        | (a, b) -> concat (run a) (run b)
  in
  v (run xs)

let inf = 100

let max a b = if a < b then b else a

let rec max_sum_between_ones_impl cs pre xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) ->
    let cs0 = if pre < h then cs + 1 else 0 in
    max cs0 (max_sum_between_ones_impl cs0 h t)

let max_sum_between_ones xs = max_sum_between_ones_impl 0 0 xs

let program = dac max_sum_between_ones