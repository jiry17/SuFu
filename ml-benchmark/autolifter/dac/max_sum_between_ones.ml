type list = Nil | Cons of int * list

let rec length x =
  match x with
  | Cons (_, t) -> length t + 1
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
          (match f t (n - 1) with
           | (l1, l2) -> (Cons (h, l1), l2))
      | _ -> (Nil, Nil)
  in
  f xs (length xs / 2)

let dac v xs =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (_, t) ->
        (match t with
         | Nil -> xs
         | _ ->
             (match split xs with
              | (a, b) -> concat (run a) (run b)))
  in
  v (run xs)

let inf = 100

let max a b = if a < b then b else a

let rec max_sum_between_ones_f cs xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) ->
      let cs = if h = 1 then 0 else cs + h in
      max cs (max_sum_between_ones_f cs t)

let max_sum_between_ones xs = max_sum_between_ones_f 0 xs

let program = dac max_sum_between_ones