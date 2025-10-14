type my_list = Cons of int * my_list | Nil

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let max a b = if a < b then b else a

let rec max_sum_between_ones_inner cs xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) ->
    let cs = if h = 1 then 0 else cs + h in
    max cs (max_sum_between_ones_inner cs t)

let max_sum_between_ones xs = max_sum_between_ones_inner 0 xs

let program = single_pass max_sum_between_ones