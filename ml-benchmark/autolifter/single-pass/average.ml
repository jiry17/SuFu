type list_ = Cons of int * list_ | Nil of unit

let div a b = if b = 0 then 0 else a / b

let rec length xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> 1 + length t

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let average xs =
  let len = length xs in
  let rec f ys =
    match ys with
    | Nil _ -> 0
    | Cons (h, t) -> h + f t
  in
  let sum = f xs in
  div sum len

let program = single_pass average