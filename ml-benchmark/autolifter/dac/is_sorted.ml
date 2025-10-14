type list = Cons of int * list | Nil

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
    if n < 1 then
      (Nil, x)
    else
      match x with
      | Cons (h, t) ->
          (match f t (n - 1) with
           | (a, b) -> (Cons (h, a), b))
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
             let sp = split xs in
             (match sp with
              | (a, b) -> concat (run a) (run b)))
  in
  v (run xs)

let inf = 100

let is_sorted =
  let rec f pre xs =
    match xs with
    | Nil -> true
    | Cons (h, t) ->
        if pre >= h then false else f h t
  in
  fun xs -> f (0 - inf) xs

let program = dac is_sorted