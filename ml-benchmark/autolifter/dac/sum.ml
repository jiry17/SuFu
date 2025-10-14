type lst = Cons of int * lst | Nil of unit

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
          (match res with
           | (r1, r2) -> (Cons (h, r1), r2))
      | _ -> (Nil (), Nil ())
  in
  f xs (length xs / 2)

let dac v xs =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (_, t) ->
        (match t with
         | Nil _ -> xs
         | _ ->
             let sp = split xs in
             (match sp with
              | (a, b) -> concat (run a) (run b)))
  in
  v (run xs)

let rec sum xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> h + sum t

let program = dac sum