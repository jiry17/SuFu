type list = Cons of int * list | Nil of unit

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
          match f t (n - 1) with
          | r1, r2 -> (Cons (h, r1), r2)
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
            match split xs with
            | a, b -> concat (run a) (run b)
  in
  v (run xs)

let inf = 100

let rec dropwhile_aux i xs =
  match xs with
  | Nil _ -> i
  | Cons (h, t) ->
      if h > 0 then i else dropwhile_aux (i + 1) t

let dropwhile xs = dropwhile_aux 0 xs

let program = dac dropwhile