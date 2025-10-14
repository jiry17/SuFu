type lst = Cons of int * lst | Nil

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
          let res = f t (n - 1) in
          match res with
          | (a, b) -> (Cons (h, a), b)
      | _ -> (Nil, Nil)
  in
  f xs (length xs / 2)

let dac v xs =
  let rec run ys =
    match ys with
    | Nil -> ys
    | Cons (_, t) ->
        match t with
        | Nil -> ys
        | _ ->
            let sp = split ys in
            match sp with
            | (a, b) -> concat (run a) (run b)
  in
  v (run xs)

let inf = 100

let max a b = if a < b then b else a

let rec max1s_f ma l xs =
  match xs with
  | Nil -> max ma l
  | Cons (h, t) ->
      if h = 0 then max1s_f (max ma l) 0 t else max1s_f ma (l + 1) t

let max1s = max1s_f 0 0

let program = dac max1s