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
          (match res with
           | (a, b) -> (Cons (h, a), b))
      | _ -> (Nil, Nil)
  in
  f xs (length xs / 2)

let dac v xs =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (_, t) ->
        match t with
        | Nil -> xs
        | _ ->
            let sp = split xs in
            match sp with
            | (a, b) -> concat (run a) (run b)
  in
  v (run xs)

let inf = 100

let rec max xs =
  match xs with
  | Nil -> 0 - inf
  | Cons (h, t) ->
      let res = max t in
      if res < h then h else res

let program = dac max