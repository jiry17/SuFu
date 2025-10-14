type lst = Cons of int * lst | Nil

let is_even a = a = 2 * (a / 2)

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
          | (l, r) -> (Cons (h, l), r)
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
            | (l, r) -> concat (run l) (run r)
  in
  v (run xs)

let inf = 100

let max a b = if a < b then b else a

let longest00s =
  let rec g len xs =
    match xs with
    | Nil -> 0
    | Cons (h, t) ->
        let len = if h = 0 then len + 1 else 0 in
        if is_even len then max len (g len t) else g len t
  in
  fun xs -> g 0 xs

let program = dac longest00s