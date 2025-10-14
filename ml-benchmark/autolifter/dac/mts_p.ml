type list = Nil of unit | Cons of int * list

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
        | a, b -> (Cons (h, a), b)
      | _ -> (Nil (), Nil ())
  in
  f xs (length xs / 2)

let dac v xs =
  let run =
    let rec f xs =
      match xs with
      | Nil _ -> xs
      | Cons (_, t) ->
        match t with
        | Nil _ -> xs
        | _ ->
          let sp = split xs in
          match sp with
          | a, b -> concat (f a) (f b)
    in
    f
  in
  v (run xs)

let max a b = if a < b then b else a

let mts_p xs =
  let rec f xs i pos mts =
    match xs with
    | Nil _ -> pos
    | Cons (h, t) ->
      let pos = if mts + h < 0 then i else pos in
      let mts = max 0 (mts + h) in
      f t (i + 1) pos mts
  in
  f xs 0 (-1) 0

let program = dac mts_p