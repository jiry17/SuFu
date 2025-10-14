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
    if n < 1 then (Nil (), x) else
    match x with
    | Cons (h, t) ->
        let res = f t (n - 1) in
        match res with
        | (a, b) -> (Cons (h, a), b)
    | _ -> (Nil (), Nil ())
  in
  let n = length xs / 2 in
  f xs n

let dac v xs =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (_, t) ->
        match t with
        | Nil _ -> xs
        | _ ->
            let sp = split xs in
            match sp with
            | (a, b) -> concat (run a) (run b)
  in
  v (run xs)

let max a b = if a < b then b else a

let mts xs =
  let rec f xs =
    match xs with
    | Nil _ -> (0, 0)
    | Cons (h, t) ->
        let res = f t in
        match res with
        | (a, b) -> (max (b + h) a, b + h)
  in
  match f xs with
  | (a, b) -> a

let program = dac mts