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
          match res with
          | (p1, p2) -> (Cons (h, p1), p2)
      | Nil _ -> (Nil (), Nil ())
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
            let sp = split xs in
            match sp with
            | (a, b) -> concat (run a) (run b)
  in
  v (run xs)

let max a b =
  if a < b then b else a

let mtp xs =
  let rec f l =
    match l with
    | Nil _ -> (1, 1)
    | Cons (h, t) ->
        let res = f t in
        match res with
        | (r1, r2) ->
            let tot = h * r2 in
            let m = max r1 tot in
            (m, tot)
  in
  match f xs with
  | (a, b) -> a

let program = dac mtp