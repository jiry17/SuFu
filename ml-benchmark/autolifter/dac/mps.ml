type list = Cons of int * list | Nil of unit

let rec length x =
  match x with
  | Cons (h, t) -> 1 + length t
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
          | (l1, l2) -> (Cons (h, l1), l2)
      | _ -> (Nil (), Nil ())
  in
  f xs (length xs / 2)

let dac v xs =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) ->
        match t with
        | Nil _ -> xs
        | _ ->
            let sp = split xs in
            match sp with
            | (a, b) -> concat (run a) (run b)
  in
  v (run xs)

let rec mps l =
  match l with
  | Nil _ -> 0
  | Cons (h, t) ->
      let res = mps t in
      if 0 < res + h then 0 else res + h

let program = dac mps