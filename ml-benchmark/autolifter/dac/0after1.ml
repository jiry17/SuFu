type t = Cons of int * t | Nil

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
    if n < 1 then (Nil, x) else
      match x with
      | Cons (h, t) ->
          let res = f t (n - 1) in
          match res with
          | (l1, l2) -> (Cons (h, l1), l2)
      | Nil -> (Nil, Nil)
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

let rec zafter1_f seen1 xs =
  match xs with
  | Nil -> false
  | Cons (h, t) ->
      if seen1 && h = 0 then true else zafter1_f (seen1 || h = 1) t

let zafter1 xs = zafter1_f false xs

let program = dac zafter1