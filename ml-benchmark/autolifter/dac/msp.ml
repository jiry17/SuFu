type l = Nil | Cons of int * l

let rec length x =
  match x with
  | Cons (h, t) -> length t + 1
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
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) ->
        match t with
        | Nil -> xs
        | _ ->
            let sp = split xs in
            match sp with
            | (a, b) -> concat (run a) (run b)
  in
  v (run xs)

let max a b = if a < b then b else a

let msp =
  let rec f pre l =
    match l with
    | Nil -> pre
    | Cons (h, t) -> max pre (f (h * pre) t)
  in
  let mpp = f 1 in
  let rec g xs =
    match xs with
    | Nil -> 1
    | Cons (h, t) -> max (mpp xs) (g t)
  in
  g

let program = dac msp