type list = Nil | Cons of int * list

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
        (match t with
         | Nil -> xs
         | _ ->
            let sp = split xs in
            (match sp with
             | (a, b) -> concat (run a) (run b)))
  in
  v (run xs)

let inf = 100

let max a b = if a < b then b else a

let longest10s2 xs =
  let rec f s0 len xs =
    match xs with
    | Nil -> 0
    | Cons (h, t) ->
        let s1 = s0 && (h = 2) in
        let s0 = (h = 1) || ((h = 0) && s0) in
        let len = if s0 || s1 then len + 1 else 0 in
        let upd = if s1 then len else 0 in
        let len = if h = 1 then 1 else if s1 then 0 else len in
        max upd (f s0 len t)
  in
  f false 0 xs

let program = dac longest10s2