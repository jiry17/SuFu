type lst = Nil | Cons of int * lst

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
    if n < 1 then
      (Nil, x)
    else
      match x with
      | Cons (h, t) ->
          let res = f t (n - 1) in
          match res with
          | (r1, r2) -> (Cons (h, r1), r2)
      | Nil -> (Nil, Nil)
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
             match sp with
             | (l1, l2) -> concat (run l1) (run l2))
  in
  v (run xs)

let max a b =
  if a < b then b else a

let mss xs =
  let rec f xs =
    match xs with
    | Nil -> (0, 0)
    | Cons (h, t) ->
        let res = f t in
        match res with
        | (m1, mps) ->
            let new_mps = max 0 (h + mps) in
            (max new_mps m1, new_mps)
  in
  let res = f xs in
  match res with
  | (ans, _) -> ans

let program = dac mss