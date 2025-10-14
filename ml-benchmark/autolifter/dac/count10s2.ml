type list = Nil of unit | Cons of int * list

let rec length x =
  match x with
  | Cons (_, t) -> 1 + length t
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
          (match res with
           | (l1, l2) -> (Cons (h, l1), l2))
      | _ -> (Nil (), Nil ())
  in
  f xs (length xs / 2)

let dac v xs =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (_, t) ->
        (match t with
         | Nil _ -> xs
         | _ ->
             let sp = split xs in
             match sp with
             | (a, b) -> concat (run a) (run b))
  in
  v (run xs)

let inf = 100

let rec count10s2 s0 xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) ->
      let upd = if s0 && h = 2 then 1 else 0 in
      let s0 = (h = 1) || (s0 && h = 0) in
      upd + count10s2 s0 t

let program = dac count10s2