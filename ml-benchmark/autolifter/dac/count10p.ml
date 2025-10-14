type l = Cons of int * l | Nil of unit

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
  let rec run xs0 =
    match xs0 with
    | Nil _ -> xs0
    | Cons (_, t) ->
        (match t with
         | Nil _ -> xs0
         | _ ->
             let sp = split xs0 in
             (match sp with
              | (a, b) -> concat (run a) (run b)))
  in
  v (run xs)

let inf = 100

let rec count10p_f s0 s1 xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) ->
      let upd = if s1 && (h = 1) then 1 else 0 in
      let s1 = (h = 0) && (s0 || s1) in
      let s0 = (h = 1) in
      upd + count10p_f s0 s1 t

let count10p xs = count10p_f false false xs

let program = dac count10p