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
          match res with
          | (l1, l2) -> (Cons (h, l1), l2)
      | _ -> (Nil (), Nil ())
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

let inf = 100

let max a b =
  if a < b then b else a

let rec longest1s_aux len xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) ->
      let len =
        if h = 1 then len + 1 else 0
      in
      max len (longest1s_aux len t)

let longest1s xs = longest1s_aux 0 xs

let program = dac longest1s