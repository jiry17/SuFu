type list_ = Cons of int * list_ | Nil of unit

let rec length x =
  match x with
  | Cons (h, t) -> length t + 1
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

let max a b = if a < b then b else a

let rec max1s_with_pos_aux pre i xs =
  match xs with
  | Nil _ ->
      let len = i - pre in
      (len, pre)
  | Cons (h, t) ->
      if h = 1 then max1s_with_pos_aux pre (i + 1) t
      else
        let len = i - pre in
        let res = max1s_with_pos_aux (i + 1) (i + 1) t in
        match res with
        | (r1, r2) ->
            if len >= r1 then (len, pre) else res

let max1s_with_pos xs = max1s_with_pos_aux 0 0 xs

let program = dac max1s_with_pos