type l = Nil of unit | Cons of int * l

let inf = 100
let two = 2
let three = 3

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

let dac v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) ->
        match t with
        | Nil _ -> xs
        | _ ->
            let sp = split xs in
            match sp with
            | (l1, l2) -> concat (run l1) (run l2)
  in
  fun xs -> v (run xs)

let rec count1s2s3s_aux s1 s2 xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) ->
      let upd = if h = 3 && (s1 || s2) then 1 else 0 in
      let s2 = (h = 2) && (s1 || s2) in
      let s1 = h = 1 in
      upd + count1s2s3s_aux s1 s2 t

let count1s2s3s = fun xs -> count1s2s3s_aux false false xs

let program = dac count1s2s3s