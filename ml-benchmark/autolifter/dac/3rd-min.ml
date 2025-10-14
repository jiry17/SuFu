type l = Cons of int * l | Nil of unit

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
        (match res with
         | (a, b) -> (Cons (h, a), b))
      | _ -> (Nil (), Nil ())
  in
  f xs (length xs / 2)

let dac v =
  fun xs ->
    let run =
      let rec f xs =
        match xs with
        | Nil _ -> xs
        | Cons (h, t) ->
          (match t with
           | Nil _ -> xs
           | _ ->
             let sp = split xs in
             (match sp with
              | (a, b) -> concat (f a) (f b)))
      in
      f
    in
    v (run xs)

let inf = 100

let min a b = if a < b then a else b
let max a b = if a > b then a else b

let trdmin xs =
  let rec f xs =
    match xs with
    | Nil _ -> (inf, inf, inf)
    | Cons (h, t) ->
      let res = f t in
      (match res with
       | (a, b, c) ->
         (min a (max b h),
          min b (max c h),
          min c h))
  in
  match f xs with
  | (a, b, c) -> a

let program = dac trdmin