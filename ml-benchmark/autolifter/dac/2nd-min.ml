type l = Nil | Cons of int * l

let rec length x =
  match x with
  | Cons (_, t) -> 1 + length t
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
          (match f t (n - 1) with
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
             (match split xs with
              | (a, b) -> concat (run a) (run b)))
  in
  v (run xs)

let inf = 100

let min a b = if a < b then a else b
let max a b = if a > b then a else b

let sndmin xs =
  (match
     let rec f xs =
       match xs with
       | Nil -> (inf, inf)
       | Cons (h, t) ->
           (match f t with
            | (a, b) -> (min a (max b h), min b h))
     in
     f xs
   with
   | (a, b) -> a)

let program = dac sndmin