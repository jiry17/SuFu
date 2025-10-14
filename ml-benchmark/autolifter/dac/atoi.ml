type list = Nil of unit | Cons of int * list

let rec length x =
  match x with
  | Cons (_, t) -> length t + 1
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
          | (a, b) -> (Cons (h, a), b)
      | _ -> (Nil (), Nil ())
  in
  f xs (length xs / 2)

let dac v =
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
  fun xs -> v (run xs)

let base = 10

let rec atoi_aux pre xs =
  match xs with
  | Nil _ -> pre
  | Cons (h, t) -> atoi_aux ((base * pre) + h) t

let atoi xs = atoi_aux 0 xs

let program = dac atoi