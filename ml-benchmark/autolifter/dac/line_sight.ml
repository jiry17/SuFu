type list = Cons of int * list | Nil of unit

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
          (match f t (n - 1) with
           | (a, b) -> (Cons (h, a), b))
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
             (match split xs with
              | (a, b) -> concat (run a) (run b)))
  in
  v (run xs)

let inf = 100

let rec line_sight_f ma xs =
  match xs with
  | Nil _ -> true
  | Cons (h, t) ->
      (match t with
       | Nil _ -> h >= ma
       | _ ->
           if h > ma then line_sight_f h t else line_sight_f ma t)

let line_sight xs = line_sight_f (0 - inf) xs

let program = dac line_sight