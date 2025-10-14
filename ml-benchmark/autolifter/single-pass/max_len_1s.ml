type list_ = Cons of int * list_ | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let max a b = if a < b then b else a

let rec f ma l xs =
  match xs with
  | Nil _ -> max ma l
  | Cons (h, t) ->
    if h = 0 then f (max ma l) 0 t else f ma (l + 1) t

let max1s xs = f 0 0 xs

let program = single_pass max1s