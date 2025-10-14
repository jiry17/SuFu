type list_ =
  | Cons of int * list_
  | Nil

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let dropwhile =
  let rec f i xs =
    match xs with
    | Nil -> i
    | Cons (h, t) ->
        if h > 0 then i else f (i + 1) t
  in
  fun xs -> f 0 xs

let program = single_pass dropwhile