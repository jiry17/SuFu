type list_t =
  | Cons of int * list_t
  | Nil

let base = 10

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) ->
      Cons (h, run t)
  in
  fun xs -> v (run xs)

let rec atoi xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) ->
    base * (atoi t) + h

let program = single_pass atoi