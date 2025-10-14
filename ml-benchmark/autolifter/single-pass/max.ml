type list_ = Cons of int * list_ | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let rec max xs =
  match xs with
  | Nil _ -> 0 - inf
  | Cons (h, t) ->
    let res = max t in
    if res < h then h else res

let program = single_pass max