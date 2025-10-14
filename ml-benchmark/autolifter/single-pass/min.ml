type list_ = Cons of int * list_ | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let rec min xs =
  match xs with
  | Nil _ -> inf
  | Cons (h, t) ->
      let res = min t in
      if res < h then res else h

let program = single_pass min