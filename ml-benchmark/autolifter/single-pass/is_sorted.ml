type list = Nil of unit | Cons of int * list

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) ->
        Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let rec is_sorted_f pre xs =
  match xs with
  | Nil _ -> true
  | Cons (h, t) ->
      if pre >= h then false else is_sorted_f h t

let is_sorted xs =
  is_sorted_f (0 - inf) xs

let program = single_pass is_sorted