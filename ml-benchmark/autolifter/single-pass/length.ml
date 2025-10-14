type list =
  | Cons of int * list
  | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let rec length x =
  match x with
  | Cons (h, t) -> length t + 1
  | Nil _ -> 0

let program = single_pass length