type list_ =
  | Cons of int * list_
  | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let rec mps l =
  match l with
  | Nil _ -> 0
  | Cons (h, t) ->
      let res = mps t in
      if 0 < res + h then 0 else res + h

let program = single_pass mps