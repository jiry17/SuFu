type l = Cons of int * l | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let max a b = if a < b then b else a

let mas =
  let rec f p np xs =
    match xs with
    | Nil _ -> max p np
    | Cons (h, t) ->
        max (max p np) (f ((max np 0) + h) ((max p 0) - h) t)
  in
  f 0 0

let expected xs =
  let res =
    let rec f p np xs =
      match xs with
      | Nil _ -> (max p 0, max np 0)
      | Cons (h, t) ->
          f ((max np 0) + h) ((max p 0) - h) t
    in
    f 0 0 xs
  in
  match res with
  | a, b -> max a b

let program = single_pass mas