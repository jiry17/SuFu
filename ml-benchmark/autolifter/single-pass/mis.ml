type l = Nil of unit | Cons of int * l

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let max a b = if a < b then b else a

let rec mis_f p np xs =
  match xs with
  | Nil _ -> max p np
  | Cons (h, t) -> mis_f (h + np) (max p np) t

let mis xs = mis_f 0 0 xs

let program = single_pass mis