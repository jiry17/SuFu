type list_ = Cons of int * list_ | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let min a b = if a < b then a else b
let max a b = if a > b then a else b

let sndmin xs =
  match
    let rec f xs =
      match xs with
      | Nil _ -> (inf, inf)
      | Cons (h, t) ->
        let res = f t in
        match res with
        | (r1, r2) -> (min r1 (max r2 h), min r2 h)
    in
    f xs
  with
  | (a, b) -> a

let program = single_pass sndmin