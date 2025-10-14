type mylist = Nil of unit | Cons of int * mylist

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let max a b = if a < b then b else a

let mps_p xs =
  let rec f xs i mps pos sum =
    match xs with
    | Nil _ -> pos
    | Cons (h, t) ->
      let sum = sum + h in
      let pos = if sum > mps then i else pos in
      let mps = max mps sum in
      f t (i + 1) mps pos sum
  in
  f xs 0 0 0 0

let program = single_pass mps_p