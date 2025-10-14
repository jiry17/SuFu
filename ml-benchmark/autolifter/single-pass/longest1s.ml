type list = Cons of int * list | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) ->
      Cons (h, run t)
  in
  fun xs ->
    v (run xs)

let inf = 100

let max a b =
  if a < b then b else a

let longest1s =
  let rec f len xs =
    match xs with
    | Nil _ -> 0
    | Cons (h, t) ->
      let len = if h = 1 then len + 1 else 0 in
      max len (f len t)
  in
  fun xs -> f 0 xs

let program = single_pass longest1s