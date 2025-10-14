type l = Cons of int * l | Nil of unit

let is_even a = a = 2 * (a / 2)

let single_pass v xs =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  v (run xs)

let inf = 100

let max a b = if a < b then b else a

let longest00s =
  let rec f len xs =
    match xs with
    | Nil _ -> 0
    | Cons (h, t) ->
      let len = if h = 0 then len + 1 else 0 in
      if is_even len then max len (f len t) else f len t
  in
  fun xs -> f 0 xs

let program = single_pass longest00s