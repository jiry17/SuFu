type lst = Cons of int * lst | Nil of unit

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) ->
      let r = run t in
      Cons (h, r)
  in
  fun xs -> v (run xs)

let inf = 100

let max a b = if a < b then b else a

let rec aux cs xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) ->
    let cs2 = if h = 0 then 0 else cs + 1 in
    let r = aux cs2 t in
    max cs2 r

let max_dist_between_zeros xs = aux 0 xs

let program = single_pass max_dist_between_zeros