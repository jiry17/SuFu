type list_ = Nil | Cons of int * list_

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let count10p =
  let rec f s0 s1 xs =
    match xs with
    | Nil -> 0
    | Cons (h, t) ->
      let upd = if s1 && (h = 1) then 1 else 0 in
      let s1 = (h = 0) && (s0 || s1) in
      let s0 = h = 1 in
      upd + f s0 s1 t
  in
  fun xs -> f false false xs

let program = single_pass count10p