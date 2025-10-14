type t = Cons of int * t | Nil

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) ->
        let r = run t in
        Cons (h, r)
  in
  fun xs -> v (run xs)

let inf = 100

let max a b = if a < b then b else a

let longest10s2 =
  let rec f s0 len xs =
    match xs with
    | Nil -> 0
    | Cons (h, t) ->
        let s1 = s0 && (h = 2) in
        let s0 = (h = 1) || ((h = 0) && s0) in
        let len = if s0 || s1 then len + 1 else 0 in
        let upd = if s1 then len else 0 in
        let len = if h = 1 then 1 else if s1 then 0 else len in
        max upd (f s0 len t)
  in
  f false 0

let program = single_pass longest10s2