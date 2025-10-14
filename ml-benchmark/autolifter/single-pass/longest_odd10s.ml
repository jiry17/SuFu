type mylist = Cons of int * mylist | Nil

let is_even a = a = 2 * (a / 2)

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let max a b = if a < b then b else a

let rec f s1 s2 len xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) ->
      let s1 = s2 && (h = 1) in
      let s2 = h = 0 in
      let len = if s1 then 1 + len else if s2 then len else 0 in
      if is_even len then f s1 s2 len t else max len (f s1 s2 len t)

let longest_odd10s = f false false 0

let program = single_pass longest_odd10s