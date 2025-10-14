type l = Nil | Cons of int * l

let inf = 100
let two = 2
let three = 3

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons(h, t) -> Cons(h, run t)
  in
  fun xs -> v (run xs)

let rec count1s2s3s_f s1 s2 xs =
  match xs with
  | Nil -> 0
  | Cons(h, t) ->
    let upd = if h = 3 && (s1 || s2) then 1 else 0 in
    let s2 = (h = 2) && (s1 || s2) in
    let s1 = h = 1 in
    upd + count1s2s3s_f s1 s2 t

let count1s2s3s = count1s2s3s_f false false

let program = single_pass count1s2s3s