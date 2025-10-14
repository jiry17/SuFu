type list_t = Nil | Cons of int * list_t

let single_pass v =
  let rec run xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let zsos =
  let rec f an xs =
    match xs with
    | Nil -> true
    | Cons (h, t) ->
        let an = an && (h = 1) in
        if (h = 0) || an then f an t else false
  in
  f true

let program = single_pass zsos