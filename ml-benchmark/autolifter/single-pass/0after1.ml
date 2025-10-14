type list_ = Nil of unit | Cons of int * list_

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) ->
      let t2 = run t in
      Cons (h, t2)
  in
  fun xs -> v (run xs)

let inf = 100

let zafter1 =
  let rec f seen1 xs =
    match xs with
    | Nil _ -> false
    | Cons (h, t) ->
      if seen1 && h = 0 then true
      else f (seen1 || h = 1) t
  in
  fun xs -> f false xs

let program = single_pass zafter1