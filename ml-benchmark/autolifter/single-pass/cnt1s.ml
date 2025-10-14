type lst = Nil of unit | Cons of int * lst

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let inf = 100

let cnt1s =
  let rec f pre cnt xs =
    match xs with
    | Nil _ -> cnt
    | Cons (h, t) ->
      let cnt = if (pre = 0) && (h = 1) then cnt + 1 else cnt in
      f h pre t
  in
  f 0 0

let program = single_pass cnt1s