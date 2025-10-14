type lst = Nil of unit | Cons of int * lst

let single_pass v =
  let rec run xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (h, run t)
  in
  fun xs -> v (run xs)

let balanced =
  let rec f cnt xs =
    match xs with
    | Nil _ -> true
    | Cons (h, t) ->
      let cnt = cnt + h in
      if cnt < 0 then false else f cnt t
  in
  fun xs -> f 0 xs

let program = single_pass balanced