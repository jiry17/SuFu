type t = Elt of int | Cons of int * t

let insert y =
  let rec f xs =
    match xs with
    | Elt x ->
        if y > x then Cons (y, Elt x) else Cons (x, Elt y)
    | Cons (h, t) ->
        if y > h then Cons (y, xs) else Cons (h, f t)
  in
  f

let sort =
  let rec f xs =
    match xs with
    | Elt x -> Elt x
    | Cons (h, t) -> insert h (f t)
  in
  f

let spec =
  let rec f xs =
    match xs with
    | Elt x -> if x >= 0 then x else 0
    | Cons (h, t) -> if h >= 0 then h + f t else 0
  in
  f

let target =
  let rec f xs =
    match xs with
    | Elt x -> Elt x
    | Cons (h, t) -> Cons (h, f t)
  in
  f

let program xs = spec (sort (target xs))