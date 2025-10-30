type l = Elt of int | Cons of int * l

let is_sorted xs =
  let rec aux pre xs =
    match xs with
    | Elt x -> pre >= x
    | Cons (h, t) -> pre >= h && aux h t
  in match xs with
    | Elt x -> true
    | Cons (h, t) -> aux h t

let abs x = if x < 0 then 0 - x else x

let min x y = if x < y then x else y

let min_diff w =
  let rec f xs =
    match xs with
    | Elt x -> 0 - abs (x - w)
    | Cons (h, t) -> min (0 - abs (h - w)) (f t)
  in
  f

let rec spec xs =
  match xs with
  | Elt x -> (0, x)
  | Cons (h, t) ->
      match spec t with
      | (a, b) -> (min (min_diff h t) a, h)

val target: l -> l compress
let rec target xs =
  match xs with
  | Elt w -> xs
  | Cons (h, t) -> Cons (h, target t)

let program xs =
  if is_sorted xs then spec (target xs) else (0, 0)