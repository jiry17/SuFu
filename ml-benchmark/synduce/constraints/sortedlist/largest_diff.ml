type l = Elt of int | Cons of int * l

let is_sorted xs =
  let rec aux pre xs =
    match xs with
    | Elt x -> pre <= x
    | Cons (h, t) -> (pre <= h) && aux h t
  in
  match xs with
  | Elt x -> true
  | Cons (h, t) -> aux h t

let abs x = if x < 0 then 0 - x else x
let max x y = if x < y then y else x

let rec max_diff w xs =
  match xs with
  | Elt x -> abs (x - w)
  | Cons (h, t) -> max (abs (h - w)) (max_diff w t)

let spec xs =
  let rec f xs =
    match xs with
    | Elt x -> (0, x)
    | Cons (h, t) -> (max (max_diff h t) (fst (f t)), h)
  in
  fst (f xs)

let rec target xs =
  match xs with
  | Elt w -> xs
  | Cons (h, t) -> Cons (h, target t)

let program xs = if is_sorted xs then spec (target xs) else 0