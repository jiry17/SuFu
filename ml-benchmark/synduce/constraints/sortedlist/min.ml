type l = Elt of int | Cons of int * l

let is_sorted =
  let rec aux pre xs =
    match xs with
    | Elt x -> pre <= x
    | Cons (h, t) -> (pre <= h) && aux h t
  in
  fun xs ->
    match xs with
    | Elt _ -> true
    | Cons (h, t) -> aux h t

let min a b = if a > b then b else a

let rec spec xs =
  match xs with
  | Elt w -> w
  | Cons (h, t) -> min h (spec t)

let rec target xs =
  match xs with
  | Elt _ -> xs
  | Cons (h, _) -> xs

let program xs = if is_sorted xs then spec (target xs) else 0