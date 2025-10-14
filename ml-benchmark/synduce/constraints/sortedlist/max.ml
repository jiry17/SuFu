type t = Elt of int | Cons of int * t

let is_sorted xs =
  let rec aux pre xs =
    match xs with
    | Elt x -> pre <= x
    | Cons (h, t) -> pre <= h && aux h t
  in
  match xs with
  | Elt x -> true
  | Cons (h, t) -> aux h t

let max a b =
  if a < b then b else a

let rec maximum xs =
  match xs with
  | Elt w -> w
  | Cons (h, t) -> max h (maximum t)

let target xs =
  match xs with
  | Elt w -> xs
  | Cons (h, _) -> xs

let program xs =
  if is_sorted xs then maximum (target xs) else 0