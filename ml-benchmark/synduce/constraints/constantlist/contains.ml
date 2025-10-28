@Input val w: int
type list = Elt of int | Cons of int * list

let head xs =
  match xs with
  | Elt w -> w
  | Cons (h, t) -> h

let rec is_const xs =
  match xs with
  | Elt x -> true
  | Cons (h, t) -> (h == head t) && is_const t

let rec spec xs =
  match xs with
  | Elt x -> if w == x then 1 else 0
  | Cons (h, t) -> if w == h then 1 else spec t

val target: list -> list compress
let rec target xs =
  match xs with
  | Elt a -> Elt a
  | Cons (h, t) -> Cons (h, t)

let program xs =
  if is_const xs then spec (target xs) else 0