@Input val winp: int
type list = Elt of int | Cons of int * list

let rec aux pre xs =
  match xs with
  | Elt x -> pre <= x
  | Cons (h, t) -> (pre <= h) && aux h t

let is_sorted xs =
  match xs with
  | Elt x -> true
  | Cons (h, t) -> aux h t

let rec spec xs =
  match xs with
  | Elt a -> if a < winp then 1 else 0
  | Cons (h, t) ->
      let c = if h < winp then 1 else 0 in
      c + spec t

val target: list -> list compress
let rec target xs =
  match xs with
  | Elt w -> xs
  | Cons (h, t) ->
      if h < winp then Cons (h, target t) else xs

let program xs =
  if is_sorted xs then spec (target xs) else 0