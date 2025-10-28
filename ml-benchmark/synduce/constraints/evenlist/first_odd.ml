type list = Elt of int | Cons of int * list

let mod2 x = x - ((x / 2) * 2)

let rec is_even_pos xs =
  match xs with
  | Elt x -> (x > 0) && (mod2 x == 0)
  | Cons (h, t) -> ((h > 0) && (mod2 h == 0)) && is_even_pos t

let rec spec xs =
  match xs with
  | Elt x -> if mod2 x == 1 then x else 0
  | Cons (h, t) -> if mod2 h == 1 then h else spec t

val target: list -> list compress
let target xs =
  match xs with
  | Elt x -> Elt x
  | Cons (h, t) -> Cons (h, t)

let program xs =
  if is_even_pos xs then spec (target xs) else 0