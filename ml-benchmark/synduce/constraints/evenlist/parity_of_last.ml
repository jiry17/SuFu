type list = Elt of int | Cons of int * list

let mod2 x = x - ((x / 2) * 2)

let rec is_even xs =
  match xs with
  | Elt x -> x > 0 && mod2 x = 0
  | Cons (h, t) -> (h > 0 && mod2 h = 0) && is_even t

let rec spec xs =
  match xs with
  | Elt x -> mod2 x
  | Cons (_, t) -> spec t

let target xs =
  match xs with
  | Elt x -> Elt x
  | Cons (h, t) -> Cons (h, t)

let program xs =
  if is_even xs then spec (target xs) else 0