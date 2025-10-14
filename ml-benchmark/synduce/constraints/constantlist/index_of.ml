type list = Elt of int | Cons of int * list

let head xs =
  match xs with
  | Elt w -> w
  | Cons (h, t) -> h

let rec is_const xs =
  match xs with
  | Elt x -> true
  | Cons (h, t) -> (h = head t) && is_const t

let w = read_int ()

let rec spec xs =
  match xs with
  | Elt x -> if w = x then 1 else 0
  | Cons (h, t) -> if w = h then 1 else if spec t = 0 then 0 else 1 + spec t

let rec target xs =
  match xs with
  | Elt a -> Elt a
  | Cons (h, t) -> Cons (h, t)

let program xs =
  if is_const xs then spec (target xs) else 0