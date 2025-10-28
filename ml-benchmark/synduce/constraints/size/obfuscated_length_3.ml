type list = Elt of int | Cons of int * list

let rec insert y xs =
  match xs with
  | Elt x -> if y < x then Cons (y, Elt x) else Cons (x, Elt y)
  | Cons (h, t) -> if y < h then Cons (y, xs) else Cons (h, insert y t)

let rec sort xs =
  match xs with
  | Elt x -> Elt x
  | Cons (h, t) -> insert h (sort t)

let rec len xs =
  match xs with
  | Elt _ -> 0
  | Cons (_, t) -> 1 + len t

let is_length_gt2 xs = len xs >= 2

val target: list -> list compress
let rec target xs =
  match xs with
  | Elt x -> Elt x
  | Cons (h, t) -> Cons (h, target t)

let program xs =
  if is_length_gt2 xs then len (sort (target xs)) else 0