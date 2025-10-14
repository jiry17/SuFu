type l = Elt of int | Cons of int * l

let rec insert y xs =
  match xs with
  | Elt x ->
      if y < x then Cons (y, Elt x) else Cons (x, Elt y)
  | Cons (h, t) ->
      if y < h then Cons (y, xs) else Cons (h, insert y t)

let rec sort xs =
  match xs with
  | Elt x -> Elt x
  | Cons (h, t) -> insert h (sort t)

let spec xs =
  match xs with
  | Elt x -> x
  | Cons (h, t) -> h

let rec target xs =
  match xs with
  | Elt x -> Elt x
  | Cons (h, t) -> Cons (h, target t)

let program xs = spec (sort (target xs))