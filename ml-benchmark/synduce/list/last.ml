type list = Elt of int | Cons of int * list
type clist = Single of int | Concat of clist * clist

let rec spec x =
  match x with
  | Elt a -> a
  | Cons (h, t) -> spec t

let rec cat a b =
  match a with
  | Elt a0 -> Cons (a0, b)
  | Cons (h, t) -> Cons (h, (cat t b))

val repr: clist -> list compress
let rec repr xs =
  match xs with
  | Single a -> Elt a
  | Concat (a, b) -> cat (repr a) (repr b)

let program x = spec (repr x)