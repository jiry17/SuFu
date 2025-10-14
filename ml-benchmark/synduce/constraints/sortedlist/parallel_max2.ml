type list = Elt of int | Cons of int * list
type clist = Single of int | Concat of clist * clist

let rec cat a b =
  match a with
  | Elt x -> Cons (x, b)
  | Cons (h, t) -> Cons (h, cat t b)

let rec repr xs =
  match xs with
  | Single x -> Elt x
  | Concat (a, b) -> cat (repr a) (repr b)

let max a b = if a < b then b else a

let rec lmax xs =
  match xs with
  | Single x -> x
  | Concat (x, y) -> max (lmax x) (lmax y)

let min a b = if a < b then a else b

let rec lmin xs =
  match xs with
  | Single x -> x
  | Concat (x, y) -> min (lmin x) (lmin y)

let rec is_part c =
  match c with
  | Single _ -> true
  | Concat (x, y) -> (lmin x) > (lmax y) && is_part x && is_part y

let rec spec xs =
  match xs with
  | Elt x -> x
  | Cons (h, t) -> max h (spec t)

let rec target c =
  match c with
  | Single _ -> c
  | Concat (x, y) -> Concat (target x, y)

let program xs =
  if is_part xs then spec (repr (target xs)) else 0