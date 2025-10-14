type clist = CNil of unit | Single of int | Concat of clist * clist
type lst = Nil of unit | Cons of int * lst

let rec cat x y =
  match x with
  | Cons (h, t) -> Cons (h, cat t y)
  | Nil _ -> y

let rec repr cl =
  match cl with
  | CNil _ -> Nil ()
  | Single h -> Cons (h, Nil ())
  | Concat (l, r) -> cat (repr l) (repr r)

let rec spec xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> h + spec t

let program cl = spec (repr cl)