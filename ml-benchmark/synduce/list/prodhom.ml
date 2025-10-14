type list_ = Nil of unit | Cons of int * list_
type clist = CNil of unit | Single of int | Concat of clist * clist

let max a b = if a < b then b else a

let rec spec xs =
  match xs with
  | Nil _ -> 1
  | Cons (hd, tl) -> hd * spec tl

let rec cat xs ys =
  match xs with
  | Nil _ -> ys
  | Cons (a, b) -> Cons (a, cat b ys)

let rec repr xs =
  match xs with
  | CNil _ -> Nil ()
  | Single a -> Cons (a, Nil ())
  | Concat (a, b) -> cat (repr a) (repr b)

let program x = spec (repr x)