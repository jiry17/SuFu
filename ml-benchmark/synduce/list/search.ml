type ilist = Nil of unit | Cons of int * ilist
type clist = CNil of unit | Single of int | Concat of clist * clist

let max a b = if a < b then b else a

let x = 0

let rec spec xs =
  match xs with
  | Nil _ -> false
  | Cons (hd, tl) -> (x = hd) || spec tl

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