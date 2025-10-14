type mylist = Elt of int | Cons of int * mylist
type clist = Single of int | Concat of clist * clist

let max x y = if x > y then x else y

let rec spec xs =
  match xs with
  | Elt a -> a
  | Cons (hd, tl) -> max hd (spec tl)

let rec cat xs ys =
  match xs with
  | Elt a -> Cons (a, ys)
  | Cons (a, b) -> Cons (a, cat b ys)

val repr: clist -> mylist compress
let rec repr xs =
  match xs with
  | Single a -> Elt a
  | Concat (a, b) -> cat (repr a) (repr b)

let program x = spec (repr x)