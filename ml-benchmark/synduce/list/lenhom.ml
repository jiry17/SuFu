type mylist = Nil of unit | Cons of int * mylist
type clist = CNil of unit | Single of int | Concat of clist * clist

let rec spec x =
  match x with
  | Nil _ -> 0
  | Cons (h, t) -> 1 + spec t

let rec cat a b =
  match a with
  | Nil _ -> b
  | Cons (h, t) -> Cons (h, cat t b)

val repr: clist -> mylist compress
let rec repr xs =
  match xs with
  | CNil _ -> Nil ()
  | Single x -> Cons (x, Nil ())
  | Concat (a, b) -> cat (repr a) (repr b)

let program x = spec (repr x)