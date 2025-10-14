type list_ = Nil of unit | Cons of int * list_
type clist = CNil of unit | Single of int | Concat of clist * clist

let mod2 x = x - ((x / 2) * 2)

let rec spec xs =
  match xs with
  | Nil _ -> 0
  | Cons (hd, tl) ->
      (if mod2 hd = 1 then hd else 0) + (spec tl)

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