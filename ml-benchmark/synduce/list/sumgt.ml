type list_ = Nil of unit | Cons of int * list_
type clist = Cnil of unit | Single of int | Concat of clist * clist

let c = read_int ()

let rec spec xs =
  match xs with
  | Nil _ -> 0
  | Cons (hd, tl) ->
      let v = if hd > c then hd else 0 in
      v + spec tl

let rec cat xs ys =
  match xs with
  | Nil _ -> ys
  | Cons (a, b) -> Cons (a, cat b ys)

let rec repr xs =
  match xs with
  | Cnil _ -> Nil ()
  | Single a -> Cons (a, Nil ())
  | Concat (a, b) -> cat (repr a) (repr b)

let program x = spec (repr x)