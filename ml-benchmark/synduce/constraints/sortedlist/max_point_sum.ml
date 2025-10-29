type mylist = Elt of int * int | Cons of int * int * mylist
type clist = Single of int * int | Concat of clist * clist

let rec cat a b =
  match a with
  | Elt (x, y) -> Cons (x, y, b)
  | Cons (x, y, t) -> Cons (x, y, cat t b)

let rec repr xs =
  match xs with
  | Single (a, b) -> Elt (a, b)
  | Concat (a, b) -> cat (repr a) (repr b)

let is_sorted =
  let rec aux pre xs =
    match xs with
    | Elt (a, b) -> pre <= a + b
    | Cons (a, b, t) -> (pre <= a + b) && aux (a + b) t
  in
  fun xs ->
    match xs with
    | Elt _ -> true
    | Cons (a, b, t) -> aux (a + b) t

let c_sorted xs = is_sorted (repr xs)

let max x y = if x < y then y else x

let rec spec xs =
  match xs with
  | Elt (a, b) -> a + b
  | Cons (a, b, t) -> max (spec t) (a + b)

val target: clist -> clist compress
let rec target xs =
  match xs with
  | Single (a, b) -> xs
  | Concat (l, r) -> Concat (l, target r)

let program xs =
  if c_sorted xs then spec (repr (target xs)) else 0