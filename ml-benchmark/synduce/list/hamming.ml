type zipCList = Cnil of unit | Single of int * int | Concat of zipCList * zipCList
type zipList = Nil of unit | Cons of int * int * zipList

let rec cat xs ys =
  match xs with
  | Nil _ -> ys
  | Cons (a, b, t) -> Cons (a, b, cat t ys)

val repr: zipCList -> zipList compress
let rec repr xs =
  match xs with
  | Cnil _ -> Nil ()
  | Single (a, b) -> Cons (a, b, Nil ())
  | Concat (a, b) -> cat (repr a) (repr b)

let rec spec xs =
  match xs with
  | Nil _ -> 0
  | Cons (a, b, t) -> if a == b then 1 + spec t else spec t

let program xs = spec (repr xs)