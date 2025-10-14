type l = Elt of int | Cons of int * l
type c = Single of int | Concat of c * c

let min = fun x -> fun y -> if x < y then x else y

let rec spec = fun xs ->
  match xs with
  | Elt a -> a
  | Cons (hd, tl) -> min hd (spec tl)

let rec cat = fun xs -> fun ys ->
  match xs with
  | Elt a -> Cons (a, ys)
  | Cons (a, b) -> Cons (a, cat b ys)

val repr: c -> l compress
let rec repr = fun xs ->
  match xs with
  | Single a -> Elt a
  | Concat (a, b) -> cat (repr a) (repr b)

let program = fun x -> spec (repr x)