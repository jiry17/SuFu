type list =
  | Nil of unit
  | Cons of (int * int) * list

type clist =
  | CNil of unit
  | Single of int * int
  | Concat of clist * clist

let rec cat a b =
  match a with
  | Nil _ -> b
  | Cons (h, t) -> Cons (h, cat t b)

@Input val xinp: int

val repr: clist -> list compress
let rec repr xs =
  match xs with
  | CNil _ -> Nil ()
  | Single x -> Cons (x, Nil ())
  | Concat (a, b) -> cat (repr a) (repr b)

let fi = function (x, _) -> x 
let se = function (_, x) -> x

let rec spec xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) ->
    if xinp == fi h then se h + spec t else spec t

let prog x = spec (repr x)