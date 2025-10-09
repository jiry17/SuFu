config NonLinear = true
config SampleSize = 6
config EnableDeepCoder = true

type list =
  | Nil
  | Cons of int * list

type clist =
  | CNil
  | Single of int
  | Concat of clist * clist

let base = 10

let rec cat a b =
  match a with
  | Nil -> b
  | Cons (h, t) -> Cons (h, cat t b)

val repr: clist -> list compress
let rec repr xs =
  match xs with
  | CNil -> Nil
  | Single x -> Cons (x, Nil)
  | Concat (a, b) -> cat (repr a) (repr b)

let fi = function (x, _) -> x 
let se = function (_, x) -> x

let rec spec xs =
  let rec f x =
    match x with
    | Nil -> (0, 1)
    | Cons (h, t) ->
      let result = f t in
      (h + 10 * fi result, 10 * se result)
  in
  fi (f xs)

let prog x = spec (repr x)