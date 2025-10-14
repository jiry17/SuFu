config EnableDeepCoder = true 
type list = Nil | Cons of int * list
type clist = CNil | Single of int | Concat of clist * clist

let max a b = if a < b then b else a

let fst = function (x, _) -> x
let snd = function (_, y) -> y

let spec xs =
  let rec f xs =
    match xs with
    | Nil -> (0, 0)
    | Cons (hd, tl) ->
        let result = f tl in
        let new_mps = max 0 (hd + fst result) in
        let new_sum = hd + snd result in
        (new_mps, new_sum)
  in
  fst (f xs)

let rec cat xs ys =
  match xs with
  | Nil -> ys
  | Cons (a, b) -> Cons (a, cat b ys)

val repr: clist -> list compress
let rec repr xs =
  match xs with
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (a, b) -> cat (repr a) (repr b)

let program x = spec (repr x)