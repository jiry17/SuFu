type list_ = Elt of int | Cons of int * list_
type clist = Single of int | Concat of clist * clist

let max x y = if x > y then x else y

let fst = function (x, _) -> x
let snd = function (_, y) -> y

let rec f xs =
  match xs with
  | Elt a -> (a, 1)
  | Cons (hd, tl) ->
    let result = f tl in
    let new_max = max (fst result) hd in
    let new_cnt = if hd > fst result then 1 else (snd result) + (if hd == fst result then 1 else 0) in
    (new_max, new_cnt)

let spec xs = snd (f xs)

let rec cat xs ys =
  match xs with
  | Elt a -> Cons (a, ys)
  | Cons (a, b) -> Cons (a, cat b ys)

val repr: clist -> list_ compress
let rec repr xs =
  match xs with
  | Single a -> Elt a
  | Concat (a, b) -> cat (repr a) (repr b)

let program x = spec (repr x)