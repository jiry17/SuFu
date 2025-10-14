type tlist = Single of int | Cons of int * tlist
type clist = Elt of int | Cat of clist * clist

let max a b = if a < b then b else a

let spec xs =
  let rec f xs =
    match xs with
    | Single a -> (a, a, true)
    | Cons (hd, tl) ->
        let result = f tl in
        match result with
        | (_, r2, _) -> (hd, max r2 hd, hd > r2)
  in
  match f xs with
  | (_, _, b) -> b

let rec first = function
| Single a -> a
| Cons (a, _) -> a 

let rec last = function
| Single a -> a
| Cons (_, t) -> last t

let rec maximum = function 
| Single a -> a 
| Cons (h, t) -> max h (maximum t)

let rec cat_list xs ys =
  match xs with
  | Single a -> Cons (a, ys)
  | Cons (a, b) -> Cons (a, cat_list b ys)

val repr: clist -> tlist compress
let rec repr xs =
  match xs with
  | Elt a -> Single a
  | Cat (a, b) -> cat_list (repr a) (repr b)

let program x = spec (repr x)