type slist = Elt of int | Cons of int * slist
type clist = Single of int | Concat of clist * clist

let rec cat_list xs ys =
  match xs with
  | Elt a -> Cons (a, ys)
  | Cons (a, b) -> Cons (a, cat_list b ys)

let rec first = function 
  | Elt a -> a
  | Cons (a, _) -> a

let rec last = function 
  | Elt a -> a
  | Cons (_, t) -> last t

val repr: clist -> slist compress
let rec repr xs =
  match xs with
  | Single a -> Elt a
  | Concat (a, b) -> cat_list (repr a) (repr b)

let spec xs =
  let rec f xs =
    match xs with
    | Elt a -> (a, a, true)
    | Cons (hd, tl) ->
        let r = f tl in
        match r with
        | (r1, r2, r3) -> (hd, r2, r3 && hd < r1)
  in
  let t = f xs in
  match t with
  | (_, _, b) -> b

let program xs = spec (repr xs)