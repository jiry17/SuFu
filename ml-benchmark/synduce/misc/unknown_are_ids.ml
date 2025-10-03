type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec sum = function 
  | Nil -> 0
  | Cons (hd, tl) -> hd + sum tl

let rec concat xs ys = match xs with 
  | Nil -> ys 
  | Cons (h, t) -> Cons (h, concat t ys)

val repr: int clist -> (int list) compress 
let rec repr = function
  | CNil -> Nil 
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> concat (repr x) (repr y)

let prog xs = sum (repr xs)
