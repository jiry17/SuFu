type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec sum = function
  | Nil -> 0
  | Cons (hd, tl) -> hd + sum tl

let max a b = if a > b then a else b

let rec mts = function
  | Nil -> 0
  | Cons (hd, tl) -> max (hd + sum tl) (mts tl)

let spec l = (sum l, mts l)

val repr: int list -> (int list) compress
let rec repr = function 
  | Nil -> Nil 
  | Cons (h, t) -> Cons (h, repr t)

let prog xs = spec (repr xs)