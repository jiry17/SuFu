config EnableDeepCoder = true

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec sum = function
  | Nil -> 0
  | Cons (hd, tl) -> hd + sum tl

let max a b = if a > b then a else b

let rec mts s = function 
  | Nil -> s
  | Cons (hd, tl) -> mts (max (s + hd) 0) tl

let rec mps = function
  | Nil -> 0
  | Cons (hd, tl) -> max (mps tl + hd) 0

let spec l = (mts 0 l, mps l)

val repr: int list -> (int list) compress
let rec repr = function 
  | Nil -> Nil 
  | Cons (h, t) -> Cons (h, repr t)

let prog xs = spec (repr xs)