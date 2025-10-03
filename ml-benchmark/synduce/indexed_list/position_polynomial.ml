config EnableDeepCoder = true
config NonLinear = true

type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of int * list

type indexed_list =
  | CNil
  | CCons of int * int * indexed_list

let rec length = function
  | Nil -> 0
  | Cons (_, tl) -> 1 + length tl

val repr: list -> indexed_list compress
let rec repr = function
  | Nil -> CNil
  | Cons (value, tl) -> CCons (value, length tl, repr tl)

let rec polynome = function
  | CNil -> 0
  | CCons (hd, i, tl) -> (hd * i) + polynome tl

let prog xs = polynome (repr xs)