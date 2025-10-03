config EnableDeepCoder = true

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

let rec sum_lt_pos = function
  | CNil -> 0
  | CCons (hd, i, tl) -> if hd > i then hd + sum_lt_pos tl else sum_lt_pos tl

let prog xs = sum_lt_pos (repr xs)