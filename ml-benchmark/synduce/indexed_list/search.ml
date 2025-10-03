type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of int * list

type indexed_list =
  | CNil
  | CCons of int * int * indexed_list

@Input val w: int 

let rec length = function
  | Nil -> 0
  | Cons (_, tl) -> 1 + length tl

let rec add_indices = function
  | Nil -> CNil
  | Cons (value, tl) -> CCons (value, length tl, add_indices tl)

val repr: list -> indexed_list compress 
let rec repr = function 
| Nil -> CNil 
| Cons (h, t) -> CCons (h, length t, repr t)

let rec spec = function 
| CNil -> 0 
| CCons (h, i, t) -> if h == w then i else spec t

let prog xs = spec (repr xs)