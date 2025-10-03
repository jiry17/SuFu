config NonLinear = true

type nat =
  | Z
  | S of nat

type list =
  | Nil
  | Cons of int * list

type compressed_list =
  | CNil
  | CCons of nat * int * compressed_list

let rec repeat a cont = function
  | Z -> cont
  | S n -> Cons (a, repeat a cont n)

let rec stutter = function
  | CNil -> Nil
  | CCons (hdc, hdv, tl) -> repeat hdv (stutter tl) hdc

let rec sum_list = function
  | Nil -> 0
  | Cons (hd, tl) -> hd + sum_list tl

let rec value = function
  | Z -> 0
  | S n -> 1 + value n

val repr: compressed_list -> list compress 
let rec repr = function 
  | CNil -> Nil
  | CCons (n, h, t) ->
    let w = value n in 
    repeat h (repr t) n 

let prog xs = sum_list (repr xs)
  