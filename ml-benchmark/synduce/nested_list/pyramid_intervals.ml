config ComposeNum = 4

let max a b = if a > b then a else b
let min a b = if a < b then a else b

type list =
  | Elt of int
  | Cons of int * list

type nested_list =
  | Line of list
  | NCons of list * nested_list

let rec lmax = function
  | Elt x -> x
  | Cons (hd, tl) -> max (lmax tl) hd

let rec aux prev = function
  | Line x -> prev >= lmax x
  | NCons (hd, tl) -> prev >= lmax hd && aux (lmax hd) tl

let rec is_sorted = function
  | Line x -> true
  | NCons (hd, tl) -> aux (lmax hd) tl

let fi = function
  | (x, _) -> x 

let se = function 
  | (_, y) -> y

let rec interval = function
  | Elt x -> (x, x)
  | Cons (hd, tl) ->
    let res = interval tl in
    let lo = fi res in 
    let hi = se res in 
    (min hd lo, max hd hi)

let rec spec = function
  | Line a ->
    let i = interval a in
    (fi i, se i, true)
  | NCons (hd, tl) ->
    match spec tl with 
    | (plo, phi, pyramidal) ->
    let i = interval hd in
    let lo = fi i in
    let hi = se i in 
    (min lo plo, max hi phi, pyramidal && plo <= lo && hi >= phi)

val repr: nested_list -> nested_list compress
let rec repr = function 
  | Line x -> let info = interval x in Line x 
  | NCons (h, t) ->
    let info = interval h in 
    NCons (h, repr t)

let prog xs = spec (repr xs)