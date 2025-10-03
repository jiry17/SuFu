type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let min a b = if a < b then a else b
let max a b = if a > b then a else b

let rec tree_min = function
  | Leaf x -> x
  | Node (a, l, r) -> min a (min (tree_min l) (tree_min r))

let rec tree_max = function
  | Leaf x -> x
  | Node (a, l, r) -> max a (max (tree_max l) (tree_max r))

@Input val lo: int 
@Input val hi: int

let rec spec = function 
| Leaf a -> lo < a && a < hi
| Node (a, l, r) -> (lo < a && a < hi) || spec l || spec r

val repr: int tree -> (int tree) compress
let rec repr = function
| Leaf a -> Leaf a 
| Node (a, l, r) -> Node (a, repr l, repr r)

let prog t = spec (repr t)
