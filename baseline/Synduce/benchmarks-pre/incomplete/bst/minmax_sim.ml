(** @synduce -s 2 -NB --no-lifting -b 3 *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let rec is_bst = function
  | Leaf x -> true
  | Node (a, l, r) -> a >= tree_max l && a <= tree_min r && is_bst l && is_bst r

and tree_min = function
  | Leaf x -> x
  | Node (a, l, r) -> min a (min (tree_min l) (tree_min r))

and tree_max = function
  | Leaf x -> x
  | Node (a, l, r) -> max a (max (tree_max l) (tree_max r))
;;

let repr x = x

let rec spec = function
  | Leaf a -> a, a
  | Node (a, l, r) ->
    let x1, y1 = spec l in
    let x2, y2 = spec r in
    max x1 (max x2 a), min y1 (min y2 a)
;;

let rec target = function
  | Leaf a -> [%synt xi_0] a
  | Node (a, l, r) -> [%synt xi_2] a (amin l) (amax r)
  [@@requires is_bst]

and amin = function
  | Leaf a -> a
  | Node (a, l, r) -> min a (min (amin l) (amin r))

and amax = function
  | Leaf a -> a
  | Node (a, l, r) -> max a (max (amax l) (amax r))
;;