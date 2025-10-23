type tree = Leaf of unit | Node of int * tree * tree
type 'a plist = PNil of unit | PCons of 'a * 'a plist
type ptree = PLeaf of unit | PNode of int * ptree plist

val repr: ptree -> tree compress
let rec repr pt =
  match pt with
  | PLeaf _ -> Leaf ()
  | PNode (a, xs) ->
      let rec l2t xs =
        match xs with
        | PNil _ -> Leaf ()
        | PCons (h, t) -> Node (0, repr h, l2t t)
      in
      Node (a, Leaf (), l2t xs)

let rec spec t =
  match t with
  | Leaf _ -> 1
  | Node (a, l, r) -> a * (spec l * spec r)

let program x = spec (repr x)