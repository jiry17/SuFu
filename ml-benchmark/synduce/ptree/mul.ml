type tree = Leaf of unit | Node of int * tree * tree
type ptree = Pleaf of unit | Pnode of int * plist
and plist = Pnil of unit | Pcons of ptree * plist

let rec repr pt =
  match pt with
  | Pleaf () -> Leaf ()
  | Pnode (a, xs) ->
      let rec l2t xs =
        match xs with
        | Pnil () -> Leaf ()
        | Pcons (h, t) -> Node (0, repr h, l2t t)
      in
      Node (a, Leaf (), l2t xs)

let rec spec t =
  match t with
  | Leaf () -> 1
  | Node (a, l, r) -> a * (spec l * spec r)

let program x = spec (repr x)