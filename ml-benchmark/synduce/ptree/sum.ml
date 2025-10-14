type tree = Leaf | Node of int * tree * tree
and ptree = PLeaf | PNode of int * plist
and plist = PNil | PCons of ptree * plist

let rec repr pt =
  match pt with
  | PLeaf -> Leaf
  | PNode (a, xs) ->
      let rec l2t xs2 =
        match xs2 with
        | PNil -> Leaf
        | PCons (h, t) ->
            let left = repr h in
            let right = l2t t in
            Node (0, left, right)
      in
      let mid = Leaf in
      let right2 = l2t xs in
      Node (a, mid, right2)

let rec spec t =
  match t with
  | Leaf -> 0
  | Node (a, l, r) ->
      let sl = spec l in
      let sr = spec r in
      let ssum = sl + sr in
      a + ssum

let program x = spec (repr x)