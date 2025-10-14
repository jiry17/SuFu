type btree = Empty of unit | Node of int * btree * btree
type zipper = Top of unit | Left of int * btree * zipper | Right of int * btree * zipper

let rec sum t =
  match t with
  | Empty _ -> 0
  | Node (a, l, r) -> a + sum l + sum r

let rec repr z =
  match z with
  | Top _ -> Empty ()
  | Left (w, tree, zz) ->
      let tw = sum tree in
      Node (w, tree, repr zz)
  | Right (w, tree, zz) ->
      let tw = sum tree in
      Node (w, repr zz, tree)

let program z = sum (repr z)