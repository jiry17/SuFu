type btree = Empty of unit | Node of int * btree * btree
type zipper = Top of unit | Left of int * btree * zipper | Right of int * btree * zipper

let max a b = if a < b then b else a

let rec mpath t =
  match t with
  | Empty _ -> 0
  | Node (a, l, r) -> a + max (mpath l) (mpath r)

val repr: zipper -> btree compress
let rec repr z =
  match z with
  | Top _ -> Empty ()
  | Left (w, tree, zz) ->
      let tw = mpath tree in
      Node (w, tree, repr zz)
  | Right (w, tree, zz) ->
      let tw = mpath tree in
      Node (w, repr zz, tree)

let program z = mpath (repr z)