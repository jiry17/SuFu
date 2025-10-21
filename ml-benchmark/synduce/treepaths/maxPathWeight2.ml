type btree = Empty of unit | Node of int * btree * btree
type zipper = Top of unit | Left of int * btree * zipper | Right of int * btree * zipper

let max a b = if a < b then b else a

let rec mpath t =
  match t with
  | Empty _ -> 0
  | Node (a, l, r) -> a + max (mpath l) (mpath r)

let rec repr z =
  match z with
  | Top _ -> Empty ()
  | Left (w, tree, zz) -> Node (w, tree, repr zz)
  | Right (w, tree, zz) -> Node (w, repr zz, tree)

val tree_rec: btree -> btree compress
let rec tree_rec t =
  match t with
  | Empty _ -> Empty ()
  | Node (a, l, r) -> Node (a, tree_rec l, tree_rec r)

val zip_rec: zipper -> zipper compress
let rec zip_rec z =
  match z with
  | Top _ -> Top ()
  | Left (w, tree, zz) -> Left (w, tree_rec tree, zip_rec zz)
  | Right (w, tree, zz) -> Right (w, tree_rec tree, zip_rec zz)

let program z = mpath (repr (zip_rec z))