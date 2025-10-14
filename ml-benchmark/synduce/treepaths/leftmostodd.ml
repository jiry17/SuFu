type btree = Empty of unit | Node of int * btree * btree
type zipper = Top of unit | Left of int * btree * zipper | Right of int * btree * zipper

let max a b = if a < b then b else a

let mod2 x = x - ((x / 2) * 2)

let spec t =
  let rec f t =
    match t with
    | Empty _ -> (false, 1)
    | Node (a, l, r) ->
        let result = f l in
        match result with
        | (b, i) ->
            if b then (b, i) else
            if 1 = mod2 a then (true, a) else f r
  in
  let res = f t in
  match res with (_, i) -> i

let repr z =
  let rec f z =
    match z with
    | Top _ -> Empty ()
    | Left (w, tree, zz) -> Node (w, tree, f zz)
    | Right (w, tree, zz) -> Node (w, f zz, tree)
  in
  f z

let tree_rec t =
  let rec f t =
    match t with
    | Empty _ -> Empty ()
    | Node (a, l, r) -> Node (a, f l, f r)
  in
  f t

let zip_rec z =
  let rec f z =
    match z with
    | Top _ -> Top ()
    | Left (w, tree, zz) -> Left (w, tree_rec tree, f zz)
    | Right (w, tree, zz) -> Right (w, tree_rec tree, f zz)
  in
  f z

let program z = spec (repr (zip_rec z))