type btree =
  | Empty of unit
  | Node of int * btree * btree

type zipper =
  | Top of unit
  | Left of int * btree * zipper
  | Right of int * btree * zipper

let max a b = if a < b then b else a

let spec t =
  let rec f s t =
    match t with
    | Empty () -> s
    | Node (a, l, r) ->
        let result = f s l in
        match result with
        | (r1, r2) ->
            let sum = r1 + a in
            f (sum, max r2 sum) r
  in
  let res = f (0, 0) t in
  match res with
  | (_, v) -> v

let rec repr z =
  match z with
  | Top () -> Empty ()
  | Left (w, tree, zz) ->
      let info = spec tree in
      Node (w, tree, repr zz)
  | Right (w, tree, zz) ->
      let info = spec tree in
      Node (w, repr zz, tree)

let program z = spec (repr z)