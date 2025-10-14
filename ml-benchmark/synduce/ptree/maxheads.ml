type tree = Telt of int | Tnode of int * tree * tree

type ptree = Pelt of int | Pnode of int * plist
and plist = Elt of ptree | Cons of ptree * plist

let max a b = if a < b then b else a

let rec repr x =
  match x with
  | Telt a -> Pelt a
  | Tnode (a, l, r) ->
      let rec repr_list y =
        match y with
        | Telt a ->
            let z = Pelt a in
            Elt z
        | Tnode (a2, l2, r2) ->
            Cons (Pelt a2, Cons (repr l2, repr_list r2))
      in
      Pnode (a, Cons (repr l, repr_list r))

let rec spec x =
  match x with
  | Pelt a -> a
  | Pnode (a, l) ->
      let rec maxh_aux y =
        match y with
        | Elt a2 -> spec a2
        | Cons (hd, tl) -> spec hd
      in
      max a (maxh_aux l)

let program x = spec (repr x)