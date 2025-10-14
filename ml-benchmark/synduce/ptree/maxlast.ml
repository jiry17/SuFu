type tree = Telt of int | Tnode of int * tree * tree

type ptree = Pelt of int | Pnode of int * plist
and plist = Elt of ptree | Cons of ptree * plist

let max a b = if a < b then b else a

let rec last t =
  match t with
  | Telt w -> w
  | Tnode (_, _, r) -> last r

let rec repr x =
  match x with
  | Telt a -> Pelt a
  | Tnode (a, l, r) ->
      let rec repr_list y =
        match y with
        | Telt a ->
            let z = Pelt a in
            Elt z
        | Tnode (a, l, r) ->
            Cons (Pelt a, Cons (repr l, repr_list r))
      in
      Pnode (a, Cons (repr l, repr_list r))

let rec spec x =
  match x with
  | Pelt a -> a
  | Pnode (a, l) ->
      let rec maxh_aux y =
        match y with
        | Elt a -> spec a
        | Cons (hd, tl) -> maxh_aux tl
      in
      max a (maxh_aux l)

let rec target t =
  match t with
  | Telt w -> t
  | Tnode (w, l, r) -> Tnode (w, target l, target r)

let program t = spec (repr (target t))