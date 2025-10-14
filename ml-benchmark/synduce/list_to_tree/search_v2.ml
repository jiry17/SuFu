type tree = Empty | Node of int * tree * tree
type listt = Nil | Cons of int * listt

let rec cat x y =
  match x with
  | Cons (h, t) -> Cons (h, cat t y)
  | Nil -> y

let rec dec res t =
  match t with
  | Empty -> res
  | Node (w, l, r) ->
      let tmp = dec res r in
      let res2 = Cons (w, tmp) in
      dec res2 l

let rec repr t =
  match t with
  | Empty -> Nil
  | Node (w, l, r) ->
      let rr = repr r in
      let res = Cons (w, rr) in
      dec res l

let rec target t =
  match t with
  | Empty -> Empty
  | Node (w, l, r) ->
      let l2 = target l in
      let r2 = target r in
      Node (w, l2, r2)

let x = 0

let rec spec xs =
  match xs with
  | Nil -> false
  | Cons (h, t) ->
      if h = x then true else spec t

let program xs = spec (repr (target xs))