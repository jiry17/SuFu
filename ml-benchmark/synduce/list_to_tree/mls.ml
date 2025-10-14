type tree = Empty | Node of int * tree * tree
type list = Nil | Cons of int * list

let max a b = if a < b then b else a

let rec cat x y =
  match x with
  | Cons (h, t) -> Cons (h, cat t y)
  | Nil -> y

let rec dec res t =
  match t with
  | Empty -> res
  | Node (w, l, r) -> Cons (w, dec (dec res l) r)

let rec repr t =
  match t with
  | Empty -> Nil
  | Node (w, l, r) -> Cons (w, dec (repr l) r)

let rec target t =
  match t with
  | Empty -> Empty
  | Node (w, l, r) -> Node (w, target l, target r)

let x = 0

let rec spec xs =
  match xs with
  | Nil -> (0, 0)
  | Cons (h, t) ->
      match spec t with
      | result1, result2 ->
          let sum = h + result1 in
          (sum, max result2 sum)

let program xs = spec (repr (target xs))