type tree = Empty of unit | Node of int * tree * tree
type list = Nil of unit | Cons of int * list

let rec cat x y =
  match x with
  | Cons (h, t) -> Cons (h, cat t y)
  | Nil _ -> y

let rec dec res t =
  match t with
  | Empty _ -> res
  | Node (w, l, r) -> Cons (w, dec (dec res r) l)

let rec repr t =
  match t with
  | Empty _ -> Nil ()
  | Node (w, l, r) -> Cons (w, dec (repr r) l)

let rec target t =
  match t with
  | Empty _ -> Empty ()
  | Node (w, l, r) -> Node (w, target l, target r)

let x = read_int ()

let rec spec xs =
  match xs with
  | Nil _ -> false
  | Cons (h, t) -> if h = x then true else spec t

let program xs = spec (repr (target xs))