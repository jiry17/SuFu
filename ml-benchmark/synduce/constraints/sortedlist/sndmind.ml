type clist = Ctwo of int * int | Concat of clist * clist

type llist = Two of int * int | Cons of int * llist

let rec cat a b =
  match a with
  | Two (x, y) -> Cons (x, Cons (y, b))
  | Cons (h, t) -> Cons (h, cat t b)

let rec repr xs =
  match xs with
  | Ctwo (x, y) -> Two (x, y)
  | Concat (a, b) -> cat (repr a) (repr b)

let min a b =
  if a < b then a else b

let max a b =
  if a < b then b else a

let spec xs =
  let rec f xs =
    match xs with
    | Two (x, y) -> (min x y, max x y)
    | Cons (h, t) ->
      let res = f t in
      match res with
      | (p, q) -> (min p h, min q (max p h))
  in
  match f xs with
  | (_, b) -> b

let is_sorted =
  let rec aux pre xs =
    match xs with
    | Two (x, y) -> (pre > x) && (x > y)
    | Cons (h, t) -> (pre > h) && (aux h t)
  in
  fun xs ->
    match xs with
    | Two (x, y) -> x > y
    | Cons (h, t) -> aux h t

let rec target c =
  match c with
  | Ctwo (a, b) -> c
  | Concat (l, r) -> Concat (l, target r)

let program c =
  if is_sorted (repr c) then spec (repr (target c)) else 0