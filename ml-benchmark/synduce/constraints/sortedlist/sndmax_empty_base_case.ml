type clist = Empty | Elt of int | Concat of clist * clist
type ilist = Nil | Cons of int * ilist

let rec cat a b =
  match a with
  | Nil -> b
  | Cons (h, t) -> Cons (h, cat t b)

let rec repr xs =
  match xs with
  | Empty -> Nil
  | Elt w -> Cons (w, Nil)
  | Concat (l, r) -> cat (repr l) (repr r)

let rec all_pos c =
  match c with
  | Empty -> true
  | Elt w -> w > 0
  | Concat (l, r) -> all_pos l && all_pos r

let geq_head x xs =
  match xs with
  | Nil -> true
  | Cons (h, t) -> x >= h

let rec is_sorted xs =
  match xs with
  | Nil -> true
  | Cons (h, t) -> geq_head h t && is_sorted t

let min x y = if x < y then x else y
let max x y = if x > y then x else y

let spec xs =
  let rec f xs =
    match xs with
    | Nil -> (0, 0)
    | Cons (h, t) ->
      let res = f t in
      match res with
      | (r1, r2) -> (max r1 h, max r2 (min r1 h))
  in
  let res = f xs in
  match res with
  | (_, s) -> s

val target: clist -> clist compress
let rec target c =
  match c with
  | Empty -> c
  | Elt w -> c
  | Concat (l, r) -> Concat (target l, target r)

let program c =
  if is_sorted (repr c) then spec (repr (target c)) else 0