type ulist = Unil | Uelt of int | Usplit of ulist * int * int * ulist
type llist = Nil | Cons of int * llist

let rec repr_f res xs =
  match xs with
  | Unil -> res
  | Uelt x -> Cons (x, res)
  | Usplit (x, a, b, y) ->
      let t1 = repr_f res y in
      let t2 = Cons (b, t1) in
      let t3 = Cons (a, t2) in
      repr_f t3 x

let repr xs = repr_f Nil xs

let is_unimodal xs =
  let rec aux_down pre xs =
    match xs with
    | Nil -> pre >= 0
    | Cons (h, t) -> (pre >= 0) && ((pre > h) && (aux_down h t))
  and aux_up pre xs =
    match xs with
    | Nil -> pre >= 0
    | Cons (h, t) -> (pre >= 0) && (if pre < h then aux_up h t else aux_down h t)
  in
  match xs with
  | Nil -> true
  | Cons (h, t) -> aux_up h t

let max a b = if a < b then b else a

let rec spec xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) -> max h (spec t)

let rec target xs =
  match xs with
  | Unil -> Unil
  | Uelt x -> Uelt x
  | Usplit (x, a, b, y) -> Usplit (target x, a, b, target y)

let program xs =
  if is_unimodal (repr xs) then spec (repr (target xs)) else 0