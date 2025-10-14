type ulist = Unil of unit | Uelt of int | Usplit of ulist * int * int * ulist
type lst = Nil of unit | Cons of int * lst

let rec repr_acc res xs =
  match xs with
  | Unil _ -> res
  | Uelt x -> Cons (x, res)
  | Usplit (x, a, b, y) ->
      let r1 = repr_acc res y in
      let r2 = Cons (b, r1) in
      let r3 = Cons (a, r2) in
      repr_acc r3 x

let repr xs =
  repr_acc (Nil ()) xs

let rec aux_down pre xs =
  match xs with
  | Nil _ -> true
  | Cons (h, t) -> pre >= h && aux_down h t

let rec aux_up pre xs =
  match xs with
  | Nil _ -> true
  | Cons (h, t) ->
      if pre <= h then aux_up h t else aux_down h t

let is_unimodal xs =
  match xs with
  | Nil _ -> true
  | Cons (h, t) -> aux_up h t

let w = 0

let rec spec xs =
  match xs with
  | Nil _ -> false
  | Cons (h, t) -> h = w || spec t

let rec target xs =
  match xs with
  | Unil _ -> Unil ()
  | Uelt x -> Uelt x
  | Usplit (x, a, b, y) -> Usplit (target x, a, b, target y)

let program xs =
  if is_unimodal (repr xs) then spec (repr (target xs)) else false