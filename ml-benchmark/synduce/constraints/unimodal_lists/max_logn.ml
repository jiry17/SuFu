type ulist = Unil of unit | Uelt of int | Usplit of ulist * int * int * ulist
type ilist = Nil of unit | Cons of int * ilist

let repr =
  let rec f res xs =
    match xs with
    | Unil _ -> res
    | Uelt x -> Cons (x, res)
    | Usplit (x, a, b, y) -> f (Cons (a, Cons (b, f res y))) x
  in
  fun xs -> f (Nil ()) xs

let is_unimodal =
  let rec aux_down pre xs =
    match xs with
    | Nil _ -> pre > 0
    | Cons (h, t) -> (pre > 0) && ((pre > h) && (aux_down h t))
  and aux_up pre xs =
    match xs with
    | Nil _ -> pre > 0
    | Cons (h, t) -> (pre > 0) && (if pre < h then aux_up h t else aux_down h t)
  in
  fun xs ->
    match xs with
    | Nil _ -> true
    | Cons (h, t) -> aux_up h t

let max a b = if a < b then b else a

let rec spec xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> max h (spec t)

let rec target xs =
  match xs with
  | Unil _ -> Unil ()
  | Uelt x -> Uelt x
  | Usplit (x, a, b, y) ->
      if a > b then Usplit (target x, a, b, y) else Usplit (x, a, b, target y)

let program xs =
  if is_unimodal (repr xs) then spec (repr (target xs)) else 0