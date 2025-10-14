type ulist = Unil of unit | Uelt of int | Usplit of ulist * int * int * ulist
type list_ = Nil of unit | Cons of int * list_

let repr =
  let rec repr_f res xs =
    match xs with
    | Unil _ -> res
    | Uelt x -> Cons (x, res)
    | Usplit (x, a, b, y) ->
        let fy = repr_f res y in
        let cb = Cons (b, fy) in
        let ca = Cons (a, cb) in
        repr_f ca x
  in
  fun xs -> repr_f (Nil ()) xs

let is_unimodal =
  let rec aux_down pre xs =
    match xs with
    | Nil _ -> true
    | Cons (h, t) -> (pre >= h) && aux_down h t
  and aux_up pre xs =
    match xs with
    | Nil _ -> true
    | Cons (h, t) ->
        if pre <= h then aux_up h t else aux_down h t
  in
  fun xs ->
    match xs with
    | Nil _ -> true
    | Cons (h, t) -> aux_up h t

let rec spec xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> h + spec t

let rec target xs =
  match xs with
  | Unil _ -> Unil ()
  | Uelt x -> Uelt x
  | Usplit (x, a, b, y) -> Usplit (target x, a, b, target y)

let program xs =
  if is_unimodal (repr xs) then spec (repr (target xs)) else 0