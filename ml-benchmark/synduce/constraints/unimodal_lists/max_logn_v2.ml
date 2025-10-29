type ulist = Uelt of int | Usplit of ulist * int * int * ulist
type llist = Elt of int | Cons of int * llist

let rec aux res xs =
  match xs with
  | Uelt x -> Cons (x, res)
  | Usplit (x, a, b, y) ->
      let t0 = aux res y in
      let t1 = Cons (b, t0) in
      let t2 = Cons (a, t1) in
      aux t2 x

let rec repr xs =
  match xs with
  | Uelt x -> Elt x
  | Usplit (x, a, b, y) ->
      let t0 = repr y in
      let t1 = Cons (b, t0) in
      let t2 = Cons (a, t1) in
      aux t2 x

let rec aux_down pre xs =
  match xs with
  | Elt x -> x > pre
  | Cons (h, t) ->
      if h > pre then aux_down h t else false

let rec aux_up pre xs =
  match xs with
  | Elt x -> x < pre
  | Cons (h, t) ->
      if pre < h then aux_up h t else aux_down h t

let is_unimodal xs =
  match xs with
  | Elt _ -> true
  | Cons (h, t) -> aux_up h t

let max a b = if a < b then b else a

let rec spec xs =
  match xs with
  | Elt x -> x
  | Cons (h, t) ->
      let m = spec t in
      max h m

val target: ulist -> ulist compress
let rec target xs =
  match xs with
  | Uelt x -> Uelt x
  | Usplit (x, a, b, y) ->
      if a > b then
        let tx = target x in
        Usplit (tx, a, b, y)
      else
        let ty = target y in
        Usplit (x, a, b, ty)

let program xs =
  if is_unimodal (repr xs) then
    let t = target xs in
    let r = repr t in
    spec r
  else
    0