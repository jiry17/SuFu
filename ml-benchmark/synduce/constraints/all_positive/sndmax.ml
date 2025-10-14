type clist = Cnil of unit | Single of int | Concat of clist * clist
type tlist = Nil of unit | Cons of int * tlist

let rec allpos c =
  match c with
  | Cnil _ -> true
  | Single w -> w > 0
  | Concat (l, r) -> allpos l && allpos r

let rec cat x y =
  match x with
  | Cons (h, t) -> Cons (h, cat t y)
  | Nil _ -> y

let rec repr cl =
  match cl with
  | Cnil _ -> Nil ()
  | Single h -> Cons (h, Nil ())
  | Concat (l, r) -> cat (repr l) (repr r)

let min a b = if a < b then a else b
let max a b = if a < b then b else a

let spec xs =
  let rec f xs =
    match xs with
    | Nil _ -> (0, 0)
    | Cons (h, t) ->
        let res = f t in
        let r1 = fst res in
        let r2 = snd res in
        (max h r1, max r2 (min r1 h))
  in
  let p = f xs in
  snd p

let rec target xs =
  match xs with
  | Cnil _ -> Cnil ()
  | Single h -> Single h
  | Concat (l, r) -> Concat (target l, target r)

let program xs =
  if allpos xs then spec (repr (target xs)) else 0