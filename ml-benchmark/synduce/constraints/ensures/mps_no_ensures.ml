config EnableDeepCoder = true

type clist = Cnil of unit | Single of int | Concat of clist * clist
type ilist = Nil of unit | Cons of int * ilist

let rec cat x y =
  match x with
  | Cons (h, t) -> Cons (h, cat t y)
  | Nil _ -> y

val repr: clist -> ilist compress
let rec repr cl =
  match cl with
  | Cnil _ -> Nil ()
  | Single h -> Cons (h, Nil ())
  | Concat (l, r) -> cat (repr l) (repr r)

let max a b = if a < b then b else a

let spec xs =
  let rec f xs =
    match xs with
    | Nil _ -> (0, 0)
    | Cons (h, t) ->
        let res = f t in
        match res with
        | (a1, a2) -> (max 0 (a1 + h), a2 + h)
  in
  let tmp = f xs in
  match tmp with
  | (v, _) -> v

let program cl = spec (repr cl)