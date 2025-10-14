config EnableDeepCoder = true

type list = Nil of unit | Cons of int * list
type clist = CNil of unit | Single of int | Concat of clist * clist

let max a b = if a < b then b else a

let spec xs =
  let rec f xs =
    match xs with
    | Nil _ -> (0, 0, 0)
    | Cons (hd, tl) ->
      let result = f tl in
      match result with
      | (sum, mps, mts) ->
        let new_sum = hd + sum in
        let new_mps = max 0 (hd + mps) in
        let new_mts = max mts (hd + sum) in
        (new_sum, new_mps, new_mts)
  in
  let res = f xs in
  match res with
  | (_, mps, mts) -> (mps, mts)

let rec cat xs ys =
  match xs with
  | Nil _ -> ys
  | Cons (a, b) -> Cons (a, cat b ys)

val repr: clist -> list compress
let rec repr xs =
  match xs with
  | CNil _ -> Nil ()
  | Single a -> Cons (a, Nil ())
  | Concat (a, b) -> cat (repr a) (repr b)

let program x = spec (repr x)