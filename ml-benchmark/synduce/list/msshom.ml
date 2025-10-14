config EnableDeepCoder = true
config ComposeNum = 2

type list =
  | Nil of unit
  | Cons of int * list

type clist =
  | Single of int
  | Concat of clist * clist

let max a b =
  if a < b then b else a

let spec xs =
  let rec f xs =
    match xs with
    | Nil _ -> (0, 0, 0, 0)
    | Cons (hd, tl) ->
      let result = f tl in
      match result with
      | (r1, r2, r3, r4) ->
        let new_sum = hd + r1 in
        let new_mts = max r2 new_sum in
        let new_mps = max 0 (hd + r3) in
        let new_mss = max r4 new_mps in
        (new_sum, new_mts, new_mps, new_mss)
  in
  match f xs with
  | (_, _, _, x4) -> x4

let rec cat xs ys =
  match xs with
  | Nil _ -> ys
  | Cons (a, b) -> Cons (a, cat b ys)

val repr: clist -> list compress
let rec repr xs =
  match xs with
  | Single a -> Cons (a, Nil ())
  | Concat (a, b) -> cat (repr a) (repr b)

let program x =
  spec (repr x)